# esqueleto de grid search
# se espera que los alumnos completen lo que falta
#   para recorrer TODOS cuatro los hiperparametros

rm(list = ls()) # Borro todos los objetos
gc() # Garbage Collection

require("data.table")
require("rpart")
require("parallel")

PARAM <- list()
# reemplazar por las propias semillas
PARAM$semillas <- c(100417, 100547, 100733, 100747, 100829)

#------------------------------------------------------------------------------
# particionar agrega una columna llamada fold a un dataset
#  que consiste en una particion estratificada segun agrupa
# particionar( data=dataset, division=c(70,30), agrupa=clase_ternaria, seed=semilla)
#   crea una particion 70, 30

particionar <- function(data, division, agrupa = "", campo = "fold", start = 1, seed = NA) {
  if (!is.na(seed)) set.seed(seed)

  bloque <- unlist(mapply(function(x, y) {
    rep(y, x)
  }, division, seq(from = start, length.out = length(division))))

  data[, (campo) := sample(rep(bloque, ceiling(.N / length(bloque))))[1:.N],
    by = agrupa
  ]
}
#------------------------------------------------------------------------------

ArbolEstimarGanancia <- function(semilla, param_basicos) {
  # particiono estratificadamente el dataset
  particionar(dataset, division = c(7, 3), agrupa = "binariaR", seed = semilla)

  # genero el modelo
  # quiero predecir clase_ternaria a partir del resto
  modelo <- rpart("binariaR ~ .",
    data = dataset[fold == 1], # fold==1  es training,  el 70% de los datos
    xval = 0,
    control = param_basicos
  ) # aqui van los parametros del arbol

  # aplico el modelo a los datos de testing
  prediccion <- predict(modelo, # el modelo que genere recien
    dataset[fold == 2], # fold==2  es testing, el 30% de los datos
    type = "prob"
  ) # type= "prob"  es que devuelva la probabilidad

  # prediccion es una matriz con TRES columnas,
  #  llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
  # cada columna es el vector de probabilidades


  # calculo la ganancia en testing  qu es fold==2
  ganancia_test <- dataset[
    fold == 2,
    sum(ifelse(prediccion[, "POS"] > 0.025,
      ifelse(binariaR == "POS", 117000, -3000),
      0
    ))
  ]

  # escalo la ganancia como si fuera todo el dataset
  ganancia_test_normalizada <- ganancia_test / 0.3

  return(ganancia_test_normalizada)
}
#------------------------------------------------------------------------------

ArbolesMontecarlo <- function(semillas, param_basicos) {
  # la funcion mcmapply  llama a la funcion ArbolEstimarGanancia
  #  tantas veces como valores tenga el vector  PARAM$semillas
  ganancias <- mcmapply(ArbolEstimarGanancia,
    semillas, # paso el vector de semillas
    MoreArgs = list(param_basicos), # aqui paso el segundo parametro
    SIMPLIFY = FALSE,
    mc.cores = 5 # en Windows este valor debe ser 1
  )

  ganancia_promedio <- mean(unlist(ganancias))

  return(ganancia_promedio)
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

# Aqui se debe poner la carpeta de la computadora local
setwd("~/buckets/b1/") # Establezco el Working Directory
# cargo los datos

# cargo los datos
dataset <- fread("./datasets/dataset_pequeno.csv")

#REYNOSO:
  # creo variable nueva
dataset<-dataset[,binariaR:=ifelse(clase_ternaria=="","",ifelse(clase_ternaria=="BAJA+2","POS","NEG"))]
  #borro clase ternaria
dataset<-dataset[,clase_ternaria:=NULL]

# trabajo solo con los datos con clase, es decir 202107
dataset <- dataset[binariaR != ""]

# genero el archivo para Kaggle
# creo la carpeta donde va el experimento
# HT  representa  Hiperparameter Tuning
dir.create("./exp/", showWarnings = FALSE)
dir.create("./exp/HT2020/", showWarnings = FALSE)
archivo_salida <- "./exp/HT2020/gridsearchBinario.txt"

# genero la data.table donde van los resultados del Grid Search
tb_grid_search <- data.table( max_depth = integer(),
                              min_split = integer(),
                              min_bucket=integer(), #REYNOSO
                              cp=numeric(), #REYNOSO
                              ganancia_promedio = numeric() )


# itero por los loops anidados para cada hiperparametro
for (c_cp in c(-1,-0.5,0,0.1)){ #REYNOSO: agrego un loop externo para cambiar cp
  #elijo esos valores para tener dos negativos (del cual uno es máximo y otro a medio camino entre -1 y 0. Mantengo 0 y 0.1) 
  
for (c_minbucket in c(2,10,20,40,60,80,100,120,200,400)){ #REYNOSO: agrego un loop externo para cambiar minbucket
    #c_minbucket=vminsplit/5. Para que en la mayoría de los casos se cumpla que minbucket*2<=minsplit

for (vmax_depth in c(4, 6, 8, 10, 12, 14)) {
  #REYNOSO: mantengo esos valores. En mis experimentos se maximiza la ganancia con 6, así qie considero razonable los valores propuestos.
  for (vmin_split in c(10, 50, 100, 200, 300, 400, 500, 600, 1000,2000)) {
    #REYNOSO: propongo valores crecientes en forma casi exponencial
    # notar como se agrega

    # vminsplit  minima cantidad de registros en un nodo para hacer el split
    param_basicos <- list(
      "cp" = c_cp, #REYNOSO: modifico por la variable a iterar
      "minsplit" = vmin_split,
      "minbucket" = c_minbucket, #REYNOSO: modifico por la variable a iterar
      "maxdepth" = vmax_depth
    ) # profundidad máxima del arbol

    # Un solo llamado, con la semilla 17
    ganancia_promedio <- ArbolesMontecarlo(PARAM$semillas, param_basicos)

    # agrego a la tabla
    tb_grid_search <- rbindlist( 
      list( tb_grid_search, 
            list( vmax_depth, vmin_split, c_minbucket,c_cp,ganancia_promedio) ) )

  }}} #REYNOSO: aquí cierro los loop creados, ya que la línea siguiente indica que debe ejecutarse en el loop más externo

  # escribo la tabla a disco en cada vuelta del loop mas externo
  Sys.sleep(2)  # espero un par de segundos

  fwrite( tb_grid_search,
          file = archivo_salida,
          sep = "\t" )
}
