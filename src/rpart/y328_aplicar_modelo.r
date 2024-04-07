# Generacion de archivo para Kaggle
# clase_binaria2  POS = { BAJA+1, BAJA+2 }
# se usan weights

# cargo las librerias que necesito
require("data.table")
require("rpart")
require("rpart.plot")

# cargar aqui sus parametros ganadores de la 
# optimización de hiperparametros
PARAM <- list()

PARAM$minsplit <- 788
PARAM$minbucket <- 300
PARAM$maxdepth <- 7
PARAM$peso_positivos <- 36.2

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui comienza el programa

setwd("~/buckets/b1/") # Establezco el Working Directory

# cargo el dataset
dataset <- fread("./datasets/dataset_pequeno.csv")

dtrain <- dataset[foto_mes == 202107] # defino donde voy a entrenar
dapply <- dataset[foto_mes == 202109] # defino donde voy a aplicar el modelo

# defino la clase_binaria2  POS = {BAJA+1, BAJA+2}
dtrain[, clase_binaria2 := 
  ifelse(clase_ternaria=="CONTINUA", "NEG", "POS" )]

campos_buenos <- setdiff( colnames(dtrain), c( "clase_ternaria" ) )


# genero el modelo,  aqui se construye el arbol

# calculo los pesos
vpeso <<- copy(as.vector(
  dtrain[, ifelse( clase_binaria2=="POS", PARAM$peso_positivos, 1.0)  ]))

# genero los parametros propios de rpart
param_rpart <- list()
param_rpart$cp <- -1.0  # lo dejo fijo
param_rpart$minsplit <- PARAM$minsplit
param_rpart$minbucket <- PARAM$minbucket
param_rpart$maxdepth <- PARAM$maxdepth


modelo <- rpart(
  formula = "clase_binaria2 ~ .",
  data = dtrain[, campos_buenos, with=FALSE], # los datos donde voy a entrenar
  xval = 0,
  control = param_rpart,
  weights = vpeso
)


# aplico el modelo a los datos nuevos
prediccion <- predict(
  object = modelo,
  newdata = dapply,
  type = "prob"
)

# prediccion es una matriz con DOS columnas,
# llamadas "POS", "NEG"
# cada columna es el vector de probabilidades

# agrego a dapply una columna nueva que es la probabilidad de POS
dapply[, prob_pos := prediccion[, "POS"]]

# genero el archivo para Kaggle
# primero creo la carpeta donde va el experimento
dir.create("./exp/")
dir.create("./exp/KA3280")

# ordeno descendente por probabilidad
setorder( dapply, -prob_pos )

dapply[ , Predicted := 0L ]

# genero distintas salidas, una para cada corte
# debe ser recorrido en orden creciente
for( envios in seq( 7000, 13000, 500 ) ) {

  dapply[ 1:envios, Predicted := 1L]

  # solo los campos para Kaggle
  fwrite(dapply[, list(numero_de_cliente, Predicted)],
    file = paste0( "./exp/KA3280/KA3280_001_", envios, ".csv"),
    sep = ","
  )

}
