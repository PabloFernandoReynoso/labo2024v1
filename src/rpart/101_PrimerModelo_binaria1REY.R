# Arbol elemental con libreria  rpart
# Debe tener instaladas las librerias  data.table  ,  rpart  y  rpart.plot

# cargo las librerias que necesito
require("data.table")
require("rpart")
require("rpart.plot")

# Aqui se debe poner la carpeta de la materia de SU computadora local
setwd("C:\\Users\\pablo\\labo2024v1\\") # Establezco el Working Directory

# cargo el dataset
dataset <- fread("./datasets/dataset_pequeno.csv")

#REYNOSO:
# creo variable nueva
dataset<-dataset[,binariaR:=ifelse(clase_ternaria=="","",ifelse(clase_ternaria=="BAJA+2","POS","NEG"))]
#borro clase ternaria
dataset<-dataset[,clase_ternaria:=NULL]

dtrain <- dataset[foto_mes == 202107] # defino donde voy a entrenar
dapply <- dataset[foto_mes == 202109] # defino donde voy a aplicar el modelo

# genero el modelo,  aqui se construye el arbol
# quiero predecir clase_ternaria a partir de el resto de las variables
modelo <- rpart(
        formula = "binariaR ~ .",
        data = dtrain, # los datos donde voy a entrenar
        xval = 0,
        cp = -0.5, # esto significa no limitar la complejidad de los splits
        minsplit = 200, # minima cantidad de registros para que se haga el split
        minbucket = 200, # tamaño minimo de una hoja
        maxdepth = 6
) # profundidad maxima del arbol


# grafico el arbol
prp(modelo,
        extra = 101, digits = -5,
        branch = 1, type = 4, varlen = 0, faclen = 0,cex=.75
)


# aplico el modelo a los datos nuevos
prediccion <- predict(
        object = modelo,
        newdata = dapply,
        type = "prob"
)

# prediccion es una matriz con TRES columnas,
# llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
# cada columna es el vector de probabilidades

# agrego a dapply una columna nueva que es la probabilidad de BAJA+2
dapply[, prob_baja2 := prediccion[, "POS"]]

# solo le envio estimulo a los registros
#  con probabilidad de BAJA+2 mayor  a  1/40
dapply[, Predicted := as.numeric(prob_baja2 > 1 / 40)]

# genero el archivo para Kaggle
# primero creo la carpeta donde va el experimento
#dir.create("./exp/")
#dir.create("./exp/KA2001")

# solo los campos para Kaggle
fwrite(dapply[, list(numero_de_cliente, Predicted)],
        file = "./exp/KA2001/K101_038.csv",
        sep = ","
)

