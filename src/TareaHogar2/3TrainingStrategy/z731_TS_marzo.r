#Necesita para correr en Google Cloud
#  64 GB de memoria RAM
# 256 GB de espacio en el disco local
#   8 vCPU


#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")



#Parametros del script
kexperimento  <- "TS7310"

kexp_input  <- "FE7250"

kfuture       <- c( 202105 )

kfinal_train  <- c( 202103 )

ktraining     <- c( 202103 )
kvalidation   <- c( 202103 )
ktesting      <- c( 202103 )
# FIN Parametros del script

#------------------------------------------------------------------------------

options(error = function() { 
  traceback(20); 
  options(error = NULL); 
  stop("exiting after script error") 
})

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Aqui empieza el programa

setwd( "~/buckets/b1/" )

#cargo el dataset donde voy a entrenar
#esta en la carpeta del exp_input y siempre se llama  dataset.csv.gz
dataset_input  <- paste0( "./exp/", kexp_input, "/dataset.csv.gz" )
dataset  <- fread( dataset_input )


#creo la carpeta donde va el experimento
dir.create( paste0( "./exp/", kexperimento, "/"), showWarnings = FALSE )
setwd(paste0( "./exp/", kexperimento, "/"))   #Establezco el Working Directory DEL EXPERIMENTO


setorder( dataset, foto_mes, numero_de_cliente )

#grabo los datos del futuro
fwrite( dataset[ foto_mes %in% kfuture, ],
        file= "dataset_future.csv.gz",
        logical01= TRUE,
        sep= "," )

#grabo los datos donde voy a entrenar los Final Models
fwrite( dataset[ foto_mes %in% kfuture, ],
        file= "dataset_train_final.csv.gz",
        logical01= TRUE,
        sep= "," )


#grabo los datos donde voy a hacer el training y la optimizacion de hiperparametros
dataset[  , fold_train := 0L ]
dataset[ foto_mes %in% ktraining, fold_train := 1L ]

dataset[  , fold_validate := 0L ]
dataset[ foto_mes %in% kvalidation, fold_validate := 1L ]

dataset[  , fold_test := 0L ]
dataset[ foto_mes %in% ktesting, fold_test := 1L ]

fwrite( dataset[ fold_train + fold_validate + fold_test >= 1 , ],
        file= "dataset_training.csv.gz",
        logical01= TRUE,
        sep= "," )

