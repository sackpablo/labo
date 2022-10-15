#Necesita para correr en Google Cloud
# 128 GB de memoria RAM
# 256 GB de espacio en el disco local
#   8 vCPU


#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("primes")

require("lightgbm")

#Parametros del script
kexperimento  <- "ZZ7955"
kexp_input  <- "HT7415"

ksemillerio  <- 50
kmodelos  <- 2
# FIN Parametros del script

ksemilla_primos  <- 102191  #reemplazar por la propia semilla

#------------------------------------------------------------------------------
options(error = function() { 
  traceback(20); 
  options(error = NULL); 
  stop("exiting after script error") 
})
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Aqui empieza el programa

#genero un vector de una cantidad de ksemillerio  de semillas,  buscando numeros primos al azar
primos  <- generate_primes(min=100000, max=1000000)  #genero TODOS los numeros primos entre 100k y 1M
set.seed( ksemilla_primos ) #seteo la semilla que controla al sample de los primos
ksemillas  <- sample(primos)[ 1:ksemillerio ]   #me quedo con ksemillerio primos al azar


base_dir <- "~/buckets/b1/"

#creo la carpeta donde va el experimento
dir.create( paste0( base_dir, "exp/", kexperimento, "/"), showWarnings = FALSE )
setwd(paste0( base_dir, "exp/", kexperimento, "/"))   #Establezco el Working Directory DEL EXPERIMENTO

#leo la salida de la optimizaciob bayesiana
arch_log  <- paste0( base_dir, "exp/", kexp_input, "/BO_log.txt" )
tb_log  <- fread( arch_log )
setorder( tb_log, -ganancia )

#leo el nombre del expermento de la Training Strategy
arch_TS  <- paste0( base_dir, "exp/", kexp_input, "/TrainingStrategy.txt" )
TS  <- readLines( arch_TS, warn=FALSE )

#leo el dataset donde voy a aplicar el modelo final
arch_future  <- paste0( base_dir, "exp/", TS, "/dataset_future.csv.gz" )
dfuture <- fread( arch_future )
setorder( dfuture, foto_mes, numero_de_cliente )


#leo el dataset donde voy a entrenar el modelo final
arch_dataset  <- paste0( base_dir, "exp/", TS, "/dataset_train_final.csv.gz" )
dataset  <- fread( arch_dataset )

#creo la clase binaria
dataset[ , clase01 := ifelse( clase_ternaria %in% c("BAJA+1","BAJA+2"), 1, 0 )  ]

campos_buenos  <- setdiff( colnames(dataset),
                           c( "clase_ternaria", "clase01") )



#donde almaceno las <iteracion_bayesiana, semilla> que ya se procesaron
if( file.exists( "tb_modelos_semillas.txt" ) )
{
  tb_modelos_semillas  <- fread( "tb_modelos_semillas.txt" )
} else {
  tb_modelos_semillas  <- data.table( iter_bayesiana= integer(),
                                      semilla= integer() )
}


#genero un modelo para cada uno de las "kmodelos" MEJORES iteraciones de la Bayesian Optimization
for( imodelo in  1:kmodelos )
{
  parametros  <- as.list( copy( tb_log[ imodelo ] ) )
  iteracion_bayesiana  <- parametros$iteracion_bayesiana

  #creo CADA VEZ el dataset de lightgbm
  dtrain  <- lgb.Dataset( data=    data.matrix( dataset[ , campos_buenos, with=FALSE] ),
                          label=   dataset[ , clase01],
                          weight=  dataset[ , ifelse( clase_ternaria %in% c("BAJA+2"), 1.0000001, 1.0)],
                          free_raw_data= FALSE
                        )

  ganancia  <- parametros$ganancia

  #elimino los parametros que no son de lightgbm
  parametros$experimento  <- NULL
  parametros$cols         <- NULL
  parametros$rows         <- NULL
  parametros$fecha        <- NULL
  parametros$prob_corte   <- NULL
  parametros$estimulos    <- NULL
  parametros$ganancia     <- NULL
  parametros$iteracion_bayesiana  <- NULL


  #Donde voy almacenando el acumulado de las ganancias
  nom_semillerio  <- paste0( "tb_prediccion_semillerio_", sprintf( "%03d", iteracion_bayesiana ),".txt" )

  #aqui guardo el acumulado de las probabilidades y rankings
  if( file.exists( nom_semillerio ) ) {
    tb_prediccion_semillerio  <- fread( nom_semillerio )
  } else {
    tb_prediccion_semillerio  <- dfuture[  , list(numero_de_cliente, foto_mes) ]
    tb_prediccion_semillerio[ , pred_acum_prob := 0L ]
    tb_prediccion_semillerio[ , pred_acum_rank := 0L ]
  }

  #calculo lo que me queda por procesar
  isemilla  <-  nrow( tb_modelos_semillas[  iter_bayesiana == iteracion_bayesiana ] )
  semillas_faltan <-  setdiff( ksemillas,  tb_modelos_semillas[  iter_bayesiana == iteracion_bayesiana, semilla  ]  )

  if( length( semillas_faltan ) == 0 )  break  #salgo del loop si no hay nada para procesar

  for( semilla  in  semillas_faltan )
  {
    isemilla  <- isemilla + 1

    #asigno la semilla
    parametros$seed  <- semilla
  
    #genero el modelo entrenando en los datos finales
    set.seed( parametros$seed )
    modelo_final  <- lightgbm( data= dtrain,
                               param=  parametros,
                               verbose= -100 )


    #creo y grabo la importancia de variables solo para la primer semilla
    if( isemilla == 1 ) {
      tb_importancia  <- as.data.table( lgb.importance( modelo_final ) )
      fwrite( tb_importancia,
              file= paste0( "impo_", 
                            sprintf( "%02d", imodelo ),
                            "_",
                            sprintf( "%03d", iteracion_bayesiana ),
                            ".txt" ),
              sep= "\t" )

      rm( tb_importancia )
    }


    #genero la prediccion, Scoring
    prediccion  <- predict( modelo_final,
                            data.matrix( dfuture[ , campos_buenos, with=FALSE ] ) )

    #creo los rankings a partir de las probabilidades
    prediccion_rank  <- frank( prediccion, ties.method= "random" )

    #acumulo  en tb_prediccion_semillerio
    setorder( tb_prediccion_semillerio, foto_mes, numero_de_cliente )
    tb_prediccion_semillerio[ , pred_acum_prob := pred_acum_prob + prediccion ]
    tb_prediccion_semillerio[ , pred_acum_rank := pred_acum_rank + prediccion_rank ]


    #genero los archivos para Kaggle de esta vuelta
    #los archivos se van pisando en cada vuelta
    cortes  <- seq( from=  7000,
                    to=   11000,
                    by=     500 )

    #ordeno por ranking acumulado descendente
    setorder( tb_prediccion_semillerio, -pred_acum_rank )

    for( corte in cortes )
    {
      tb_prediccion_semillerio[  , Predicted := 0L ]
      tb_prediccion_semillerio[ 1:corte, Predicted := 1L ]

      nom_submit  <- paste0( kexperimento, 
                             "_",
                             sprintf( "%02d", imodelo ),
                             "_",
                             sprintf( "%03d", iteracion_bayesiana ),
                             "_",
                             sprintf( "%05d", corte ),
                             ".csv" )

      fwrite( tb_prediccion_semillerio[ , list( numero_de_cliente, Predicted ) ],
              file= nom_submit,
              sep= "," )
    }

    #reordeno  
    setorder( tb_prediccion_semillerio, foto_mes, numero_de_cliente )

    #elimino el campo Predicted, ya no lo necesito
    tb_prediccion_semillerio[  , Predicted := NULL ]

    #grabo el semillerio, para poder reutilizarlo en caso que se me corte el proceso/vm
    fwrite( tb_prediccion_semillerio,
            file= nom_semillerio,
            sep= "\t" )

    #acumulo lo que ya corri
    tb_modelos_semillas  <- rbind( tb_modelos_semillas,
                                   list( iteracion_bayesiana, semilla ) )
    fwrite( tb_modelos_semillas,
            file = "tb_modelos_semillas.txt",
            sep= "\t" )

    rm( modelo_final )
    gc()
  }



  #borro y limpio la memoria para la vuelta siguiente del for
  rm( tb_prediccion_semillerio )
  rm( parametros )
  rm( dtrain )
  gc()
}

