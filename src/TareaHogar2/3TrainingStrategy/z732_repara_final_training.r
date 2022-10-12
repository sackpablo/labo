#Necesita para correr en Google Cloud
# 128 GB de memoria RAM
# 256 GB de espacio en el disco local
#   8 vCPU


#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")

#------------------------------------------------------------------------------
options(error = function() { 
  traceback(20); 
  options(error = NULL); 
  stop("exiting after script error") 
})
#------------------------------------------------------------------------------

generar_final_training  <- function( FE, TS, periodo )
{
  arch_FE  <- paste0( "./exp/", FE, "/dataset.csv.gz" )
  dataset  <- fread( arch_FE )

  arch_train_final  <- paste0( "./exp/", TS, "/dataset_train_final.csv.gz" )

  #grabo los datos donde voy a entrenar los Final Models
  fwrite( dataset[ foto_mes %in% periodo, ],
          file= arch_train_final,
          logical01= TRUE,
          sep= "," )

  rm( dataset )
  gc()
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Aqui empieza el programa

setwd( "~/buckets/b1/" )


generar_final_training( "FE7250", "TS7310", c( 202103 ) )
generar_final_training( "FE7250", "TS7311", c( 202101, 202102, 202103 ) )
generar_final_training( "FE7250", "TS7312", c( 202012, 202101, 202102, 202103 ) )

generar_final_training( "FE7252", "TS7314", c( 202101, 202102, 202103 ) )
generar_final_training( "FE7252", "TS7315", c( 202101, 202102, 202103 ) )

generar_final_training( "FE7253", "TS7316", c( 202101, 202102, 202103 ) )

generar_final_training( "FE7256", "TS7318", c( 202101, 202102, 202103 ) )

cat( "Ha finalizado la actualizacion.\n")
date()

