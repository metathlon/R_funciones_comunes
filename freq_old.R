
#==============================================================================================================
# Funcion para hacer tablas de frecuencias
#  df             = data.frame
#  variables      = una o varias variables c("aSDF","fds")
#  group_by_col   = una o varias variables c("SEXO","COINCIDEN")
#  decimales      = 2 (default), número de decimales para el redondeo
#  show_warnings  = por defecto muestra los warnings pero se puede poner en FALSE para rMarkdown y esas cosas
#  n              = default TRUE: show total of valid cases
#  missing        = default TRUE: show total of missing cases
#  min            = default TRUE: show min
#  max            = default TRUE: show max
#  mean           = default TRUE: show mean
#  sd             = default TRUE: show standard deviation
#  median         = default TRUE: show median
#  range          = default TRUE: show range
#
#==============================================================================================================
# EJEMPLO DE USO
# -------------------
# Generamos datos aleatorios
# edad_ <- sample(x = 65:100, size=30, replace = TRUE )
# estatura_ <- as.integer(sample(x = 120:205, size=30, replace = TRUE ))
# sexo_ <- sample(x = c("Hombre", "Mujer"), prob = c(.5,.5), size = 30, replace = TRUE)
# rubio_ <- sample(x = c("Si", "No"), prob = c(.2,.8), size = 30, replace = TRUE)
# data_ <- data.frame(EDAD=edad_,ESTATURA=estatura_,SEXO=sexo_,RUBIO=rubio_)
# 
# # Medias
# medias(data_,variables="EDAD")
# medias(data_,variables="EDAD", group_by_col="SEXO")
# 
# medias(data_,variables=c("EDAD","ESTATURA"))
# medias(data_,variables=c("EDAD","ESTATURA"), group_by_col=c("SEXO","RUBIO"))
#
#==================================================================================
freq <- function(df,..., group_by_col = NULL, pretty=TRUE, decimales=2, show_warnings = TRUE, n=TRUE, total=TRUE) {
  require("dplyr")

  vars <- enquos(...)
  for (v in vars) 
  {
      result_temp <- df %>% group_by(!! v) %>%
                           summarise(
                                      n_valido = n(),
                                     ) %>%
                           mutate(rel.freq = (n_valido/sum(n_valido))*100)

      
      #-------- add the column for values within the variable
      #result_temp <- as.data.frame(result_temp)
      colnames(result_temp)[1] <- "VALUES"

      #-------- add the column with the variable NAME
      if (pretty == TRUE)
      {
        result_temp <- mutate(result_temp,VAR=c(quo_name(v),rep("",nrow(result_temp)-1)))

      }
      else 
      {
       result_temp <- mutate(result_temp,VAR=rep(quo_name(v),nrow(result_temp)))
      }
      #-- take VAR to the front
      result_temp <- result_temp %>% select(VAR, everything())

      #-------- add totals row 
      if (total == TRUE)
      {
          var_total <- paste(quo_name(v),"TOTAL")
          levels(result_temp$VALUES) <- c(levels(result_temp$VALUES),"--")
          result_temp <- result_temp %>% rbind(c(var_total,"--",sum(.$n_valido),sum(.$rel.freq)))
          if (pretty == TRUE)
          {
            result_temp <- result_temp %>% rbind(rep(NA,nrow(result_temp)))
          }
      }
      

      #-------- add totals row 
      if (total == TRUE)
      {
          if (pretty == TRUE)
          {
          }
      }

      if (exists("result_df")) result_df <- rbind(result_df,result_temp)
      else result_df <- result_temp  
      
  }
  
  if (exists("result_df")) 
  {
    result_df <- as.data.frame(result_df)
    udaic_freq <- result_df

    class(udaic_freq) <- "udaic_freq"
    return(udaic_freq)
  }
}

print.udaic_freq <- function(x, ...) {

  cat("========================")
  

}