
#==============================================================================================================
# Funcion para hacer tablas de medias
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
medias <- function(df,variables, group_by_col = NULL, decimales=2, show_warnings = TRUE, n=TRUE, missing=TRUE, min=TRUE, max= TRUE, mean=TRUE, sd=TRUE, median=TRUE, range=TRUE) {
  require("dplyr")
  require("forcats")

  if (!is.null(group_by_col))
  {
    #------------------------------------- CONVERESION EXPLICITA DE NA a MISSING
    df = df %>% mutate_if(is.factor,
                          fct_explicit_na,
                          na_level = "---")
    
    df = df %>% group_by_at(group_by_col)
  }

  for (v in variables) {
    real_v = df[,substitute(v)]
    if (length(real_v) > 0 & !is.factor(real_v))
    {
        result_temp <- df %>% summarise(
                                 n = n() - sum(is.na(.data[[v]])),
                                 missing = sum(is.na(.data[[v]])),
                                 min = round(min(.data[[v]], na.rm=TRUE), digits=decimales),
                                 max = round(max(.data[[v]], na.rm=TRUE), digits=decimales),
                                 mean = round(mean(.data[[v]], na.rm = TRUE), digits=decimales),
                                 sd = round(sd(.data[[v]], na.rm = TRUE), digits=decimales),
                                 median = round(median(.data[[v]], na.rm = TRUE), digits=decimales),
                                 range = round(max(.data[[v]], na.rm = TRUE) - min(.data[[v]], na.rm = TRUE), digits=decimales)
                         )
        result_temp <- as.data.frame(result_temp)
        result_temp <- cbind(c(v,rep("",nrow(result_temp)-1)),result_temp)
        colnames(result_temp)[1] <- "VAR"
        if (!is.null(group_by_col))
        {
          colnames(result_temp)[2] <- group_by_col
        }

        if (n == FALSE) result_temp$n <- NULL
        if (missing == FALSE) result_temp$missing <- NULL
        if (min == FALSE) result_temp$min <- NULL
        if (max == FALSE) result_temp$max <- NULL
        if (mean == FALSE) result_temp$mean <- NULL
        if (sd == FALSE) result_temp$sd <- NULL
        if (median == FALSE) result_temp$median <- NULL
        if (range == FALSE) result_temp$range <- NULL
        


      if (exists("result_df")) result_df <- rbind(result_df,result_temp)
      else result_df <- result_temp  
    }
    else {
      warning(paste(v, "no es una variable adecuada para hacer medias ¿Quizás es un factor o esta vacía?"), call. = show_warnings)
    }
    
  }
  if (exists("result_df")) return(result_df)
}
