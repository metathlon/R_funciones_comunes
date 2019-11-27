
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
require("dplyr")

udaic_freq <- function(df, var, total=TRUE, decimales=2)
{
  v <- enquo(var)
  print(decimales)
  print(total)
  result_temp <- df %>% group_by(!! v) %>%
                     summarise(
                                n = n(),
                               ) %>%
                     mutate(rel.freq = round((n/sum(n))*100,decimales))
  #-------- add the column for Values within the variable
  colnames(result_temp)[1] <- "Values"

  #-------- add the column with the variable NAME
  var_column = rep(quo_name(v),nrow(result_temp))
  result_temp <- mutate(result_temp,Variable=var_column)


  #-- take Variable_column to the front
  result_temp <- result_temp %>% select(Variable, everything())

  #-------- add totals row
  if (total == TRUE)
  {
      var_total <- paste(quo_name(v),"TOTAL")
      levels(result_temp$Values) <- c(levels(result_temp$Values),"--")
      result_temp <- result_temp %>% rbind(c(var_total,"--",sum(.$n),sum(.$rel.freq)))
  }


  class(result_temp) <- "udaic.freq"
  return(result_temp)
}




freq <- function(df,..., group_by_col = NULL, pretty=TRUE, decimales=2, show_warnings = TRUE, n=TRUE, total=TRUE) {

  vars <- enquos(...)
  
  result_df <- list()
 
  for (v in vars)
  {
      result_df[[length(result_df)+1]] <- udaic_freq(df,!! v,total=total, decimales=decimales)

  }

  if (exists("result_df"))
  {
    #udaic_freq <- result_df

    class(result_df) <- "udaic_freq"

    return(result_df)
  }
}



print.udaic_freq <- function(x, ...) {

  
  max_char <- c(0,0,0,0)

  max_char[1] <- nchar("Variable")
  max_char[2] <- nchar("Values")
  max_char[3] <- nchar("n")
  max_char[4] <- nchar("rel.freq")

  i = 1
  len_tot <- length(x)
  while(i <= len_tot)
  {
    temp <- data.frame(Variable=x[[i]]$Variable, Values=x[[i]]$Values, n=x[[i]]$n, rel.freq=x[[i]]$rel.freq)  
    
    j = 1
    j_total = nrow(temp)
    while(j <= j_total)
    {
      if(max_char[1] < nchar(as.character(temp$Variable[j]))) max_char[1] <- nchar(as.character(temp$Variable[j]))
      if(max_char[2] < nchar(as.character(temp$Values[j]))) max_char[2] <- nchar(as.character(temp$Values[j]))
      if(max_char[3] < nchar(as.character(temp$n[j]))) max_char[3] <- nchar(as.character(temp$n[j]))
      if(max_char[4] < nchar(as.character(temp$rel.freq[j]))) max_char[4] <- nchar(as.character(temp$rel.freq[j]))
      j <- j +1
    }
    i <- i+1
  }

  max_char[1] <- max_char[1] + 2
  max_char[2] <- max_char[2] + 2
  max_char[3] <- max_char[3] + 2
  max_char[4] <- max_char[4] + 2

  #print(max_char)
  #print(sum(max_char))
  #cat("========================\n")

  i = 1
  len_tot <- length(x)

  while(i <= len_tot)
  {
    temp <- data.frame(Variable=x[[i]]$Variable, Values=x[[i]]$Values, n=x[[i]]$n, rel.freq=x[[i]]$rel.freq)  

    if (i==1)
    {
      m_1 <- max_char[1] - nchar("Variable")
      m_2 <- max_char[2] - nchar("Values")
      m_3 <- max_char[3] - nchar("n")
      m_4 <- max_char[4] - nchar("rel.freq")

      cat("|",rep("",m_1),"Variable")
      cat("|",rep("",m_2),"Values")
      cat("|",rep("",m_3),"n")
      cat("|",rep("",m_4),"rel.freq","|")
      cat("\n") 
      cat("|",paste(rep("─",max_char[1]), collapse=""))
      cat("|",paste(rep("─",max_char[2]), collapse=""))
      cat("|",paste(rep("─",max_char[3]), collapse=""))
      cat("|",paste(rep("─",max_char[4]), collapse=""),"|")
      cat("\n") 
    
    }
    else
    {
      cat("|",paste(rep("─",max_char[1]), collapse=""))
      cat("|",paste(rep("─",max_char[2]), collapse=""))
      cat("|",paste(rep("─",max_char[3]), collapse=""))
      cat("|",paste(rep("─",max_char[4]), collapse=""),"|")
      cat("\n") 
      #names(temp) <- NULL #quitamos los nombres de las columnas
    }

    #print(temp)  

    
    j = 1
    j_total = nrow(temp)
    while(j <= j_total)
    {
      m_1 <- max_char[1] - nchar(as.character(temp$Variable[j]))
      m_2 <- max_char[2] - nchar(as.character(temp$Values[j]))
      m_3 <- max_char[3] - nchar(as.character(temp$n[j]))
      m_4 <- max_char[4] - nchar(as.character(temp$rel.freq[j])) - 1 # el simbolo %

      #cat(m_1, m_2, m_3, m_4)
      #cat("\n")
      cat("|",rep("",m_1),trimws(as.character(temp$Variable[j])))
      cat("|",rep("",m_2),trimws(as.character(temp$Values[j])))
      cat("|",rep("",m_3),trimws(as.character(temp$n[j])))
      cat("|",rep("",m_4),paste(as.character(temp$rel.freq[j]),"%",sep=""),"|")
      cat("\n")
      j <- j +1
    }

    i <- i+1
  }
  
}


freq_old <- function(df,..., group_by_col = NULL, pretty=TRUE, decimales=2, show_warnings = TRUE, n=TRUE, total=TRUE) {

  vars <- enquos(...)
  
  for (v in vars)
  {

      result_temp <- df %>% group_by(!! v) %>%
                           summarise(
                                      n = n(),
                                     ) %>%
                           mutate(rel.freq = (n/sum(n))*100)


      #-------- add the column for Values within the variable
      #result_temp <- as.data.frame(result_temp)
      colnames(result_temp)[1] <- "Values"

      #-------- add the column with the variable NAME
      var_column = rep(quo_name(v),nrow(result_temp))
      result_temp <- mutate(result_temp,Variable=var_column)

      #-- take Variable_column to the front
      result_temp <- result_temp %>% select(Variable, everything())

      #-------- add totals row
      if (total == TRUE)
      {
          var_total <- paste(quo_name(v),"TOTAL")
          levels(result_temp$Values) <- c(levels(result_temp$Values),"--")
          result_temp <- result_temp %>% rbind(c(var_total,"--",sum(.$n),sum(.$rel.freq)))
      }
      row.names(result_temp)

      if (exists("result_df")) result_df <- rbind(result_df,result_temp)
      else result_df <- result_temp

  }

  if (exists("result_df"))
  {
    udaic_freq <- result_df

    class(udaic_freq) <- "udaic_freq"
    return(udaic_freq)
  }
}
