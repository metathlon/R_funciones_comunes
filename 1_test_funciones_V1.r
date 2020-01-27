
rm(list=ls())
#--------------------------------- EJEMPLO DOCUMENTACION

edad_ <- sample(x = 65:100, size=30, replace = TRUE )
estatura_ <- as.integer(sample(x = 120:205, size=30, replace = TRUE ))
sexo_ <- sample(x = c("Hombre", "Mujer"), prob = c(.5,.5), size = 30, replace = TRUE)
rubio_ <- sample(x = c("Si", "No"), prob = c(.2,.8), size = 30, replace = TRUE)
data_ <- data.frame(EDAD=edad_,ESTATURA=estatura_,SEXO=sexo_,RUBIO=rubio_)

source("funciones_comunes/freq.R")

freq(data_,SEXO,RUBIO)
freq(data_,SEXO,RUBIO, decimales = 3)
freq(data_,SEXO,RUBIO, decimales = 3, sort_by_percent = TRUE)

load(file="DATOS/DF_ANALIZAR.RDAta")

source("funciones_comunes/freq.R")

library(knitr)


f <- freq(df,SEXO, DIAGNOSTICO, sort_by_percent = TRUE)
f

kable(f)

d <- f[[2]]
