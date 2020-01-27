#==================================================================================
# Funcion para quitar los valores perdidos de las variables tipo FACTOR
# ---------------------------------------------------------------------------------
# Resulta que si dejas valores perdidos como factor en ocasiones R intenta operarlos
# en general no es un problema pero si queremos hacer grupos con dplyr
# o alguna cosa que requiera tener en cuenta los factores (ANOVAS, etc)
# tener un nivel perdido puede ser un problema
#
# ----------------------------------------------------------------------------------
#  df             = data.frame
#  na_label       = etiqueta para el level NA por defecto "---"
#==================================================================================
# EJEMPLO DE USO
# -------------------
# # Generamos datos aleatorios
# edad_ <- sample(x = 65:100, size=30, replace = TRUE )
# sexo_ <- sample(x = c("Hombre", "Mujer"), prob = c(.5,.5), size = 30, replace = TRUE)
# sexo_[sample(x = c(TRUE, FALSE), prob = c(.2,.8), size = 30, replace = TRUE)] <- NA
# data_ <- as.data.frame(cbind(edad_,sexo_))
#
# # Comprobamos que existen NA
# summary(data_$sexo_)
#
# #aplicamos la funcion
# data_ <- explicit_missing(data_)
#
# # Comprobamos que en vez de NA ahora hay un tercer nivel denominado ---
# summary(data_$sexo_)
#
#==================================================================================
explicit_missing <- function(df, na_label = "---")
{
  require("dplyr")
  require("forcats")
  #------------------------------------- CONVERESION EXPLICITA DE NA a MISSING
  df = df %>% mutate_if(is.factor,
                        fct_explicit_na,
                        na_level = na_label)
  return(df)
}




