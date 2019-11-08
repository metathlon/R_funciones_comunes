#==================================================================================
# Funcion para hacer tablas de frecuencias
#  df             = data.frame
#  variables      = una o varias variables c("aSDF","fds")
#  nombre.vars    = TRUE/FALSE para si quieres el nombre de la variable como fila
#==================================================================================

freq <- function(df, variables, decimales=2, nombre.vars = TRUE) {
  
  # vars = substitute(variables)
  
  
  for (v in variables) {
    v.table = table(df[,substitute(v)])
    v.df <- cbind(rownames(v.table),v.table, paste(round(prop.table(v.table) * 100, digits=decimales),"%", sep=""))
    
    if (nombre.vars){
      v.df <- cbind("",v.df)
      v.df <- rbind(c(v,"","",""),v.df)
    } 
    
    rownames(v.df) <- NULL
    if (!exists("freq.table")) freq.table <- v.df
    else freq.table = rbind(freq.table, v.df)
    
  }
  
  
  if (exists("freq.table")) {
    if (nombre.vars) colnames(freq.table) <- c("Variable","Categorias","n", "%")
    else colnames(freq.table) <- c("Categorias","n", "%")
    return(freq.table)
  } 
  else function(e) print("No se ha podido formar tabla de resultados")
}