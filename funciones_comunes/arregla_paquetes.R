# -- FUNCTION
findBrokenPackages <- function(packages) {
  for (p in packages) {
    tryCatch(ncol(asNamespace(p)$.__NAMESPACE__.$S3methods),
             error = function(e) print(c(p, e)))
  }
}

# -- Lista de los paquetes
allPackages <- installed.packages()[, 1]

# -- usamos la funcion
findBrokenPackages(allPackages)
