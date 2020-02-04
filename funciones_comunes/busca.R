require(stringr) # -- para regex
busca <- function(data, string="") {
  return(names(data)[grep(regex(string),names(data), ignore.case=TRUE)]  )
}

