require(stringr) # -- para regex
busca <- function(data, string="", show.col.number= FALSE) {
  if (!show.col.number) return(names(data)[grep(regex(string),names(data), ignore.case=TRUE)]  )
  return(grep(regex(string),names(data), ignore.case=TRUE)) 
}

