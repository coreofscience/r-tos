readISI <- function(file) {
  
  M <- convert2df(file, dbsource = "isi", format = "plaintext")
  
}

readScopus <- function(file) {
  
  M <- convert2df(file, dbsource = "scopus", format = "bibtex")
  M
}