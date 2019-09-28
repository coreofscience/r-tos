library("tidyverse")
library("bibliometrix")
library("igraph")

read_isi_file <- function(isi_file){
  text <- readFiles(isi_file)
  tos_dataframe <- convert2df(text, dbsource = "isi", format = "plaintext")
  tos_dataframe$IDWOS <- rownames(tos_dataframe)
  return(tos_dataframe)
}
