library("tidyverse")
library("bibliometrix")
library("igraph")

read_isi_file <- function(isi_file){
  text <- readFiles(isi_file)
  tos_dataframe <- convert2df(text, dbsource = "isi", format = "plaintext")
  tos_dataframe$IDWOS <- rownames(tos_dataframe)
  return(tos_dataframe)
}

split_references <- function(tos_dataframe, separator){
  dataframe_splitted  <- tos_dataframe %>% separate_rows(CR, sep = separator)
  return(dataframe_splitted)
}


format_dataframe <- function(tos_dataframe) {
  dataframe_formated <-  tos_dataframe %>%
    mutate(IDWOS2 = paste(IDWOS, sep = ", ",
                          paste("V", sep = "", VL),
                          paste("P", sep = "", BP),
                          paste("DOI ", sep = "", DI)
    )
    )
  return(dataframe_formated)
}

graph_compute <- function (tos_dataframe){
  edges <- dataframe_formated[!is.na(dataframe_formated$CR), c("IDWOS2", "CR")]
  tos_graph <- graph.data.frame(edges, directed = TRUE)
  return(tos_graph)
}


