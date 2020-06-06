source("scripts/scopus_functions.R")

tos_scopus <- function(fileinput) {
  # Read scopus bitex file  
  scopus_dataframe <- read_scopus_file(fileinput)
  # Create the edgelist
  edge_list <- edge_list_scopus(scopus_dataframe)
  # ToS labels 
  titles <- titles_scopus(scopus_dataframe, edge_list)
  # Create graph
  graph <- graph_scopus(edge_list)
  # Create ToS table
  tos_structure <- tos_labels(graph, titles)
  # Calculate subareas
  graph_subareas <- sub_area(graph)
  # Importance of the topic 
  importance <- importance_bibliometrix(scopus_dataframe)
  
  list(scopus = scopus_dataframe,
       network = graph,
       tos = tos_structure,
       subareas = graph_subareas,
       pccion_anual = importance)
  
}