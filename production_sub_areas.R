production_sub_area <- function(graph){
  # library(igraph)
  # library(tidyverse)
  # library(tidyr)
  # library(bibliometrix)
  # library(ggplot2)
  
  
  sub_areas <- cluster_louvain(as.undirected(graph, mode = "each"), weights = NULL)
  graph_1 <- 
    graph %>%
    set_vertex_attr(name = "sub_area",
                    value = membership(sub_areas))
  df_graph <- 
    data.frame(vertices = V(graph_1)$name, 
               sub_area = V(graph_1)$sub_area,
               stringsAsFactors = TRUE) %>% 
    arrange(desc(sub_area))
  df_graph$year <- str_extract(df_graph$vertices, "[0-9]+")
  df_graph <- na.omit(df_graph)
  
  
  all_data <- 
    data.frame(Year = c(min(unique(df_graph$year)): max(unique(df_graph$year))),
               stringsAsFactors = FALSE)
  names_all_data <- list("Year")
  
  for (i in 1:length(sub_areas)){
    frecuency_sub_area <-
      df_graph %>% 
      filter(sub_area == i) %>% 
      count(year)
    names(frecuency_sub_area) <- c("Año", "Frecuencia")
    
    frec_sub_area <- ifelse(all_data$Year %in% frecuency_sub_area$Año,
                            frecuency_sub_area$Frecuencia, 0)
    all_data <- cbind(all_data, frec_sub_area)
    
    names_all_data <- append(names_all_data,
                             paste("production_sub_area", as.character(i), sep = ""))
  }
  names(all_data) <- names_all_data
  
  all_data_largo <- all_data %>% 
    gather(key="sub_areas", value="frecuencia", 2:length(sub_areas))
  
  all_data_largo
}