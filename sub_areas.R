modularity <- function(graph_tos_scopus) {
  
  graph_tos_scopus_undirected = as.undirected(graph_tos_scopus, 
                                              mode = "each")
  sub_areas = cluster_louvain(graph_tos_scopus_undirected)
  graph_tos_scopus_und_subareas <- 
    graph_tos_scopus_undirected %>% set_vertex_attr(name = "sub_area",
                                                    value = membership(sub_areas)
                                                    )
  graph_tos_scopus_und_subareas
}

