modularity <- function(graph_tos) {
  
  graph_tos_und = as.undirected(graph_tos,
                                mode = "each")
  sub_areas = cluster_louvain(graph_tos_scopus_undirected)
  graph_tos_sub_areas <- 
    graph_tos_und %>% 
    set_vertex_attr(name = "sub_area",
                    value = membership(sub_areas)
                    )
  graph_tos_sub_areas
}
