modularity <- function(graph_wos_scopus) {
  
  graph_tos_und = as.undirected(graph_wos_scopus,
                                mode = "each")
  sub_areas = cluster_louvain(graph_tos_und)
  graph_wos_scopus_sub_areas <- 
    graph_tos_und %>% 
    set_vertex_attr(name = "sub_area",
                    value = membership(sub_areas)
                    )
  graph_tos_sub_areas
}
