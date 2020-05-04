wos_scopus <- function(wos_graph, scopus_graph) {
  
  edgelist_tos_wos <- 
    tos_wos %>% 
    select(ID_TOS, 
           CR_NESTED) %>% 
    unnest(CR_NESTED) %>% 
    mutate(TARGET = sub("^(\\S*\\s+\\S+\\s+\\S+).*", # removing strings after second comma 
                        "\\1",
                        CR)) %>% 
    select(-CR) %>% 
    rename(SOURCE = "ID_TOS")
  
  edgelist_tos_scopus <- 
    tos_scopus %>% 
    select(ID_TOS,
           REFS_NESTED) %>% 
    unnest(REFS_NESTED) %>% 
    mutate(TARGET = )
    
  
  #####
  
  edgelist_wos_tos <- 
    as_tibble(as_edgelist(wos_graph)) %>% 
    rename(source = "V1",
           target = "V2") %>% 
    mutate(id_tos_source = sub("^(\\S*\\s+\\S+\\s+\\S+).*", # removing strings after second comma 
                               "\\1", 
                               edgelist_wos$source),
           id_tos_target = sub("^(\\S*\\s+\\S+\\s+\\S+).*", # removing strings after second comma 
                               "\\1", 
                               edgelist_wos$target)) %>% 
    select(id_tos_source, 
           id_tos_target)
   
  
  edgelist_scopus_tos <- 
    as_tibble(as_edgelist(scopus_graph)) %>% 
    rename(id_tos_source = "V1",
           id_tos_target = "V2") 
  
  edgelist_tos <- 
    bind_rows(edgelist_wos_tos, 
              edgelist_scopus_tos) %>% 
    distinct()
  
  graph_tos <- 
    graph.data.frame(edgelist_tos, 
                                directed = TRUE) %>% 
    simplify()
  
  graph_tos_scopus <- 
    delete.vertices(graph_tos, 
                    which(degree(graph_tos, mode = "in") == 1 & 
                            degree(graph_tos, mode = "out") == 0)) %>% 
    giant.component()
  
  graph_tos_scopus
  
}