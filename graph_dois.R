graph_dois <- function(wos_graph, scopus_graph) {
  
  edgelist_wos_id <- 
    as_tibble(as_edgelist(wos_graph)) %>% 
    rename(source = "V1",
           target = "V2") %>% 
    mutate(id_tos_source = sub("^(\\S*\\s+\\S+\\s+\\S+).*", # removing strings after second comma 
                               "\\1", 
                               edgelist_wos$source),
           id_tos_target = sub("^(\\S*\\s+\\S+\\s+\\S+).*", # removing strings after second comma 
                               "\\1", 
                               edgelist_wos$target)) 
  
  list_wos_doi <- 
    edgelist_wos_id %>%
    set_names("source_target",
              "source_target",
              "id_tos_source_target",
              "id_tos_source_target") 
  
  wos_source <- list_wos_doi[,c(1,3)]
  wos_target <- list_wos_doi[,c(2,4)]
  wos_source_target <- 
    bind_rows(wos_source,
              wos_target) %>% 
    distinct()
  
  
  
}