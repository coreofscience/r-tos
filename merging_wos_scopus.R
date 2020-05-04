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
  
  edgelist_tos_wos_year <- 
    edgelist_tos_wos %>% 
    left_join(tos_wos %>% 
                select(ID_TOS, 
                       PY),
              by = c("SOURCE" = "ID_TOS"))
  
  edgelist_tos_scopus <- 
    as_tibble(biblio_scopus) %>% 
    mutate(ID_TOS = str_extract(SR, ".*,")) %>% 
    separate_rows(CR, sep = "; ") %>% 
    select(ID_TOS,
           CR,
           PY) %>% 
    mutate(lastname = sub("\\., .*", "", CR),
           lastname = sub(",", "", lastname),
           lastname = sub("\\.", "", lastname),# extracting lastnames,
           year = str_extract(CR, "\\(([0-9]{4})\\)"),
           year = str_remove_all(year, "\\(|\\)")) %>% 
    filter(!grepl(pattern = "[():[:digit:]]", lastname),
           str_length(year) == 4) %>% 
    mutate(CR = paste0(lastname, ", ", year, ",")) %>% 
    select(ID_TOS, CR, PY)
  
  edgelist_tos_scopus_year <- 
    edgelist_tos_scopus %>% 
    left_join(tos_wos %>% 
                select(ID_TOS, 
                       PY),
              by = c("SOURCE" = "ID_TOS"))
  
  edgelist_tos <- 
    bind_rows(edgelist_tos_wos, 
              edgelist_tos_scopus) %>% 
    distinct() %>% 
    na.omit()
  
  graph_tos <- 
    graph.data.frame(edgelist_tos,
                     directed = TRUE) %>% 
    simplify()
  
  giant.component <- function(graph) {
    cl <- clusters(graph)
    induced.subgraph(graph, which(cl$membership == which.max(cl$csize)))
  }
  
  graph_wos_scopus <- 
    delete.vertices(graph_tos, 
                    which(degree(graph_tos, mode = "in") == 1 & 
                            degree(graph_tos, mode = "out") == 0)) %>% 
    giant.component()
  
  graph_wos_scopus
  
}