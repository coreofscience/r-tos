if (!require(bibliometrix)) {
  install.packages("bibliometrix")
}

if (!require(tidyverse)) {
  install.packages("tidyverse")
}

if (!require(igraph)) {
  install.packages("igraph")
}

# Read scopus bibtex
read_scopus_file <- function (scopus_file) {
  scopus_dataframe <- 
    convert2df(scopus_file,
               dbsource = "scopus",
               format = "bibtex") %>% 
    as_tibble() %>% 
    mutate(SR_TOS = str_c(SR,SO))
  
  return(scopus_dataframe)
}

# Create the edgelist

edge_list_scopus <- function (scopus_dataframe) {
  edge_list <-
    scopus_dataframe %>%
    separate_rows(CR, sep = "; ") %>%  # the CR data is removed here, something to improve strsplit could be an option
    mutate(lastname = sub("\\., .*", "", CR),
           lastname = sub(",", "", lastname),
           lastname = sub("\\.", "", lastname),# extracting lastnames,
           year = str_extract(CR, "\\(([0-9]{4})\\)"),
           year = str_remove_all(year, "\\(|\\)")) %>% 
    filter(!grepl(pattern = "[():[:digit:]]", lastname),
           str_length(year) == 4) %>% 
    mutate(CR_SO = str_match(CR, pattern = "\\([0-9]{4}\\)\\s*(.*?)\\s*,")[,2],
           CR_SO = str_c(lastname, ", ", year, ", ", CR_SO)) %>% 
    filter(CR_SO != "") %>% 
    mutate(TITLE = str_extract(CR, ".*\\(([0-9]{4})\\)"),
           TITLE = str_remove(TITLE, "^(.*)[\\.,]"),
           TITLE = str_remove(TITLE, "\\(([0-9]{4})\\)"),
           TITLE = str_trim(TITLE)) %>% 
    select(SR_TOS, CR_SO, TITLE) %>% 
    unique()
  
  step1 <- 
  step2 <- str_remove(step1, "")
  step3 <- str_remove(step2, "")
  step4 <- str_trim(step3)
    
    str_view(step1, )
  
  return(edge_list)
}

graph_scopus <- function (edge_list) {
  graph_raw <- 
    graph.data.frame(edge_list,
                     directed = TRUE) %>% 
    simplify() 
  
  graph_cleaned <-
    delete.vertices(graph_raw, 
                    which(degree(graph_raw,
                                 mode = "in") == 1 &
                          degree(graph_raw,
                                 mode = "out") == 0)
    )
  
  giant.component <- function(graph) {
    cl <- clusters(graph)
    induced.subgraph(graph, which(cl$membership == which.max(cl$csize)))
  }
  
  graph_gc <- giant.component(graph_cleaned)
  
  subareas <- 
    as.undirected(graph_gc,
                  mode = "each") %>% 
    cluster_louvain()
  
  graph <- 
    graph_gc %>% 
    set_vertex_attr(name = "sub_area",
                    value = membership(subareas))
  
  return(graph)
}

tos_labels <- function(graph) {
  network_metrics <- tibble(
    id = V(graph)$name,
    indegree = degree(graph, mode = "in"),
    outdegree = degree(graph, mode = "out"),
    bet = betweenness(graph)
  )
  
  roots <- 
    network_metrics %>% 
    filter(outdegree == 0) %>% 
    arrange(desc(indegree)) %>%
    head(10) %>% 
    mutate(tos = "raiz",
           order = 1:length(tos)) %>% 
    select(-indegree,
           -outdegree,
           -bet)
  
  trunk <- 
    network_metrics %>% 
    arrange(desc(bet)) %>% 
    head(10) %>% 
    mutate(tos = "tronco",
           order = 1:length(tos)) %>% 
    select(-indegree,
           -outdegree,
           -bet)
  
  leaves <- 
    network_metrics %>% 
    filter(indegree == 0) %>% 
    arrange(desc(indegree)) %>% 
    head(60) %>% 
    mutate(tos = "hoja",
           order = 1:length(tos)) %>% 
    select(-indegree,
           -outdegree,
           -bet)
  
  tos_structure <- 
    bind_rows(roots,
              trunk,
              leaves)
  
  return(tos_structure)
}

sub_area <- function (graph) {
  # Identify the first three clusters
  subareas_3 <- 
    tibble(
      subarea = V(graph)$sub_area) %>% 
    group_by(subarea) %>% 
    count() %>% 
    arrange(desc(n)) %>%  
    head(3) %>%
    select(subarea)
  
  graph_subarea_1 <- 
    graph %>% 
    delete_vertices(V(graph)$sub_area != subareas_3$subarea[1])
  
  sub_area_net_metrics_1 <-
    tibble(
     id = V(graph_subarea_1)$name,
     indegree = degree(graph_subarea_1,
                       mode = "in"),
     outdegree = degree(graph_subarea_1,
                        mode = "out"),
     bet = betweenness(graph_subarea_1)
    )

  graph_subarea_2 <- 
    graph %>% 
    delete_vertices(V(graph)$sub_area != subareas_3$subarea[2])
  
  sub_area_net_metrics_2 <-
    tibble(
      id = V(graph_subarea_2)$name,
      indegree = degree(graph_subarea_2,
                        mode = "in"),
      outdegree = degree(graph_subarea_2,
                         mode = "out"),
      bet = betweenness(graph_subarea_2)
    )
  
  graph_subarea_3 <- 
    graph %>% 
    delete_vertices(V(graph)$sub_area != subareas_3$subarea[3])
  
  sub_area_net_metrics_3 <-
    tibble(
      id = V(graph_subarea_3)$name,
      indegree = degree(graph_subarea_3,
                        mode = "in"),
      outdegree = degree(graph_subarea_3,
                         mode = "out"),
      bet = betweenness(graph_subarea_3)
    )
  
  subareas_plot <-
    tibble(subareas = V(graph)$sub_area) %>% 
    group_by(subareas) %>%
    count() %>%
    arrange(desc(n)) %>% 
    ggplot(aes(x = reorder(subareas, n), y = n)) + 
    geom_point() +
    xlab("subareas") +
    ylab("papers") +
    ggtitle("Relationship of subareas by size")
  
  list(subarea_1 = sub_area_net_metrics_1,
         subarea_2 = sub_area_net_metrics_2,
         subarea_3 = sub_area_net_metrics_3,
         tipping_poing = subareas_plot)
}
