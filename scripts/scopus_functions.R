if (!require(bibliometrix)) {
  install.packages("bibliometrix")
}

if (!require(tidyverse)) {
  install.packages("tidyverse")
}

if (!require(igraph)) {
  install.packages("igraph")
}

if (!require(lubridate)) {
  install.packages("lubridate")
}

if (!require(tm)) {
  install.packages("tm")
}

if (!require(wordcloud)) {
  install.packages("wordcloud")
}

if (!require(gt)) {
  install.packages("gt")
}

if (!require(rebus)) {
  install.packages("rebus")
}

# Read scopus bibtex
read_scopus_file <- function (scopus_file) {
  scopus_dataframe <- 
    convert2df(scopus_file,
               dbsource = "scopus",
               format = "bibtex") %>% 
    as_tibble() %>% 
    mutate(SR_TOS = str_extract(SR, one_or_more(WRD) %R%
                                 SPC %R% one_or_more(WRD) %R%
                                 "," %R% SPC %R% 
                                 one_or_more(DGT) %R% ","),
           SR_TOS = str_c(SR_TOS, " ", SO))
  
  return(scopus_dataframe)
}

# Create a df with CRs

cited_references_df <- function(scopus_dataframe) {
  
  pattern_authors <- 
    SPC %R% 
    one_or_more(WRD) %R%
    SPC %R%
    one_or_more(or(WRD, ANY_CHAR))
  
  pattern_titles <- 
    OPEN_PAREN %R% 
    repeated(DGT, 4) %R% 
    CLOSE_PAREN %R%
    one_or_more(or(WRD,ANY_CHAR))
  
  pattern_year <- 
    OPEN_PAREN %R% 
    repeated(DGT, 4) %R% 
    CLOSE_PAREN 
  
  pattern_journal <- 
    one_or_more(or(WRD,SPC))
  
  pattern_volume <-
    one_or_more(or(WRD, SPC))
  
  pattern_pages <- 
    "PP. " %R%
    one_or_more(or(DGT, ANY_CHAR))
  
  cited_references <- 
    scopus_dataframe %>%
    separate_rows(CR, sep = "; ") %>% 
    select(SR_TOS, 
           CR) %>% 
    mutate(CR_AUTHOR = str_remove(CR, pattern_authors),
           CR_TITLE_1 = str_extract(CR, pattern_authors),
           CR_TITLE = str_remove(CR_TITLE_1, pattern_titles),
           CR_TITLE = str_trim(CR_TITLE),
           CR_YEAR_1 <- str_extract(CR_TITLE_1, pattern_titles),
           CR_YEAR = str_extract(CR_YEAR_1, repeated(DGT, 4)),
           CR_JOURNAL_1 = str_remove(CR_YEAR_1, pattern_year),
           CR_JOURNAL = str_extract(CR_JOURNAL_1, pattern_journal),
           CR_JOURNAL = str_trim(CR_JOURNAL),
           CR_VOLUME_1 = str_remove(CR_JOURNAL_1, pattern_journal),
           CR_VOLUME = str_extract(CR_VOLUME_1, pattern_volume),
           CR_PAGES = str_extract(CR_VOLUME_1, pattern_pages),
           CR_PAGES = str_remove(CR_PAGES, "PP. ")) %>% 
    select(SR_TOS, 
           CR, 
           CR_AUTHOR, 
           CR_TITLE, 
           CR_YEAR, 
           CR_JOURNAL, 
           CR_VOLUME, 
           CR_PAGES) %>% 
    mutate(lastname = sub("\\., .*", "", CR),
           lastname = sub(",", "", lastname),
           lastname = sub("\\.", "", lastname),
           CR_SO = str_c(lastname, 
                         ", ",
                         CR_YEAR,
                         ", ",
                         CR_JOURNAL)) %>% 
    select(-lastname)
  
  return(cited_references)

}

# Create the edgelist

edge_list_scopus <- function (scopus_dataframe) {
 
  edge_list <-
    cited_references %>% 
    select(SR_TOS, 
           CR_SO) %>% 
    na.omit()
  
  return(edge_list)
}

titles_scopus <- function (scopus_dataframe, edge_list) {
  titles_orig <- 
    scopus_dataframe %>% 
    select(SR_TOS, TI)
  
  titles <- 
    edge_list %>% 
    select(CR_SO,
           TITLE) %>%
    anti_join(titles_orig,
              by = c("CR_SO" = "SR_TOS")) %>% 
    rename(SR_TOS = "CR_SO",
           TI = "TITLE") %>% 
    bind_rows(titles_orig)
  
  return(titles)
    
}
  
  
graph_scopus <- function (cited_references) {
  
  edge_list <-
    cited_references %>% 
    select(SR_TOS, 
           CR_SO) %>% 
    na.omit()
  
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

tos_labels <- function(graph, cited_references, scopus_dataframe) {
  
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
    arrange(desc(outdegree)) %>% 
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
  
  tos_scopus_df <- 
    tos_structure %>% 
    left_join(scopus_dataframe %>% 
                select(SR_TOS,
                       TI,
                       PY,
                       AU,
                       SO),
              by = c("id" = "SR_TOS")) %>% 
    rename(TITLE = "TI",
           YEAR = "PY",
           AUTHOR = "AU",
           JOURNAL = "SO")
  
  tos_cited_ref <-
    tos_scopus_df %>% 
    filter(is.na(TITLE)) %>%
    select(id,
           tos,
           order) %>% 
    left_join(cited_references %>% 
                select(CR_SO,
                       CR_TITLE,
                       CR_YEAR,
                       CR_AUTHOR,
                       CR_JOURNAL),
              by = c("id" = "CR_SO")) %>% 
    filter(!duplicated(id)) %>% 
    rename(TITLE = "CR_TITLE",
           YEAR = "CR_YEAR",
           AUTHOR = "CR_AUTHOR",
           JOURNAL = "CR_JOURNAL")
  
  tos_structure <- 
    rbind(tos_cited_ref,
          tos_scopus_df %>%
            filter(!is.na(TITLE))) %>% 
    select(order,
           tos,
           TITLE,
           YEAR,
           AUTHOR,
           JOURNAL,
           id)
  
  return(tos_structure)
}

sub_area <- function (graph, cited_references, scopus_dataframe) {
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

  roots_1 <- 
    sub_area_net_metrics_1 %>% 
    filter(outdegree == 0) %>% 
    arrange(desc(indegree)) %>%
    head(10) %>% 
    mutate(tos = "raiz",
           order = 1:length(tos)) %>% 
    select(-indegree,
           -outdegree,
           -bet)
  
  trunk_1 <- 
    sub_area_net_metrics_1 %>% 
    arrange(desc(bet)) %>% 
    head(10) %>% 
    mutate(tos = "tronco",
           order = 1:length(tos)) %>% 
    select(-indegree,
           -outdegree,
           -bet)
  
  leaves_1 <- 
    sub_area_net_metrics_1 %>% 
    filter(indegree == 0) %>% 
    arrange(desc(indegree)) %>% 
    head(60) %>% 
    mutate(tos = "hoja",
           order = 1:length(tos)) %>% 
    select(-indegree,
           -outdegree,
           -bet)
  
  tos_scopus_subarea_1 <- 
    bind_rows(roots_1,
              trunk_1,
              leaves_1) %>% 
    left_join(scopus_dataframe %>% 
                select(SR_TOS,
                       TI,
                       PY,
                       AU,
                       SO),
              by = c("id" = "SR_TOS")) %>% 
    rename(TITLE = "TI",
           YEAR = "PY",
           AUTHOR = "AU",
           JOURNAL = "SO")
  
  tos_cited_ref_subarea_1 <-
    tos_scopus_subarea_1 %>% 
    filter(is.na(TITLE)) %>%
    select(id,
           tos,
           order) %>% 
    left_join(cited_references %>% 
                select(CR_SO,
                       CR_TITLE,
                       CR_YEAR,
                       CR_AUTHOR,
                       CR_JOURNAL),
              by = c("id" = "CR_SO")) %>% 
    filter(!duplicated(id)) %>% 
    rename(TITLE = "CR_TITLE",
           YEAR = "CR_YEAR",
           AUTHOR = "CR_AUTHOR",
           JOURNAL = "CR_JOURNAL")
  
  tos_structure_1 <- 
    rbind(tos_cited_ref_subarea_1,
          tos_scopus_subarea_1 %>%
            filter(!is.na(TITLE))) %>% 
    select(order,
           tos,
           TITLE,
           YEAR,
           AUTHOR,
           JOURNAL,
           id)
  
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
  
  roots_2 <- 
    sub_area_net_metrics_2 %>% 
    filter(outdegree == 0) %>% 
    arrange(desc(indegree)) %>%
    head(10) %>% 
    mutate(tos = "raiz",
           order = 1:length(tos)) %>% 
    select(-indegree,
           -outdegree,
           -bet)
  
  trunk_2 <- 
    sub_area_net_metrics_2 %>% 
    arrange(desc(bet)) %>% 
    head(10) %>% 
    mutate(tos = "tronco",
           order = 1:length(tos)) %>% 
    select(-indegree,
           -outdegree,
           -bet)
  
  leaves_2 <- 
    sub_area_net_metrics_2 %>% 
    filter(indegree == 0) %>% 
    arrange(desc(indegree)) %>% 
    head(60) %>% 
    mutate(tos = "hoja",
           order = 1:length(tos)) %>% 
    select(-indegree,
           -outdegree,
           -bet)
  #### Subarea 2 ####
  
  tos_scopus_subarea_2 <- 
    bind_rows(roots_2,
              trunk_2,
              leaves_2) %>% 
    left_join(scopus_dataframe %>% 
                select(SR_TOS,
                       TI,
                       PY,
                       AU,
                       SO),
              by = c("id" = "SR_TOS")) %>% 
    rename(TITLE = "TI",
           YEAR = "PY",
           AUTHOR = "AU",
           JOURNAL = "SO")
  
  tos_cited_ref_subarea_2 <-
    tos_scopus_subarea_2 %>% 
    filter(is.na(TITLE)) %>%
    select(id,
           tos,
           order) %>% 
    left_join(cited_references %>% 
                select(CR_SO,
                       CR_TITLE,
                       CR_YEAR,
                       CR_AUTHOR,
                       CR_JOURNAL),
              by = c("id" = "CR_SO")) %>% 
    filter(!duplicated(id)) %>% 
    rename(TITLE = "CR_TITLE",
           YEAR = "CR_YEAR",
           AUTHOR = "CR_AUTHOR",
           JOURNAL = "CR_JOURNAL")
  
  tos_structure_2 <- 
    rbind(tos_cited_ref_subarea_2,
          tos_scopus_subarea_2 %>%
            filter(!is.na(TITLE))) %>% 
    select(order,
           tos,
           TITLE,
           YEAR,
           AUTHOR,
           JOURNAL,
           id)
  
  #### Subarea 3 ####
  
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
  
  roots_3 <- 
    sub_area_net_metrics_3 %>% 
    filter(outdegree == 0) %>% 
    arrange(desc(indegree)) %>%
    head(10) %>% 
    mutate(tos = "raiz",
           order = 1:length(tos)) %>% 
    select(-indegree,
           -outdegree,
           -bet)
  
  trunk_3 <- 
    sub_area_net_metrics_3 %>% 
    arrange(desc(bet)) %>% 
    head(10) %>% 
    mutate(tos = "tronco",
           order = 1:length(tos)) %>% 
    select(-indegree,
           -outdegree,
           -bet)
  
  leaves_3 <- 
    sub_area_net_metrics_3 %>% 
    filter(indegree == 0) %>% 
    arrange(desc(indegree)) %>% 
    head(60) %>% 
    mutate(tos = "hoja",
           order = 1:length(tos)) %>% 
    select(-indegree,
           -outdegree,
           -bet)
  
  tos_scopus_subarea_3 <- 
    bind_rows(roots_3,
              trunk_3,
              leaves_3) %>% 
    left_join(scopus_dataframe %>% 
                select(SR_TOS,
                       TI,
                       PY,
                       AU,
                       SO),
              by = c("id" = "SR_TOS")) %>% 
    rename(TITLE = "TI",
           YEAR = "PY",
           AUTHOR = "AU",
           JOURNAL = "SO")
  
  tos_cited_ref_subarea_3 <-
    tos_scopus_subarea_3 %>% 
    filter(is.na(TITLE)) %>%
    select(id,
           tos,
           order) %>% 
    left_join(cited_references %>% 
                select(CR_SO,
                       CR_TITLE,
                       CR_YEAR,
                       CR_AUTHOR,
                       CR_JOURNAL),
              by = c("id" = "CR_SO")) %>% 
    filter(!duplicated(id)) %>% 
    rename(TITLE = "CR_TITLE",
           YEAR = "CR_YEAR",
           AUTHOR = "CR_AUTHOR",
           JOURNAL = "CR_JOURNAL")
  
  tos_structure_3 <- 
    rbind(tos_cited_ref_subarea_3,
          tos_scopus_subarea_3 %>%
            filter(!is.na(TITLE))) %>% 
    select(order,
           tos,
           TITLE,
           YEAR,
           AUTHOR,
           JOURNAL,
           id)
  
  #### Plotting subgraphs ####
  
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
  
  graph_subareas_plot <- 
    delete.vertices(graph,
                    which(V(graph)$sub_area != subareas_3$subarea[1] &
                            V(graph)$sub_area != subareas_3$subarea[2] &
                            V(graph)$sub_area != subareas_3$subarea[3]))
   
    colr = c("red", "brown", "blue")
  V(graph_subareas_plot)$color <- ifelse(V(graph_subareas_plot)$sub_area == subareas_3$subarea[1], "red",
                                         ifelse(V(graph_subareas_plot)$sub_area == subareas_3$subarea[2], "blue", 
                                                "green"))
  
  list(subarea_1 = tos_structure_1,
         subarea_2 = tos_structure_2,
         subarea_3 = tos_structure_3,
         tipping_poing = subareas_plot,
       graph_subareas_network = graph_subareas_plot)
}

importance_bibliometrix <- function (scopus_dataframe) {
  importance_biblio <- 
    biblioAnalysis(scopus_dataframe)
  
  anual_pccion_plot <- 
    tibble(years = importance_biblio$Years,
           papers = importance_biblio$nAUperPaper) %>% 
    group_by(years) %>% 
    summarise(papers = sum(papers)) %>% 
    arrange(desc(years)) %>% 
    filter(years > 2001,
           years < year(Sys.Date())) %>% 
    ggplot(aes(x = years, y = papers)) +
    geom_line() + 
    ggtitle("Scientific anual production")
  
  author_pccion <- 
    as_tibble(importance_biblio$Authors) %>% 
    head(15) %>% 
    rename(Author = "AU",
           Publications = "n")
  
  journals_pccion <- 
    as_tibble(importance_biblio$Sources) %>% 
    head(15)
  
  list(anual_pccion = anual_pccion_plot,
       author_pccion = author_pccion,
       journals_pccion = journals_pccion)
}

wordclouds <- function (subarea_1, subarea_2, subarea_3) {
  
  jeopCorpus <- Corpus(VectorSource(subarea_1$TITLE %>% na.omit()))
  
  paperCorp <- jeopCorpus
  paperCorp <- tm_map(paperCorp, removePunctuation)
  paperCorp <- tm_map(paperCorp, removeNumbers)
  # added tolower
  paperCorp <- tm_map(paperCorp, content_transformer(tolower))
  paperCorp <- tm_map(paperCorp, removeWords, stopwords("english"))
  # moved stripWhitespace
  
  paperCorp <- tm_map(paperCorp, stripWhitespace)
  paperCorp <- tm_map(paperCorp, stemDocument)
  
  paperCorp_1 <- tm_map(paperCorp, removeWords, c("viral", 
                                                  "market"))
  
  jeopCorpus_2 <- Corpus(VectorSource(subarea_2$TITLE %>% na.omit()))
  
  paperCorp_2 <- jeopCorpus_2
  paperCorp_2 <- tm_map(paperCorp_2, removePunctuation)
  paperCorp_2 <- tm_map(paperCorp_2, removeNumbers)
  # added tolower
  paperCorp_2 <- tm_map(paperCorp_2, content_transformer(tolower))
  paperCorp_2 <- tm_map(paperCorp_2, removeWords, stopwords("english"))
  # moved stripWhitespace
  
  paperCorp_2 <- tm_map(paperCorp_2, stripWhitespace)
  paperCorp_2 <- tm_map(paperCorp_2, stemDocument)
  
  paperCorp_2 <- tm_map(paperCorp_2, removeWords, c("viral", 
                                                  "market"))
  
  jeopCorpus_3 <- Corpus(VectorSource(subarea_3$TITLE %>% na.omit()))
  
  paperCorp_3 <- jeopCorpus_3
  paperCorp_3 <- tm_map(paperCorp_3, removePunctuation)
  paperCorp_3 <- tm_map(paperCorp_3, removeNumbers)
  # added tolower
  paperCorp_3 <- tm_map(paperCorp_3, content_transformer(tolower))
  paperCorp_3 <- tm_map(paperCorp_3, removeWords, stopwords("english"))
  # moved stripWhitespace
  
  paperCorp_3 <- tm_map(paperCorp_3, stripWhitespace)
  paperCorp_3 <- tm_map(paperCorp_3, stemDocument)
  
  paperCorp_3 <- tm_map(paperCorp_3, removeWords, c("viral", 
                                                    "market"))
  
  
  list(wordcloud_1 = paperCorp_1,
       wordcloud_2 = paperCorp_2,
       wordcloud_3 = paperCorp_3)
}