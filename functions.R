library("tidyverse")
library("bibliometrix")
library("igraph")


read_isi_file <- function (isi_file) {
    text <-readFiles(isi_file)
    tos_dataframe <- convert2df(text, dbsource = "isi", format = "plaintext")
    tos_dataframe$IDWOS <-rownames(tos_dataframe)
    return(tos_dataframe)
}


split_references <- function (tos_dataframe, separator) {
    dataframe_splitted <- tos_dataframe %>% separate_rows(CR, sep = separator)
    return(dataframe_splitted)
}


format_dataframe <- function (tos_dataframe) {
    dataframe_formated <- tos_dataframe %>% mutate(
        IDWOS2 = paste(IDWOS, sep = ", ", 
            paste("V", sep = "", VL),
            paste("P", sep = "", BP),
            paste("DOI ", sep = "", DI)
        )
    )
    return(dataframe_formated)
}


graph_compute <- function (tos_dataframe) {
    edges <- tos_dataframe[
        !is.na(tos_dataframe$CR),
        c("IDWOS2", "CR")
    ]
    tos_graph <-graph.data.frame(edges, directed = TRUE)
    return(tos_graph)
}


clean_graph <- function (tos_graph) {
    tos_cleaned = delete.vertices(
        tos_graph, 
        which(degree(tos_graph, mode = "in") == 1 &
            degree(tos_graph, mode = "out") == 0
        )
    )
    
    return(tos_cleaned)
    
}


tos <- function(tos_graph) {
    network.metrics <- data.frame(
        id = V(tos_graph)$name,
        indegree = degree(tos_graph, mode = "in"),
        outdegree = degree(tos_graph, mode = "out"),
        bet = betweenness(tos_graph),
        stringsAsFactors = FALSE
    )
    roots <- 
        network.metrics %>% 
        filter(outdegree == 0) %>% 
        arrange(desc(indegree)) %>% 
        mutate(tos = "raiz") %>% 
        select(-indegree,
               -outdegree,
               -bet)
    trunk <- 
        network.metrics %>% 
        arrange(desc(bet)) %>% 
        mutate(tos = "tronco") %>% 
        select(-indegree,
               -outdegree,
               -bet)
    leaves <- 
        network.metrics %>% 
        filter(indegree == 0) %>% 
        arrange(desc(indegree)) %>% 
        mutate(tos = "hoja") %>% 
        select(-indegree,
               -outdegree,
               -bet)
    tos = rbind(roots, 
                trunk,
                leaves)
    
    return(tos)
}
