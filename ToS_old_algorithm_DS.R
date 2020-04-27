if (!require(bibliometrix)) {
    install.packages("bibliometrix")
}

if (!require(igraph)) {
    install.packages("igraph")
}

if (!require(stringr)) {
    install.packages("stringr")
}

if (!require(stringdist)) {
    install.packages("stringdist")
}

if (!require(tidyverse)) {
    install.packages("tidyverse")
}

tos_wos <- function(file) { 
    
    library(stringr)
    library(stringdist)
    library(bibliometrix)
    library(igraph)
    library(tidyverse)
    
    source("readFiles.R")
    
    data_wos <- readISI(file)
    
    data_wos$ID_WOS <- rownames(data_wos)
    
    data_wos$ID_WOS <- ifelse(!is.na(data_wos$VL), 
                               paste(data_wos$ID_WOS,
                                     data_wos$VL,
                                     sep = ", V"),
                              data_wos$ID_WOS)
    
    data_wos$ID_WOS <- ifelse(!is.na(data_wos$BP), 
                               paste(data_wos$ID_WOS,
                                     data_wos$BP,
                                     sep = ", P"),
                              data_wos$ID_WOS)
    
    data_wos$ID_WOS <- ifelse(!is.na(data_wos$DI),
                               paste(data_wos$ID_WOS,
                                     data_wos$DI,
                                     sep = ", DOI "),
                              data_wos$ID_WOS)
    
    edgelist <- 
        as.tibble(data_wos) %>% 
        mutate(cited_references = CR) %>% 
        separate_rows(CR, sep = ";") %>% 
        select(ID_WOS, CR) %>% 
        filter(CR != "" & is.na(CR) == FALSE)
    
    graph <- 
        graph.data.frame(edgelist) %>% 
        simplify(graph)
    
    graph_1 <- delete.vertices(graph, 
                               which(degree(graph, mode = "in") == 1 & 
                                         degree(graph, mode = "out") == 0))
    giant.component <- function(graph) {
        cl <- clusters(graph)
        induced.subgraph(graph, which(cl$membership == which.max(cl$csize)))
    }
    
    graph_2 <- giant.component(graph_1)
    
    network.metrics <- tibble(
        id = V(graph_2)$name,
        indegree = degree(graph_2, mode = "in"),
        outdegree = degree(graph_2, mode = "out"),
        bet = betweenness(graph_2)
    )
    
    seminals <- network.metrics[network.metrics$outdegree == 0,
                                c("id","indegree")]
    seminals <- head(seminals[with(seminals, order(-indegree)),],10)
    
    structurals <- network.metrics[network.metrics$bet > 0,
                                   c("id", "bet")]
    structurals <- head(structurals[with(structurals, order(-bet)),],10)
    
    current <- network.metrics[network.metrics$indegree == 0,
                               c("id","outdegree")]
    current <- head(current[with(current, order(-outdegree)),], 60)
    
    if(sum(network.metrics$bet) == 0 ) { 
           stop("Your ToS does not have trunk, check the search out")} else {
           
    
    seminals$ToS <- "Raiz"
    seminals$order <- 1:length(seminals$id)
    structurals$ToS <- "Tronco"
    structurals$order <- 1:length(structurals$id)
    current$ToS <- "Hojas"
    current$order <- 1:length(current$id)
    
    tos <- rbind(seminals[,c(1,3,4)], structurals[,c(1,3,4)], current[,c(1,3,4)])
           }
    
    # tos.1 <- merge(tos, wom.raw.1, by.x = "id", by.y = "SR", all.x = TRUE)
    
    # tos.2 <- tos.1[,c("id", "ToS","AU", "TI", "DI")]
    
    list(df = data_wos, 
         graph = graph_2,
         net_metrics = network.metrics,
         tos = tos)
    
}

