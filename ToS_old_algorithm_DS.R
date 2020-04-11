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

tos_wos <- function(file) { 
    
    library(stringr)
    library(stringdist)
    library(bibliometrix)
    library(igraph)
    
    source("readFiles.R")
    
    wom.raw.1 <- readISI(file)
    
    wom.raw.1$ID_WOS <- rownames(wom.raw.1)
    wom.raw.1$ID_WOS <- ifelse(!is.na(wom.raw.1$VL), 
                               paste(wom.raw.1$ID_WOS,
                                     wom.raw.1$VL,
                                     sep = ", V"),
                               wom.raw.1$ID_WOS)
    wom.raw.1$ID_WOS <- ifelse(!is.na(wom.raw.1$BP), 
                               paste(wom.raw.1$ID_WOS,
                                     wom.raw.1$BP,
                                     sep = ", P"),
                               wom.raw.1$ID_WOS)
    wom.raw.1$ID_WOS <- ifelse(!is.na(wom.raw.1$DI),
                               paste(wom.raw.1$ID_WOS,
                                     wom.raw.1$DI,
                                     sep = ", DOI "),
                               wom.raw.1$ID_WOS)
    
    wom.raw.1 <- 
        wom.raw.1 %>% 
        separate_rows(CR, sep = "; ") %>% 
        select(ID_WOS, CR) %>% 
        filter(CR != "" & is.na(CR) == FALSE)
    
    graph <- graph.data.frame(wom.raw.1)
    graph.1 <- simplify(graph)
    graph.2 <- delete.vertices(graph.1, 
                               which(degree(graph.1, mode = "in") == 1 & 
                                         degree(graph.1, mode = "out") == 0))
    giant.component <- function(graph) {
        cl <- clusters(graph)
        induced.subgraph(graph, which(cl$membership == which.max(cl$csize)))
    }
    
    graph.3 <- giant.component(graph.2)
    
    network.metrics <- data.frame(
        id = V(graph.3)$name,
        indegree = degree(graph.3, mode = "in"),
        outdegree = degree(graph.3, mode = "out"),
        bet = betweenness(graph.3),
        stringsAsFactors = FALSE
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
    
    list(df = wom.raw.1, 
         graph = graph.3,
         net_metrics = network.metrics,
         tos = tos)
    
}

