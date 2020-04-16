network.metrics <- function(graph) {
  
  network.metrics <- data.frame(
    id = V(graph)$name,
    indegree = degree(graph, mode = "in"),
    outdegree = degree(graph, mode = "out"),
    bet = betweenness(graph),
    stringsAsFactors = FALSE
  )
   
  network.metrics
  
}