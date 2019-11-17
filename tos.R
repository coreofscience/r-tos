source('functions.R')

tos <- function(fileinput) {
    tos_dataframe <- read_isi_file(fileinput)
    tos_dataframe <- split_references(tos_dataframe, "; ")
    tos_dataframe <- format_dataframe(tos_dataframe)
    tos_graph <- graph_compute(tos_dataframe)
    tos_graph <- clean_graph(tos_graph)
    tos_structure <- tos_labels(tos_graph)

    return(tos_structure)
}
