source('functions.R')

filename <-  "data.txt"

tos_dataframe <- read_isi_file(filename)
tos_dataframe <- split_references(tos_dataframe, "; ")
tos_dataframe <- format_dataframe(tos_dataframe)
tos_graph <- graph_compute(tos_dataframe)
tos_graph <- clean_graph(tos_graph)

print("Successfully !!!")

