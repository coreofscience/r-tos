source('functions.R')

filename < - "f8417957-a805-489d-96e1-fc63ed9036e1.txt"

tos_dataframe < -read_isi_file(filename)
tos_dataframe < -split_references(tos_dataframe, "; ")
tos_dataframe < -format_dataframe(tos_dataframe)
tos_graph < -graph_compute(tos_dataframe)
tos_graph < -clean_graph(tos_graph)
