sub_areas <- function(graph_tos) {
  
  library(reticulate)
  py_install("networkx")
  py_install("numpy")
  py_run_string("import networkx as nx")
  
  py_run_string("my_python_array = np.array([2,4,6,8])")
  
}