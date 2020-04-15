tos_scopus <- function(file) {
  
  source("readFiles.R")
  
  data_raw_scopus <- 
    readScopus(file) %>% 
    separate_rows(CR, 
                  sep = "; ") 
  
  edge_list_raw <- sub("\\., .*", "", data_raw_scopus$CR)
  
}
