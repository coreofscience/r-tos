tos_scopus <- function(biblio_scopus) {
  
  tos_scopus_ref <- 
    as_tibble(biblio_scopus) %>% 
    separate_rows(CR, sep = "; ") %>%  # the CR data is removed here, something to improve strsplit could be an option
    nest(data = CR) %>% 
    rename("REFS_NESTED" = data,
           "ID_SCOPUS" = SR) %>% 
    mutate(CR = biblio_scopus$CR) %>% 
    select(ID_SCOPUS, everything())
}