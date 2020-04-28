forest_wos <- function(biblio_wos) {
  
  biblio_wos_ref <- 
    as_tibble(biblio_wos) %>% 
    separate_rows(CR, sep = ";") %>%  # the CR data is removed here, something to improve
    nest(data = CR) %>% 
    rename("CR_FOREST" = data) %>% 
    mutate(CR = biblio_wos$CR)
}


