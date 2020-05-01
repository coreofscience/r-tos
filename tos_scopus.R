tos_scopus <- function(biblio_scopus) {
  
  tos_scopus_ref <- 
    as_tibble(biblio_scopus) %>% 
    mutate(ID_TOS = str_extract(SR, ".*,")) %>% 
    separate_rows(CR, sep = "; ") %>%  # the CR data is removed here, something to improve strsplit could be an option
    nest(data = CR) %>% 
    rename("REFS_NESTED" = data) %>% 
    mutate(CR = biblio_scopus$CR,) %>% 
    select(ID_TOS, everything())
  
  edge_list_scopus <- 
    tos_scopus_ref %>% 
    select(ID_SCOPUS, REFS_NESTED) %>% 
    unnest(REFS_NESTED) %>%  
    mutate(lastname = sub("\\., .*", "", CR),
           lastname = sub(",", "", lastname),
           lastname = sub("\\.", "", lastname),# extracting lastnames,
           year = str_extract(CR, "\\(([0-9]{4})\\)"),
           year = str_remove_all(year, "\\(|\\)")) %>%   # extracting year needs to be improved
    filter(!grepl(pattern = "[():[:digit:]]", lastname),
           str_length(year) == 4) %>% 
    mutate(ID_SCOPUS = paste0(lastname, ", ", year, ","),
           SR = str_trim(SR)) %>% 
    select(SR, id_scopus)
}