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
    select(ID_TOS, REFS_NESTED) %>% 
    unnest(REFS_NESTED) %>%  
    mutate(lastname = sub("\\., .*", "", CR),
           lastname = sub(",", "", lastname),
           lastname = sub("\\.", "", lastname),# extracting lastnames,
           year = str_extract(CR, "\\(([0-9]{4})\\)"),
           year = str_remove_all(year, "\\(|\\)")) %>%   # extracting year needs to be improved
    filter(!grepl(pattern = "[():[:digit:]]", lastname),
           str_length(year) == 4) %>% 
    mutate(CR = paste0(lastname, ", ", year, ",")) %>% 
    select(ID_TOS, CR)
  
  unmatched_refs_scopus <- 
    edge_list_scopus %>% 
    anti_join(tos_scopus_ref, 
              by = c("CR" = "ID_TOS")) %>% 
    select(CR) %>% 
    set_names("ID_TOS") %>% 
    distinct() %>% 
    mutate(PY = str_extract(ID_TOS, 
                            ", [0-9]{4},"),
           PY = str_remove_all(PY, ","),
           PY = as.numeric(str_trim(PY)))
  
  tos_scopus_ref_cols <-
    tos_scopus_ref[0,] 
  
  unmatched_refs_full_scopus <-
    complete(tos_scopus_ref_cols,
             unmatched_refs_scopus)
  
  tos_scopus <- 
    tos_scopus_ref %>% 
    bind_rows(unmatched_refs_full_scopus) %>% 
    mutate(ID_TOS = sub("^(\\S*\\s+\\S+\\s+\\S+).*", # removing strings after second comma 
                        "\\1", 
                        ID_TOS)
    ) %>% 
    select(ID_TOS, everything()) # There is a mistake here - NA and NULL (REF)
  
  tos_scopus 
}