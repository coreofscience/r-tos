tos_wos <- function(biblio_wos) {
  
  tos_wos_ref <- 
    as_tibble(biblio_wos) %>% 
    separate_rows(CR, sep = ";") %>%  # the CR data is removed here, something to improve strsplit could be an option
    nest(data = CR) %>% 
    rename("REFS_NESTED" = data) %>% 
    mutate(CR = biblio_wos$CR) %>% 
    select(ID_WOS, everything())
  
  edge_list_wos <- 
    tos_wos_ref %>% 
    select(ID_WOS, REFS_NESTED) %>% 
    unnest(REFS_NESTED)
  
  unmatched_refs <- 
    edge_list_wos %>% 
    anti_join(tos_wos_ref, 
              by = c("CR" = "ID_WOS")) %>% 
    select(CR) %>% 
    set_names("ID_WOS") %>% 
    distinct() %>% 
    mutate(PY = str_extract(ID_WOS, 
                            ", [0-9]{4},"),
           PY = str_remove_all(PY, ","),
           PY = as.numeric(str_trim(PY)))
  
  tos_wos_ref_cols <-
    tos_wos_ref[0,] 
  
  unmatched_refs_full <-
    complete(tos_wos_ref_cols,
             unmatched_refs)

  tos_wos <- 
    tos_wos_ref %>% 
    bind_rows(unmatched_refs_full) %>% 
    mutate(ID_TOS = sub("^(\\S*\\s+\\S+\\s+\\S+).*", # removing strings after second comma 
                               "\\1", 
                               ID_WOS)
           ) %>% 
    select(ID_TOS, everything()) # There is a mistake here - NA and NULL (REF)
  
  tos_wos  # We need to extract the year from this tibble
}