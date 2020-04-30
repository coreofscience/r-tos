forest_wos <- function(biblio_wos) {
  
  biblio_wos_ref <- 
    as_tibble(biblio_wos) %>% 
    separate_rows(CR, sep = ";") %>%  # the CR data is removed here, something to improve strsplit could be an option
    nest(data = CR) %>% 
    rename("CR_FOREST" = data) %>% 
    mutate(CR = biblio_wos$CR)
  
  edge_list <- 
    biblio_wos_ref %>% 
    select(ID_WOS, CR_FOREST) %>% 
    unnest(CR_FOREST)
  
  unmatched_refs <- 
    edge_list %>% 
    anti_join(biblio_wos_ref, 
              by = c("CR" = "ID_WOS")) %>% 
    select(CR) %>% 
    distinct() 
  
  unmatched_refs_dois <- 
    unmatched_refs %>% 
    mutate(dois = str_extract(CR, "10\\..*")) %>% 
    na.omit() %>% 
    mutate(dois = sub(", DOI.*", "", dois), 
           dois = str_replace_all(dois, " ", "" ))

  unmatched_refs_dois_full <- 
    map(unmatched_refs_dois$dois, 
        .f = safely(function(x) oadoi_fetch(x,
                                            email = "sebastian.robledo@gmail.com")))
  ####
  references_wos <- 
    str_split_fixed(unmatched_refs$CR, 
                    pattern = ", ",
                    6)  %>% 
    as_tibble() %>% 
    mutate(ID_WOS = unmatched_refs$CR) %>% 
    as_tibble() %>% 
    filter(V1 != "" & V2 != "" & V3 != "" & V4 != "" & V5 != "" & V6 != "")
  
}

