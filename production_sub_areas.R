production_sub_area <- function(graph){
  
  ####Agregar atributo sub_area.
  source('sub_areas.R')
  graph_1 <- modularity(graph)
  
  ####Recolectar datos de producción.
  df_graph <- 
    data.frame(vertices = V(graph_1)$name, 
               sub_area = V(graph_1)$sub_area,
               stringsAsFactors = TRUE) %>% 
    arrange(desc(sub_area))
  df_graph$year <- str_extract(df_graph$vertices, "[0-9]+")
  df_graph <- filter(df_graph, year >= 1900)
  df_graph <- na.omit(df_graph)
  
  
  all_data <- 
    data.frame(Year = c(min(unique(df_graph$year)): max(unique(df_graph$year))),
               stringsAsFactors = FALSE)
  names_all_data <- list("Year")
  
  for (i in 1:length(unique(df_graph$sub_area))){
    frecuency_sub_area <-
      df_graph %>% 
      filter(sub_area == i) %>% 
      count(year)
    names(frecuency_sub_area) <- c("Año", "Frecuencia")
    
    frec_sub_area <- ifelse(all_data$Year %in% frecuency_sub_area$Año,
                            frecuency_sub_area$Frecuencia, 0)
    all_data <- cbind(all_data, frec_sub_area)
    
    names_all_data <- append(names_all_data,
                             paste("production_sub_area", as.character(i), sep = ""))
  }
  
  names(all_data) <- names_all_data
  
  all_data <-
    all_data %>% 
    gather(key="sub_areas", value="frecuencia", 2:(length(unique(df_graph$sub_area))+1))
  
  ####Tamaño de comunidades.
  len_comunities <- 
    df_graph %>% 
    group_by(sub_area) %>% 
    tally()
  
  
  #####Codigo para graficar las tres principales (mas grandes) comunidades.
  big_comunities <- 
    len_comunities %>%
    arrange(desc(n))
  
  comunity1 <- paste("production_sub_area", as.character(big_comunities$sub_area[1]), sep = "")
  comunity2 <- paste("production_sub_area", as.character(big_comunities$sub_area[2]), sep = "")
  comunity3 <- paste("production_sub_area", as.character(big_comunities$sub_area[3]), sep = "")
  
  para_graficar <-
    all_data[all_data$sub_areas == comunity1
             | all_data$sub_areas == comunity2
             | all_data$sub_areas == comunity3, ]
  
  ploting <-
    ggplot(para_graficar, aes(x=Year, y=frecuencia, color=sub_areas)) +
    xlim(c(1990, 2020)) +
    geom_point(size=0.6) +
    geom_line() +
    labs(title = "Producción sub_areas",
         x = "Years",
         y = "Frecuencia")

  
  list(production = all_data,
       graph = graph_1,
       length_comunities = len_comunities,
       plot = ploting)
}