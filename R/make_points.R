make_points <- function() {
  
  shp <- clean_mb_map()
  shp_pop <- join_pop(shp)
  pt_pop <- cast_points(shp_pop)
  
  pt_pop %>% fwrite("tidy_data/pop_points.csv")
  
  pt_pop
  
}

clean_mb_map <- function() { 
  
  shp <- sf::st_read("raw_data/cda_map/gda_000a11a_e.shp", stringsAsFactors = FALSE) %>% 
    janitor::clean_names() %>% 
    filter(prname == "Manitoba") %>% 
    select(loc = dauid) %>% 
    mutate(loc = as.integer(loc))
  
  shp
  
}

join_pop <- function(shp) {
  
  df_pop <- data.table::fread("tidy_data/cda_data_clean.csv")[,
    .(pop = first(pop)), loc ]
  
  shp_pop <- shp %>% left_join(df_pop)
  
  shp_pop
}

cast_points <- function(shp_pop) {
  
  gon_pop <- shp_pop %>% 
    filter(!is.na(pop) & pop > 0)
  
  # gon_pop <- gon_pop[1:100, ] # UNCOMMENT TO SHRINK FOR TESTING
  
  pt_pop <- suppressWarnings(gon_pop %>% 
    st_sample_exact(.$pop) %>% 
    as_tibble() %>% 
    mutate(loc = gon_pop$loc) %>% 
    st_as_sf(sf_column_name = "geometry") %>% 
    st_cast("POINT")
  )
  cd_pop <- pt_pop %>% 
    st_coordinates() %>% 
    as_tibble() %>% 
    mutate(loc = pt_pop$loc) %>% 
    group_by(loc) %>% 
    mutate(id = row_number())
    
  
  cd_pop
  
  
  
}


