inform_points <- function() {
  
  pt_pop <- fread("tidy_data/pop_points.csv")
  df_dat <- fread("tidy_data/census_factors_1.csv") 
  
  setkey(pt_pop, loc, id)
  setkey(df_dat, loc, id)
  
  pt_out <- pt_pop[df_dat][!is.na(X)]
  
  pt_out %>% fwrite("tidy_data/map_data.csv")
  
  pt_out
}