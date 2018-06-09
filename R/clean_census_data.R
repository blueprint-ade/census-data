clean_census_data <- function() {
  
  cd  <- data.table::fread("raw_data/cda_data/cda_data.csv")
  cd  <- cd %>% janitor::clean_names()
  cds <- cd[, .(#---------------------------------------------SELECT
    loc      = geo_code_por, 
    geo_level,
    var_num  = member_id_profile_of_dissemination_areas_2247, 
    var_name = dim_profile_of_dissemination_areas_2247,
    val      = dim_sex_3_member_id_1_total_sex)
  ][#---------------------------------------------------------FILTER
    geo_level == 4
  ][, 
    val := as.numeric(val)
  ]
  
  cds[, pop := first(val), loc]
  
  
  data.table::fwrite(cds, 'tidy_data/cda_data_clean.csv')

}