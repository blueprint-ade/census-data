census_factors <- function(dat, ...) {
  
  cds <- data.table::fread('tidy_data/cda_data_clean.csv')
  
  cds <- cds[str_sub(loc, 1, 2) == "46"]
  
  ls_factors <- list(
    
    
    df_marstat = cds %>% 
      micro_sample_fct(
        marstat ~ c(60, 63)),
    
    df_lang = cds %>% 
      micro_sample_fct(
        lang    ~ 101:104),
    
    df_emp_income = cds %>% 
      micro_sample_fct(
        emp_income  ~ c(666, 668))
    
  )
  
  df_factors <- ls_factors %>% reduce(full_join)
  
  data.table::fwrite(df_factors, "tidy_data/census_factors_1.csv")
  
}
