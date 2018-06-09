micro_sample_lgl <- function(dat, loc, ..., pop = pop) {
  
  pop <- enquo(pop)
  loc <- enquo(loc)
  vars <- quos(...)
  
  dat[is.na(dat)] <- 0
  
  make_true <- function(lgl, id) {
    
    lgl[id] <- TRUE
    lgl
    
  }
  
  get_vec <- function(pop, var) {
    
    x <- rep(FALSE, pop)
    t <- make_true(x, sample(1:pop, var))
    t
    
    
  }
  
  dat %>% mutate_at(
    vars(!!!vars), 
    funs(map2(!!pop, ., ~get_vec(.x, .y)))
  ) %>% unnest() %>% 
    group_by(!!loc) %>% 
    mutate(id = row_number())
  
}

shake <- function(dat, ...) {
  
  vars <- quos(...)
  
  dat %>% mutate_at(vars(!!!vars), funs(sample(., length(.))))
  
}


micro_sample_fct <- function(dat, ..., 
                             loc = loc, var_num = var_num, var_name = var_name,
                             val = val) {
  
  loc <- enquo(loc)
  var_num <- enquo(var_num)
  var_name <- enquo(var_name)
  val <- enquo(val)
  
  f <- rlang::list2(...)
  
  print(f)
  
  vars <- map(f, ~eval(.[[3]]))
  q_vars <- map(f, ~.[[2]])
  
  names(vars) <- map_chr(q_vars, ~quo_name(.x))
  
  df_nest <- vars %>% imap_dfr(~dat %>% filter(!!var_num %in% .x, 
                                               !is.na(!!val)) %>% 
                                 select(!!loc, !!var_name, !!val) %>% 
                                 mutate(var_grp = .y)) %>% 
    mutate(x = map2(!!var_name, !!val, ~rep(.x, .y)),
           x = map(x, ~sample(.x, length(.x))))
  
  df_out <- df_nest %>% unnest() %>% select(-!!var_name, -!!val) %>% 
    group_by(!!loc, var_grp) %>% 
    mutate(id = row_number()) %>% spread(var_grp, x) %>% 
    ungroup()
  
  df_out %>% 
    group_by(!!loc) %>% 
    shake(!!!q_vars)
  
}
