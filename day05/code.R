library(tidyverse)

# are numbers in `update` ordered as indicated by `rule`
test_rule <- \(rule, update){
  if (update == '') {
    return(FALSE)
  }
  left_present <- str_detect(update, rule[1])
  right_present <- str_detect(update, rule[2])
  pattern <- paste0(rule[1],'.*?', rule[2])
  pattern_present <- str_detect(update, pattern)
  if (!left_present || !right_present) {
    return(TRUE)
  } else if (pattern_present) {
    return(TRUE) 
  } else {
    return(FALSE)  
  }
}

# check update against all rules
test_rules <- \(update){
  rules %>% 
    map_lgl(\(rule) {test_rule(rule, update)}) %>% 
    all()
}


# file <- 'day05/example.txt'
file <- 'day05/input.txt'

# part 1
input <- 
  file %>% 
  read_file() %>% 
  str_split('\n\n', simplify = T) %>% 
  as.character() %>% 
  map(~str_split(., '\n')) %>% 
  flatten()

rules <- input[[1]] %>% map(~str_split(., '\\|') %>% unlist())
updates <- input[[2]] %>% keep(!. == '')

updates %>% 
  keep(test_rules) %>% 
  map_dbl(
    . %>%
      str_split(',') %>% 
      unlist() %>%
      as.numeric() %>% 
      {\(x) x[(length(x) %/% 2) + 1]}()
  ) %>% 
  sum()

# part 2

# get invalid updates
invalid <- \(updates) keep(updates, ~!test_rules(.))

# correct the ordering for a broken rule & return update
switch_numbers <- \(rule, update){
  left <- rule[1]
  right <- rule[2]
  left_pattern <- paste0('(?<=([^0-9]|^))', left, '(?=([^0-9]|$))')
  right_pattern <- paste0('(?<=([^0-9*]|^))', right, '(?=([^0-9*]|$))')
  
  (
    updated <- update %>% 
      str_replace(left_pattern, paste0(right,'*')) %>% 
      str_replace(right_pattern, paste0(left, '*')) %>% 
      str_remove_all('\\*') 
  )
  return(updated)
}

get_broken_rules <- \(update){
  rules |> keep(~!test_rule(., update = update))
}

fix_update <- \(update){
  while (!test_rules(update)) {
    broken_rules <- get_broken_rules(update)
    rule <- broken_rules[[1]]
    update <- switch_numbers(rule = rule, update = update)   
  }
  
  return(
    update |> 
      str_split(',') |> 
      unlist() |> 
      as.numeric() |> 
      {\(x) x[length(x) %/% 2 + 1]}()
  )
}

#' part 2: 6305

invalid(updates) |> 
  map_dbl(fix_update, .progress = 'fixing') |> 
  sum()
