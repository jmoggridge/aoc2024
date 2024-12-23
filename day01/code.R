library(tidyverse)

# part 1
read_lines('day01/input') %>% 
  str_split('\\s\\s\\s') %>% 
  transpose() %>% 
  map(~as.numeric(.) %>% sort()) %>%
  transpose() %>% 
  map_dbl(~abs(.[[1]] - .[[2]] )) %>% 
  sum()
  
# part 2
# Calculate a total similarity score by adding up each number in the left list
# after multiplying it by the number of times that number appears in the right list.

# input <- read_lines('day01/input')
lists <- 
  read_lines('day01/input') %>% 
  str_split('\\s\\s\\s') %>% 
  transpose() 

table_right <- 
  tibble(x = lists[[2]] %>% as.numeric()) %>% 
  count(x) %>% 
  deframe()

# sum_left_times_tab_right <- 
lists[[1]] %>% 
  map_dbl(~ {
    if (!. %in% names(table_right)) {
      return(0)
    }
    table_right[.] * as.numeric(.)
  }
  ) %>% 
  sum()
