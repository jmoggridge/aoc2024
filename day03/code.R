library(tidyverse)

mul <- \(x){
  str_extract_all(x, '[0-9]+', simplify = T) %>%  as.numeric %>% prod
}

read_lines('day03/input') %>% 
  str_extract_all('mul\\([0-9]+,[0-9]+\\)', simplify = T) %>% 
  map_dbl(mul) %>% 
  sum()  
  
# part 2

do <- T

read_lines('day03/input') %>% 
  paste0(collapse = '') %>% 
  str_extract_all("mul\\([0-9]+,[0-9]+\\)|do\\(\\)|don't\\(\\)", simplify = T) %>% 
  as.character() %>% 
  map_dbl(
    \(x) {
      if (x == 'do()') {
        do <<- T
        return(0)
      } else if (x == "don't()" || !do) {
        do <<- F
        return(0)
      } else {
        return(mul(x))
      }
    }
  ) %>%
  sum()


