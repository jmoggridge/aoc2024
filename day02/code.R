library(tidyverse)

# a report only counts as safe if both of the following are true:
# The levels are either all increasing or all decreasing.
# Any two adjacent levels differ by at least one and at most three.
# How many reports are safe?

is_safe <- \(x){
  diffs <- map_dbl(1:(length(x)-1), ~x[.] - x[. + 1])
  dists <- abs(diffs)
  all_incr_or_decr <- all(diffs < 0 ) || all(diffs > 0)
  all_within_1_and_3 <- all(dists >= 1) && all(dists <= 3)
  all_incr_or_decr && all_within_1_and_3
}

read_lines('day02/input') %>% 
  map(~str_split(., ' ') %>% flatten() %>%  as.numeric()) %>% 
  map_lgl(is_safe) %>% 
  sum()

# part 2
problem_dampener <- \(x){
  map(1:length(x), \(y) discard_at(x, y))
}

read_lines('day02/input') %>% 
  map_lgl(
    \(x) {
      x <- str_split(x, ' ') %>% flatten() %>%  as.numeric()
      problem_dampener(x) %>% 
        map_lgl(is_safe) %>% 
        any()
    }) %>% 
  sum()


