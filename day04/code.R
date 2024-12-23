library(tidyverse)

count_xmas <- \(x) x %>% paste0(collapse = ' ') %>% str_count('XMAS')
rev_lines <- \(x) str_split(x, '') %>% map_chr(~rev(.) %>% paste0(collapse = ''))

horiz_fwd_lines <- read_lines('day04/input')
horiz_rev_lines <- horiz_fwd_lines %>% rev_lines()

grid <- horiz_fwd_lines %>% str_split('', simplify = T)

vert_fwd_lines <- 
  map(seq_len(ncol(grid)), \(i) grid[,i]) %>% 
  map_chr(paste0, collapse = '')
vert_rev_lines <- vert_fwd_lines %>% rev_lines()

tlbr <- grid %>% split(row(grid) - col(grid))
bltr <- grid %>% split(col(grid) + row(grid))

c(
  fwd_count = horiz_fwd_lines %>% count_xmas(),
  rev_count = horiz_rev_lines %>% count_xmas(),
  down_count = vert_fwd_lines %>% count_xmas(),
  up_count = vert_rev_lines %>% count_xmas(),
  tlbr_fwd_count = tlbr %>% map(paste0, collapse='') %>% count_xmas(),
  tlbr_rev_count = 
    map(tlbr, rev) %>% 
    map(paste0, collapse='') %>% 
    count_xmas(),
  bltr_fwd_count = bltr %>% map(paste0, collapse='') %>% count_xmas(),
  bltr_rev_count = 
    map(bltr, rev) %>% 
    map(paste0, collapse='') %>% 
    count_xmas()
) %>% 
  sum

# part 2

map(
  2:(nrow(grid)-1),
  \(x) {
    map_dbl(
      2:(ncol(grid)-1), 
      \(y){
        corners <- paste0(
          a = grid[x, y],
          tl = grid[x-1, y-1],
          tr = grid[x-1, y+1],
          bl = grid[x+1, y-1],
          br = grid[x+1, y+1]
        ) 
        if (corners %in% c( 'ASMSM', 'AMSMS', 'ASSMM', 'AMMSS')){
          return(1)
        } else {
          return(0)
        }
      }
    )
  }
) %>% 
  map_dbl(sum) %>% 
  sum
