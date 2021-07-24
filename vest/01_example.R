# libs ----
library(geomander)
library(readr)

# vars ----
state <- 'nj'
year <- 2016

# data ----
vest <- get_vest(state, year = year)
match_tb <- read_csv(file = glue::glue('{state}/{state}_{year}_centroid_match.csv'))
block <- tigris::blocks(state = state, year = 2020)

# pop placeholder; As of Aug 16, we can update to actual vap
block$vap <- 1

# estimate ----
# each of these is an estimated block level portion of votes
block_rep <- estimate_down(wts = block$vap, value = vest$pre_16_rep, group = match_tb$block2vtd)
block_dem <- estimate_down(wts = block$vap, value = vest$pre_16_dem, group = match_tb$block2vtd)
