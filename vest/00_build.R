# libs ----
library(geomander) # >= 1.1.0.9000
library(PL94171)
library(here)
library(sf)
library(readr)
library(tigris) # >= 1.4.1.9000
library(tibble)

# time log ----
log_time <- function(path, state, year) {
  if (!fs::file_exists(path)) {
    fs::file_create(path)
  }
  lines <- readLines(con = path)
  lines <- c(lines, paste0(Sys.time(),',', state, ',', year))
  writeLines(text = lines, con = path)
}

# prep ----
sf_use_s2(FALSE)
years <- c(2016, 2018, 2020)

# run
for (year in years){
  states <- vest_states(year)
  for (state in states) {
    try({
    fs::dir_create(here(state))
    block <- tigris::blocks(state, year = 2020)
    vest <- get_vest(state = state, year = year) %>% st_transform(st_crs(block))
    match_list <- geo_match(from = block, to = vest, method = 'centroid')
    tb <- tibble(block2vtd = match_list, block_GEOID = block$GEOID20)
    write_csv(tb, file = here(glue::glue('{state}/{state}_{year}_centroid_match.csv')))
    log_time('log_time.txt', state, year)
    })
  }
}

