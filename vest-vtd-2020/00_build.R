# libs ----
library(tidyverse)
library(geomander) # >= 1.1.0.9000
library(PL94171)
library(here)
library(sf)
library(tigris) # >= 1.4.1.9000
library(censable)

# helpers ----
source(here('utils.R'))

# prep ----
sf_use_s2(FALSE)
years <- c(2016, 2018, 2020)

# run
for (year in years){
  states <- vest_states(year)
  for (state in states) {
    fs::dir_create(here(state))
    
    if (!fs::file_exists(here(glue::glue('{state}/{state}_{year}_2020_block_data.csv'))) &
        !fs::file_exists(here(glue::glue('{state}/{state}_{year}_2020_vtd_data.csv')))) {
      
      
      # Step 1: Match VEST precincts to 2010 Census blocks ----
      block <- tigris::blocks(state, year = 2010)
      vest <- get_vest(state = state, year = year, clean_names = FALSE) %>% 
        st_transform(st_crs(block))
      match_list <- geo_match(from = block, to = vest, method = 'centroid')
      tb <- tibble(block10_vest = match_list, block_GEOID = block$GEOID10)
      
      # Step 2: Add populations to 2010 blocks ---- 
      dec <- build_dec('block', state = state, geometry = FALSE, groups = 'all') %>% 
        rename(block = GEOID) %>% 
        select(-NAME)
      
      # Step 3: Estimate election data down to 2010 blocks ----
      elec_at_2010 <- tibble(GEOID = block$GEOID10)
      elections <- names(vest)[str_detect(names(vest), str_c('G', year - 2000))|
                                 str_detect(names(vest), str_c('R', year - 2000))]
      for (election in elections) {
        elec_at_2010 <- elec_at_2010 %>% 
          mutate(!!election := estimate_down(value = vest[[election]], wts = dec[['vap']], 
                                             group = match_list))
      }
      
      # Step 4: Crosswalk election data to 2020 blocks by area ----
      cw <- pl_crosswalk(toupper(state))
      rt <- pl_retally(elec_at_2010, crosswalk = cw)
      
      # Step 5: Aggregate to 2020 VTDs by BAFs
      baf <- pl_get_baf(toupper(state), 'VTD')
      if (length(baf) > 0) {
        baf <- baf[[1]]
        baf <- baf %>% 
          rename(GEOID = BLOCKID) %>% 
          mutate(STATEFP = match_fips(state), 
                 GEOID20 = paste0(STATEFP, COUNTYFP, DISTRICT))
        
        rt <- rt %>% left_join(baf, by = 'GEOID')
        
        # agg
        vtd <- rt %>% 
          select(-GEOID, -area_land, -area_water) %>% 
          group_by(GEOID20) %>% 
          summarize(across(where(is.character), .fns = unique),
                    across(where(is.numeric), .fns = sum)) %>% 
          relocate(GEOID20, .before = everything()) %>% 
          relocate(STATEFP, .before = COUNTYFP)
        
        # Write to File ----
        write_csv(vtd, file = here(glue::glue('{state}/{state}_{year}_2020_vtd_data.csv')))
      } else {
        write_csv(rt, file = here(glue::glue('{state}/{state}_{year}_2020_block_data.csv')))
      }
      
      log_time(here('log_time.txt'), state, year)
      
    }
  }
}

