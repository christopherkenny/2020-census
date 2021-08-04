log_time <- function(path, state, year) {
  if (!fs::file_exists(path)) {
    fs::file_create(path)
  }
  lines <- readLines(con = path)
  lines <- c(lines, paste0(Sys.time(),',', state, ',', year))
  writeLines(text = lines, con = path)
}


get_vtd_2020 <- function(state) {
  fips <- censable::match_fips(state = state)
  
  url <- glue::glue('https://www2.census.gov/geo/tiger/TIGER2020PL/LAYER/VTD/2020/tl_2020_{fips}_vtd20.zip')
  zip_path <- withr::local_tempfile(fileext = 'zip')
  PL94171:::download_census(url = url, path = zip_path)
 
  unz_path <- fs::dir_create(tempdir(), glue::glue('vtd_2020_{fips}'))
  
  zip::unzip(zipfile = zip_path, exdir = unz_path)
  
  read_sf(fs::path(unz_path, glue::glue('tl_2020_{fips}_vtd20'), ext = 'shp'))
}