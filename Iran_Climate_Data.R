if (!require(here)) { install.packages('here') }; require(here)
if (!require(readr)) { install.packages('readr') }; require(readr)
if (!require(terra)) { install.packages('terra') }; require(terra)
if (!require(sf)) { install.packages('sf') }; require(sf)
if (!require(tidyverse)) { install.packages('tidyverse') }; require(tidyverse)
if (!require(raster)) { install.packages('raster') }; require(raster)
if (!require(rasterVis)) { install.packages('rasterVis') }; require(rasterVis)

# Read Rasters CHIRPS data

precip_all <- here('Data/CHIRPS/') %>%
  list.files(pattern = '\\.tif$',
             full.names = T) %>%
  rast()
#


# Read Iran Boundary
n_iran_bound <- st_read(here('Data/gadm36_IRN_shp'), layer = 'gadm36_IRN_1') %>%
  filter(NAME_1 %in% c('Mazandaran', 'Golestan', 'Gilan')) %>%
  vect()

# Clip Precip Rasters
precip <- precip_all %>% crop(n_iran_bound) %>% mask(n_iran_bound)

#The data needs to be given standard x-axis ticks to match up with our 
#monthly data across all years so we build that here as well as making the start and end
#dates for visually grouping years.

data_breaks <-data.frame(
  start = map(2008:2021,
              . %>%
                paste0(., '.',
                       stringr::str_pad(c(1),
                                        width = 2,
                                        side = 'left',
                                        pad = 0))) %>%
    unlist,
  end = map(2008:2021,
            . %>%
              paste0(., '.',
                     stringr::str_pad(c(12),
                                      width = 2,
                                      side = 'left',
                                      pad = 0))) %>%
    unlist(),
  colors = as.factor(2008:2021))

date_range <- map(2008:2021, . %>%
                    paste0(., '.',
                           stringr::str_pad(1:12,
                                            width = 2,
                                            side = 'left',
                                            pad = 0)
                    )
) %>% unlist()

# Precip Data Values
precip_df <- values(precip) %>%
  colSums(na.rm = T) %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  `colnames<-`(c('Name', 'Precip')) %>%
  mutate(Year.Month = str_extract(Name, '\\d{4}\\.\\d{2}$') %>%
           factor(levels=date_range),
         Year = str_extract(Name, '\\d{4}') %>%
           factor(levels=as.factor(2008:2021)),
         Month = str_extract(Name, '\\d{2}$') %>%
           factor(levels = str_pad(1:12, width = 2, 'left', '0')))

# Normalize the total precip to average per pixel
non_na_pixels <- precip[[1]] %>%
  values() %>%
  is.na() %>%
  `!` %>%
  sum() # total pixel with data in them (irregular boundary)


