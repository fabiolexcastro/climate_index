

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, geodata, SPEI, terra, rgeos, rnaturalearthdata, glue, stringr, sf, tidyverse, gtools, fs)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999)

# Source ------------------------------------------------------------------
source('PET.R')
calc_index <- function(mdl, tpe){
  
  cat(mdl, '\n')
  
  # Historical
  hst <- glue('{mdl}/{tpe}') %>% dir_ls()
  ids <- parse_number(basename(hst)) %>% sort()
  
  # To join the historical datasets
  hst.tbl <- purrr::map(hst, read.table, sep = ';', header = T) %>% 
    purrr::map(., as_tibble) %>% 
    bind_rows() %>% 
    mutate(month = str_sub(Date, 6, 7)) %>% 
    left_join(., crds[,c('ID', 'dem')], by = 'ID') %>% 
    drop_na() %>% 
    mutate(et0 = ETo(lat = Lat, z = dem, TN = Tmin, TX = Tmax, Rs = rsds, method_ETo = 'HS', doy = Doy)) %>% 
    group_by(ID, Long, Lat, Year, month) %>% 
    dplyr::summarise(Prec = sum(Prec), 
                     Tmin = mean(Tmin), 
                     Tmax = mean(Tmax), 
                     rsds = mean(rsds), 
                     et0 = sum(et0)) %>% 
    ungroup() %>% 
    left_join(., crds[,c('ID', 'dem')], by = 'ID') %>% 
    mutate(balance = Prec - et0)
  
  ids <- unique(hst.tbl$ID)
  
  # To calculate the SPEI 
  hst.tbl <- map(.x = 1:length(ids), .f = function(i){
    cat(i, '\n')
    df <- filter(hst.tbl, ID == ids[i])
    
    spei <- list()
    spi  <- list()
    
    for(i in c(1, 3, 6)){
      spei[[i]] <- as.numeric(SPEI::spei(data = df$balance, scale = i)$fitted)
      spi[[i]] <- as.numeric(SPEI::spi(data = df$Prec, scale = i)$fitted)
    }
    
    spei <- spei[!sapply(spei, is.null)]
    spi <- spi[!sapply(spi, is.null)]
    df <- mutate(df, 
                 spei_01 = spei[[1]], 
                 spei_03 = spei[[2]], 
                 spei_06 = spei[[3]],
                 spi_01 = spi[[1]], 
                 spi_03 = spi[[2]], 
                 spi_06 = spei[[3]]) 
    return(df)
  }) %>% 
    bind_rows() %>% 
    mutate(model = basename(mdl))
  
  cat('Done!\n')
  return(hst.tbl) 
  
}

# Load data ---------------------------------------------------------------
wrld <- rnaturalearthdata::map_units50 %>% st_as_sf() 
wrld <- wrld[st_is_valid(wrld),]
root <- '../tbl/cm6'
mdls <- dir_ls(root) %>% as.character()

# Testing
test <- dir_ls(mdls[1]) %>% grep('ssp585', ., value = T) %>% as.character() %>% dir_ls()
test <- read.table(test[2], sep = ';', header = T) %>% as_tibble()

# Checking the coordinates ------------------------------------------------
crds <- mdls[1] %>% dir_ls() %>% .[1] %>% dir_ls() %>% map(., read.table, header = T, sep = ';') %>% map(., slice, 1) %>% bind_rows() %>% distinct(ID, Long, Lat)
crds <- filter(crds, Long < 100)

plot(st_geometry(wrld))
points(crds$Long, crds$Lat, pch = 16, col = 'red')

crds <- st_as_sf(x = crds, coords = c('Long', 'Lat'), crs = st_crs(4326))
crds <- mutate(crds, country = st_intersection(crds, wrld)[,'admin'] %>% pull(admin))
crds <- st_coordinates(crds) %>% as.data.frame %>% mutate(ID = pull(crds, 1), country = pull(crds, 3))

# Download SRTM  ----------------------------------------------------------

# Extract the country for the points
srt1 <- geodata::elevation_3s(lon = 12.87, lat = 3.8, path = '../tmpr')
srt2 <- geodata::elevation_3s(lon = 8.875, lat = 6.125, path = '../tmpr')
srt3 <- geodata::elevation_3s(lon = 4.875, lat = 7.375, path = '../tmpr')
srtm <- terra::mosaic(x = srt1, y = srt2)
srtm <- terra::mosaic(x = srtm, y = srt3)
crds <- mutate(crds, dem = terra::extract(srtm, crds[,c(1, 2)])[,2])

# To calculate the index --------------------------------------------------
hist <- map2(.x = mdls, .y = rep('Hist', length(mdls)), .f = calc_index)
hist <- bind_rows(hist)
hist <- dplyr::select(hist, model, everything())
dir.create('../rds/clima', recursive = T)
saveRDS(object = hist, file = '../rds/clima/historical_models.rds')


ftre <- map2(.x = mdls, .y = rep('ssp585', length(mdls)), .f = calc_index)
ftre <- bind_rows(ftre)


calc_index <- function(mdl, tpe){
  
  mdl <- mdls[6]
  tpe <- 'ssp585'
  
  # Historical
  hst <- glue('{mdl}/{tpe}') %>% dir_ls()
  ids <- parse_number(basename(hst)) %>% sort()
  
  # To join the historical datasets
  hst.tbl <- purrr::map(hst, read.table, sep = ';', header = T) %>% 
    purrr::map(., as_tibble) %>% 
    bind_rows() %>% 
    mutate(month = str_sub(Date, 6, 7)) %>% 
    left_join(., crds[,c('ID', 'dem')], by = 'ID') %>% 
    drop_na()
  
  for(i in 1:nrow(hst.tbl)){
    cat(i, '\n')
    hst.tbl[i,] %>% 
      mutate(et0 = ETo(lat = Lat, z = dem, TN = Tmin, TX = Tmax, Rs = rsds, method_ETo = 'HS', doy = Doy))
  }
  
  
  rsl <- hst.tbl %>% 
    mutate(et0 = ETo(lat = Lat, z = dem, TN = Tmin, TX = Tmax, Rs = rsds, method_ETo = 'HS', doy = Doy)) %>% 
    group_by(ID, Long, Lat, Year, month) %>% 
    dplyr::summarise(Prec = sum(Prec), 
                     Tmin = mean(Tmin), 
                     Tmax = mean(Tmax), 
                     rsds = mean(rsds), 
                     et0 = sum(et0)) %>% 
    ungroup() %>% 
    left_join(., crds[,c('ID', 'dem')], by = 'ID') %>% 
    mutate(balance = Prec - et0)
  
  ids <- unique(hst.tbl$ID)
  
  # To calculate the SPEI 
  hst.tbl <- map(.x = 1:length(ids), .f = function(i){
    cat(i, '\n')
    df <- filter(hst.tbl, ID == ids[i])
    
    spei <- list()
    spi  <- list()
    
    for(i in c(1, 3, 6)){
      spei[[i]] <- as.numeric(SPEI::spei(data = df$balance, scale = i)$fitted)
      spi[[i]] <- as.numeric(SPEI::spi(data = df$Prec, scale = i)$fitted)
    }
    
    spei <- spei[!sapply(spei, is.null)]
    spi <- spi[!sapply(spi, is.null)]
    df <- mutate(df, 
                 spei_01 = spei[[1]], 
                 spei_03 = spei[[2]], 
                 spei_06 = spei[[3]],
                 spi_01 = spi[[1]], 
                 spi_03 = spi[[2]], 
                 spi_06 = spei[[3]]) 
    return(df)
  }) %>% 
    bind_rows() %>% 
    mutate(model = basename(mdl))
  
  cat('Done!\n')
  return(hst.tbl) 
  
}