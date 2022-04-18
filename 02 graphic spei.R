
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, hrbrthemes, extrafont, showtext, geodata, lubridate, zoo, SPEI, terra, rgeos, rnaturalearthdata, glue, stringr, sf, tidyverse, gtools, fs)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999)

# Function ----------------------------------------------------------------
make_graph <- function(ide, mdl){
  
  # ide <- i_hs[2]
  # mdl <- mdls[1]
  
  # Subsetting the datasets
  cat('Start', ide, mdl, sep = ' ', '\n')
  hst <- hist %>% filter(model == mdl & ID == ide)
  ftr <- ftre %>% filter(model == mdl & ID == ide)
  tbl <- rbind(hst, ftr)
  tbl <- mutate(tbl, date = paste0(Year, '-', month, '-01'))
  tbl <- dplyr::select(tbl, date, everything())
  tbl <- mutate(tbl, date = as.Date(date, format = '%Y-%m-%d'))
  
  # To tidy the table
  dfm <- tbl %>% dplyr::select(date, model, spei_01:spi_06) %>% gather(var, value, -date, -model)
  
  # SPEI 
  spei <- dfm %>% filter(var %in% c('spei_01', 'spei_03', 'spei_06'))
  lbl <- tibble(var = unique(spei$var), variable = c('SPEI 1', 'SPEI 3', 'SPEI 6'))
  spei <- inner_join(spei, lbl, by = 'var')
  
  # To make the graph SPEI
  g_spei <- ggplot(data = spei, aes(x = date, y = value)) + 
    geom_line() + 
    facet_wrap(.~variable, ncol = 1, nrow = 3) +
    scale_y_continuous(limits = c(-3, 3)) +
    labs(x = 'Date', y = 'SPEI') + 
    ggtitle(label = glue('SPEI / Coordinate ID: {ide}'), 
            subtitle = glue('Model: {mdl}')) +
    theme_ipsum_es() + 
    theme(axis.text.x = element_text(size = 30, family = 'Roboto'),
          axis.text.y = element_text(size = 30, family = 'Roboto'), 
          axis.title.x = element_text(size = 40, family = 'Roboto'),
          axis.title.y = element_text(size = 40, family = 'Roboto'), 
          plot.title = element_text(size = 48, face = 'bold', family = 'Roboto'),
          plot.subtitle = element_text(size = 48, face = 'bold', family = 'Roboto'), 
          strip.text = element_text(size = 40, face = 'bold', family = 'Roboto')) 
  
  out <- glue('../png/graphs/{ide}')
  ifelse(!file.exists(out), dir_create(out), print('Already exists!'))
  ggsave(plot = g_spei, filename = glue('{out}/SPEI {mdl} {ide}.png'), units = 'in', width = 7, height = 13, dpi = 300)
  
  # SPI 
  spi <- dfm %>% filter(var %in% c('spi_01', 'spi_03', 'spi_06'))
  lbl <- tibble(var = unique(spi$var), variable = c('SPI 1', 'SPI 3', 'SPI 6'))
  spi <- inner_join(spi, lbl, by = 'var')
  
  # To make the graph SPI
  g_spi <- ggplot(data = spi, aes(x = date, y = value)) + 
    geom_line() + 
    facet_wrap(.~variable, ncol = 1, nrow = 3) +
    scale_y_continuous(limits = c(-3, 3)) +
    labs(x = 'Date', y = 'SPEI') + 
    ggtitle(label = glue('SPI / Coordinate ID: {ide}'), 
            subtitle = glue('Model: {mdl}')) +
    theme_ipsum_es() + 
    theme(axis.text.x = element_text(size = 30, family = 'Roboto'),
          axis.text.y = element_text(size = 30, family = 'Roboto'), 
          axis.title.x = element_text(size = 40, family = 'Roboto'),
          axis.title.y = element_text(size = 40, family = 'Roboto'), 
          plot.title = element_text(size = 48, face = 'bold', family = 'Roboto'),
          plot.subtitle = element_text(size = 48, face = 'bold', family = 'Roboto'), 
          strip.text = element_text(size = 40, face = 'bold', family = 'Roboto')) 
  
  ggsave(plot = g_spi, filename = glue('{out}/SPI {mdl} {ide}.png'), units = 'in', width = 7, height = 13, dpi = 300)
  cat('Done!\n')
  
}


# Font --------------------------------------------------------------------
font_add_google(family = 'Roboto', name = 'Roboto condensed')
showtext_auto()

# Load data ---------------------------------------------------------------
root <- '../rds/clima'
hist <- root %>% dir_ls(., regexp = 'hist') %>% readRDS()
ftre <- root %>% dir_ls(., regexp = 'futu') %>% readRDS()

# Get the name of each model 
mdls <- unique(hist$model)
i_hs <- unique(hist$ID)
i_ft <- unique(ftre$ID)

# To make the map ---------------------------------------------------------

# All IDs and all models
purrr::map(.x = 1:length(mdls), .f = function(i){
  purrr::map(.x = 1:length(i_hs), .f = function(j){
    make_graph(mdl = mdls[i], ide = i_hs[j])  
  })
})

