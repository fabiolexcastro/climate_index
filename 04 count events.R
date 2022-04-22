
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, sf, fs, tidyverse, crayon, glue, ggthemes, colourpicker)

# Function ----------------------------------------------------------------
count_event <- function(gid, mdl, vrb){
  
  # gid <- gids[1]; mdl <- mdls[1]; vrb <- vrbl[3]
  
  cat(bgYellow(gid, mdl, vrb, '\n', sep = ' '))
  dbs <- filter(dbse, model == mdl, ID == gid)
  dbs <- dplyr::select(dbs, model, ID, Year, month, vrb)
  colnames(dbs)[5] <- 'value'
  
  cat(bgMagenta('To calculate the frequence\n'))
  rsl <- rbind(dbs %>% filter(value <= -1.5) %>% group_by(Year) %>% summarise(count = n()) %>% ungroup() %>% mutate(type = 'negative'),
               dbs %>% filter(value >=  1.5) %>% group_by(Year) %>% summarise(count = n()) %>% ungroup() %>% mutate(type = 'positive'))
  rsl <- mutate(rsl, variable = vrb, model = mdl, id = gid)
  cat(bgGreen('Finish\n'))
  return(rsl)
  
}
make_graph <- function(tble){
  gplt <- ggplot() + 
    geom_col(data = tble, aes(x = dcde, y = frequence, fill = type, col = type), position = 'dodge') + 
    facet_wrap(.~model) + 
    scale_fill_manual(values = c('#47ABBA', '#B88746'), labels = c('Wet', 'Dry')) +
    scale_color_manual(values = c('#47ABBA', '#B88746')) +
    labs(x = 'Decade', y = 'Frequence months', fill = '') +
    guides(fill = guide_legend(ncol = 2), col = 'none')  +
    theme_minimal() +
    theme(legend.position = 'bottom', 
          legend.text = element_text(family = 'serif', size = 48),
          axis.text.x = element_text(family = 'serif', size = 44), 
          axis.text.y = element_text(family = 'serif', size = 44), 
          axis.title.x = element_text(family = 'serif', size = 46),
          axis.title.y = element_text(family = 'serif', size = 46),
          strip.text.x = element_text(family = 'serif', face = 'bold', size = 60),
          strip.text.y = element_text(family = 'serif', face = 'bold', size = 60),
          panel.grid.major.x = element_blank(), 
          panel.grid.major.y = element_line(linetype = 'dashed'))
}

# Load data ---------------------------------------------------------------
root <- '../rds/clima'
hist <- root %>% dir_ls(., regexp = 'hist') %>% readRDS()
ftre <- root %>% dir_ls(., regexp = 'futu') %>% readRDS() %>% dplyr::select(model, everything())
dbse <- rbind(hist, ftre)

gids <- unique(dbse$ID)
mdls <- unique(dbse$model)
vrbl <- dplyr::select(dbse, starts_with('sp')) %>% colnames()

# Decades
dcds <- tibble(year = 1980:2060, dcde = c(rep(1980, 10), rep(1990, 10), rep(2000, 10), rep(2010, 10), rep(2020, 10), rep(2030, 10), rep(2040, 10), rep(2050, 11)))

# Models 
wrnw <- 'INM_CM4_8'
htwt <- 'CANESM5'
wrdr <- 'MIROC_ES2L'
htdr <- 'EC_EARTH3'
ensm <- 'GFDL_ESM4'

# Count  ------------------------------------------------------------------
freq <- purrr::map(1:length(gids), function(i){
  purrr::map(1:length(mdls), function(j){
    purrr::map(1:length(vrbl), function(k){
      count_event(gid = gids[i], mdl = mdls[j], vrb = vrbl[k])
    })
  })
})

rslt <- flatten(freq) %>% flatten() %>% bind_rows()
write.csv(rslt, '../tbl/frq/freq_events.csv', row.names = FALSE)

# To make the graph -------------------------------------------------------

# SPEI 06 -----------------------------------------------------------------

# Warn-wet
ww_sp06 <- filter(rslt, variable == 'spei_06', model == wrnw)
ww_sp06 <- inner_join(ww_sp06, dcds, by = c('Year' = 'year')) %>% dplyr::select(dcde, id, Year, model, everything())
ww_sp06 <- ww_sp06 %>% group_by(dcde, id, model, type, variable) %>% summarise(count = sum(count)) %>% ungroup()
ww_sp06 <- ww_sp06 %>% spread(type, count)

# Hot-wet
hw_sp06 <- filter(rslt, variable == 'spei_06', model == htwt)
hw_sp06 <- inner_join(hw_sp06, dcds, by = c('Year' = 'year')) %>% dplyr::select(dcde, id, Year, model, everything())
hw_sp06 <- hw_sp06 %>% group_by(dcde, id, model, type, variable) %>% summarise(count = sum(count)) %>% ungroup()
hw_sp06 <- hw_sp06 %>% spread(type, count)

# Warn-dry
wd_sp06 <- filter(rslt, variable == 'spei_06', model == wrdr)
wd_sp06 <- inner_join(wd_sp06, dcds, by = c('Year' = 'year')) %>% dplyr::select(dcde, id, Year, model, everything())
wd_sp06 <- wd_sp06 %>% group_by(dcde, id, model, type, variable) %>% summarise(count = sum(count)) %>% ungroup()
wd_sp06 <- wd_sp06 %>% spread(type, count)

# Hot - dry
hd_sp06 <- filter(rslt, variable == 'spei_06', model == htdr)
hd_sp06 <- inner_join(hd_sp06, dcds, by = c('Year' = 'year')) %>% dplyr::select(dcde, id, Year, model, everything())
hd_sp06 <- hd_sp06 %>% group_by(dcde, id, model, type, variable) %>% summarise(count = sum(count)) %>% ungroup()
hd_sp06 <- hd_sp06 %>% spread(type, count)

# Ensemble
en_sp06 <- filter(rslt, variable == 'spei_06', model == ensm)
en_sp06 <- inner_join(en_sp06, dcds, by = c('Year' = 'year')) %>% dplyr::select(dcde, id, Year, model, everything())
en_sp06 <- en_sp06 %>% group_by(dcde, id, model, type, variable) %>% summarise(count = sum(count)) %>% ungroup()
en_sp06 <- en_sp06 %>% spread(type, count)

# Join --------------------------------------------------------------------
rslt <- rbind(ww_sp06, hw_sp06, wd_sp06, hd_sp06, en_sp06)
rslt <- mutate(rslt, dcde = factor(dcde, levels = unique(rslt$dcde)))
rslt <- gather(rslt, type, frequence, -dcde, -model, -variable, -id)
rslt <- mutate(rslt, type = factor(type, levels = c('negative', 'positive')))

# To make the graph -------------------------------------------------------
gids <- unique(rslt$id)

# 56 ----------------------------------------------------------------------
r0056 <- rslt %>% filter(id == 56)
g0056 <- make_graph(tble = r0056)
ggsave(plot = g0056, filename = '../png/graphs/frequence events/frequence_spei_06_0056.png', units = 'in', width = 13, height = 8, dpi = 300)
        
# 132 ---------------------------------------------------------------------
r0132 <- rslt %>% filter(id == 132)
g0132 <- make_graph(tble = r0132)
ggsave(plot = g0132, filename = '../png/graphs/frequence events/frequence_spei_06_0132.png', units = 'in', width = 13, height = 8, dpi = 300)

# 432 ---------------------------------------------------------------------
r0432 <- rslt %>% filter(id == 432)
g0432 <- make_graph(tble = r0432)
ggsave(plot = g0432, filename = '../png/graphs/frequence events/frequence_spei_06_0432.png', units = 'in', width = 13, height = 8, dpi = 300)

# 508 ---------------------------------------------------------------------
r0508 <- rslt %>% filter(id == 508)
g0508 <- make_graph(tble = r0508)
ggsave(plot = g0508, filename = '../png/graphs/frequence events/frequence_spei_06_0508.png', units = 'in', width = 13, height = 8, dpi = 300)

# 1669 --------------------------------------------------------------------
r1669 <- rslt %>% filter(id == 1669)
g1669 <- make_graph(tble = r1669)
ggsave(plot = g1669, filename = '../png/graphs/frequence events/frequence_spei_06_1669.png', units = 'in', width = 13, height = 8, dpi = 300)

# 1762 --------------------------------------------------------------------
r1762 <- rslt %>% filter(id == 1762)
g1762 <- make_graph(tble = r1762)
ggsave(plot = g1762, filename = '../png/graphs/frequence events/frequence_spei_06_1762.png', units = 'in', width = 13, height = 8, dpi = 300)

# 2266 --------------------------------------------------------------------
r2266 <- rslt %>% filter(id == 2266)
g2266 <- make_graph(tble = r2266)
ggsave(plot = g2266, filename = '../png/graphs/frequence events/frequence_spei_06_2266.png', units = 'in', width = 13, height = 8, dpi = 300)

# 2731 --------------------------------------------------------------------
r2731 <- rslt %>% filter(id == 2731)
g2731 <- make_graph(tble = r2731)
ggsave(plot = g2731, filename = '../png/graphs/frequence events/frequence_spei_06_2731.png', units = 'in', width = 13, height = 8, dpi = 300)


