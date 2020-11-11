
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, gtools, tidyverse)

# Load data ---------------------------------------------------------------
mps <- shapefile('../shapefiles/base/mpios_zone.shp')
lbl <- data.frame(month_abb = month.abb, mes = 1:12)
gcm <- list.files('../raster/climate/ipcc/rcp_4.5/2050s', full.names = TRUE)

# Precipitation -----------------------------------------------------------
ppt <- map(.x = 1:length(gcm), .f = function(k){
  list.files(gcm[k], full.names = TRUE, pattern = '.tif$') %>% 
    grep('ha_prec_', ., value = TRUE) %>% 
    mixedsort() %>% 
    stack()
})

pp2 <- lapply(1:12, function(j)mean(stack(lapply(1:30, function(k) ppt[[j]][[1]]))))
pp2 <- stack(pp2)
names(pp2) <- paste0('prec_', 1:12)
vls.ppt <- rasterToPoints(pp2) %>% 
  as_tibble() %>% 
  gather(var, value, -x, -y) %>% 
  mutate(mes = parse_number(var)) %>% 
  inner_join(., lbl, by = 'mes') %>% 
  dplyr::select(x, y, month_abb, value) %>% 
  mutate(month_abb = factor(month_abb, levels = month.abb))

summary(vls.ppt$value)

gg.ppt <- ggplot(vls.ppt)  +
  geom_tile(aes(x = x, y =  y, fill = value)) +
  facet_wrap(~ month_abb) +
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(n = 8, name = "GnBu"), 
                       na.value = 'white', limits = c(0, 150), breaks = seq(0, 150, 25)) +
  geom_polygon(data = mps, aes(x=long, y = lat, group = group), color = 'grey', fill='NA') +
  # geom_polygon(data = dpt, aes(x=long, y = lat, group = group), color = 'grey', fill='NA') +
  theme_bw() +
  # scale_x_continuous(breaks = c(-78, -77, -76, -75)) +
  coord_equal(xlim = extent(mps)[1:2], ylim = extent(mps)[3:4]) +
  labs(title = 'Precipitación acumulada RCP 4.5 2080s', fill = 'mm',  x = 'Longitud', y = 'Latitud') +
  theme(legend.position = 'bottom',
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key.width = unit(5, 'line')) +
  guides(shape = guide_legend(override.aes = list(size = 10)))

# Saving the plot
ggsave(plot = gg.ppt, filename = '../png/mapas/Mapa Ppt acum mensual rcp 45 2080s.png', width = 9, height = 12, units = 'in', dpi = 300)

# Temperature -------------------------------------------------------------
tmp <- map(.x = 1:length(gcm), .f = function(k){
  list.files(gcm[k], full.names = TRUE, pattern = '.tif$') %>% 
    grep('ha_tmean_', ., value = TRUE) %>% 
    mixedsort() %>% 
    stack()
})

tm2 <- lapply(1:12, function(j)mean(stack(lapply(1:30, function(k) tmp[[j]][[1]]))))
tm2 <- stack(tm2)
names(tm2) <- paste0('tmean_', 1:12)
vls.tmp <- rasterToPoints(tm2) %>% 
  as_tibble() %>% 
  gather(var, value, -x, -y) %>% 
  mutate(mes = parse_number(var)) %>% 
  inner_join(., lbl, by = 'mes') %>% 
  dplyr::select(x, y, month_abb, value) %>% 
  mutate(month_abb = factor(month_abb, levels = month.abb))
vls.tmp <- vls.tmp %>% mutate(value = value/10)
summary(vls.tmp$value)

gg.tmp <- ggplot(vls.tmp)  +
  geom_tile(aes(x = x, y =  y, fill = value)) +
  facet_wrap(~ month_abb) +
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(n = 8, name = "YlOrRd"), 
                       na.value = 'white', limits = c(0, 30), breaks = seq(0, 30, 3)) +
  geom_polygon(data = mps, aes(x=long, y = lat, group = group), color = 'grey', fill='NA') +
  # geom_polygon(data = dpt, aes(x=long, y = lat, group = group), color = 'grey', fill='NA') +
  theme_bw() +
  # scale_x_continuous(breaks = c(-78, -77, -76, -75)) +
  coord_equal(xlim = extent(mps)[1:2], ylim = extent(mps)[3:4]) +
  labs(title = 'Temperatura promedio RCP 4.5 2050s', fill = '°C',  x = 'Longitud', y = 'Latitud') +
  theme(legend.position = 'bottom',
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key.width = unit(5, 'line')) +
  guides(shape = guide_legend(override.aes = list(size = 10)))

# Saving the plot
ggsave(plot = gg.tmp, filename = '../png/mapas/Mapa Tmp prom mensual rcp 45 2080s.png', width = 9, height = 12, units = 'in', dpi = 300)

