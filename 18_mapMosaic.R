

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, gtools, tidyverse)

# Load data ---------------------------------------------------------------
mps <- shapefile('../shapefiles/base/mpios_zone.shp')
lbl <- data.frame(month_abb = month.abb, mes = 1:12)
tav <- list.files('../raster/climate/worldclim/baseline/1ha', full.names = TRUE, pattern = '.tif')
tav <- grep('tmean_', tav, value = TRUE)
tav <- tav[1:12]
tav <- mixedsort(tav)  
tav <- stack(tav)

# Making the map ----------------------------------------------------------
vls <- rasterToPoints(tav) %>% 
  as_tibble() %>% 
  gather(var, value, -x, -y) %>% 
  mutate(mes = parse_number(var)) %>% 
  inner_join(., lbl, by = 'mes') %>% 
  dplyr::select(x, y, month_abb, value) %>% 
  mutate(month_abb = factor(month_abb, levels = month.abb))
vls <- vls %>% mutate(value = value / 10)
vls %>% 
  filter(month_abb == 'Jan')
summary(vls$value)

gg <- ggplot(vls)  +
  geom_tile(aes(x = x, y =  y, fill = value)) +
  facet_wrap(~ month_abb) +
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(n = 8, name = "YlOrRd"), 
                       na.value = 'white', limits = c(0, 28), breaks = seq(0, 28, 4)) +
  geom_polygon(data = mps, aes(x=long, y = lat, group = group), color = 'grey', fill='NA') +
  # geom_polygon(data = dpt, aes(x=long, y = lat, group = group), color = 'grey', fill='NA') +
  theme_bw() +
  scale_x_continuous(breaks = c(-78, -77, -76, -75)) +
  coord_equal(xlim = extent(mps)[1:2], ylim = extent(mps)[3:4]) +
  labs(title = 'Temperatura promedio mensual linea base', fill = '°C',  x = 'Longitud', y = 'Latitud') +
  theme(legend.position = 'bottom',
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key.width = unit(5, 'line')) +
  guides(shape = guide_legend(override.aes = list(size = 10)))

# Saving the plot
ggsave(plot = gg, filename = '../png/mapas/Mapa Temp avg mensual baseline.png', width = 9, height = 12, units = 'in', dpi = 300)


# Precipitation
ppt <- list.files('../raster/climate/worldclim/baseline/1ha', full.names = TRUE, pattern = '.tif')
ppt <- grep('prec_', ppt, value = TRUE)
ppt <- ppt[1:12]
ppt <- mixedsort(ppt)  
ppt <- stack(ppt)

# Making the map ----------------------------------------------------------
vls <- rasterToPoints(ppt) %>% 
  as_tibble() %>% 
  gather(var, value, -x, -y) %>% 
  mutate(mes = parse_number(var)) %>% 
  inner_join(., lbl, by = 'mes') %>% 
  dplyr::select(x, y, month_abb, value) %>% 
  mutate(month_abb = factor(month_abb, levels = month.abb))
vls %>% 
  filter(month_abb == 'Jan')
summary(vls$value)

gg <- ggplot(vls)  +
  geom_tile(aes(x = x, y =  y, fill = value)) +
  facet_wrap(~ month_abb) +
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(n = 8, name = "GnBu"), 
                       na.value = 'white', limits = c(0, 500), breaks = seq(0, 500, 50)) +
  geom_polygon(data = mps, aes(x=long, y = lat, group = group), color = 'grey', fill='NA') +
  # geom_polygon(data = dpt, aes(x=long, y = lat, group = group), color = 'grey', fill='NA') +
  theme_bw() +
  scale_x_continuous(breaks = c(-78, -77, -76, -75)) +
  coord_equal(xlim = extent(mps)[1:2], ylim = extent(mps)[3:4]) +
  labs(title = 'Precipitación acumulada linea base', fill = 'mm',  x = 'Longitud', y = 'Latitud') +
  theme(legend.position = 'bottom',
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key.width = unit(5, 'line')) +
  guides(shape = guide_legend(override.aes = list(size = 10)))

# Saving the plot
ggsave(plot = gg, filename = '../png/mapas/Mapa Ppt acum mensual baseline.png', width = 9, height = 12, units = 'in', dpi = 300)


