### Map latest CHL monthly value from VIIRS satellite using erddap
### the source of data is `nesdisVHNSQchlaMonthly`(VIIRS) and 'erdMH1chla8day' (MODIS). See https://coastwatch.pfeg.noaa.gov/erddap/griddap/nesdisVHNSQchlaMonthly.html
### data is extracted with `rerddap::griddap` for a selected region of interest.
### By E. Montes (emontesh@usf.edu) 
### 2020-08-31

require("rerddap")
require("ggplot2")
require("mapdata")

# get latest Monthly chl (VIIRS)
chlaInfo <- info('nesdisVHNSQchlaMonthly')
viirsCHLA <- griddap(chlaInfo, latitude = c(-20., -60.), longitude = c(-90., -47.), time = c('last','last'), fields = 'chlor_a')

# get latest 8-day chl (MODIS)
chlaInfo_8d <- info('erdMH1chla8day')
MODIS_CHLA_8d <- griddap(chlaInfo_8d, latitude = c(-20., -60.), longitude = c(-90., -47.), time = c('last','last'), fields = 'chlorophyll')

# Map monthly chl (VIIRS)
mycolor <- colors$chlorophyll
w <- map_data("worldHires", ylim = c(-60., -20.), xlim = c(-90., -47.))
ggplot(data = viirsCHLA$data, aes(x = lon, y = lat, fill = log(chlor_a))) + 
  geom_polygon(data = w, aes(x = long, y = lat, group = group), fill = "grey80") +
  geom_raster(interpolate = FALSE) +
  scale_fill_gradientn(colours = mycolor, na.value = NA) +
  theme_bw() + ylab("latitude") + xlab("longitude") +
  coord_fixed(1.3, xlim = c(-90., -47.),  ylim = c(-60., -20.)) + ggtitle("Latest VIIRS Monthly Chla")

# Map 8-day chl (MODIS)
mycolor <- colors$chlorophyll
w <- map_data("worldHires", ylim = c(-60., -20.), xlim = c(-90., -47.))
ggplot(data = MODIS_CHLA_8d$data, aes(x = lon, y = lat, fill = log(chlorophyll))) + 
  geom_polygon(data = w, aes(x = long, y = lat, group = group), fill = "grey80") +
  geom_raster(interpolate = FALSE) +
  scale_fill_gradientn(colours = mycolor, na.value = NA) +
  theme_bw() + ylab("latitude") + xlab("longitude") +
  coord_fixed(1.3, xlim = c(-90., -47.),  ylim = c(-60., -20.)) + ggtitle("Latest MODIS 8-day Chla")


