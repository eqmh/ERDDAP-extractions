### Map latest CHL monthly value from VIIRS satellite using erddap
### the source of data is `nesdisVHNSQchlaMonthly`. See https://coastwatch.pfeg.noaa.gov/erddap/griddap/nesdisVHNSQchlaMonthly.html
### data is extracted with `rerddap::griddap` for a particular coordinate and stored as csv file.
### By E. Montes (emontesh@usf.edu) 
### 2019-04-10

require("rerddap")
require("ggplot2")
require("mapdata")

# get latest daily chl
chlaInfo <- info('nesdisVHNSQchlaMonthly')
viirsCHLA <- griddap(chlaInfo, latitude = c(8., 16.), longitude = c(-84, -64), time = c('last','last'), fields = 'chlor_a')


mycolor <- colors$chlorophyll
w <- map_data("worldHires", ylim = c(8., 16.), xlim = c(-84., -64.))
ggplot(data = viirsCHLA$data, aes(x = lon, y = lat, fill = log(chlor_a))) + 
  geom_polygon(data = w, aes(x = long, y = lat, group = group), fill = "grey80") +
  geom_raster(interpolate = FALSE) +
  scale_fill_gradientn(colours = mycolor, na.value = NA) +
  theme_bw() + ylab("latitude") + xlab("longitude") +
  coord_fixed(1.3, xlim = c(-84., -64.),  ylim = c(8., 16.)) + ggtitle("Latest VIIRS Monthly Chla")




