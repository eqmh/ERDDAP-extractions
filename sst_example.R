### Map latest SST monthly value from GHRSST product using erddap
### the source of data is `jplMURSST41` See https://coastwatch.pfeg.noaa.gov/erddap/griddap/jplMURSST41.html
### data is extracted with `rerddap::griddap` for a selected region of interest.
### By E. Montes (emontesh@usf.edu) 
### 2020-08-31

require("rerddap")
require("ggplot2")
require("mapdata")

sstInfo <- info('jplMURSST41')
# get latest 3-day composite sst
GHRSST <- griddap(sstInfo, latitude = c(-60., -20.), longitude = c(-90., -47.), time = c('last','last'), fields = 'analysed_sst')

mycolor <- colors$temperature
w <- map_data("worldHires", ylim = c(-60., -20.), xlim = c(-90., -47.))
ggplot(data = GHRSST$data, aes(x = lon, y = lat, fill = analysed_sst)) + 
  geom_polygon(data = w, aes(x = long, y = lat, group = group), fill = "grey80") +
  geom_raster(interpolate = FALSE) +
  scale_fill_gradientn(colours = mycolor, na.value = NA) +
  theme_bw() + ylab("latitude") + xlab("longitude") +
  coord_fixed(1.3, xlim = c(-90., -47.),  ylim = c(-60., -20.)) + ggtitle("Latest daily SST data")