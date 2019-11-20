### griddap function example
### Map latest SST monthly value from GHRSST using erddap
### the source of data is `jplMURSST41`. See https://coastwatch.pfeg.noaa.gov/erddap/info/jplMURSST41/index.html
### By E. Montes (emontesh@usf.edu) 
### 2019-04-10

library(ggplot2)
library(mapdata)
library(rerddap)

require("ggplot2")
require("mapdata")
require("rerddap")
sstInfo <- info('jplMURSST41')

## Define coordinates
latitude = c(8., 16.)
longitude = c(-84., -64.)

# get latest daily sst
murSST <- griddap(sstInfo, latitude=c(8., 16.), longitude=c(-84., -64.), time = c('last','last'), fields = 'analysed_sst')
mycolor <- colors$temperature
w <- map_data("worldHires", ylim = c(8., 16.), xlim = c(-84., -64.))

## plot SST map
ggplot(data = murSST$data, aes(x = lon, y = lat, fill = analysed_sst)) +
  geom_polygon(data = w, aes(x = long, y = lat, group = group), fill = "grey80") +
  geom_raster(interpolate = FALSE) +
  scale_fill_gradientn(colours = mycolor, na.value = NA) +
  theme_bw() + ylab("latitude") + xlab("longitude") +
  coord_fixed(1.3, xlim = c(-84., -64.),  ylim = c(8., 16.)) + ggtitle("Latest MUR SST")
