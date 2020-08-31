### Map latest SST monthly value from GHRSST using erddap
### See https://coastwatch.pfeg.noaa.gov/erddap/griddap/nesdisVHNSQchlaMonthly.html
### data is extracted with `rerddap::griddap` for a selected region of interest.
### By E. Montes (emontesh@usf.edu) 
### 2020-08-31

require("rerddap")
sstInfo <- info('erdVHsstaWS3day')
viirsSST1 <- griddap(sstInfo, latitude = c(12, 12), longitude = c(-74., -74.), time = c('2015-01-01','2015-12-31'), fields = 'sst')
tempTime <- as.Date(viirsSST1$data$time, origin = '1970-01-01', tz = "GMT")
tempFrame <- data.frame(time = tempTime, sst = viirsSST1$data$sst)

require("ggplot2")
ggplot(tempFrame, aes(time, sst)) + geom_line() + theme_bw() + ylab("sst") + ggtitle("VIIRS SST at (11N, 74W)")