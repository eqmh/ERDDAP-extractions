require("rerddap")
sstInfo <- info('erdVHsstaWS3day')
viirsSST1 <- griddap(sstInfo, latitude = c(12, 12), longitude = c(-74., -74.), time = c('2015-01-01','2015-12-31'), fields = 'sst')
tempTime <- as.Date(viirsSST1$data$time, origin = '1970-01-01', tz = "GMT")
tempFrame <- data.frame(time = tempTime, sst = viirsSST1$data$sst)

require("ggplot2")
ggplot(tempFrame, aes(time, sst)) + geom_line() + theme_bw() + ylab("sst") + ggtitle("VIIRS SST at (11N, 74W)")