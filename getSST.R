### Get SST timeseries from satellite products using erddap
### the source of data is `jplMURSST41`. See https://coastwatch.pfeg.noaa.gov/erddap/info/jplMURSST41/index.html
### data is extracted with `rerddap::griddap` for a particular coordinate and stored as csv file.
### E Klein. eklein@usb.ve
### 2019-04-10

library(readr)
library(rerddap)
library(lubridate)
library(dplyr)
library(flexdashboard)
library(reshape2)
library(leaflet)
library(ggplot2)
library(vegan)
library(xts)
library(dygraphs)
library(plotly)

library(RColorBrewer)
palette(brewer.pal(8, "Set2"))

## functions

## remove all spaces from string
NoSpaces = function(x){
  return(gsub(" ", "", x))
}

## set site coordinates and time for SST extraction
SSTSiteName = "Playa 7 Olas"   ## for the resulting file name
SSTcoords.lon = -74.
SSTcoords.lat = 12.

SSTstartDate = "2002-06-01"

## set climatological date start-end
SSTclimStartDate = "2002-06-01"
SSTclimEndDate = "2012-12-31"

## set dataset source
SSTsource = info("jplMURSST41")

##
## Get sst 
SST <- griddap(SSTsource, 
              time=c(SSTstartDate, "last"),
              longitude = c(SSTcoords.lon,SSTcoords.lon),
              latitude = c(SSTcoords.lat,SSTcoords.lat),
              fields = "analysed_sst",
              fmt = "csv")

SST = SST[,c(1,4)]
names(SST) = c("time", "SST")

## convert time to a Data object
SST$time = as.Date(ymd_hms(SST$time))

##
## Calculate climatology
SST.clim = SST %>% filter(time>=ymd(SSTclimStartDate), time<=SSTclimEndDate) %>% 
  group_by(yDay = yday(time)) %>% 
  summarise(SST.mean = mean(SST),
            SST.median = median(SST),
            SST.sd = sd(SST),
            SST.q5 = quantile(SST, 0.05),
            SST.q10 = quantile(SST, 0.10),
            SST.q25 = quantile(SST, 0.25),
            SST.q75 = quantile(SST, 0.75),
            SST.q90 = quantile(SST, 0.90),
            SST.q95 = quantile(SST, 0.95),
            SST.min = min(SST),
            SST.max = max(SST))

## Plot SST
SST.xts = as.xts(SST$SST, SST$time)
dygraph(SST.xts, 
        ylab = "Sea Surface Temperature °C") %>% 
  dySeries("V1", label ="SST Â°C", color = "steelblue") %>%
  dyHighlight(highlightCircleSize = 5, 
              highlightSeriesBackgroundAlpha = 0.2,
              hideOnMouseOut = FALSE) %>% 
  dyOptions(fillGraph = FALSE, fillAlpha = 0.4) %>% 
  dyRangeSelector(dateWindow = c(max(SST$time) - years(5), max(SST$time)))

### SST Last year with smoothed Climatology {data-width=250}

## subset SST for last year
SST.lastyear = SST %>% filter(year(time)==max(year(time)))

## make the plot
pp = ggplot(SST.clim, aes(yDay, SST.mean))
pp = pp + geom_line() + geom_smooth(span=0.25, se=FALSE, colour="steelblue") +  
  geom_ribbon(aes(ymin=SST.q25, ymax=SST.q75), fill="steelblue", alpha=0.5) +
  geom_line(data=SST.lastyear, aes(yday(time), SST), colour="red") + 
  ylab("Sea Surface Temperature °C") + xlab("Day of the Year") + 
  theme_bw(base_size = 9) 
ggplotly(pp) %>% plotly::config(displayModeBar = F) 

## save SST
write_csv(SST, path = paste0(NoSpaces(SSTSiteName), "_SST.csv"))
write_csv(SST.clim, path = paste0(NoSpaces(SSTSiteName), "_Climatology.csv"))




