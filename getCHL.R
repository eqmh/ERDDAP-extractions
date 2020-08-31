### Get CHL timeseries from satellite products using erddap
### the source of data is `nesdisVHNSQchlaMonthly`. See https://coastwatch.pfeg.noaa.gov/erddap/griddap/nesdisVHNSQchlaMonthly.html
### data is extracted with `rerddap::griddap` for a particular coordinate and stored as csv file.
### E Klein. eklein@usb.ve modified by E. Montes (emontesh@usf.edu) 
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

## set site coordinates and time for CHL extraction
CHLSiteName = "GPatagonia"   ## for the resulting file name
CHLcoords.lon = -64
CHLcoords.lat = -41.7

CHLstartDate = "2012-01-01"

## set climatological date start-end
CHLclimStartDate = "2012-01-01"
CHLclimEndDate = "2016-12-31"

## set dataset source
CHLsource = info("erdMH1chla8day")

##
## Get CHL 
CHL <- griddap(CHLsource, 
               time=c(CHLstartDate, "last"),
               longitude = c(CHLcoords.lon,CHLcoords.lon),
               latitude = c(CHLcoords.lat,CHLcoords.lat),
               fields = "chlorophyll", fmt = "csv")

CHL = CHL[,c(1,4)]
names(CHL) = c("time", "CHL")
CHL = na.omit(CHL)

## convert time to a Data object
CHL$time = as.Date(ymd_hms(CHL$time))

##
## Calculate climatology
CHL.clim = CHL %>% filter(time>=ymd(CHLclimStartDate), time<=CHLclimEndDate) %>% 
  group_by(yDay = yday(time)) %>% 
  summarise(CHL.mean = mean(CHL),
            CHL.median = median(CHL),
            CHL.sd = sd(CHL),
            CHL.q5 = quantile(CHL, 0.05),
            CHL.q10 = quantile(CHL, 0.10),
            CHL.q25 = quantile(CHL, 0.25),
            CHL.q75 = quantile(CHL, 0.75),
            CHL.q90 = quantile(CHL, 0.90),
            CHL.q95 = quantile(CHL, 0.95),
            CHL.min = min(CHL),
            CHL.max = max(CHL))

## Plot CHL
CHL.xts = as.xts(CHL$CHL, CHL$time)
dygraph(CHL.xts, 
        ylab = "Chlorophyll a (mg m-3) @ lon: -64 and lat: 41.7") %>% 
  dySeries("V1", label ="CHL", color = "steelblue") %>%
  dyHighlight(highlightCircleSize = 5, 
              highlightSeriesBackgroundAlpha = 0.2,
              hideOnMouseOut = FALSE) %>% 
  dyOptions(fillGraph = FALSE, fillAlpha = 0.4) %>% 
  dyRangeSelector(dateWindow = c(max(CHL$time) - years(5), max(CHL$time)))

### CHL Last year with smoothed Climatology {data-width=250}

## subset CHL for last year
CHL.lastyear = CHL %>% filter(year(time)==max(year(time)))

## make the plot
pp = ggplot(CHL.clim, aes(yDay, CHL.mean))
pp = pp + geom_line() + geom_smooth(span=0.25, se=FALSE, colour="steelblue") +  
  geom_ribbon(aes(ymin=CHL.q25, ymax=CHL.q75), fill="steelblue", alpha=0.5) +
  geom_line(data=CHL.lastyear, aes(yday(time), CHL), colour="red") + 
  ylab("Chlorophyll a (mg m-3) @ lon = -64 and lat = 41.7") + xlab("Day of the Year") + 
  theme_bw(base_size = 9) 
ggplotly(pp) %>% plotly::config(displayModeBar = F) 

## save CHL
write_csv(CHL, path = paste0(NoSpaces(CHLSiteName), "_CHL.csv"))
write_csv(CHL.clim, path = paste0(NoSpaces(CHLSiteName), "_Climatology.csv"))




