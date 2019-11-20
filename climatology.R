

function(datetime, valuevar, stTitle="Station", stLabel="allData", 
         year.start = min(year(datetime)), year.end = max(year(datetime)), 
         summerStart=1, summerEnd=120, spannumber = 0.5, 
         yscale="auto", ymax = 32, ymin=19,
         makeplot=T, savedata=F, saveplot=F, plotOnly=T){
  
  
  ## Funtion to calculate day climatology.
  ## by Eduardo Klein. eklein@usb.ve
  ## last version 03/AUG/2017
  ## produced under CC-BY 4.0 license
  ## inputs:
  ## datetime: timestamp vector as POSIXct or Date object
  ## valuevar: a numeric vector of the measured variable
  ## stTitle: a character with the station name. No spaces allowed
  ## stLabel: a character with a label, like depth or other thing. No spaces allowed
  ## summerStart: day of the year when summer starts
  ## summerEnd: day of the year whre summer ends. summerEnd>summerStart
  ## spannumber: smoother(loess) parameter. smaller -> less smooth
  ## makeplot: if TRUE the plot is created
  ## savedata: if TRUE climatology data will be saved as csv file
  ## saveplot: if TRUE climatology plot will be saved as png file
  ## 20180129: MAD added to the resulting data frame
  ## 20180901: add start and end years as parameters
  
  require(dplyr)
  require(lubridate)
  require(R.utils)
  require(ggplot2)
  
  ## check for allowed classes and values
  if(!is.POSIXct(datetime) & !is.Date(datetime)) return("timesptamp not POSIXct or Date.")
  if(!is.numeric(valuevar)) return("values are not numeric")
  if(gregexpr(" ", stTitle)[[1]][1]!=-1) return("Please don't use spaces in the station name")
  if(gregexpr(" ", stLabel)[[1]][1]!=-1) return("Please don't use spaces in the station label")
  if(summerStart>=summerEnd) return("summerStart >= summerEnd")
  

  
  ## make a dataframe with time.values
  tt = data.frame(dateHour = round_date(datetime, "hour"), Temp = valuevar)
  
  ## filter by year.start, year.end
  tt = tt %>% filter(year(dateHour)>=year.start, year(dateHour)<=year.end)
  
  years.avail = unique(year(tt$dateHour))
  ## montly means
  
  tt.yearmonth = tt %>% group_by(yy = year(dateHour), mm = month(dateHour)) %>%
    summarise(Temp.mean = mean(Temp, na.rm = T), 
              Temp.max = max(Temp, na.rm=T),
              Temp.min = min(Temp, na.rm=T))
  
  ## this is NOAA's MMM
  tt.month = tt.yearmonth %>% group_by(mm) %>% 
    summarise(Temp.mean = mean(Temp.mean, na.rm=T))
  
  MMM = max(tt.month$Temp.mean, na.rm=T)
  
  
  
  ## calculate daily average
  tt.day = tt %>% group_by(dateDay = round_date(dateHour, "day")) %>% 
    summarise(Temp.mean = mean(Temp, na.rm = T), 
              Temp.max = max(Temp, na.rm=T),
              Temp.min = min(Temp, na.rm=T))
  

  ## adds day of the year
  tt.day$yDay = yday(tt.day$dateDay)
  
  
  ## calculate climatological day average
  
  tt.clim = tt.day %>% group_by(yDay) %>% 
    summarise(Temp.dmm = mean(Temp.mean, na.rm=T), 
              Temp.dmax = max(Temp.max, na.rm=T), ## mean of the maximun daily value
              Temp.dmin = min(Temp.min, na.rm=T), ## mean of the minimum daily value
              Temp.dsd = sd(Temp.mean, na.rm=T), 
              Temp.mad = mad(Temp.mean, na.rm=T),
              Temp.q01 = quantile(Temp.mean, na.rm=T, probs = 0.01),
              Temp.q003 = quantile(Temp.mean, na.rm=T, probs = 0.003), 
              Temp.q10 = quantile(Temp.mean, na.rm=T, probs = 0.10), 
              Temp.q32 = quantile(Temp.mean, na.rm=T, probs = 0.32), 
              Temp.q50 = quantile(Temp.mean, na.rm=T, probs = 0.50), 
              Temp.q68 = quantile(Temp.mean, na.rm=T, probs = 0.68), 
              Temp.q90 = quantile(Temp.mean, na.rm=T, probs = 0.90), 
              Temp.q997 = quantile(Temp.mean, na.rm=T, probs = 0.997), 
              Temp.q99 = quantile(Temp.mean, na.rm=T, probs = 0.99))

  ## fill the possible time gaps
  fullyear = data.frame(yDay=1:366)
  tt.clim = full_join(tt.clim, fullyear, by="yDay")
  tt.clim = tt.clim[order(tt.clim$yDay),]


  ## write day climatology
  if(savedata){
    ## filename
    climfilename = paste0(stTitle,"_dayClimatology_", stLabel, ".csv")
    ## write file
    write.csv(file=climfilename, tt.clim, row.names = F)
  }  ## end save data  
  
  ## climatological year
  ## calculate the mean of the daily mean, max and min temp
  ## using daily data
  climyear = tt.clim  %>%
    group_by(yDay) %>%
    summarise(Temp.mm = mean(Temp.dmm, na.rm=T),
              Temp.max = mean(Temp.dmax, na.rm=T),
              Temp.min = mean(Temp.dmin, na.rm=T),
              Temp.sd = mean(Temp.dsd, na.rm=T),
              Temp.2sd = mean(Temp.dsd, na.rm=T) * 2,
              Temp.3sd = mean(Temp.dsd, na.rm=T) * 3,
              ndays = n())


  ## smooth the mean
  climyear$Temp.mm.smooth = predict(loess(climyear$Temp.mm~climyear$yDay, span=spannumber), climyear$yDay)
  
  ## smooth the sd
  climyear$Temp.sd.smooth = predict(loess(climyear$Temp.sd~climyear$yDay, span=spannumber), climyear$yDay)
  climyear$Temp.2sd.smooth = predict(loess(climyear$Temp.2sd~climyear$yDay, span=spannumber), climyear$yDay)
  climyear$Temp.3sd.smooth = predict(loess(climyear$Temp.3sd~climyear$yDay, span=spannumber), climyear$yDay)
  
  ## calculate min and max value sfor the y-axis
  if (yscale == "auto"){
    ymin = floor(min(climyear$Temp.mm.smooth - climyear$Temp.3sd.smooth, na.rm=T))
    ymax = ceiling(max(climyear$Temp.mm.smooth + climyear$Temp.3sd.smooth, na.rm=T))
  }
  ### this is a trick to plot the X labels with dates not yDays
  climyear$Date = ymd(19900101)+ climyear$yDay - 1
  
  ## subset for the summer days
  climyear.summer = filter(climyear, yDay>=summerStart, yDay<=summerEnd)
 
  
  
  ## calculate global mean & summer stats
  glob.mean = mean(climyear$Temp.mm, na.rm =T)
  summer.mean = mean(climyear.summer$Temp.mm, na.rm=T)
  summer.max = mean(climyear.summer$Temp.max, na.rm=T)
  summer.min= mean(climyear.summer$Temp.min, na.rm=T)
  

  ## make the plot
  if(makeplot){
    pp = ggplot(climyear, aes(Date, Temp.mm))
    pp = pp +
      ## what to plot
      geom_ribbon(aes(ymax=Temp.mm.smooth + Temp.sd.smooth, ymin=Temp.mm.smooth - Temp.sd.smooth), fill="grey70", alpha=0.75)+
      geom_point(size=0.7) +
      geom_smooth(se=F, method="loess", span=spannumber, colour="red1", size=0.75) + 
      geom_line(aes(Date, Temp.mm.smooth + Temp.2sd.smooth), linetype=3, colour="red3") +
      geom_line(aes(Date, Temp.mm.smooth - Temp.2sd.smooth), linetype=3, colour="red3") +
      geom_line(aes(Date, Temp.mm.smooth + Temp.3sd.smooth), linetype=3, colour="red4") +
      geom_line(aes(Date, Temp.mm.smooth - Temp.3sd.smooth), linetype=3, colour="red4") +
      ## reference lines
      geom_segment(aes(x = ymd(19900101), xend = ymd(19901231), 
                       y = glob.mean, yend = glob.mean), colour = "blue",linetype = 2) +
      geom_segment(aes(x = ymd(19900101), xend = ymd(19900501), 
                       y = summer.mean, yend = summer.mean), colour = "tan2",linetype = 2) +
      geom_segment(aes(x = ymd(19900101), xend = ymd(19900501), 
                       y = summer.max, yend = summer.max), colour = "tan4",linetype = 2) +
      geom_segment(aes(x = ymd(19900101), xend = ymd(19900501), 
                       y = summer.min, yend = summer.min), colour = "tan4",linetype = 2) +
      ## labels and scales
      ylab("Mean daily Temperature (°C)") + xlab("") + 
      ggtitle(paste0(stTitle, " -- Climatological year -- ", stLabel)) +
      scale_x_date(date_breaks = "1 month", date_labels = "%b") +
      scale_y_continuous(breaks = seq(ymin, ymax, 1)) +
      theme_bw(base_size = 12) +
      ## annotations
      annotate("text",  x = ymd(19900701), y=glob.mean - 0.2, 
               label = paste0("Year Mean Temperature = ", sprintf("%6.3f", glob.mean)),
               hjust=0, size = 3) + 
      annotate("text",  x = ymd(19900515), y=summer.mean, 
               label = paste0("Summer Mean = ", sprintf("%6.3f", summer.mean)),
               hjust=0, size = 3) + 
      annotate("text", x = ymd(19900515), y=summer.max, 
               label = paste0("Summer Mean Max = ", sprintf("%6.3f", summer.max)), 
               hjust=0, size = 3) + 
      annotate("text", x = ymd(19900515), y= summer.min, 
               label = paste0("Summer Mean Min = ", sprintf("%6.3f", summer.min)),
               hjust=0, size =3 ) +
      annotate("text", x = ymd(19900101), y=climyear$Temp.mm.smooth[1] + climyear$Temp.sd.smooth[1] + 0.15, 
               label = "1SD", hjust=0, size =3 ) +
      annotate("text", x = ymd(19900101), y=climyear$Temp.mm.smooth[1] + climyear$Temp.2sd.smooth[1] + 0.15,
               label = "2SD", hjust=0, size =3 ) +
      annotate("text", x = ymd(19900101), y=climyear$Temp.mm.smooth[1] + climyear$Temp.3sd.smooth[1] + 0.15,
               label = "3SD", hjust=0, size =3 ) +
      annotate("text", x= ymd(19900101), y = ymin, 
               label = paste0("Data from years: ", seqToHumanReadable(years.avail)), hjust = 0, size = 4) +
      annotate("text", x= ymd(19901201), y = ymin, label = paste0("MMM = ", sprintf("%6.3f", MMM)))
    
    ##print(pp)
    
    if(saveplot){
      plotfilename = paste0(stTitle,"_climatology_", stLabel, ".png")
      ggsave(filename = plotfilename, pp)
    }  ## end save plot
  }  ## end plot
  
  if(plotOnly){
    return(pp)
  }else {
    return(tt.clim)
  }
 
}