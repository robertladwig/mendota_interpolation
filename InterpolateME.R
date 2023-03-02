## This script attempts to interpolate the biweekly and hourly Lake Mendota data into one cohesive dataframe

##### (1) Loading of biweekly monitoring data:
# Package ID: knb-lter-ntl.29.34 Cataloging System:https://pasta.edirepository.org.
# Data set title: North Temperate Lakes LTER: Physical Limnology of Primary Study Lakes 1981 - current.
# Data set creator:  John Magnuson - University of Wisconsin 
# Data set creator:  Stephen Carpenter - University of Wisconsin 
# Data set creator:  Emily Stanley - University of Wisconsin 
# Contact:    -  NTL LTER  - ntl.infomgr@gmail.com
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/29/34/03e232a1b362900e0f059859abe8eb97" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "lakeid",     
                 "year4",     
                 "daynum",     
                 "sampledate",     
                 "depth",     
                 "rep",     
                 "sta",     
                 "event",     
                 "wtemp",     
                 "o2",     
                 "o2sat",     
                 "deck",     
                 "light",     
                 "frlight",     
                 "flagdepth",     
                 "flagwtemp",     
                 "flago2",     
                 "flago2sat",     
                 "flagdeck",     
                 "flaglight",     
                 "flagfrlight"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt1$lakeid)!="factor") dt1$lakeid<- as.factor(dt1$lakeid)
if (class(dt1$year4)=="factor") dt1$year4 <-as.numeric(levels(dt1$year4))[as.integer(dt1$year4) ]               
if (class(dt1$year4)=="character") dt1$year4 <-as.numeric(dt1$year4)
if (class(dt1$daynum)=="factor") dt1$daynum <-as.numeric(levels(dt1$daynum))[as.integer(dt1$daynum) ]               
if (class(dt1$daynum)=="character") dt1$daynum <-as.numeric(dt1$daynum)                                   
# attempting to convert dt1$sampledate dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1sampledate<-as.Date(dt1$sampledate,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1sampledate) == length(tmp1sampledate[!is.na(tmp1sampledate)])){dt1$sampledate <- tmp1sampledate } else {print("Date conversion failed for dt1$sampledate. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp1sampledate) 
if (class(dt1$depth)=="factor") dt1$depth <-as.numeric(levels(dt1$depth))[as.integer(dt1$depth) ]               
if (class(dt1$depth)=="character") dt1$depth <-as.numeric(dt1$depth)
if (class(dt1$rep)!="factor") dt1$rep<- as.factor(dt1$rep)
if (class(dt1$sta)!="factor") dt1$sta<- as.factor(dt1$sta)
if (class(dt1$event)!="factor") dt1$event<- as.factor(dt1$event)
if (class(dt1$wtemp)=="factor") dt1$wtemp <-as.numeric(levels(dt1$wtemp))[as.integer(dt1$wtemp) ]               
if (class(dt1$wtemp)=="character") dt1$wtemp <-as.numeric(dt1$wtemp)
if (class(dt1$o2)=="factor") dt1$o2 <-as.numeric(levels(dt1$o2))[as.integer(dt1$o2) ]               
if (class(dt1$o2)=="character") dt1$o2 <-as.numeric(dt1$o2)
if (class(dt1$o2sat)=="factor") dt1$o2sat <-as.numeric(levels(dt1$o2sat))[as.integer(dt1$o2sat) ]               
if (class(dt1$o2sat)=="character") dt1$o2sat <-as.numeric(dt1$o2sat)
if (class(dt1$deck)=="factor") dt1$deck <-as.numeric(levels(dt1$deck))[as.integer(dt1$deck) ]               
if (class(dt1$deck)=="character") dt1$deck <-as.numeric(dt1$deck)
if (class(dt1$light)=="factor") dt1$light <-as.numeric(levels(dt1$light))[as.integer(dt1$light) ]               
if (class(dt1$light)=="character") dt1$light <-as.numeric(dt1$light)
if (class(dt1$frlight)!="factor") dt1$frlight<- as.factor(dt1$frlight)
if (class(dt1$flagdepth)!="factor") dt1$flagdepth<- as.factor(dt1$flagdepth)
if (class(dt1$flagwtemp)!="factor") dt1$flagwtemp<- as.factor(dt1$flagwtemp)
if (class(dt1$flago2)!="factor") dt1$flago2<- as.factor(dt1$flago2)
if (class(dt1$flago2sat)!="factor") dt1$flago2sat<- as.factor(dt1$flago2sat)
if (class(dt1$flagdeck)!="factor") dt1$flagdeck<- as.factor(dt1$flagdeck)
if (class(dt1$flaglight)!="factor") dt1$flaglight<- as.factor(dt1$flaglight)
if (class(dt1$flagfrlight)!="factor") dt1$flagfrlight<- as.factor(dt1$flagfrlight)

# Here is the structure of the input data frame:
str(dt1)                            
attach(dt1)                            
detach(dt1)               

##### (2) Loading of high-frequency buoy data:
# Package ID: knb-lter-ntl.130.30 Cataloging System:https://pasta.edirepository.org.
# Data set title: North Temperate Lakes LTER: High Frequency Water Temperature Data - Lake  Mendota Buoy 2006 - current.
# Data set creator:  John Magnuson - University of Wisconsin 
# Data set creator:  Stephen Carpenter - University of Wisconsin 
# Data set creator:  Emily Stanley - University of Wisconsin 
# Metadata Provider:  NTL Information Manager - University of Wisconsin 
# Contact:    -  NTL LTER  - ntl.infomgr@gmail.com
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 
inUrl2  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/130/30/63d0587cf326e83f57b054bf2ad0f7fe" 
infile2 <- tempfile()
try(download.file(inUrl2,infile2,method="curl"))
if (is.na(file.size(infile2))) download.file(inUrl2,infile2,method="auto")


dt2 <-read.csv(infile2,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "sampledate",     
                 "year4",     
                 "month",     
                 "daynum",     
                 "hour",     
                 "depth",     
                 "wtemp",     
                 "flag_wtemp"    ), check.names=TRUE)

unlink(infile2)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

# attempting to convert dt2$sampledate dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp2sampledate<-as.Date(dt2$sampledate,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp2sampledate) == length(tmp2sampledate[!is.na(tmp2sampledate)])){dt2$sampledate <- tmp2sampledate } else {print("Date conversion failed for dt2$sampledate. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp2sampledate) 
if (class(dt2$year4)=="factor") dt2$year4 <-as.numeric(levels(dt2$year4))[as.integer(dt2$year4) ]               
if (class(dt2$year4)=="character") dt2$year4 <-as.numeric(dt2$year4)
if (class(dt2$month)=="factor") dt2$month <-as.numeric(levels(dt2$month))[as.integer(dt2$month) ]               
if (class(dt2$month)=="character") dt2$month <-as.numeric(dt2$month)
if (class(dt2$daynum)=="factor") dt2$daynum <-as.numeric(levels(dt2$daynum))[as.integer(dt2$daynum) ]               
if (class(dt2$daynum)=="character") dt2$daynum <-as.numeric(dt2$daynum)
if (class(dt2$depth)=="factor") dt2$depth <-as.numeric(levels(dt2$depth))[as.integer(dt2$depth) ]               
if (class(dt2$depth)=="character") dt2$depth <-as.numeric(dt2$depth)
if (class(dt2$wtemp)=="factor") dt2$wtemp <-as.numeric(levels(dt2$wtemp))[as.integer(dt2$wtemp) ]               
if (class(dt2$wtemp)=="character") dt2$wtemp <-as.numeric(dt2$wtemp)
if (class(dt2$flag_wtemp)!="factor") dt2$flag_wtemp<- as.factor(dt2$flag_wtemp)
# Here is the structure of the input data frame:
str(dt2)                            
attach(dt2)                            
detach(dt2)               


library(tidyverse)
library(lubridate)

##### (3) Data merging:
biweekly <- dt1 %>%
  filter(lakeid == 'ME' & year4 >= min(dt2$year4)) %>%
  mutate('year4' = year(sampledate),
         'month' = month(sampledate),
         'hour' = 12,
         'daynum' = yday(sampledate)) %>%
  select('sampledate', 'year4', 'month', 'daynum', 'hour', 'depth', 'wtemp', 'flagwtemp') %>%
  rename('flag_wtemp' = 'flagwtemp')
hourly <- dt2

df <- rbind(biweekly, hourly)
hour = as.character(df$hour)
idx = which(nchar((hour))>2)
hour[idx] = gsub('.{0,2}$', '', hour[idx])
check = which(is.na(hour))
hour_corr = as.numeric(hour)

time = paste0(as.Date(df$sampledate),' ',hour_corr,":00:00")
df$datetime =as.POSIXct(strptime(time,format = "%Y-%m-%d %H:%M:%S"))

str(df)

text.size = 10
width = 6.5
height = 5
units = 'in'
precision = 'hours' # if models runs on hourly time step
conversion = 1 # conversion factor, change to 1 if everything's in mmol/m3
legend.title = 'Temperature (degC)'
resample = FALSE
interval = 1
method = 'match'
color.palette = 'RdYlBu' # which colors do you like?
color.direction = -1
obs.color = 'white'
obs.alpha = 0.2
obs.shape = 16
obs.size = 1
shiftPalette = NULL
zlim = c(0,35) # limit of axis
na_value = 'grey90' # color of values that exceed zlim

.is_heatmap <- function(file, var_name){
  
  glm_nc <- get_glm_nc(file)
  in.nc <- var_name %in% names(glm_nc$var)
  if (!all(in.nc)){
    close_glm_nc(glm_nc)
    stop(paste(var_name[!in.nc], collapse=', '),' not in ', file)
  }
  
  
  dims <-unlist(lapply(X = var_name, FUN = function(x) length(glm_nc$var[[x]]$dim)))
  
  close_glm_nc(glm_nc)
  #dim == 4 is heatmap (3D)
  return(dims==4 | dims == 0)
}

get_glm_nc  <-  function(file){
  if(length(file) < 1 || is.na(file)){
    stop('glm_nc file must be supplied string or proper file handle')
  }
  glm_nc	<- 	nc_open(file, readunlim=TRUE)
  return(glm_nc)
}

close_glm_nc <- function(glm_nc){
  nc_close(glm_nc)
}


# library(imputeTS)
# dt2_interp <- dt2
# for (i in unique(dt2_interp$datetime)){
#   idx = which(dt2_interp$datetime == i)
#   if (any(is.na(dt2_interp[idx,]))){
#     if (sum(!is.na(dt2_interp$wtemp[idx])) <= 2){
#       dt2_interp = dt2_interp[-c(idx),]
#     } else {
#       id.df = na.interpolation(dt2_interp$wtemp[idx], option = "spline")
#       dt2_interp$wtemp[idx] = id.df
#     }
#   }
# }

library(akima)
.interpolate2grid <- function(xyzData, xcol = 1, ycol = 2, zcol = 3) {
  # Interpolate field or modeled data to grid
  # xcol, ycol, and zcol and column numbers from data.frame
  # The spreads of x and y must be within four orders of magnitude of each other for interp to work
  # Therefore must scale data to be within similar magnitude to numeric dates (1e6)
  gridData <-interp2xyz(akima::interp(x = as.numeric(xyzData[,xcol]), y=(xyzData[,ycol]*1e6), z=xyzData[,zcol], duplicate="mean", linear = T,
                                      extrap = T,
                                      xo = as.numeric(seq(min(xyzData[,xcol]), max(xyzData[,xcol]), by = 'hours')), #precision
                                      yo = 1e6*seq(min(xyzData[,ycol]), max(xyzData[,ycol]), by = 0.5)), data.frame=TRUE) %>%
    dplyr::mutate(x =  as.POSIXct(x, origin = '1970-01-01', tz = Sys.timezone())) %>%
    dplyr::mutate(y = y/1e6) %>%
    dplyr::arrange(x,y)
  
  return(gridData)
}

##### (4) Interpolation:
idx.na = which(is.na(df$wtemp))
df_omit <- df[-c(idx.na),]
# Akima interpolation of observed data (Gridded Bivariate Interpolation for Irregular Data)

str(df_omit)
observed_df <- .interpolate2grid(df_omit, xcol = 9, ycol = 6, zcol = 7) %>%
  rename(DateTime = .data$x, Depth = .data$y, var = .data$z)
head(observed_df)
tail(observed_df)

##### (5) Save data file:
save(observed_df, file = 'observed_df_lter_hourly.RData')
library(feather)
write_feather(observed_df, 'observed_df_lter_hourly.feather')

##### (6) Visualise data:
h3 = ggplot(data = observed_df, aes(DateTime, Depth)) +
  geom_raster(aes(fill = var), interpolate = F) +
  # geom_point(data = data, aes(x = .data$DateTime, y = .data$Depth), color = obs.color, alpha = obs.alpha, shape = obs.shape, size = obs.size) +
  scale_y_reverse(expand = c(0.01,0.01)) +
  scale_x_datetime(expand = c(0.01,0.01), limits = c(min(observed_df$DateTime), max(observed_df$DateTime))) +
  scale_fill_distiller(palette = color.palette, direction = color.direction, na.value = na_value, values = shiftPalette, limits = zlim) +
  ylab('Depth (m)') + xlab('Date') +
  labs(fill = legend.title) +
  theme_bw(base_size = text.size)+
  theme(axis.title.x = element_blank(),
        legend.justification=c(0.9,1),
        legend.position = "top",
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.key.width =  unit(0.7, "in"),
        legend.key.height =  unit(0.08, "in"),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(0,-10,-5,-10))
h3
ggsave(file=paste0('wtemp_heatmap_lter_hourly.png'), h3, dpi = 300,width = 18,height = 6, units = 'in')

##### (7) Reverse matrix:
mydata_test <- observed_df

deps = seq(min(mydata_test$Depth), max(mydata_test$Depth), 1)
p.data <- c()
for (ki in unique(mydata_test$DateTime)){
  yi <- which(mydata_test$DateTime == ki)
  wdat = (mydata_test$var[yi])
  wdep = (mydata_test$Depth[yi])
  if (max((mydata_test$Depth[yi])) < max(deps)){
    wdat <- c(wdat, max(wdat))
    wdep <- c(wdep, max(deps))
  }
  if (min((mydata_test$Depth[yi])) > min(deps)){
    wdat <- c(min(wdat), wdat)
    wdep <- c(min(deps), wdep)
  }
  intdat <- approx(wdep, wdat, deps)$y
  p.data <- rbind(p.data, (intdat))
}

print('Finished reversing the wtemp data')

data <- as.data.frame(cbind(rep(1, length(unique(mydata_test$DateTime))), p.data))
colnames(data) <- c('date',paste0('temp_',deps))
data$date = as.Date(sort(unique(mydata_test$DateTime )))

##### (8) Save data file:
save(data, file = 'observed_df_lter_hourly_wide.RData')
write_feather(data, 'observed_df_lter_hourly_wide.feather')