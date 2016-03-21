opar<-par()
setwd('C:/Users/Brian G. Barkley/Dropbox/PA/BrianMarkRachelSam/Code/PA_project/')

###Load Libraries
# install.packages('mvtnorm')
library(ggmap)
library(RgoogleMaps)

library(gstat)
library(spBayes)
library(classInt)
library(RColorBrewer)
library(MBA)
library(fields)
library(rgl)
library(geoR)
library(spatial)
library(MASS)
library(verification)
library(MCMCpack)
library(coda)
library(xtable)
library(INLA)
library(maptools)
library(mvtnorm)
# install.packages('gganimate')
library(gganimate)
# library(Hmisc)
# install.packages("installr")
# require(installr)
# updateR()
# 
# # #make a map of pennsylvania from a 779 homework (useless) ---------------------------------------------------------------------
# 
# penns<-map("county","pennsylvania",fill=F,plot=T)
# county.ID<-sapply(strsplit(penns$names,","),function(x) x[2])
# penn<-map2SpatialPolygons(penns,IDs=county.ID)
# 
# x.res=100
# y.res=100
# 
# longitude<- c(rep(min(na.omit(penns$x)),2), rep(max(na.omit(penns$x)),2))
# latitutde <- c(min(na.omit(penns$y)), rep(max(na.omit(penns$y)),2), min(na.omit(penns$y)))
# LonLat <- cbind(longitude,latitutde)
# Temp <- rnorm(length(latitutde), 5)
# # LonLat<-cbind(rnorm(10,-76.5,0.01),rnorm(10,39.5,0.5)); Temp <- rnorm(10,1,1);
# 
# 
# pdf(paste("maptest-", Sys.Date(), ".pdf", sep=""), 8,5)
# surfaceTemp<-mba.surf(cbind(LonLat, Temp), no.X=x.res, no.Y=y.res, h=5, m=2, extend=F)$xyz.est
# image.plot(surfaceTemp,xaxs="r", yaxs="r", xlab="Longitude", ylab="Latitude",main="Sample Temperatures")
# # points(LonLat, pch=9, cex=3*Temp,col="black",xlab="X", ylab="Y", main="Sample Sites")
# # plot(LonLat)
# # contour(surfaceTemp, add=T, lty=3)
# lines(penns)
# dev.off()
# 

# import some sales data -------------------------------------------------

yearly_sales <- read.csv(
  'C:/Users/Brian G. Barkley/Dropbox/PA/BrianMarkRachelSam/Data/PA_county_liquor_sales.csv',
  header=T, skip=1)

yearly_sales$diff1213_1314 <- yearly_sales$DollarSalesRY2013.14-yearly_sales$DollarSalesRY2012.13
plot( yearly_sales$CountyShareRY2013.14, yearly_sales$diff1213_1314)
# yearly_sales$logdiff1213_1314 <- log(yearly_sales$diff1213_1314)
yearly_sales$logCountyShareRY2013.14 <- log(yearly_sales$CountyShareRY2013.14)

plot( yearly_sales$logCountyShareRY2013.14 , yearly_sales$diff1213_1314)

sales_counties <- as.character(yearly_sales$County)
sales_counties <- tolower(sales_counties)
sales_counties <- sales_counties[sales_counties!="total"]
sales_counties %in% pa_counties$subregion
# match(sales_counties,pa_counties$subregion)
all(sales_counties[match(pa_counties$subregion,sales_counties)]==pa_counties$subregion)
sales_to_map_order <- match(pa_counties$subregion,sales_counties)
yr_sales <- yearly_sales[sales_to_map_order,] #re-order sales dataset
all(tolower(yr_sales$County)==pa_counties$subregion) ##check - yess
yr_sales$sqrtsales_diff <- ifelse(yr_sales$diff1213_1314<0,-sqrt(-yr_sales$diff1213_1314), sqrt(yr_sales$diff1213_1314))

#now we can put this up in sales_map above


# Find locations ----------------------------------------------------------

setwd('C:/Users/Brian G. Barkley/Dropbox/PA/BrianMarkRachelSam/Data/RYReports/')

locs_ds <-  read.csv("Locations2013-2014-v4.csv", skip=2, 
                     colClasses = c("numeric","character", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric"))
setwd('C:/Users/Brian G. Barkley/Dropbox/PA/BrianMarkRachelSam/')
locs_ds <- locs_ds[,1:9]
lon_lats <- matrix(NA, nrow=dim(locs_ds)[1], ncol=2)

for(loc in 1:dim(locs_ds)[1]){
  lon_lats[loc,] <- unlist(geocode(paste(locs_ds[loc,2], "Pennsylvania"), source="google"))
  if(loc%%100 ==0){print(paste("#", loc, ":", lon_lats[loc,]))}
}

locs_ds[,8:9] <- lon_lats
names(locs_ds)[8:9] <-  c("lon", "lat")
locs_ds2 <- locs_ds
good_rows <- locs_ds$lat<42.3 & locs_ds$lat>39.6 & locs_ds$lon< -74.67 & locs_ds$lon> -83
good_rows[is.na(good_rows)] <- 0
good_rows <- ifelse(good_rows==1,T,F)
locs_ds3 <- locs_ds[good_rows,]
# Warning messages:
#   1: geocode failed with status ZERO_RESULTS, location = "1007 Cranberry Twp Cranberry Mall 20111 Rt 19"
# 2: geocode failed with status ZERO_RESULTS, location = "Easton Northampton Crossings 3718 Easton-Nazareth Hwy "
# 3: geocode failed with status ZERO_RESULTS, location = "Maple Glen Maple Glen Shopping Ctr 1973 Norristown Rd "
# 4: geocode failed with status ZERO_RESULTS, location = "Mcmurray Donaldsons Crossroads S C 3929 Washington Rd "
# 5: geocode failed with status ZERO_RESULTS, location = "Paoli Paoli Shopping Ctr 17-19 Leopard "
# 6: geocode failed with status ZERO_RESULTS, location = "Carlisle Carlisle Marketplace 281 S Spring Garden St "
# 7: geocode failed with status ZERO_RESULTS, location = "Hermitage Hermitage Towne Plz 2321 E State St "
# 8: geocode failed with status ZERO_RESULTS, location = "North Wales Gwynedd Crossing Sc 1200 Bethlehem Pike, "
# 9: geocode failed with status ZERO_RESULTS, location = "Philadephia Ivy Ridge Shopping Ctr 7146 Ridge Ave "
# 10: geocode failed with status ZERO_RESULTS, location = "Holmes Macdade Shopping Ctr 2143 Macdade Blvd "
# 11: geocode failed with status ZERO_RESULTS, location = "Feasterville Southampton Village Sc 162 E Street Rd "
# 12: geocode failed with status ZERO_RESULTS, location = "Audubon Audubon Village Shopping Ctr 2860 Audubon Vill Dr "
# 13: geocode failed with status ZERO_RESULTS, location = "Hazle Township Church Hill Mall 1089 N. Church St. "
# 14: geocode failed with status ZERO_RESULTS, location = "Eddystone Eddystone Shopping Ctr 1562 Chester Pike, "
# 15: geocode failed with status ZERO_RESULTS, location = "Blakeslee 248 Rt "
# 16: geocode failed with status ZERO_RESULTS, location = "Carlisle Stonehedge Sq Shopping Ctr 950 Walnut Bottom Rd "
# 17: geocode failed with status ZERO_RESULTS, location = "Hamlin Hamlin Shopping "
# 18: geocode failed with status ZERO_RESULTS, location = "Emmaus East Penn Plz 1325 Chestnut St "
# 19: geocode failed with status ZERO_RESULTS, location = "Hatfield Hilltown Crossings Shopping Ctr 1547 Bethlehem Pike "
# 20: geocode failed with status ZERO_RESULTS, location = "Easton Forks Town Ctr 341 Town Ctr Blvd "
# 21: geocode failed with status ZERO_RESULTS, location = "Palmyra Palmyra Shopping Ctr 901 E Main "
# 22: geocode failed with status ZERO_RESULTS, location = "Jamison Warwick Sq Shop Ctr 2395 Old York Rd "
# 23: geocode failed with status ZERO_RESULTS, location = "Mountaintop Weis Markets Sc 223 South Mountain Blvd, "
# 24: geocode failed with status ZERO_RESULTS, location = "White Oak Oak Park Mall 2001 Lincoln Way "
# 25: geocode failed with status ZERO_RESULTS, location = "Sunbury Sunbury Plz 1135 N. 4th St "
# 26: geocode failed with status ZERO_RESULTS, location = "Columbia Columbia Shopping Ctr 36 S 18th St "
# 27: geocode failed with status ZERO_RESULTS, location = "Delmont Salem 22 Plz 6518 Rte "
# 28: geocode failed with status ZERO_RESULTS, location = "New Holland New Holland Shopping Ctr 681 W Main St "
# 29: geocode failed with status ZERO_RESULTS, location = "South Park The Bavarian Village 2550 Brownsville Rd "
# 30: geocode failed with status ZERO_RESULTS, location = "Enola E Penn Ctr 736 Wertzville Road "
# 31: geocode failed with status ZERO_RESULTS, location = "Bath Bath Shopping Ctr 362 S Walnut St "
# 32: geocode failed with status ZERO_RESULTS, location = "Clearfield Clearfield Mall 1824 Daisy St "
# 33: geocode failed with status ZERO_RESULTS, location = "Conyngham Valley Plz 653 State Rte "
# 34: geocode failed with status ZERO_RESULTS, location = "Milton Weis Market Shopping Ctr 551 Mahoning St "
# 35: geocode failed with status ZERO_RESULTS, location = "Mansfield Mansfield Plz 181 N Main St "
# 36: geocode failed with status ZERO_RESULTS, location = "Gap Village At Gap Shopping Ctr 5360 Lincoln "
# 37: geocode failed with status ZERO_RESULTS, location = "Hopewell Twp Green Garden Shopping Ctr 3113 Green Garden Rd "
# 38: geocode failed with status ZERO_RESULTS, location = "Clarion 800 Ctr 845 Main St "
# 39: geocode failed with status ZERO_RESULTS, location = "Plains Plains Plz 21 N River St "
# 40: geocode failed with status ZERO_RESULTS, location = "Corry Corry Plz 350 W Columbus Ave "
# 41: geocode failed with status ZERO_RESULTS, location = "Baden Northern Lights Shoppers City 1603 State "
# 42: geocode failed with status ZERO_RESULTS, location = "Mcdonald Mcdonald Shopping Plz 301 W Barr St "

# zz<-geocode("Pittsburgh 1601 Liberty Ave", source="google")
# zz <- geocode("Fine Wine & Good Spirits", source="google")
geocodeQueryCheck()

# Make a general areal map -------------------------------------------------------
# install.packages('ggplot2')
library(ggplot2)
# this creates an example formatted as your obesity.map - you have this already...
set.seed(22)    # for reproducible example
# map.county <- map_data('county')
pa_county_map_data<-map_data("county","pennsylvania")

pa_counties   <- unique(pa_county_map_data[,5:6])
sales_map <- data.frame(stringsAsFactors = FALSE,
                        state_names=pa_counties$region,  
                        county_names=pa_counties$subregion, 
                        sales_13    = yr_sales$DollarSalesRY2012.13,
                        sales_14    = yr_sales$DollarSalesRY2013.14,
                        logsales_13    = log(yr_sales$DollarSalesRY2012.13),
                        sqrtsales_14    = sqrt(yr_sales$DollarSalesRY2013.14),
                        sales_diff  = yr_sales$diff1213_1314,
                        sqrtsales_diff = yr_sales$sqrtsales_diff,
                        sales_share = yr_sales$CountyShareRY2013.14,
                        sales_logshare = yr_sales$logCountyShareRY2013.14) 
         
  
# you start here...
# from URL http://stackoverflow.com/questions/23714052/ggplot-mapping-us-counties-problems-with-visualization-shapes-in-r

library(data.table)   # use data table merge - it's *much* faster
map_county <- data.table(map_data('county'))
setkey(map_county,region,subregion)
sales_map <- data.table(sales_map) 
setkey(sales_map,state_names,county_names)
map_df      <- map_county[sales_map]

pdf(paste("Plots/SalesMap-", Sys.Date(), ".pdf", sep=""), 8,5)
ggplot(map_df, aes(x=long, y=lat, group=group, fill=logsales_13)) + 
  geom_polygon(col="#000000")+coord_map() +
  ggtitle("2013 Liquor Sales by County")+
  scale_fill_gradient("log(2013 Sales)",low=scales::muted('blue'), high=scales::muted('green') )

ggplot(map_df, aes(x=long, y=lat, group=group, fill=sqrtsales_14)) + 
  geom_polygon(col="#000000")+coord_map() +
  ggtitle("2014 Liquor Sales by County")+
  scale_fill_gradient("Sqrt(2014 Sales)",low=('#996633'), high=('#FFFF00'))

ggplot(map_df, aes(x=long, y=lat, group=group, fill=sqrtsales_diff)) + 
  geom_polygon(col="#000000")+coord_map() +
  ggtitle("2013-2014 Change in Liquor Sales by County")+
  scale_fill_gradient2("Signed Sqrt(13-14 Sales Diff)",low=scales::muted('red'), high=scales::muted('blue'))


# p <- ggplot() + 
#   geom_polygon(data=map_df, 
#                aes(x=long, y=lat, group=group, fill=logsales_13),
#                col="#000000")+
#   coord_map() +
#   ggtitle("2013 Liquor Sales by County")+
#   scale_fill_gradient("log(2013 Sales)",low=scales::muted('blue'), high=scales::muted('green') )



p <- ggplot() + 
  geom_polygon(data = map_df,
               aes(x=long, y=lat, group=group, fill=sqrtsales_14),
               col="#000000")+
  coord_map() +
  ggtitle("2014 Liquor Sales by ")+
  scale_fill_gradient("Sqrt(County Sales)",low=('#996633'), high=('#FFFF00'))+ 
  theme(panel.background = element_rect(
    fill = 'light blue', colour = 'black'))

# ggplot(locs_ds3, aes(x=lon, y=lat, group=LicenseeOrFulfillmentCenter)) +
  p+ geom_point( aes(x=lon, y=lat,
                     size=sqrt(DollarSales)),
                 locs_ds3,colour="blue",
             alpha=0.75)

  
## you can make more difference for those with negative sales cahnge


dev.off()


save.image(file='C:/Users/Brian G. Barkley/Dropbox/PA/BrianMarkRachelSam/Code/PA_project/Image-2015-12-11.R')
save(locs_ds, file='C:/Users/Brian G. Barkley/Dropbox/PA/BrianMarkRachelSam/Code/PA_project/locs_ds-2015-12-11.R')





# GGanimating map ---------------------------------------------------------

#runif(nrow(pa_counties), min=0, max=100))
# install.packages('tidyr')
library(magrittr)
library(tidyr)
library(dplyr)

sales_map_tidy <- sales_map %>% 
  tidyr::gather(type, year_sales, (sales_13:sales_logshare),
                convert=TRUE) 

sales_map_tidy$year <- NA

myfun1 <- function(str, type){
  
  if(!is.element(type, c("scale", "year")) ){
    stop("type must either equal 'scale' or 'year'")
  } else
    if (type == "scale"){
      out <-  switch(str,
                     'sales_13' = "total_sales",
                     'sales_14' = "total_sales",
                     'logsales_13' = "log_sales",
                     'logsales_14' = "log_sales",
                     'sqrtsales_14' = "sqrt_sales",
                     'sales_diff' = "diff_sales",
                     'sqrtsales_diff' = "sqrt_diff_sales",
                     'sales_share'  = "share_of_sales",
                     'sales_logshare' = "log_share_of_sales"#,
                     # 'type not supported'
                     
      )
      return(out)
    } else {
      out <-  switch(str,
                     'sales_13' = '2013',
                     'sales_14' = "2014",
                     'logsales_13' = '2013',
                     'logsales_14' = "2014",
                     'sqrtsales_14' = "2014",
                     'sales_diff' = "2013-2014",
                     'sqrtsales_diff' = "2013-2014",
                     'sales_share'  = "201NA",
                     'sales_logshare' = "201NA"#,
                     # 'type not supported'
                     
      )
      return(out)
    }
  
}

for (ii in 1:nrow(sales_map_tidy)){
  sales_map_tidy$year[ii] <- myfun1(str = sales_map_tidy$type[ii],
                                    type = "year")
  sales_map_tidy$type[ii] <- myfun1(str = sales_map_tidy$type[ii],
                                    type = "scale")
}

# install.packages('data.table')
library(data.table)   # use data table merge - it's *much* faster
map_county <- data.table(map_data('county'))
setkey(map_county,region,subregion)
tidy_map <- data.table(sales_map_tidy) 
setkey(tidy_map,state_names,county_names)
map_df      <- map_county[tidy_map]

# install.packages('devtools')
# library(devtools)
# devtools::install_github("dgrtwo/gganimate")
# library(gganimate)
# library(gapminder)
# install.packages('ImageMagick')
# pdf(paste("Plots/SalesMap-", Sys.Date(), ".pdf", sep=""), 8,5)
df1 <- dplyr::filter(map_df, 
                     ((type == "total_sales")&(year=='2013')) |
                       ((type =="diff_sales"))
)

g <- ggplot(df1, 
            aes(x=long, y=lat, group=group, fill=year_sales,
                frame = type)) 
   
# ggplot(df1, aes(year_sales, colour = type)) +
#   geom_freqpoly(binwidth = 1000)
# 
newscale <- c(
  min(df1[df1$type=='diff_sales',]$year_sales),
  0,
  median(df1[df1$type=='diff_sales',]$year_sales),
  max(df1[df1$type=='diff_sales',]$year_sales),
  quantile(df1[df1$type=='total_sales',]$year_sales, .25),
  median(df1[df1$type=='total_sales',]$year_sales),
  quantile(df1[df1$type=='total_sales',]$year_sales, .75),
  max(df1[df1$type=='total_sales',]$year_sales)
)

PAnimate <- g +  geom_polygon(aes(frame = type),col="#000000") +
  coord_map() +
  # ggtitle("2013 Liquor Sales by County")+
#   scale_fill_gradient("sales", low=scales::muted('lightblue'), 
#                       high=scales::muted('red'))#, guide="legend" )
#   scale_fill_gradientn(colours=c("darkblue", "blue", "lightblue",
#                                  
#                                  "yellow", "green", "lightgreen", "yellow"),
#                        values=scales::rescale(newscale,to=0:1))
scale_fill_distiller( type = "seq",# guide="legend", # palette = "Diamond\nclarity", 
                   direction = 1, values = scales::rescale(newscale,to=0:1))


gg_animate(PAnimate, interval = 5)

gg_animate(PAnimate, "PAnimate.gif", interval = 5)
