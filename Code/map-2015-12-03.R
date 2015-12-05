opar<-par()
setwd('C:/Users/Brian G. Barkley/Dropbox/PA/BrianMarkRachelSam/')

###Load Libraries
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
# library(Hmisc)

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

sales_counties<-as.character(yearly_sales$County)
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


# Make a general areal map -------------------------------------------------------

library(ggplot2)
# this creates an example formatted as your obesity.map - you have this already...
set.seed(22)    # for reproducible example
# map.county <- map_data('county')
pa_county_map_data<-map_data("county","pennsylvania")

pa_counties   <- unique(pa_county_map_data[,5:6])
sales_map <- data.frame(state_names=pa_counties$region,  
                        county_names=pa_counties$subregion, 
                        sales_13    = yr_sales$DollarSalesRY2012.13,
                        sales_14    = yr_sales$DollarSalesRY2013.14,
                        logsales_13    = log(yr_sales$DollarSalesRY2012.13),
                        sqrtsales_14    = sqrt(yr_sales$DollarSalesRY2013.14),
                        sales_diff  = yr_sales$diff1213_1314,
                        sqrtsales_diff = yr_sales$sqrtsales_diff,
                        sales_share = yr_sales$CountyShareRY2013.14,
                        sales_logshare = yr_sales$logCountyShareRY2013.14) 
                                                  #runif(nrow(pa_counties), min=0, max=100))

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

## you can make more difference for those with negative sales cahnge


dev.off()



