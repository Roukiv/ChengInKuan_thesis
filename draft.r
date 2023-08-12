require(tidyverse)
library(spdep)
#library(CARBayes)
library(rgdal)
#library(rgeos)
library(spgwr)

setwd("C:/Users/Ivor Kuan/Desktop/thesis_2/data")

#-----------------------------------------------------------------------------

# input data, merging data sets
#uk_la <- readOGR(dsn = '.', layer = 'infuse_dist_lyr_2011_clipped_reduce_1pc')

uk_la <- readOGR(dsn = '.', layer = 'DEC_2022_UK')
uk_la<-spTransform(uk_la, CRS("+proj=longlat +datum=NAD83"))

# remember that 2011 is fine since there is no change till 2022
#crds<-coordinates(uk_la)
#uk_la <- cbind(uk_la,crds)
plot(uk_la, axes = TRUE, cex = 0.6)

r21 <- read.csv("C:/Users/Ivor Kuan/Desktop/thesis_2/data/UK_Accident_all/n21_as1_v2.csv")

#-----------------------------------------------------------------------------

# full set of a severity level

uk_la_21<-merge(uk_la,r21, by="geo_code", duplicateGeoms = TRUE)


# although the shapefile is not suppose to have problem the data seems not matching 
# therefore there is the double check and fix
# view(uk_la_21@data)
missed <- subset(uk_la_21@data,is.na(number_of_casualties)) 
# check what is missing

missed[,c(1,3)]


missed_ni <- subset(missed[,c(1,3)], grepl("^95", geo_code)) 
# needs remove
missed_gb <- missed[!(missed$geo_code %in% missed_ni$geo_code), ][,c(1,3)]
# needs to fix (code or name changed, this mixed up with 0 obs)

uk_la_21_v1 <- uk_la_21[!(uk_la_21$geo_code %in% missed_ni$geo_code), ]
# removed North Ireland
view(uk_la_21_v1@data)

#-----------------------------------------------------------------------------

# Scotland

e <- subset(uk_la_21_v1@data, grepl("^S", geo_code))
e <- subset(e, is.na(number_of_casualties))[,c(1,3,4)]
# Scotland
e1 <- subset(r21, grepl("^S", geo_code))
e1 <- e1[,c(1,2)]

subset(e1, grepl("^S12000027", geo_code))
# is the geo code info got missing in the process of data wrangling?
# observation of "e" will also be use to check the relationship before and after
# Hence 
# S12000027    Shetland Islands with 0 obs
# S12000010        East Lothian with 0 obs
# S12000046        Glasgow City changed to S12000043
# S12000045 East Dunbartonshire changed to S12000009

#-----------------------------------------------------------------------------

# England and Wales

e <- subset(missed_gb, grepl("^E", geo_code))
# all missing value in England, Wales does not have missing in this case
# since the data the merge with old map with new data
# the process is find missing data in the merged data set (which old and update 
# and update it)

c11to21 <- read.csv("C:/Users/Ivor Kuan/Desktop/thesis_2/data/UK_Accident_all/11to21.csv")
c11to21 <- c11to21[,c(1,2,4,5)]

colnames(c11to21)[1:2] <- c("geo_code","geo_label")
c11to21 <- right_join(c11to21, e, by = "geo_code")


e1 <- c11to21[,1] != c11to21[,3]
row_numbers <- which(e1 %in% c(TRUE, NA))
c11to21[c(row_numbers),-c(2,4)]
# Hence
# E07000048 E06000058       Christchurch               Christchurch
# E07000100 E07000240          St Albans                  St Albans
# E07000104 E07000241    Welwyn Hatfield            Welwyn Hatfield
# E07000097 E07000242 East Hertfordshire         East Hertfordshire
# E07000101 E07000243          Stevenage                  Stevenage
# E07000205 E07000244    Suffolk Coastal            Suffolk Coastal
# E07000206 E07000244            Waveney                    Waveney
# E07000201 E07000245       Forest Heath               Forest Heath
# E07000204 E07000245     St Edmundsbury             St Edmundsbury
# E41000052      <NA>               <NA>   Cornwall,Isles of Scilly
# E41000324      <NA>               <NA> City of London,Westminster





















#-----------------------------------------------------------------------------

# subset without zero accident region

uk_la_21.sub <- subset(uk_la_21, !is.na(number_of_casualties))

plot(uk_la, col = ifelse(is.na(uk_la_21.sub$number_of_casualties), "black", 
                         "red"),axes = TRUE, cex = 0.6)

plot(uk_la_21.sub, axes = TRUE, cex = 0.6) 

#-----------------------------------------------------------------------------

# plot

library(RColorBrewer)
require(classInt)

variable<-uk_la_21@data$number_of_casualties
summary(variable)
sum(variable, na.rm=T)
intervals<-11
colors<-brewer.pal(intervals, "RdYlGn") # choice of colors
classes<-classIntervals(variable, intervals, style="fixed",
                        fixedBreaks=c(0,1:10*90,2330))
color.table<-findColours(classes, colors)

plot(uk_la_21, col=color.table,axes = TRUE, asp = 2)
legend(0,58, legend=names(attr(color.table, "table")),
       fill=attr(color.table, "palette"), cex=0.4, bty="n")
title(main="Number of casualties in Great Britain in 2021 with severity level 3")

#-----------------------------------------------------------------------------

# correlation

e <- c()
for (i in 1:82){
  e <- c(e,cor(uk_la_21.sub@data[4],uk_la_21.sub@data[i+4]))
}
# 16,36,37,72

summary(e)
a <- data.frame(small = e<(-0.2),
       big = e>(0.2))

# make a grid of correlation to see any colinearity?


c<-cor(uk_la_21.sub@data[c(4:86)])
library(caret)
library(mlbench)
drop = findCorrelation(c, cutoff = .6)
drop = names(num)[drop]




#-----------------------------------------------------------------------------


GWRbandwidth <- gwr.sel(number_of_casualties ~ sp30+rt_sc+u_y, 
                      data=uk_la_21.sub,adapt=T)


gwr.model = gwr(number_of_casualties ~ sp30+rt_sc+u_y, 
                data = uk_la_21.sub, adapt=GWRbandwidth)




require(GWmodel)
DM<-gw.dist(dp.locat=coordinates(uk_la))

bw.gwr <- bw.ggwr(number_of_casualties ~ sp30+rt_sc,  
                  data = uk_la_21,
                  family = "poisson",
                  approach = "AICc",
                  kernel = "bisquare", 
                  adaptive = TRUE,
                  dMat = DM )
bw.gwr





