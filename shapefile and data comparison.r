require(tidyverse)
library(spdep)
library(rgdal)
library(spgwr)

setwd("C:/Users/Ivor Kuan/Desktop/thesis_2/data")

uk_la <- readOGR(dsn = '.', layer = 'DEC_2022_UK')
uk_la<-spTransform(uk_la, CRS("+proj=longlat +datum=NAD83"))

# this used the first version of data set (which is gone now)
r21_a1 <- read.csv("C:/Users/Ivor Kuan/Desktop/thesis_2/data/UK_Accident_all/n21_as3_v2.csv")

# remove north ireland
subset(uk_la@data, grepl("^N", LAD22CD))
uk_la <- uk_la[-c(310:320),]
view(uk_la@data)
plot(uk_la, axes = TRUE, cex = 0.6) 


A <- data.frame(geo_code = unlist(uk_la@data$LAD22CD))
B <- data.frame(geo_code = unlist(r21_a1$geo_code))

# Finding shared observations between A and B based on ID
shared <- merge(A, B, by = "geo_code")
# Finding unique observations in A (not in B)
unique_A <- A[!(A$geo_code %in% B$geo_code), ]
# Finding unique observations in B (not in A)
unique_B <- B[!(B$geo_code %in% A$geo_code), ]

unique_A
sort(unique_B)












