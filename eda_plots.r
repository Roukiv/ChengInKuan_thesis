require(tidyverse)
require(spdep)
require(rgdal)
require(spgwr)
require(GWmodel)
require(car)
require(viridis)
require(cowplot)

setwd("C:/Users/Ivor Kuan/Desktop/thesis_2/data")

#-----------------------------------------------------------------------------

# input data, merging data sets

uk_la <- readOGR(dsn = '.', layer = 'DEC_2022_UK')
uk_la <- uk_la[-c(310:320),] # remove north ireland
uk_la<-spTransform(uk_la, CRS("+proj=longlat +datum=NAD83"))

d1 <- read.csv("C:/Users/Ivor Kuan/Desktop/thesis_2/data/UK_Accident_all/n21_as1_v2.csv")
d2 <- read.csv("C:/Users/Ivor Kuan/Desktop/thesis_2/data/UK_Accident_all/n21_as2_v2.csv")
d3 <- read.csv("C:/Users/Ivor Kuan/Desktop/thesis_2/data/UK_Accident_all/n21_as3_v2.csv")
m1 <-merge(uk_la,d1, by="LAD22CD", duplicateGeoms = TRUE)
m2 <-merge(uk_la,d2, by="LAD22CD", duplicateGeoms = TRUE)
m3 <-merge(uk_la,d3, by="LAD22CD", duplicateGeoms = TRUE)
m1@data[is.na(m1@data)] <- 0
m2@data[is.na(m2@data)] <- 0
m3@data[is.na(m3@data)] <- 0

spl_t <- function(title) {
  wrap_t <- strwrap(title, width = 30)
  paste(wrap_t, collapse = "\n")}
#-----------------------------------------------------------------------------
# response variable
#-----------------------------------------------------------------------------

setwd("C:/Users/Ivor Kuan/Desktop/thesis_2/image_thesis/dv")
m <- data.frame(a1 = m1@data$number_of_casualties, 
                a2 = m2@data$number_of_casualties,
                a3 = m3@data$number_of_casualties, 
                LAD22CD = m3@data$LAD22CD)
m <- merge(uk_la, m, by="LAD22CD", duplicateGeoms = TRUE)
uk_la<-spTransform(m, CRS("+proj=longlat +datum=NAD83"))

gap<-c(0,1:9)
graph <- paste0("plot",1:6, ".png")
t1 <- "Number of casualties in Great Britain in 2021 with severity level "


for (i in 1:3){
  t2 <- paste0("Histogram of ", t1, i)
  png(file=graph[i], width = 600, height = 880, units = "px")
  print(hist(m@data[,10+i], breaks=40,xlab = "Number of casualties",
       main = spl_t(t2), cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5))
  dev.off()
  m@data[,10+i] <- log(m@data[,10+i])
  m@data[m@data == -Inf] <- 0
  t3 <- paste0("Logged ", t1, i)
  png(file=graph[i+3], width = 1080, height = 1780, units = "px")
  print(spplot(m, colnames(m@data)[i+10], 
               colorkey = list(space="right"), scales = list(draw = TRUE), 
               col.regions = c("pink","red","orange","yellow","green","cyan4","blue",
                               "darkorchid3","black"), main = list(label = spl_t(t3), cex = 3),
               at = gap, par.settings = list(axis.text = list(cex = 3))))
  dev.off()
}

#-----------------------------------------------------------------------------
# independent variables
#-----------------------------------------------------------------------------
titles <- c("First road class Mortorway", "First road class A standard Mortorway",
           "First road class A", "First road class B", "First road class C",
           "First road class Unidentified", "Second road class Unknown", 
           "No Second road involved", "Second road class Mortorway",
           "Second road class A standard Mortorway", "Second road class A", 
           "Second road class B", "Second road class C", "Second road class Unidentified",
           "Road type Roundabout", "Road type One way street",
           "Road type Dual carriageway", "Road type Single carriageway",
           "Road type Slip road", "No junction within 20 metres",
           "Junction Roundabout", "Junction Mini-roundabout",
           "T or staggered junction", "Junction Slip road", "Junction Crossroads",
           "Junction More than 4 arms (not roundabout)",
           "Junction Private drive or entrance",
           "Junction Other junction", "Junction control by Authorised person ",
           "Junction control by Auto traffic signal", "Junction control by Stop sign",
           "No junction controlled or Give way",
           "No Pedestrian crossing human control in 50 metres",
           "Pedestrian crossing human control by school crossing patrol or authorised person",
           "No Pedestrian crossing physical facilities in 50 metres",
           "Pedestrian crossing physical facilities Pelican, puffin, toucan or 
              similar non-junction pedestrian light crossing",
           "Pedestrian crossing physical facilities Pedestrian phase at traffic signal junction",
           "Pedestrian crossing physical facilities Footbridge or subway",
           "Pedestrian crossing physical facilities Zebra crossing and Central refuge",
           "Light condition Daylight", "Light condition Darkness - lights lit",
           "Light condition Darkness - lights unlit",
           "Light condition Darkness - no lighting",
           "Light condition Darkness - lighting unknown",
           "Weather condition High wind", "Weather condition Fine",
           "Weather condition Rain", "Weather condition Snow",
           "Weather condition Fog or mist", "Road surface conditions Dry",
           "Road surface conditions Wet or damp", "Road surface conditions Snow",
           "Road surface conditions Frost or ice",
           "Road surface conditions Flood over 3cm deep",
           "No Special condition ", "Special condition Auto traffic signal problems",
           "Special condition Road problems", "Special condition Oil or diesel",
           "Special condition Mud", "No Carriageway hazard",
           "Carriageway hazard object on road", "Carriageway hazard Previous accident",
           "Carriageway hazard animal or human in carriageway", "Urban area",
           "Rural area", "Speed limit 20km/h", "Speed limit 30km/h", 
           "Speed limit 40km/h", "Speed limit 50km/h", "Speed limit 60km/h",
           "Speed limit 70km/h")
for (j in 1:3){
  keys <- paste0("map", 12:(length(titles)+11), "_",j)
  if (j == 1) {m <- m1} else if (j == 2) {m <- m2} else if (j == 3) {m <- m3}
  for (i in 12:(length(titles)+11)) {
    graph <- paste0(keys[i-11], ".png")
    png(file=graph, width = 1080, height = 1780, units = "px")
    print(spplot(m, colnames(m@data)[i], colorkey=list(space="right"), 
                 main = list(label=spl_t(paste0(titles[i-11], " of severity ",j)),cex=3),
                 col.regions = viridis(16), par.settings = list(axis.text = list(cex = 3))))
    dev.off()
  }
}
#-----------------------------------------------------------------------------







