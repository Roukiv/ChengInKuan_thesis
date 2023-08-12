require(tidyverse)
require(spdep)
require(rgdal)
require(spgwr)
require(GWmodel)
require(car)
require(viridis)
require(corrplot)

setwd("C:/Users/Ivor Kuan/Desktop/thesis_2/data")

#-----------------------------------------------------------------------------

# input data, merging data sets

uk_la <- readOGR(dsn = '.', layer = 'DEC_2022_UK')
uk_la <- uk_la[-c(310:320),] # remove north ireland
m3<-spTransform(uk_la, CRS("+proj=longlat +datum=NAD83"))

s3_21 <- read.csv("C:/Users/Ivor Kuan/Desktop/thesis_2/data/UK_Accident_all/n21_as3_v2.csv")
uk_la <- uk_la[-c(50,54),] # remove missing
m3 <-merge(uk_la,s3_21, by="LAD22CD", duplicateGeoms = TRUE)
m3@data$number_of_casualties <- log(m3@data$number_of_casualties)
summary(m3@data)
# missed <- subset(m2@data,is.na(number_of_casualties)) both m2 and m3

View(m3@data)

#-----------------------------------------------------------------------------

# correlation

#-----------------------------------------------------------------------------

c3 <- cor(na.omit(m3@data[,-c(1:10)]))

summary(c3[1,-1])

indices <- which(c3[1,-1] <= quantile(c3[1,-1],0.25) | c3[1,-1] >= quantile(c3[1,-1],0.75))
c3[1,indices]

#-----------------------------------------------------------------------------

# correlation plot

#-----------------------------------------------------------------------------

svg(file="cor_s3.svg", width = 12.5, height = 8.854)
par(mfrow=c(3,4))
  corrplot.mixed(c3[2:7,2:7], order = 'AOE')
  corrplot.mixed(c3[8:15,8:15], order = 'AOE')
  corrplot.mixed(c3[16:20,16:20], order = 'AOE')
  corrplot.mixed(c3[21:29,21:29], order = 'AOE')
  corrplot.mixed(c3[30:33,30:33], order = 'AOE')
  corrplot.mixed(c3[36:40,36:40], order = 'AOE')
  corrplot.mixed(c3[41:45,41:45], order = 'AOE')
  corrplot.mixed(c3[46:50,46:50], order = 'AOE')
  corrplot.mixed(c3[51:55,51:55], order = 'AOE')
  corrplot.mixed(c3[56:60,56:60], order = 'AOE')
  corrplot.mixed(c3[61:64,61:64], order = 'AOE')
  corrplot.mixed(c3[67:72,67:72], order = 'AOE')
dev.off()

#-----------------------------------------------------------------------------

# modeling

#-----------------------------------------------------------------------------

# Try to put it into lm model
# remove p > 0.8 the 0.7 0.6 0.5 0.4 0.3 NA cor&vif 0.1

model <- lm(number_of_casualties ~ .
            -sr_a -sr_b -sr_u -rt_ow -rt_sr -jd_mr -phc_c -ppf_n -lc_dl -lc_dln
            -lc_dlu -wc_hw -rsc_w -sc_m -u_y -sp50 -jd_c -sp70
            # 0.8
            -sr_am
            # 0.7
            -jd_m 
            # 0.6
            -wc_fm -rsc_fl -ch_pa -sp40 -wc_r -sc_od -fr_m -fr_a -fr_c -fr_u
            -fr_am -sr_c -jd_n -jc_gu -jc_ats
            # 0.5
            -jd_sr -ppf_njl
            # 0.4
            -rsc_s -sp20 -sp30 -sp60
            # 0.3
            -sr_m -jc_ap -ppf_fs -lc_dn -lc_l -rt_sc -rt_r -sc_n -ch_vo -ch_n
            -sc_rr -ch_lo -sc_ap -sr_n -jd_tsj -jd_pe -jc_ss -ppf_ptsj
            # cor&vif 0.05
            
            ,
            data = m3@data[,-c(1:10,18,44,76)])


summary(model)

vif(model)

#-----------------------------------------------------------------------------

gdm <- gw.dist(dp.locat = coordinates(m3))

bw <- bw.gwr(number_of_casualties ~ fr_b + rt_dc + jd_r + jd_o + ppf_zcr + wc_s + rsc_fi, 
             m3, approach="r", kernel="exponential", adaptive=T)

g3 <- gwr.basic(number_of_casualties ~ fr_b + rt_dc + jd_r + jd_o + ppf_zcr + 
                  wc_s + rsc_fi, m3, bw=bw, kernel="exponential",
                adaptive=T, dMat=gdm, F123.test=T)
g3

# Test GWR better or OLS (F1,F2)
# Variability test (F3)
# Parameter significant test


dgn <- gwr.collin.diagno(number_of_casualties ~ fr_b + rt_dc + jd_r + jd_o + 
                         ppf_zcr + wc_s + rsc_fi, m3, bw, kernel="exponential", 
                       adaptive=T, dMat=gdm)
summary(dgn$VIF) # VIF of variables, >10 should be remove
summary(dgn$local_CN) # >10 then check VDP
summary(dgn$VDP) # if observation > 0.5 than should treat carefully


# spatial autocorrelation test (similar to F4 test)
# do this after correlation, F1-3, VIF, etc


nei<-knearneigh(coordinates(uk_la), k=50) # Computing the closest neighbors
nei<-knn2nb(nei)
moran.test(g3$SDF$residual, nb2listw(nei))
# did the GWR remove the spatial heterogeneity 
lm.morantest(model, nb2listw(nei))
# no for both


gwrmc <- gwr.montecarlo(number_of_casualties ~ fr_b + rt_dc + jd_r + jd_o + ppf_zcr + 
                 + wc_s + rsc_fi, data=m3, nsims=99, bw=bw, adaptive=T, 
               kernel = "exponential")
# Monte Carlo (randomisation) test for significance of GWR parameter variability
gwrmc

#-----------------------------------------------------------------------------
setwd("C:/Users/Ivor Kuan/Desktop/thesis_2/image_thesis/m3_rss")

# Local coefficient & Local standard error & Residual

map_result <- cbind(g3$SDF@data[,c(1:8,11,14:21)],m3@data$LAD22CD)
colnames(map_result)[c(1:8,18)] = c(paste(colnames(map_result)[1:8],
                                          "_mean", sep = ""),"LAD22CD")
m3 <- merge(m3,map_result, by="LAD22CD")
m3 <-spTransform(m3, CRS("+proj=longlat +datum=NAD83"))
m3_result <- c("Local coefficient of Intercept",
               "Local coefficient of First road class B", 
               "Local coefficient of Road type Dual carriageway", 
               "Local coefficient of Junction Roundabout", 
               "Local coefficient of Junction Other", 
               "Local coefficient of Central refuge and zebra crossing", 
               "Local coefficient of Weather condition Snowing", 
               "Local coefficient of Road surface condition Frost or ice",
               "Residual",
               "Local standard error of Intercept",
               "Local standard error of First road class B", 
               "Local standard error of Road type Dual carriageway", 
               "Local standard error of Junction Roundabout", 
               "Local standard error of Junction Other", 
               "Local standard error of Central refuge and zebra crossing", 
               "Local standard error of Weather condition Snowing", 
               "Local standard error of Road surface condition Frost or ice")
spl_t <- function(title) {
  wrap_t <- strwrap(title, width = 40)
  paste(wrap_t, collapse = "\n")
}
key1 <- paste0("estimate_",1:8)
key2 <- paste0("se_",1:8)
graph <- c(key1,"res",key2)
for (i in 1:(ncol(map_result)-1)){
  print(i)
  png(file=paste0(graph[i],".png"), width = 1080, height = 1780, units = "px")
  print(spplot(m3, colnames(m3@data)[ncol(m3@data)-ncol(map_result)+1+i], 
               colorkey=list(space="right"), scales = list(draw = TRUE), 
               main = list(spl_t(m3_result[i]), cex=3), 
               par.settings = list(axis.text = list(cex = 3))))
  dev.off()
}

#-----------------------------------------------------------------------------

# local correlation plot

m3_factors <- c("First road class B", "Road type Dual carriageway", 
                "Junction Roundabout", "Junction Other", 
                "Central refuge and zebra crossing", "Weather condition Snowing",
                "Road surface condition Frost or ice")
m3_plot_name <- c("v1","v2","v3","v4","v5","v6","v7")
all <- combn(m3_factors, 2)
all <- apply(all, 2, function(x) {
  paste0(x[1], " & ", x[2])
})
m3_plot_name <- combn(m3_plot_name, 2)
m3_plot_name <- apply(m3_plot_name, 2, function(x) {
  paste0(x[1], "_", x[2])
})

lc<-as.data.frame(cbind(dgn$corr.mat[,8:28],m3@data$LAD22CD))
colnames(lc)[ncol(lc)] = "LAD22CD"
lc[, 1:(ncol(lc) - 1)] <- lapply(lc[, 1:(ncol(lc) - 1)], as.double)
m3 <- merge(m3,lc, by="LAD22CD")
m3<-spTransform(m3, CRS("+proj=longlat +datum=NAD83"))

for (i in 1:(ncol(lc)-1)){
  print(i)
  png(file=paste0(m3_plot_name[i],".png"), width = 1080, height = 1780, units = "px")
  print(spplot(m3, colnames(m3@data)[ncol(m3@data)-ncol(lc)+1+i],  
               colorkey=list(space="right"), scales = list(draw = TRUE), 
               main = list(spl_t(paste("Local correlation of ",all[i], sep = "")),cex=3),
               par.settings = list(axis.text = list(cex = 3))))
  dev.off()
}

#-----------------------------------------------------------------------------



