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
uk_la<-spTransform(uk_la, CRS("+proj=longlat +datum=NAD83"))

s2_21 <- read.csv("C:/Users/Ivor Kuan/Desktop/thesis_2/data/UK_Accident_all/n21_as2_v2.csv")
uk_la <- uk_la[-c(50,54),] # remove missing
m2 <-merge(uk_la,s2_21, by="LAD22CD", duplicateGeoms = TRUE)
m2@data$number_of_casualties <- log(m2@data$number_of_casualties)
summary(m2@data)
# missed <- subset(m2@data,is.na(number_of_casualties)) both m2 and m3
# 50,54

View(m2@data)

#-----------------------------------------------------------------------------

# m2

#-----------------------------------------------------------------------------

# correlation

#-----------------------------------------------------------------------------

c2 <- cor(na.omit(m2@data[,-c(1:10)]))

summary(c2[1,-1])

indices <- which(c2[1,] <= quantile(c2[1,],0.25) | c2[1,] >= quantile(c2[1,],0.75))
c2[1,indices]

#-----------------------------------------------------------------------------

# correlation plot

#-----------------------------------------------------------------------------

svg(file="cor_s2.svg", width = 12.5, height = 8.854)
par(mfrow=c(3,4))
  corrplot.mixed(c2[2:7,2:7], order = 'AOE')
  corrplot.mixed(c2[8:15,8:15], order = 'AOE')
  corrplot.mixed(c2[16:20,16:20], order = 'AOE')
  corrplot.mixed(c2[21:29,21:29], order = 'AOE')
  corrplot.mixed(c2[30:33,30:33], order = 'AOE')
  corrplot.mixed(c2[36:40,36:40], order = 'AOE')
  corrplot.mixed(c2[41:45,41:45], order = 'AOE')
  corrplot.mixed(c2[46:50,46:50], order = 'AOE')
  corrplot.mixed(c2[51:55,51:55], order = 'AOE')
  corrplot.mixed(c2[56:60,56:60], order = 'AOE')
  corrplot.mixed(c2[61:64,61:64], order = 'AOE')
  corrplot.mixed(c2[67:72,67:72], order = 'AOE')
dev.off()

#-----------------------------------------------------------------------------

# modeling

#-----------------------------------------------------------------------------

# Try to put it into lm model
# remove p > 0.8 the 0.7 0.6 0.5 0.4 0.3 NA cor&vif 0.1

model <- lm(number_of_casualties ~ .
            -fr_m -jd_c -jd_o -jc_ss -jc_gu -ppf_n -wc_r -rsc_s -fr_b -fr_u
            -sc_rr -sp60 -sc_od 
            # 0.8
            -jd_n -sc_ap -ch_pa -sp70 -rsc_d -ch_vo
            # 0.7
            -phc_c -rsc_fi
            # 0.6
            -sr_c -jd_pe -sp50 -sr_m -jd_mr
            # 0.5
            -ch_lo -ch_n
            # 0.4
            -ppf_njl -ppf_fs -sc_n -sr_am -fr_am -ppf_ptsj -fr_c -jc_ats
            # 0.3
            -sr_n -rt_sc -lc_dl -sr_a -sr_b -rt_r -rt_ow -lc_dn -sc_m 
            # cor&vif 
            -sp40 -sp30 -lc_dlu -jc_ap -jd_m -sr_u -jd_tsj -jd_sr -fr_a -rsc_w
            -u_y -wc_f -lc_dln -wc_fm -lc_l
            # 0.05
            
            ,
            data = m2@data[,-c(1:10,18,44,76)])


summary(model)

vif(model)

#-----------------------------------------------------------------------------

gdm <- gw.dist(dp.locat = coordinates(m2))

bw <- bw.gwr(number_of_casualties ~ rt_dc + rt_sr + jd_r + ppf_zcr +
               wc_hw + wc_s + rsc_fl + sp20, 
             m2, approach="aic", kernel="bisquare",
             adaptive=T)

g2 <- gwr.basic(number_of_casualties ~ rt_dc + rt_sr + jd_r + ppf_zcr +
                  wc_hw + wc_s + rsc_fl + sp20, m2, 
                bw=bw, kernel="bisquare",
                adaptive=T,dMat=gdm,
                F123.test=T)
g2 

# Test GWR better or OLS (F1,F2)
# Variability test (F3)
# Parameter significant test


dgn <- gwr.collin.diagno(number_of_casualties ~ rt_dc + rt_sr + jd_r + ppf_zcr + 
                           wc_hw + wc_s + rsc_fl + sp20,
                       m2, bw, kernel="bisquare", adaptive=T,dMat=gdm)
summary(dgn$VIF) # VIF of variables, >10 should be remove
summary(dgn$local_CN) # >10 then check VDP
summary(dgn$VDP) # if observation > 0.5 than should treat carefully

# spatial autocorrelation test
# do this after correlation, F1-3, VIF, etc

nei <- knearneigh(coordinates(uk_la), k=50) # Computing the closest neighbors
nei <- knn2nb(nei)
moran.test(g2$SDF$residual, nb2listw(nei))
# did the GWR remove the spatial heterogeneity 
lm.morantest(model, nb2listw(nei))
# gwr does better but both model's residual show spatial heterogeneity 


gwrmc <- gwr.montecarlo(number_of_casualties ~ rt_dc + rt_sr + jd_r + ppf_zcr + 
                          wc_hw + wc_s + rsc_fl + sp20, 
               data=m2, nsims=99, bw=bw, adaptive=T)

# Monte Carlo (randomisation) test for significance of GWR parameter variability
gwrmc

#-----------------------------------------------------------------------------
setwd("C:/Users/Ivor Kuan/Desktop/thesis_2/image_thesis/m2_aic")

# Local coefficient & Local standard error & Residual

map_result <- cbind(g2$SDF@data[,c(1:9,12,15:23)],m2@data$LAD22CD)
colnames(map_result)[c(1:9,20)] = c(paste(colnames(map_result)[1:9],
                                          "_mean", sep = ""),"LAD22CD")
m2 <- merge(m2,map_result, by="LAD22CD")
m2 <-spTransform(m2, CRS("+proj=longlat +datum=NAD83"))
m2_result <- c("Local coefficient of Intercept",
               "Local coefficient of Road type Dual carriageway", 
               "Local coefficient of Road type Slip road",
               "Local coefficient of Junction Roundabout", 
               "Local coefficient of Central refuge and zebra crossing",
               "Local coefficient of Weather condition High wind", 
               "Local coefficient of Weather condition Snowing", 
               "Local coefficient of Road surface condition Flood over 3cm deep", 
               "Local coefficient of Speed limit 20 mph",
               "Residual",
               "Local standard error of Intercept",
               "Local standard error of Road type Dual carriageway", 
               "Local standard error of Road type Slip road",
               "Local standard error of Junction Roundabout", 
               "Local standard error of Central refuge and zebra crossing",
               "Local standard error of Weather condition High wind", 
               "Local standard error of Weather condition Snowing", 
               "Local standard error of Road surface condition Flood over 3cm deep", 
               "Local standard error of Speed limit 20 mph")
spl_t <- function(title) {
  wrap_t <- strwrap(title, width = 40)
  paste(wrap_t, collapse = "\n")
}
key1 <- paste0("estimate_",1:9)
key2 <- paste0("se_",1:9)
graph <- c(key1,"res",key2)
for (i in 1:(ncol(map_result)-1)){
  print(i)
  png(file=paste0(graph[i],".png"), width = 1080, height = 1780, units = "px")
  print(spplot(m2, colnames(m2@data)[ncol(m2@data)-ncol(map_result)+1+i], 
               colorkey=list(space="right"), scales = list(draw = TRUE), 
               main = list(spl_t(m2_result[i]), cex=3), 
               par.settings = list(axis.text = list(cex = 3))))
  dev.off()
}

#-----------------------------------------------------------------------------

# local correlation plot

m2_factors <- c("Road type Dual carriageway", "Road type Slip road", "Junction Roundabout", 
                "Central refuge and zebra crossing", "Weather condition High wind", 
                "Weather condition Snowing", "Road surface condition Flood over 3cm deep", 
                "Speed limit 20 mph")
m2_plot_name <- c("v1","v2","v3","v4","v5","v6","v7","v8")
all <- combn(m2_factors, 2)
all <- apply(all, 2, function(x) {
  paste0(x[1], " & ", x[2])
})
m2_plot_name <- combn(m2_plot_name, 2)
m2_plot_name <- apply(m2_plot_name, 2, function(x) {
  paste0(x[1], "_", x[2])
})

lc<-as.data.frame(cbind(dgn$corr.mat[,9:36],m2@data$LAD22CD))
colnames(lc)[ncol(lc)] = "LAD22CD"
lc[, 1:(ncol(lc) - 1)] <- lapply(lc[, 1:(ncol(lc) - 1)], as.double)
m2 <- merge(m2,lc, by="LAD22CD")
m2<-spTransform(m2, CRS("+proj=longlat +datum=NAD83"))

for (i in 1:(ncol(lc)-1)){
  print(i)
  png(file=paste0(m2_plot_name[i],".png"), width = 1080, height = 1780, units = "px")
  print(spplot(m2, colnames(m2@data)[ncol(m2@data)-ncol(lc)+1+i],  
               colorkey=list(space="right"), scales = list(draw = TRUE), 
               main = list(spl_t(paste("Local correlation of ",all[i], sep = "")),cex=3),
               par.settings = list(axis.text = list(cex = 3))))
  dev.off()
}

#-----------------------------------------------------------------------------