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

s1_21 <- read.csv("C:/Users/Ivor Kuan/Desktop/thesis_2/data/UK_Accident_all/n21_as1_v2.csv")
uk_la <- uk_la[-c(50,54),] # remove missing
uk_la <- uk_la[-c(5,36,64,74,88,97,101,111,112,119,123,124,135,155,168,199,201,
                  203,206,227,284,293,305,311,322,361),] # remove missing
m1 <-merge(uk_la,s1_21, by="LAD22CD", duplicateGeoms = TRUE)
m1@data$number_of_casualties <- log(m1@data$number_of_casualties)
summary(m1@data)
# missed <- subset(m1@data,is.na(number_of_casualties)) 
# 5,36,50,54,66,76,90,99,103,113,114,121,125,126,137,157,
# 170,201,203,205,208,229,286,295,307,313,324,363

View(m1@data)

#-----------------------------------------------------------------------------

# m1

#-----------------------------------------------------------------------------

# correlation

#-----------------------------------------------------------------------------

c1 <- cor(na.omit(m1@data[,-c(1:10)]))
# sr_am and sc_od, the valuen of these 2 in s1 does not changed

which(colnames(m1@data)=="sr_am") # 21
which(colnames(m1@data)=="sc_od") # 60
which(colnames(m1@data)=="u_n") # 76, in urban = rural
which(colnames(m1@data)=="sr_unk") # 18, meaningless
which(colnames(m1@data)=="phc_n") # 44, control = not control

a <- summary(c1[1,])

indices <- which(c1[1,] <= quantile(c1[1,],0.25) | c1[1,] >= quantile(c1[1,],0.75))
c1[1,indices]

#-----------------------------------------------------------------------------

# correlation plot

#-----------------------------------------------------------------------------

c1[is.na(c1)] <- 0
svg(file="cor_s1.svg", width = 12.5, height = 8.854)
par(mfrow=c(3,4))
  corrplot.mixed(c1[2:7,2:7], order = 'AOE')
  corrplot.mixed(c1[8:15,8:15], order = 'AOE')
  corrplot.mixed(c1[16:20,16:20], order = 'AOE')
  corrplot.mixed(c1[21:29,21:29], order = 'AOE')
  corrplot.mixed(c1[30:33,30:33], order = 'AOE')
  corrplot.mixed(c1[36:40,36:40], order = 'AOE')
  corrplot.mixed(c1[41:45,41:45], order = 'AOE')
  corrplot.mixed(c1[46:50,46:50], order = 'AOE')
  corrplot.mixed(c1[51:55,51:55], order = 'AOE')
  corrplot.mixed(c1[56:60,56:60], order = 'AOE')
  corrplot.mixed(c1[61:64,61:64], order = 'AOE')
  corrplot.mixed(c1[67:72,67:72], order = 'AOE')
dev.off()

#-----------------------------------------------------------------------------

# modeling

#--------------------------------------------------------------------------

# Try to put it into lm model
# remove p > 0.8 the 0.7 0.6 0.5 0.4 0.3 NA cor&vif 0.05

model <- lm(number_of_casualties ~ . 
            -fr_c -rt_ow -jd_sr -wc_s -rsc_w -rsc_fi -sc_rr -sc_m -ch_pa -sp30 
            -sp40 -sp50 -fr_a -fr_u -jd_tsj -jd_c -jd_o -ch_lo -sp70
            # 0.8
            -sr_m 
            # 0.7
            -fr_am -rsc_fl
            # 0.6
            -rt_dc -jd_pe -wc_hw -wc_fm -phc_c -wc_f -ch_vo -sp20 -wc_r -rsc_s
            # 0.5
            -jd_m
            # 0.4
            -jd_n -jc_ss -jc_ats -jc_gu -ch_n 
            # 0.3
            -jc_ap -sr_n -lc_l -ppf_n -sr_b -sr_c -ppf_njl -ppf_ptsj -ppf_fs
            -lc_dl -sc_ap 
            # na cor&vif
            -rt_sc -lc_dln -lc_dlu -sr_u -ppf_zcr -rt_r -rt_sr -jd_mr -sc_n
            # 0.05
            ,
            data = m1@data[,-c(1:10,18,21,44,69,76)])



summary(model)

c1[c(2,5,32,34:40,42,43,55,63,69),c(2,5,32,34:40,42,43,55,63,69)]

vif(model)


#-----------------------------------------------------------------------------

gdm <- gw.dist(dp.locat = coordinates(m1))

bw <- bw.gwr(number_of_casualties ~ fr_m + fr_b + sr_a + jd_r + lc_dn + 
               rsc_d + u_y + sp60, m1, approach="aic", kernel="exponential",
       adaptive=T)

g1 <- gwr.basic(number_of_casualties ~ fr_m + fr_b + sr_a + jd_r + lc_dn + 
                  rsc_d + u_y + sp60, m1, bw=bw, kernel="exponential",
          adaptive=T,dMat=gdm, F123.test=T)
g1 
# Test GWR better or OLS (F1,F2)
# Variability test (F3)
# Parameter significant test


dgn <- gwr.collin.diagno(number_of_casualties ~ fr_m + fr_b + sr_a + jd_r + 
                         lc_dn + rsc_d + u_y + sp60, m1, bw, 
                       kernel="exponential", adaptive=T,dMat=gdm)
summary(dgn$VIF) # VIF of variables, >10 should be remove
summary(dgn$local_CN) # >10 then check VDP
summary(dgn$VDP) # if observation > 0.5 than should treat carefully

# spatial autocorrelation test
# do this after correlation, F1-3, VIF, etc


nei<-knearneigh(coordinates(uk_la), k=50) # Computing the closest neighbors
nei<-knn2nb(nei)
moran.test(g1$SDF$residual, nb2listw(nei))
# did the GWR remove the spatial heterogeneity 
lm.morantest(model, nb2listw(nei))
# gwr did remove spatial heterogeneity

gwr.montecarlo(number_of_casualties ~ fr_m + fr_b + sr_a + jd_r + 
                 lc_dn + rsc_d + u_y + sp60, m1, nsims=99, bw=bw, adaptive=T)

#-----------------------------------------------------------------------------
setwd("C:/Users/Ivor Kuan/Desktop/thesis_2/image_thesis/m1_aic")
# Local coefficient & Local standard error & Residual

map_result <- cbind(g1$SDF@data[,c(1:9,12,15:23)],m1@data$LAD22CD)
colnames(map_result)[c(1:9,20)] = c(paste(colnames(map_result)[1:9],
                                          "_mean", sep = ""),"LAD22CD")
m1 <- merge(m1,map_result, by="LAD22CD")
m1 <-spTransform(m1, CRS("+proj=longlat +datum=NAD83"))
m1_result <- c("Local coefficient of Intercept",
               "Local coefficient of First road class Motorway", 
               "Local coefficient of First road class B ", 
               "Local coefficient of Second road class A ", 
               "Local coefficient of Junction Roundabout", 
               "Local coefficient of Light condition Darkness - No lighting", 
               "Local coefficient of Road surface condition Dry", 
               "Local coefficient of Urban environment", 
               "Local coefficient of Speed limit 60 mph",
               "Residual",
               "Local standard error of Intercept",
               "Local standard error of First road class Motorway", 
               "Local standard error of First road class B ", 
               "Local standard error of Second road class A ", 
               "Local standard error of Junction Roundabout", 
               "Local standard error of Light condition Darkness - No lighting", 
               "Local standard error of Road surface condition Dry", 
               "Local standard error of Urban environment", 
               "Local standard error of Speed limit 60 mph")
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
  print(spplot(m1, colnames(m1@data)[ncol(m1@data)-ncol(map_result)+1+i], 
               colorkey=list(space="right"), scales = list(draw = TRUE), 
               main = list(spl_t(m1_result[i]), cex=3), 
               par.settings = list(axis.text = list(cex = 3))))
  dev.off()
}

#-----------------------------------------------------------------------------

# local correlation plot

m1_factors <- c("First road class Motorway", " First road class B ", 
                " Second road class A ", "Junction Roundabout", 
                "Light condition Darkness - No lighting", 
                "Road surface condition Dry", "Urban environment", "Speed limit 60 mph")
m1_plot_name <- c("v1","v2","v3","v4","v5","v6","v7","v8")
all <- combn(m1_factors, 2)
all <- apply(all, 2, function(x) {
  paste0(x[1], " & ", x[2])
})
m1_plot_name <- combn(m1_plot_name, 2)
m1_plot_name <- apply(m1_plot_name, 2, function(x) {
  paste0(x[1], "_", x[2])
})

lc<-as.data.frame(cbind(dgn$corr.mat[,9:36],m1@data$LAD22CD))
colnames(lc)[ncol(lc)] = "LAD22CD"
lc[, 1:(ncol(lc) - 1)] <- lapply(lc[, 1:(ncol(lc) - 1)], as.double)
m1 <- merge(m1,lc, by="LAD22CD")
m1<-spTransform(m1, CRS("+proj=longlat +datum=NAD83"))

for (i in 1:(ncol(lc)-1)){
  print(i)
  png(file=paste0(m1_plot_name[i],".png"), width = 1080, height = 1780, units = "px")
  print(spplot(m1, colnames(m1@data)[ncol(m1@data)-ncol(lc)+1+i],  
               colorkey=list(space="right"), scales = list(draw = TRUE), 
               main = list(spl_t(paste("Local correlation of ",all[i], sep = "")),cex=3),
               par.settings = list(axis.text = list(cex = 3))))
  dev.off()
}

#-----------------------------------------------------------------------------





