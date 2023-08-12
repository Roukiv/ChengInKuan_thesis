require(tidyverse)

setwd("C:/Users/Ivor Kuan/Desktop/thesis_2/data")
d21 <- read.csv("C:/Users/Ivor Kuan/Desktop/thesis_2/data/UK_Accident_all/d21.csv")

colnames(d21)

#-------------------------------------------------------------------------------

# find/explore/understand the relationship of the data

# when variable X have condition A, what is the condition of variable Y?
filtered_data <- d21[d21$road_type == 6, ]
column_values <- filtered_data$junction_detail
table(column_values)

#-------------------------------------------------------------------------------
# Unchanged

# "accident_severity"                      
# "number_of_casualties"                   
# "local_authority_ons_district"   
# "speed_limit"
#-------------------------------------------------------------------------------
# remove

# "number of vehicles"
#-------------------------------------------------------------------------------
# Changed

# "first_road_class"
# "second_road_class"
# "road_type" 
# "junction_detail"                        
# "junction_control"                       
# "pedestrian_crossing_human_control"      
# "pedestrian_crossing_physical_facilities"
# "light_conditions"                       
# "weather_conditions"                     
# "road_surface_conditions"                
# "special_conditions_at_site"             
# "carriageway_hazards"                    
# "urban_or_rural_area"  
#-------------------------------------------------------------------------------
# fixing the wrong information and create new data frame

setwd("C:/Users/Ivor Kuan/Desktop/thesis_2/image_thesis/dv")
n21 <- d21[, c("accident_severity", "number_of_casualties", 
               "local_authority_ons_district")]
colnames(n21)[colnames(n21) == "local_authority_ons_district"] ="LAD22CD"

n21$LAD22CD[n21$LAD22CD == "E06000028"] <- "E06000058"
n21$LAD22CD[n21$LAD22CD == "E06000029"] <- "E06000058"
n21$LAD22CD[n21$LAD22CD == "E06000048"] <- "E06000058"

n21$LAD22CD[n21$LAD22CD == "E07000049"] <- "E06000059"
n21$LAD22CD[n21$LAD22CD == "E07000050"] <- "E06000059"
n21$LAD22CD[n21$LAD22CD == "E07000051"] <- "E06000059"
n21$LAD22CD[n21$LAD22CD == "E07000052"] <- "E06000059"
n21$LAD22CD[n21$LAD22CD == "E07000053"] <- "E06000059"

n21$LAD22CD[n21$LAD22CD == "E07000004"] <- "E06000060"
n21$LAD22CD[n21$LAD22CD == "E07000005"] <- "E06000060"
n21$LAD22CD[n21$LAD22CD == "E07000006"] <- "E06000060"
n21$LAD22CD[n21$LAD22CD == "E07000007"] <- "E06000060"

n21$LAD22CD[n21$LAD22CD == "E07000150"] <- "E06000061"
n21$LAD22CD[n21$LAD22CD == "E07000152"] <- "E06000061"
n21$LAD22CD[n21$LAD22CD == "E07000153"] <- "E06000061"
n21$LAD22CD[n21$LAD22CD == "E07000156"] <- "E06000061"

n21$LAD22CD[n21$LAD22CD == "E07000151"] <- "E06000062"
n21$LAD22CD[n21$LAD22CD == "E07000154"] <- "E06000062"
n21$LAD22CD[n21$LAD22CD == "E07000155"] <- "E06000062"

n21$LAD22CD[n21$LAD22CD == "E07000190"] <- "E07000246"
n21$LAD22CD[n21$LAD22CD == "E07000191"] <- "E07000246"

n21$LAD22CD[n21$LAD22CD == "E08000020"] <- "E08000037"

n21$LAD22CD[n21$LAD22CD == "S12000009"] <- "S12000045"
n21$LAD22CD[n21$LAD22CD == "S12000015"] <- "S12000047"
n21$LAD22CD[n21$LAD22CD == "S12000024"] <- "S12000048"
n21$LAD22CD[n21$LAD22CD == "S12000043"] <- "S12000049"
n21$LAD22CD[n21$LAD22CD == "S12000044"] <- "S12000050"
#-------------------------------------------------------------------------------

n21$fr_m <- ifelse(d21$first_road_class == "1", 1, 0)
n21$fr_am <- ifelse(d21$first_road_class == "2", 1, 0)
n21$fr_a <- ifelse(d21$first_road_class == "3", 1, 0)
n21$fr_b <- ifelse(d21$first_road_class == "4", 1, 0)
n21$fr_c <- ifelse(d21$first_road_class == "5", 1, 0)
n21$fr_u <- ifelse(d21$first_road_class == "6", 1, 0)

n21$sr_unk <- ifelse(d21$second_road_class == "-1", 1, 0)
n21$sr_n <- ifelse(d21$second_road_class == "0", 1, 0)
n21$sr_m <- ifelse(d21$second_road_class == "1", 1, 0)
n21$sr_am <- ifelse(d21$second_road_class == "2", 1, 0)
n21$sr_a <- ifelse(d21$second_road_class == "3", 1, 0)
n21$sr_b <- ifelse(d21$second_road_class == "4", 1, 0)
n21$sr_c <- ifelse(d21$second_road_class == "5", 1, 0)
n21$sr_u <- ifelse(d21$second_road_class == "6", 1, 0)

n21$rt_r <- ifelse(d21$road_type == "1", 1, 0)
n21$rt_ow <- ifelse(d21$road_type == "2", 1, 0)
n21$rt_dc <- ifelse(d21$road_type == "3", 1, 0)
n21$rt_sc <- ifelse(d21$road_type == "6", 1, 0)
n21$rt_sr <- ifelse(d21$road_type == "7", 1, 0)

n21$jd_n <- ifelse(d21$junction_detail == "0", 1, 0)
n21$jd_r <- ifelse(d21$junction_detail == "1", 1, 0)
n21$jd_mr <- ifelse(d21$junction_detail == "2", 1, 0)
n21$jd_tsj <- ifelse(d21$junction_detail == "3", 1, 0)
n21$jd_sr <- ifelse(d21$junction_detail == "5", 1, 0)
n21$jd_c <- ifelse(d21$junction_detail == "6", 1, 0)
n21$jd_m <- ifelse(d21$junction_detail == "7", 1, 0)
n21$jd_pe <- ifelse(d21$junction_detail == "8", 1, 0)
n21$jd_o <- ifelse(d21$junction_detail == "9", 1, 0)

n21$jc_ap <- ifelse(d21$junction_control == "1", 1, 0)
n21$jc_ats <- ifelse(d21$junction_control == "2", 1, 0)
n21$jc_ss <- ifelse(d21$junction_control == "3", 1, 0)
n21$jc_gu <- ifelse(d21$junction_control == "4", 1, 0)

n21$phc_n <- ifelse(d21$pedestrian_crossing_human_control == "0", 1, 0)
n21$phc_c <- ifelse(d21$pedestrian_crossing_human_control %in% c("1", "2"), 1, 0)

n21$ppf_n <- ifelse(d21$pedestrian_crossing_physical_facilities == "0", 1, 0)
n21$ppf_njl <- ifelse(d21$pedestrian_crossing_physical_facilities == "4", 1, 0)
n21$ppf_ptsj <- ifelse(d21$pedestrian_crossing_physical_facilities == "5", 1, 0)
n21$ppf_fs <- ifelse(d21$pedestrian_crossing_physical_facilities == "7", 1, 0)
n21$ppf_zcr <- ifelse(d21$pedestrian_crossing_physical_facilities %in% c("1", "8"), 1, 0)

n21$lc_l <- ifelse(d21$light_conditions == "1", 1, 0)
n21$lc_dl <- ifelse(d21$light_conditions == "4", 1, 0)
n21$lc_dln <- ifelse(d21$light_conditions == "5", 1, 0)
n21$lc_dn <- ifelse(d21$light_conditions == "6", 1, 0)
n21$lc_dlu <- ifelse(d21$light_conditions == "7", 1, 0)

n21$wc_hw <- ifelse(d21$weather_conditions %in% c("4","5","6"), 1, 0)
n21$wc_f <- ifelse(d21$weather_conditions %in% c("1","4"), 1, 0)
n21$wc_r <- ifelse(d21$weather_conditions %in% c("2","5"), 1, 0)
n21$wc_s <- ifelse(d21$weather_conditions %in% c("3","6"), 1, 0)
n21$wc_fm <- ifelse(d21$weather_conditions == "7", 1, 0)

n21$rsc_d <- ifelse(d21$road_surface_conditions == "1", 1, 0)
n21$rsc_w <- ifelse(d21$road_surface_conditions == "2", 1, 0)
n21$rsc_s <- ifelse(d21$road_surface_conditions == "3", 1, 0)
n21$rsc_fi <- ifelse(d21$road_surface_conditions == "4", 1, 0)
n21$rsc_fl <- ifelse(d21$road_surface_conditions == "5", 1, 0)

n21$sc_n <- ifelse(d21$special_conditions_at_site == "0", 1, 0)
n21$sc_ap <- ifelse(d21$special_conditions_at_site %in% c("1","2"), 1, 0)
n21$sc_rr <- ifelse(d21$special_conditions_at_site %in% c("3","4","5"), 1, 0)
n21$sc_od <- ifelse(d21$special_conditions_at_site == "6", 1, 0)
n21$sc_m <- ifelse(d21$special_conditions_at_site == "7", 1, 0)

n21$ch_n <- ifelse(d21$carriageway_hazards == "0", 1, 0)
n21$ch_vo <- ifelse(d21$carriageway_hazards %in% c("1","2"), 1, 0)
n21$ch_pa <- ifelse(d21$carriageway_hazards == "3", 1, 0)
n21$ch_lo <- ifelse(d21$carriageway_hazards %in% c("6","7"), 1, 0)

n21$u_y <- ifelse(d21$urban_or_rural_area == "1", 1, 0)
n21$u_n <- ifelse(d21$urban_or_rural_area == "2", 1, 0)

n21$sp20 <- ifelse(d21$speed_limit == "20", 1, 0)
n21$sp30 <- ifelse(d21$speed_limit == "30", 1, 0)
n21$sp40 <- ifelse(d21$speed_limit == "40", 1, 0)
n21$sp50 <- ifelse(d21$speed_limit == "50", 1, 0)
n21$sp60 <- ifelse(d21$speed_limit == "60", 1, 0)
n21$sp70 <- ifelse(d21$speed_limit == "70", 1, 0)

rm(list=setdiff(ls(), "n21")) # clean the global environment

n21_as1 <- subset(n21, accident_severity == "1")[,-c(1)]
n21_as2 <- subset(n21, accident_severity == "2")[,-c(1)]
n21_as3 <- subset(n21, accident_severity == "3")[,-c(1)]

# Quality of casualty

# sum of accidents
# Quantity of causality, mean of causality is a weaker version of this
# the situation of vehicle data is similar to this



x <- n21_as1[-c(1:nrow(n21_as1)),]
loc_list <- unique(n21_as1$LAD22CD)
var_list <- names(n21_as1)
# create a clean df with all variables
for (i in 1:length(loc_list)) {
  for (j in 1:length(var_list)){
    if (j == 1) {
      x[i,1] <- sum(subset(n21_as1, LAD22CD == loc_list[i])[,1])
    } else if(j == 2) {
      x[i,2] <- loc_list[i]
    } else {
      x[i,j] <- mean(subset(n21_as1, LAD22CD == loc_list[i])[,j])
    }
  }
  print(i)
}
n21_as1_v2 <- x


x <- n21_as1[-c(1:nrow(n21_as1)),]
loc_list <- unique(n21_as2$LAD22CD)
var_list <- names(n21_as2)
# create a clean df with all variables
for (i in 1:length(loc_list)) {
  for (j in 1:length(var_list)){
    if (j == 1) {
      x[i,1] <- sum(subset(n21_as2, LAD22CD == loc_list[i])[,1])
    } else if(j == 2) {
      x[i,2] <- loc_list[i]
    } else {
      x[i,j] <- mean(subset(n21_as2, LAD22CD == loc_list[i])[,j])
    }
  }
  print(i)
}
n21_as2_v2 <- x



x <- n21_as1[-c(1:nrow(n21_as1)),]
loc_list <- unique(n21_as3$LAD22CD)
var_list <- names(n21_as3)
# create a clean df with all variables
for (i in 1:length(loc_list)) {
  for (j in 1:length(var_list)){
    if (j == 1) {
      x[i,1] <- sum(subset(n21_as3, LAD22CD == loc_list[i])[,1])
    } else if(j == 2) {
      x[i,2] <- loc_list[i]
    } else {
      x[i,j] <- mean(subset(n21_as3, LAD22CD == loc_list[i])[,j])
    }
  }
  print(i)
}
n21_as3_v2 <- x


#-------------------------------------------------------------------------------
write.csv(n21_as1_v2,"C:/Users/Ivor Kuan/Desktop/thesis_2/data/UK_Accident_all/n21_as1_v2.csv", 
          row.names = FALSE)

#-------------------------------------------------------------------------------
write.csv(n21_as2_v2,"C:/Users/Ivor Kuan/Desktop/thesis_2/data/UK_Accident_all/n21_as2_v2.csv", 
          row.names = FALSE)

#-------------------------------------------------------------------------------
write.csv(n21_as3_v2,"C:/Users/Ivor Kuan/Desktop/thesis_2/data/UK_Accident_all/n21_as3_v2.csv", 
          row.names = FALSE)

#-------------------------------------------------------------------------------

