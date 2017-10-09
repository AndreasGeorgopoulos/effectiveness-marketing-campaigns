library(xtable)
library(R2HTML)
library(dplyr)
library(geosphere)
library(splm)

#  Import dataset
data_stores_full_operated <- readRDS("final_df_stores_full_operated.rds")

# Check if there are Missing values per column
sapply(data_stores_full_operated, function(x) any(is.na(x)))



### --- Weight Matrix based on Harvesine distances between restaurants ---###

rest_coord_full_operated <- unique(data_stores_full_operated[c("Longitudine","Latitudine","RESTAURANT_CODE")])
coordinates_df_fully_operated <- data.matrix(unique(rest_coord_full_operated[,c("Longitudine","Latitudine")]))

# Harvesine Distance Matrix of all Restaurants (dist in km)
harv_distance_matrix <- as.data.frame(distm(coordinates_df_fully_operated) / 1000 ) # km
colnames(harv_distance_matrix) <- rest_coord_full_operated$RESTAURANT_CODE
row.names(harv_distance_matrix) <- rest_coord_full_operated$RESTAURANT_CODE

# First neighbor min dist for each store (so that all restaurants have a neighbor apart from threshold distance)
harv_distance_matrix_min <- harv_distance_matrix
harv_distance_matrix_min[harv_distance_matrix_min==0] <- Inf
harv_distance_matrix_min[matrix(c(1:nrow(harv_distance_matrix_min), apply(harv_distance_matrix_min, 1, which.min)), ncol=2)] <- 1 # find min dist per row(store) and replace with 1 (neighbor)

# Additional neighbors if dist < 1 km
neighb_matrix <- as.matrix(ifelse(harv_distance_matrix_min<=2 & harv_distance_matrix_min>0 ,1,0))

# Number of neighbors per store
nn <- rowSums(neighb_matrix)

# Row standardised Weight Matrix
neighb_matrix_row_stand <- mat2listw(neighb_matrix, style = "W")

### --- END --- ###





### --- Find optimal threshold distance (km) for considering neighbor-relationship between restaurants --- ###

formul_4 <- NET_SALES ~ Mean_TemperatureC + Extreme_events + Holidays + Weekend + month + Sport.Events + Adstock_TV_GRP + Radio_GRPS + cInternet + Adstock_Offline_Other
formul_5 <- RECEIPTS_COUNT ~ Mean_TemperatureC + Extreme_events + Holidays + Weekend + month + Sport.Events + Adstock_TV_GRP_Traffic + Radio_GRPS + cInternet + Adstock_Offline_Other_Traffic

error_rate_km <- data.frame("k" = 1:10, "ErrorRate" = NA, "ErrorRate_Traffic" = NA)
formul <- NET_SALES ~ Mean_TemperatureC + Extreme_events + Holidays + Weekend + month + Sport.Events + Adstock_TV_GRP + Radio_GRPS  + cInternet + Adstock_Offline_Other
for (i in 1:10) { # dist 1-10 km
  neighb_matrix_knn <- as.matrix(ifelse(harv_distance_matrix_min<=i & harv_distance_matrix_min>0 ,1,0))

  # SEM fixed effects
  sar_fixed_knn <- spml(formula = formul_4, data = data_stores_full_operated, index = c("RESTAURANT_CODE", "BUSINESS_DATE"),
                        listw = mat2listw(neighb_matrix_knn, style = "W"), lag = FALSE, spatial.error = "b", model = "within",
                        effect = "individual", method = "eigen")
  sar_fixed_knn_traffic <- spml(formula = formul_5, data = data_stores_full_operated, index = c("RESTAURANT_CODE", "BUSINESS_DATE"),
                        listw = mat2listw(neighb_matrix_knn, style = "W"), lag = FALSE, spatial.error = "b", model = "within",
                        effect = "individual", method = "eigen")
  error_rate_km[i,"ErrorRate"] <- sqrt(sum(sar_fixed_knn$resid^2)/368639)
  error_rate_km[i,"ErrorRate_Traffic"] <- sqrt(sum(sar_fixed_knn_traffic$resid^2)/368639)
}

write.csv(error_rate_km,"error_rate_km.csv") # optimal threshold distance 1km, for traffic 

### -- END --- ###



# Net Sales -----------------------------

# Row-standardised Weight Matrix with optimal threshold distance of 1km
neighb_matrix <- as.matrix(ifelse(harv_distance_matrix_min<=1 & harv_distance_matrix_min>0 ,1,0))
neighb_matrix_row_stand <- mat2listw(neighb_matrix, style = "W")

formul_sales <- NET_SALES ~ Mean_TemperatureC + Extreme_events + Holidays + Weekend + month + Sport.Events + Adstock_TV_GRP + Radio_GRPS + cInternet + Adstock_Offline_Other

sar_fixed_sales <- spml(formula = formul_sales, data = data_stores_full_operated, index = c("RESTAURANT_CODE", "BUSINESS_DATE"),
                    listw = neighb_matrix_row_stand, lag = FALSE, spatial.error = "b", model = "within",
                    effect = "individual", method = "eigen")

summary(sar_fixed_sales)
# save results
print(xtable(summary(sar_fixed_sales)), type = "html", file="results_sales.html")

# Fixed Effects
FE_sales <- effects(sar_fixed_sales)
HTML(FE_sales,file="effects_sales.html")
# Standard error 
sqrt(sum(sar_fixed_sales$resid^2)/368639)
# pseudo-Rsquared
summary(sar_fixed_sales)$rsqr 



# --- Traffic ----------------------------
formul_tr <- RECEIPTS_COUNT ~ Mean_TemperatureC + Extreme_events + Holidays + Weekend + month + Sport.Events + Adstock_TV_GRP_Traffic + Radio_GRPS + cInternet + Adstock_Offline_Other_Traffic


sar_fixed_tr <- spml(formula = formul_tr, data = data_stores_full_operated, index = c("RESTAURANT_CODE", "BUSINESS_DATE"),
                    listw = neighb_matrix_row_stand, lag = FALSE, spatial.error = "b", model = "within",
                    effect = "individual", method = "eigen")

summary(sar_fixed_tr)
# save results
print(xtable(summary(sar_fixed_tr)), type = "html", file="results_traffic.html")

# Fixed Effects
FE_tr <- effects(sar_fixed_tr)
HTML(FE_tr,file="effects_traffic.html")

# SE
sqrt(sum(sar_fixed_tr$resid^2)/368639)
# pseudo-Rsquared
summary(sar_fixed_tr)$rsqr 




### --- Tests for spatial autocorrelation --- ###

# Net Sales --------------------------------
test2_sales <- bsktest(x = formul_sales, data = data_stores_full_operated, listw = neighb_matrix_row_stand,test = "LM2")
test3_sales <- bsktest(x = formul_sales, data = data_stores_full_operated, listw = neighb_matrix_row_stand, test = "CLMlambda")

# Receipts Count ----------------------------
test2_tr <- bsktest(x = formul_tr, data = data_stores_full_operated, listw = neighb_matrix_row_stand,test = "LM2")
test3_tr <- bsktest(x = formul_tr, data = data_stores_full_operated, listw = neighb_matrix_row_stand, test = "CLMlambda")

### --- END --- ###
