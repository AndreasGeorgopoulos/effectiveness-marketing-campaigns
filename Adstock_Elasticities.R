
# Import Dataset
data_aggr_province <- read.csv("final_df_aggr_provinces.csv", header = TRUE)
# Check if there are Missing values per column
sapply(data_aggr_province, function(x) any(is.na(x)))


# Adstock variables
adstock_ab <- function(x, rate=0, beta =10){
  adstocked_advertising = stats::filter(x=x, filter=rate, method="recursive")
  a_adstocked_advertising = atan(adstocked_advertising / beta)
  return(as.numeric(a_adstocked_advertising))
}

# Grid Search function
grid_search <- function(pivot_beta, activity, data_input, dependent_var){
  a = seq(0, 0.99, by=0.05)         # parameter alpha
  b = seq(1,2*max(activity)+1, by = pivot_beta) # parameter beta
  k = 0
  RMSE_df = data.frame(matrix(ncol = length(a), nrow = length(b)))
  colnames(RMSE_df)  <- a
  row.names(RMSE_df) <- b
  for (i in a) {
    l = 0
    k = k + 1
    for (j in b) {
      l = l + 1
      adstocked_advertising = stats::filter(x=activity, filter=i, method="recursive")
      a_adstocked_advertising = atan(adstocked_advertising / j)
      model_fit <- lm(dependent_var ~ a_adstocked_advertising, data = data_input)   # formula of regression equation
      RMSE_df[l,k] <- summary(model_fit)$sigma
    }
  }
  position_best_model = which(RMSE_df == min(RMSE_df), arr.ind = TRUE)  # find row and col of min SSE
  best_model_a = as.numeric(colnames(RMSE_df)[position_best_model[2]])  # take corresponding col name (alpha)
  best_model_b = as.numeric(rownames(RMSE_df)[position_best_model[1]])  # take corresponding row name (beta)
  best_model_RMSE = RMSE_df[position_best_model[1],position_best_model[2]]
  return(list(best_model_a,best_model_b,best_model_RMSE,RMSE_df))
}




###--- Adstock variable for each communication campaign per Province for Net_Sales --- ###

provinces = as.character(unique(data_aggr_province$Provincia))

adstock_parameters_province <- data.frame("Provincia" = provinces, "a_tv" = 0, "b_tv" = 0, "a_radio" = 0, "b_radio" = 0, "a_online"=0, "b_online" = 0, "a_offline" =0, "b_offline" =0)

ream = length(provinces)
for (pr in provinces) {
  ream = ream -1
  cat("Computing parameters for province: ", pr, "\n")
  cat("Remaning provinces: ", ream, "\n")
  print("...")
  # extract data 
  province_sales <- data_aggr_province[data_aggr_province$Provincia == pr, ]
  k  = 2
  for (campaign in c("TV_GRPS","Radio_GRPS","cInternet", "cOffline_Others")) {
    # pivot for beta
    if(campaign %in% c("TV_GRPS","Radio_GRPS"))
      pivot = 10
    else
      pivot = 200
    search <- grid_search(pivot,province_sales[,campaign],province_sales, province_sales$Total_Net_Sales)
    best_alpha  <- as.numeric(search[1]) 
    best_beta   <- as.numeric(search[2])
    # add best parameters in df
    adstock_parameters_province[adstock_parameters_province$Provincia == pr, k] <- best_alpha
    adstock_parameters_province[adstock_parameters_province$Provincia == pr, k+1] <- best_beta
    k = k + 2
  }
}

write.csv(adstock_parameters_province, "adstock_parameters_province_sales.csv")
adstock_parameters_province <- read.csv("adstock_parameters_province_sales.csv", header = TRUE)

# compute adstock with best alpha,beta for each province and stack in one dataframe: date, province, adstocks (to be ready merge with final df)
adstock_df_per_province <- data.frame(BUSINESS_DATE = character(), Provincia = as.character(),  Adstock_TV_GRP = numeric(),  Adstock_Radio_GRP = numeric(),  Adstock_Online = numeric(),  Adstock_Offline_Other = numeric())


for (pr in provinces) {
  prov_df_ads <- data.frame(BUSINESS_DATE = unique(data_aggr_province$BUSINESS_DATE), Provincia = NA,  Adstock_TV_GRP = 0,  Adstock_Radio_GRP = 0,  Adstock_Online = 0,  Adstock_Offline_Other = 0)
  prov_df_ads$Provincia <- pr

  province_sales <- data_aggr_province[data_aggr_province$Provincia == pr, ]
  cat("Caculating Adstock variables for province: ", pr, "\n")
  k  = 2
  m = 3
  for (campaign in c("TV_GRPS","Radio_GRPS","cInternet", "cOffline_Others")) {
    adstock_var <- c()
    adstock_var <- adstock_ab(province_sales[,campaign],adstock_parameters_province[adstock_parameters_province$Provincia == pr, k],adstock_parameters_province[adstock_parameters_province$Provincia == pr, k+1])
    prov_df_ads[,m] <-adstock_var
    k = k + 2
    m = m + 1
  }
  adstock_df_per_province <- rbind(adstock_df_per_province,prov_df_ads)
}

write.csv(adstock_df_per_province, "adstock_df_per_province_sales.csv")




### --- Sample Adstock Variables for Brescia & Napoli -- ###

# Write csv for plotting adstock for 2 provinces: 
ploting_adst <- data.frame(matrix(nrow = length(unique(data_aggr_province$BUSINESS_DATE))))
ploting_adst$Date <- unique(data_aggr_province$BUSINESS_DATE)
ploting_adst$GRP_TV <- data_aggr_province[data_aggr_province$Provincia == "Brescia", "TV_GRPS"]
ploting_adst$Euro_spent_Offline <- data_aggr_province[data_aggr_province$Provincia == "Brescia", "cOffline_Others"]
ploting_adst$Adstock_TV <- adstock_df_per_province[adstock_df_per_province$Provincia=="Brescia", "Adstock_TV_GRP"]
ploting_adst$Adstock_Offline <-  adstock_df_per_province[adstock_df_per_province$Provincia=="Brescia", "Adstock_Offline_Other"]
ploting_adst$Adstock_TV_forCOUNT <- adstock_df_per_province_tr[adstock_df_per_province_tr$Provincia=="Brescia", "Adstock_TV_GRP_Traffic"]
ploting_adst$Adstock_Offline_forCOUNT <- adstock_df_per_province_tr[adstock_df_per_province_tr$Provincia=="Brescia", "Adstock_Offline_Other_Traffic"]
write.csv(ploting_adst, "ploting_adst_BRESCIA.csv")


ploting_adst <- data.frame(matrix(nrow = length(unique(data_aggr_province$BUSINESS_DATE))))
ploting_adst$Date <- unique(data_aggr_province$BUSINESS_DATE)
ploting_adst$GRP_TV <- data_aggr_province[data_aggr_province$Provincia == "Agrigento", "TV_GRPS"]
ploting_adst$Adstock_TV <- adstock_df_per_province[adstock_df_per_province$Provincia=="Agrigento", "Adstock_TV_GRP"]
ploting_adst$Adstock_TV_forCOUNT <- adstock_df_per_province_tr[adstock_df_per_province_tr$Provincia=="Agrigento", "Adstock_TV_GRP_Traffic"]
write.csv(ploting_adst, "ploting_adst_Agrigento.csv")

### --- END --- ###





###--- Adstock variable for each communication campaign per Province for Traffic --- ###

# compute adstock parameters per province
adstock_parameters_province_tr <- data.frame("Provincia" = provinces, "a_tv" = 0, "b_tv" = 0, "a_radio" = 0, "b_radio" = 0, "a_online"=0, "b_online" = 0, "a_offline" =0, "b_offline" =0)
ream = length(provinces)
for (pr in provinces) {
  ream = ream - 1
  cat("Computing parameters for province: ", pr, "\n")
  cat("Remaning provinces: ", ream, "\n")
  print("...")
  # extract data 
  province_tr <- data_aggr_province[data_aggr_province$Provincia == pr, ]
  k  = 2
  for (campaign in c("TV_GRPS","Radio_GRPS","cInternet", "cOffline_Others")) {
    # pivot for beta
    if(campaign %in% c("TV_GRPS","Radio_GRPS"))
      pivot = 10
    else
      pivot = 200
    search <- grid_search(pivot,province_tr[,campaign],province_tr, province_tr$Total_Receipts_Count)
    best_alpha  <- as.numeric(search[1]) 
    best_beta   <- as.numeric(search[2])
    # add best parameters in df
    adstock_parameters_province_tr[adstock_parameters_province_tr$Provincia == pr, k] <- best_alpha
    adstock_parameters_province_tr[adstock_parameters_province_tr$Provincia == pr, k+1] <- best_beta
    k = k + 2
  }
}

write.csv(adstock_parameters_province_tr, "adstock_parameters_province_traffic.csv")


# compute adstock variables with best alpha,beta for each province and stack in one dataframe: date, province, adstocks (to be ready merge with final df)
adstock_df_per_province_tr <- data.frame(BUSINESS_DATE = character(), Provincia = as.character(),  Adstock_TV_GRP_Traffic = numeric(),  Adstock_Radio_GRP_Traffic = numeric(),  Adstock_Online_Traffic = numeric(),  Adstock_Offline_Other_Traffic = numeric())

for (pr in provinces) {
  prov_df_ads <- data.frame(BUSINESS_DATE = unique(data_aggr_province$BUSINESS_DATE), Provincia = NA,  Adstock_TV_GRP_Traffic = 0,  Adstock_Radio_GRP_Traffic = 0,  Adstock_Online_Traffic = 0,  Adstock_Offline_Other_Traffic = 0)
  prov_df_ads$Provincia <- pr
  
  province_tr <- data_aggr_province[data_aggr_province$Provincia == pr, ]
  cat("Caculating Adstock variables for province: ", pr, "\n")
  k  = 2
  m = 3
  for (campaign in c("TV_GRPS","Radio_GRPS","cInternet", "cOffline_Others")) {
    adstock_var <- c()
    adstock_var <- adstock_ab(province_tr[,campaign],adstock_parameters_province_tr[adstock_parameters_province_tr$Provincia == pr, k],adstock_parameters_province_tr[adstock_parameters_province_tr$Provincia == pr, k+1])
    prov_df_ads[,m] <-adstock_var
    k = k + 2
    m = m + 1
  }
  adstock_df_per_province_tr <- rbind(adstock_df_per_province_tr,prov_df_ads)
}

write.csv(adstock_df_per_province_tr, "adstock_df_per_province_traffic.csv")








### --- SIMULATION - Elasticities per province --- ###
elasticities_per_province <- data.frame(Provincia = as.character(provinces),  Short_Elasticity_TV_sales = 0,  Long_Elasticity_TV_sales = 0,  Short_Elasticity_Offline_sales = 0,  Long_Elasticity_Offline_sales = 0, Short_Elasticity_TV_traffic = 0,  Long_Elasticity_TV_traffic = 0,  Short_Elasticity_Offline_traffic = 0,  Long_Elasticity_Offline_traffic = 0)


for (pr in provinces) {
  TV_adj<-tvgrp <- data_aggr_province[data_aggr_province$Provincia == pr, "TV_GRPS"]
  offline_adj<-offlinegrp <- data_aggr_province[data_aggr_province$Provincia == pr, "cOffline_Others"]

  adstock<-list()
  adstock[[1]] <- adstock_df_per_province[adstock_df_per_province$Provincia==pr, "Adstock_TV_GRP"] #tv sales
  adstock[[2]] <- adstock_df_per_province[adstock_df_per_province$Provincia==pr, "Adstock_Offline_Other"] #offline sales
  adstock[[3]] <-  adstock_df_per_province_tr[adstock_df_per_province_tr$Provincia==pr, "Adstock_TV_GRP_Traffic"] #tv tr
  adstock[[4]] <-  adstock_df_per_province_tr[adstock_df_per_province_tr$Provincia==pr, "Adstock_Offline_Other_Traffic"] #offline tr
  
  elasticity_sales_full.tv<-c()
  elasticity_traffic_full.tv<-c()
  elasticity_sales_full_short.tv<-c()
  elasticity_traffic_full_short.tv<-c()
  
  elasticity_sales_full.offline<-c()
  elasticity_traffic_full.offline<-c()
  elasticity_sales_full_short.offline<-c()
  elasticity_traffic_full_short.offline<-c()
  
  for (i in 1:length(tvgrp)) {
    TV_adj<-tvgrp
    TV_adj[i]<-TV_adj[i]*1.01
    offline_adj<-offlinegrp
    offline_adj[i]<-offline_adj[i]*1.01
    
    adj_adstock<-list()
    adj_adstock[[1]] <- adstock_ab(TV_adj,adstock_parameters_province[adstock_parameters_province$Provincia == pr, "a_tv"],adstock_parameters_province[adstock_parameters_province$Provincia == pr, "b_tv"]) #tv sales
    adj_adstock[[2]] <- adstock_ab(offline_adj,adstock_parameters_province[adstock_parameters_province$Provincia == pr, "a_offline"],adstock_parameters_province[adstock_parameters_province$Provincia == pr, "b_offline"]) #offline sales
    adj_adstock[[3]] <- adstock_ab(TV_adj,adstock_parameters_province_tr[adstock_parameters_province_tr$Provincia == pr, "a_tv"],adstock_parameters_province_tr[adstock_parameters_province_tr$Provincia == pr, "b_tv"]) #tv tr
    adj_adstock[[4]] <- adstock_ab(offline_adj,adstock_parameters_province_tr[adstock_parameters_province_tr$Provincia == pr, "a_offline"],adstock_parameters_province_tr[adstock_parameters_province_tr$Provincia == pr, "b_offline"]) #offline tr
    
    
    salesChange <- sum(244.326 * (adj_adstock[[1]]-adstock[[1]]))
    elasticity_sales_full.tv[i] <- salesChange
    elasticity_sales_full_short.tv[i] <- (244.326 * (adj_adstock[[1]][i]-adstock[[1]][i]))
    
    salesChange <- sum(317.514 * (adj_adstock[[2]]-adstock[[2]]))
    elasticity_sales_full.offline[i] <- salesChange
    elasticity_sales_full_short.offline[i] <- (317.514 * (adj_adstock[[2]][i]-adstock[[2]][i]))
    
    trafficChange <- sum(28.498 * (adj_adstock[[3]]-adstock[[3]]))
    elasticity_traffic_full.tv[i] <- trafficChange
    elasticity_traffic_full_short.tv[i] <- (28.498 * (adj_adstock[[3]][i]-adstock[[3]][i]))
    
    trafficChange <- sum(44.314 * (adj_adstock[[4]]-adstock[[4]]))
    elasticity_traffic_full.offline[i] <- trafficChange
    elasticity_traffic_full_short.offline[i] <- (44.314 * (adj_adstock[[4]][i]-adstock[[4]][i]))
  }
  
  elasticity.traffic.full.tv<-mean(elasticity_traffic_full.tv)
  elasticity.traffic.full.short.tv<-mean(elasticity_traffic_full_short.tv)
  elasticity.sales.full.tv<-mean(elasticity_sales_full.tv)
  elasticity.sales.full.short.tv<-mean(elasticity_sales_full_short.tv)
  
  elasticity.traffic.full.offline<-mean(elasticity_traffic_full.offline)
  elasticity.traffic.full.short.offline<-mean(elasticity_traffic_full_short.offline)
  elasticity.sales.full.offline<-mean(elasticity_sales_full.offline)
  elasticity.sales.full.short.offline<-mean(elasticity_sales_full_short.offline)
  
  elasticities_per_province[elasticities_per_province$Provincia == pr , "Short_Elasticity_TV_sales"] <- elasticity.sales.full.short.tv
  elasticities_per_province[elasticities_per_province$Provincia == pr , "Long_Elasticity_TV_sales"]  <- elasticity.sales.full.tv
  elasticities_per_province[elasticities_per_province$Provincia == pr , "Short_Elasticity_Offline_sales"] <- elasticity.sales.full.short.offline
  elasticities_per_province[elasticities_per_province$Provincia == pr , "Long_Elasticity_Offline_sales"]  <- elasticity.sales.full.offline
  
  elasticities_per_province[elasticities_per_province$Provincia == pr , "Short_Elasticity_TV_traffic"] <- elasticity.traffic.full.short.tv
  elasticities_per_province[elasticities_per_province$Provincia == pr , "Long_Elasticity_TV_traffic"]  <- elasticity.traffic.full.tv
  elasticities_per_province[elasticities_per_province$Provincia == pr , "Short_Elasticity_Offline_traffic"] <- elasticity.traffic.full.short.offline
  elasticities_per_province[elasticities_per_province$Provincia == pr , "Long_Elasticity_Offline_traffic"]  <- elasticity.traffic.full.offline
}
### --- END --- ###
write.csv(elasticities_per_province,"elasticities_per_province.csv")


