source("Step_6_Approach_3_KNN_Cascade.R")

####
#### 4 APPLYING MODELS  ####
####

A1_validation <- validation_wide
A2_validation <- validation_wide
A3_validation <- validation_wide


### APPROACH #1: RF INDEPENDENT APPROACH

A1_vali_floor <- predict(A1_rf_tr_mdl_fl, A1_validation) #Floor
A1_vali_lat <- predict(A1_rf_tr_mdl_lat, A1_validation)
A1_vali_long <- predict(A1_rf_tr_mdl_lon, A1_validation)
A1_results <- cbind(A1_vali_lat,A1_vali_long,A1_vali_floor)
#write.csv(A1_results,file = "results_independent_RF.csv", row.names = FALSE)


### APPROACH #2: RF WATERFALL APPROACH

A2_vali_building <- predict(A2_rf_tr_mdl_bid, A2_validation) #Floor
A2_validation$BUILDINGID <- A2_vali_building #always insert the new predictions

A2_vali_floor <- predict(A2_rf_tr_mdl_fl, A2_validation)
A2_validation$FLOOR <- A2_vali_floor

A2_vali_lat <- predict(A2_rf_tr_mdl_lat, A2_validation)
A2_validation$LATITUDE <- A2_vali_lat

A2_vali_lon <- predict(A2_rf_tr_mdl_lon, A2_validation)
A2_results <- cbind(A2_vali_lat, A2_vali_lon, A2_vali_floor)
#write.csv(A2_results,file = "results_cascaded_RF.csv", row.names = FALSE)