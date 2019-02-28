source("Approach_1_RF_Independent.R")

####
#### 3.2 APPROACH #2: RF CASCADING APPROACH ####
####

#Train and predict Building
#Train and predict Floor with predictions of Building
#Train and predict Latitude with predictions of Building and Floor
#Train and predict Longitude with predictions of Building, Floor and Latitude


### TRAIN BUILDING

# Random Forest - search for best mtry
A2_rf_tr_bmtry_bid <- tuneRF(Redu_tr_SigW[A1_rf_tr_vec], Redu_tr_SigW$BUILDINGID, 
                             ntreeTry=100,stepFactor=2,improve=0.05,trace=TRUE, plot=T) 

# Random Forest - actual model
# start_A2_rf_tr_bid <- Sys.time()
# A2_rf_tr_mdl_bid <- randomForest(y = Redu_tr_SigW$BUILDINGID, 
#                                  x = Redu_tr_SigW[A1_rf_tr_vec], importance = TRUE, 
#                                  method = "rf", ntree = 100, mtry = 19, trControl = control) 
# stop_A2_rf_tr_bid <- Sys.time()
# time_A2_rf_tr_bid_model <- stop_A2_rf_tr_bid - start_A2_rf_tr_bid #30sec
#saveRDS(A2_rf_tr_mdl_bid,"./models/A2_RF_Building.rds")
Model_A2_RF_Building <- readRDS("./models/A2_RF_Building.rds")

# Predict Building
A2_rf_bid_pred <- predict(A2_rf_tr_mdl_bid, A2_Redu_te_SigW)
A2_Redu_te_SigW$BUILDINGID <- A2_rf_bid_pred #add predicted Building ID to test set
confusionMatrix(A2_rf_bid_pred, A2_Redu_te_SigW$orig_BUILDINGID) # 99.8% & K: 0.996
A2_Redu_tr_SigW$pred_BUILDING <- A2_rf_bid_pred #so we can take it into account in the next training


#### TRAIN FLOOR

A2_rf_tr_fl_vec <- grep("WAP|BUILDINGID", names(A2_Redu_tr_SigW), value = TRUE)

# Random Forest - search for best mtry
A2_rf_tr_bmtry_fl <- tuneRF(A2_Redu_tr_SigW[A2_rf_tr_fl_vec], A2_Redu_tr_SigW$FLOOR, 
                            ntreeTry=100,stepFactor=2,improve=0.05,trace=TRUE, plot=T) 

# Random Forest - actual model
# start_A2_rf_tr_fl <- Sys.time()
# A2_rf_tr_mdl_fl <- randomForest(y = A2_Redu_tr_SigW$FLOOR, 
#                                  x = A2_Redu_tr_SigW[A2_rf_tr_fl_vec], importance = TRUE, 
#                                  method = "rf", ntree = 100, mtry = 38, trControl = control) 
# stop_A2_rf_tr_fl <- Sys.time()
# time_A2_rf_tr_fl_model <- stop_A2_rf_tr_fl - start_A2_rf_tr_fl #58sec
#saveRDS(A2_rf_tr_mdl_fl,"./models/A2_RF_Floor.rds")
Model_A2_RF_Floor <- readRDS("./models/A2_RF_Floor.rds")

# Predict Floor
A2_rf_fl_pred <- predict(A2_rf_tr_mdl_fl, A2_Redu_te_SigW)
A2_Redu_te_SigW$FLOOR <- A2_rf_fl_pred #add predicted Building ID to test set
confusionMatrix(A2_rf_fl_pred, A2_Redu_te_SigW$orig_FLOOR) # 97.9% & K: 0.974
A2_Redu_tr_SigW$pred_FLOOR <- A2_rf_fl_pred


#### TRAIN LATITUDE

A2_rf_tr_lat_vec <- grep("WAP|BUILDINGID|FLOOR", names(A2_Redu_tr_SigW), value = TRUE)

# Random Forest - search for best mtry
A2_rf_tr_bmtry_lat <- tuneRF(A2_Redu_tr_SigW[A2_rf_tr_lat_vec], A2_Redu_tr_SigW$LATITUDE, 
                             ntreeTry=100,stepFactor=2,improve=0.05,trace=TRUE, plot=T) 

# Random Forest - actual model
# start_A2_rf_tr_lat <- Sys.time()
# A2_rf_tr_mdl_lat <- randomForest(y = A2_Redu_tr_SigW$LATITUDE, 
#                                 x = A2_Redu_tr_SigW[A2_rf_tr_lat_vec], importance = TRUE, 
#                                 method = "rf", ntree = 100, mtry = 66, trControl = control) 
# stop_A2_rf_tr_lat <- Sys.time()
# time_A2_rf_tr_lat_model <- stop_A2_rf_tr_lat - start_A2_rf_tr_lat #2 min
#saveRDS(A2_rf_tr_mdl_lat,"./models/A2_RF_Latitude.rds")
Model_A2_RF_Latitude <- readRDS("./models/A2_RF_Latitude.rds")

# Predict Latitude
A2_rf_lat_pred <- predict(A2_rf_tr_mdl_lat, A2_Redu_te_SigW)
A2_Redu_te_SigW$LATITUDE <- A2_rf_lat_pred #add predicted Building ID to test set
A2_Redu_tr_SigW$pred_LATITUDE <- A2_rf_lat_pred
postResample(pred = A2_Redu_te_SigW$LATITUDE, obs = A2_Redu_te_SigW$orig_LATITUDE) #MAE: 4.0m


#### TRAIN LONGITUDE

A2_rf_tr_lon_vec <- grep("WAP|BUILDINGID|FLOOR|LATITUDE", names(A2_Redu_tr_SigW), value = TRUE)

# Random Forest - search for best mtry
A2_rf_tr_bmtry_lon <- tuneRF(A2_Redu_tr_SigW[A2_rf_tr_lon_vec], A2_Redu_tr_SigW$LONGITUDE, 
                             ntreeTry=100,stepFactor=2,improve=0.05,trace=TRUE, plot=T) 

# Random Forest - actual model
# start_A2_rf_tr_lon <- Sys.time()
# A2_rf_tr_mdl_lon <- randomForest(y = A2_Redu_tr_SigW$LONGITUDE, 
#                                  x = A2_Redu_tr_SigW[A2_rf_tr_lon_vec], importance = TRUE, 
#                                  method = "rf", ntree = 100, mtry = 132, trControl = control) 
# stop_A2_rf_tr_lon <- Sys.time()
# time_A2_rf_tr_lat_model <- stop_A2_rf_tr_lon - start_A2_rf_tr_lon #2.7min
# saveRDS(A2_rf_tr_mdl_lon,"./models/A2_RF_Longitude.rds")
Model_A2_RF_Longitude <- readRDS("./models/A2_RF_Longitude.rds")

# Predict Longitude
A2_rf_lon_pred <- predict(A2_rf_tr_mdl_lon, A2_Redu_te_SigW)
A2_Redu_te_SigW$LONGITUDE <- A2_rf_lon_pred #add predicted Building ID to test set
A2_Redu_tr_SigW$pred_LONGITUDE <- A2_rf_lon_pred
postResample(pred = A2_Redu_te_SigW$LONGITUDE, obs = A2_Redu_te_SigW$orig_LONGITUDE) #MAE is 5m


#### Error Analysis

#Classification Error Building
ggplot(A2_Redu_te_SigW, aes(x = orig_BUILDINGID, y= A2_rf_bid_pred, color = orig_FLOOR, size = 1))+
  geom_jitter()+xlab("Real Building")+ylab("Predicted Building")+ggtitle("A2-RF Waterfall: Accuracy - Building")

#Classification Error Floor
ggplot(A2_Redu_te_SigW, aes(x = orig_FLOOR, y= A2_rf_fl_pred, color = orig_BUILDINGID, size = 1))+
  geom_jitter()+xlab("Real Floor")+ylab("Predicted Floor")+ggtitle("A2-RF Waterfall: Accuracy - Floor")

#Regression Error Latitude
A2_Redu_te_SigW$Lat_MAE <- abs(A2_Redu_te_SigW$orig_LATITUDE - A2_Redu_te_SigW$LATITUDE)

ggplot(A2_Redu_te_SigW, aes(orig_LATITUDE, Lat_MAE)) + geom_smooth(color="blue", size=1.5)+
  geom_point()+ geom_hline(yintercept = 4,color = "red", size=1)+
  ggtitle("A2-RF Waterfall: MAE Analysis - Latitude")+
  ylab("Individual Mean Absolute Errors")+xlab("Actual Latitude")+
  ylim(c(0,30)) #ylim excludes 10 errors

#Regression Error Longitude
A2_Redu_te_SigW$Long_MAE <- abs(A2_Redu_te_SigW$orig_LONGITUDE - A2_Redu_te_SigW$LONGITUDE)

ggplot(A2_Redu_te_SigW, aes(orig_LONGITUDE, Long_MAE)) + geom_smooth(color="blue", size=1.5)+
  geom_point()+ geom_hline(yintercept = 5,color = "red", size=1)+
  ggtitle("A2-RF Waterfall: MAE Analysis - Longitude")+
  ylab("Individual Mean Absolute Errors")+xlab("Actual Longitude")+
  ylim(c(0,40)) #ylim is excluding 15 errors as well