source("Step_3_Creating_test_and_train_sets.R")

####
#### 3.1 APPROACH #1: RAF INDEPENDENT APPROACH ####
####


### TRAIN FLOOR

# Cross Validation 
control <- trainControl(method = "repeatedcv", number = 10, repeats = 2, returnData = TRUE)

# Random Forest - search for best mtry
A1_rf_tr_vec <- grep("WAP", names(Redu_tr_SigW), value = T)
A1_rf_tr_bmtry_fl <- tuneRF(Redu_tr_SigW[A1_rf_tr_vec], Redu_tr_SigW$FLOOR, 
                            ntreeTry=100,stepFactor=2,improve=0.05,trace=TRUE, plot=T) #mtry = 38

# Random Forest - actual model
#start_A1_rf_tr_fl <- Sys.time()
#A1_rf_tr_mdl_fl <- randomForest(y = Redu_tr_SigW$FLOOR, 
#x = Redu_tr_SigW[A1_rf_tr_vec], importance = TRUE, 
#method = "rf", ntree = 100, mtry = 38, trControl = control) 
#stop_A1_rf_tr_fl <- Sys.time()
#time_A1_rf_tr_fl_model <- stop_A1_rf_tr_fl - start_A1_rf_tr_fl #1.3 min
#saveRDS(A1_rf_tr_mdl_fl,"./models/A1_RF_Floor.rds")
Model_A1_RF_Floor <- readRDS("./models/A1_RF_Floor.rds")


### TRAIN LATITUDE

# Random Forest - search for best mtry
A1_rf_tr_bmtry_lat <- tuneRF(Redu_tr_SigW[A1_rf_tr_vec], Redu_tr_SigW$LATITUDE,  
                             ntreeTry=100,stepFactor=2,improve=0.05,trace=TRUE, plot=T) #mtry = 66

# Random Forest - actual model
#start_A1_rf_tr_lat <- Sys.time()
#A1_rf_tr_mdl_lat <- randomForest(y = Redu_tr_SigW$LATITUDE, 
#x = Redu_tr_SigW[A1_rf_tr_vec], importance = TRUE, 
#method = "rf", ntree = 100, mtry = 66, trControl = control) 
#stop_A1_rf_tr_lat <- Sys.time()
#time_A1_rf_tr_lat_model <- stop_A1_rf_tr_lat - start_A1_rf_tr_lat #1.8 min
#saveRDS(A1_rf_tr_mdl_lat,"./models/A1_RF_Latitude.rds")
Model_A1_RF_Latitude <- readRDS("./models/A1_RF_Latitude.rds")


### TRAIN LONGITUDE

# Random Forest - search for best mtry
A1_rf_tr_bmtry_lon <- tuneRF(Redu_tr_SigW[A1_rf_tr_vec], Redu_tr_SigW$LONGITUDE, 
                             ntreeTry=100,stepFactor=2,improve=0.05,trace=TRUE, plot=T) #mtry = 66

# Random Forest - actual model
# start_A1_rf_tr_lon <- Sys.time()
# A1_rf_tr_mdl_lon <- randomForest(y = Redu_tr_SigW$LONGITUDE, 
#                                  x = Redu_tr_SigW[A1_rf_tr_vec], importance = TRUE, 
#                                  method = "rf", ntree = 100, mtry = 66, trControl = control) 
# stop_A1_rf_tr_lon <- Sys.time()
# time_A1_rf_tr_lon_model <- stop_A1_rf_tr_lon - start_A1_rf_tr_lon 
#saveRDS(A1_rf_tr_mdl_lon,"./models/A1_RF_Longitude.rds")
Model_A1_RF_Longitude <- readRDS("./models/A1_RF_Longitude.rds")


#### TEST FLOOR

A1_rf_fl_pred <- predict(A1_rf_tr_mdl_fl, Redu_te_SigW)
Redu_te_SigW$FLOOR <- A1_rf_fl_pred #add predicted Building ID to test set
confusionMatrix(A1_rf_fl_pred, Redu_te_SigW$orig_FLOOR) #A: 98.2% & Kappa: 0.976

#### TEST LATITUDE

A1_rf_lat_pred <- predict(A1_rf_tr_mdl_lat, Redu_te_SigW)
Redu_te_SigW$LATITUDE <- A1_rf_lat_pred #add predicted Building ID to test set
postResample(pred = Redu_te_SigW$LATITUDE, obs = Redu_te_SigW$orig_LATITUDE) #4.2m

#### TEST LONGITUDE

A1_rf_lon_pred <- predict(A1_rf_tr_mdl_lon, Redu_te_SigW)
Redu_te_SigW$LONGITUDE <- A1_rf_lon_pred #add predicted Building ID to test set
postResample(pred = Redu_te_SigW$LONGITUDE, obs = Redu_te_SigW$orig_LONGITUDE) #5.1m

#### Error Analysis

#Classification Error Floor
ggplot(Redu_te_SigW, aes(x = orig_FLOOR, y= A1_rf_fl_pred, color = orig_BUILDINGID, size = 1))+
  geom_jitter()+xlab("Real Floor")+ylab("Predicted Floor")+ggtitle("A1- RF Independent: Accuracy Floor ")

#Regression Error Latitude
Redu_te_SigW$Lat_MAE <- abs(Redu_te_SigW$orig_LATITUDE - Redu_te_SigW$LATITUDE)

ggplot(Redu_te_SigW, aes(orig_LATITUDE, Lat_MAE)) + geom_smooth(color="blue", size=1.5)+
  geom_point()+ geom_hline(yintercept = 4.2,color = "red", size=1)+
  ggtitle("A1-RF Independent: MAE Analysis - Latitude")+
  ylab("Individual Mean Absolute Errors")+xlab("Actual Latitude")+
  ylim(c(0,30)) #ylim excludes 10 errors

#Regression Error Longitude
Redu_te_SigW$Long_MAE <- abs(Redu_te_SigW$orig_LONGITUDE - Redu_te_SigW$LONGITUDE)

ggplot(Redu_te_SigW, aes(orig_LONGITUDE, Long_MAE)) + geom_smooth(color="blue", size=1.5)+
  geom_point()+ geom_hline(yintercept = 4.2,color = "red", size=1)+
  ggtitle("A1-RF Independent: MAE Analysis - Longitude")+
  ylab("Individual Mean Absolute Errors")+xlab("Actual Longitude")+
  ylim(c(0,50)) #ylim is excluding 10 
