source("Step_5_Approach_2_RF_Cascade.R")

####
#### 3.3 APPROACH #3: KNN CASCADING APPROACH ####
####


### TRAIN BUILDING 

A3_rf_tr_vec <- grep("WAP", names(A3_Redu_tr_SigW), value = T)

metric_ID <- "Accuracy"

# start_A3_knn_tr_bid <- Sys.time()
# A3_knn_tr_mdl_bid <- train(y = A3_Redu_tr_SigW$BUILDINGID, 
#                              x = A3_Redu_tr_SigW[A3_rf_tr_vec],
#                              method = "knn", trControl = control, metric = metric_ID) 
# stop_A3_knn_tr_bid <- Sys.time()
# time_A3_knn_tr_bid <- stop_A3_knn_tr_bid - start_A3_knn_tr_bid #3 min 
# saveRDS(A3_knn_tr_mdl_bid,"./models/A3_KNN_Building.rds")
Model_A3_KNN_Building <- readRDS("./models/A3_KNN_Building.rds")


# Predict Building
A3_knn_bid_pred <- predict(A3_knn_tr_mdl_bid, A3_Redu_te_SigW)
A3_Redu_te_SigW$BUILDINGID <- A3_knn_bid_pred #add predicted Building ID to test set
confusionMatrix(A3_knn_bid_pred, A3_Redu_te_SigW$orig_BUILDINGID) # 99.3% & K: 0.989


### TRAIN FLOOR 

A3_rf_tr_vec_fl <- grep("WAP|BUILDINGID", names(A3_Redu_tr_SigW), value = T)

# start_A3_knn_tr_fl <- Sys.time()
# A3_knn_tr_mdl_fl <- train(y = A3_Redu_tr_SigW$FLOOR, 
#                            x = A3_Redu_tr_SigW[A3_rf_tr_vec],
#                            method = "knn", trControl = control, metric = metric_ID) 
# stop_A3_knn_tr_fl <- Sys.time()
# time_A3_knn_tr_fl <- stop_A3_knn_tr_fl - start_A3_knn_tr_fl #3 min 
# saveRDS(A3_knn_tr_mdl_fl,"./models/A3_KNN_Floor.rds")
Model_A3_KNN_Floor <- readRDS("./models/A3_KNN_Floor.rds")

# Predict Floor
A3_knn_fl_pred <- predict(A3_knn_tr_mdl_fl, A3_Redu_te_SigW)
A3_Redu_te_SigW$FLOOR <- A3_knn_fl_pred 
confusionMatrix(A3_knn_fl_pred, A3_Redu_te_SigW$orig_FLOOR) # 92.3% & K: 0.9015


### TRAIN LATITUDE 

metric_ID_lon <- "Rsquared"

## Train set transformation 
#as KNN is distance based you need to dummify the buildings & normalise the WAPs

#dummify the Buildings
dum <- dummyVars(~., data = A3_Redu_tr_SigW[c("BUILDINGID")])
pred <- predict(dum, newdata = A3_Redu_tr_SigW)
pred1 <- as.data.frame(pred)

# Creating new trainings set
A3_Redu_tr_SigW_dum <- A3_Redu_tr_SigW

# Changing 100 (na) to weakest (-105)
A3_Redu_tr_SigW_dum[,1:393] <- apply(
  A3_Redu_tr_SigW_dum[,1:393], 2, function(x) ifelse(x == 100, -105, x)
)

# Normalising
preprocesswaps <- preProcess(A3_Redu_tr_SigW_dum[,1:393], method = c("range"))
trans_A3_Redu_tr_SigW_dum <- predict(preprocesswaps, A3_Redu_tr_SigW_dum[,1:393])

#Add dummified variables back in the train set
trans_A3_Redu_tr_SigW_dum$BuildingIDTC <- pred1$BUILDINGID.TC
trans_A3_Redu_tr_SigW_dum$BuildingIDTI <- pred1$BUILDINGID.TI
trans_A3_Redu_tr_SigW_dum$BuildingIDTD <- pred1$BUILDINGID.TD
trans_A3_Redu_tr_SigW_dum$LATITUDE <- A3_Redu_tr_SigW$LATITUDE #needs to be original value for training


### TEST SET TRANSFORMATION

#dummify the Buildings
dumtest <- dummyVars(~., data = A3_Redu_te_SigW[c("BUILDINGID")])
predtest <- predict(dumtest, newdata = A3_Redu_te_SigW)
pred1test <- as.data.frame(predtest)

# Creating new test set
A3_Redu_te_SigW_dum <- A3_Redu_te_SigW

# Changing 100 (na) to weakest (-105)
A3_Redu_te_SigW_dum[,1:393] <- apply(
  A3_Redu_te_SigW_dum[,1:393], 2, function(x) ifelse(x == 100, -105, x)
)

# Normalising
preprocesswapstest <- preProcess(A3_Redu_te_SigW_dum[,1:393], method = c("range"))
trans_A3_Redu_te_SigW_dum <- predict(preprocesswapstest, A3_Redu_te_SigW_dum[,1:393])

#Add dummified variables back in the train set
trans_A3_Redu_te_SigW_dum$BuildingIDTC <- pred1test$BUILDINGID.TC
trans_A3_Redu_te_SigW_dum$BuildingIDTI <- pred1test$BUILDINGID.TI
trans_A3_Redu_te_SigW_dum$BuildingIDTD <- pred1test$BUILDINGID.TD
trans_A3_Redu_te_SigW_dum$orig_LATITUDE <- A3_Redu_te_SigW$LATITUDE

#Create Vector of independent variables
A3_rf_tr_vec_lat <- grep("WAP|Building", names(trans_A3_Redu_tr_SigW_dum), value = T)

# Knn model
# start_A3_knn_tr_lat <- Sys.time()
# A3_knn_tr_mdl_lat <- train(y = trans_A3_Redu_tr_SigW_dum$LATITUDE, 
#                           x = trans_A3_Redu_tr_SigW_dum[A3_rf_tr_vec_lat],
#                           method = "knn", trControl = control, metric = metric_ID_lon) 
# stop_A3_knn_tr_lat <- Sys.time()
# time_A3_knn_tr_lat <- stop_A3_knn_tr_lat - start_A3_knn_tr_lat #3 min 
# saveRDS(A3_knn_tr_mdl_lat,"./models/A3_KNN_Latitude.rds")
Model_A3_KNN_Latitude <- readRDS("./models/A3_KNN_Latitude.rds")

# Predict Latitude
A3_knn_lat_pred <- predict(A3_knn_tr_mdl_lat, trans_A3_Redu_te_SigW_dum)
trans_A3_Redu_te_SigW_dum$LATITUDE <- A3_knn_lat_pred 
postResample(pred = trans_A3_Redu_te_SigW_dum$LATITUDE, obs = trans_A3_Redu_te_SigW_dum$orig_LATITUDE) #MAE: 3.7m


### TEST LONGITUDE KNN 

trans_A3_Redu_tr_SigW_dum$LONGITUDE <- A3_Redu_tr_SigW$LONGITUDE
trans_A3_Redu_te_SigW_dum$orig_LONGITUDE <- A3_Redu_te_SigW$orig_LONGITUDE
colnames(trans_A3_Redu_tr_SigW_dum)

# Normalising
preprocesswaps1 <- preProcess(A3_Redu_tr_SigW_dum[,1:393,395], method = c("range"))
trans_A3_Redu_tr_SigW_dum1 <- predict(preprocesswaps1, A3_Redu_tr_SigW_dum[,1:393,395])

#Create Vector of independet variables
A3_rf_tr_vec_lon <- grep("WAP|Building|LATITUDE", names(trans_A3_Redu_tr_SigW_dum1), value = T)

# start_A3_knn_tr_lon <- Sys.time()
# A3_knn_tr_mdl_lon <- train(y = trans_A3_Redu_tr_SigW_dum$LONGITUDE, 
#                               x = trans_A3_Redu_tr_SigW_dum[A3_rf_tr_vec_lon],
#                               method = "knn", trControl = control, metric = metric_ID_lon) 
# stop_A3_knn_tr_lon <- Sys.time()
# time_knn_Lon <- stop_A3_knn_tr_lon - start_A3_knn_tr_lon 
# saveRDS(A3_knn_tr_mdl_lon,"./models/A3_KNN_Longitude.rds")
Model_A3_KNN_Longitude <- readRDS("./models/A3_KNN_Longitude.rds")

# Predict Longitude
A3_knn_lon_pred <- predict(A3_knn_tr_mdl_lon, trans_A3_Redu_te_SigW_dum)
trans_A3_Redu_te_SigW_dum$LONGITUDE <- A3_knn_lon_pred 
postResample(pred = trans_A3_Redu_te_SigW_dum$LONGITUDE, obs = trans_A3_Redu_te_SigW_dum$orig_LONGITUDE) #MAE: 5.18


### Error Analysis

#Classification Error Building
ggplot(A3_Redu_te_SigW, aes(x = orig_BUILDINGID, y= A3_knn_bid_pred, color = orig_FLOOR, size = 1))+
  geom_jitter()+xlab("Real Building")+ylab("Predicted Building")+ggtitle("A3-KNN Waterfall: Accuracy - Building")

#Classification Error Floor
#after Building
ggplot(A3_Redu_te_SigW, aes(x = orig_FLOOR, y= A3_knn_fl_pred, color = orig_BUILDINGID, size = 1))+
  geom_jitter()+xlab("Real Floor")+ylab("Predicted Floor")+ggtitle("A3-KNN Waterfall: Accuracy - Floor")
#after settype
ggplot(A3_Redu_te_SigW, aes(x = orig_FLOOR, y= A3_knn_fl_pred, color = SETTYPE, size = 1))+
  geom_jitter()+xlab("Real Floor")+ylab("Predicted Floor")+ggtitle("A3-KNN Waterfall: Accuracy - Floor")

#Regression Error Latitude
trans_A3_Redu_te_SigW_dum$Lat_MAE <- abs(trans_A3_Redu_te_SigW_dum$orig_LATITUDE - trans_A3_Redu_te_SigW_dum$LATITUDE)

ggplot(trans_A3_Redu_te_SigW_dum, aes(orig_LATITUDE, Lat_MAE)) + geom_smooth(color="blue", size=1.5)+
  geom_point()+ geom_hline(yintercept = 3.7,color = "red", size=1)+
  ggtitle("A3-KNN Waterfall: MAE Analysis - Latitude")+
  ylab("Individual Mean Absolute Errors")+xlab("Actual Latitude")+
  ylim(c(0,30)) #ylim excludes 10 errors

#Regression Error Longitude
trans_A3_Redu_te_SigW_dum$Long_MAE <- abs(trans_A3_Redu_te_SigW_dum$orig_LONGITUDE - trans_A3_Redu_te_SigW_dum$LONGITUDE)

ggplot(trans_A3_Redu_te_SigW_dum, aes(orig_LONGITUDE, Long_MAE)) + geom_smooth(color="blue", size=1.5)+
  geom_point()+ geom_hline(yintercept = 5,color = "red", size=1)+
  ggtitle("A3-KNN Waterfall: MAE Analysis - Longitude")+
  ylab("Individual Mean Absolute Errors")+xlab("Actual Longitude")+
  ylim(c(0,40)) #ylim is excluding 15 errors as well
