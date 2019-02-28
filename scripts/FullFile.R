# Main file
# Florian UNGER
# 25/2/2019

### Packages 

install.packages(c("plot3Drgl", "corrplot", "plot3D", "rgl", 
                   "car", "manipulateWidget", "reshape" , 
                   "reshape2", "Rfast", "randomForest", "rstudioapi",
                   "esquisse"))

pacman::p_load(plot3Drgl, rgl, car, ggplot2, plotly, rstudioapi)

esquisse:: esquisser()

### Github

current_path = rstudioapi::getActiveDocumentContext()$path #save working directory
setwd(dirname(current_path))
setwd("..")

### Data files 

train_wide <- read.csv("C:/Users/Dell/Desktop/Ubiqum Data Analytics/IoT/Wifi/trainingData.csv",
                       header = TRUE)
test_wide <- read.csv("C:/Users/Dell/Desktop/Ubiqum Data Analytics/IoT/Wifi/validationData.csv",
                      header = TRUE)
validation_wide <- read.csv("C:/Users/Dell/Desktop/Ubiqum Data Analytics/IoT/Wifi/testData.csv",
                      header = TRUE)

#Changing variables to factors & POSIXct
varis <- c("FLOOR", "BUILDINGID", "SPACEID", 
           "RELATIVEPOSITION", "USERID", "PHONEID")
for (v in varis){
  train_wide[,v] <- as.factor(train_wide[,v])}
train_wide$TIMESTAMP <- as.POSIXct(train_wide$TIMESTAMP, origin = "1970-01-01")  

for (v in varis){
  test_wide[,v] <- as.factor(test_wide[,v])}
test_wide$TIMESTAMP <- as.POSIXct(test_wide$TIMESTAMP, origin = "1970-01-01") 

#rename variables 
levels(test_wide$BUILDINGID) <- c("TI", "TD", "TC")
levels(train_wide$BUILDINGID) <- c("TI", "TD", "TC")

#unique values - decision to merge sets
unique_train <- unique(paste(train_wide$LONGITUDE, train_wide$LATITUDE, train_wide$FLOOR)) #933 locations
unique_test <- unique(paste(test_wide$LONGITUDE, test_wide$LATITUDE, test_wide$FLOOR)) #1061 locations

#remove duplicates
train_wide <- unique(train_wide)
test_wide <- unique(test_wide)

#label font
black.bold.italic.16.text <- element_text(face = "bold.italic", color = "black", size = 16)
black.16.text <- element_text(color = "black", size = 16)

####
#### Initial Graphical Exploration ####
####

#change to long format for visualisation purposes 
id.vars <-  c("LONGITUDE","LATITUDE","FLOOR","BUILDINGID","SPACEID","RELATIVEPOSITION",
              "USERID","PHONEID", "TIMESTAMP")
train_long <- train_wide %>% melt(id.vars) %>% 
  mutate(value = value + 105) %>% mutate(value = case_when(value == 205 ~ 0, TRUE ~ value))

test_long <- test_wide %>% melt(id.vars) %>% 
  mutate(value = value + 105) %>% mutate(value = case_when(value == 205 ~ 0, TRUE ~ value))

train_long_wo$SET <- "TRAIN"
test_long_wo$SET <- "TEST"
train_test_wo <- rbind(train_long_wo, test_long_wo)

buildingTI <- filter(train_long_wo, train_long_wo$BUILDINGID =="TI")
ggplot(data = buildingTI) +
  aes(x = LONGITUDE, y = LATITUDE, color = Intensity, size = 1) +
  geom_point() + theme_minimal() + facet_wrap(vars(FLOOR))+
  labs(title = "Train: TI-Floors by observation", x = "Longitude", y = "Latitude",
       subtitle = "Most observations are useless as they do not display any Wi-Fi strength")
  
#delete rows with no WAPs connections
train_long_wo <- filter(train_long, train_long$value != 0)
test_long_wo <- filter(test_long, test_long$value != 0)

#bin the WAPS into categories 
train_long_wo$Intensity <- ifelse(train_long_wo[,c("value")] > 75, "Outliers",
                                  ifelse(train_long_wo[,c("value")] > 38, "Very Good",
                                         ifelse(train_long_wo[,c("value")] > 34, "OK",
                                                ifelse(train_long_wo[,c("value")] > 25, "not good", "useless"))))

test_long_wo$Intensity <- ifelse(test_long_wo[,c("value")] > 75, "Outliers",
                                 ifelse(test_long_wo[,c("value")] > 38, "Very Good",
                                        ifelse(test_long_wo[,c("value")] > 34, "OK",
                                               ifelse(test_long_wo[,c("value")] > 25, "not good", "useless"))))

train_long_wo$Intensity <- as.factor(train_long_wo$Intensity)
test_long_wo$Intensity <- as.factor(test_long_wo$Intensity)

#Wireless Access Points strengths
ggplot(test_long_wo, aes(value)) + geom_bar() #no outliers in test data
ggplot(train_long_wo, aes(value)) + geom_bar() #outliers in training data 

ggplot(train_long_wo, aes(value, color = FLOOR, fill = FLOOR)) + geom_bar()+ 
  scale_y_continuous(trans='log2')+ ggtitle("Set I: Log-scale: Strength of WAPs")
ggplot(test_long_wo, aes(value, color = FLOOR, fill = FLOOR)) + geom_bar()+ 
  scale_y_continuous(trans='log2')+ ggtitle("Set II: Log-scale: Strength of WAPs")

ggplot(train_long_wo, aes(Intensity)) + geom_bar(stat = "count")+
  geom_text(aes(label=..count..),stat="count",position=position_stack(0.5), cex=5)+
  ggtitle("Count of WAPs in Subcategories")#775 outliers in train

ggplot(data = train_long_wo) + aes(x = Intensity, fill = FLOOR) +
  geom_bar(stat = "count") +
  theme_minimal()+
  ggtitle("Count of WAPs in Subcategories")

ggplot(test_long_wo, aes(Intensity)) + geom_bar()

#Deep-dive into outliers 
Out_data <- filter(train_long_wo, Intensity == "Outliers")

ggplot(Out_data, aes(FLOOR, fill=USERID)) + geom_bar() + facet_grid(Out_data$BUILDINGID)+
  xlab("Floor")+ylab("Count")+ggtitle("Outliers by Building, Floor, User")#Outliers in TC floor 3,4

ggplot(Out_data, aes(USERID, fill=FLOOR)) + geom_bar()+ facet_grid(Out_data$BUILDINGID)+
  ggtitle("Outliers after Building and User")+
  theme(axis.text = black.bold.italic.16.text)#user 6 is causing the outliers in TC (Floor 3,4)

ggplot(Out_data, aes(PHONEID)) + geom_bar() #using phone #19
ggplot(Out_data, aes(USERID)) + geom_bar() + facet_grid(Out_data$FLOOR) #user 6 only on Floor 3,4
ggplot(train_long_wo, aes(USERID)) + geom_bar() + facet_grid(train_long_wo$FLOOR) #sig. amount of Floor 3,4 obs done by user 6 -> leave in

#Floor 3/4 User 6 behaviour
Floor_3_4_train <- train_long_wo %>% filter(FLOOR == 3|4)
ggplot(Floor_3_4_train, aes(USERID, fill=Intensity)) + geom_bar()

Floor_3_train <- train_long_wo %>% filter(FLOOR == 3)
ggplot(Floor_3_train, aes(USERID, fill=FLOOR)) + geom_bar()

#difference between test and train
train_long_wo$SET <- "TRAIN"
test_long_wo$SET <- "TEST"
train_test_wo <- rbind(train_long_wo, test_long_wo)

tr_te_wo_TI <- train_test_wo %>% filter(BUILDINGID == "TI")
tr_te_wo_TI_3 <- tr_te_wo_TI %>% filter(FLOOR == 3)

ggplot(data = tr_te_wo_TI_3) +
  aes(x = LONGITUDE, y = LATITUDE, color = SET, size = 2) +
  geom_point() + theme_minimal() + ggtitle("TI: 3rd Floor: Test vs Train")

ggplot(data = Floor_4_train) + aes(x = USERID, fill = Intensity) + geom_bar() + 
  scale_fill_brewer(palette = "Dark2") +   labs(title = "TC:Floor 4 observations",
       x = "User ID", y = "Count",
       subtitle = "User 6 captures valuable information") + theme_minimal()

User_6_train <- train_long_wo %>% filter(USERID == 6)
ggplot(User_6_train, aes(value, fill=FLOOR)) + geom_bar()#User 6 too important to eliminate
  
ggplot(User_6_train, aes(TIMESTAMP, value)) + geom_jitter() #outliers irrelevant of time 

#Plot 1: ggplot with facet_grid
ggplot(train_long, aes(LONGITUDE, LATITUDE, colour = factor(FLOOR))) + geom_point() + 
  facet_grid(FLOOR~., scales = "free") 

#Plot 2: plot_ly and scatter3D (interactive)
plot_ly(train_long,type = "scatter3d", x = ~ LONGITUDE, y= ~LATITUDE, z=~FLOOR, mode = "markers",
        marker = list(size = 4, color = ~ FLOOR, size = 1, replace = TRUE))

####
#### Creating test and training samples to test different approaches ####
####

#merge training and test set
train_wide$SETTYPE <- "train"
test_wide$SETTYPE <- "test"
train_and_test <- rbind(train_wide,test_wide) #combine both sets
notcommon <- list("SPACEID", "RELATIVEPOSITION", "USERID", "PHONEID") #delete non-mutual columns
for (n in notcommon){
  train_and_test[,n] <- NULL}

#understand if all WAPs are significant in train_and_test (no, therefore remove SD <5)
SD_train_and_test <- sapply(train_and_test, sd)
SD_train_and_test_df <- as.data.frame(SD_train_and_test)
colnames(SD_train_and_test_df) <- "SD"

#plot SD of individual WAPs
ggplot(SD_train_and_test_df,aes(SD)) + geom_histogram(colour="black", fill="white", binwidth = 1)+
  geom_density(aes(y=10*..count..), colour="black", adjust=1)+xlim(-1, 70)+
  stat_bin(binwidth=2, geom="text", colour="black", size=3.5, aes(label=..count..),vjust = -1)+
  theme(axis.text = black.16.text)+ geom_vline(xintercept = 5,color = "red", size=1.5)+
  labs(title = "SD per WAPs",
       x = "SD", y = "Count",
       subtitle = "with a 5% SD threshold 127 WAPs are eliminated")

#remove WAPs with SD <5
AllW_tr_and_te <- grep("WAP", names(train_and_test), value = TRUE)
tr_and_te_with_SigW <- train_and_test[-which(apply(train_and_test[AllW_tr_and_te], 2, sd) <= 5)] #remove 127 WAPs

#1 sample 'train & test set with Sig WAPs' after building and floor
Redu_tr_te_SigW <- tr_and_te_with_SigW %>% group_by(BUILDINGID, FLOOR) %>% sample_n(550)

#2 split this reduced set then in training and test
Redu_tr_SigW <- Redu_tr_te_SigW %>% group_by(BUILDINGID, FLOOR) %>% sample_n(440)
Redu_te_SigW <- anti_join(Redu_tr_te_SigW, Redu_tr_SigW)
Redu_te_SigW <- plyr::rename(Redu_te_SigW, orig_FLOOR = FLOOR,
                       orig_BUILDINGID = BUILDINGID, 
                       orig_LATITUDE = LATITUDE,
                       orig_LONGITUDE = LONGITUDE)

# Create separate train/test sets to make different approaches comparable
A2_Redu_tr_SigW <- Redu_tr_SigW
A3_Redu_tr_SigW <- Redu_tr_SigW

A2_Redu_te_SigW <- Redu_te_SigW
A3_Redu_te_SigW <- Redu_te_SigW

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

