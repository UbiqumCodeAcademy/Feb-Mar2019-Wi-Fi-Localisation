# Preparing data set
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

### Securing package location
Sys.getenv("R_LIBS_USER") #we checked where the libraries are stored
.libPaths("C:\\Users\\Dell\\Documents\\R\\win-library\\3.5") #manually chage the lib paths
.libPaths()

### Data files 

train_wide <- read.csv("C:/Users/Dell/Desktop/Ubiqum Data Analytics/IoT/Wi-Fi Localisation/datasets/trainingData.csv",
                       header = TRUE)
test_wide <- read.csv("C:/Users/Dell/Desktop/Ubiqum Data Analytics/IoT/Wi-Fi Localisation/datasets/validationData.csv",
                      header = TRUE)
validation_wide <- read.csv("C:/Users/Dell/Desktop/Ubiqum Data Analytics/IoT/Wi-Fi Localisation/datasets/testData.csv",
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