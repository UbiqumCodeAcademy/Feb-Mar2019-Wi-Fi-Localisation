source("Preparing_dataset")

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