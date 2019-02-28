source("Graphical_Exploration.R")

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