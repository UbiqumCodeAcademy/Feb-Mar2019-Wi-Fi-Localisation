## Wi-Fi Localisation - Project Description

**Project Goal:** 
Investigate the possibility of using "WiFi Access Points" to determine a person's location inside closed facilities. Aim is to determine which machine learning models work best.
 
**Data characteristics:**  
We have been provided with three datasets of observations (Wi-Fi fingerprints)for a multi-building industrial campus. Each observation is associated with a location (building, floor, and location ID). Thereby, we are using the signal intensity recorded from multiple "WiFi Access Points" within the building to determine the person's location.

* Train: contains ~19K observations with 520 Wireless Access Points (WAPs), Longitude, Latitude, Floor, Timestamp, BuildingID, Relative Space ID, UserID and Phone ID
* Test: contains ~1K observations with 520 Wireless Access Points (WAPs), Longitude, Latitude, Floor, Timestamp, BuildingID and Phone ID
* Validation: ~5K observations with 520 WAPs, Longitude, Latitude and Floor are missing. **As we have three datasets, we are allowed to merge Train and Test**

**Language used:** R 

## Wi-Fi Localisation  - Technical Approach

**1. Preparing dataset:**

* Change timestamp to POSIXct format and remaining variables to factors format 
* Check and remove duplicates
* Check unique location points for Train and testset (decision to merge sets)

**2. Graphical Exploration:**

* Change wide data format to a long format using melt()
* Deep-dive into Outliers (specifically User #6)
* Bin signal strenght into several categories (https://www.metageek.com/training/resources/wifi-signal-strength-basics.html)
* Understand differences in observations between test and train set (see graph below)
![train1vstrain2](https://user-images.githubusercontent.com/45852632/53890563-b9b49580-4028-11e9-90ff-10cdc8e7d844.PNG)

**3. Creating test and training sets:**

* Plot SD of individual WAPs to understand, whether they are different from 100 (= no signal)(see graph below)
![treating waps](https://user-images.githubusercontent.com/45852632/53890631-da7ceb00-4028-11e9-8448-f0571aad8c99.PNG)
* Remove WAPs with SD smaller than 5% as they are not relevant
* Cbind train and test to one common DF with ~21K observations
* Using sample_n() to reduce the DF to 5.6K observations, which are again split in 80% - 20% train and test set

**4. Approach 1: RF Independent:**

* We use 520 WAPs to independently classify floor, latitude and longitude
* Error Metrics (see graph below)
![error metrics independent](https://user-images.githubusercontent.com/45852632/53890661-ea94ca80-4028-11e9-9ba3-259542785c11.PNG)

**5. Approach 2: RF Waterfall approach:**

* We use 520 WAPs to independently classify building, use 520 WAPs & classified building to classify floor, use 520 WAPs and classified building and floor to predict latitude, before using all predicted/classified variables to predict longitude.
* Error Metrics (see graph below)
![error metrics rf waterfall](https://user-images.githubusercontent.com/45852632/53890682-f3859c00-4028-11e9-9e8e-b466bc93973e.PNG)

**6. Approach 3: KNN Waterfall approach:**

* We use 520 WAPs to independently classify building, use 520 WAPs & classified builidng to classify floor, use 520 WAPs and classified building and floor to predict latitude, before using all predicted/classified variables to predict longitude.
* Error Metrics (see graph below)
![error metrics knn waterfall](https://user-images.githubusercontent.com/45852632/53890706-fda79a80-4028-11e9-92a0-89ea415579ca.PNG)

**7. Applying RF Models to the validation set** 
