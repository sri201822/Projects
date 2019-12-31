### Assignment-Linear Regression (Geely Auto)

requiredPackages = c('ggplot2','tidyr','plyr','MASS','car')

# used these packages below during analysis

#Installing the required packages if not already installed and activating all packages using library()

for(item in requiredPackages){
  if(!require(item,character.only = TRUE)){
    install.packages(item)
  }
  library(item, character.only = TRUE)
}

# All the pacakges required are installed and activated.

## Step 1 : Set working directory and load the file
### Import data
car_df<- read.csv("CarPrice_Assignment.csv",stringsAsFactors = F)
# View(car_df)
dim(car_df) # dimensions of data frame
# 205 rows and 26 columns

sum(!complete.cases(car_df)) #0---No incomplete cases
sum(is.na(car_df))#0--- no NAs
str(car_df)

####################################################################################################################
##################################### Data Preparation and Data Cleaning ##########################################
###################################################################################################################

### we need to separate CarName variable into Company  and Car_Model for further analysis
library(tidyr)
car_df <-separate(car_df, CarName,into = c("Company", "Car_Model"),sep = " ",extra = "merge")
sum(is.na(car_df)) #2-- For row indices 139,142 there is no Car_Model names so it is filled with NA
str(car_df)

# Replace NAs in Car_Model column with "missing"
sum(is.na(car_df))# 2 NAs in row indices 139,142  in column--- Car_Model

## Replacing NAs with missing
car_df$Car_Model[which(is.na(car_df$Car_Model))] <- "missing"

####################################################################################################################
#################################Check for duplicates 

unique(car_df$car_ID)
sum(duplicated(car_df$car_ID)) # No duplicates in car_ID column
sum(duplicated(car_df))### No duplicates rows in entire dataset

##########################################################################################################
#################################### Correcting spelling mistakes (Company,Car_Model columns)

levels(as.factor(car_df$Company))

# we can see that there are few spelling mistakes in company names which requires correction
# maxda to mazda
# Nissan to nissan
# porcshce to porsche
# toyouta to toyota
# vokswagen to volkswagen
# vw to volkswagen
### Let's correct all these company names
# install.packages('plyr')
library(plyr) # used mapvalues() from this package to correct spellings

car_df$Company<-mapvalues(car_df$Company, from = c("maxda", "porcshce", "vokswagen","vw", "Nissan", "toyouta"),
                          to = c("mazda","porsche","volkswagen","volkswagen","nissan","toyota"))

# mapvalues() is a function from plyr package which is used to correct Company names here

levels(as.factor(car_df$Company)) # Now everything seems fine
unique(car_df$Company)


######### Verifying if any correction required in Car_Model columnn (Spelling mistakes)
unique(car_df$Car_Model)
# we can see that 100ls,100 ls are treated as 2 different unique values but both of them are same

car_df$Car_Model[which(car_df$Car_Model %in% c("100 ls"))] <- "100ls"
unique(car_df$Car_Model)
unique(as.factor(car_df$Car_Model)) # 141 levels

###################################################################################################################
####################### Converting strings to factors 
str(car_df)
# converting Company,Car_Model,symboling into factors
car_df$symboling<-as.factor(car_df$symboling)

car_df$Company <- as.factor(car_df$Company)

car_df$Car_Model<-as.factor(car_df$Car_Model)

################################################################################################################
########################### Conversion of categorical variables into numeric####################################
################################################################################################################

# In linear Regression model, all dependent and independent variables should be numerical
################################################################################################################
############## convertng factors with 2 levels to numerical variables

car_df$fueltype <- as.factor(car_df$fueltype)
levels(car_df$fueltype) <- c(0,1) # diesel (0), gas(1)
car_df$fueltype <- as.numeric(levels(car_df$fueltype))[car_df$fueltype]
#View(car_df$fueltype)
summary(as.factor(car_df$fueltype))
# diesel--20
# gas--185


car_df$aspiration <- as.factor(car_df$aspiration)
levels(car_df$aspiration)<-c(0,1) # std(0),Turbo(1)
car_df$aspiration<-as.numeric(levels(car_df$aspiration))[car_df$aspiration]
# car_df$aspiration
summary(as.factor(car_df$aspiration))
# std---168
# Turbo---37

car_df$doornumber<-as.factor(car_df$doornumber)
levels(car_df$doornumber)<-c(0,1) # four(0),two(1)
car_df$doornumber<-as.numeric(levels(car_df$doornumber))[car_df$doornumber]
# car_df$doornumber
summary(as.factor(car_df$doornumber))
# four---115
# two---90

car_df$enginelocation <- as.factor(car_df$enginelocation)
levels(car_df$enginelocation)<-c(0,1) # front(0),rear(1)
car_df$enginelocation <- as.numeric(levels(car_df$enginelocation))[car_df$enginelocation]
# car_df$enginelocation
summary(as.factor(car_df$enginelocation))
# front---202
# rear---3

###############################################################################################################
####################### Creating dummy variables for factors with more than 2 levels

#### Creating dummy variables for symboling
str(car_df$symboling)
summary(car_df$symboling)
levels(as.factor(car_df$symboling))
dummy_symboling <- data.frame(model.matrix( ~symboling,data = car_df))
#View(dummy_symboling)
dummy_symboling<-dummy_symboling[,-1]
str(dummy_symboling)
summary(dummy_symboling)

#### Company variable
levels(as.factor(car_df$Company))
dummy_company <- data.frame(model.matrix( ~Company, data = car_df))
#View(dummy_company)
dummy_company<-dummy_company[,-1] 
str(dummy_company)
summary(dummy_company)

#### carbody
dummy_cbody <- data.frame(model.matrix( ~carbody,data=car_df))
#View(dummy_cbody)
dummy_cbody <- dummy_cbody[,-1] 
str(dummy_cbody)
summary(dummy_cbody)

#### drivewheel
dummy_drivewheel <- data.frame(model.matrix( ~drivewheel,data = car_df))
#View(dummy_drivewheel)
dummy_drivewheel<-dummy_drivewheel[,-1] 
str(dummy_drivewheel)
summary(dummy_drivewheel)

#### enginetype
dummy_etype <- data.frame(model.matrix( ~enginetype,data = car_df))
#View(dummy_etype)
dummy_etype <- dummy_etype[,-1]
str(dummy_etype)
summary(dummy_etype)

#### cylinder number
dummy_cnum <- data.frame(model.matrix(~cylindernumber,data = car_df))
#View(dummy_cnum)
dummy_cnum <- dummy_cnum[,-1]
str(dummy_cnum)
summary(dummy_cnum)

#### fuelsystem
dummy_fuelsys <- data.frame(model.matrix(~fuelsystem,data = car_df))
#View(dummy_fuelsys)
dummy_fuelsys <- dummy_fuelsys[,-1]
str(dummy_fuelsys)
summary(dummy_fuelsys)


########################################################################################################################
################### Replacing categorical variables with dummy variables created

# Now replace the created dummy variables in place of categorical variables in car_df dataset all at once

car_df <- cbind(car_df[,setdiff(names(car_df),
            c('symboling','Company','carbody','drivewheel','enginetype','cylindernumber','fuelsystem'))],
              dummy_symboling,dummy_company,dummy_cbody,dummy_drivewheel,dummy_etype,dummy_cnum,dummy_fuelsys)

##########################################################################################################
##################### Dealing with numeric variables and removing outliers#################################
###########################################################################################################

################################### Check the outliers and do outlier capping

# Carlength outliers:
#Using Box plot for identiying outliers
boxplot(car_df$carlength, main="Car Length")
quantile(x = car_df$carlength,seq(0,1,0.01))
summary(car_df$carlength)
# Mean and Median are close. so no outliers

# carheight outliers: 
#Using Box plot for identiying outliers
boxplot(car_df$carheight, main="Car Height")
quantile(car_df$carheight,seq(0,1,0.01))
summary(car_df$carheight)
#Mean and Median are close. so no outliers

#carwidth outliers: 
boxplot(car_df$carwidth, main="Car Width")
quantile(car_df$carwidth,seq(0,1,0.01))
summary(car_df$carwidth)
# Mean and median are close to each other. so no outliers

#highwaympg outliers: 
boxplot(car_df$highwaympg, main="highwaympg")
quantile(x = car_df$highwaympg,seq(0,1,0.01))
summary(car_df$highwaympg)
#Mean and median are close to each other. so no outliers

# citympg outliers: 
boxplot(car_df$citympg, main="citympg")
quantile(x = car_df$citympg,seq(0,1,0.01))
summary(car_df$citympg)
#Mean and median are close to each other. So no outliers

# peak rpm outliers: 
boxplot(car_df$peakrpm, main="peakrpm")
quantile(x = car_df$peakrpm,seq(0,1,0.01))
summary(car_df$peakrpm)
# Mean and median are close to each other. So no outliers found

# horspower outliers: (Outliers present)
boxplot(car_df$horsepower, main="Horsepower")
# From boxplot we can see that there are few outliers and we need to do outliercapping
quantile(x = car_df$horsepower,seq(0,1,0.01))
# see that 99% of data contains 207 as maximum value but if u consider 100% max value is 288. This is an outlier.
# normalise with 99%
car_df$horsepower[which(car_df$horsepower > 207)] <- 207
summary(car_df$horsepower)

# compression ratio outliers: (outliers present)
boxplot(car_df$compressionratio, main="compression ratio")
# From boxplot we can see that there are outliers and we need to do outlier capping
quantile(x = car_df$compressionratio,seq(0,1,0.01))
summary(car_df$compressionratio)
# Abrupt deviation found from 91 percentile, normalise with 10.94 - 90th percentile item
car_df$compressionratio[which(car_df$compressionratio > 10.94)] <- 10.94
summary(car_df$compressionratio)

# stroke outliers:(outliers present)
boxplot(car_df$stroke, main="Stroke") 
quantile(car_df$stroke,seq(0,1,0.01))
#Note that there is a jump between 0% and 1%. 
#here we will cap all values below 2.6400 to 2.6400. 
car_df$stroke[which(car_df$stroke<2.6400)]<-2.6400
summary(car_df$stroke)

# boreratio:
boxplot(car_df$boreratio, main="Bore Ratio")
quantile(car_df$boreratio,seq(0,1,0.01))
summary(car_df$boreratio)
# Mean and Median are close so no outliers found.

# curbweight outliers: 
boxplot(car_df$curbweight, main="Car Weight")
quantile(car_df$curbweight,seq(0,1,0.01))
# at 0% there is an outlier and above 98% there is an outlier
summary(car_df$curbweight)
car_df$curbweight[which(car_df$curbweight > 3768.40)] <- 3768.40
car_df$curbweight[which(car_df$curbweight < 1819.72)] <- 1819.72
summary(car_df$curbweight)

# enginesize outliers : (outliers present)
boxplot(car_df$enginesize, main="Engine Size")
quantile(car_df$enginesize,seq(0,1,0.01))
summary(car_df$enginesize)
# There is a jump between 32% and 33%. But, we cannot have 67% outliers. 
#So, we check the next change and it is between 49% and 50% but we can not have 50% outliers
#So we check the next change and it is between 93% and 94%
#Here we will cap all values above 209.00(96% value) to 209.00

car_df$enginesize[which(car_df$enginesize>209.00)]<-209.00
summary(car_df$enginesize)

#########################################################################################################################
##### Data Preparation, dummy variable creation, Data cleaning, Outlier capping is done

View(car_df)
str(car_df)
dim(car_df) # 205 rows 71 columns

###################### Remove Car_ID column, Car_Model
### we can remove Car_ID as it is useful only for identification purpose but not an indicator of price.
# Removing car_Model as we are not using it in modelling (Only Company is used not Car_Model in model building)
car_df<- car_df[-c(1,2)]
View(car_df)
dim(car_df) # 205,69

###Now All variables are modified as required

######################################################################################################
######################## Deriving new metrics##########################################################
#######################################################################################################

# Let us derive some useful metrics from available numeric variables
###  Overall mpg (Fuel efficiency)
car_df$overall_mpg <- round(car_df$citympg+car_df$highwaympg,2)
summary(car_df$overall_mpg)

### overall mpg to horse power ratio ( More horsepower leads to better fuel efficiency)
car_df$overallmpg2horsepower <- round(car_df$overall_mpg/car_df$horsepower,2)
summary(car_df$overallmpg2horsepower)

### overall mpg to curbweight ratio (More curbweight leads to less fuel efficiency)
car_df$overallmpg2curbweight <- round(car_df$overall_mpg/car_df$curbweight,4)
summary(car_df$overallmpg2curbweight)
# These are few derived variables

#########################################################################################################
############################Model Building##############################################################
#######################################################################################################

### Finally required data is prepared and ready to start model building

### Let's split the data into train(70%) and test(30%)

set.seed(100) # to reproduce same data
trainindices= sample(1:nrow(car_df), 0.7*nrow(car_df))

train = car_df[trainindices,]
test = car_df[-trainindices,]

dim(train) # 143 rows and 72 columns
dim(test) # 62 rows and 72 columns

# View(train)

############### Model Building on train data#######
library('MASS')
library('car')

model_1 <-lm(price~.,data=train) # Taking all variables
summary(model_1)
# R-squared:  0.9804,	Adjusted R-squared:  0.9656 
# 10 columns have not defined because of singularities (10 NAs)

#################################################################################################################
#################################### stepAIC
# stepAIC makes multiple calls while checking which variables to keep
# The last call that step makes, contains only the variables it considers to be important in the model. 
# some insignifican variables have been removed. 
# Now store the last model equation of stepwise method into an object called model_2

step <- stepAIC(model_1, direction = "both")
step
# final output of stepAIC is taken as model_2

model_2 <-lm(price ~ fueltype + aspiration + enginelocation + 
               carlength + carwidth + carheight + curbweight + enginesize + 
               stroke + citympg + highwaympg + symboling0 + symboling1 + 
               symboling2 + Companybmw + Companybuick + Companychevrolet + 
               Companydodge + Companyjaguar + Companymazda + Companymercury + 
               Companymitsubishi + Companynissan + Companypeugeot + Companyplymouth + 
               Companyrenault + Companysaab + Companysubaru + Companytoyota + 
               Companyvolkswagen + carbodyhardtop + carbodyhatchback + carbodysedan + 
               carbodywagon + drivewheelrwd + enginetypeohc + cylindernumberfive + 
               cylindernumberfour + cylindernumbersix + overallmpg2curbweight, 
             data = train)
summary(model_2)
# R-squared:  0.9791,	Adjusted R-squared:  0.9709
#### We can observe that in model_2, there is slight drop in R-squared value but adjusted R-squared raised slightly

###################################################################################################################
###################################################### VIF function

# Next, we will apply the VIF function to check the multicollinearity of the independent variables and  
# remove the variables with VIF>2 in order of their insignificance. (Industry standard vif<2)
vif(model_2)
#write.csv(x = vif(model_2),file = "vif1.csv")

##################################################################################################################
#########################################Dropping high vif and insignificant variables one by one 

# Drop fueltype as it has high vif and low p-value
# vif =6.991647
# Estimate  Std. Error t value   Pr(>|t|) 
# -1384.168    978.328  -1.415    0.160164

model_3 <-lm(price ~ aspiration + enginelocation + 
               carlength + carwidth + carheight + curbweight + enginesize + 
               stroke + citympg + highwaympg + symboling0 + symboling1 + 
               symboling2 + Companybmw + Companybuick + Companychevrolet + 
               Companydodge + Companyjaguar + Companymazda + Companymercury + 
               Companymitsubishi + Companynissan + Companypeugeot + Companyplymouth + 
               Companyrenault + Companysaab + Companysubaru + Companytoyota + 
               Companyvolkswagen + carbodyhardtop + carbodyhatchback + carbodysedan + 
               carbodywagon + drivewheelrwd + enginetypeohc + cylindernumberfive + 
               cylindernumberfour + cylindernumbersix + overallmpg2curbweight, 
             data = train)
summary(model_3)
# R-squared:  0.9787,	Adjusted R-squared:  0.9706
# slight drop in R-square also in adjusted R-square which can be allowed
vif(model_3)
# write.csv(x = vif(model_3),file = "vif2.csv")

# Drop highwaympg as it has high vif and is insignificant
# vif = 39.120262
# Estimate  Std. Error t value     Pr(>|t|) 
#  -128.227    106.029  -1.209     0.229292 

model_4 <-lm(price ~ aspiration + enginelocation + 
               carlength + carwidth + carheight + curbweight + enginesize + 
               stroke + citympg  + symboling0 + symboling1 + 
               symboling2 + Companybmw + Companybuick + Companychevrolet + 
               Companydodge + Companyjaguar + Companymazda + Companymercury + 
               Companymitsubishi + Companynissan + Companypeugeot + Companyplymouth + 
               Companyrenault + Companysaab + Companysubaru + Companytoyota + 
               Companyvolkswagen + carbodyhardtop + carbodyhatchback + carbodysedan + 
               carbodywagon + drivewheelrwd + enginetypeohc + cylindernumberfive + 
               cylindernumberfour + cylindernumbersix + overallmpg2curbweight, 
             data = train)
summary(model_4)
# R-squared:  0.9784,	Adjusted R-squared:  0.9704
# slight drop in R-square and adjusted R-square which can be allowed
vif(model_4)
# write.csv(x = vif(model_4),file = "vif3.csv")

# Drop symboling0 as it has high vif and it is less significant
# vif = 3.810578
# Estimate  Std. Error t value    Pr(>|t|) 
# 556.359    503.422   1.105      0.271643 

model_5 <-lm(price ~ aspiration + enginelocation + 
               carlength + carwidth + carheight + curbweight + enginesize + 
               stroke + citympg  + symboling1 + 
               symboling2 + Companybmw + Companybuick + Companychevrolet + 
               Companydodge + Companyjaguar + Companymazda + Companymercury + 
               Companymitsubishi + Companynissan + Companypeugeot + Companyplymouth + 
               Companyrenault + Companysaab + Companysubaru + Companytoyota + 
               Companyvolkswagen + carbodyhardtop + carbodyhatchback + carbodysedan + 
               carbodywagon + drivewheelrwd + enginetypeohc + cylindernumberfive + 
               cylindernumberfour + cylindernumbersix + overallmpg2curbweight, 
             data = train)
summary(model_5)
# R-squared:  0.9781,	Adjusted R-squared:  0.9704
# slight drop in R-square and no change in adjusted R-square which can be allowed
vif(model_5)
# write.csv(x = vif(model_5),file = "vif4.csv")

# Drop symoling2 as it has high vif and is less significant
# vif =  2.343027
# Estimate  Std. Error t value    Pr(>|t|) 
# 540.903    466.647   1.159      0.249034 

model_6 <-lm(price ~ aspiration + enginelocation + 
               carlength + carwidth + carheight + curbweight + enginesize + 
               stroke + citympg  + symboling1 + 
               Companybmw + Companybuick + Companychevrolet + 
               Companydodge + Companyjaguar + Companymazda + Companymercury + 
               Companymitsubishi + Companynissan + Companypeugeot + Companyplymouth + 
               Companyrenault + Companysaab + Companysubaru + Companytoyota + 
               Companyvolkswagen + carbodyhardtop + carbodyhatchback + carbodysedan + 
               carbodywagon + drivewheelrwd + enginetypeohc + cylindernumberfive + 
               cylindernumberfour + cylindernumbersix + overallmpg2curbweight, 
             data = train)
summary(model_6)
# R-squared:  0.9778,	Adjusted R-squared:  0.9703
# slight drop in R-square and adjusted R-square which can be allowed
vif(model_6)
# write.csv(x = vif(model_6),file = "vif5.csv")

# Drop carheight
# vif =  4.440993
# Estimate  Std. Error t value    Pr(>|t|) 
#   150.937    102.204   1.477    0.142688

model_7 <-lm(price ~ aspiration + enginelocation + 
               carlength + carwidth +  curbweight + enginesize + 
               stroke + citympg  + symboling1 + 
               Companybmw + Companybuick + Companychevrolet + 
               Companydodge + Companyjaguar + Companymazda + Companymercury + 
               Companymitsubishi + Companynissan + Companypeugeot + Companyplymouth + 
               Companyrenault + Companysaab + Companysubaru + Companytoyota + 
               Companyvolkswagen + carbodyhardtop + carbodyhatchback + carbodysedan + 
               carbodywagon + drivewheelrwd + enginetypeohc + cylindernumberfive + 
               cylindernumberfour + cylindernumbersix + overallmpg2curbweight, 
             data = train)
summary(model_7)
# R-squared:  0.9774,	Adjusted R-squared:  0.97
# slight drop in R-square and adjusted R-square which can be allowed
vif(model_7)
# write.csv(x = vif(model_7),file = "vif6.csv")

# Drop carbodyhardtop as it has high value and is insignificant
# vif =   3.817167
# Estimate  Std. Error t value  Pr(>|t|) 
# -1311.895   1006.756  -1.303   0.195341

model_8 <-lm(price ~ aspiration + enginelocation + 
               carlength + carwidth +  curbweight + enginesize + 
               stroke + citympg  + symboling1 + 
               Companybmw + Companybuick + Companychevrolet + 
               Companydodge + Companyjaguar + Companymazda + Companymercury + 
               Companymitsubishi + Companynissan + Companypeugeot + Companyplymouth + 
               Companyrenault + Companysaab + Companysubaru + Companytoyota + 
               Companyvolkswagen+ carbodyhatchback + carbodysedan + 
               carbodywagon + drivewheelrwd + enginetypeohc + cylindernumberfive + 
               cylindernumberfour + cylindernumbersix + overallmpg2curbweight, 
             data = train)
summary(model_8)
# R-squared:  0.977,	Adjusted R-squared:  0.9698
# slight drop in R-square and adjusted R-square which can be allowed
vif(model_8)
# write.csv(x = vif(model_8),file = "vif7.csv")

# Drop carbodyhatchback as it has high vif and relatively less significant
# vif = 6.787661 
# Estimate  Std. Error t value    Pr(>|t|) 
# -1304.459    646.168  -2.019   0.045990

model_9 <-lm(price ~ aspiration + enginelocation + 
               carlength + carwidth +  curbweight + enginesize + 
               stroke + citympg  + symboling1 + 
               Companybmw + Companybuick + Companychevrolet + 
               Companydodge + Companyjaguar + Companymazda + Companymercury + 
               Companymitsubishi + Companynissan + Companypeugeot + Companyplymouth + 
               Companyrenault + Companysaab + Companysubaru + Companytoyota + 
               Companyvolkswagen+carbodysedan + 
               carbodywagon + drivewheelrwd + enginetypeohc + cylindernumberfive + 
               cylindernumberfour + cylindernumbersix + overallmpg2curbweight, 
             data = train)
summary(model_9)
# R-squared:  0.9761,	Adjusted R-squared:  0.9689
# slight drop in R-square and adjusted R-square which can be allowed
vif(model_9)
# write.csv(x = vif(model_9),file = "vif8.csv")

# Drop carbodysedan as it has high vif and is insignificant
# vif =  2.544455 
# Estimate  Std. Error t value    Pr(>|t|) 
#  -341.144    388.537  -0.878    0.381863

model_10 <-lm(price ~ aspiration + enginelocation + 
                carlength + carwidth +  curbweight + enginesize + 
                stroke + citympg  + symboling1 + 
                Companybmw + Companybuick + Companychevrolet + 
                Companydodge + Companyjaguar + Companymazda + Companymercury + 
                Companymitsubishi + Companynissan + Companypeugeot + Companyplymouth + 
                Companyrenault + Companysaab + Companysubaru + Companytoyota + 
                Companyvolkswagen+
                carbodywagon + drivewheelrwd + enginetypeohc + cylindernumberfive + 
                cylindernumberfour + cylindernumbersix + overallmpg2curbweight, 
              data = train)
summary(model_10)
# R-squared:  0.976,	Adjusted R-squared:  0.969
# slight drop in R-square and slight raise in  adjusted R-square which can be allowed
vif(model_10)
# write.csv(x = vif(model_10),file = "vif9.csv")


# Drop carlength as it has high vif and is relatively less significant
# vif =   8.405374
# Estimate  Std. Error t value  Pr(>|t|) 
# 73.502     28.276   2.599     0.010618

model_11 <-lm(price ~ aspiration + enginelocation + 
                carwidth +  curbweight + enginesize + 
                stroke + citympg  + symboling1 + 
                Companybmw + Companybuick + Companychevrolet + 
                Companydodge + Companyjaguar + Companymazda + Companymercury + 
                Companymitsubishi + Companynissan + Companypeugeot + Companyplymouth + 
                Companyrenault + Companysaab + Companysubaru + Companytoyota + 
                Companyvolkswagen+
                carbodywagon + drivewheelrwd + enginetypeohc + cylindernumberfive + 
                cylindernumberfour + cylindernumbersix + overallmpg2curbweight, 
              data = train)
summary(model_11)
# R-squared:  0.9745,	Adjusted R-squared:  0.9674
# Drop in R-square and adjusted R-square is slightly high which can be allowed
vif(model_11)
# write.csv(x = vif(model_11),file = "vif10.csv")


# Drop overallmpg2curbweight as it has high vif and is relatively less significant
# vif =  96.756833 
# Estimate  Std. Error t value    Pr(>|t|) 
# 262063.066 128026.435   2.047   0.043025

model_12 <-lm(price ~ aspiration + enginelocation + 
                carwidth +  curbweight + enginesize + 
                stroke + citympg  + symboling1 + 
                Companybmw + Companybuick + Companychevrolet + 
                Companydodge + Companyjaguar + Companymazda + Companymercury + 
                Companymitsubishi + Companynissan + Companypeugeot + Companyplymouth + 
                Companyrenault + Companysaab + Companysubaru + Companytoyota + 
                Companyvolkswagen+
                carbodywagon + drivewheelrwd + enginetypeohc + cylindernumberfive + 
                cylindernumberfour + cylindernumbersix, data = train)
summary(model_12)
# R-squared:  0.9735,	Adjusted R-squared:  0.9664
# slight drop in R-square and adjusted R-square which can be allowed
vif(model_12)
# write.csv(x = vif(model_12),file = "vif11.csv")

# Drop Citympg as it has high vif and is insignificant
# vif = 4.121583
# Estimate  Std. Error t value  Pr(>|t|) 
#  -59.183     38.296  -1.545   0.125063

model_13 <-lm(price ~ aspiration + enginelocation + 
                carwidth +  curbweight + enginesize + 
                stroke +symboling1 + 
                Companybmw + Companybuick + Companychevrolet + 
                Companydodge + Companyjaguar + Companymazda + Companymercury + 
                Companymitsubishi + Companynissan + Companypeugeot + Companyplymouth + 
                Companyrenault + Companysaab + Companysubaru + Companytoyota + 
                Companyvolkswagen+
                carbodywagon + drivewheelrwd + enginetypeohc + cylindernumberfive + 
                cylindernumberfour + cylindernumbersix, data = train)
summary(model_13)
# R-squared:  0.973,	Adjusted R-squared:  0.966
# slight drop in R-square and adjusted R-square which can be allowed
vif(model_13)
# write.csv(x = vif(model_13),file = "vif12.csv")

# Drop enginetypeohc as it has high vif and is relatively less significant
# vif= 7.631093
# Estimate  Std. Error t value  Pr(>|t|)
# -2038.600    781.140  -2.610  0.010287

model_14 <-lm(price ~ aspiration + enginelocation + 
                carwidth +  curbweight + enginesize + 
                stroke +symboling1 + 
                Companybmw + Companybuick + Companychevrolet + 
                Companydodge + Companyjaguar + Companymazda + Companymercury + 
                Companymitsubishi + Companynissan + Companypeugeot + Companyplymouth + 
                Companyrenault + Companysaab + Companysubaru + Companytoyota + 
                Companyvolkswagen+
                carbodywagon + drivewheelrwd +  cylindernumberfive + 
                cylindernumberfour + cylindernumbersix, data = train)
summary(model_14)
# R-squared:  0.9713,	Adjusted R-squared:  0.9643
# Drop in R-square and adjusted R-square is slightly high which can be allowed
vif(model_14)
# write.csv(x = vif(model_14),file = "vif13.csv")

# Drop aspiration as it has high vif and is relatively less significant
# vif= 2.137205
# Estimate  Std. Error t value   Pr(>|t|)
# 1536.853    482.272   3.187    0.001857

model_15 <-lm(price ~ enginelocation + 
                carwidth +  curbweight + enginesize + 
                stroke +symboling1 + 
                Companybmw + Companybuick + Companychevrolet + 
                Companydodge + Companyjaguar + Companymazda + Companymercury + 
                Companymitsubishi + Companynissan + Companypeugeot + Companyplymouth + 
                Companyrenault + Companysaab + Companysubaru + Companytoyota + 
                Companyvolkswagen+
                carbodywagon + drivewheelrwd +  cylindernumberfive + 
                cylindernumberfour + cylindernumbersix, data = train)
summary(model_15)
# R-squared:  0.9688,	Adjusted R-squared:  0.9614
# Drop in R-square and adjusted R-square is slightly high which can be allowed
vif(model_15)
# write.csv(x = vif(model_15),file = "vif14.csv")

# Drop drivewheelrwd as it has high vif and is relatively less significant
# vif= 3.612888 
# Estimate  Std. Error t value    Pr(>|t|)
#  -1488.924    527.936  -2.820   0.005653

model_16 <-lm(price ~ enginelocation + 
                carwidth +  curbweight + enginesize + 
                stroke +symboling1 + 
                Companybmw + Companybuick + Companychevrolet + 
                Companydodge + Companyjaguar + Companymazda + Companymercury + 
                Companymitsubishi + Companynissan + Companypeugeot + Companyplymouth + 
                Companyrenault + Companysaab + Companysubaru + Companytoyota + 
                Companyvolkswagen+
                carbodywagon +cylindernumberfive + 
                cylindernumberfour + cylindernumbersix, data = train)
summary(model_16)
# R-squared:  0.9666,	Adjusted R-squared:  0.9591
# Drop in R-square and adjusted R-square is slightly high which can be allowed
vif(model_16)
# write.csv(x = vif(model_16),file = "vif15.csv")

# Drop enginesize as it has high vif and is relatively less significant
# vif=12.395782 
# Estimate  Std. Error t value  Pr(>|t|)
# 37.678     14.087   2.675     0.008563

model_17 <-lm(price ~ enginelocation + 
                carwidth +  curbweight +
                stroke +symboling1 + 
                Companybmw + Companybuick + Companychevrolet + 
                Companydodge + Companyjaguar + Companymazda + Companymercury + 
                Companymitsubishi + Companynissan + Companypeugeot + Companyplymouth + 
                Companyrenault + Companysaab + Companysubaru + Companytoyota + 
                Companyvolkswagen+
                carbodywagon +cylindernumberfive + 
                cylindernumberfour + cylindernumbersix, data = train)
summary(model_17)
# R-squared:  0.9646,	Adjusted R-squared:  0.957
# Drop in R-square and adjusted R-square is slightly high which can be allowed
vif(model_17)
# write.csv(x = vif(model_17),file = "vif16.csv")

# Drop stroke as it has high vif and is relatively less significant
# vif= 2.901186
# Estimate  Std. Error t value    Pr(>|t|)
# -2008.4688    849.5994  -2.364  0.019725

model_18 <-lm(price ~ enginelocation + 
                carwidth +  curbweight +
                symboling1 + 
                Companybmw + Companybuick + Companychevrolet + 
                Companydodge + Companyjaguar + Companymazda + Companymercury + 
                Companymitsubishi + Companynissan + Companypeugeot + Companyplymouth + 
                Companyrenault + Companysaab + Companysubaru + Companytoyota + 
                Companyvolkswagen+
                carbodywagon +cylindernumberfive + 
                cylindernumberfour + cylindernumbersix, data = train)
summary(model_18)
# R-squared:  0.9629,	Adjusted R-squared:  0.9553
# Drop in R-square and adjusted R-square is slightly high which can be allowed
vif(model_18)
# write.csv(x = vif(model_18),file = "vif17.csv")

# Drop cylindernumbersix as it has high vif and is relatively less significant
# vif=5.137257 
#  Estimate  Std. Error t value   Pr(>|t|)
# -4589.3609   1038.4214  -4.420  0.00002209918736

model_19 <-lm(price ~ enginelocation + 
                carwidth +  curbweight +
                symboling1 + 
                Companybmw + Companybuick + Companychevrolet + 
                Companydodge + Companyjaguar + Companymazda + Companymercury + 
                Companymitsubishi + Companynissan + Companypeugeot + Companyplymouth + 
                Companyrenault + Companysaab + Companysubaru + Companytoyota + 
                Companyvolkswagen+
                carbodywagon +cylindernumberfive + 
                cylindernumberfour, data = train)
summary(model_19)
# R-squared:  0.9567,	Adjusted R-squared:  0.9484
# Drop in R-square and adjusted R-square is slightly high which can be allowed
vif(model_19)
# write.csv(x = vif(model_19),file = "vif18.csv")

# Drop cylindernumberfour as it has high vif and is relatively less significant
# vif=  3.329158
#  Estimate  Std. Error t value     Pr(>|t|)
#  -2552.9443    672.3770  -3.797     0.000232

model_20 <-lm(price ~ enginelocation + 
                carwidth +  curbweight +
                symboling1 + 
                Companybmw + Companybuick + Companychevrolet + 
                Companydodge + Companyjaguar + Companymazda + Companymercury + 
                Companymitsubishi + Companynissan + Companypeugeot + Companyplymouth + 
                Companyrenault + Companysaab + Companysubaru + Companytoyota + 
                Companyvolkswagen+
                carbodywagon +cylindernumberfive, data = train)
summary(model_20)
# R-squared:  0.9515,	Adjusted R-squared:  0.9426
# Drop in R-square and adjusted R-square is slightly high which can be allowed
vif(model_20)
# write.csv(x = vif(model_20),file = "vif19.csv")

# Drop carwidth as it has high vif and is relatively less significant
# vif=6.732334
# Estimate  Std. Error t value           Pr(>|t|)
#  1096.5901    195.0259   5.623    0.000000124585122

model_21 <-lm(price ~ enginelocation + 
                curbweight +
                symboling1 + 
                Companybmw + Companybuick + Companychevrolet + 
                Companydodge + Companyjaguar + Companymazda + Companymercury + 
                Companymitsubishi + Companynissan + Companypeugeot + Companyplymouth + 
                Companyrenault + Companysaab + Companysubaru + Companytoyota + 
                Companyvolkswagen+
                carbodywagon +cylindernumberfive, data = train)
summary(model_21)
# R-squared:  0.9387,	Adjusted R-squared:  0.9281
# Drop in R-square and adjusted R-square is high as all variables with high vif are significant 
vif(model_21)
# write.csv(x = vif(model_21),file = "vif20.csv")

# Drop companybuick as it has high vif and is relatively less significant
model_22 <-lm(price ~ enginelocation + 
                curbweight +
                symboling1 + 
                Companybmw +Companychevrolet + 
                Companydodge + Companyjaguar + Companymazda + Companymercury + 
                Companymitsubishi + Companynissan + Companypeugeot + Companyplymouth + 
                Companyrenault + Companysaab + Companysubaru + Companytoyota + 
                Companyvolkswagen+
                carbodywagon +cylindernumberfive, data = train)
summary(model_22)
# R-squared:  0.915,	Adjusted R-squared:  0.9011
# Drop in R-square and adjusted R-square is high as all variables with high vif are significant 
vif(model_22)
# write.csv(x = vif(model_22),file = "vif21.csv")

###########################################################################################################
# Finally we achieved all variables with vif < 2 which is our desired criteria 
# All variables have vif <2
########################## Remove variables with high p-value###############################################
####################### Now let's remove insignificant variables depending on p-value

# Drop Companychevrolet as it has high p-value

model_23 <-lm(price ~ enginelocation + 
                curbweight +
                symboling1 + 
                Companybmw +
                Companydodge + Companyjaguar + Companymazda + Companymercury + 
                Companymitsubishi + Companynissan + Companypeugeot + Companyplymouth + 
                Companyrenault + Companysaab + Companysubaru + Companytoyota + 
                Companyvolkswagen+
                carbodywagon +cylindernumberfive, data = train)

summary(model_23)
# R-squared:  0.915,	Adjusted R-squared:  0.9019

# Drop cylindernumberfive as it has high p-value

model_24 <-lm(price ~ enginelocation + 
                curbweight +
                symboling1 + 
                Companybmw +
                Companydodge + Companyjaguar + Companymazda + Companymercury + 
                Companymitsubishi + Companynissan + Companypeugeot + Companyplymouth + 
                Companyrenault + Companysaab + Companysubaru + Companytoyota + 
                Companyvolkswagen+
                carbodywagon, data = train)

summary(model_24)
# R-squared:  0.9144,	Adjusted R-squared:  0.902

# Drop Companysaab as it has high p-value 

model_25 <-lm(price ~ enginelocation + 
                curbweight +
                symboling1 + 
                Companybmw +
                Companydodge + Companyjaguar + Companymazda + Companymercury + 
                Companymitsubishi + Companynissan + Companypeugeot + Companyplymouth + 
                Companyrenault +Companysubaru + Companytoyota + 
                Companyvolkswagen+
                carbodywagon, data = train)
summary(model_25)
# R-squared:  0.9133,	Adjusted R-squared:  0.9015

# Drop Companysubaru as it has high p-value

model_26 <-lm(price ~ enginelocation + 
                curbweight +
                symboling1 + 
                Companybmw +
                Companydodge + Companyjaguar + Companymazda + Companymercury + 
                Companymitsubishi + Companynissan + Companypeugeot + Companyplymouth + 
                Companyrenault +Companytoyota + 
                Companyvolkswagen+
                carbodywagon, data = train)
summary(model_26)
# R-squared:  0.912,	Adjusted R-squared:  0.9008

# Drop Companyvolkswagen as it has high p-value

model_27 <-lm(price ~ enginelocation + 
                curbweight +
                symboling1 + 
                Companybmw +
                Companydodge + Companyjaguar + Companymazda + Companymercury + 
                Companymitsubishi + Companynissan + Companypeugeot + Companyplymouth + 
                Companyrenault +Companytoyota + 
                carbodywagon, data = train)
summary(model_27)
# R-squared:  0.9106,	Adjusted R-squared:  0.9001

# Drop Companyrenault as it has high p-value

model_28 <-lm(price ~ enginelocation + 
                curbweight +
                symboling1 + 
                Companybmw +
                Companydodge + Companyjaguar + Companymazda + Companymercury + 
                Companymitsubishi + Companynissan + Companypeugeot + Companyplymouth + 
                Companytoyota + 
                carbodywagon, data = train)
summary(model_28)
## R-squared:  0.9093,	Adjusted R-squared:  0.8994

# Drop Companymazda as it has high p-value 

model_29 <-lm(price ~ enginelocation + 
                curbweight +
                symboling1 + 
                Companybmw +
                Companydodge + Companyjaguar +Companymercury + 
                Companymitsubishi + Companynissan + Companypeugeot + Companyplymouth + 
                Companytoyota + 
                carbodywagon, data = train)
summary(model_29)
# R-squared:  0.9081,	Adjusted R-squared:  0.8989

# Drop Companynissan as it has high p-value

model_30 <-lm(price ~ enginelocation + 
                curbweight +
                symboling1 + 
                Companybmw +
                Companydodge + Companyjaguar +Companymercury + 
                Companymitsubishi + Companypeugeot + Companyplymouth + 
                Companytoyota + 
                carbodywagon, data = train)
summary(model_30)
# R-squared:  0.9071,	Adjusted R-squared:  0.8986

# Drop Companydodge as it has high p-value

model_31 <- lm(price ~ enginelocation + 
                 curbweight +
                 symboling1 + 
                 Companybmw +
                 Companyjaguar +Companymercury + 
                 Companymitsubishi + Companypeugeot + Companyplymouth + 
                 Companytoyota + 
                 carbodywagon, data = train)
summary(model_31)
# R-squared:  0.9053,	Adjusted R-squared: 0.8974

# Drop Companymercury as it has high p-value

model_32 <-lm(price ~ enginelocation + 
                curbweight +
                symboling1 + 
                Companybmw +
                Companyjaguar + 
                Companymitsubishi + Companypeugeot + Companyplymouth + 
                Companytoyota + 
                carbodywagon, data = train)
summary(model_32)
#  R-squared:  0.9035,	Adjusted R-squared:  0.8962

# Drop Companyplymouth as it has high p-value

model_33 <-lm(price ~ enginelocation + 
                curbweight +
                symboling1 + 
                Companybmw +
                Companyjaguar + 
                Companymitsubishi + Companypeugeot+ 
                Companytoyota + 
                carbodywagon, data = train)
summary(model_33)
#  R-squared:  0.9018,	Adjusted R-squared:  0.8951

# Drop Companyjaguar as it has high p-value

model_34 <-lm(price ~ enginelocation + 
                curbweight +
                symboling1 + 
                Companybmw +
                Companymitsubishi + Companypeugeot+ 
                Companytoyota + 
                carbodywagon, data = train)
summary(model_34)
#  R-squared:  0.8998,	Adjusted R-squared:  0.8938

# Drop Companymitsubishi as it has high p-value

model_35 <-lm(price ~ enginelocation + 
                curbweight +
                symboling1 + 
                Companybmw +
                 Companypeugeot+ 
                Companytoyota + 
                carbodywagon, data = train)
summary(model_35)
#  R-squared:  0.8948,	Adjusted R-squared:  0.8893

########################################################################################################################
################################### Model Evaluation by predicting prices of test data #################################
########################################################################################################################

# Let us use the model built using train data to predict the price in test data and see which has 
# good rsquared value also the differnce between modelled r squared and predicted rsquare should be minimum
Predict_1 <- predict(model_35,test[,-18])
test$test_price <- Predict_1
r <- cor(test$price,test$test_price)
rsquared <- cor(test$price,test$test_price)^2
rsquared # 0.8289643
# Difference=0.8948-0.8289643=6.58%
# Let's try to decrease the difference by removing insignificant variables

#########################################################################################################################
###### Continue to reduce the number of variables by removing less significant variables and predict using that model

# Drop symboling1 as it has high p-value

model_36 <-lm(price ~ enginelocation + 
                curbweight +
                Companybmw +
                Companypeugeot+ 
                Companytoyota + 
                carbodywagon, data = train)
summary(model_36)
#  R-squared:  0.8868,	Adjusted R-squared:  0.8818

# Model Prediction
Predict_2 <- predict(model_36,test[,-18])
test$test_price <- Predict_2
r <- cor(test$price,test$test_price)
rsquared <- cor(test$price,test$test_price)^2
rsquared # 0.8245543
# Difference = 0.8868-0.8245543=6.224%

# Let's try to decrease the difference by removing variables with high p-value


# Drop Companytoyota as it has high p-value 

model_37 <-lm(price ~ enginelocation + 
                curbweight +
                Companybmw +
                Companypeugeot+ 
                carbodywagon, data = train)
summary(model_37)
#  R-squared:  0.8752,	Adjusted R-squared:  0.8707

Predict_3 <- predict(model_37,test[,-18])
test$test_price <- Predict_3
r <- cor(test$price,test$test_price)
rsquared <- cor(test$price,test$test_price)^2
rsquared # 0.8282771

###################### Difference=0.8752-0.8282771=4.692% (Best model)

# This is the best model with good modelled R-squared value and also good predicted R-squared value with less difference

#########################################################################################################################
################################################## Final Model###########################################################
########################################################################################################################

# Among all the built models, model_37 has good modelled r squared value also good predicted r squared value
# That results in less differnce
# The differnce obtained is 4.69% which is good
# Difference of 5% is allowed
# So our model is good to go
final_model <- model_37
summary(final_model)
# R-squared:  0.8752,	Adjusted R-squared:  0.8707---modelled r squared value and adjusted r squared value

# Final prediction using final model
Predict <- predict(final_model,test[,-18])
test$test_price <- Predict # predicted price
r <- cor(test$price,test$test_price)
rsquared <- cor(test$price,test$test_price)^2
rsquared # 0.8282771--- Predicted r squared value #################(Best model)############################
##################### Difference=0.8752-0.8282771=4.69229% (approx 4.69%)

######################################################################################################################
################################ Plots showing how well the model is fit#############################################
#####################################################################################################################
par(mfrow=c(2,2))
plot(final_model)

# Observations from graphs
# 1. Residuals vs Fitted :-We could find equally spread residuals around a linear line without distinct patterns, 
# so it is a good indication that it does not have non-linear relationships
# 2. Normal Q-Q :- It's good residuals are lined well on the straight dashed line.
# 3. Scale-Location :- It's good we can see a horizontal line with equally (randomly) spread points.
# 4. Residuals vs Leverage :- The plot identified the influential observation on few rows
# where price could have outliers

############################################ Errors in model
test$error <- test$price - test$test_price
test$s_no <- c(1:nrow(test))
# View(test)

##################################### Plots for Randomness (Random error pattern)

library(ggplot2)
ggplot(test,aes(x = s_no,y = error)) + 
  geom_point() + geom_line() + 
  geom_abline(intercept = 0,slope = 1) + 
  xlab("No of Cars") + ylab("Error between predicted and actual price")

## From plot we can see that error in the predicted price and price is random and it is evenly distributed.

###################  Predicted price vs actual price plot

ggplot(test,aes(x=test_price,y=price))+geom_point()+geom_smooth()
# Linear graph

ggplot(test,aes(x = s_no,y = test_price)) + 
  geom_line(aes(color = factor("red",labels = c("predicted_price")))) +
  geom_line(aes(x = s_no, y = price, colour = factor("blue",labels = c("price")))) + 
  labs(title = "Predicted_Price Vs ActualPrice", x = "CarIDs", y = "Prices")  +
  theme(legend.title = element_blank())
# Red---Predicted price
# Blue is actual price

# From the graph, it can be inferred that the model is predicting the Values correctly and 
# slight deviation is because of random error and outliers in price column
# View(test[,c('price','test_price')])

####################################################################################################################
############################################# Question and Answers###################################################
#####################################################################################################################
# Questions#########
# Which variables are significant in predicting the price of a car
# How well those variables describe the price of a car
### Answers#########################
# The significant variables in predicting the price of car are
# enginelocation (Positive correlation)
# curbweight (Positive correlation)
# Companypeugeot (Negative correlation)
# carbodywagon (Negative correlation)
## Companybmw (Positive correlation)
# All the above 5 variables are highly significant in predicting the price of a car
# With 1 unit increase in either of enginelocation or curbweight or Companybmw, the price value increases corresponding to
# beta coefficients of the variables
# Actually it makes sense, increase in curbweight means more metal is used which results in increase in price of car

# Similarly with 1 unit increase in either of Companypeugeot or carbodywagon, the price value decreases corresponding to
# beta coefficients of the variables

# Coefficients:
#                   Estimate Std. Error  t value  Pr(>|t|)    
#  (Intercept)    -2.225e+04  1.302e+03 -17.090  < 2e-16 ***
#  enginelocation  1.801e+04  1.725e+03  10.441  < 2e-16 ***
#  curbweight      1.399e+01  5.152e-01  27.162  < 2e-16 ***
#  Companybmw      6.806e+03  1.731e+03   3.931 0.000134 ***
#  Companypeugeot -6.680e+03  1.308e+03  -5.107 1.08e-06 ***
#  carbodywagon   -3.031e+03  7.500e+02  -4.041 8.81e-05 ***
#  ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 2938 on 137 degrees of freedom
#Multiple R-squared:  0.8752,	Adjusted R-squared:  0.8707 
#F-statistic: 192.2 on 5 and 137 DF,  p-value: < 2.2e-16

### The differnce between modelled R-squared and predicted R-squared is"4.69%" where as allowed is 5% 
# Model is accurate and has difference between Modelled R-squared and Predicted R-squared value with in the limits
# So final model is accurate and can be deployed for prediction
