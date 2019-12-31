library(dplyr)
library(lubridate)
library(ggplot2)

## HR Analytics Case study
employee_survey_data<-read.csv('employee_survey_data.csv',stringsAsFactors = F)
general_data <- read.csv('general_data.csv',stringsAsFactors = F)
manager_survey_data <- read.csv('manager_survey_data.csv',stringsAsFactors = F)
in_time <- read.csv('in_time.csv',stringsAsFactors = F)
out_time <- read.csv('out_time.csv',stringsAsFactors = F)

#############################################################################################################
##############################Dealing with in_time and out_time dataframes###################################
#############################################################################################################
# we can observe that 1st column in in_time and out_time doesn't have a column name
# So let's set the column name as EmployeeID so that it will be easier to apply merge operation later
colnames(in_time)[1]<-"EmployeeID"
colnames(out_time)[1]<-"EmployeeID"
str(in_time)
str(out_time)
# 1-1-2015,14-1-2015,26-1-2015,5-3-2015,1-5-2015,17-7-2015,17-9-2015,2-10-2015,09-11-2015,10-11-2015,
#11-11-2015,25-12-2015
# All these dates have both in-time and out-time filled with NAs for all employees.
# So these days are official holidays given by the company to its employees
# So let's remove these dates records from in-time and out-time

all_na <- function(x) any(!is.na(x)) #checks if all values are NA or not

in_time <- in_time%>% select_if(all_na)
#View(in_time)
out_time <-out_time%>%select_if(all_na)
#View(out_time)

# 12 days records filled with complete NAs are removed(Holidays)
str(in_time)
str(out_time)

# we need to convert character type into Datetime format (POSIXct)
in_time[,2:250] <- lapply(in_time[,2:250],function(x) as_datetime(x))
out_time[,2:250] <- lapply(out_time[,2:250],function(x) as_datetime(x))
str(in_time)
str(out_time)

###########################################################################################################
################################## Working hours and average working hours#################################
###########################################################################################################

# Let's calculate the number of working hours for each employee and store it in a dataframe x
x<-out_time[,2:250]-in_time[,2:250]
x<-lapply(x,as.numeric)
str(x)
x <- as.data.frame(x)
y <-x # contains number of working hours for each employee
x$EmployeeID <-in_time$EmployeeID

#Let's Calculate the avg work hours of each employee
x$work_avg <- rowMeans(x[,1:249], na.rm = TRUE)
#View(x)
max(x$work_avg) #11.03096
which.max(x$work_avg) # EmployeeID 651
#let's round work_avg to 2 decimals
x$work_avg<-round(x$work_avg,2)

############################################################################################################
############################################## Merging dataframes###########################################
############################################################################################################

length(unique(employee_survey_data$EmployeeID))  # 4410, Employee ID is the primary key
length(unique(general_data$EmployeeID))          # 4410, Employee ID is the primary key
length(unique(manager_survey_data$EmployeeID))   # 4410, Employee ID is the primary key
length(unique(x$EmployeeID))                     # 4410, Employee ID is the primary key

hr_analytics <- merge(employee_survey_data,general_data, by="EmployeeID", all = F)
hr_analytics <- merge(hr_analytics, manager_survey_data, by="EmployeeID", all = F)
hr_analytics <- merge(hr_analytics,x[,c("EmployeeID","work_avg")], by= "EmployeeID", all = F)
#View(hr_analytics)
str(hr_analytics)
summary(hr_analytics$work_avg)


############################################################################################################
################################### Deriving new metrics ###################################################
############################################################################################################

# Over time (Employees working more than 8 hrs on an average)
hr_analytics$Over_time <- ifelse(hr_analytics$work_avg > 8, 1, 0)
# Overtime---1 indicates yes while 0 = no

# inadequate time (working less than 8 hours on an average)
hr_analytics$inadequate_time <- ifelse(hr_analytics$work_avg < 8, 1, 0)
# inadequate---1 indicates yes while 0 = no

# Number of leaves taken
for(i in 1:4410)
{
hr_analytics$number_of_leaves_taken[i]<-sum(is.na(y[i,]))
}
# Here y is number of working hours obtained by taking difference of out_time and in_time
summary(hr_analytics$number_of_leaves_taken)
#View(hr_analytics)
#View(hr_analytics$number_of_leaves_taken)
str(hr_analytics)

#############################################################################################################
################################### Missing values imputation################################################
#############################################################################################################

# EnvironmentSatisfaction,JobSatisfaction,WorkLifeBalance are having only levels like 1,2,3,4
# So they better give more information as categorical variables rather than as continuous variables
# Even mean,median of these above variables doesn't give proper sense so converting above variables to factors.
hr_analytics$EnvironmentSatisfaction<-as.factor(hr_analytics$EnvironmentSatisfaction)
hr_analytics$JobSatisfaction<-as.factor(hr_analytics$JobSatisfaction)
hr_analytics$WorkLifeBalance<-as.factor(hr_analytics$WorkLifeBalance)

sapply(hr_analytics,function(x) sum(is.na(x))) # to get all the missing values count present columnwise

sum(is.na(hr_analytics$EnvironmentSatisfaction))# 25 NAs
summary(hr_analytics$EnvironmentSatisfaction) # Levels:1,2,3,4
# Replacing missing values with mode (Here mode is 3)
hr_analytics$EnvironmentSatisfaction[which(is.na(hr_analytics$EnvironmentSatisfaction))]<-"3"
summary(hr_analytics$EnvironmentSatisfaction) # Levels:1,2,3,4

sum(is.na(hr_analytics$JobSatisfaction)) # 20 NAs
summary(hr_analytics$JobSatisfaction)
# Replacing missing values with mode (Here mode is 4)
hr_analytics$JobSatisfaction[which(is.na(hr_analytics$JobSatisfaction))]<-"4"
summary(hr_analytics$JobSatisfaction)

sum(is.na(hr_analytics$WorkLifeBalance))# 38 NAs
summary(hr_analytics$WorkLifeBalance)
# Replacing missing values with mode (Here mode is 3)
hr_analytics$WorkLifeBalance[which(is.na(hr_analytics$WorkLifeBalance))]<-"3"
summary(hr_analytics$WorkLifeBalance)

sum(is.na(hr_analytics$NumCompaniesWorked)) # 19 NAs
summary(hr_analytics$NumCompaniesWorked)
boxplot(hr_analytics$NumCompaniesWorked,main="Number of companies worked") 
# Replacing missing values with median
hr_analytics$NumCompaniesWorked[which(is.na(hr_analytics$NumCompaniesWorked))] <- median(hr_analytics$NumCompaniesWorked,na.rm = TRUE)

sum(is.na(hr_analytics$TotalWorkingYears)) # 9
boxplot(hr_analytics$TotalWorkingYears,main="Total working years") 
# Replacing missing values with median
hr_analytics$TotalWorkingYears[which(is.na(hr_analytics$TotalWorkingYears))]<-median(hr_analytics$TotalWorkingYears,na.rm = TRUE)
summary(hr_analytics$TotalWorkingYears)

#View(hr_analytics)
str(hr_analytics)


################################# EDA - Categorical variable distribution #################

## Visualizing attrition rate in the data
ggplot(hr_analytics, aes(x=factor(Attrition), fill = factor(Attrition))) + 
  geom_bar(alpha=0.8) + geom_text(stat='count', aes(label=scales::percent((..count..)/sum(..count..))), vjust=-1) + ggtitle("Plot 1: Attrition") + 
  labs(x="Attrition", y="Employee Count", fill="Attrition")


## Visualizing employees who have left the company against each category. 
### Business travel wise attrition
ggplot(subset(hr_analytics, Attrition == "Yes"), aes(x=factor(BusinessTravel), fill = factor(BusinessTravel))) + 
  geom_bar(alpha=0.8) + ggtitle("Plot 2: Business Travel") + labs(x="Business Travel", y="Employee Count")
summary(as.factor(hr_analytics$BusinessTravel))

### Department wise attrition
ggplot(subset(hr_analytics, Attrition == "Yes"), aes(x=factor(Department), fill = factor(Department))) + 
  geom_bar(alpha=0.8) + ggtitle("Plot 3: Department") + labs(x="Department", y="Employee Count")
summary(as.factor(hr_analytics$Department))

### Gender wise attrition
ggplot(subset(hr_analytics, Attrition == "Yes"), aes(x=factor(Gender), fill = factor(Gender))) + 
  geom_bar(alpha=0.8) + ggtitle("Plot 4: Gender") + labs(x="Gender", y="Employee Count")
summary(as.factor(hr_analytics$Gender))


### Education Field wise attrition
ggplot(subset(hr_analytics, Attrition == "Yes"), aes(x=factor(EducationField), fill = factor(EducationField))) + 
  geom_bar(alpha=0.8) + ggtitle("Plot 5: Education Field") + labs(x="Education Field", y="Employee Count")
summary(as.factor(hr_analytics$EducationField))

### Job Role wise attrition
ggplot(subset(hr_analytics, Attrition == "Yes"), aes(x=factor(JobRole), fill = factor(JobRole))) + 
  geom_bar(alpha=0.8) + ggtitle("Plot 6: Job Role") + labs(x="Job Role", y="Employee Count")
summary(as.factor(hr_analytics$JobRole))

### Marital Status wise attrition
ggplot(subset(hr_analytics, Attrition == "Yes"), aes(x=factor(MaritalStatus), fill = factor(MaritalStatus))) + 
  geom_bar(alpha=0.8) + ggtitle("Plot 7: Marital Status") + labs(x="Marital Status", y="Employee Count")
summary(as.factor(hr_analytics$MaritalStatus))

### Stock Option Level wise attrition
ggplot(subset(hr_analytics, Attrition == "Yes"), aes(x=factor(StockOptionLevel), fill = factor(StockOptionLevel))) + 
  geom_bar(alpha=0.8) + ggtitle("Plot 8: Stock Option Level") + labs(x="Stock Option Level", y="Employee Count")
summary(as.factor(hr_analytics$StockOptionLevel))

### Performance Rating wise attrition
ggplot(subset(hr_analytics, Attrition == "Yes"), aes(x=factor(PerformanceRating), fill = factor(PerformanceRating))) + 
  geom_bar(alpha=0.8) + ggtitle("Plot 9: Performance Rating") + labs(x="Performance Rating", y="Employee Count")
summary(as.factor(hr_analytics$PerformanceRating))

### Over time wise attrition
ggplot(subset(hr_analytics, Attrition == "Yes"), aes(x=factor(Over_time), fill = factor(Over_time))) + 
  geom_bar(alpha=0.8) + ggtitle("Plot 10: Over Time") + labs(x="Over Time", y="Employee Count")
summary(as.factor(hr_analytics$Over_time))

### Inadequate time wise attrition
ggplot(subset(hr_analytics, Attrition == "Yes"), aes(x=factor(inadequate_time), fill = factor(inadequate_time))) + 
  geom_bar(alpha=0.8) + ggtitle("Plot 11: Inadequate Time") + labs(x="Inadequate Time", y="Employee Count")
summary(as.factor(hr_analytics$inadequate_time))

### Environment Satisfaction wise attrition
ggplot(subset(hr_analytics, Attrition == "Yes"), aes(x=factor(EnvironmentSatisfaction), fill = factor(EnvironmentSatisfaction))) + 
  geom_bar(alpha=0.8) + ggtitle("Plot 12: Environment Satisfaction") + labs(x="Environment Satisfaction", y="Employee Count")
summary(as.factor(hr_analytics$EnvironmentSatisfaction))

### Job Involvement wise attrition
ggplot(subset(hr_analytics, Attrition == "Yes"), aes(x=factor(JobInvolvement), fill = factor(JobInvolvement))) + 
  geom_bar(alpha=0.8) + ggtitle("Plot 13: Job Involvement") + labs(x="Job Involvement", y="Employee Count")
summary(as.factor(hr_analytics$JobInvolvement))

### Job Satisfaction wise attrition
ggplot(subset(hr_analytics, Attrition == "Yes"), aes(x=factor(JobSatisfaction), fill = factor(JobSatisfaction))) + 
  geom_bar(alpha=0.8) + ggtitle("Plot 14: Job Satisfaction") + labs(x="Job Satisfaction", y="Employee Count")
summary(as.factor(hr_analytics$JobSatisfaction))

### Work Life Balance wise attrition
ggplot(subset(hr_analytics, Attrition == "Yes"), aes(x=factor(WorkLifeBalance), fill = factor(WorkLifeBalance))) + 
  geom_bar(alpha=0.8) + ggtitle("Plot 15: Work Life Balance") + labs(x="Work Life Balance", y="Employee Count")
summary(as.factor(hr_analytics$WorkLifeBalance))

### JobLevel wise attrition
ggplot(subset(hr_analytics, Attrition == "Yes"), aes(x=factor(JobLevel), fill = factor(JobLevel))) + 
  geom_bar(alpha=0.8) + ggtitle("Plot 16: Job Level") + labs(x="Job Level", y="Employee Count")
summary(as.factor(hr_analytics$JobLevel))


###########################################################################################################
######################## Creating Dummy Variables##########################################################
###########################################################################################################

# Convert categorical variables with 2 levels into numeric
str(hr_analytics$Attrition)
hr_analytics$Attrition <- ifelse(hr_analytics$Attrition == "Yes", 1,0)
str(hr_analytics$Gender)
hr_analytics$Gender <- ifelse(hr_analytics$Gender == "Female",1,0)
str(hr_analytics$Over18)
hr_analytics$Over18 <- ifelse(hr_analytics$Over18 == "Y", 1,0)

# For categorical variables with more than 2 levels
hr_analytics_fact <- hr_analytics[,c("EnvironmentSatisfaction","JobSatisfaction","WorkLifeBalance",
                                     "BusinessTravel","Department","EducationField", "Education",
                                     "JobRole","MaritalStatus","JobInvolvement","JobLevel",
                                     "PerformanceRating")]
#Convert categorical attributes to factors
hr_analytics_fact <- data.frame(sapply(hr_analytics_fact,factor))
str(hr_analytics_fact)

#Creating dummy variables
dummies <- data.frame(sapply(hr_analytics_fact, function(x)
  data.frame(model.matrix(~x, data = hr_analytics_fact))[,-1]))
#View(dummies)
#View(hr_analytics)
# Replacing categorical variables with dummies
hr_analytics<-cbind(hr_analytics[,-c(2,3,4,7,8,10,11,14,15,16,28,29)],dummies)
#View(hr_analytics)
dim(hr_analytics) #4410 61
str(hr_analytics)
View(hr_analytics)

hist(hr_analytics$Age)
hist(hr_analytics$DistanceFromHome)
hist(hr_analytics$MonthlyIncome)
hist(hr_analytics$NumCompaniesWorked)
hist(hr_analytics$PercentSalaryHike)
hist(hr_analytics$TrainingTimesLastYear)
hist(hr_analytics$YearsAtCompany)
hist(hr_analytics$YearsSinceLastPromotion)

boxplot(hr_analytics$Age,main='Age') # No outliers
boxplot(hr_analytics$DistanceFromHome,main='Distance from home') #No Outliers
boxplot(hr_analytics$MonthlyIncome,main='Monthly Income')
quantile(hr_analytics$MonthlyIncome,seq(0,1,0.01))
# There are few outliers but we can't neglect those outliers
boxplot(hr_analytics$NumCompaniesWorked,main="Number of companies worked")
# Outlier is present but we can't neglect those outliers
boxplot(hr_analytics$PercentSalaryHike,main="Percentage Salary Hike")# No Outliers
boxplot(hr_analytics$TrainingTimesLastYear,main='Training Times Last year')
# Few lower and upper outliers are present but we can't neglect those outliers
boxplot(hr_analytics$YearsAtCompany,main="Years at company")
# Outliers are high but we can't neglect them
boxplot(hr_analytics$YearsSinceLastPromotion,main="Years since last promotion")
# Outliers are high but we can't neglect them

# All the outliers gives few important details related to company and employees. so we can't remove them

##########################################################################################################
##################################### Scaling ############################################################
##########################################################################################################
# We need to scale the below  variables

hr_analytics_scaled <-hr_analytics
hr_analytics_scaled$Age<-scale(hr_analytics_scaled$Age,center = T,scale = T)
hr_analytics_scaled$DistanceFromHome<-scale(hr_analytics_scaled$DistanceFromHome,center = T,scale = T)
hr_analytics_scaled$MonthlyIncome<-scale(hr_analytics_scaled$MonthlyIncome,center = T,scale = T)
hr_analytics_scaled$NumCompaniesWorked<-scale(hr_analytics_scaled$NumCompaniesWorked,center = T,scale = T)
hr_analytics_scaled$PercentSalaryHike<-scale(hr_analytics_scaled$PercentSalaryHike,center = T,scale = T)
hr_analytics_scaled$StockOptionLevel<-scale(hr_analytics_scaled$StockOptionLevel,center = T,scale = T)
hr_analytics_scaled$TotalWorkingYears<-scale(hr_analytics_scaled$TotalWorkingYears,center = T,scale = T)
hr_analytics_scaled$TrainingTimesLastYear<-scale(hr_analytics_scaled$TrainingTimesLastYear,center =T,scale =T)
hr_analytics_scaled$YearsAtCompany<-scale(hr_analytics_scaled$YearsAtCompany,center = T,scale = T)
hr_analytics_scaled$YearsSinceLastPromotion<-scale(hr_analytics_scaled$YearsSinceLastPromotion,center=T,scale=T)
hr_analytics_scaled$YearsWithCurrManager<-scale(hr_analytics_scaled$YearsWithCurrManager,center = T,scale = T)
hr_analytics_scaled$work_avg<-scale(hr_analytics_scaled$work_avg,center = T,scale = T)
hr_analytics_scaled$number_of_leaves_taken<-scale(hr_analytics_scaled$number_of_leaves_taken,center=T,scale=T)
str(hr_analytics_scaled$Attrition)
# Let's drop EmployeeID as it is only given for the purpose of identification but not actually an indicator.
hr_analytics_scaled$EmployeeID<-NULL
View(hr_analytics_scaled)
############################################################################################################
###################################### MODEL BUILDING ######################################################
############################################################################################################

set.seed(100) # to reproduce same data
trainindices= sample(1:nrow(hr_analytics_scaled), 0.7*nrow(hr_analytics_scaled))

train = hr_analytics_scaled[trainindices,]
test = hr_analytics_scaled[-trainindices,]

dim(train) #3087 60
dim(test)  #1323 60
#View(train)

# Let's start modelling
model_1<-glm(Attrition~.,data=train,family = 'binomial')
summary(model_1)
library('MASS')
library('car')
step<-stepAIC(model_1,direction = "both")
step
model_2 <-glm(Attrition ~ Age + MonthlyIncome + NumCompaniesWorked + 
                StockOptionLevel + TotalWorkingYears + TrainingTimesLastYear + 
                YearsAtCompany + YearsSinceLastPromotion + YearsWithCurrManager + 
                Over_time + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely + EducationField.xLife.Sciences + 
                EducationField.xMarketing + EducationField.xMedical + EducationField.xOther + 
                EducationField.xTechnical.Degree + Education.x5 + JobRole.xHuman.Resources + 
                JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                JobInvolvement.x3 + JobLevel.x5, family = "binomial", data = train)
summary(model_2)
vif(model_2)
sort(vif(model_2))
#write.csv(vif(model_2),'vif1.csv')

# Drop YearsAtCompany as it has high vif and is less significant
# vif= 4.740086
#  Estimate Std. Error z value Pr(>|z|)
#  0.36408    0.13692   2.659 0.007835
model_3 <-glm(Attrition ~ Age + MonthlyIncome + NumCompaniesWorked + 
                StockOptionLevel + TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                Over_time + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely + EducationField.xLife.Sciences + 
                EducationField.xMarketing + EducationField.xMedical + EducationField.xOther + 
                EducationField.xTechnical.Degree + Education.x5 + JobRole.xHuman.Resources + 
                JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                JobInvolvement.x3 + JobLevel.x5, family = "binomial", data = train)
summary(model_3)
vif(model_3)
sort(vif(model_3))
#write.csv(vif(model_3),'vif2.csv')

# Drop MaritalStatus.xMarried as it has high vif and insignificant
# vif=2.160487961
#  Estimate Std. Error z value Pr(>|z|)
#  0.33057    0.17017   1.943 0.052072
model_4 <-glm(Attrition ~ Age + MonthlyIncome + NumCompaniesWorked + 
                StockOptionLevel + TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                Over_time + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely + EducationField.xLife.Sciences + 
                EducationField.xMarketing + EducationField.xMedical + EducationField.xOther + 
                EducationField.xTechnical.Degree + Education.x5 + JobRole.xHuman.Resources + 
                JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                JobRole.xSales.Executive +MaritalStatus.xSingle + 
                JobInvolvement.x3 + JobLevel.x5, family = "binomial", data = train)
summary(model_4)
vif(model_4)
sort(vif(model_4))
#write.csv(vif(model_4),'vif3.csv')

# Drop BusinessTravel.xTravel_Rarely as VIF is very high
# vif= 4.750318
#  Estimate Std. Error z value Pr(>|z|)
#  0.95651    0.27228   3.513 0.000443
model_5 <-glm(Attrition ~ Age + MonthlyIncome + NumCompaniesWorked + 
                StockOptionLevel + TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                Over_time + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                EducationField.xLife.Sciences + 
                EducationField.xMarketing + EducationField.xMedical + EducationField.xOther + 
                EducationField.xTechnical.Degree + Education.x5 + JobRole.xHuman.Resources + 
                JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                JobRole.xSales.Executive +MaritalStatus.xSingle + 
                JobInvolvement.x3 + JobLevel.x5, family = "binomial", data = train)
summary(model_5)
vif(model_5)
sort(vif(model_5))
#write.csv(vif(model_5),'vif4.csv')

# Drop EducationField.xOther as VIF is very high
# vif=2.599835966
#  Estimate Std. Error z value Pr(>|z|)
#  -1.83937    0.40096  -4.587 4.49e-06
model_6 <-glm(Attrition ~ Age + MonthlyIncome + NumCompaniesWorked + 
                StockOptionLevel + TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                Over_time + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                EducationField.xLife.Sciences + 
                EducationField.xMarketing + EducationField.xMedical + 
                EducationField.xTechnical.Degree + Education.x5 + JobRole.xHuman.Resources + 
                JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                JobRole.xSales.Executive +MaritalStatus.xSingle + 
                JobInvolvement.x3 + JobLevel.x5, family = "binomial", data = train)
summary(model_6)
vif(model_6)
sort(vif(model_6))
#write.csv(vif(model_6),'vif5.csv')

# Drop EducationField.xLife.Sciences as VIF is very high
# vif=3.092628
#  Estimate Std. Error z value Pr(>|z|)
#  -0.40850    0.20229  -2.019 0.043451
model_7 <-glm(Attrition ~ Age + MonthlyIncome + NumCompaniesWorked + 
                StockOptionLevel + TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                Over_time + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                EducationField.xMarketing + EducationField.xMedical + 
                EducationField.xTechnical.Degree + Education.x5 + JobRole.xHuman.Resources + 
                JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                JobRole.xSales.Executive +MaritalStatus.xSingle + 
                JobInvolvement.x3 + JobLevel.x5, family = "binomial", data = train)
summary(model_7)
vif(model_7)
sort(vif(model_7))
#write.csv(vif(model_7),'vif6.csv')

# Drop WorkLifeBalance.x2 as VIF is very high
# vif=3.103891235
#  Estimate Std. Error z value Pr(>|z|)
#  -1.04734    0.23015  -4.551 5.35e-06 
model_8 <-glm(Attrition ~ Age + MonthlyIncome + NumCompaniesWorked + 
                StockOptionLevel + TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                Over_time + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 +WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                EducationField.xMarketing + EducationField.xMedical + 
                EducationField.xTechnical.Degree + Education.x5 + JobRole.xHuman.Resources + 
                JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                JobRole.xSales.Executive +MaritalStatus.xSingle + 
                JobInvolvement.x3 + JobLevel.x5, family = "binomial", data = train)
summary(model_8)
vif(model_8)
sort(vif(model_8))
#write.csv(vif(model_8),'vif7.csv')

# Drop TotalWorkingYears as VIF is very high
# vif=2.543545
#  Estimate Std. Error z value Pr(>|z|)
#  -0.49803    0.10592  -4.702 2.58e-06

model_9 <-glm(Attrition ~ Age + MonthlyIncome + NumCompaniesWorked + 
                StockOptionLevel + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                Over_time + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 +WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                EducationField.xMarketing + EducationField.xMedical + 
                EducationField.xTechnical.Degree + Education.x5 + JobRole.xHuman.Resources + 
                JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                JobRole.xSales.Executive +MaritalStatus.xSingle + 
                JobInvolvement.x3 + JobLevel.x5, family = "binomial", data = train)
summary(model_9)
vif(model_9)
sort(vif(model_9))

# All variables have vif<2
# Now let's remove insignificant variables

# Drop JobRole.xHuman.Resources as P is very high
#  Estimate Std. Error z value Pr(>|z|)
# -0.36896    0.31256  -1.180 0.237821
model_10 <-glm(Attrition ~ Age + MonthlyIncome + NumCompaniesWorked + 
                 StockOptionLevel + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 Over_time + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 +WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                 EducationField.xMarketing + EducationField.xMedical + 
                 EducationField.xTechnical.Degree + Education.x5 +  
                 JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive +MaritalStatus.xSingle + 
                 JobInvolvement.x3 + JobLevel.x5, family = "binomial", data = train)
summary(model_10)

# Drop EducationField.xMedical  as P is very high
#  Estimate Std. Error z value Pr(>|z|)
# -0.15709    0.13037  -1.205 0.228230
model_11 <-glm(Attrition ~ Age + MonthlyIncome + NumCompaniesWorked + 
                 StockOptionLevel + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 Over_time + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 +WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                 EducationField.xMarketing + 
                 EducationField.xTechnical.Degree + Education.x5 +  
                 JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive +MaritalStatus.xSingle + 
                 JobInvolvement.x3 + JobLevel.x5, family = "binomial", data = train)
summary(model_11)

# Drop JobLevel.x5   as P is very high
#  Estimate Std. Error z value Pr(>|z|)
# -0.35844    0.28644  -1.251 0.210808 
model_12 <-glm(Attrition ~ Age + MonthlyIncome + NumCompaniesWorked + 
                 StockOptionLevel + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 Over_time + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 +WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                 EducationField.xMarketing + 
                 EducationField.xTechnical.Degree + Education.x5 +  
                 JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive +MaritalStatus.xSingle + 
                 JobInvolvement.x3 , family = "binomial", data = train)
summary(model_12)

# Drop JobRole.xResearch.Director  as P is very high
#  Estimate Std. Error z value Pr(>|z|)
# 0.30494    0.23185   1.315 0.188419
model_13 <-glm(Attrition ~ Age + MonthlyIncome + NumCompaniesWorked + 
                 StockOptionLevel + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 Over_time + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 +WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                 EducationField.xMarketing + 
                 EducationField.xTechnical.Degree + Education.x5 +  
                 JobRole.xManager + JobRole.xManufacturing.Director +  
                 JobRole.xSales.Executive +MaritalStatus.xSingle + 
                 JobInvolvement.x3 , family = "binomial", data = train)
summary(model_13)

# Drop EducationField.xMarketing  as P is very high
#  Estimate Std. Error z value Pr(>|z|)
# -0.24231    0.18057  -1.342 0.179616
model_14 <-glm(Attrition ~ Age + MonthlyIncome + NumCompaniesWorked + 
                 StockOptionLevel + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 Over_time + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 +WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                 EducationField.xTechnical.Degree + Education.x5 +  
                 JobRole.xManager + JobRole.xManufacturing.Director +  
                 JobRole.xSales.Executive +MaritalStatus.xSingle + 
                 JobInvolvement.x3 , family = "binomial", data = train)
summary(model_14)

# Drop MonthlyIncome  as P is very high
#  Estimate Std. Error z value Pr(>|z|)
#  -0.07850    0.05961  -1.317 0.187910
model_15 <-glm(Attrition ~ Age +NumCompaniesWorked + 
                 StockOptionLevel + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 Over_time + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 +WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                 EducationField.xTechnical.Degree + Education.x5 +  
                 JobRole.xManager + JobRole.xManufacturing.Director +  
                 JobRole.xSales.Executive +MaritalStatus.xSingle + 
                 JobInvolvement.x3 , family = "binomial", data = train)
summary(model_15)

# Drop WorkLifeBalance.x4  as P is very high
# Estimate Std. Error z value Pr(>|z|)
# -0.33708    0.20186  -1.670 0.094952
model_16 <-glm(Attrition ~ Age +NumCompaniesWorked + 
                 StockOptionLevel + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 Over_time + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 +WorkLifeBalance.x3 + 
                 BusinessTravel.xTravel_Frequently + 
                 EducationField.xTechnical.Degree + Education.x5 +  
                 JobRole.xManager + JobRole.xManufacturing.Director +  
                 JobRole.xSales.Executive +MaritalStatus.xSingle + 
                 JobInvolvement.x3 , family = "binomial", data = train)
summary(model_16)

# Drop JobRole.xSales.Executive  as P is very high
# Estimate Std. Error z value Pr(>|z|)
# 0.22661    0.13563   1.671 0.094757
model_17 <-glm(Attrition ~ Age +NumCompaniesWorked + 
                 StockOptionLevel + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 Over_time + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 +WorkLifeBalance.x3 + 
                 BusinessTravel.xTravel_Frequently + 
                 EducationField.xTechnical.Degree + Education.x5 +  
                 JobRole.xManager + JobRole.xManufacturing.Director +  
                 MaritalStatus.xSingle + 
                 JobInvolvement.x3 , family = "binomial", data = train)
summary(model_17)

# Drop StockOptionLevel  as P is very high
# Estimate Std. Error z value Pr(>|z|)
# -0.09106    0.05736  -1.588 0.112383
model_18 <-glm(Attrition ~ Age +NumCompaniesWorked + 
                 TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 Over_time + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 +WorkLifeBalance.x3 + 
                 BusinessTravel.xTravel_Frequently + 
                 EducationField.xTechnical.Degree + Education.x5 +  
                 JobRole.xManager + JobRole.xManufacturing.Director +  
                 MaritalStatus.xSingle + 
                 JobInvolvement.x3 , family = "binomial", data = train)
summary(model_18)

# Drop JobInvolvement.x3  as P is very high
# Estimate Std. Error z value Pr(>|z|)
# -0.19989    0.11374  -1.757  0.07885
model_19 <-glm(Attrition ~ Age +NumCompaniesWorked + 
                 TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 Over_time + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 +WorkLifeBalance.x3 + 
                 BusinessTravel.xTravel_Frequently + 
                 EducationField.xTechnical.Degree + Education.x5 +  
                 JobRole.xManager + JobRole.xManufacturing.Director +  
                 MaritalStatus.xSingle, family = "binomial", data = train)
summary(model_19)

# Drop Education.x5  as P is very high
model_20 <-glm(Attrition ~ Age +NumCompaniesWorked + 
                 TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 Over_time + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 +WorkLifeBalance.x3 + 
                 BusinessTravel.xTravel_Frequently + 
                 EducationField.xTechnical.Degree +  
                 JobRole.xManager + JobRole.xManufacturing.Director +  
                 MaritalStatus.xSingle, family = "binomial", data = train)
summary(model_20)

# Drop JobRole.xManager  as P is very high
model_21 <-glm(Attrition ~ Age +NumCompaniesWorked + 
                 TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 Over_time + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 +WorkLifeBalance.x3 + 
                 BusinessTravel.xTravel_Frequently + 
                 EducationField.xTechnical.Degree +  
                 JobRole.xManufacturing.Director +  
                 MaritalStatus.xSingle, family = "binomial", data = train)
summary(model_21)
final_model <-model_21

###########################################################################################################
############################## Model Evaluation############################################################
###########################################################################################################

### Test Data ####

### Model Evaluation

test_pred = predict(final_model, type = "response", 
                    newdata = test[,-2])

summary(test_pred)

test$prob <- test_pred
View(test)

test_actual_attrition<-factor(ifelse(test$Attrition==1,"Yes","No"))


library(e1071)
library(caret)


#P_Cutoff>=0.5
test_predict_attrition<-factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_conf <- confusionMatrix(test_predict_attrition, test_actual_attrition, positive = "Yes")
test_conf

#P_Cutoff>=0.4
test_predict_attrition<-factor(ifelse(test_pred >= 0.40, "Yes", "No"))
test_conf <- confusionMatrix(test_predict_attrition, test_actual_attrition, positive = "Yes")
test_conf

#Finding the Optimal Probability Cutoff where sensitivity, accuracy and specificity are very similar
perform_fn <- function(cutoff) 
{
  predicted_attrition <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_attrition, test_actual_attrition, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Summary of test probability

summary(test_pred)

s = seq(.01,.80,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 
dev.off()

plot(s,OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


op_p_cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]
#Optimal P_Cutoff is 0.186
test_predict_attrition<-factor(ifelse(test_pred >= op_p_cutoff, "Yes", "No"))
conf_final <- confusionMatrix(test_predict_attrition, test_actual_attrition, positive = "Yes")
conf_final

#           Accuracy    : 0.7491
#           Sensitivity : 0.7536          
#           Specificity : 0.7482
### As accuracy, sensitivity and specificity are similar optimal P cut off is fixed at 0.186


acc <- conf_final$overall[1]
sens <- conf_final$byClass[1]
spec <- conf_final$byClass[2]
acc
sens
spec
test$predicted <-test_predict_attrition
test$actual <-test_actual_attrition
table(test$predicted,test$actual)
View(test)

##################################################################################################
### KS -statistic - Test Data ######
test_predict_attrition <- ifelse(test_predict_attrition=="Yes",1,0)
test_actual_attrition <- ifelse(test_actual_attrition=="Yes",1,0)

install.packages("ROCR")
library(ROCR)
#on testing  data
pred_object_test<- prediction(test_predict_attrition, test_actual_attrition)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test) #0.5018311 (50.18%)

## K Statistic is 50.18% which lies between 40% and 60%, hence considering the model as good fit

# Lift & Gain Chart 

# plotting the lift chart

# Loading dplyr package 
require(dplyr)
library(dplyr)

lift <- function(labels,predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}
attrition_decile = lift(as.factor(test_actual_attrition),as.factor(test_pred),groups = 10)
attrition_decile
plot(attrition_decile$Gain,type='o')
plot(attrition_decile$Cumlift,type='o')
## Gain in increading and Lift is decreasing which is considered as a sign for good model.


############################################################################################################
##################################### POSSIBLE REASONS FOR ATTRITION########################################
############################################################################################################

# The more an employee works OVER TIME on an average the more are the chances that he/she will leave the company.

# If an employee works with the SAME MANAGER for a longer period of time,the lesser are the chances
# that employee will leave the company.

# Employees with more AGE and experience are less likely to leave the company

# Employees who are unmarried(single) are more likely to leave the company.

# Employees with Environment Satisfaction, Job Satisfaction and Work life balance have less chances of leaving.

# Employees who didnot receive promotion since last few years are more likely to leave the company.

# Employees worked in many companies are more likely to leave the company.

# Employees who are frequently on business travel are more likely to leave the company.

###########################################################################################################
############################################## RECOMMENDATIONS ############################################
###########################################################################################################

#Environment Satisfaction, Job Satisfaction and Work life balance, 
#the better these are for employees the less are their chances of leaving the company so improve them.

#The more an employee works overtime on an average the more are the chances that he/she will leave the company.
#so don't make employee work for more overtime

# Don't make employees go on business travel frequently

