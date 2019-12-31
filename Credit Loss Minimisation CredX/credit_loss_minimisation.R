library(plyr)
library(dplyr)
library(ggplot2)
library(corrplot)
library(Information)
library(caret)
library(car)
library(caTools)
library(dummies)
library(MASS)
library(e1071)
library(DMwR)
library(rpart)
library(rpart.plot)
library(randomForest)
library(ROCR)
library(gridExtra)
library(kernlab)
library(readr)
library(doParallel)
library(parallel)

#################### 1. Data loading and understanding data ##########################################################
demog <- read.csv("Demographic data.csv", stringsAsFactors=F)
str(demog) #71295 records
summary(demog)
names(demog)

bureau <- read.csv("Credit Bureau data.csv", stringsAsFactors=F)
str(bureau) #71295 records
summary(bureau)
names(bureau)

## Converting column names in both data sets to lower case for easy analysis
names(demog) <- tolower(names(demog))
names(bureau) <- tolower(names(bureau))

## Checking if application ID's are different in both the files
sum(!is.element(demog$application.id, bureau$application.id)) #0 
## application ID's are same in both the files

## Checking count of unique application ID's in both data sets
count(distinct(demog, application.id)) #71292 unique records in demographic data
count(distinct(bureau, application.id)) #71292 unique records in credit bureau

## Removing duplicate records from both the data sets
demog <- demog[-which(duplicated(demog$application.id) == T), ]
bureau <- bureau[-which(duplicated(bureau$application.id) == T), ]

## Merging both the data sets by application ID
master <- merge(demog, bureau, by = "application.id")
str(master)
summary(master)


## As performance tag column is present in both the file, there are two performance tag columns in merged data
## Checking for difference in this tag
levels(as.factor(master$performance.tag.x - master$performance.tag.y)) # 0 
## Performance tag values are same in both the files
## Removing one column of performance tag
master <- master[,-which(names(master) == "performance.tag.x")]
names(master)[29] <- "performance_tag"
names(master)

## There some column names which are very lengthy, for ease of operation and analysis renaming the same
colnames(master)[colnames(master) == "marital.status..at.the.time.of.application."] <- "marital_status"     
colnames(master)[colnames(master) == "no.of.dependents"] <- "dependents"
colnames(master)[colnames(master) == "type.of.residence"] <- "residence_type" 
colnames(master)[colnames(master) == "no.of.months.in.current.residence"] <- "curr_res"
colnames(master)[colnames(master) == "no.of.months.in.current.company"] <- "curr_exp" 
colnames(master)[colnames(master) == "no.of.times.90.dpd.or.worse.in.last.6.months"] <- "dpd90_6m"
colnames(master)[colnames(master) == "no.of.times.60.dpd.or.worse.in.last.6.months"] <- "dpd60_6m"
colnames(master)[colnames(master) == "no.of.times.30.dpd.or.worse.in.last.6.months"] <- "dpd30_6m"
colnames(master)[colnames(master) == "no.of.times.90.dpd.or.worse.in.last.12.months"] <- "dpd90_12m"
colnames(master)[colnames(master) == "no.of.times.60.dpd.or.worse.in.last.12.months"] <- "dpd60_12m"
colnames(master)[colnames(master) == "no.of.times.30.dpd.or.worse.in.last.12.months"] <- "dpd30_12m"
colnames(master)[colnames(master) == "avgas.cc.utilization.in.last.12.months"] <- "cc_util_12m"
colnames(master)[colnames(master) == "no.of.trades.opened.in.last.6.months"] <- "trades_6m"                   
colnames(master)[colnames(master) == "no.of.trades.opened.in.last.12.months"] <- "trades_12m" 
colnames(master)[colnames(master) == "no.of.pl.trades.opened.in.last.6.months"] <- "pltrades_6m" 
colnames(master)[colnames(master) == "no.of.pl.trades.opened.in.last.12.months"] <- "pltrades_12m"    
colnames(master)[colnames(master) == "no.of.inquiries.in.last.6.months..excluding.home...auto.loans."] <- "inquiries_6m"
colnames(master)[colnames(master) == "no.of.inquiries.in.last.12.months..excluding.home...auto.loans."] <- "inquiries_12m"                                              
colnames(master)[colnames(master) == "presence.of.open.home.loan"] <- "open_hl" 
colnames(master)[colnames(master) == "outstanding.balance"] <- "outstanding_balance"   
colnames(master)[colnames(master) == "total.no.of.trades"] <- "tot_trades"  
colnames(master)[colnames(master) == "presence.of.open.auto.loan"] <- "open_al"
#############################################################################################################################

####################################### 2. EDA #######################################################
## 1) We will find the count of missing data in each column
## 2) Univariate analysis will be performed on each column to find the outliers of numeric variables
##    Outliers will be fixed by replacing them with 2nd percentile and 98th percentile values whereever applicable
## 3) BiVariate analysis will be performed on each column to check the relation with performance tag
##      a) WOE analysis
##      b) Total number of customers in each group
##      c) Default percentage in each group
## 4) IV Value analysis will be performed to identify which variables are important for model building

## 2.1 Finding count of missing values or NA in each column ##

## Columns with NA values and percentage
sapply(master[colSums(is.na(master)) > 0], 
       function(x) 
         list(sum(is.na(x)), 
              round(sum(is.na(x)*100/71292), 4)))
## Below are columns with NA values
##  Columns     dependents  cc_util_12m   trades_6m   open_hl   outstanding_balance     performance_tag
##  NA Count       3           1058           1         272           272                   1425           
## Percentage   0.0042         1.484       0.0014      0.3815        0.3815                1.9988  

## Columns with missing values and percentage
sapply(master[,which(sapply(master, is.character))], 
       function(x) 
         list(length(which(x=="")), 
              round(length(which(x==""))*100/71292, 4)))
##  Columns     gender  marital_status  education   profession  residence_type
##  Missing       2        6              119         14           8             
##  Percentage  0.0028    0.0084         0.1669    0.0196        0.0112 

## For all the variables, missing values will be handled in WOE transformation in further analysis


## 2.2 Univariate analysis for Outlier treatement of numeric variables##
## From the data dictionary, it is understood that below are the numeric columns on which outlier treatment will be carried out
## age, no. of dependents, income, No of months in current residence, No of months in current company
## No of times 90 DPD or worse in last 6 months, No of times 60 DPD or worse in last 6 months, No of times 30 DPD or worse in last 6 months
## No of times 90 DPD or worse in last 12 months, No of times 60 DPD or worse in last 12 months, No of times 30 DPD or worse in last 12 months
## Avgas CC Utilization in last 12 months, No of trades opened in last 6 months, No of trades opened in last 12 months
## No of PL trades opened in last 6 months, No of PL trades opened in last 12 months,
## No of Inquiries in last 6 months, No of Inquiries in last 12 months
## Outstanding Balance, Total No of Trades

## Any Outliers found will be replaced with 2nd percentile and 98th percentile value

## Outlier treatement of age variable
boxplot(master$age, main = "age", col = "lightgrey", boxwex =0.5)
quantile(master$age, seq(0,1,0.01))
## There are outliers observed in age variable
## Age is varying from -3 to 65
## Fixing the outliers
## Assmption: Generally credit cards will be issued to people with minimum 18 years of age, so converting age <18 to 18
master$age[master$age < 18] <- 18

## Outlier treatement of No. of dependents variable
boxplot(master$dependents, main = "dependents", col = "lightgrey", boxwex =0.5)
quantile(master$dependents, seq(0,1,0.01), na.rm = T)
## There are no outliers observed in No. of dependents variable

## Outlier treatement of income variable
boxplot(master$income, main = "income", col = "lightgrey", boxwex =0.5)
quantile(master$income, seq(0,1,0.01), na.rm = T)
## One negative value of income(-0.5) is observed which is invalid as income cannot be negative.
## Fixing the outliers by replacing negative value with 2nd percentile value of 4.5
master$income[master$income < 4.5] <- 4.5

## Outlier treatement of No of months in current residence variable
boxplot(master$curr_res, main = "No. of months in current residence variable", col = "lightgrey", boxwex =0.5)
quantile(master$curr_res, seq(0,1,0.01), na.rm = T)
## There are no outliers observed in No of months in current residence

## Outlier treatement of No of months in current company variable
boxplot(master$curr_exp, main = "No. of months in current company", col = "lightgrey", boxwex =0.5)
quantile(master$curr_exp, seq(0,1,0.01), na.rm = T)
## Upper limit outliers observed , so limiting the experience to 98th percentile value
master$curr_exp[which(master$curr_exp > 72)] <- 72

## Outlier treatement of No of times 90 DPD or worse in last 6 months variable
boxplot(master$dpd90_6m, main = "No. of times 90 DPD or worse in last 6 months", col = "lightgrey", boxwex =0.5)
quantile(master$dpd90_6m, seq(0,1,0.01), na.rm = T)
## Box plot shows 1, 2 and 3 as outliers
## Not doing any treatment for this variable as 3 times 90 DPD or worse in last 6 months is possible if the customer is delaying every payment.
## Asumption: based on the data dictionary it is assumed that 90DPD or worse is the max DPD that credX is considering

## Outlier treatement of No of times 60 DPD or worse in last 6 months variable
boxplot(master$dpd60_6m, main = "No of times 60 DPD or worse in last 6 months", col = "lightgrey", boxwex =0.5)
quantile(master$dpd60_6m, seq(0,1,0.01), na.rm = T)
## Box plot shows 3, 4 and 5 as outliers
## Not doing any treatment for this variable as 5 times 60 DPD or worse in last 6 months is possible if the customer is delaying every payment by 61 days.

## Outlier treatement of No of times 30 DPD or worse in last 6 months variable
boxplot(master$dpd30_6m, main = "No of times 30 DPD or worse in last 6 months", col = "lightgrey", boxwex =0.5)
quantile(master$dpd30_6m, seq(0,1,0.01), na.rm = T)
## Box plot shows 3, 4, 5, 6 and 7 as outliers
## in a span of 180 days customer can become 30 DPD only 6 times, so capping the limit to 6
master$dpd30_6m[master$dpd30_6m > 6] <- 6

## Outlier treatement of No of times 90 DPD or worse in last 12 months variable
boxplot(master$dpd90_12m, main = "No of times 90 DPD or worse in last 12 months", col = "lightgrey", boxwex =0.5)
quantile(master$dpd90_12m, seq(0,1,0.01), na.rm = T)
## Box plot shows 3, 4 and 5 as outliers
## Not doing any treatment for this variable as 5 times 90 DPD or worse in last 12 months is possible
## Asumption: based on the data dictionary it is assumed that 90DPD or worse is the max DPD that credX is considering

## Outlier treatement of No of times 60 DPD or worse in last 12 months variable
boxplot(master$dpd60_12m, main = "No of times 60 DPD or worse in last 12 months", col = "lightgrey", boxwex =0.5)
quantile(master$dpd60_12m, seq(0,1,0.01), na.rm = T)
## Box plot shows 3, 4, 5, 6 and 7 as outliers
## Not doing any treatment for this variable as 7 times 60 DPD or worse in last 12 months is possible

## Outlier treatement of No of times 30 DPD or worse in last 12 months variable
boxplot(master$dpd30_12m, main = "No of times 30 DPD or worse in last 12 months", col = "lightgrey", boxwex =0.5)
quantile(master$dpd30_12m, seq(0,1,0.01), na.rm = T)
## Box plot shows 3, 4, 5, 6, 7, 8 and 9 as outliers
## Not doing any treatment for this variable as 9 times 30 DPD or worse in last 12 months is possible

## Outlier treatement of Avgas CC Utilization in last 12 months variable
boxplot(master$cc_util_12m, main = "Avgas CC Utilization in last 12 months", col = "lightgrey", boxwex =0.5)
quantile(master$cc_util_12m, seq(0,1,0.01), na.rm = T)
## Box plot shows outliers > 90
## Not doing any treatment for this variable as credit utilization depends on the credit card limit and there might be customers whose limit is greater than 125K


## Outlier treatement of No of trades opened in last 6 months variable
boxplot(master$trades_6m, main = "No of trades opened in last 6 months", col = "lightgrey", boxwex =0.5)
quantile(master$trades_6m, seq(0,1,0.01), na.rm = T)
## Max number of trades is 12 and minimum is zero


## Outlier treatement of No of trades opened in last 12 months variable
boxplot(master$trades_12m, main = "No of trades opened in last 12 months", col = "lightgrey", boxwex =0.5)
quantile(master$trades_12m, seq(0,1,0.01), na.rm = T)
## Max number of trades is 28 and minimum is zero, outliers observed in upper limit
## Not doing any outlier treatment as 28 transactions in 12 months is very ordinary situation


## Outlier treatement of Number of times the customers has inquired in last 6 months variable
boxplot(master$inquiries_6m, main = "No. of inquiries in last 6 months", col = "lightgrey", boxwex =0.5)
quantile(master$inquiries_6m, seq(0,1,0.01), na.rm = T)
## Max number of inquiries is 10 and minimum is zero
## Not doing any outlier treatment as 10 inquiries in 6 months is very ordinary situation

## Outlier treatement of Number of times the customers has inquired in last 12 months variable
boxplot(master$inquiries_12m, main = "No. of inquiries in last 12 months", col = "lightgrey", boxwex =0.5)
quantile(master$inquiries_12m, seq(0,1,0.01), na.rm = T)
## Max number of inquiries is 20 and minimum is zero
## Not doing any outlier treatment as 20 inquiries in 12 months is very ordinary situation

## Outlier treatement of Outstanding balance variable
boxplot(master$outstanding_balance, main = "Outstanding Balance", col = "lightgrey", boxwex =0.5)
quantile(master$outstanding_balance, seq(0,1,0.01), na.rm = T)
## No outliers observed in outstanding balance

## Outlier treatement of Total No of Trades variable
boxplot(master$tot_trades, main = "Total No of Trades", col = "lightgrey", boxwex =0.5)
quantile(master$tot_trades, seq(0,1,0.01), na.rm = T)
## Max number of trades is 44 and minimum is zero
## Not doing any outlier treatment as 44 trades is common


## 2.3 Bi-Variate Analysis ##
## From the data it is understood that performance.tag is the dependent variable.
## There are NA values in performance.tag which are the cases we consider as card is rejected.
## Separating the data sets for further analysis
reject_data <- subset(master, is.na(master$performance_tag))
approved_data <- subset(master, !is.na(master$performance_tag))

## WOE is always calculated as % of goods/ % of bads
## In given data of performance tag, 0 means good customer and 1 means bad(default) customer
## As the information package in R considers 1 as good and 0 as bad, reversing the performance tag values for futher WOE analysis
approved_data$performance_tag <- (approved_data$performance_tag - 1) * (-1)


## Correlation Analysis
nums <- sapply(approved_data,is.numeric)
cor_data<- approved_data[ , nums]
cor_index<- cor(cor_data, use = "complete.obs") 
corrplot(cor_index, type = "full",tl.pos = "dt", method = "circle", tl.cex = 1, tl.col = 'black', order = "hclust", diag = FALSE)

## Creating custom plot functions to use for all variables:
## Plot 1 group wise customer count
plot1 <- function(plot_data, plot_title, labelx)
{
  ggplot(plot_data, aes(factor(plot_data[,1]))) + 
    geom_bar(alpha=0.8) + geom_text(aes(label=..count..),stat="count") +
    ggtitle(plot_title) +
    labs(x=labelx, y="No.of customers")
}

## Plot 2 Group wise default percentage
plot2 <- function(plot_data, plot_title, labelx)
{
  ggplot(plot_data, aes(factor(plot_data[,1]), fill = factor(plot_data[,2]))) + 
    geom_bar(position = "fill", alpha=0.8) + 
    ggtitle(plot_title) +
    labs(x=labelx, y="% of customers", fill = "Performance")
}

## Converting some of the variables into factors and bins for some variables for ease of analysis
approved_data$gender <- as.factor(approved_data$gender)
reject_data$gender <- as.factor(reject_data$gender)

approved_data$marital_status <- as.factor(approved_data$marital_status)
reject_data$marital_status <- as.factor(reject_data$marital_status)

approved_data$dependents <- as.factor(approved_data$dependents)
reject_data$dependents <- as.factor(reject_data$dependents)

approved_data$education <- as.factor(approved_data$education)
reject_data$education <- as.factor(reject_data$education)

approved_data$profession <- as.factor(approved_data$profession)
reject_data$profession <- as.factor(reject_data$profession)

approved_data$residence_type <- as.factor(approved_data$residence_type)
reject_data$residence_type <- as.factor(reject_data$residence_type)

approved_data$dpd90_6m <- as.factor(approved_data$dpd90_6m)
reject_data$dpd90_6m <- as.factor(reject_data$dpd90_6m)

approved_data$dpd60_6m <- as.factor(approved_data$dpd60_6m)
reject_data$dpd60_6m <- as.factor(reject_data$dpd60_6m)

approved_data$dpd30_6m <- as.factor(approved_data$dpd30_6m)
reject_data$dpd30_6m <- as.factor(reject_data$dpd30_6m)

approved_data$dpd90_12m <- as.factor(approved_data$dpd90_12m)
reject_data$dpd90_12m <- as.factor(reject_data$dpd90_12m)

approved_data$dpd60_12m <- as.factor(approved_data$dpd60_12m)
reject_data$dpd60_12m <- as.factor(reject_data$dpd60_12m)

approved_data$dpd30_12m <- as.factor(approved_data$dpd30_12m)
reject_data$dpd30_12m <- as.factor(reject_data$dpd30_12m)

approved_data$open_hl <- as.factor(approved_data$open_hl)
reject_data$open_hl <- as.factor(reject_data$open_hl)

approved_data$open_al <- as.factor(approved_data$open_al)
reject_data$open_al <- as.factor(reject_data$open_al)

approved_data$income <- as.factor(cut(approved_data$income, pretty(approved_data$income)))
reject_data$income <- as.factor(cut(reject_data$income, pretty(reject_data$income)))

approved_data$curr_exp <- as.factor(cut(approved_data$curr_exp, breaks = c(0, 12, 24, 36, 48, 60, 72)))
reject_data$curr_exp <- as.factor(cut(reject_data$curr_exp, breaks = c(0, 12, 24, 36, 48, 60, 72)))

approved_data$cc_util_12m <- as.factor(cut(approved_data$cc_util_12m, breaks = c(-1, 5, 7, 9, 12, 15, 22, 38, 52, 72, 113)))
reject_data$cc_util_12m <- as.factor(cut(reject_data$cc_util_12m, breaks = c(-1, 5, 7, 9, 12, 15, 22, 38, 52, 72, 113)))

approved_data$tot_trades <- as.factor(cut(approved_data$tot_trades, breaks = c(-1, 2, 3, 4, 5, 6, 7, 8, 10, 44)))
reject_data$tot_trades <- as.factor(cut(reject_data$tot_trades, breaks = c(-1, 2, 3, 4, 5, 6, 7, 8, 10, 44)))

## WOE calculation for all the variables
IV <- create_infotables(approved_data[,-1], y="performance_tag", bins = 10, parallel=FALSE)

## Age varibale analysis
IV$Tables$age
plot_infotables(IV, "age")
approved_data$age <- as.factor(cut(approved_data$age, breaks = c(17, 30, 35, 38, 41, 44, 47, 50, 53, 57, 65)))
reject_data$age <- as.factor(cut(reject_data$age, breaks = c(17, 30, 35, 38, 41, 44, 47, 50, 53, 57, 65)))
plot1(approved_data[,c("age","performance_tag")], "Plot 1.1 : Age group Analysis", "Age Groups")
plot2(approved_data[,c("age","performance_tag")], "Plot 1.2: Age wise(Default Percentage)", "Age Groups")
summarise(group_by(approved_data, age), default_percentage = 1 - (sum(performance_tag)/n()))
##	Age group of 58 to 65 is having highest number of customers followed by 54 to 57
##	Percentage of default is varying from 3.7 to 4.51 with highest in 36 to 41 age group followed by 54 to 57 and lowest in 52 to 53
##	WOE analysis shows that % of defaulters compared to % of good customers is high in age groups 36 to 41 followed by 54 to 57 and 31 to 35. In all other groups, % of good customers is higher compared to % of defaulters.

## Gender Analysis
IV$Tables$gender
plot_infotables(IV, "gender")
plot1(approved_data[,c("gender","performance_tag")], "Plot 2.1 : Gender Analysis", "Gender")
plot2(approved_data[,c("gender","performance_tag")], "Plot 2.2: Gender wise(Default Percentage)", "Gender")
summarise(group_by(approved_data, gender), default_percentage = 1 - (sum(performance_tag)/n()))
## Male gender is having highest number of customers.
## Default percentage is similar across gender, but females are having slightly higher default percentage.
## From WOE it is evident that, distribution of bad customers compared to good is high in Female.

## Marital Status Analysis
IV$Tables$marital_status
plot_infotables(IV, "marital_status")
plot1(approved_data[,c("marital_status","performance_tag")], "Plot 3.1: Marital status Analysis", "Marital Status")
plot2(approved_data[,c("marital_status","performance_tag")], "Plot 3.2: Marital status(Default Percentage)", "Marital Status")
summarise(group_by(approved_data, marital_status), default_percentage = 1 - (sum(performance_tag)/n()))
## Married customers are more(more than 5 times) compared to Single customers
## Percentage of default is similar across marital status, with single customers having sligthly highler default rate

## No. of Dependents Analysis
IV$Tables$dependents
plot_infotables(IV, "dependents")
plot1(approved_data[,c("dependents","performance_tag")], "Plot 4.1: No. Of dependents", "No. Of dependents")
plot2(approved_data[,c("dependents","performance_tag")], "Plot 4.2: No. Of dependents(Default Percentage)", "No. Of dependents")
summarise(group_by(approved_data, dependents), default_percentage = (1 - sum(performance_tag)/n()))
## Customers with number of dependents 3, 2 and 1 are higher.
## Default rate is higher in customers with 3 dependants and low in customers with 2 dependents
## WOE: Distribution of default compared to good customers is high in 3 dependents customer followed by 1 and vice versa in 2.

## Income Analysis
IV$Tables$income
plot_infotables(IV, "income")
plot1(approved_data[,c("income","performance_tag")], "Plot 5.1: Income level", "Income Level")
plot2(approved_data[,c("income","performance_tag")], "Plot 5.2: Income Level(Default Percentage)", "Income Level")
summarise(group_by(approved_data, income), default_percentage = (1-sum(performance_tag)/n()))
## Customer are higher with income 31 to 40 followed by 21 to 30.
## As the income level is increasing, default rate is decreasing with highest rate for income less than 10.
## WOE: Distribution of defaulters decreases with increase in income

## Education Level Analysis
IV$Tables$education
plot_infotables(IV, "education")
plot1(approved_data[,c("education","performance_tag")], "Plot 6.1: Education", "Education Level")
plot2(approved_data[,c("education","performance_tag")], "Plot 6.2: Education(Default Percentage)", "Education Level")
summarise(group_by(approved_data, education), default_percentage = (1 - sum(performance_tag)/n()))
## Customers with professional education are higher followed by Masters and Bachelor.
## Default rate is higher in customers with education "Others"
## WOE: Distribution of default customers compared to good customers is very high in "Others" category.

## Profession Analysis
IV$Tables$profession
plot_infotables(IV, "profession")
plot1(approved_data[,c("profession","performance_tag")], "Plot 7.1: Profession Level", "Profession Level")
plot2(approved_data[,c("profession","performance_tag")], "Plot 7.2: Profession level(Default Percentage)", "Profession Level")
summarise(group_by(approved_data, profession), default_percentage = (1 - sum(performance_tag)/n()))
## Customers with Salaried are highest in profession
##	Default rate is higher in the Self Employed(SE) category
##	WOE: Distribution of default customers compared to good customers is very high in SE category.

## Residence Type Analysis
IV$Tables$residence_type
plot_infotables(IV, "residence_type")
plot1(approved_data[,c("residence_type","performance_tag")], "Plot 8.1: Residence type", "Residence type")
plot2(approved_data[,c("residence_type","performance_tag")], "Plot 8.2: Residence type(Default Percentage)", "Residence type")
summarise(group_by(approved_data, residence_type), default_percentage = (1 - sum(performance_tag)/n()))
# No. of customers are more with Rented house followed by Owned house
# Default rate is same across all residence types except for "Others" which is lower compared to other groups.

## Months in current residence analysis
IV$Tables$curr_res
plot_infotables(IV, "curr_res")
approved_data$curr_res <- as.factor(cut(approved_data$curr_res, breaks = c(5, 9, 28, 49, 72, 97, 126)))
reject_data$curr_res <- as.factor(cut(reject_data$curr_res, breaks = c(5, 9, 28, 49, 72, 97, 126)))
plot1(approved_data[,c("curr_res","performance_tag")], "Plot 9.1: Months in Current Residence", "Months in Current Residence")
plot2(approved_data[,c("curr_res","performance_tag")], "Plot 9.2: Months in Current Residence(Default Percentage)", "Months in Current Residence")
summarise(group_by(approved_data, curr_res), default_percentage = (1 - sum(performance_tag)/n()))
##	Customers with months in current residence 6 to 9 are high.
##	Default rate is decreasing with increase in months from 10 to 126.
##	WOE: Distribution of default customers is high compared to good in months 10 to 28.

## Months in current Company analysis
IV$Tables$curr_exp
plot_infotables(IV, "curr_exp")
plot1(approved_data[,c("curr_exp","performance_tag")], "Plot 10.1: Experience in Current company", "Experience in Current company")
plot2(approved_data[,c("curr_exp","performance_tag")], "Plot 10.2: Experience in Current company(Default Percentage)", "Experience in Current company")
summarise(group_by(approved_data, curr_exp), default_percentage = (1 - sum(performance_tag)/n()))
## No. Customers are similar in all experience groups except for >60. which is lesser compared to others
## Default percentage is slightly decreasing as the experience is increasing till 60 and increased afer 60
## WOE: Distribution of default customers compared to good is high in customers with experience <= 2 years and vice versa with 4 to 5 years' experience.

## No of times 90 DPD or worse in last 6 months analysis
IV$Tables$dpd90_6m
plot_infotables(IV, "dpd90_6m")
plot1(approved_data[,c("dpd90_6m","performance_tag")], "Plot 11.1: No of times 90 DPD or worse in last 6 months", "90 DPD in 6 months")
plot2(approved_data[,c("dpd90_6m","performance_tag")], "Plot 11.2: No of times 90 DPD or worse in last 6 months(Default Percentage)", "90 DPD in 6 months")
summarise(group_by(approved_data, dpd90_6m), default_percentage = (1 - sum(performance_tag)/n()))
# Number of customers are high with 0 90 dpd in last 6 months
# Default percentage is increasing with increase in 90 dpd
#	WOE: Distribution of default compared to good is increasing with increase in number of times 90 DPD in last 6 months

## No of times 60 DPD or worse in last 6 months analysis
IV$Tables$dpd60_6m
plot_infotables(IV, "dpd60_6m")
plot1(approved_data[,c("dpd60_6m","performance_tag")], "Plot 12.1: No of times 60 DPD or worse in last 6 months", "60 DPD in 6 months")
plot2(approved_data[,c("dpd60_6m","performance_tag")], "Plot 12.2: No of times 60 DPD or worse in last 6 months(Default Percentage)", "60 DPD in 6 months")
summarise(group_by(approved_data, dpd60_6m), default_percentage = (1 - sum(performance_tag)/n()))
# Number of customers are high with 0 60 dpd in last 6 months
# Default percentage is increasing with increase in 60 dpd till 3 and slightly decresing from 3 till 5
#	WOE: Distribution of default compared to good increasing with increase in number of times 60 DPD in last 6 months till 3 and slightly decreasing later.

## No of times 30 DPD or worse in last 6 months analysis
IV$Tables$dpd30_6m
plot_infotables(IV, "dpd30_6m")
plot1(approved_data[,c("dpd30_6m","performance_tag")], "Plot 13.1: No of times 30 DPD or worse in last 6 months", "30 DPD in 6 months")
plot2(approved_data[,c("dpd30_6m","performance_tag")], "Plot 13.2: No of times 30 DPD or worse in last 6 months(Default Percentage)", "30 DPD in 6 months")
summarise(group_by(approved_data, dpd30_6m), default_percentage = (1 - sum(performance_tag)/n()))
# Number of customers are high with 0 30 dpd in last 6 months
# Default percentage is increasing with increase in 30 dpd till 5 and slightly decresing from 5 till 6

## No of times 90 DPD or worse in last 12 months analysis
IV$Tables$dpd90_12m
plot_infotables(IV, "dpd90_12m")
plot1(approved_data[,c("dpd90_12m","performance_tag")], "Plot 14.1: No of times 90 DPD or worse in last 12 months", "90 DPD in 12 months")
plot2(approved_data[,c("dpd90_12m","performance_tag")], "Plot 14.2: No of times 90 DPD or worse in last 12 months(Default Percentage)", "90 DPD in 12 months")
summarise(group_by(approved_data, dpd90_12m), default_percentage = (1 - sum(performance_tag)/n()))
##	Number of customers are high with 0 90 dpd in last 12 months
##	Default percentage is increasing with increase in count of 90 dpd in last 12 months
##	WOE: Distribution of default compared to good increasing with increase in number of times 90 DPD in last 12 months.

## No of times 60 DPD or worse in last 12 months analysis
IV$Tables$dpd60_12m
plot_infotables(IV, "dpd60_12m")
plot1(approved_data[,c("dpd60_12m","performance_tag")], "Plot 15.1: No of times 60 DPD or worse in last 12 months", "60 DPD in 12 months")
plot2(approved_data[,c("dpd60_12m","performance_tag")], "Plot 15.2: No of times 60 DPD or worse in last 12 months(Default Percentage)", "60 DPD in 12 months")
summarise(group_by(approved_data, dpd60_12m), default_percentage = (1 - sum(performance_tag)/n()))
## Number of customers are high with 0 60 dpd in last 6 months
## Default percentage is increasing with increase in 60 dpd till 6 and decreased from 6 to 7
##	WOE: Distribution of default compared to good high in 6 number of times 60 DPD in last 12 months

## No of times 30 DPD or worse in last 12 months analysis
IV$Tables$dpd30_12m
plot_infotables(IV, "dpd30_12m")
plot1(approved_data[,c("dpd30_12m","performance_tag")], "Plot 16.1: No of times 30 DPD or worse in last 12 months", "30 DPD in 12 months")
plot2(approved_data[,c("dpd30_12m","performance_tag")], "Plot 16.2: No of times 30 DPD or worse in last 12 months(Default Percentage)", "30 DPD in 12 months")
summarise(group_by(approved_data, dpd30_12m), default_percentage = (1 - sum(performance_tag/n())))
# Number of customers are high with 0 30 dpd in last 6 months
# Default percentage is increasing with increase in 30 dpd till 5 and decresing from 5 till 9

## Avgas CC Utilization in last 12 months analysis
IV$Tables$cc_util_12m
plot_infotables(IV, "cc_util_12m")
plot1(approved_data[,c("cc_util_12m","performance_tag")], "Plot 17.1: Avgas CC Utilization in last 12 months", "CC Utilization in 12 months")
plot2(approved_data[,c("cc_util_12m","performance_tag")], "Plot 17.2: Avgas CC Utilization in last 12 months(Default Percentage)", "CC Utilization in 12 months")
summarise(group_by(approved_data, cc_util_12m), default_percentage = (1- sum(performance_tag/n())))
##	Customers with average credit card utilization from 10 to 12 is higher.
##	As the Utilization is increasing, the default rate is increasing till 72.
##	WOE: Distribution of good compared to default is decreasing with increase in utilization.

## No of trades opened in last 6 months analysis
IV$Tables$trades_6m
plot_infotables(IV, "trades_6m")
approved_data$trades_6m <- as.factor(cut(approved_data$trades_6m, breaks = c(-1, 0, 1, 2, 3, 4, 12)))
reject_data$trades_6m <- as.factor(cut(reject_data$trades_6m, breaks = c(-1, 0, 1, 2, 3, 4, 12)))
plot1(approved_data[,c("trades_6m","performance_tag")], "Plot 18.1: No of trades opened in last 6 months", "No of trades opened in last 6 months")
plot2(approved_data[,c("trades_6m","performance_tag")], "Plot 18.2: No of trades opened in last 6 months(Default Percentage)", "No of trades opened in last 6 months")
summarise(group_by(approved_data, trades_6m), default_percentage = (1 - sum(performance_tag/n())))
##	Customers with 1 trade in last 6 months are higher.
##	Default rate is increasing with number of trades till 4 and reduced from 5 to 12.
##	WOE: Distribution of default compared to good increased with increase in trades till 4 and reduced from 5 to 12.

## No of trades opened in last 12 months analysis
IV$Tables$trades_12m
plot_infotables(IV, "trades_12m")
approved_data$trades_12m <- as.factor(cut(approved_data$trades_12m, breaks = c(-1, 0, 1, 2, 3, 5, 7, 9, 12, 28)))
reject_data$trades_12m <- as.factor(cut(reject_data$trades_12m, breaks = c(-1, 0, 1, 2, 3, 5, 7, 9, 12, 28)))
plot1(approved_data[,c("trades_12m","performance_tag")], "Plot 19.1: No of trades opened in last 12 months", "No of trades opened in last 12 months")
plot2(approved_data[,c("trades_12m","performance_tag")], "Plot 19.2: No of trades opened in last 12 months", "No of trades opened in last 12 months")
summarise(group_by(approved_data, trades_12m), default_percentage = (1 - sum(performance_tag/n())))
## Customers with 1 trade in last 12 months are higher.
## Default rate is increasing with number of trades from 3 to 9 and reduced from 10 to 28.
## WOE: Distribution of default compared to good increased with increase in trades from 3 to 9 and reduced from 10 to 28.

## No of PL trades opened in last 6 months analysis
IV$Tables$pltrades_6m
plot_infotables(IV, "pltrades_6m")
approved_data$pltrades_6m <- as.factor(cut(approved_data$pltrades_6m, breaks = c(-1, 0, 1, 2, 6)))
reject_data$pltrades_6m <- as.factor(cut(reject_data$pltrades_6m, breaks = c(-1, 0, 1, 2, 6)))
plot1(approved_data[,c("pltrades_6m","performance_tag")], "Plot 20.1: No of PL trades opened in last 6 months", "No of PL trades opened in last 6 months")
plot2(approved_data[,c("pltrades_6m","performance_tag")], "Plot 20.2: No of PL trades opened in last 6 months", "No of PL trades opened in last 6 months")
summarise(group_by(approved_data, pltrades_6m), default_percentage = (1 - sum(performance_tag/n())))
##	Customers with 0 PL trades in last 6 months are higher.
##	Default rate is increasing with number of PL trades till 2 and reduced from 3 to 6.
##	WOE: Distribution of default compared to good increased with in trades 1 and 2 and reduced from 3 to 6.

## No of PL trades opened in last 12 months analysis
IV$Tables$pltrades_12m
plot_infotables(IV, "pltrades_12m")
approved_data$pltrades_12m <- as.factor(cut(approved_data$pltrades_12m, breaks = c(-1, 0, 1, 2, 3, 4, 5, 12)))
reject_data$pltrades_12m <- as.factor(cut(reject_data$pltrades_12m, breaks = c(-1, 0, 1, 2, 3, 4, 5, 12)))
plot1(approved_data[,c("pltrades_12m","performance_tag")], "Plot 21.1: No of PL trades opened in last 12 months", "No of PL trades opened in last 12 months")
plot2(approved_data[,c("pltrades_12m","performance_tag")], "Plot 21.2: No of PL trades opened in last 12 months(Default Percentage)", "No of PL trades opened in last 12 months")
summarise(group_by(approved_data, pltrades_12m), default_percentage = (1 - sum(performance_tag/n())))
## Customers with 0 PL trades in last 12 months are higher.
## Default rate is increasing with number of PL trades till 4 and reduced from 5 to 12.
## WOE: Distribution of default compared to good increased with in trades 2 and 4 and slightly reduced from 5 to 12.

## No of Inquiries in last 6 months (excluding home & auto loans) analysis
IV$Tables$inquiries_6m
plot_infotables(IV, "inquiries_6m")
approved_data$inquiries_6m <- as.factor(cut(approved_data$inquiries_6m, breaks = c(-1, 0, 1, 2, 4, 10)))
reject_data$inquiries_6m <- as.factor(cut(reject_data$inquiries_6m, breaks = c(-1, 0, 1, 2, 4, 10)))
plot1(approved_data[,c("inquiries_6m","performance_tag")], "Plot 22.1: No of inquiries in last 6 months", "No of inquiries in last 6 months")
plot2(approved_data[,c("inquiries_6m","performance_tag")], "Plot 22.2: No of inquiries in last 6 months(Default Percentage)", "No of inquiries in last 6 months")
summarise(group_by(approved_data, inquiries_6m), default_percentage = (1 - sum(performance_tag/n())))
## Customers with 0 inquiries in last 6 months are higher.
## Default rate is increasing with number of inquiries till 4 and reduced from 5 to 10.
## WOE: Distribution of default compared to good increased with inquiries 1 to 4 and reduced from 5 to 10.

## No of Inquiries in last 12 months (excluding home & auto loans) analysis
IV$Tables$inquiries_12m
plot_infotables(IV, "inquiries_12m")
approved_data$inquiries_12m <- as.factor(cut(approved_data$inquiries_12m, breaks = c(-1, 0, 1, 2, 3, 4, 5, 8, 20)))
reject_data$inquiries_12m <- as.factor(cut(reject_data$inquiries_12m, breaks = c(-1, 0, 1, 2, 3, 4, 5, 8, 20)))
plot1(approved_data[,c("inquiries_12m","performance_tag")], "Plot 23.1: No of inquiries in last 12 months", "No of inquiries in last 12 months")
plot2(approved_data[,c("inquiries_12m","performance_tag")], "Plot 23.2: No of inquiries in last 12 months(Default Percentage)", "No of inquiries in last 12 months")
summarise(group_by(approved_data, inquiries_12m), default_percentage = (1 - sum(performance_tag/n())))
# Number of customers with inquiries = 0 is higher in last 12 months
# Percentage of default is varying with number of inquiries in last 12 months and there is no particular trend

## Presence of open home loan Analysis
IV$Tables$open_hl
plot_infotables(IV, "open_hl")
plot1(approved_data[,c("open_hl","performance_tag")], "Plot 24.1: Presence of open home loan", "Presence of open home loan")
plot2(approved_data[,c("open_hl","performance_tag")], "Plot 24.2: Presence of open home loan", "Presence of open home loan")
summarise(group_by(approved_data, open_hl), default_percentage = (1 - sum(performance_tag/n())))
##	Customers without presence of open home loan are higher.
##	Default rate is higher in customers without open home loan
##	WOE: Distribution of default compared to good is high in customers without open home loan.

## Presence of open Auto loan Analysis
IV$Tables$open_al
plot_infotables(IV, "open_al")
plot1(approved_data[,c("open_al","performance_tag")], "Plot 25.1: Presence of open auto loan", "Presence of open auto loan")
plot2(approved_data[,c("open_al","performance_tag")], "Plot 25.2: Presence of open auto loan", "Presence of open auto loan")
summarise(group_by(approved_data, open_al), default_percentage = (1 - sum(performance_tag/n())))
##	Customers without presence of open auto loan are higher.
##	Default rate is higher in customers without auto loan
##	WOE: Distribution of default compared to good is high in customers without auto loan.

## Outstanding Balance Analysis
IV$Tables$outstanding_balance
plot_infotables(IV, "outstanding_balance")
approved_data$outstanding_balance <- as.factor(cut(approved_data$outstanding_balance, breaks = c(-1, 6843, 25509, 386813, 585402, 774228, 972455, 1357300, 2960998, 3282314, 5218801)))
reject_data$outstanding_balance <- as.factor(cut(reject_data$outstanding_balance, breaks = c(-1, 6843, 25509, 386813, 585402, 774228, 972455, 1357300, 2960998, 3282314, 5218801)))
plot1(approved_data[,c("outstanding_balance","performance_tag")], "Plot 26.1: Outstanding Balance", "Outstanding Balance")
plot2(approved_data[,c("outstanding_balance","performance_tag")], "Plot 26.2: Outstanding Balance(Default Percentage)", "Outstanding Balance")
summarise(group_by(approved_data, outstanding_balance), default_percentage = (1 - sum(performance_tag/n())))
## Customer count is similar in all groups of outstanding balance
## Default rate increased with outstanding till 972455.
## WOE: Distribution of default compared to good is higher for outstanding between 25522 and 1357300.

## Total No of Trades analysis
IV$Tables$tot_trades
plot_infotables(IV, "tot_trades")
plot1(approved_data[,c("tot_trades","performance_tag")], "Plot 27.1: Total trades", "Total Trades")
plot2(approved_data[,c("tot_trades","performance_tag")], "Plot 27.2: Total Trades(Default Percentage)", "Total Trades")
summarise(group_by(approved_data, tot_trades), default_percentage = (1 - sum(performance_tag/n())))
##	Customers with trades greater than 10 are higher.
##	Default rate increases with increase in total trades till 10 and decreases later.
##	WOE: Distribution of default compared to good is higher for trades greater than 5.

## IV Value analysis
approved_data$performance_tag <- (approved_data$performance_tag - 1) * (-1)
IV <- create_infotables(approved_data[,-1], y="performance_tag", bins=10, parallel=FALSE)
IV_Value = data.frame(IV$Summary)
IV_Value

grid.table(IV$Summary, rows=NULL)

## Below are variables which are having medium(0.1) to Strong(0.5) predictive power
##            Variable    IV Value
##         cc_util_12m 3.170946e-01
##          trades_12m 2.979723e-01
##        pltrades_12m 2.958971e-01
##       inquiries_12m 2.954176e-01
## outstanding_balance 2.462796e-01
##            dpd30_6m 2.442226e-01
##         pltrades_6m 2.197272e-01
##          tot_trades 2.196693e-01
##           dpd30_12m 2.182230e-01
##           dpd90_12m 2.156436e-01
##            dpd60_6m 2.112635e-01
##        inquiries_6m 2.051762e-01
##           dpd60_12m 1.881931e-01
##           trades_6m 1.860271e-01
##            dpd90_6m 1.626497e-01


##################### 3. Demographic data logistic regression model ########################################## 
demog_lr <- master[,c(1:11, 29)]
demog_lr <- subset(demog_lr, !is.na(demog_lr$performance_tag))
## Below are the demographic columns with missing and NA values as per analysis in previous sections, 
##  Columns     gender  marital_status  education   profession  residence_type   dependents
##  Missing       2        6              119         14           8                3
##  Percentage  0.0028    0.0084         0.1669    0.0196        0.0112           0.0042

## Since all the percentages are less than 0.2, imputing the missing values by deleting them
demog_lr$gender <- as.character(demog_lr$gender)
demog_lr <- subset(demog_lr, demog_lr$gender != "")
demog_lr$gender <- as.factor(demog_lr$gender)

demog_lr$marital_status <- as.character(demog_lr$marital_status)
demog_lr <- subset(demog_lr, demog_lr$marital_status != "")
demog_lr$marital_status <- as.factor(demog_lr$marital_status)

demog_lr$education <- as.character(demog_lr$education)
demog_lr <- subset(demog_lr, demog_lr$education != "")
demog_lr$education <- as.factor(demog_lr$education)

demog_lr$profession <- as.character(demog_lr$profession)
demog_lr <- subset(demog_lr, demog_lr$profession != "")
demog_lr$profession <- as.factor(demog_lr$profession)

demog_lr$residence_type <- as.character(demog_lr$residence_type)
demog_lr <- subset(demog_lr, demog_lr$residence_type != "")
demog_lr$residence_type <- as.factor(demog_lr$residence_type)

demog_lr$dependents <- as.numeric(demog_lr$dependents)
demog_lr <- subset(demog_lr, !is.na(demog_lr$dependents))
demog_lr$dependents <- as.factor(demog_lr$dependents)

#converting all factor variables into numeric(dummies)
demog_lr <- dummy.data.frame(demog_lr)

#converting dependent varibale into factor
demog_lr$performance_tag <- as.factor(ifelse(demog_lr$performance_tag == 1, "yes", "no"))

set.seed(100)

# splitting into train and test data
split_indices <- sample.split(demog_lr$performance_tag, SplitRatio = 0.70)
train_demog <- demog_lr[split_indices, ]
test_demog <- demog_lr[!split_indices, ]

# Building initial model without application ID parameter
model1 <- glm(performance_tag ~ . - application.id, family = "binomial", data = train_demog)
summary(model1)

# Using stepwise algorithm for removing insignificant variables
model_step <- stepAIC(model1, direction = "both")
model_step

# STEP AIC had resulted in below model
model2 <- glm(formula = performance_tag ~ genderF + dependents2 + income + 
                professionSE + curr_res + curr_exp, family = "binomial", data = train_demog)
summary(model2)
vif(model2)
## gender Female is having significance of 0.1242(>>0.05), hence removing the same
## VIF values are ~1

model3 <- glm(formula = performance_tag ~ dependents2 + income + 
                professionSE + curr_res + curr_exp, family = "binomial", data = train_demog)
summary(model3)
vif(model3)
## Profession SE is having significance of 0.0773(>>0.05), hence removing the same
## VIF values are ~1

model4 <- glm(formula = performance_tag ~ dependents2 + income + curr_res + curr_exp, family = "binomial", data = train_demog)
summary(model4)
vif(model4)
## All the variables are having P < 0.05, but some are not ***
## Dependents 2 is having significance of 0.047(~0.05), hence removing the same
## VIF values are ~1

model5 <- glm(formula = performance_tag ~ income + curr_res + curr_exp, family = "binomial", data = train_demog)
summary(model5)
vif(model5)
## All the variables are having P < 0.05, but still some are not ***
## No. of months in current residence(curr_res) is having significance of 0.0324(>0.01), hence removing the same
## VIF values are ~1

model6 <- glm(formula = performance_tag ~ income + curr_exp, family = "binomial", data = train_demog)
summary(model6)
## All the variables are now significant with P < 0.001
## Hence considering model6 as final model of demographic data
model_final <- model6

#### Model evaluation on test data
# Predicting probabilities of responding for the test data
predictions_logit <- predict(model_final, newdata = test_demog[, -28], type = "response")
summary(predictions_logit)

test_demog$predicted_probability <- predictions_logit
test_demog <- test_demog[order(test_demog$predicted_probability, decreasing = T), ]

# Let's use the probability cutoff of 50%.
predicted_performance <- factor(ifelse(predictions_logit >= 0.50, "yes", "no"))

# Creating confusion matrix for identifying the model evaluation.
conf <- confusionMatrix(predicted_performance, test_demog$performance_tag, positive = "yes")
conf
## Accuracy: 0.9578
## Sensitivity : 0.00000        
## Specificity : 1.00000 

## Sensitivity is 0, so need to find optimal cut off probability

## Finding cutoff probability
perform_fn <- function(cutoff) 
{
  predicted_performance <- factor(ifelse(test_demog$predicted_probability >= cutoff, "yes", "no"))
  conf <- confusionMatrix(predicted_performance, test_demog$performance_tag, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Creating cutoff values from 0.01 to 0.99 for plotting and initiallizing a matrix of 1000 X 4.
s = seq(.01,.99,length=100)
OUT = matrix(0,100,3)

for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

# plotting cutoffs 
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
##legend(0,0.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(1,1,1,1),c("Sensitivity","Specificity","Accuracy"))

# From the graph optimal cutoff probability seems to be around 4.2% which is 0.042
predicted_performance <- factor(ifelse(test_demog$predicted_probability >= 0.042, "yes", "no"))

## Creating confusion matrix to check validity of optimal cut off probability
conf_final <- confusionMatrix(predicted_performance, test_demog$performance_tag, positive = "yes")
acc <- conf_final$overall[1]
sens <- conf_final$byClass[1]
spec <- conf_final$byClass[2]

acc #0.536
sens #0.527
spec #0.536
## At the optimal cut off probability of 0.042, Accuracy, Sensitivity and Specificity are in the same range of around 0.5

## Model developed on Demographic data is giving an accuracy of 53% at 4.2% probability, which is a very poor predictive model
## This implicates that we need to consider credit bureau data as well for model generation
#######################################################################################################################################

############################## 4. Model using combined data ###############################################
## We will be creating two data sets one with WOE transformation and the other with standard imputation techniques
## These two data sets will be used for modelling on combined data

######## WOE data preparation
## For all the variables we will be doing WOE transformation as there are some missing values.
## Creating NA level in columns where NA values are present
approved_data$cc_util_12m <- addNA(approved_data$cc_util_12m)
approved_data$outstanding_balance <- addNA(approved_data$outstanding_balance)
approved_data$trades_6m <- addNA(approved_data$trades_6m)
approved_data$dependents <- addNA(approved_data$dependents)
approved_data$open_hl <- addNA(approved_data$open_hl)

reject_data$cc_util_12m <- addNA(reject_data$cc_util_12m)
reject_data$outstanding_balance <- addNA(reject_data$outstanding_balance)
reject_data$trades_6m <- addNA(reject_data$trades_6m)
## WOE transformation of all variables
approved_data$age_woe <- mapvalues(approved_data$age, from = IV$Tables$age$age, to = IV$Tables$age$WOE)
approved_data$gender_woe <- mapvalues(approved_data$gender, from = IV$Tables$gender$gender, to = IV$Tables$gender$WOE)
approved_data$marital_status_woe <- mapvalues(approved_data$marital_status, from = IV$Tables$marital_status$marital_status, to = IV$Tables$marital_status$WOE)
approved_data$dependents_woe <- mapvalues(approved_data$dependents, from = IV$Tables$dependents$dependents, to = IV$Tables$dependents$WOE)
approved_data$income_woe <- mapvalues(approved_data$income, from = IV$Tables$income$income, to = IV$Tables$income$WOE)
approved_data$education_woe <- mapvalues(approved_data$education, from = IV$Tables$education$education, to = IV$Tables$education$WOE)
approved_data$profession_woe <- mapvalues(approved_data$profession, from = IV$Tables$profession$profession, to = IV$Tables$profession$WOE)
approved_data$residence_type_woe <- mapvalues(approved_data$residence_type, from = IV$Tables$residence_type$residence_type, to = IV$Tables$residence_type$WOE)
approved_data$curr_res_woe <- mapvalues(approved_data$curr_res, from = IV$Tables$curr_res$curr_res, to = IV$Tables$curr_res$WOE)
approved_data$curr_exp_woe <- mapvalues(approved_data$curr_exp, from = IV$Tables$curr_exp$curr_exp, to = IV$Tables$curr_exp$WOE)
approved_data$open_hl_woe <- mapvalues(approved_data$open_hl, from = IV$Tables$open_hl$open_hl, to = IV$Tables$open_hl$WOE)
approved_data$open_al_woe <- mapvalues(approved_data$open_al, from = IV$Tables$open_al$open_al, to = IV$Tables$open_al$WOE)
approved_data$cc_util_12m_woe <- mapvalues(approved_data$cc_util_12m, from = IV$Tables$cc_util_12m$cc_util_12m, to = IV$Tables$cc_util_12m$WOE)
approved_data$trades_12m_woe <- mapvalues(approved_data$trades_12m, from = IV$Tables$trades_12m$trades_12m, to = IV$Tables$trades_12m$WOE)
approved_data$pltrades_12m_woe <- mapvalues(approved_data$pltrades_12m, from = IV$Tables$pltrades_12m$pltrades_12m, to = IV$Tables$pltrades_12m$WOE)
approved_data$inquiries_12m_woe <- mapvalues(approved_data$inquiries_12m, from = IV$Tables$inquiries_12m$inquiries_12m, to = IV$Tables$inquiries_12m$WOE)
approved_data$outstanding_balance_woe <- mapvalues(approved_data$outstanding_balance, from = IV$Tables$outstanding_balance$outstanding_balance, to = IV$Tables$outstanding_balance$WOE)
approved_data$dpd30_6m_woe <- mapvalues(approved_data$dpd30_6m, from = IV$Tables$dpd30_6m$dpd30_6m, to = IV$Tables$dpd30_6m$WOE)
approved_data$pltrades_6m_woe <- mapvalues(approved_data$pltrades_6m, from = IV$Tables$pltrades_6m$pltrades_6m, to = IV$Tables$pltrades_6m$WOE)
approved_data$tot_trades_woe <- mapvalues(approved_data$tot_trades, from = IV$Tables$tot_trades$tot_trades, to = IV$Tables$tot_trades$WOE)
approved_data$dpd30_12m_woe <- mapvalues(approved_data$dpd30_12m, from = IV$Tables$dpd30_12m$dpd30_12m, to = IV$Tables$dpd30_12m$WOE)
approved_data$dpd90_12m_woe <- mapvalues(approved_data$dpd90_12m, from = IV$Tables$dpd90_12m$dpd90_12m, to = IV$Tables$dpd90_12m$WOE)
approved_data$dpd60_6m_woe <- mapvalues(approved_data$dpd60_6m, from = IV$Tables$dpd60_6m$dpd60_6m, to = IV$Tables$dpd60_6m$WOE)
approved_data$inquiries_6m_woe <- mapvalues(approved_data$inquiries_6m, from = IV$Tables$inquiries_6m$inquiries_6m, to = IV$Tables$inquiries_6m$WOE)
approved_data$dpd60_12m_woe <- mapvalues(approved_data$dpd60_12m, from = IV$Tables$dpd60_12m$dpd60_12m, to = IV$Tables$dpd60_12m$WOE)
approved_data$trades_6m_woe <- mapvalues(approved_data$trades_6m, from = IV$Tables$trades_6m$trades_6m, to = IV$Tables$trades_6m$WOE)
approved_data$dpd90_6m_woe <- mapvalues(approved_data$dpd90_6m, from = IV$Tables$dpd90_6m$dpd90_6m, to = IV$Tables$dpd90_6m$WOE)

## Creating a new data frame with WOE data
woe_data <- approved_data[, c(1, 30:56, 29)]
str(woe_data)
## Converting the independent variables into numeric format
woe_data[] <- lapply(woe_data, function(x) as.numeric(as.character(x)))

#converting dependent varibale into factor
woe_data$performance_tag <- as.factor(ifelse(woe_data$performance_tag == 1, "yes", "no"))
str(woe_data)

############# Data with traditional missing value imputation
req_data <- master[,-1]
str(req_data)
req_data <- subset(req_data, !is.na(req_data$performance_tag))

## Calculating the missing value or Na value percentages
sapply(req_data[colSums(is.na(req_data)) > 0], 
       function(x) 
         list(sum(is.na(x)), 
              round(sum(is.na(x)*100/69867), 4)))

sapply(req_data[,which(sapply(req_data, is.character))], 
       function(x) 
         list(length(which(x=="")), 
              round(length(which(x==""))*100/69867, 4)))

## Since all the percentages are less than 1.5%, imputing the missing values by deleting them
req_data$gender <- as.character(req_data$gender)
req_data <- subset(req_data, req_data$gender != "")
req_data$gender <- as.factor(req_data$gender)

req_data$marital_status <- as.character(req_data$marital_status)
req_data <- subset(req_data, req_data$marital_status != "")
req_data$marital_status <- as.factor(req_data$marital_status)

req_data$education <- as.character(req_data$education)
req_data <- subset(req_data, req_data$education != "")
req_data$education <- as.factor(req_data$education)

req_data$profession <- as.character(req_data$profession)
req_data <- subset(req_data, req_data$profession != "")
req_data$profession <- as.factor(req_data$profession)

req_data$residence_type <- as.character(req_data$residence_type)
req_data <- subset(req_data, req_data$residence_type != "")
req_data$residence_type <- as.factor(req_data$residence_type)

req_data$dependents <- as.numeric(req_data$dependents)
req_data <- subset(req_data, !is.na(req_data$dependents))

req_data$cc_util_12m <- as.numeric(req_data$cc_util_12m)
req_data <- subset(req_data, !is.na(req_data$cc_util_12m))

req_data$trades_6m <- as.numeric(req_data$trades_6m)
req_data <- subset(req_data, !is.na(req_data$trades_6m))

req_data$open_hl <- as.numeric(req_data$open_hl)
req_data <- subset(req_data, !is.na(req_data$open_hl))

req_data$outstanding_balance <- as.numeric(req_data$outstanding_balance)
req_data <- subset(req_data, !is.na(req_data$outstanding_balance))

req_data$performance_tag <- (req_data$performance_tag - 1) * (-1)

#converting dependent varibale into factor
req_data$performance_tag <- as.factor(ifelse(req_data$performance_tag == 1, "yes", "no"))

############################# 4.1. Logistic Regression Model ##############################################

## woe_data is the combined data set of demographic and credit bureau data with WOE transformation
## Variables with IV greater than 0.1 and less than 0.5 are considered as medium to strong predictors
## Considering only these variables for logistic regression model on combined data.

## Creating a separate data frame with WOE values of selected variables
lr_data <- woe_data[,c(1, 14:29)]
colnames(lr_data)
str(lr_data)

set.seed(100)
# splitting into train and test data
split_indices_lr <- sample.split(lr_data$performance_tag, SplitRatio = 0.70)
train_lr <- lr_data[split_indices_lr, ]
test_lr <- lr_data[!split_indices_lr, ]

## Finding share of default and non default customers
table(train_lr$performance_tag)[2]/(table(train_lr$performance_tag)[1] + table(train_lr$performance_tag)[2])
## As the default is only 4.2% in the training data, the model will be biased if the training data is not balanced.
## Balancing the training data using SMOTE
train_lr_smote <- SMOTE(performance_tag ~., train_lr, perc.over = 1000, k = 5, perc.under = 200)
table(train_lr_smote$performance_tag)[2]/(table(train_lr_smote$performance_tag)[2] + table(train_lr_smote$performance_tag)[1])
## Default is ~ 35% which is a sign of balanced data 

# Building initial model without application ID parameter
lr_model1 <- glm(performance_tag ~ .-application.id, family = "binomial", data = train_lr_smote)
summary(lr_model1)

# Using stepwise algorithm for removing insignificant variables
lr_model2 <- stepAIC(lr_model1, direction = "both")
lr_model2

# STEP AIC had resulted in below model
lr_model3 <- glm(formula = performance_tag ~ cc_util_12m_woe + trades_12m_woe + pltrades_12m_woe + inquiries_12m_woe + 
                   outstanding_balance_woe + dpd30_6m_woe + pltrades_6m_woe + tot_trades_woe + dpd90_12m_woe + 
                   dpd60_6m_woe + dpd60_12m_woe + dpd90_6m_woe, family = "binomial", data = train_lr_smote)
summary(lr_model3)
vif(lr_model3)
## All variables are having P value < 0.05, but not all are ***.
## dpd60_6m_woe and dpd30_6m_woe are having high VIF(>10). Checking correlation
cor(lr_data$dpd30_6m_woe, lr_data$dpd60_6m_woe)
## High correlation of 0.95. Removing dpd60_6m_woe as it has high P values and VIF between both

lr_model4 <- glm(formula = performance_tag ~ cc_util_12m_woe + trades_12m_woe + pltrades_12m_woe + inquiries_12m_woe + 
                   outstanding_balance_woe + dpd30_6m_woe + pltrades_6m_woe + tot_trades_woe + dpd90_12m_woe + 
                   dpd60_12m_woe + dpd90_6m_woe, family = "binomial", data = train_lr_smote)
summary(lr_model4)
vif(lr_model4)
## Removing dpd90_6m_woe as its P value is highest among all (>0.05) at 0.108

lr_model5 <- glm(formula = performance_tag ~ cc_util_12m_woe + trades_12m_woe + pltrades_12m_woe + inquiries_12m_woe + 
                   outstanding_balance_woe + dpd30_6m_woe + pltrades_6m_woe + tot_trades_woe + dpd90_12m_woe + 
                   dpd60_12m_woe, family = "binomial", data = train_lr_smote)
summary(lr_model5)
## Removing dpd90_12m_woe as its significance is * and P value is highest among all

lr_model6 <- glm(formula = performance_tag ~ cc_util_12m_woe + trades_12m_woe + pltrades_12m_woe + inquiries_12m_woe + 
                   outstanding_balance_woe + dpd30_6m_woe + pltrades_6m_woe + tot_trades_woe + 
                   dpd60_12m_woe, family = "binomial", data = train_lr_smote)
summary(lr_model6)
vif(lr_model6)
## Removing pltrades_12m_woe as its VIF and P value is highest among all

lr_model7 <- glm(formula = performance_tag ~ cc_util_12m_woe + trades_12m_woe + pltrades_6m_woe + inquiries_12m_woe + 
                   outstanding_balance_woe + dpd30_6m_woe + tot_trades_woe + dpd60_12m_woe, family = "binomial", data = train_lr_smote)
summary(lr_model7)
## Removing pltrades_6m_woe as its significance is * and P value is highest among all

lr_model8 <- glm(formula = performance_tag ~ cc_util_12m_woe + trades_12m_woe + tot_trades_woe + inquiries_12m_woe + 
                   outstanding_balance_woe + dpd30_6m_woe + dpd60_12m_woe, family = "binomial", data = train_lr_smote)
summary(lr_model8)
## All the variables are now strongly significant with ***. Hence considering model 8 as final model.

#### Evaluating Logistic regression model on combined data
# Predicting probabilities of responding for the test data
predictions_logit_lr <- predict(lr_model8, newdata = test_lr[, -17], type = "response")
summary(predictions_logit_lr)

test_lr$predicted_probability <- predictions_logit_lr
test_lr <- test_lr[order(test_lr$predicted_probability, decreasing = T),]

# Let's use the probability cutoff of 50%.
predicted_performance_lr <- factor(ifelse(predictions_logit_lr >= 0.50, "yes", "no"))

# Creating confusion matrix for identifying the model evaluation.
conf <- confusionMatrix(predicted_performance_lr, test_lr$performance_tag, positive = "yes")
conf
## Accuracy: 0.7961
## Sensitivity : 0.1776     
## Specificity : 0.8233

## Finding cutoff probability
perform_fn <- function(cutoff) 
{
  predicted_performance_lr <- factor(ifelse(test_lr$predicted_probability >= cutoff, "yes", "no"))
  conf <- confusionMatrix(predicted_performance_lr, test_lr$performance_tag, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Creating cutoff values from 0.01 to 0.99 for plotting and initiallizing a matrix of 1000 X 4.
s = seq(.01,.99,length=100)
OUT = matrix(0,100,3)

for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

# plotting cutoffs 
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,0.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(1,1,1,1),c("Sensitivity","Specificity","Accuracy"))

prob_cutoff_lr <- s[which(abs(OUT[,1]-OUT[,2])<=0.017)]
prob_cutoff_lr
#optimal cutoff probability seems to be around 39.6% which is 0.3960606
predicted_performance_lr <- factor(ifelse(test_lr$predicted_probability >= 0.3960606, "yes", "no"))

test_lr$predict_performance <- predicted_performance_lr
## Creating confusion matrix to check validity of optimal cut off probability
conf_lr <- confusionMatrix(predicted_performance_lr, test_lr$performance_tag, positive = "yes")
acc_lr <- conf_lr$overall[1]
sens_lr <- conf_lr$byClass[1]
spec_lr <- conf_lr$byClass[2]

acc_lr #0.6399
sens_lr #0.6334
spec_lr #0.6402
## At the optimal cut off probability of 0.3960606, Accuracy, Sensitivity and Specificity are in the same range of around 0.63 to 0.64

### KS -statistic - Logistic Regression ######
predicted_performance_lr <- ifelse(predicted_performance_lr == "yes", 1, 0)
actual_performance_lr <- ifelse(test_lr$performance_tag == "yes", 1, 0)
pred_object_lr<- prediction(predicted_performance_lr, actual_performance_lr)
performance_measures_lr<- performance(pred_object_lr, "tpr", "fpr")
ks_table_lr <- attr(performance_measures_lr, "y.values")[[1]] - (attr(performance_measures_lr, "x.values")[[1]])
max(ks_table_lr) #0.2737 (27.37%)

#ROC Curve for Logistic regression
lr_ROCR <- performance(pred_object_lr, measure = "auc")
lr_ROCR <- lr_ROCR@y.values[[1]]
lr_ROCR 
# Area under curve is : 0.6368756

## Creating lift gain chart using logistic regression

actual_performance <- as.factor(ifelse(test_lr$performance_tag=="yes",1,0))
predict_performance <- as.factor(ifelse(test_lr$predict_performance=="yes",1,0))

## writing a function for calculating lift and gain and creating Lift gain table
lift <- function(labels , predicted_prob, groups) {
  
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

LG = lift(actual_performance, predict_performance, groups = 10)
LG 

# Gain chart
plot(LG$bucket,LG$Gain,col="red",type="l",main="Gain Chart")

# Lift chart
plot(LG$bucket,LG$Cumlift,col="red",type="l",main="Lift Chart")
## From the charts and lift, gain table it is evident that by targetting 40% of customers we can eliminated 65% of default customers


######################################## 4.2 Decision Tree Model ################################################

######################################## 4.2.1 Decision Tree with WOE Data ######################################

set.seed(100)

split_indices_dt <- sample.split(woe_data$performance_tag, SplitRatio = 0.70)
train_dt <- woe_data[split_indices_dt, ]
test_dt <- woe_data[!split_indices_dt, ]

## As the default is only 4.4% in the entire data, the model will be biased if the training data is not balanced.
## Balancing the training data using SMOTE
train_dt_smote <- SMOTE(performance_tag ~., train_dt, perc.over = 1000, k = 5, perc.under = 200)

## Model Creation
tree.model <- rpart(performance_tag ~ .-application.id, data = train_dt_smote, method = "class",  
                    control = rpart.control(minsplit = 10, minbucket = 10, cp = 0.01))

# display decision tree
prp(tree.model)

#predictions on the test set
tree.predict <- predict(tree.model, test_dt, type = "class")

# evaluating the results
confusionMatrix(tree.predict, test_dt$performance_tag, positive = "yes")
# Accuracy: 0.9469
# Sensitivity: 0.0226
# Specificity: 0.9875

# setting the number of folds in cross test to 3
tree.control = trainControl(method = "cv", number = 3)

# set the search space for CP
tree.grid = expand.grid(cp = seq(0,0.001,0.0025))

tree.model <- train(performance_tag ~ .-application.id, data = train_dt_smote, method = "rpart", metric = "Accuracy",
                    trControl = tree.control, tuneGrid = tree.grid, control = rpart.control(minsplit = 50, minbucket = 20))

# Cross validated model results
tree.model  ## Accuracy: 0.9319344

# looking at best value of hyperparameter
tree.model$bestTune

# make predictions on test set
tree.predict <- predict.train(tree.model, test_dt)

# accuracy
confusionMatrix(tree.predict, test_dt$performance_tag, positive = "yes")  
## Accuracy : 0.9406
## Sensitivity : 0.0248        
## Specificity : 0.9809

# Finding optimal cut off probability 
tree_pred <- predict(tree.model, test_dt[,-29], type = "prob")
# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_default_dt <- as.factor(ifelse(tree_pred[,2] >= cutoff, "yes","no"))
  conf <- confusionMatrix(predicted_default_dt, test_dt$performance_tag)
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

s = seq(.01,.95,length=100)
OUT = matrix(0,100,3)
for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 


plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
grid(50, lwd = 2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col= "blue",lwd=2)
box()
legend("topright", legend=c("Sensitivity","Specificity","Accuracy"),
       col=c(2,"darkgreen",4,"darkred"), cex=0.5,lwd=c(3,3,3,3), text.font=14,y.intersp = 0.3)

prob_cutoff_dt <- s[which(abs(OUT[,1]-OUT[,2])<=0.06)]
prob_cutoff_dt #0.0669697

test_dt_predicted<- as.factor(ifelse(tree_pred[,2] >= 0.0669697, 1,0))
test_dt_actual<-as.factor(ifelse(test_dt$performance_tag=="yes",1,0))

optimal_conf <- confusionMatrix(test_dt_predicted, test_dt_actual)
acc_dt <- optimal_conf$overall[1]
sens_dt <- optimal_conf$byClass[1]
spec_dt <- optimal_conf$byClass[2]

acc_dt #0.6143
sens_dt #0.6165
spec_dt #0.5633
## At the optimal cut off probability of 0.0669697, Accuracy, Sensitivity and Specificity are in the same range of around 0.56 to 0.61

### KS -statistic - Decision Tree ######
pred_object_dt<- prediction(as.numeric(test_dt_predicted), as.numeric(test_dt_actual))
performance_measures_dt<- performance(pred_object_dt, "tpr", "fpr")
ks_table_dt <- attr(performance_measures_dt, "y.values")[[1]] - (attr(performance_measures_dt, "x.values")[[1]])
max(ks_table_dt) #0.1799055 (17.99%)

#ROC Curve for Decision tree
dt_ROCR <- performance(pred_object_dt, measure = "auc")
dt_ROCR <- dt_ROCR@y.values[[1]]
dt_ROCR 
# Area under curve is : 0.5899527

######################################## 4.2.2 Decision Tree with normal Data ######################################
set.seed(100)
split_indices_dt1 <- sample.split(req_data$performance_tag, SplitRatio = 0.70)
train_dt1 <- req_data[split_indices_dt1, ]
test_dt1 <- req_data[!split_indices_dt1, ]

## As the default is only 4.4% in the entire data, the model will be biased if the training data is not balanced.
## Balancing the training data using SMOTE
train_dt1_smote <- SMOTE(performance_tag ~., train_dt1, perc.over = 1000, k = 5, perc.under = 200)

## Model Creation
tree.model1 <- rpart(performance_tag ~ ., data = train_dt1_smote, method = "class",  
                    control = rpart.control(minsplit = 10, minbucket = 10, cp = 0.01))

# display decision tree
prp(tree.model1)

#predictions on the test set
tree.predict1 <- predict(tree.model1, test_dt1, type = "class")

# evaluating the results
confusionMatrix(tree.predict1, test_dt1$performance_tag, positive = "yes")
# Accuracy: 0.9339
# Sensitivity: 0.9721
# Specificity: 0.0622

# setting the number of folds in cross test to 3
tree.control1 = trainControl(method = "cv", number = 3)

# set the search space for CP
tree.grid1 = expand.grid(cp = seq(0,0.001,0.0025))

tree.model2 <- train(performance_tag ~ ., data = train_dt1_smote, method = "rpart", metric = "Accuracy",
                    trControl = tree.control1, tuneGrid = tree.grid1, control = rpart.control(minsplit = 50, minbucket = 20))

# Cross validated model results
tree.model2  ## Accuracy: 0.9085119

# looking at best value of hyperparameter
tree.model2$bestTune

# make predictions on test set
tree.predict1 <- predict.train(tree.model2, test_dt1)

# accuracy
confusionMatrix(tree.predict1, test_dt1$performance_tag, positive = "yes")  
## Accuracy : 0.9258
## Sensitivity : 0.9641        
## Specificity : 0.0518

# Finding optimal cut off probability 
tree_pred2 <- predict(tree.model2, test_dt1[,-28], type = "prob")
tree_pred2
# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_default_dt2 <- as.factor(ifelse(tree_pred2[,2] >= cutoff, "yes","no"))
  conf <- confusionMatrix(predicted_default_dt2, test_dt1$performance_tag)
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

s = seq(.01,.95,length=100)
OUT = matrix(0,100,3)
for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
grid(50, lwd = 2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col= "blue",lwd=2)
box()
legend("topright", legend=c("Sensitivity","Specificity","Accuracy"),
       col=c(2,"darkgreen",4,"darkred"), cex=0.5,lwd=c(3,3,3,3), text.font=14,y.intersp = 0.3)

prob_cutoff_dt1 <- s[which(abs(OUT[,1]-OUT[,2])<=0.08)]
prob_cutoff_dt1 #0.9215152

test_dt_predicted1<- as.factor(ifelse(tree_pred2[,2] >= 0.9215152, 1,0))
test_dt_actual1<-as.factor(ifelse(test_dt1$performance_tag=="yes",1,0))

optimal_conf <- confusionMatrix(test_dt_predicted1, test_dt_actual1)
acc_dt1 <- optimal_conf$overall[1]
sens_dt1 <- optimal_conf$byClass[1]
spec_dt1 <- optimal_conf$byClass[2]

acc_dt1 #0.5686
sens_dt1 #0.6198
spec_dt1 #0.5664
## At the optimal cut off probability of 0.9215, Accuracy, Sensitivity and Specificity are in the same range of around 0.56 to 0.62

### KS -statistic - Decision Tree ######
pred_object_dt1<- prediction(as.numeric(test_dt_predicted1), as.numeric(test_dt_actual1))
performance_measures_dt1<- performance(pred_object_dt1, "tpr", "fpr")
ks_table_dt1 <- attr(performance_measures_dt1, "y.values")[[1]] - (attr(performance_measures_dt1, "x.values")[[1]])
max(ks_table_dt1) #0.1862 (18.62%)

#ROC Curve for Decision tree
dt_ROCR1 <- performance(pred_object_dt1, measure = "auc")
dt_ROCR1 <- dt_ROCR1@y.values[[1]]
dt_ROCR1 
# Area under curve is : 0.5931255


################################################ 4.3 Random Forest Modelling #######################################################

################### 4.3.1 Random Forest with WOE data #######################################
set.seed(100)

split_indices_rf <- sample.split(woe_data$performance_tag, SplitRatio = 0.70)
train_rf <- woe_data[split_indices_rf, ]
test_rf <- woe_data[!split_indices_rf, ]

## As the default is only 4.4% in the entire data, the model will be biased if the training data is not balanced.
## Balancing the training data using SMOTE
train_rf_smote <- SMOTE(performance_tag ~., train_rf, perc.over = 1000, k = 5, perc.under = 200)

## Model building
data.rf <- randomForest(performance_tag ~ .-application.id, data=train_rf_smote, proximity=FALSE, ntree=50, mtry=5, importance=TRUE, do.trace=TRUE)
summary(data.rf)

test_predict_rf <- predict(data.rf, newdata=test_rf[,-29], type = "prob")
table(test_predict_rf[,-1], test_rf$performance_tag)

## Finding optimal cutoff probability
perform_fn_rf <- function(cutoff) 
{
  predicted_response_rf <- as.factor(ifelse(test_predict_rf[, 2] >= cutoff, "yes", "no"))
  conf <- confusionMatrix(predicted_response_rf, test_rf$performance_tag, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  OUT_rf_demo <- t(as.matrix(c(sens, spec, acc))) 
  colnames(OUT_rf_demo) <- c("sensitivity", "specificity", "accuracy")
  return(OUT_rf_demo)
}

s_demo = seq(.01,.99,length=100)
OUT_rf_demo = matrix(0,100,3)

for(i in 1:100)
{
  OUT_rf_demo[i,] = perform_fn_rf(s_demo[i])
}

# plotting cutoffs
plot(s_demo, OUT_rf_demo[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s_demo,OUT_rf_demo[,2],col="darkgreen",lwd=2)
lines(s_demo,OUT_rf_demo[,3],col=4,lwd=2)
box()

prob_cutoff_rf <- s_demo[which(abs(OUT_rf_demo[,1]-OUT_rf_demo[,2])<0.07)]
prob_cutoff_rf #0.08919192

predicted_perf_rf <- factor(ifelse(test_predict_rf[, 2] >= 0.08919192, "yes", "no"))
conf_rf <- confusionMatrix(predicted_perf_rf, test_rf$performance_tag, positive = "yes")

acc_rf <- conf_rf$overall[1]
sens_rf <- conf_rf$byClass[1] 
spec_rf <- conf_rf$byClass[2]

acc_rf #0.5798187
sens_rf #0.6233032
spec_rf #0.577904
## At the optimal cut off probability of 0.089, Accuracy, Sensitivity and Specificity are in the same range of around 0.57 to 0.625

### KS -statistic - Random Forest 1 ######
predicted_performance_rf <- ifelse(predicted_perf_rf =="yes",1,0)
actual_performance_rf <- ifelse(test_rf$performance_tag=="yes",1,0)

pred_performance_test_rf<- prediction(predicted_performance_rf, actual_performance_rf)
performance_measures_rf<- performance(pred_performance_test_rf, "tpr", "fpr")
ks_table_rf <- attr(performance_measures_rf, "y.values")[[1]] - (attr(performance_measures_rf, "x.values")[[1]])
max(ks_table_rf) #0.2012071 (20.12%)

#Area under ROC Curve for Random forest 1
rf_ROCR <- performance(pred_performance_test_rf, measure = "auc")
rf_ROCR <- rf_ROCR@y.values[[1]]
rf_ROCR 
# Area under curve is : 0.6006036

## Checking the important variables
importance <- data.rf$importance 
importance <- data.frame(importance)
importance


################### 4.3.2 Random Forest with normal data #######################################

set.seed(100)

split_indices_rf2 <- sample.split(req_data$performance_tag, SplitRatio = 0.70)
train_rf2 <- req_data[split_indices_rf2, ]
test_rf2 <- req_data[!split_indices_rf2, ]

## As the default is only 4.4% in the entire data, the model will be biased if the training data is not balanced.
## Balancing the training data using SMOTE
train_rf2_smote <- SMOTE(performance_tag ~., train_rf2, perc.over = 1000, k = 5, perc.under = 200)

## Model building
data.rf2 <- randomForest(performance_tag ~ ., data=train_rf2_smote, proximity=FALSE, ntree=50, mtry=5, importance=TRUE, do.trace=TRUE)
summary(data.rf2)

test_predict_rf2 <- predict(data.rf2, newdata=test_rf2[,-28], type = "prob")
table(test_predict_rf2[,-1], test_rf2$performance_tag)

## Finding optimal cutoff probability
perform_fn_rf <- function(cutoff) 
{
  predicted_response_rf2 <- as.factor(ifelse(test_predict_rf2[, 2] >= cutoff, "yes", "no"))
  conf <- confusionMatrix(predicted_response_rf2, test_rf2$performance_tag, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  OUT_rf_demo <- t(as.matrix(c(sens, spec, acc))) 
  colnames(OUT_rf_demo) <- c("sensitivity", "specificity", "accuracy")
  return(OUT_rf_demo)
}

s_demo = seq(.01,.99,length=100)
OUT_rf_demo = matrix(0,100,3)

for(i in 1:100)
{
  OUT_rf_demo[i,] = perform_fn_rf(s_demo[i])
}

# plotting cutoffs
plot(s_demo, OUT_rf_demo[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s_demo,OUT_rf_demo[,2],col="darkgreen",lwd=2)
lines(s_demo,OUT_rf_demo[,3],col=4,lwd=2)
box()

prob_cutoff_rf2 <- s_demo[which(abs(OUT_rf_demo[,1]-OUT_rf_demo[,2])<0.025)]
prob_cutoff_rf2 #0.8613131

predicted_perf_rf2 <- factor(ifelse(test_predict_rf2[, 2] >= 0.8613131, "yes", "no"))
conf_rf2 <- confusionMatrix(predicted_perf_rf2, test_rf2$performance_tag, positive = "yes")

acc_rf2 <- conf_rf2$overall[1]
sens_rf2 <- conf_rf2$byClass[1] 
spec_rf2 <- conf_rf2$byClass[2]

acc_rf2 #0.6036
sens_rf2 #0.6038
spec_rf2 #0.5979
## At the optimal cut off probability of 0.8613131, Accuracy, Sensitivity and Specificity are in the same range of around 0.59 to 0.60

### KS -statistic - Random Forest 1 ######
predicted_performance_rf2 <- ifelse(predicted_perf_rf2 =="yes",1,0)
actual_performance_rf2 <- ifelse(test_rf2$performance_tag=="yes",1,0)

pred_performance_test_rf2<- prediction(predicted_performance_rf2, actual_performance_rf2)
performance_measures_rf2<- performance(pred_performance_test_rf2, "tpr", "fpr")
ks_table_rf2 <- attr(performance_measures_rf2, "y.values")[[1]] - (attr(performance_measures_rf2, "x.values")[[1]])
max(ks_table_rf2) #0.2017964 (20.17%)

#Area under ROC Curve for Random forest 1
rf2_ROCR <- performance(pred_performance_test_rf2, measure = "auc")
rf2_ROCR <- rf2_ROCR@y.values[[1]]
rf2_ROCR 
# Area under curve is : 0.6008982


############################################### 4.4 SVM Model #######################################################
set.seed(100)
split_indices_svm <- sample.split(woe_data$performance_tag, SplitRatio = 0.70)
train_svm <- woe_data[split_indices_svm, ]
test_svm <- woe_data[!split_indices_svm, ]

## As the default is only 4.4% in the entire data, the model will be biased if the training data is not balanced.
## Balancing the training data using SMOTE
train_svm_smote <- SMOTE(performance_tag ~., train_svm, perc.over = 1100, k = 5, perc.under = 110)
str(train_svm_smote)
## Snce there are approx 50k rows of data with 28 columns, it is taking lot of time and system is crashing while doing SVM modelling.
## To over come this using only 20% of SMOTE balanced train data
train_indices = sample(1:nrow(train_svm_smote), 0.2*nrow(train_svm_smote))
train_svm1 = train_svm_smote[train_indices,]
table(train_svm1$performance_tag)

################################################ 4.4.a Linear Kernel Model ################################################
no_cores <- detectCores() - 2
cl <- makePSOCKcluster(no_cores)
registerDoParallel(cl) ## parallel processing initiation
## Using linear kernel with default parameters
set.seed(100)
LM_1 <- ksvm(performance_tag~ .-application.id, data = train_svm1, scale = FALSE, kernel = "vanilladot")
LM_1 ##Default C = 1
Eval_LM1 <- predict(LM_1, test_svm) ## Predicting the model using test data
cfm1 <- confusionMatrix(Eval_LM1,test_svm$performance_tag) ## Confusion matrix of predicted vs actual test target variable
cfm1
###  Accuracy: 0.5502   Sensitivity: 0.734   Specificity: 0.542

## Using Hyperparameter tuning and Cross Validation to arrive at an optimal value of C and best model
trainControl_lm <- trainControl(method="cv", number=3)
metric <- "Accuracy"
## grid_lm <- expand.grid(C=c(0.1, 0.5, 1, 2, 3)) 
grid_lm <- expand.grid(C=c(0.1, 1, 5, 10, 15))
##grid_lm <- expand.grid(C=c(10, 15, 25, 50))

## Performing 3 fold cross validation as the system resource are getting utilized at 100% and laptop is crashing
set.seed(100)
fit.lm <- train(performance_tag~.-application.id, data=train_svm1, method="svmLinear", metric=metric, tuneGrid=grid_lm, trControl=trainControl_lm, allowParallel = TRUE)
print(fit.lm) 
plot(fit.lm)
## Highest accuracy of 0.6272 is acheived at C = 15, which will be considered as optimal value and best fit linear model

##Evaluating with test data
eval_fitlm <- predict(fit.lm, test_svm)##Predicting using test data
cfm_fitlm <- confusionMatrix(eval_fitlm,test_svm$performance_tag) ## Confusion matrix of predicted vs actual test target variable
cfm_fitlm
###  Accuracy: 0.5468   Sensitivity: 0.7443   Specificity: 0.5380

################################################ 4.4.b Radial Kernel Model ################################################
#Using RBF Kernel with default parameters
set.seed(100)
RBF1 <- ksvm(performance_tag~ .-application.id, data = train_svm1, scale = FALSE, kernel = "rbfdot")
RBF1 #Default hyper parameters: C = 1, Sigma = 0.028
Eval_RBF1<- predict(RBF1, test_svm)## Predicting the model using test data
cfm_RBF1 <- confusionMatrix(Eval_RBF1,test_svm$performance_tag) ## Confusion matrix of predicted vs actual test target variable
cfm_RBF1
###  Accuracy: 0.6588   Sensitivity: 0.526   Specificity: 0.6646

## Using Hyperparameter tuning and Cross Validation to arrive at an optimal value of C, Sigma and best model
### Tried Performing 5 fold cross validation with 4 sigma values and 5 C values. 
### However, evenafter running for 12 hrs model output is not acheived and system ran into hang state
### To optimize the model execution time, trying 3 fold cross validation
set.seed(100)
trainControl_RBF <- trainControl(method="cv", number=3, verboseIter = TRUE) ##verboseIter is used to print logs of training model
metric <- "Accuracy"
grid_RBF <- expand.grid(.sigma=c(0.05, 0.5, 1, 2), .C=c(1,2,3))
fit.RBF <- train(performance_tag~.-application.id, data=train_svm1, method="svmRadial", metric=metric, tuneGrid=grid_RBF, trControl=trainControl_RBF, allowParallel = TRUE)
print(fit.RBF) 
plot(fit.RBF)
## Highest accuracy of 0.874 was acheived when Sigma = 0.5 and C = 3 as per model outcome

##Evaluating with test data
Eval_fitRBF <- predict(fit.RBF, test_svm)
cfm_fitRBF <- confusionMatrix(Eval_fitRBF,test_svm$performance_tag) ##Created confusion matrix on test data
cfm_fitRBF
###  Accuracy: 0.8792   Sensitivity: 0.1119   Specificity: 0.9129

################################################ 4.4.C Polynomial Kernel Model ################################################

#Using Polynomial Kernel with default parameters
set.seed(100)
Poly1 <- ksvm(performance_tag~ .-application.id, data = train_svm1, scale = FALSE, kernel = "polydot")
Poly1 ### Default hyperparameters  C=1 Degree = 1, Scale = 1
Eval_Poly1<- predict(Poly1, test_svm)## Predicting the model using test data
cfm_Poly1 <- confusionMatrix(Eval_Poly1,test_svm$performance_tag) ## Confusion matrix of predicted vs actual test target variable
cfm_Poly1
###  Accuracy: 0.5502   Sensitivity: 0.7341   Specificity: 0.54214
## Using Hyperparameter tuning and Cross Validation to arrive at an optimal value of C, Sigma and best model
### To optimize the model execution time, trying 3 fold cross validation
set.seed(100)
trainControl_Poly <- trainControl(method="cv", number=3, verboseIter = TRUE) ##verboseIter is used to print logs of training model
metric <- "Accuracy"
grid_Poly <- expand.grid(C=c(0.01, 0.1, 1, 2), degree=c(1,2), scale = c(1, 5, 10))
fit.Poly <- train(performance_tag~.-application.id, data=train_svm1, method="svmPoly", metric=metric, tuneGrid=grid_Poly, trControl=trainControl_Poly, allowParallel = TRUE)
print(fit.Poly) 
plot(fit.Poly)
## Highest accuracy of 0.725 was acheived when C = 1, Degree = 2 and Scale = 5 as per model outcome
##Evaluating with test data
Eval_fitPoly <- predict(fit.Poly, test_svm)
cfm_fitPoly <- confusionMatrix(Eval_fitPoly,test_svm$performance_tag) ##Created confusion matrix on test data
cfm_fitPoly
###  Accuracy: 0.6612   Sensitivity: 0.4343   Specificity: 0.6711
### Accuracy when cheked on test data is slighlty marginally higher compared to train data. This model doest seem to overfit model
### In Polynomial Kernel model, model with Degree of 2, C of 0.01 and Scale =1 is best fit as per cross validation followed.
stopCluster(cl)

## Out of all the three SVM models, Radial has high accuracy but has a very low sensitivity if 0.11
## Between Linear and Polynomial the diff between accuracy, sensitivity and specificity is low for Linear SVM
# It is very hard to conclude anything from svm model as the the accuracy ,specificity and sensitivity are not consistent
# Also, as we modelled using a small dataset of 20% due to respource crunch we are not considering SVM.

############################################ 5. Analyzing performance of all models ##############################################

## Logistic Regression
acc_lr  #0.64
sens_lr #0.633
spec_lr #0.64
max(ks_table_lr) #0.2737
lr_ROCR #0.6368

## Decision Tree with woe data
acc_dt  #0.614
sens_dt #0.616
spec_dt #0.563
max(ks_table_dt) #0.1799
dt_ROCR #0.5899

## Decision Tree with normal data
acc_dt1  #0.5686
sens_dt1 #0.6198
spec_dt1 #0.5664
max(ks_table_dt1) #0.1862
dt_ROCR1 #0.5931

## Random forest with WOE data and all parameters
acc_rf  #0.5798
sens_rf #0.6233
spec_rf #0.5779
max(ks_table_rf) #0.2012
rf_ROCR #0.6006

## Random forest with Normal data
acc_rf2  #0.6036
sens_rf2 #0.6038
spec_rf2 #0.5979
max(ks_table_rf2) #0.2017
rf2_ROCR #0.6008

## By Comparing the Performance parameters of all the models it is evident that Logistic regression model is having High accuracy, sensitivity, specificity, KS Statistic and area under ROCR curve
## Hence, we will select Logistic regression as our model for analysis.


############################################# 6. Application score card ###################################################
str(lr_data)
lr_data$predict_default <- predict(lr_model8, newdata = lr_data[, -17], type = "response")
lr_data$predict_nondefault <- 1 - lr_data$predict_default
lr_data$odds <-  log(lr_data$predict_nondefault/lr_data$predict_default)
factor = 20/log(2)
offset = 400 - (factor*log(10))
lr_data$score = offset + (factor * lr_data$odds)

## Cut off score is equivalent to the score at cut off probability. Calculating the same
cutoff_default <- prob_cutoff_lr
cutoff_nondefault <- 1 - cutoff_default
cutoff_odds <- log(cutoff_nondefault/cutoff_default)
cutoff_score <- offset + (factor * cutoff_odds)
cutoff_score ##345.7351

## Calculating Application scores for rejected data
## First doing WOE transformation for rejected data as the model selected is based on WOE data.
reject_data$cc_util_12m_woe <- mapvalues(reject_data$cc_util_12m, from = IV$Tables$cc_util_12m$cc_util_12m, to = IV$Tables$cc_util_12m$WOE)
reject_data$trades_12m_woe <- mapvalues(reject_data$trades_12m, from = IV$Tables$trades_12m$trades_12m, to = IV$Tables$trades_12m$WOE)
reject_data$pltrades_12m_woe <- mapvalues(reject_data$pltrades_12m, from = IV$Tables$pltrades_12m$pltrades_12m, to = IV$Tables$pltrades_12m$WOE)
reject_data$inquiries_12m_woe <- mapvalues(reject_data$inquiries_12m, from = IV$Tables$inquiries_12m$inquiries_12m, to = IV$Tables$inquiries_12m$WOE)
reject_data$outstanding_balance_woe <- mapvalues(reject_data$outstanding_balance, from = IV$Tables$outstanding_balance$outstanding_balance, to = IV$Tables$outstanding_balance$WOE)
reject_data$dpd30_6m_woe <- mapvalues(reject_data$dpd30_6m, from = IV$Tables$dpd30_6m$dpd30_6m, to = IV$Tables$dpd30_6m$WOE)
reject_data$pltrades_6m_woe <- mapvalues(reject_data$pltrades_6m, from = IV$Tables$pltrades_6m$pltrades_6m, to = IV$Tables$pltrades_6m$WOE)
reject_data$tot_trades_woe <- mapvalues(reject_data$tot_trades, from = IV$Tables$tot_trades$tot_trades, to = IV$Tables$tot_trades$WOE)
reject_data$dpd30_12m_woe <- mapvalues(reject_data$dpd30_12m, from = IV$Tables$dpd30_12m$dpd30_12m, to = IV$Tables$dpd30_12m$WOE)
reject_data$dpd90_12m_woe <- mapvalues(reject_data$dpd90_12m, from = IV$Tables$dpd90_12m$dpd90_12m, to = IV$Tables$dpd90_12m$WOE)
reject_data$dpd60_6m_woe <- mapvalues(reject_data$dpd60_6m, from = IV$Tables$dpd60_6m$dpd60_6m, to = IV$Tables$dpd60_6m$WOE)
reject_data$inquiries_6m_woe <- mapvalues(reject_data$inquiries_6m, from = IV$Tables$inquiries_6m$inquiries_6m, to = IV$Tables$inquiries_6m$WOE)
reject_data$dpd60_12m_woe <- mapvalues(reject_data$dpd60_12m, from = IV$Tables$dpd60_12m$dpd60_12m, to = IV$Tables$dpd60_12m$WOE)
reject_data$trades_6m_woe <- mapvalues(reject_data$trades_6m, from = IV$Tables$trades_6m$trades_6m, to = IV$Tables$trades_6m$WOE)
reject_data$dpd90_6m_woe <- mapvalues(reject_data$dpd90_6m, from = IV$Tables$dpd90_6m$dpd90_6m, to = IV$Tables$dpd90_6m$WOE)

lr_reject_data <- reject_data[,c(30:44, 29)]
lr_reject_data[] <- lapply(lr_reject_data, function(x) as.numeric(as.character(x)))

lr_reject_data$predict_default <- predict(lr_model8, newdata = lr_reject_data[, -16], type = "response")
lr_reject_data$predict_non_default <- 1 - lr_reject_data$predict_default
lr_reject_data$odds <-  log(lr_reject_data$predict_non_default/lr_reject_data$predict_default)
lr_reject_data$score = offset + (factor * lr_reject_data$odds)

lr_reject_data$predicted_performance <- factor(ifelse(lr_reject_data$score > cutoff_score, "non-default", "default"))
table(lr_reject_data$predicted_performance)
## default    non-default 
##  1385          40 

table(lr_reject_data$predicted_performance)[2]/nrow(lr_reject_data)
##97% correctly identified by Model


## Comparision of score in approved data and rejected data
summary(lr_data$score)
## Min score in approved data : 321.8
## Mean score in approved data: 356.5
## Max score in approved data : 389.7

summary(lr_reject_data$score)
## Min score in rejected data : 322.5
## Mean score in rejected data: 331.9
## Max score in approved data : 358.2

boxplot(lr_data$score, main = "Approved Data Scores", col = "lightgrey", boxwex =0.5)
boxplot(lr_reject_data$score, main = "Rejected Data Scores", col = "lightgrey", boxwex =0.5)

################################### 7. Assessing financial benefits ########################################
str(lr_data)

## From original approved data without model
lr_data$performance_tag <- as.factor(ifelse(lr_data$performance_tag == "yes", "default", "non-default"))
lr_data$predicted_performance <- as.factor(ifelse(lr_data$score <= cutoff_score, "default", "non-default"))
conf_original <- confusionMatrix(lr_data$predicted_performance, lr_data$performance_tag, positive = "non-default")
conf_original
##                Reference
## Prediction    default non-default
##  default        1805       23969
##  non-default    1142       42951

table(lr_data$predicted_performance)
## default    non-default 
##  25774       44093
auto_accept_application = table(lr_data$predicted_performance)[2]/(table(lr_data$predicted_performance)[2] + table(lr_data$predicted_performance)[1])
## 63.1%

# Assumptions:
## Avg credit loss per default : 1500$
## Acquisition cost per customer : 5 $
## Avg yearly revenue from non default customer: 100$
## Yearly profit = Revenue - total acquisition cost - total credit loss

cost_nomodel = 5 * 69867 ## 3,49,335
credit_loss_nomodel = 1500 * 2947 ##44,20,500
revenue_nomodel = 100 * 66920 ## 66,92,000
profit_nomodel = revenue_nomodel - cost_nomodel - credit_loss_nomodel ## 19,22,165


cost_model = 5 * 44093  ##2,20,465
credit_loss_model = 1500 * 1142 ## 17,13,000
revenue_model = 100 * 42951 ##42,95,100
profit_model = revenue_model - credit_loss_model - cost_model ## 23,61,635

credit_loss_saved = credit_loss_nomodel - credit_loss_model ## 27,07,500
revenue_loss = revenue_nomodel - revenue_model ## 23,96,900
profit_gain = 100*(profit_model - profit_nomodel)/profit_nomodel ## 22.86%
