setwd("I:/EDA case study")
loan_data <- read.csv("loan.csv", stringsAsFactors = FALSE)


library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)
library(readr)
library(lubridate)
library(scales)

summary(loan_data)
str(loan_data)

#Data Cleaning

#most of the columns are NA's so getting rid of of those columns which have values NA's
na_columns <- loan_data %>%
  summarise_all(funs(sum(is.na(.))/n())) 
#finding the percentage of NA's in each column and creating a vector to know hom many columns are required for analysis

na_columns <- gather(na_columns,key='col_name',value = 'na_percentage')
required_columns <- data.frame(filter(na_columns,na_percentage<0.05))
#filtering the columns based on their na_prcentage.(Assumption) Columns containing less than 5% of NA's are the required columns

required_columns <- (required_columns$col_name)
loan_data_modified <- loan_data[,(colnames(loan_data) %in% required_columns)]

#There are som columns which are mostly valued to be ZEROES.
#Some columns like "policy_code", "initial_list_status" and "pymnt_plan" have same values in all observations.
#There are some columns like "id","member_id", "url", "description","employ_title","zip_code","addr_state","title" which are not useful for analysis 
#Getting rid of these columns which are not required for analysis point of view

unrequired_columns <- c("id","member_id","emp_title","url","desc","title","zip_code","addr_state","pymnt_plan","acc_now_delinq","chargeoff_within_12_mths","delinq_amnt","pub_rec_bankruptcies","tax_liens","pymnt_plan","initial_list_status","collections_12_mths_ex_med","policy_code")
loan_data_modified <- loan_data_modified[,!(colnames(loan_data_modified) %in% unrequired_columns)]

#Variables are different types in remaining data
#Applicants details
#Loan details
#Performance of applicant after loan is issued

#Problem statement clearly states that we should identify risky applicants at the time of application
#So applicant behaviour cannot be the part of analysis because performance related variables will not be available at the time of application

applicant_performance_variables <- c("delinq_2yrs","earliest_cr_line","inq_last_6mths","open_acc","pub_rec","revol_bal","revol_util","total_acc","out_prncp","out_prncp_inv","total_pymnt","total_pymnt_inv","total_rec_prncp","total_rec_int","total_rec_late_fee","recoveries","collection_recovery_fee","last_pymnt_d","last_pymnt_amnt","next_pymnt_d","last_credit_pull_d")
loan_data_modified <- loan_data_modified[,!(colnames(loan_data_modified) %in% applicant_performance_variables)]


#extracting numeric values from term,int_rate,emp_length for easy way of analysis
loan_data_modified$term <- parse_number(loan_data_modified$term)
loan_data_modified$int_rate <- parse_number(loan_data_modified$int_rate)
loan_data_modified$emp_length <- parse_number(loan_data_modified$emp_length)
#There are NA's in emp_length lets consider them for analysis to check whether it will be affected to find risky applicant.

#changing issue date in required format
loan_data_modified$issue_d <- paste("01-",loan_data_modified$issue_d,sep="")
loan_data_modified$issue_d <- as.Date(loan_data_modified$issue_d,"%d-%B-%y")
#extracting year from issue date
loan_data_modified$year <- as.factor(format(loan_data_modified$issue_d,"%Y"))

#Defaulted applicants are charged-off and non-defaulted are Fully paid
#So removing current from status column and assigning binary values for charged of and fully paid for ease of analysis

current <- filter(loan_data_modified,loan_status %in% c("Current"))
current$loan_status <- factor(current$loan_status)

#Assigning level "1" for charged off and "0" for fully paid
loan_data_modified$loan_status <- ifelse(loan_data_modified$loan_status=="Charged Off",1,0)

#Univariate analysis
#Lets find what is total default rate
mean(loan_data_modified$loan_status)
#14.16% of defualt rate is observed
ggplot(loan_data_modified, aes(x = as.factor(loan_data_modified$loan_status))) + geom_bar(aes(y = (..count..)/sum(..count..)))                                                                                                                            

#Lets see year wise distribution of loans
ggplot(loan_data_modified, aes(x = as.factor(loan_data_modified$year))) + geom_bar(aes(y = (..count..)/sum(..count..))) + geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) + scale_y_continuous(labels = percent) + labs(y = "Percent")                                                                                                                           
#From 2010 & 2011 applicants have been increased

#Lets see purpose wise loans 
ggplot(loan_data_modified, aes(x = as.factor(loan_data_modified$purpose))) + geom_bar(aes(y = (..count..)/sum(..count..))) + geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) + scale_y_continuous(labels = percent) + labs(y = "Percent")
#47% loans are Debt consolidation
#13% loans are credit card
#10% loans are other
#7.5% loans are for home improvement
#and so on...

#Lets see purpose wise defaulters in the dataset
ggplot(loan_data_modified, aes(x = as.factor(loan_data_modified$purpose))) + geom_bar(aes(y = (..count..)/sum(..count..))) + geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) + scale_y_continuous(labels = percent) + labs(y = "Percent") + theme(axis.text.y=element_blank(), axis.ticks=element_blank(),axis.title.y=element_blank(),axis.text.x = element_text(angle = 90, hjust = 1)) + facet_wrap(~loan_status)
#Plot clearly shows that the 5 variables affect the company most, because in 14% of total defaulters 11.5% are from these 5 variables
#Lets Filter out these 5 variables and do further analysis

loan_data_modified <- filter(loan_data_modified,purpose %in% c("credit_card","debt_consolidation","home_improvement","small_business","other"))

#Lets see default rate with respect to each variable
ggplot(loan_data_modified, aes(x = as.factor(loan_data_modified$emp_length))) + geom_bar(aes(y = (..count..)/sum(..count..))) + geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) + facet_wrap(~loan_status)
#Plot shows that employers with less than 1 year and more than 10 years experience are more risky applicants when compared to others

ggplot(loan_data_modified, aes(x = as.factor(loan_data_modified$home_ownership))) + geom_bar(aes(y = (..count..)/sum(..count..))) + geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) + facet_wrap(~loan_status)
#Plot shows Rented applicants and mortgage applicants are more risky applicants when compared to own House applicants

ggplot(loan_data_modified, aes(x = as.factor(loan_data_modified$term))) + geom_bar(aes(y = (..count..)/sum(..count..))) + geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) + facet_wrap(~loan_status)
#This plot clearly shows that total 14.6% of defaulters are choosing 36months term.

ggplot(loan_data_modified, aes(x = as.factor(loan_data_modified$verification_status))) + geom_bar(aes(y = (..count..)/sum(..count..))) + geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) + facet_wrap(~loan_status)
#This plot shows that Not verified applicants are risky applicants

#Till now we have considered only applicants variables and now in univarate analysis we will consider Loan variables
#Loan variables are numeric and continous variables better we bin them according to quantile basis.
#Employ annual incme is also a numeric variable which is continuos we will try bin it too
#These variables are derived metric not only for univariate analysis but also for bivariate analysis. 

quantile(loan_data_modified$loan_amnt)
#25th - 6000
#50th - 10000
#75th - 16000
#100th - 35000
loan_data_modified$loan_amnt_categories <- ifelse(loan_data_modified$loan_amnt<=6000,"Less_loan_amnt",
                                                  ifelse(loan_data_modified$loan_amnt>6000 & loan_data_modified$loan_amnt<=10000,"Average_loan_amnt",
                                                         ifelse(loan_data_modified$loan_amnt>10000 & loan_data_modified$loan_amnt<=16000,"High_loan_amnt","Very_high_loan_amnt")))

quantile(loan_data_modified$funded_amnt)
#25th - 6000
#50th - 10000
#75th - 15000
#100th - 35000
loan_data_modified$funded_amnt_categories <- ifelse(loan_data_modified$funded_amnt<=6000,"Less_funded_amnt",
                                                    ifelse(loan_data_modified$funded_amnt>6000 & loan_data_modified$funded_amnt<=15000,"Average_funded_amnt",
                                                           ifelse(loan_data_modified$funded_amnt>15000 & loan_data_modified$funded_amnt<=25000,"High_funded_amnt","Very_high_funded_amnt")))

quantile(loan_data_modified$funded_amnt_inv)
#25th - 6000
#50th - 10000
#75th - 15000
#100th - 35000
loan_data_modified$funded_amnt_inv_categories <- ifelse(loan_data_modified$funded_amnt<=5500,"Less_funded_amnt_inv",
                                                        ifelse(loan_data_modified$funded_amnt>5500 & loan_data_modified$funded_amnt<=15000,"Average_funded_amnt_inv",
                                                               ifelse(loan_data_modified$funded_amnt>15000 & loan_data_modified$funded_amnt<=25000,"High_funded_amnt_inv","Very_high_funded_amnt_inv")))
quantile(loan_data_modified$int_rate)
#25th - 9.63%
#50th - 11.99%
#75th - 14.74
#100th - 24.59%
loan_data_modified$int_rate_categories <- ifelse(loan_data_modified$int_rate<= 9.63,"Less_int_rate",
                                                 ifelse(loan_data_modified$int_rate>9.63 & loan_data_modified$int_rate<=11.99,"Average_int_rate",
                                                        ifelse(loan_data_modified$int_rate>11.99 & loan_data_modified$int_rate<=14.74,"High_int_rate","Very_high_int_rate")))

quantile(loan_data_modified$installment)
#25th - 185.64
#50th - 307.04
#75th - 461.34
#100th - 1305.19
loan_data_modified$installment_categories <- ifelse(loan_data_modified$installment<= 185.64,"Less_installment",
                                                    ifelse(loan_data_modified$installment>185.64 & loan_data_modified$installment<=307.04,"Average_installment",
                                                           ifelse(loan_data_modified$installment>307.04 & loan_data_modified$installment<=461.34,"High_installment","Very_high_installment")))
quantile(loan_data_modified$annual_inc)
#25th - 42000
#50th - 60000
#75th - 83717
#100th - 6000000
#As we see there is an outlier,lets confirm this by plotting annual income with respect to loan_status

library(psych)
ggplot(loan_data_modified, aes(x = loan_data_modified$annual_inc)) + geom_histogram() + facet_wrap(~loan_status)
#This clearly shows that there might some observation but due to outliers they are not visible in the plot
#Used histogram because its a continous variable
summary(loan_data_modified$annual_inc)
#95th percentile is 1,44,000 and 100th percentile is 6000000 that is almost a big difference
#Taking 95th percentile for analysis 
loan_data_modified$annual_inc_categories<-ifelse(loan_data_modified$annual_inc<=42000,"Less_income",
                                                 ifelse(loan_data_modified$annual_inc>42000 & loan_data_modified$annual_inc<=69784,"Average_income",
                                                        ifelse(loan_data_modified$annual_inc>69784 & loan_data_modified$annual_inc<=144000,"High_income","Very_high_income")))

quantile(loan_data_modified$dti)
#25th - 8.75
#50th - 13.88
#75th - 18.93
#100th - 29.99
loan_data_modified$debt_to_inc_categories <- ifelse(loan_data_modified$dti<= 8.75,"Less_installment",
                                                    ifelse(loan_data_modified$dti>8.75 & loan_data_modified$dti<=13.88,"Average_installment",
                                                           ifelse(loan_data_modified$dti>13.88 & loan_data_modified$dti<=18.93,"High_installment","Very_high_installment")))

#Continuing Univariate analysis with respect to derived categorical metrics with loan status

ggplot(loan_data_modified, aes(x = as.factor(loan_data_modified$loan_amnt_categories))) + geom_bar(aes(y = (..count..)/sum(..count..))) + geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) + facet_wrap(~loan_status)
#Average_loan_amnt applicants and less_loan_amnt applicants are more defaulters

ggplot(loan_data_modified, aes(x = as.factor(loan_data_modified$int_rate_categories))) + geom_bar(aes(y = (..count..)/sum(..count..))) + geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) + facet_wrap(~loan_status)
#High int_rate applicants are more defaulters 

ggplot(loan_data_modified, aes(x = as.factor(loan_data_modified$installment_categories))) + geom_bar(aes(y = (..count..)/sum(..count..))) + geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) + facet_wrap(~loan_status)
#There is equal distribution with installment.

ggplot(loan_data_modified, aes(x = as.factor(loan_data_modified$annual_inc_categories))) + geom_bar(aes(y = (..count..)/sum(..count..))) + geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) + facet_wrap(~loan_status)
#This plot states that Average income and less income applicants are more risky applicants
#This plot also follows the trend loan_amnt that means these two are correlated

ggplot(loan_data_modified, aes(x = as.factor(loan_data_modified$debt_to_inc_categories))) + geom_bar(aes(y = (..count..)/sum(..count..))) + geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) + facet_wrap(~loan_status)
#This plot is obvious that as it follows the trend of loan_amnt and annual_inc

#Key points observed in univariate analysis done so far
# 1. Observed 5 purposes which are highly applied and defaulted
# 2. Employee length has a clear trend showing decreasing trend from 1 year experience and sudden increase in 10 year experience
# 3. Applicants who stay in rent and mortgages are more risky applicants
# 4. Applicants who opt for 60months term are not at all risky (Interestinng Observation)
# 5. There is correlation between loan_amnt and annual income
# 6. Loan_amnt requested by applicant has been decided on the basis of their income
# 7. Based on purpose of the loan how variables will efect the percentage of defaulters is unknown
# 8. As most of the applicants approach for debt_consolidation and credit loss which contribute over 60%

#Bivariate analysis
#Lets see how all the variables affect each purpose with respect to loan status
#lets create subsets from loan data modified for each purpose for better understanding

#1. Debt consolidation
Debt_consolidation <- subset(loan_data_modified, loan_data_modified$purpose == "debt_consolidation")

ggplot(Debt_consolidation, aes(x = as.factor(Debt_consolidation$loan_amnt_categories))) + geom_bar(aes(y = (..count..)/sum(..count..))) + geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) + facet_wrap(~loan_status)
#Default rate for each category(default_count/total_count)
#Average loan_amnt - 18%
#High loan_amnt - 15%
#Less loan_amnt - 17%
#Very high_amnt - 10%
#So its clear that Average loan amnt and less amnt contributing factors

ggplot(Debt_consolidation, aes(x = as.factor(Debt_consolidation$term))) + geom_bar(aes(y = (..count..)/sum(..count..))) + geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) + facet_wrap(~loan_status)
#As we already seen that 36 months term have more default rate 

ggplot(Debt_consolidation, aes(x = as.factor(Debt_consolidation$int_rate_categories))) + geom_bar(aes(y = (..count..)/sum(..count..))) + geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) + facet_wrap(~loan_status)
#Default rate for each category(default_count/total_count)
#Average_int_rate - 15%
#High_int_rate - 26%
#less_int_rate - 11%
#Very_high_int_rate - 7%
#Interestingly High_int_rate is very much high this is because total applicants are less and high defaulters are present

ggplot(Debt_consolidation, aes(x = as.factor(Debt_consolidation$year))) + geom_bar(aes(y = (..count..)/sum(..count..))) + geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) + facet_wrap(~loan_status)
#2009 was worst year for debt consolidation

ggplot(Debt_consolidation, aes(x = as.factor(Debt_consolidation$installment_categories))) + geom_bar(aes(y = (..count..)/sum(..count..))) + geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) + facet_wrap(~loan_status)
#Default rate for each category(default_count/total_count)
#Average_installment - 15%
#High_installment - 14%
#less_installment - 14.5%
#Very_high_installment - 15.5%
#It shows that they are equally distributed

ggplot(Debt_consolidation, aes(x = as.factor(Debt_consolidation$emp_length))) + geom_bar(aes(y = (..count..)/sum(..count..))) + geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) + facet_wrap(~loan_status)
#Plot shows that less than 1 year experience , 2 year experience are more defaulters 
#55.5% of defaulters are less than 3 years experienced applicants

ggplot(Debt_consolidation, aes(x = as.factor(Debt_consolidation$home_ownership))) + geom_bar(aes(y = (..count..)/sum(..count..))) + geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) + facet_wrap(~loan_status)
#Default rate for each category(default_count/total_count)
#mortgage - 12%
#own - 17.14%
#rent - 16.66%
#Own house applicants are high because total are very less
#But Rented applicants are having same percentage of own housed applicants with large no.of total applicants.
#So rented applicants are more defaulters

ggplot(Debt_consolidation, aes(x = as.factor(Debt_consolidation$annual_inc_categories))) + geom_bar(aes(y = (..count..)/sum(..count..))) + geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) + facet_wrap(~loan_status)
#Default rate for each category(default_count/total_count)
#Average_inc - 15%
#High_inc - 12.73%
#less_inc - 17%
#Very_high_inc - 12%
#Less_inc & Average income applicants are more defaulters because of applications and high rate of default

ggplot(Debt_consolidation, aes(x = as.factor(Debt_consolidation$debt_to_inc_categories))) + geom_bar(aes(y = (..count..)/sum(..count..))) + geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) + facet_wrap(~loan_status)
#Average_inc - 15.7%
#High_inc - 14.4%
#less_inc - 17.4%
#Very_high_inc - 12.6%
#Its obvious that it follows both loan_amnt proportionally and with annual_inc inversely

ggplot(Debt_consolidation, aes(x = as.factor(Debt_consolidation$verification_status))) + geom_bar(aes(y = (..count..)/sum(..count..))) + geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) + facet_wrap(~loan_status)
#Default rate for each category(default_count/total_count)
#Not verified - 25%
#source verified - 5%
#Verified - 10%
#Its obvious to say that not verified applicants are more risky applicants

#Key points for debt consolidaion
#1. High interest rated applicants are more risky applicants
#2. Applicants with less than 3 years experiece are risky applicants
#3. Rented applicants are more risky applicants for debt consoliation
#4. Annual income,loan amnt and dti follow same trend
#5. Average and less category applicants are more defaulters

#2.Credit card
credit_card <- subset(loan_data_modified, loan_data_modified$purpose == "credit_card")

ggplot(credit_card, aes(x = as.factor(credit_card$loan_amnt_categories))) + geom_bar(aes(y = (..count..)/sum(..count..))) + geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) + facet_wrap(~loan_status)
#Default rate for each category(default_count/total_count)
#Average loan_amnt - 14%
#High loan_amnt - 7.6%
#Less loan_amnt - 14.5%
#Very high_amnt - 5.2%
#A clear trend that shows applicants less than the average loan amnt are more risky applicants

ggplot(credit_card, aes(x = as.factor(credit_card$term))) + geom_bar(aes(y = (..count..)/sum(..count..))) + geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) + facet_wrap(~loan_status)
#As we already seen that 36 months term have more default rate 

ggplot(credit_card, aes(x = as.factor(credit_card$int_rate_categories))) + geom_bar(aes(y = (..count..)/sum(..count..))) + geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) + facet_wrap(~loan_status)
#Default rate for each category(default_count/total_count)
#Average_int_rate - 13.6%
#High_int_rate - 13.9%
#less_int_rate - 9.5%
#Very_high_int_rate - 3%
#Average and high interest rated applicants are contributing more for credit card

ggplot(credit_card, aes(x = as.factor(credit_card$year))) + geom_bar(aes(y = (..count..)/sum(..count..))) + geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) + facet_wrap(~loan_status)
#2008 & 2009 are worst years

ggplot(credit_card, aes(x = as.factor(credit_card$installment_categories))) + geom_bar(aes(y = (..count..)/sum(..count..))) + geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) + facet_wrap(~loan_status)
#Default rate for each category(default_count/total_count)
#Average_installment - 12.3%
#High_installment - 8%
#less_installment - 15.4%
#Very_high_installment - 7.3%
#Applicants who are paying average and less installments are more risky appicants

ggplot(credit_card, aes(x = as.factor(credit_card$emp_length))) + geom_bar(aes(y = (..count..)/sum(..count..))) + geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) + facet_wrap(~loan_status)
#Plot shows that less than 1, 2 & 3 year experience are more defaulters 
#40% of defaulters are less than 3 years experienced applicants

ggplot(credit_card, aes(x = as.factor(credit_card$home_ownership))) + geom_bar(aes(y = (..count..)/sum(..count..))) + geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) + facet_wrap(~loan_status)
#Default rate for each category(default_count/total_count)
#mortgage - 9.9%
#own - 8.6%
#rent - 11%
#Home ownership is not showing any variation between own,rented and mortgage

ggplot(credit_card, aes(x = as.factor(credit_card$annual_inc_categories))) + geom_bar(aes(y = (..count..)/sum(..count..))) + geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) + facet_wrap(~loan_status)
#Default rate for each category(default_count/total_count)
#Average_inc - 10.2%
#High_inc - 10.6%
#less_inc - 10.5%
#Very_high_inc - 8.36%
#Same observation applicants less than high_inc are more risky applicants

ggplot(credit_card, aes(x = as.factor(credit_card$debt_to_inc_categories))) + geom_bar(aes(y = (..count..)/sum(..count..))) + geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) + facet_wrap(~loan_status)
#Average_inc - 10.89%
#High_inc - 10.94%
#less_inc - 12.8%
#Very_high_inc - 8.36%
#It follows both loan_amnt and inc variables

ggplot(credit_card, aes(x = as.factor(credit_card$verification_status))) + geom_bar(aes(y = (..count..)/sum(..count..))) + geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) + facet_wrap(~loan_status)
#Default rate for each category(default_count/total_count)
#Not verified - 16%
#source verified - 3.8%
#Verified - 7.7%
#Its obvious to say that not verified applicants are more risky applicants

#Keypoints observed for credit card applicants
#1. Loan_amnt,annual income and dti are of similar impact
#2. High int_rated applicants are more risky implicants
#3. Employers with less than 3 year experience are likely to default
#4. High and avg installment applicants are risky applicants


#3. Home_improvement
home_improvement <- subset(loan_data_modified, loan_data_modified$purpose == "home_improvement")

ggplot(home_improvement, aes(x = as.factor(home_improvement$loan_amnt_categories))) + geom_bar(aes(y = (..count..)/sum(..count..))) + geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) + facet_wrap(~loan_status)
#Default rate for each category(default_count/total_count)
#Average loan_amnt - 14.39%
#High loan_amnt - 11.5%
#Less loan_amnt - 12.65%
#Very high_amnt - 6.8%
#A clear trend that shows applicants less than the average loan amnt are more risky applicants

ggplot(home_improvement, aes(x = as.factor(home_improvement$term))) + geom_bar(aes(y = (..count..)/sum(..count..))) + geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) + facet_wrap(~loan_status)
#As we already seen that 36 months term have more default rate 

ggplot(home_improvement, aes(x = as.factor(home_improvement$int_rate_categories))) + geom_bar(aes(y = (..count..)/sum(..count..))) + geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) + facet_wrap(~loan_status)
#Default rate for each category(default_count/total_count)
#Average_int_rate - 12.45%
#High_int_rate - 17.7%
#less_int_rate - 12.32%
#Very_high_int_rate - 3.5%
#Avg and less interest rated applicants are contributing more for home_improvement

ggplot(home_improvement, aes(x = as.factor(home_improvement$year))) + geom_bar(aes(y = (..count..)/sum(..count..))) + geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) + facet_wrap(~loan_status)
#2009 was worst year

ggplot(home_improvement, aes(x = as.factor(home_improvement$installment_categories))) + geom_bar(aes(y = (..count..)/sum(..count..))) + geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) + facet_wrap(~loan_status)
#Default rate for each category(default_count/total_count)
#Average_installment - 10.8%
#High_installment - 11.46%
#less_installment - 12.21%
#Very_high_installment - 12.55%
#Applicants who are paying high and very high installments are more risky appicants

ggplot(home_improvement, aes(x = as.factor(home_improvement$emp_length))) + geom_bar(aes(y = (..count..)/sum(..count..))) + geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) + facet_wrap(~loan_status)
#1-3 year experience :- 16%
#4-6 year experience :- 10.9%
#7-9 year experience :- 8.9%
#10 year experience :- 9.7%
#Plot shows that less than 1, 2 & 3 year experience are more defaulters 
#35% of defaulters are less than 3 years experienced applicants


ggplot(home_improvement, aes(x = as.factor(home_improvement$home_ownership))) + geom_bar(aes(y = (..count..)/sum(..count..))) + geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) + facet_wrap(~loan_status)
#Default rate for each category(default_count/total_count)
#mortgage - 9.9%
#own - 8.6%
#rent - 11%
#Mortgage applicants are very high compared to rent and own house applicants

ggplot(home_improvement, aes(x = as.factor(home_improvement$annual_inc_categories))) + geom_bar(aes(y = (..count..)/sum(..count..))) + geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) + facet_wrap(~loan_status)
#Default rate for each category(default_count/total_count)
#Average_inc - 12%
#High_inc - 10.97%
#less_inc - 13%
#Very_high_inc - 11.11%
#Applicants less than avg_inc are more risky applicants

ggplot(home_improvement, aes(x = as.factor(home_improvement$debt_to_inc_categories))) + geom_bar(aes(y = (..count..)/sum(..count..))) + geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) + facet_wrap(~loan_status)
#Average_inc - 12%
#High_inc - 9.98%
#less_inc - 14%
#Very_high_inc - 6.4%
#It follows both loan_amnt and inc variables

ggplot(home_improvement, aes(x = as.factor(home_improvement$verification_status))) + geom_bar(aes(y = (..count..)/sum(..count..))) + geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) + facet_wrap(~loan_status)
#Default rate for each category(default_count/total_count)
#Not verified - 17%
#source verified - 6%
#Verified - 9.43%
#Its obvious to say that not verified applicants are more risky applicants

#Keypoints for home_improvement
#1. Loan_amnt,annual_inc follow the same trend applicants below avg range are risky
#2. High installment category applicants are more defaulters
#3. Less than 3 year experience are risky applicants
#4. mortgage living applicants are risky applicants

#4. small_business
small_business <- subset(loan_data_modified, loan_data_modified$purpose == "small_business")

ggplot(small_business, aes(x = as.factor(small_business$loan_amnt_categories))) + geom_bar(aes(y = (..count..)/sum(..count..))) + geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) + facet_wrap(~loan_status)
#Default rate for each category(default_count/total_count)
#Average loan_amnt - 26.6%
#High loan_amnt - 30.5%
#Less loan_amnt - 27%
#Very high_amnt - 21.56%
#A clear trend that shows applicants less than the high loan amnt are more risky applicants

ggplot(small_business, aes(x = as.factor(small_business$term))) + geom_bar(aes(y = (..count..)/sum(..count..))) + geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) + facet_wrap(~loan_status)
#As we already seen that 36 month term applicants are more defaulters

ggplot(small_business, aes(x = as.factor(small_business$int_rate_categories))) + geom_bar(aes(y = (..count..)/sum(..count..))) + geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) + facet_wrap(~loan_status)
#Default rate for each category(default_count/total_count)
#Average_int_rate - 19.81%
#High_int_rate - 35%
#less_int_rate - 17%
#Very_high_int_rate - 29%
#high and very high interest rated applicants are contributing more for small_business

ggplot(small_business, aes(x = as.factor(small_business$year))) + geom_bar(aes(y = (..count..)/sum(..count..))) + geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) + facet_wrap(~loan_status)
#2009 was worst year

ggplot(small_business, aes(x = as.factor(small_business$installment_categories))) + geom_bar(aes(y = (..count..)/sum(..count..))) + geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) + facet_wrap(~loan_status)
#Default rate for each category(default_count/total_count)
#Average_installment - 21%
#High_installment - 24.5%
#less_installment - 24.2%
#Very_high_installment - 31%
#Applicants who are paying high and very high installments are more risky appicants

ggplot(small_business, aes(x = as.factor(small_business$emp_length))) + geom_bar(aes(y = (..count..)/sum(..count..))) + geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) + facet_wrap(~loan_status)
#Plot shows that less than 1, 2 & 3 year experience are more defaulters 
#35% of defaulters are less than 3 years experienced applicants
#45% of defaulters are 1 year experienced applicants


ggplot(small_business, aes(x = as.factor(small_business$home_ownership))) + geom_bar(aes(y = (..count..)/sum(..count..))) + geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) + facet_wrap(~loan_status)
#Default rate for each category(default_count/total_count)
#mortgage - 25%
#own - 21%(Total applicants are less)
#rent - 25%
#Mortgage and rented applicants are very high risky

ggplot(small_business, aes(x = as.factor(small_business$annual_inc_categories))) + geom_bar(aes(y = (..count..)/sum(..count..))) + geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) + facet_wrap(~loan_status)
#Default rate for each category(default_count/total_count)
#Average_inc - 23%
#High_inc - 29%
#less_inc - 24%
#Very_high_inc - 25%
#Applicants high and very annual income are more risky applicants

ggplot(small_business, aes(x = as.factor(small_business$debt_to_inc_categories))) + geom_bar(aes(y = (..count..)/sum(..count..))) + geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) + facet_wrap(~loan_status)
#Average_inc - 24.6%
#High_inc - 26%
#less_inc - 28%
#Very_high_inc - 18%
#It follows both loan_amnt and inc variables

ggplot(small_business, aes(x = as.factor(small_business$verification_status))) + geom_bar(aes(y = (..count..)/sum(..count..))) + geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) + facet_wrap(~loan_status)
#Default rate for each category(default_count/total_count)
#Not verified - 45%
#source verified - 6.4%
#Verified - 20%
#Its obvious to say that not verified applicants are more risky applicants
#But verified applicants also are risky in small_business case.

#Keypoints in small_business
#1. High and very high int_rate applicants are more defaulters
#2. High and very high installment paying applicants are more defaulters
#3. High and very high annnual_inc category applicants are more defaulters
#4. 1 year experienced applicants are more risky 
#5. Verified and not verified both are risky in small_business

#Key points from bivariate analysis
#1. In all purposes high and very high rate of interest applicants are more defaulters
#2. In all purposes 2009 was worst year for all applicants
#3. Except in debt consolidation loan amnt category was not a clear indicator 
#4. In home_improvement mortgage applicants are more defaulters












