############################  NYC Parking Tickets: An Exploratory Analysis  #############################################

## Objective: Purpose of this case study is to conduct an exploratory data analysis that helps you understand the data.
## Data: NYC Police Department has collected data for parking tickets. Out of these, the data files from 2014 to 2017 are publicly available on Kaggle. We will try and perform some exploratory analysis on this data.
## Analysis will be carried out in below phases
## Phase 1: Loading, quality verification and Cleansing of source Data
## Phase 2: Examine the data to answer the assignment questions asked in assignment
## Phase 3: Performing Aggregation tasks to answer the assignment questions

# Loading  SparkR Evironment and starting the session
spark_path <- '/usr/local/spark'

if (nchar(Sys.getenv("SPARK_HOME")) < 1) {
  Sys.setenv(SPARK_HOME = spark_path)
}

library(SparkR, lib.loc = c(file.path(Sys.getenv("SPARK_HOME"), "R", "lib")))

sparkR.session(master = "yarn", sparkConfig = list(spark.driver.memory = "1g"))

#Libraries: All the required libraries will be loaded in this section
library(stringr)
library(ggplot2)
library(reshape2)


################################### Phase 1: Loading, quality verification and Cleansing of source Data #########################################

# Loading the source data into data frames
nyc_2015 <- read.df("hdfs:///common_folder/nyc_parking/Parking_Violations_Issued_-_Fiscal_Year_2015.csv", source= "csv", header= "true", inferSchema= "true")
nyc_2016 <- read.df("hdfs:///common_folder/nyc_parking/Parking_Violations_Issued_-_Fiscal_Year_2016.csv", source= "csv", header= "true", inferSchema= "true")
nyc_2017 <- read.df("hdfs:///common_folder/nyc_parking/Parking_Violations_Issued_-_Fiscal_Year_2017.csv", source= "csv", header= "true", inferSchema= "true")

# Quality verification of source data
head(nyc_2015)
dim(nyc_2015) 
str(nyc_2015) 
printSchema(nyc_2015)
## In 2015 data set there are 11809233 records with 51 columns of data
## All the columns data types are as per data dictionary except for Issue Date, Violation Location, Date First Observed, Unregistered Vehicle, 
## Latitude, Longitude, Community Board, Community Council, Census Tract, BIN and BBL
## Data types of these columns will be corrected in further analysis whenever necessary

head(nyc_2016)
dim(nyc_2016) 
str(nyc_2016) 
printSchema(nyc_2016)
## In 2016 data set there are 10626899 records with 51 columns of data
## All the columns data types are as per data dictionary except for Issue Date, Violation Location, Unregistered Vehicle,
## Latitude, Longitude, Community Board, Community Council, Census Tract, BIN and BBL
## Data types of these columns will be corrected in further analysis whenever necessary

head(nyc_2017)
dim(nyc_2017) 
str(nyc_2017) 
printSchema(nyc_2017)  
## In 2017 data set there are 10803028 records with 43 columns of data
## All the columns data types are as per data dictionary except for Issue Date and Violation Location 
## Data types of these columns will be corrected in further analysis whenever necessary

## It is observed in all the three data sets that columns names are in sentence case with spaces and unregistered vehicle column name had ? symbol
## Replacing space with "_", removing "?" and converting all columns to lower cases.
## Correcting 2015 data column names
colnames(nyc_2015) <- gsub("^\\s+|\\s+$", "", colnames(nyc_2015)) ##Removing leading and trailing spaces
colnames(nyc_2015) <- gsub(" ", "_", colnames(nyc_2015))          ##Replacing spaces in between with _
colnames(nyc_2015) <- gsub("\\?", "", colnames(nyc_2015))         ##Removing ? symbol
colnames(nyc_2015) <- tolower(colnames(nyc_2015))                 ##Converting to lower case
colnames(nyc_2015)
head(nyc_2015)

## Correcting 2016 data column names
colnames(nyc_2016) <- gsub("^\\s+|\\s+$", "", colnames(nyc_2016)) ##Removing leading and trailing spaces
colnames(nyc_2016) <- gsub(" ", "_", colnames(nyc_2016))          ##Replacing spaces in between with _
colnames(nyc_2016) <- gsub("\\?", "", colnames(nyc_2016))         ##Removing ? symbol
colnames(nyc_2016) <- tolower(colnames(nyc_2016))                 ##Converting to lower case
colnames(nyc_2016)
head(nyc_2016)

## Correcting 2017 data column names
colnames(nyc_2017) <- gsub("^\\s+|\\s+$", "", colnames(nyc_2017)) ##Removing leading and trailing spaces
colnames(nyc_2017) <- gsub(" ", "_", colnames(nyc_2017))          ##Replacing spaces in between with _
colnames(nyc_2017) <- gsub("\\?", "", colnames(nyc_2017))         ##Removing ? symbol
colnames(nyc_2017) <- tolower(colnames(nyc_2017))                 ##Converting to lower case
colnames(nyc_2017)
head(nyc_2017)

## From data dictionary it is understood that "summons_number" is the ticket number issued.
## Since it is unique number checking and removing duplicates with in built function of "dropDuplicates"
nyc_2015<- dropDuplicates(nyc_2015, "summons_number")
head(nyc_2015)
dim(nyc_2015)
## After removing duplicates, 2015 data number of records are 10951256

nyc_2016<- dropDuplicates(nyc_2016, "summons_number")
head(nyc_2016)
dim(nyc_2016)
## After running identify and remove duplicates on 2016 data number of records are 10626899, no duplicates found

nyc_2017<- dropDuplicates(nyc_2017, "summons_number")
head(nyc_2017)
dim(nyc_2017)
## After running identify and remove duplicates on 2017 data number of records are 10803028, no duplicates found

## Adding SQL JAR to environment as SQL queries are required for further analysis
sql("ADD JAR /opt/cloudera/parcels/CDH/lib/hive/lib/hive-hcatalog-core-1.1.0-cdh5.11.2.jar")

## From the column names it is found that below columns exist in 2015 and 2016 data, but not in 2017
## latitude, longitude, community_board, community_council, census_tract, bin, bbl and nta
## Checking the quality issues in these columns

## Creating temporary SQL views of source data
createOrReplaceTempView(nyc_2015, "mrk_2015")
createOrReplaceTempView(nyc_2016, "mrk_2016")
createOrReplaceTempView(nyc_2017, "mrk_2017")

## Checking for null values of above mentioned columns in 2015 data
null_count2015 <- SparkR::sql("select sum(case when latitude is NULL then 1 end) lat_null,
                              sum(case when longitude is NULL then 1 end) long_null,
                              sum(case when community_board is NULL then 1 end) cb_null,
                              sum(case when community_council is NULL then 1 end) cc_null,
                              sum(case when census_tract is NULL then 1 end) cen_null,
                              sum(case when bin is NULL then 1 end) bin_null,
                              sum(case when bbl is NULL then 1 end) bbl_null,
                              sum(case when nta is NULL then 1 end) nta_null
                              from arrs_2015")
head(null_count2015)
##  lat_null long_null  cb_null  cc_null cen_null bin_null bbl_null nta_null      
## 10951256  10951256 10951256 10951256 10951256 10951256 10951256 10951256
## 10951256 null records(equal to total records) of each column are found in 2015 data, so these column can be dropped

## Checking for null values of above mentioned columns in 2016 data
null_count2016 <- SparkR::sql("select sum(case when latitude is NULL then 1 end) lat_null,
                              sum(case when longitude is NULL then 1 end) long_null,
                              sum(case when community_board is NULL then 1 end) cb_null,
                              sum(case when community_council is NULL then 1 end) cc_null,
                              sum(case when census_tract is NULL then 1 end) cen_null,
                              sum(case when bin is NULL then 1 end) bin_null,
                              sum(case when bbl is NULL then 1 end) bbl_null,
                              sum(case when nta is NULL then 1 end) nta_null
                              from arrs_2016")
head(null_count2016)
## lat_null long_null  cb_null  cc_null cen_null bin_null bbl_null nta_null      
## 10626899  10626899 10626899 10626899 10626899 10626899 10626899 10626899
## 10626899 null records(equal to total records) of each column are found in 2016 data, so these columns can be dropped

## Since all the values in latitude, longitude, community_board, community_council, census_tract, bin, bbl and nta are null
## Dropping these columns from 2015 and 2016 data
nyc_2015<- drop(nyc_2015, c("latitude", "longitude", "community_board", "community_council", "census_tract", "bin", "bbl", "nta"))
colnames(nyc_2015)

nyc_2016<- drop(nyc_2016, c("latitude", "longitude", "community_board", "community_council", "census_tract", "bin", "bbl", "nta"))
colnames(nyc_2016)


## It is mentioned ticket issue data should be considered for either financial year or calendar year for the assignment
## Understanding issue date 
head(nyc_2015)
head(nyc_2016)
head(nyc_2017)
## It is observed that:
## issue date is in mm/dd/yyyy format in all the three data files
## However it is recorded as string, so converting the same to date format in the three source data frames
nyc_2015$issue_date <- SparkR::to_date(nyc_2015$issue_date, 'MM/dd/yyyy')
head(nyc_2015)

nyc_2016$issue_date <- SparkR::to_date(nyc_2016$issue_date, 'MM/dd/yyyy')
head(nyc_2016)

nyc_2017$issue_date <- SparkR::to_date(nyc_2017$issue_date, 'MM/dd/yyyy')
head(nyc_2017)

## Creating temporary SQL views of updated source data
createOrReplaceTempView(nyc_2015, "mrk_nyc_2015")
createOrReplaceTempView(nyc_2016, "mrk_nyc_2016")
createOrReplaceTempView(nyc_2017, "mrk_nyc_2017")

## Checking for null values in issue_date
null_issue_date2015 <- SparkR::sql("select count(*) as null_date from mrk_nyc_2015 where issue_date is NULL")
head(null_issue_date2015)
## 0 null records found in 2015 data for issue date

null_issue_date2016 <- SparkR::sql("select count(*) as null_date from mrk_nyc_2016 where issue_date is NULL")
head(null_issue_date2016)
## 0 null records found in 2016 data for issue date

null_issue_date2017 <- SparkR::sql("select count(*) as null_date from mrk_nyc_2017 where issue_date is NULL")
head(null_issue_date2017)
## 0 null records found in 2017 data for issue date

## Understanding the issue date range and tickets spread across months
idr_2015 <- SparkR::sql("select year(issue_date) as Yr, month(issue_date) as mnth, count(*) as tickets from mrk_nyc_2015 
                        group by Yr, mnth
                        order by Yr desc")
head(idr_2015, nrow(idr_2015))

idr_2016 <- SparkR::sql("select year(issue_date) as Yr, month(issue_date) as mnth, count(*) as tickets from mrk_nyc_2016 
                        group by Yr, mnth
                        order by Yr desc")
head(idr_2016, nrow(idr_2016))

idr_2017 <- SparkR::sql("select year(issue_date) as Yr, month(issue_date) as mnth, count(*) as tickets from mrk_nyc_2017 
                        group by Yr, mnth
                        order by Yr desc")
head(idr_2017, nrow(idr_2017))

## From the three queries it is understood that data is spread across years:
## 2015 data file has data prior to 2014 till June 2015.
## 2016 data file has data spread across many years including fiscal year 2015 and 2017
## 2017 data file has data spread across many years including fiscal year 2015 and 2016
## Assumption: We are assuming that some of the tickets issued in 2015, 2016 or 2017 are reported in the successive years which is practical case scenario.
## As we need to do analysis for Fiscal year or calendar year of 2015, 2016 and 2017, we are extracting all three fiscal years data from three source data file and then combine into one file each for fiscal years 2015, 2016 and 2017
## As per US law fiscal year is from Oct 1st of previous year to 30th September of current year

## Extracting Fiscal year 2015 data from 2015, 2016 and 2017 files
fy15_15 <- SparkR::sql("select * from mrk_nyc_2015 where issue_date BETWEEN '2014-10-01' AND '2015-09-30'")
head(fy15_15)
nrow(fy15_15) #7809072 records found in 2015 file for Fiscal year 2015.

fy15_16 <- SparkR::sql("select * from mrk_nyc_2016 where issue_date BETWEEN '2014-10-01' AND '2015-09-30'")
head(fy15_16)
nrow(fy15_16) #2951766 records found in 2016 file for Fiscal year 2015.

fy15_17 <- SparkR::sql("select * from mrk_nyc_2017 where issue_date BETWEEN '2014-10-01' AND '2015-09-30'")
head(fy15_17)
nrow(fy15_17) #313 records found in 2016 file for Fiscal year 2015.

## Combining Fiscal year 2015 data from all three sources
nyc_fy2015 <- rbind(fy15_15, fy15_16, fy15_17)
head(nyc_fy2015)
nrow(nyc_fy2015) #10761151 are the total records for FY2015 


## Extracting Fiscal year 2016 data from 2016 and 2017 files. 2015 file is not considered as data after June 2015 is not present in 2015 file
fy16_16 <- SparkR::sql("select * from mrk_nyc_2016 where issue_date BETWEEN '2015-10-01' AND '2016-09-30'")
head(fy16_16)
nrow(fy16_16) #7671177 records found in 2016 file for Fiscal year 2016.

fy16_17 <- SparkR::sql("select * from mrk_nyc_2017 where issue_date BETWEEN '2015-10-01' AND '2016-09-30'")
head(fy16_17)
nrow(fy16_17) #2721121 records found in 2016 file for Fiscal year 2016.

## Combining Fiscal year 2016 data from all three sources
nyc_fy2016 <- rbind(fy16_16, fy16_17)
head(nyc_fy2016)
nrow(nyc_fy2016) #10392298 are the total records for FY2016 


## Extracting Fiscal year 2017 data from 2016 and 2017 files. 2015 file is not considered as data after June 2015 is not present in 2015 file
fy17_16 <- SparkR::sql("select * from mrk_nyc_2016 where issue_date BETWEEN '2016-10-01' AND '2017-09-30'")
head(fy17_16)
nrow(fy17_16) #1781 records found in 2016 file for Fiscal year 2017.

fy17_17 <- SparkR::sql("select * from mrk_nyc_2017 where issue_date BETWEEN '2016-10-01' AND '2017-09-30'")
head(fy17_17)
nrow(fy17_17) #8078339 records found in 2016 file for Fiscal year 2017.

## Combining Fiscal year 2016 data from all three sources
nyc_fy2017 <- rbind(fy17_16, fy17_17)
head(nyc_fy2017)
nrow(nyc_fy2017) #8080120 are the total records for FY2017 

## nyc_fy2015, nyc_fy2016, nyc_fy2017 are files with fiscal years data extracted from all the source files.

## Checking and removing duplicate summons number from the combined data files
nyc_fy2015<- dropDuplicates(nyc_fy2015, "summons_number")
head(nyc_fy2015)
nrow(nyc_fy2015)
## 10539160 are the total records in FY2015 after removing duplicates of summons number

nyc_fy2016<- dropDuplicates(nyc_fy2016, "summons_number")
head(nyc_fy2016)
nrow(nyc_fy2016)
## 10392298 is the total records in FY2016 after removing duplicates of summons number. No duplicate records

nyc_fy2017<- dropDuplicates(nyc_fy2017, "summons_number")
head(nyc_fy2017)
nrow(nyc_fy2017)
## 8080120 is the total records in FY2017 after removing duplicates of summons number. No duplicate records

### The data from nyc_fy2015, nyc_fy2016 and nyc_2017 will be used for further analysis of Phase 2 and 3

#################################################################################################################################################

############################# Phase 2: Examine the data to answer the assignment questions asked in assignment ############################

## Creating temporary SQL views of final data prepared in Phase 1
createOrReplaceTempView(nyc_fy2015, "mrk_fy2015")
createOrReplaceTempView(nyc_fy2016, "mrk_fy2016")
createOrReplaceTempView(nyc_fy2017, "mrk_fy2017")

## Question 1: Find the total number of tickets for each year.
tickets <- c(nrow(nyc_fy2015), nrow(nyc_fy2016), nrow(nyc_fy2017))
year <- c("FY2015", "FY2016", "FY2017")
tickets_by_year <- data.frame(year, tickets)
tickets_by_year
## year  tickets
## FY2015 10539160
## FY2016 10392298
## FY2017  8080120

## Plotting the year wise ticket count for comparision study
ggplot(tickets_by_year, aes(x=year, y = tickets, fill=tickets)) + 
  geom_col(alpha=0.8) + ggtitle("Plot 1: Total number of tickets for each year") + 
  labs(x="Years", y="No.Of Tickets") + geom_text(aes(label=tickets))
## From the plot it is evident that number of tickets issued is showing a decline trend with a considerable decrease in Fiscal year 2017.

## Question 2: Find out the number of unique states from where the cars that got parking tickets came from.
state_2015<- SparkR::sql("SELECT count(distinct(registration_state)) as states from mrk_fy2015")
head(state_2015)
## In 2015 tickets came from 69 unique states

state_2016<- SparkR::sql("SELECT count(distinct(registration_state)) as states from mrk_fy2016")
head(state_2016)
## In 2016 tickets came from 67 unique states

state_2017<- SparkR::sql("SELECT count(distinct(registration_state)) as states from mrk_fy2017")
head(state_2017)
## In 2017 tickets came from 67 unique states

## Analyzing the registration_state names:
state_name_2015 <- SparkR::sql("SELECT registration_state, count(*) as tickets from mrk_fy2015
                               group by registration_state
                               order by tickets desc")
head(state_name_2015, nrow(state_name_2015))

state_name_2016 <- SparkR::sql("SELECT registration_state, count(*) as tickets from mrk_fy2016
                               group by registration_state
                               order by tickets desc")
head(state_name_2016, nrow(state_name_2016))


state_name_2017 <- SparkR::sql("SELECT registration_state, count(*) as tickets from mrk_fy2017
                               group by registration_state
                               order by tickets desc")
head(state_name_2017, nrow(state_name_2017))
## It is found that "NY" is the registration_state with maximum number of tickets in FY2015, FY2016 and FY2017
## Also one state name with numeric value 99 is found in all the financial years
## As asked in the assignment, replacing the same with with the state having maximum entries("NY")
nyc_fy2015$registration_state <- ifelse(nyc_fy2015$registration_state == "99", "NY", nyc_fy2015$registration_state)
head(nyc_fy2015)

nyc_fy2016$registration_state <- ifelse(nyc_fy2016$registration_state == "99", "NY", nyc_fy2016$registration_state)
head(nyc_fy2016)

nyc_fy2017$registration_state <- ifelse(nyc_fy2017$registration_state == "99", "NY", nyc_fy2017$registration_state)
head(nyc_fy2017)
## Updating temporary SQL views after updating registration state
createOrReplaceTempView(nyc_fy2015, "mrk_fy2015")
createOrReplaceTempView(nyc_fy2016, "mrk_fy2016")
createOrReplaceTempView(nyc_fy2017, "mrk_fy2017")

## Find out the number of unique states after replacing state with code 99
state_2015<- SparkR::sql("SELECT count(distinct(registration_state)) as states from mrk_fy2015")
head(state_2015)
## In 2015 tickets came from 68 unique states

state_2016<- SparkR::sql("SELECT count(distinct(registration_state)) as states from mrk_fy2016")
head(state_2016)
## In 2016 tickets came from 66 unique states

state_2017<- SparkR::sql("SELECT count(distinct(registration_state)) as states from mrk_fy2017")
head(state_2017)
## In 2017 tickets came from 66 unique states


##Question 3: Some parking tickets don't have the address for violation location on them, which is a cause for concern. 
##            Write a query to check the number of such tickets.

## For this considering Violation location, house number and street name fields
## If these fields are empty it implies that address is not captured
## Writing queries to count these fields having null values in all years
no_address_2015 <- SparkR::sql("SELECT count(*) - count(violation_location) as null_location,
                               count(*) - count(house_number) as null_hno,
                               count(*) - count(street_name) as null_street from mrk_fy2015")
head(no_address_2015) 
no_add_2015 <- data.frame(head(no_address_2015))
no_add_2015$year <- "FY2015"
colnames(no_add_2015) <- c("Violation_Location", "House_Number", "Street_Name", "Year")
no_add_2015

no_address_2016 <- SparkR::sql("SELECT count(*) - count(violation_location) as null_location,
                               count(*) - count(house_number) as null_hno,
                               count(*) - count(street_name) as null_street from mrk_fy2016")
head(no_address_2016)
no_add_2016 <- data.frame(head(no_address_2016))
no_add_2016$year <- "FY2016"
colnames(no_add_2016) <- c("Violation_Location", "House_Number", "Street_Name", "Year")

no_address_2017 <- SparkR::sql("SELECT count(*) - count(violation_location) as null_location,
                               count(*) - count(house_number) as null_hno,
                               count(*) - count(street_name) as null_street from mrk_fy2017")
head(no_address_2017)
no_add_2017 <- data.frame(head(no_address_2017))
no_add_2017$year <- "FY2017"
colnames(no_add_2017) <- c("Violation_Location", "House_Number", "Street_Name", "Year")

null_address <- rbind(no_add_2015, no_add_2016, no_add_2017)
null_address

## The queries written above returns the following output with count of null entries for address fields year wise
##  Violation_Location   House_Number   Street_Name     Year
##           1452260      1617406        6497         FY2015
##           1975285      2133853        4354         FY2016
##           1527752      1694560        2646         FY2017

## Plotting the year wise null entries for comparision study
null_address <- melt(null_address, id.vars='Year')
ggplot(null_address, aes(x=Year, y=value, fill=variable)) + 
  geom_bar(stat='identity', position='dodge') +  ggtitle("Plot 2: Records with missing address for each year")
##It is observed that :
##  .	Violation location and house number discrepancies are highest in FY2016 followed by FY2015 and FY2017. 
##  .	Street name is highest in FY2015, followed by FY2016 and FY2017.
################################################################################################################################################

############################# Phase 3: Performing Aggregation tasks to answer the assignment questions ###################################

## Question 1: How often does each violation code occur? Display the frequency of the top five violation codes.

## Calculating FY 2015 Violation code counts by grouping them and displaying top5

vc_counts_2015 <- summarize(groupBy(nyc_fy2015, nyc_fy2015$violation_code), count = n(nyc_fy2015$violation_code))
vc_2015 <- head(arrange(vc_counts_2015, desc(vc_counts_2015$count)), 5)
vc_2015$Year <- "FY2015"
vc_2015
##  violation_code   count   Year
##      21          1475086 FY2015
##      38          1261904 FY2015
##      14           908916 FY2015
##      36           765892 FY2015
##      37           721594 FY2015

## Calculating FY 2016 Violation code counts by grouping them and displaying top5
vc_counts_2016 <- summarize(groupBy(nyc_fy2016, nyc_fy2016$violation_code), count = n(nyc_fy2016$violation_code))
vc_2016 <- head(arrange(vc_counts_2016, desc(vc_counts_2016$count)), 5)
vc_2016$Year <- "FY2016"
vc_2016
##  violation_code   count   Year
##          21      1506924 FY2016
##          36      1344464 FY2016
##          38      1078318 FY2016
##          14       839806 FY2016
##          37       648304 FY2016

## Calculating FY 2017 Violation code counts by grouping them and displaying top5
vc_counts_2017 <- summarize(groupBy(nyc_fy2017, nyc_fy2017$violation_code), count = n(nyc_fy2017$violation_code))
vc_2017 <- head(arrange(vc_counts_2017, desc(vc_counts_2017$count)), 5)
vc_2017$Year <- "FY2017"
vc_2017
##  violation_code   count   Year
##          21      1115585 FY2017
##          36      1105358 FY2017
##          38       805463 FY2017
##          14       678873 FY2017
##          20       466399 FY2017

## Combining all three years data into single data frame and plotting the data for comparision
vc_count <- rbind(vc_2015, vc_2016, vc_2017)

ggplot(vc_count, aes(as.factor(violation_code))) + 
  geom_col(aes(y = count, fill = count)) + 
  ggtitle("Plot 3: Top 5 Violation Codes") + 
  labs(x="Violation Code", y="Count") +
  facet_wrap(~Year) + 
  geom_text(aes(as.factor(violation_code), count, label=count),vjust=-0.3)
## .	Violation Code 21 is the highest in all three years with frequency relatively less in FY2017.
## .	Violation Code 38 is the second highest in FY2015, whereas code 36 in FY2016 & FY2017.
## .	Violation Code 14 is the third highest in FY2015, whereas code 38 in FY2016 & FY2017.
## .	Violation Code 36 is the fourth highest in FY2015, whereas code 14 in FY2016 & FY2017.
## .	Violation Code 37 is the fifth highest in FY2015 & FY2016, whereas code 20 in FY2017.


## Question 2: How often does each 'vehicle body type' get a parking ticket? How about the 'vehicle make'? 
## 2 a) Vehicle body type analysis year wise
## Calculating FY 2015  Vehicle body type counts by grouping them and displaying top5
vb_counts_2015 <- summarize(groupBy(nyc_fy2015, nyc_fy2015$vehicle_body_type), count = n(nyc_fy2015$vehicle_body_type))
vb_2015 <- head(arrange(vb_counts_2015, desc(vb_counts_2015$count)), 5)
vb_2015$Year <- "FY2015"
vb_2015
##    vehicle_body_type   count   Year
##           SUBN       3345607 FY2015
##           4DSD       2972532 FY2015
##           VAN        1554789 FY2015
##           DELV        808104 FY2015
##           SDN         429761 FY2015

## Calculating FY 2016  Vehicle body type counts by grouping them and displaying top5
vb_counts_2016 <- summarize(groupBy(nyc_fy2016, nyc_fy2016$vehicle_body_type), count = n(nyc_fy2016$vehicle_body_type))
vb_2016 <- head(arrange(vb_counts_2016, desc(vb_counts_2016$count)), 5)
vb_2016$Year <- "FY2016"
vb_2016
##    vehicle_body_type   count   Year
##           SUBN       3444465 FY2016
##           4DSD       2937601 FY2016
##           VAN        1445801 FY2016
##           DELV        708818 FY2016
##           SDN         420817 FY2016

## Calculating FY 2017  Vehicle body type counts by grouping them and displaying top5
vb_counts_2017 <- summarize(groupBy(nyc_fy2017, nyc_fy2017$vehicle_body_type), count = n(nyc_fy2017$vehicle_body_type))
vb_2017 <- head(arrange(vb_counts_2017, desc(vb_counts_2017$count)), 5)
vb_2017$Year <- "FY2017"
vb_2017
##    vehicle_body_type   count   Year
##           SUBN       2814791 FY2016
##           4DSD       2323842 FY2016
##           VAN        1041662 FY2016
##           DELV        510284 FY2016
##           SDN         304873 FY2016

## Combining all three years data into single data frame and plotting the data for comparision
vb_count <- rbind(vb_2015, vb_2016, vb_2017)

ggplot(vb_count, aes(as.factor(vehicle_body_type))) + 
  geom_col(aes(y = count, fill = count)) + 
  ggtitle("Plot 4: Top 5 Vehicle Body Types") + 
  labs(x="Vehicle Body Types", y="Count") +
  facet_wrap(~Year) + 
  geom_text(aes(as.factor(vehicle_body_type), count, label=count),vjust=-0.3)

##From the plot it is evident that, vehicle body types SUBN, followed by 4DSD, VAN, DELV and SDN are issued high parking tickets and remain the same across the years.

## 2 b) Vehicle make analysis year wise
## Calculating FY 2015  Vehicle make counts by grouping them and displaying top5
vm_counts_2015 <- summarize(groupBy(nyc_fy2015, nyc_fy2015$vehicle_make), count = n(nyc_fy2015$vehicle_make))
vm_2015 <- head(arrange(vm_counts_2015, desc(vm_counts_2015$count)), 5)
vm_2015$Year <- "FY2015"
vm_2015
##  vehicle_make   count   Year
##      FORD      1350905 FY2015
##      TOYOT     1088734 FY2015
##      HONDA      981975 FY2015
##      NISSA      806593 FY2015
##      CHEVR      795428 FY2015

## Calculating FY 2016  Vehicle make counts by grouping them and displaying top5
vm_counts_2016 <- summarize(groupBy(nyc_fy2016, nyc_fy2016$vehicle_make), count = n(nyc_fy2016$vehicle_make))
vm_2016 <- head(arrange(vm_counts_2016, desc(vm_counts_2016$count)), 5)
vm_2016$Year <- "FY2016"
vm_2016
##  vehicle_make   count   Year
##     FORD       1279043 FY2016
##     TOYOT      1145346 FY2016
##     HONDA      1007007 FY2016
##     NISSA       833840 FY2016
##     CHEVR       723850 FY2016

## Calculating FY 2017  Vehicle make counts by grouping them and displaying top5
vm_counts_2017 <- summarize(groupBy(nyc_fy2017, nyc_fy2017$vehicle_make), count = n(nyc_fy2017$vehicle_make))
vm_2017 <- head(arrange(vm_counts_2017, desc(vm_counts_2017$count)), 5)
vm_2017$Year <- "FY2017"
vm_2017
##  vehicle_make  count   Year
##     FORD      946351 FY2017
##     TOYOT     911247 FY2017
##     HONDA     810702 FY2017
##     NISSA     692699 FY2017
##     CHEVR     528301 FY2017

## Combining all three years data into single data frame and plotting the data for comparision
vm_count <- rbind(vm_2015, vm_2016, vm_2017)

ggplot(vm_count, aes(as.factor(vehicle_make))) + 
  geom_col(aes(y = count, fill = count)) + 
  ggtitle("Plot 5: Top 5 Vehicle Make") + 
  labs(x="Vehicle Body Types", y="Count") +
  facet_wrap(~Year) + 
  geom_text(aes(as.factor(vehicle_make), count, label=count),vjust=-0.3)

## From the plot and above table it is evident that FORD is the vehicle make which had received highest number of tickets in all the three fiscal years.
## FORD is followed by Toyota, Honda, Nissan and Chevrolet respectively in all the three years. 
## A downward trend can be observed from FY2015 to FY2017 which followed the similar with total number of tickets issued.

## Question 3a: 'Violation Precinct' (this is the precinct of the zone where the violation occurred). Using this, can you make any insights for parking violations in any specific areas of the city?
vp_counts_2015 <- summarize(groupBy(nyc_fy2015, nyc_fy2015$violation_precinct), count = n(nyc_fy2015$violation_precinct))
vp_2015 <- head(arrange(vp_counts_2015, desc(vp_counts_2015$count)), 6)
vp_2015$Year <- "FY2015"
vp_2015
##    violation_precinct   count   Year
##                  0     1452260 FY2015
##                 19      559288 FY2015
##                 18      374333 FY2015
##                 14      366759 FY2015
##                  1      304537 FY2015
##                114      297477 FY2015

vp_counts_2016 <- summarize(groupBy(nyc_fy2016, nyc_fy2016$violation_precinct), count = n(nyc_fy2016$violation_precinct))
vp_2016 <- head(arrange(vp_counts_2016, desc(vp_counts_2016$count)), 6)
vp_2016$Year <- "FY2016"
vp_2016
## violation_precinct   count   Year
##                  0 1975284 FY2016
##                 19  537111 FY2016
##                 18  306044 FY2016
##                 14  302426 FY2016
##                  1  297320 FY2016
##                 13  285956 FY2016

vp_counts_2017 <- summarize(groupBy(nyc_fy2017, nyc_fy2017$violation_precinct), count = n(nyc_fy2017$violation_precinct))
vp_2017 <- head(arrange(vp_counts_2017, desc(vp_counts_2017$count)), 6)
vp_2017$Year <- "FY2017"
vp_2017
## violation_precinct   count   Year
##                  0 1527752 FY2017
##                 19  394295 FY2017
##                 14  275231 FY2017
##                  1  252632 FY2017
##                 18  233525 FY2017
##                114  222967 FY2017

## Combining all three years data into single data frame and plotting the data for comparision
vp_count <- rbind(vp_2015, vp_2016, vp_2017)

ggplot(vp_count, aes(as.factor(violation_precinct))) + 
  geom_col(aes(y = count, fill = count)) + 
  ggtitle("Plot 6: Violation Precint") + 
  labs(x="Violation Zones", y="Count") +
  facet_wrap(~Year) + 
  geom_text(aes(as.factor(violation_precinct), count, label=count),vjust=-0.3)
## From the plot, it is evident that:
##	Violation Precinct (Zone 19) recorded highest number of tickets across years FY2015, 16 and 17.
##	Zones 18 and 14 are close to each other in ticket issuing across the three years are next to zone 19.
##	Zone 1 and 114 are in 4th and 5th position across the three years.
##	There is a downward trend observed from FY2015 to FY2017 in all the zones.

## Question 3b:'Issuer Precinct' (this is the precinct that issued the ticket)
ip_counts_2015 <- summarize(groupBy(nyc_fy2015, nyc_fy2015$issuer_precinct), count = n(nyc_fy2015$issuer_precinct))
ip_2015 <- head(arrange(ip_counts_2015, desc(ip_counts_2015$count)), 6)
ip_2015$Year <- "FY2015"
ip_2015
## issuer_precinct   count    Year
##              0  1669892  FY2015
##             19   544705  FY2015
##             18   366772  FY2015
##             14   355166  FY2015
##              1   296478  FY2015
##            114   293277  FY2015


ip_counts_2016 <- summarize(groupBy(nyc_fy2016, nyc_fy2016$issuer_precinct), count = n(nyc_fy2016$issuer_precinct))
ip_2016 <- head(arrange(ip_counts_2016, desc(ip_counts_2016$count)), 6)
ip_2016$Year <- "FY2016"
ip_2016
## issuer_precinct    count    Year
##               0  2255803  FY2016
##              19   523530  FY2016
##              18   297434  FY2016
##              14   293425  FY2016
##               1   287829  FY2016
##              13   280263  FY2016

ip_counts_2017 <- summarize(groupBy(nyc_fy2017, nyc_fy2017$issuer_precinct), count = n(nyc_fy2017$issuer_precinct))
ip_2017 <- head(arrange(ip_counts_2017, desc(ip_counts_2017$count)), 6)
ip_2017$Year <- "FY2017"
ip_2017
## issuer_precinct    count    Year
##               0  1755324  FY2017
##              19   383991  FY2017
##              14   270229  FY2017
##               1   244489  FY2017
##              18   225233  FY2017
##             114   218268  FY2017

## Combining all three years data into single data frame and plotting the data for comparision
ip_count <- rbind(ip_2015, ip_2016, ip_2017)

ggplot(ip_count, aes(as.factor(issuer_precinct))) + 
  geom_col(aes(y = count, fill = count)) + 
  ggtitle("Plot 7: Issuer Precint") + 
  labs(x="Issuer Zone", y="Count") +
  facet_wrap(~Year) + 
  geom_text(aes(as.factor(issuer_precinct), count, label=count),vjust=-0.3)

## .	"0" is the issuer precinct which recorded highest number of tickets. However 0 is not a valid code, so excluding the same from analysis.
## .	Issuer precinct "19" recorded highest number of tickets across three years.
## .	Issuer precinct "18" recorded second highest number of tickets for FY2015 and 16, while precinct "14" recorded second highest for FY2017.
## .	Issuer precinct "14" recorded third highest number of tickets for FY2015 and 16, while precinct "1" recorded third highest for FY2017.
## .	Issuer precinct "1" recorded fourth highest number of tickets for FY2015 and 16, while precinct "18" recorded fourth highest for FY2017.
## .	Issuer precinct "114" recorded fourth highest number of tickets for FY2015 and 17, while precinct "13" recorded fourth highest for FY2016.


## Question 4: Find the violation code frequency across three precincts which have issued the most number of tickets - 
## do these precinct zones have an exceptionally high frequency of certain violation codes? Are these codes common across precincts?

vc_2015<- SparkR::sql("SELECT violation_code, count(*)as tickets, issuer_precinct
                      from mrk_fy2015
                      where issuer_precinct IN (19,18,14)
                      group by violation_code, issuer_precinct
                      order by tickets desc")
head(vc_2015, nrow(vc_2015))
head(vc_2015)
## violation_code  tickets  issuer_precinct                                        
##           14    114496              18
##           38     85453              19
##           37     77840              19
##           69     77246              14
##           14     73794              14
##           14     62032              19

vc_2016<- SparkR::sql("SELECT violation_code, count(*) as tickets, issuer_precinct
                      from mrk_fy2016
                      where issuer_precinct IN (19,18,14)
                      group by violation_code, issuer_precinct
                      order by tickets desc")
head(vc_2016, nrow(vc_2016))
head(vc_2016)
## violation_code tickets issuer_precinct                                        
##         14      91931              18
##         38      74159              19
##         37      74044              19
##         46      74027              19
##         69      61577              14
##         14      59077              19

vc_2017<- SparkR::sql("SELECT violation_code, count(*) as tickets, issuer_precinct
                      from mrk_fy2017
                      where issuer_precinct IN (19,14,1)
                      group by violation_code, issuer_precinct
                      order by tickets desc")
head(vc_2017, nrow(vc_2017))
head(vc_2017)
## violation_code tickets issuer_precinct                                        
##          46     65062              19
##          14     59907              14
##          14     55446               1
##          38     54146              19
##          37     53131              19
##          69     42521              14


## Question 5: find out the properties of parking violations across different times of the day

## 5a: Find a way to deal with missing values, if any
vt_fy2015<- SparkR::sql("select count(*) as null_violation_time from mrk_fy2015 where violation_time is NULL")
head(vt_fy2015) 
## There are null values in violation time in FY2015
nyc_fy2015_new <- subset(nyc_fy2015, isNotNull(nyc_fy2015$violation_time))
nrow(nyc_fy2015_new) #10537447 records after dropping null values from 2015 data

vt_fy2016<- SparkR::sql("select count(*) as null_violation_time from mrk_fy2016 where violation_time is NULL")
head(vt_fy2016) 
## There are null values in violation time in FY2016
nyc_fy2016_new <- subset(nyc_fy2016, isNotNull(nyc_fy2016$violation_time))
nrow(nyc_fy2016_new) #10391878 records after dropping null values from 2016 data

vt_fy2017<- SparkR::sql("select count(*) as null_violation_time from mrk_fy2017 where violation_time is NULL")
head(vt_fy2017) 
## There are null values in violation time in FY2015
nyc_fy2017_new <- subset(nyc_fy2017, isNotNull(nyc_fy2017$violation_time))
nrow(nyc_fy2017_new) # 8080077 records after dropping null values from 2017 data

## 5b: The Violation Time field is specified in a strange format. Find a way to make this into a time attribute that you can use to divide into groups.
str(nyc_fy2015_new)
str(nyc_fy2016_new)
str(nyc_fy2017_new)
## it is observed that violation time column in char with values "0953A" "0520P" "0545P" etc.
## Assumption it is understood that first two characters are hours, next characted are minutes
## Also, A in the end is considered as AM and P as PM

##converting A to AM and P to PM
nyc_fy2015_new$AMPM <- "M"
nyc_fy2015_new$violation_time <- concat(nyc_fy2015_new$violation_time, nyc_fy2015_new$AMPM)  
nyc_fy2015_new <- drop(nyc_fy2015_new, c("AMPM"))
str(nyc_fy2015_new)

nyc_fy2016_new$AMPM <- "M"
nyc_fy2016_new$violation_time <- concat(nyc_fy2016_new$violation_time, nyc_fy2016_new$AMPM)  
nyc_fy2016_new <- drop(nyc_fy2016_new, c("AMPM"))
str(nyc_fy2016_new)

nyc_fy2017_new$AMPM <- "M"
nyc_fy2017_new$violation_time <- concat(nyc_fy2017_new$violation_time, nyc_fy2017_new$AMPM)  
nyc_fy2017_new <- drop(nyc_fy2017_new, c("AMPM"))
str(nyc_fy2017_new)

## Also observed some records are observed as 12XXAMM, converting 12XXAM to 00XXAM
nyc_fy2015_new$violation_Hour <- substr(nyc_fy2015_new$violation_time, 1, 2)
nyc_fy2015_new$violation_minute <- substr(nyc_fy2015_new$violation_time, 3, 4)
nyc_fy2015_new$violation_AMPM <- substr(nyc_fy2015_new$violation_time, 5, 6)
nyc_fy2015_new$violation_Hour <- ifelse(nyc_fy2015_new$violation_Hour == "12" & nyc_fy2015_new$violation_AMPM == "AM", "00", nyc_fy2015_new$violation_Hour)
nyc_fy2015_new$violation_time <- concat(nyc_fy2015_new$violation_Hour, nyc_fy2015_new$violation_minute, nyc_fy2015_new$violation_AMPM)
nyc_fy2015_new$violation_time <- to_timestamp(x = nyc_fy2015_new$violation_time, format = "hhmma")
nyc_fy2015_new <- drop(nyc_fy2015_new, c("violation_Hour", "violation_minute", "violation_AMPM"))
str(nyc_fy2015_new)


nyc_fy2016_new$violation_Hour <- substr(nyc_fy2016_new$violation_time, 1, 2)
nyc_fy2016_new$violation_minute <- substr(nyc_fy2016_new$violation_time, 3, 4)
nyc_fy2016_new$violation_AMPM <- substr(nyc_fy2016_new$violation_time, 5, 6)
nyc_fy2016_new$violation_Hour <- ifelse(nyc_fy2016_new$violation_Hour == "12" & nyc_fy2016_new$violation_AMPM == "AM", "00", nyc_fy2016_new$violation_Hour)
nyc_fy2016_new$violation_time <- concat(nyc_fy2016_new$violation_Hour, nyc_fy2016_new$violation_minute, nyc_fy2016_new$violation_AMPM)
nyc_fy2016_new$violation_time <- to_timestamp(x = nyc_fy2016_new$violation_time, format = "hhmma")
nyc_fy2016_new <- drop(nyc_fy2016_new, c("violation_Hour", "violation_minute", "violation_AMPM"))
str(nyc_fy2016_new)

nyc_fy2017_new$violation_Hour <- substr(nyc_fy2017_new$violation_time, 1, 2)
nyc_fy2017_new$violation_minute <- substr(nyc_fy2017_new$violation_time, 3, 4)
nyc_fy2017_new$violation_AMPM <- substr(nyc_fy2017_new$violation_time, 5, 6)
nyc_fy2017_new$violation_Hour <- ifelse(nyc_fy2017_new$violation_Hour == "12" & nyc_fy2017_new$violation_AMPM == "AM", "00", nyc_fy2017_new$violation_Hour)
nyc_fy2017_new$violation_time <- concat(nyc_fy2017_new$violation_Hour, nyc_fy2017_new$violation_minute, nyc_fy2017_new$violation_AMPM)
nyc_fy2017_new$violation_time <- to_timestamp(x = nyc_fy2017_new$violation_time, format = "hhmma")
nyc_fy2017_new <- drop(nyc_fy2017_new, c("violation_Hour", "violation_minute", "violation_AMPM"))
str(nyc_fy2017_new)


## deriving the hour from violation time and creating a new column to create bins
nyc_fy2015_new$vhour <- hour(nyc_fy2015_new$violation_time)
nyc_fy2016_new$vhour <- hour(nyc_fy2016_new$violation_time)
nyc_fy2017_new$vhour <- hour(nyc_fy2017_new$violation_time)

str(nyc_fy2015_new)
str(nyc_fy2016_new)
str(nyc_fy2017_new)

## Creating updated temp sql view
createOrReplaceTempView(nyc_fy2015_new, "mrknew_fy2015")
createOrReplaceTempView(nyc_fy2016_new, "mrknew_fy2016")
createOrReplaceTempView(nyc_fy2017_new, "mrknew_fy2017")


## Question 5c: Divide 24 hours into six equal discrete bins of time. The intervals you choose are at your discretion. For each of these groups, find the three most commonly occurring violations.
## Creating bins and bin wise analysis
## bin analysis for 2015
bin_2015 <- SparkR::sql("select violation_code,
                        CASE  when vhour between 0 and 3  THEN '0to3'\
                        when vhour between 4 and 7  THEN '4to7'\
                        when vhour between 8 and 11  THEN '8to11'\
                        when vhour between 12 and 15  THEN '12to15'\
                        when vhour between 16 and 19  THEN '16to19'\
                        ELSE '20to23' END  as bin_number
                        from mrknew_fy2015")
createOrReplaceTempView(bin_2015, "bin_2015")

bin_2015 <- SparkR::sql("select bin_number,
                        violation_code,
                        tickets
                        from (select bin_number,
                        violation_code,
                        tickets,
                        dense_rank() over (partition by bin_number order by tickets desc) Rnk
                        from (select bin_number, violation_code, count(*)as tickets
                        from bin_2015
                        group by bin_number,
                        violation_code))
                        where Rnk <= 3")

vc_bin_2015 <- data.frame(head(bin_2015, nrow(bin_2015)))

ggplot(vc_bin_2015, aes(x= as.factor(violation_code), y=tickets))+ 
  geom_col()+ facet_grid(~bin_number) + 
  xlab("Violation Code") + ylab("Tickets") + ggtitle("Plot 8. Bin wise top violation codees 2015") + 
  geom_text(aes(label=tickets),vjust=-0.3)


## bin analysis for 2016
bin_2016 <- SparkR::sql("select violation_code,
                        CASE  when vhour between 0 and 3  THEN '0to3'\
                        when vhour between 4 and 7  THEN '4to7'\
                        when vhour between 8 and 11  THEN '8to11'\
                        when vhour between 12 and 15  THEN '12to15'\
                        when vhour between 16 and 19  THEN '16to19'\
                        ELSE '20to23' END  as bin_number
                        from mrknew_fy2016")
createOrReplaceTempView(bin_2016, "bin_2016")

bin_2016 <- SparkR::sql("select bin_number,
                        violation_code,
                        tickets
                        from (select bin_number,
                        violation_code,
                        tickets,
                        dense_rank() over (partition by bin_number order by tickets desc) Rnk
                        from (select bin_number, violation_code, count(*)as tickets
                        from bin_2016
                        group by bin_number,
                        violation_code))
                        where Rnk <= 3")

vc_bin_2016 <- data.frame(head(bin_2016, nrow(bin_2016)))

ggplot(vc_bin_2016, aes(x= as.factor(violation_code), y=tickets))+ 
  geom_col()+ facet_grid(~bin_number) + 
  xlab("Violation Code") + ylab("Tickets") + ggtitle("Plot 9. Bin wise top violation codees 2016") + 
  geom_text(aes(label=tickets),vjust=-0.3)

## FY2017 Analysis
bin_2017 <- SparkR::sql("select violation_code,
                        CASE  when vhour between 0 and 3  THEN '0to3'\
                        when vhour between 4 and 7  THEN '4to7'\
                        when vhour between 8 and 11  THEN '8to11'\
                        when vhour between 12 and 15  THEN '12to15'\
                        when vhour between 16 and 19  THEN '16to19'\
                        ELSE '20to23' END  as bin_number
                        from mrknew_fy2017")
createOrReplaceTempView(bin_2017, "bin_2017")

bin_2017 <- SparkR::sql("select bin_number,
                        violation_code,
                        tickets
                        from (select bin_number,
                        violation_code,
                        tickets,
                        dense_rank() over (partition by bin_number order by tickets desc) Rnk
                        from (select bin_number, violation_code, count(*)as tickets
                        from bin_2017
                        group by bin_number,
                        violation_code))
                        where Rnk <= 3")

vc_bin_2017 <- data.frame(head(bin_2017, nrow(bin_2017)))

ggplot(vc_bin_2017, aes(x= as.factor(violation_code), y=tickets))+ 
  geom_col()+ facet_grid(~bin_number) + 
  xlab("Violation Code") + ylab("Tickets") + ggtitle("Plot 10. Bin wise top violation codes 2017") + 
  geom_text(aes(label=tickets),vjust=-0.3)


## Question 5d: For the 3 most commonly occurring violation codes, find the most common time of the day (in terms of the bins from the previous part)

## Fy2015 analysis

vc3_fy15 <- SparkR::sql("select violation_code, count(*) as tickets from mrknew_fy2015 group by violation_code order by tickets desc")
head(vc3_fy15)
## 21, 38 and 14 are 3 most commonly occuring violation codes for FY2015.
ct_2015 <- SparkR::sql("select violation_code, bin_number, count(*) as tickets 
                       from bin_2015
                       where violation_code in (21,38,14)
                       group by violation_code, bin_number
                       order by violation_code, bin_number, tickets desc")

ct_fy2015 <- data.frame(head(ct_2015, nrow(ct_2015)))

ggplot(ct_fy2015, aes(x= as.factor(bin_number), y=tickets))+ geom_col() + 
  facet_grid(~violation_code) + xlab("Hour Bin") + ylab("Tickets") + 
  ggtitle("Plot11 -  Violation code wise bin analysis FY2015") + 
  geom_text(aes(label=tickets),vjust=-0.3)


## FY2016 Analysis
vc3_fy16 <- SparkR::sql("select violation_code, count(*) as tickets from mrknew_fy2016 group by violation_code order by tickets desc")
head(vc3_fy16)
## 21, 36 and 38 are 3 most commonly occuring violation codes for FY2016.
ct_2016 <- SparkR::sql("select violation_code, bin_number, count(*) as tickets 
                       from bin_2016
                       where violation_code in (21,36,38)
                       group by violation_code, bin_number
                       order by violation_code, bin_number, tickets desc")

ct_fy2016 <- data.frame(head(ct_2016, nrow(ct_2016)))

ggplot(ct_fy2016, aes(x= as.factor(bin_number), y=tickets))+ geom_col() + 
  facet_grid(~violation_code) + xlab("Hour Bin") + ylab("Tickets") + 
  ggtitle("Plot12 -  Violation code wise bin analysis FY2016") + 
  geom_text(aes(label=tickets),vjust=-0.3)

##FY2017 Analysis
vc3_fy17 <- SparkR::sql("select violation_code, count(*) as tickets from mrknew_fy2017 group by violation_code order by tickets desc")
head(vc3_fy17)
## 21, 36 and 38 are 3 most commonly occuring violation codes for FY2016.
ct_2017 <- SparkR::sql("select violation_code, bin_number, count(*) as tickets 
                       from bin_2017
                       where violation_code in (21,36,38)
                       group by violation_code, bin_number
                       order by violation_code, bin_number, tickets desc")

ct_fy2017 <- data.frame(head(ct_2017, nrow(ct_2017)))

ggplot(ct_fy2017, aes(x= as.factor(bin_number), y=tickets))+ geom_col() + 
  facet_grid(~violation_code) + xlab("Hour Bin") + ylab("Tickets") + 
  ggtitle("Plot13 -  Violation code wise bin analysis FY2017") + 
  geom_text(aes(label=tickets),vjust=-0.3)

## Q6.Let's try and find some seasonality in this data
#First, divide the year into some number of seasons, and find frequencies of tickets for each season. 
#(Hint: Use Issue Date to segregate into seasons)
#Then, find the three most common violations for each of these seasons.
# Q6.(a) season vs count of tickets analysis
colnames(nyc_fy2015_new)
nyc_fy2015_new$issue_month<-month(nyc_fy2015_new$issue_date)
nyc_fy2016_new$issue_month<-month(nyc_fy2016_new$issue_date)
nyc_fy2017_new$issue_month<-month(nyc_fy2017_new$issue_date)

## Creating updated temp sql view
createOrReplaceTempView(nyc_fy2015_new, "mrknew_fy2015")
createOrReplaceTempView(nyc_fy2016_new, "mrknew_fy2016")
createOrReplaceTempView(nyc_fy2017_new, "mrknew_fy2017")

# 2015 Anaysis season
season_2015 <- SparkR::sql("select violation_code,
                           CASE  when issue_month IN (1,2,12)  THEN 'winter'\
                           when issue_month between 3 and 5  THEN 'spring'\
                           when issue_month between 6 and 8  THEN 'summer'\
                           when issue_month between 9 and 12  THEN 'fall'\
                           END  as season
                           from mrknew_fy2015")
createOrReplaceTempView(season_2015, "season_2015")

season_2015 <- SparkR::sql("select season,count(*) as tickets
                           from season_2015
                           group by season
                           order by tickets desc")
tickets_2015<- data.frame(head(season_2015))
tickets_2015$year<- c(2015,2015,2015,2015)
tickets_2015

# 2016 Anaysis season
season_2016 <- SparkR::sql("select violation_code,
                           CASE  when issue_month IN (1,2,12)  THEN 'winter'\
                           when issue_month between 3 and 5  THEN 'spring'\
                           when issue_month between 6 and 8  THEN 'summer'\
                           when issue_month between 9 and 12  THEN 'fall'\
                           END  as season
                           from mrknew_fy2016")
createOrReplaceTempView(season_2016, "season_2016")

season_2016 <- SparkR::sql("select season,count(*) as tickets
                           from season_2016
                           group by season
                           order by tickets desc")
tickets_2016<- data.frame(head(season_2016))
tickets_2016$year<- c(2016,2016,2016,2016)
tickets_2016


# 2017 Anaysis season
season_2017 <- SparkR::sql("select violation_code,
                           CASE  when issue_month IN (1,2,12)  THEN 'winter'\
                           when issue_month between 3 and 5  THEN 'spring'\
                           when issue_month between 6 and 8  THEN 'summer'\
                           when issue_month between 9 and 12  THEN 'fall'\
                           END  as season
                           from mrknew_fy2017")
createOrReplaceTempView(season_2017, "season_2017")

season_2017 <- SparkR::sql("select season,count(*) as tickets
                           from season_2017
                           group by season
                           order by tickets desc")
tickets_2017<- data.frame(head(season_2017))
tickets_2017$year<- c(2017,2017,2017,2017)
tickets_2017

#Comparison of Season vs. Frequency of Tickets yearwise
season_combined_tickets<- rbind(tickets_2015, tickets_2016,tickets_2017)

ggplot(season_combined_tickets, aes(x= as.factor(season), y=tickets))+ geom_col()+ facet_grid(~year) + 
  xlab("Seasons of Year") + ylab("Tickets") + ggtitle("Plot14.Seasons vs. Frequency of Tickets") + 
  geom_text(aes(label=tickets),vjust=-0.3)


# Q6 (b) :season vs. violation)_code Analysis
# For year 2015
season_2015 <- SparkR::sql("select season,
                           violation_code,
                           tickets
                           from (select season,
                           violation_code,
                           tickets,
                           dense_rank() over (partition by season order by tickets desc) Rnk
                           from (select season, violation_code, count(*)as tickets
                           from season_2015
                           group by season,
                           violation_code))
                           where Rnk <= 3")

vc_season_2015 <- data.frame(head(season_2015, nrow(season_2015)))
#seasonwise Violation Code Distribution 2015
ggplot(vc_season_2015, aes(x= as.factor(violation_code), y=tickets))+ geom_col()+ 
  facet_grid(~season) + xlab("Violation Code") + ylab("Tickets") + ggtitle("Plot15. 2015 Seasons vs. Frequency of Violation Codes") +
  geom_text(aes(label=tickets),vjust=-0.3)


# For year 2016
season_2016 <- SparkR::sql("select season,
                           violation_code,
                           tickets
                           from (select season,
                           violation_code,
                           tickets,
                           dense_rank() over (partition by season order by tickets desc) Rnk
                           from (select season, violation_code, count(*)as tickets
                           from season_2016
                           group by season,
                           violation_code))
                           where Rnk <= 3")

vc_season_2016 <- data.frame(head(season_2016, nrow(season_2016)))
#seasonwise Violation Code Distribution 2016
ggplot(vc_season_2016, aes(x= as.factor(violation_code), y=tickets))+ geom_col()+ 
  facet_grid(~season) + xlab("Violation Code") + ylab("Tickets") + ggtitle("Plot16. 2016 Seasons vs. Frequency of Violation Codes") +
  geom_text(aes(label=tickets),vjust=-0.3)


# For year 2017
season_2017 <- SparkR::sql("select season,
                           violation_code,
                           tickets
                           from (select season,
                           violation_code,
                           tickets,
                           dense_rank() over (partition by season order by tickets desc) Rnk
                           from (select season, violation_code, count(*)as tickets
                           from season_2017
                           group by season,
                           violation_code))
                           where Rnk <= 3")

vc_season_2017 <- data.frame(head(season_2017, nrow(season_2017)))
#seasonwise Violation Code Distribution 2016
ggplot(vc_season_2017, aes(x= as.factor(violation_code), y=tickets))+ geom_col()+ 
  facet_grid(~season) + xlab("Violation Code") + ylab("Tickets") + ggtitle("Plot17. 2017 Seasons vs. Frequency of Violation Codes") +
  geom_text(aes(label=tickets),vjust=-0.3)


## Q7.Find total occurrences of the three most common violation codes
#find the total amount collected for the three violation codes with maximum tickets. 
#State the code which has the highest total collection.
#What can you intuitively infer from these findings?
# For Year 2015
violations_2015<- SparkR::sql("SELECT violation_code, count(*)as tickets
                              from mrknew_fy2015 
                              group by violation_code
                              order by tickets desc")
head(violations_2015,3)
# Top 3 violations are:
#   violation_code tickets                                                        
#1            21 1474695
#2            38 1261773
#3            14  908758
fine_top3_2015<- data.frame(head(violations_2015,3))
fine_top3_2015$year <- c(2015,2015,2015)
fine_top3_2015$Avg_fine_per_ticket<- c(55,50,115)
fine_top3_2015$Total_fine_Amount<- fine_top3_2015$tickets * fine_top3_2015$Avg_fine_per_ticket
fine_top3_2015
# violation_code  tickets year Avg_fine_per_ticket Total_fine_Amount
#1             21 1474774 2015                  55          81112570
#2             38 1261793 2015                  50          63089650
#3             14  908797 2015                 115         104511655

# For year 2016
violations_2016<- SparkR::sql("SELECT violation_code, count(*)as tickets
                              from mrknew_fy2016 
                              group by violation_code
                              order by tickets desc")
head(violations_2016,3)
# Top 3 violations are:
#violation_code tickets                                                        
#1             21 1506923
#2             36 1344463
#3             38 1078318

fine_top3_2016<- data.frame(head(violations_2016,3))
fine_top3_2016$year <- c(2016,2016,2016)
fine_top3_2016$Avg_fine_per_ticket<- c(55,50,50)
fine_top3_2016$Total_fine_Amount<- fine_top3_2016$tickets * fine_top3_2016$Avg_fine_per_ticket
fine_top3_2016
# violation_code tickets year Avg_fine_per_ticket Total_fine_Amount
#1             21 1506923 2016                  55          82880765
#2             36 1344463 2016                  50          67223150
#3             38 1078318 2016                  50          53915900

# For year 2017
violations_2017<- SparkR::sql("SELECT violation_code, count(*)as tickets
                                         from mrknew_fy2017 
                                         group by violation_code
                                         order by tickets desc")
head(violations_2017,3)
# Top 3 violations are:
# violation_code tickets                                                        
#1             21 1115585
#2             36 1105358
#3             38  805463
fine_top3_2017<- data.frame(head(violations_2017,3))
fine_top3_2017$year <- c(2017,2017,2017)
fine_top3_2017$Avg_fine_per_ticket<- c(55,50,50)
fine_top3_2017$Total_fine_Amount<- fine_top3_2017$tickets * fine_top3_2017$Avg_fine_per_ticket
fine_top3_2017

# violation_code tickets year Avg_fine_per_ticket Total_fine_Amount
#1             21 1115585 2017                  55          61357175
#2             36 1105358 2017                  50          55267900
#3             38  805463 2017                  50          40273150
fine_combined_top3<-rbind(fine_top3_2015,fine_top3_2016,fine_top3_2017)

ggplot(fine_combined_top3, aes(x=as.factor(violation_code), y=tickets))+ geom_col()+ 
  facet_grid(~year) + xlab("Violation Code") + ylab("Tickets") + 
  ggtitle("Plot18. Comparison of Top 3 Violation Code vs Tickets") + 
  geom_text(aes(label=tickets),vjust=-0.3)

ggplot(fine_combined_top3, aes(x=as.factor(violation_code), y=Total_fine_Amount))+
  geom_col()+ facet_grid(~year) + xlab("Violation Code") + ylab("Total Fine Amount") +
  ggtitle("Plot19.Top 3 Violation Code vs Total Fine Amount") + 
  geom_text(aes(label=Total_fine_Amount),vjust=-0.3)

# Stopping SparkR Session
sparkR.stop()
