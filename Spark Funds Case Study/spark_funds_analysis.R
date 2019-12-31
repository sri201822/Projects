library(dplyr)
library(tidyr)
library(stringr)


# Loading and Cleaning Data
companies <- read.delim("companies.txt", header = TRUE, stringsAsFactors = FALSE)
str(companies)
names(companies)

rounds2 <- read.csv("rounds2.csv", stringsAsFactors = FALSE)
str(rounds2)
names(rounds2)

# Converting the unique ID of company permalink in both data frames to lower case as R is case sensitive
companies$permalink <- tolower(companies$permalink)
rounds2$company_permalink <- tolower(rounds2$company_permalink)


#Checkpoint 1: Data Cleaning 1
## Table 1.1 - Understanding the dataset
### 1. How many unique companies are present in the companies file?
count(distinct(companies, permalink)) #66368


### 2. How many unique companies are present in the rounds file?
count(distinct(rounds2, company_permalink)) #66368


### 3. In the companies data frame, which column can be used as the  unique key for each company? Write the name of the column.
### permalink

#renaming the permalink column to company_permalink in companies data frame to enable simple merging
names(companies)[1] <- "company_permalink"
names(companies)


### 4. Are there any companies in the rounds2 file which are not present in companies ? 
sum(!is.element(rounds2$company_permalink, companies$company_permalink)) # 0

### 5. Merge the two data frames so that all variables (columns) in the companies frame are added to the rounds2 data frame.
###    Name the merged frame master_frame.
###   How many observations are present in master_frame 
master_frame <- merge(rounds2, companies, by = "company_permalink")
names(master_frame)


#Checkpoint 2: Funding Type Analysis
## Table 2.1 - Average Values of Investments for Each of Funding Types

## checking for "NA" values in raised amount and % of NA values in total data of raised amount
sum(is.na(master_frame$raised_amount_usd))/nrow(master_frame)

## Assumption 1: as the % of NA is 17% which is a significant amount of data, replacing NA with average of funds raised
master_frame$raised_amount_usd[is.na(master_frame$raised_amount_usd)] <- mean(master_frame$raised_amount_usd, na.rm = TRUE)

### Using "group_by" function for Grouping the master_frame based in funding type
funding_type <- group_by(master_frame, funding_round_type)

### Using "summarise" function for calculating the fund wise average fund invested
funding_type <- summarise(funding_type, avg_fund = mean(raised_amount_usd, na.rm = TRUE))

### Using "arrange" function to sort the fund wise investments in decreasing order
funding_type <- arrange(funding_type, desc(avg_fund))

### Using filter function to filter only venture, angel, seed and private equity fund type
funding_type <- filter(funding_type, funding_round_type %in% c("venture", "angel", "seed", "private_equity"))

### Using filter funtion to which investment is suitable by applying condition of avg fund >= 5000000 and <= 15000000
filter(funding_type, avg_fund >= 5000000 & avg_fund <= 15000000)

# Checkpoint 3: Country Analysis
# Checking for Spread of date in country code
arrange(summarise(group_by(master_frame, country_code), count = sum(!is.na(country_code))), desc(count))
#Assumption 2:  From the summary, some of the country_code fields are blank, but the % is less than 10% so ignoring blank values

## Table 3.1 - Analysing the Top 3 English-Speaking Countries

### Using "group_by" function for Grouping based on country code
#Assumption 3: As the funding type is decided as Venture, creating filter on funding type as Venture and the same will be used for further analysis
country_grouping <- group_by(filter(master_frame, tolower(funding_round_type) == "venture"), country_code)

### Using "summarise" function for calculating the country wise total fund invested
country_grouping <- summarise(country_grouping, total_fund = sum(raised_amount_usd, na.rm = TRUE))

### Using "arrange" function to sort the country wise investments 
top9 <- head(arrange(country_grouping, desc(total_fund)),9)
top9
# Comparing the top9 with the english speaking country list shared, USA, GBR and IND are the top three english speaking countries in Top9


# Checkpoint 4: Sector Analysis 1
## Loading "mapping.csv" and converting the same from wide to long format using "gather" function
mapping <- read.csv("mapping.csv", stringsAsFactors = FALSE)
names(mapping)
mapping <- gather(mapping, main_sector, val, 2:10)
mapping <- mapping[!(mapping$val==0),]
mapping <- mapping[,-3]

## Correcting the category_list of mapping as "na" is replaced with "0"
mapping$category_list <- gsub("0", "na", mapping$category_list)
mapping$category_list <- gsub("2.na", "2.0", mapping$category_list)

## As the category_list contain values separated by "|", creating primary sector by using "separate" function
master_frame <- separate(master_frame, category_list, into=c("primary_sector", "sec_sector"), sep="\\|")
master_frame <- select(master_frame, -sec_sector)
master_frame$primary_sector <- tolower(master_frame$primary_sector)


## Renaming "category_list" to "primary_sector" in mapping dataframe for easy merger
names(mapping)[1] <- "primary_sector"
mapping$primary_sector <- tolower(mapping$primary_sector)


## Merged data frame with each primary sector mapped to its main sector 
sum(is.na(master_frame$primary_sector))


master_frame <- merge(master_frame, mapping, by = "primary_sector",all.x = T)

sum(is.na(master_frame$main_sector))/nrow(master_frame)
### Assumption 4: As there are some primary sectors which are available in master_frame but not in mapping. 
### However, the count of investments in those primary_sectors is < 1%, hence ignoring the same and assigning the main_sector as "Blanks".
master_frame$main_sector[is.na(master_frame$main_sector)] <- "Blanks"


# Checkpoint 5: Sector Analysis 2
## Create three separate data frames D1, D2 and D3 for each of the three countries containing the observations of funding type FT falling within the 5-15 million USD range.
## To acheive this, filtered the master_frame by selecting top 3 countries from top9 data frame and applied additional constraint that raised_amount_usd is between 5 million and 15 million.
D1 <- filter(master_frame, country_code == "USA", (tolower(funding_round_type) == "venture") & (raised_amount_usd>=5000000 & raised_amount_usd <= 15000000))
D2 <- filter(master_frame, country_code == "GBR", (tolower(funding_round_type) == "venture") & (raised_amount_usd>=5000000 & raised_amount_usd <= 15000000))
D3 <- filter(master_frame, country_code == "IND", (tolower(funding_round_type) == "venture") & (raised_amount_usd>=5000000 & raised_amount_usd <= 15000000))

## The total number (or count) of investments for each main sector in a separate column
## The total amount invested in each main sector in a separate column
## To acheive this grouping the three data frames by "main_sector" and summarizing the same based on count of investment and total sum of investment i.e., funds raised
## Arranged the data in descending order of count of investments and total investments
D1_inv <- group_by(D1, main_sector)
D1_inv <- summarise(D1_inv, countof_inv = sum(!is.na(main_sector)), total_inv = sum(raised_amount_usd))
D1_inv <- arrange(D1_inv, desc(countof_inv), desc(total_inv))
D1 <- merge(D1, D1_inv, by ="main_sector")

D2_inv <- group_by(D2, main_sector)
D2_inv <- summarise(D2_inv, countof_inv = sum(!is.na(main_sector)), total_inv = sum(raised_amount_usd))
D2_inv <- arrange(D2_inv, desc(countof_inv), desc(total_inv))
D2 <- merge(D2, D2_inv, by ="main_sector")

D3_inv <- group_by(D3, main_sector)
D3_inv <- summarise(D3_inv, countof_inv = sum(!is.na(main_sector)), total_inv = sum(raised_amount_usd))
D3_inv <- arrange(D3_inv, desc(countof_inv), desc(total_inv))
D3 <- merge(D3, D3_inv, by ="main_sector")

### Total number of Investments (count) in C1, C2, C3
sum(D1_inv$countof_inv) # or nrow(D1)  - 14383
sum(D2_inv$countof_inv) # or nrow(D2)  - 876
sum(D3_inv$countof_inv) # or nrow(D3)  - 498

### Total amount of investment (USD)
sum(D1_inv$total_inv) # or sum(D1$raised_amount_usd)  - 131814546739
sum(D2_inv$total_inv) # or sum(D2$raised_amount_usd)  - 8022707134
sum(D3_inv$total_inv) # or sum(D3$raised_amount_usd)  - 4728257650

### Top Sector name (no. of investment-wise)
### Second Sector name (no. of investment-wise)
### Third Sector name (no. of investment-wise)
head(D1_inv, 3)
head(D2_inv, 3)
head(D3_inv, 3)

### For point 3 (top sector count-wise), which company received the highest investment?
### Filtered D1, D2 and D3 based on main sector which is highest invested, 
### then grouped the same based on  company name and then arranged in decreasing order sum of raised amount
arrange(summarise(group_by(filter(D1, main_sector %in% D1_inv[1,1]), tolower(name)), fund = sum(raised_amount_usd)), desc(fund))[1, ]
arrange(summarise(group_by(filter(D2, main_sector %in% D2_inv[1,1]), tolower(name)), fund = sum(raised_amount_usd)), desc(fund))[1, ]
arrange(summarise(group_by(filter(D3, main_sector %in% D3_inv[1,1]), tolower(name)), fund = sum(raised_amount_usd)), desc(fund))[1, ]

### For point 4 (second best sector count-wise), which company received the highest investment? solution similar to above point
arrange(summarise(group_by(filter(D1, main_sector %in% D1_inv[2,1]), tolower(name)), fund = sum(raised_amount_usd)), desc(fund))[1, ]
arrange(summarise(group_by(filter(D2, main_sector %in% D2_inv[2,1]), tolower(name)), fund = sum(raised_amount_usd)), desc(fund))[1, ]
arrange(summarise(group_by(filter(D3, main_sector %in% D3_inv[2,1]), tolower(name)), fund = sum(raised_amount_usd)), desc(fund))[1, ]

#Exporting final master_frame to CSV file to use in tableau plots
write.csv(master_frame, "master_frame.csv")
