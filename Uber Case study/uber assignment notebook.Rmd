### Uber case study

requiredPackages = c('tidyr','dplyr','gridExtra','ggplot2','lubridate')

# used these packages below during analysis

#Installing the required packages if not already installed and activating all packages using library()

for(item in requiredPackages){
  if(!require(item,character.only = TRUE)){
    install.packages(item)
  }
  library(item, character.only = TRUE)
}

# All the pacakges required are installed and activated.

# set the working directory and load the file 'Uber Request Data.csv'.
# loading file and saving it with name uber

uber <- read.csv('Uber Request Data.csv',stringsAsFactors = FALSE)

#
##########################Basic Analysis#########################################
#

dim(uber) # 6745 rows and 6 columns
str(uber) # metadata of the dataset
head(uber)
summary(uber) # basic statistics

sum(!complete.cases(uber)) # there are 3914 incomplete cases in the dataset
sum(is.na(uber)) # there are 6564 NA values


colSums(is.na(uber)) # To check which columns have NA values

# It means Driver.id, Drop.timestamp have NA values

# Let's check if these NA values are significant or not

uber[which(is.na(uber$Driver.id)),c('Driver.id','Status')]

# when Driver.id is NA status is "No Cars Available"

# Let's count status of No cars Available 

table(uber[which(is.na(uber$Driver.id)),'Status'])    # No cars Available-2650

sum(uber$Status=='No Cars Available') # 2650

# It means No cars Available status is shown only for Diver.id is NA

# NA values are significant as they indicate that wherever no driver id there status is No cars Available
# It's representing some information so we can't ignore NAs

# Let's check for Drop.timestamp column
sum(is.na(uber$Drop.timestamp)) # 3914

table(uber[which(is.na(uber$Drop.timestamp)),'Status']) # cancelled-1264     No Cars Available-2650
sum(uber$Status=='Cancelled') #1264

# It means Status of trip is cancelled means Drop.timestamp value is NA
# For the rows having Drop.timestamp Status is Trip Completed
# and Status is No cars Available when Driver.id is NA
# so don't replace or impute any statistical measure in place of NAs

#
#############################Data Cleaning#########################################################
#

# we can see that pickout.point is having only 2 options : Airport,City.converting it into factor
# Status has 3 values: Cancelled, No cars Available,Trip completed. converting to factor
# Driver.id is from 1 to 300. Converting it into factor
# we are converting Status,Pickup.point,Driver.id into factors as they give better useful insights.

uber$Driver.id<- as.factor(uber$Driver.id)
uber$Pickup.point <- as.factor(uber$Pickup.point)

uber$Status <- as.factor(uber$Status)
str(uber)

#
############################Changing Date Time Formats##############################################
#

# we can see that Request.timestamp, Drop.timestamp both of them are in date_time format
# we need to split date and time to get more information
# Formatting Request.timestamp,Drop.timestamp
# install.packages("lubridate")
library(lubridate)

# parse_date_time() is a function from "lubridate" package

uber$Drop.timestamp  <- parse_date_time(uber$Drop.timestamp, orders = c('dmy_HMS', "dmy_HM"))
uber$Request.timestamp <- parse_date_time(uber$Request.timestamp, orders = c('dmy_HMS', "dmy_HM"))  
# here there are 2 formats in the given dataset one with seconds and other without seconds
# parse_date_time() will check through both the formats as we have given parameters in orders


# Let's split date and time for more further analysis.
#
#################################Deriving new Variables###############################################
#
# Here I am splitting date and time using separate function from dplyr package
# separating date and time from Request.timestamp column
library(dplyr)
uber <- separate(uber, Request.timestamp,c("Request.Date","Request.Time"),sep = " ", remove = FALSE)

# separate date and time from Drop.timestamp as well.

uber <- separate(uber, Drop.timestamp,c("Drop.Date","Drop.Time"),sep = " ", remove = FALSE)

# Now separate hours from Request.Time column for further analysis use.

uber <- separate(uber,Request.Time,c("Request.Hour"),sep=":",remove=FALSE)

# similarly extract Drop.Hour

uber <- separate(uber,Drop.Time,c("Drop.Hour"),sep=":",remove=FALSE)

str(uber)

# Request hour is in character format convert it into numerical format

uber$Request.Hour<- as.numeric(uber$Request.Hour)



# Deriving Timeslots using Request.Hour column


#-------
# Early morning    4AM to 8AM
#-------
# Late morning     8AM to 12AM
#------- 
# Afternoon        12AM to 4PM
#-------
# Evening          4PM to 8PM
#-------
# Late Evening      8PM to 12PM 
#-------
# Mid Night        12PM to 4AM
#-------

# Let's create a new column Time_Slot by applying condition on Request.Hour column 
# Here I chose to break the entire 24hrs in 6 Timeslots

uber$Time_Slot[(uber$Request.Hour>24)|(uber$Request.Hour <= 4)] <- c("Mid Night")
uber$Time_Slot[((uber$Request.Hour >4) & (uber$Request.Hour <=8))] <-c("Early Morning")
uber$Time_Slot[((uber$Request.Hour >8) & (uber$Request.Hour <=12))] <-c("Late Morning")
uber$Time_Slot[((uber$Request.Hour > 12) & (uber$Request.Hour <= 16))] <- c("Afternoon")
uber$Time_Slot[((uber$Request.Hour >16) & (uber$Request.Hour <=20))] <- c("Evening")
uber$Time_Slot[((uber$Request.Hour >20) & (uber$Request.Hour <= 24))] <- c("Late Evening")

sum(is.na(uber$Time_Slot)) # All timeslots are created as desired


################ Derive Trip Duration

# Let's derive time duration- the time it takes to go from city to Airport or from Airport to City
# we get Trip Duration by subtracting Drop.timestamp and Request.timestamp

uber$Trip.duration <- as.numeric(round(difftime(uber$Drop.timestamp,
                            uber$Request.timestamp,units = "mins"),0))
# Here we got trip duration in minutes

# Derive weekday from the Request.timestamp column using wday()

uber$Request.weekday <-wday(uber$Request.timestamp, label = T)

# we have derived a new column Request.weekday which has Monday to Friday days assigned as per dates

View(uber)

# write.csv(uber,"uber_clean_final.csv") I used this cleaned uber file to plot graphs in Tableau
#
#######################################Univariate Analysis###################################
#

str(uber) # Total metadata of Uber dataset

#### Basic Meta data description (given uber file)

# Number of rows    :  6745
# Number of cols    :  6
# Request id        : A unique identifier of the request made.
#                     Type-int Missing : 0 Unique : 6745
# 
# Pickup.point      : The point from which the customer made the request
#                     Type-Character Missing :0 unique:2
# Driver id         : The unique identification number of the driver
#                     Type-int Missing : 2650 unique:300 
# Status            : The status that can be either completed, cancelled or no cars available
#                     Type- character Missing : 0 unique:3
# Request.timestamp : The time and date at which the request was made by the customer
#                     Type: character Missing:0 unique: 5618
# Drop.timestamp    : The time and date at which the trip is completed 
#                     Type: character Missing:3914 unique:2598
# This is the basic metadata but we have changed some of the data formats  for obtaining better insights
# Like we have changed Request.timestamp,Drop.timestamp to Date time format
# we have converted Driver.id,Pickup.point,Status into factors
# we derived new metrics like trip_duration,weekends,Request.hours,Drop.hours for better insights


# check for duplicates

# No duplicate rows
# there are duplicate column values
length(unique(uber$Request.id)) # 6745 no duplicate values
length(unique(uber$Pickup.point)) # 6743 duplicate values 
length(unique(uber$Driver.id)) #300---- No duplicates but there are NAs
length(unique(uber$Status)) # 6742 duplicate values
length(unique(uber$Request.timestamp))# 5618 unique 1127 duplicate values
length(unique(uber$Drop.timestamp))# 2599 unique 4146 duplicate values


# Total Requests- More for city/Airport
table(uber$Pickup.point)
# Airport-3238   city-3507
# we can see that Request with City as Pickup.point is more when compared to airport as Pickup.point

# Let's verify it by plot
# Requests at Pickup.Point

library(ggplot2)

Requests_uber <- ggplot(uber,aes(x=Pickup.point))+geom_bar(fill="Blue")+
                geom_text(stat = "count",aes(label=..count..),position = position_stack(vjust=0.5))+
                xlab("Pickup.Point")+ylab("Number of Requests")+ggtitle("Requests at Pickup.Point")+
                theme(plot.title = element_text(hjust = 0.5))
Requests_uber

# Airport-3238   city-3507
# we can see that there are many requests from city than from airport

# airport_demand plot hourwise (pickup.point is Airport)

airport <- subset(uber,uber$Pickup.point=="Airport")

airport_demand_plot <- ggplot(airport,aes(x=Request.Hour))+geom_bar(fill='blue')+
                        geom_text(stat="count",aes(label=..count..),position = position_stack(vjust=0.5))+
                        xlab('Hourwise Requests')+ylab('Number of Requests')+
                        ggtitle('Demand At Airport Hourwise')+theme(plot.title = element_text(hjust = 0.5))
airport_demand_plot

# we can obtain hourwise demand by using Request.Hour column

table(airport$Request.Hour) # used this just for verification

# we can see that demand is high at 18th hour 6PM.
# Number of Requests are 405

# city_demand plot hourwise (pickup.point is City)

city <-subset(uber,uber$Pickup.point=="City")

city_demand_plot <- ggplot(city,aes(x=Request.Hour))+geom_bar(fill='Blue')+
                     geom_text(stat="count",aes(label=..count..),position = position_stack(vjust=0.5))+
                     xlab('Hourwise Requests')+ylab('Number of Requests')+
                    ggtitle('Demand In City Hourwise')+theme(plot.title = element_text(hjust = 0.5))

city_demand_plot

# we can see that at city the highest demand is at 5 AM
# Number of Requests are 353

table(city$Request.Hour) # used this for verification

# Maximum value is at 5AM and requests are 353


# Number of Drivers Available

drivers_count <-  length(unique(na.omit(uber$Driver.id)))
drivers_count

# It means that there are only 300 drivers available.

# whenever Demand crosses more than 300 then we don't have enough drivers for trips.

# supply at airport hourwise

# Supply means successful requests is met it means status should be Trip Completed
# From the above basic analysis we can say that for status as Trip Completed we have Drop.Hour values
# which are not NAs. So by using Drop.Hour we can get hourwise supply.

# For hourwise supply we can analyse using drop.hour as it is not NA only when Status is Trip Completed

airport_supply_plot <- ggplot(subset(airport,!is.na(Drop.Hour)),aes(x=Drop.Hour))+geom_bar(fill = "Red") +
                        geom_text(stat="count",aes(label=..count..),position = position_stack(vjust=0.5))+
                        labs(title ="Supply at Airport Hourwise")+ylab('Number of Requests')+
                        theme(plot.title = element_text(hjust = 0.5))+ylim(0,500)

airport_supply_plot

# supply at city hourwise
city_supply_plot <- ggplot(subset(city,!is.na(Drop.Hour)),aes(Drop.Hour))+geom_bar(fill = "red") + 
                     geom_text(stat="count",aes(label=..count..),position = position_stack(vjust=0.5))+
                     labs(title ="Supply at City Hourwise")+ylab('Number of Requests')+
                     theme(plot.title = element_text(hjust = 0.5))+ylim(0,500)

city_supply_plot

# Let's have a look at all these 4 plots at once using grid.arrange() from gridExtra library
library(gridExtra)
# using this grid.arrange() we can get more than 1 plot in single plot.

grid.arrange(city_demand_plot,city_supply_plot,airport_demand_plot,airport_supply_plot,nrow=2,ncol=2)

# From the plot we can see that hourwise demand is very high and supply hourwise is very loss 
# Both in City and At Airport.

############## Segmented Univariate Analysis########################################################

# Overall Demand hour wise

uber_requests_hourwise <-ggplot(uber,aes(x=Request.Hour,fill=Pickup.point))+geom_bar()+
                          xlab('Hourwise Requests')+ylab('Number of Requests')+
                          geom_text(stat="count",aes(label=..count..),position = position_stack(vjust=0.5))+
                          ggtitle('Total Hourwise Demand')+theme(plot.title = element_text(hjust = 0.5))
uber_requests_hourwise

# we can see that more number of requests are in 18th hour.

uber_demand_hourwise <- as.data.frame(table(uber$Request.Hour))
colnames(uber_demand_hourwise)<-c("Hour","No. of Requests")
uber_demand_hourwise[which.max(uber_demand_hourwise$`No. of Requests`),]

# we can see that maximum requests occured at 6PM (18 hour in 24hrs format) and the no. of requests is 510.

# Overall supply hourwise 

uber_supply_hourwise <- ggplot(subset(uber,!is.na(Drop.Hour)),aes(Drop.Hour,fill=Pickup.point))+geom_bar()+ 
                         geom_text(stat="count",aes(label=..count..),position = position_stack(vjust=0.5))+
                         labs(title ="Total Supply Hourwise")+ylab('Number of Requests')+
                         theme(plot.title = element_text(hjust = 0.5))+ylim(0,500)
uber_supply_hourwise

# Let's plot Supply and Demand in single plot using grid.arrange()

grid.arrange(uber_requests_hourwise,uber_supply_hourwise)

# we can clearly see that demand is not met, supply is very less.


########### Timeslot wise analysis


uber_timeslot_plot <- ggplot(uber,aes(Time_Slot,fill=Pickup.point)) + geom_bar() +
                       geom_text(stat="count",aes(label=..count..),position = position_stack(vjust = 0.5))+
                       theme(legend.position = 1,axis.text = element_text(angle = 45, hjust = 1)) +
                       labs(title="Timeslot wise analysis") + ylab("Number of Requests")+
                       theme(plot.title = element_text(hjust = 0.5))

uber_timeslot_plot
# Blue - City
# Orange- Airport
# we can see that more requests are in Evening At Airport and Early Morning in City

# Let's see the status at the pickup.point

pickup.point_status<- ggplot(uber,aes(x=Status,fill=Pickup.point))+geom_bar()+
                       geom_text(stat = "count",aes(label=..count..),position= position_stack(vjust=0.5))+
                       xlab('Status')+ylab("Number of Requests")+ggtitle('Status at Pickup.point')+
                       theme(plot.title = element_text(hjust = 0.5))
pickup.point_status

# At Airport : No Cabs Available -1713 requests
# In city :  cancelled -1066 requests

# let's see the trip duration by plotting boxplot

Trip_duration <- ggplot(uber,aes(x=Pickup.point,y=Trip.duration))+geom_boxplot(na.rm=TRUE)+
                  ggtitle("Trip Duration")+
                  theme(plot.title=element_text(hjust=0.5))+ylab('Number of Minutes')
Trip_duration
summary(uber$Trip.duration)

#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  21.00   41.00   52.00   52.41   64.00   83.00    3914 

####################weekdays anlysis########################################################

# Let's see Demand during weekdays

weekdays_demand <- ggplot(uber,aes(x=Request.weekday,fill=Pickup.point))+geom_bar()+
                    geom_text(stat = "count",aes(label=..count..),position=position_stack(vjust=0.5))+
                    ylab('Number of Requests')+ggtitle('Demand during week days')+
                    theme(plot.title=element_text(hjust=0.5))
weekdays_demand

# In city- 752 requests during Thursday and Friday
# At Airport- 684 requests during Tuesday

### Let's see status during the weekdays with respect to Pickup.Points

weekdays_Status <- ggplot(uber,aes(x=Request.weekday,fill=Status))+geom_bar()+
                    geom_text(stat = "count",aes(label=..count..),position=position_stack(vjust=0.5))+
                    facet_grid(~Pickup.point)+ylab('Number of Requests')+ggtitle("Status at weekdays")+
                    theme(plot.title=element_text(hjust=0.5))
weekdays_Status

# Here we can see that No cars Available is the highest no.of requests at Airport.

# weekdays Timeslot wise analysis with respect to Pickup.Points

weekday_Timeslot <-ggplot(uber,aes(x=Request.weekday,fill=Pickup.point))+geom_bar()+
                    geom_text(stat = "count",aes(label=..count..),position=position_stack(vjust=0.5))+
                    facet_wrap(~Time_Slot)+ylab('Number of Requests')+
                    ggtitle('weekdays Timeslot wise Analysis')+
                    theme(plot.title=element_text(hjust=0.5))
weekday_Timeslot

# No.of Requests on Thursday from Airport during Evening is highest
# Similarly on Thursay from City during Early Morning is highest

weekday_Total_Status<- ggplot(uber,aes(x=Request.weekday,fill=Pickup.point))+geom_bar()+
                        facet_grid(Status~Time_Slot)+ylab('Number of Requests')+
                        ggtitle('weekdays Timeslot wise Status')+theme(plot.title=element_text(hjust=0.5))+
                        theme(legend.position = 1,axis.text = element_text(angle = 45, hjust = 1))
weekday_Total_Status
# Blue - City   Orange- Airport

# No cabs available Status is highest on Thursday with Airport as Pickup.Point
# we can see that Early morning status is mostly Cancelled with pickup.point as city irrespective of day
# table(uber$Request.weekday,uber$Time_Slot,uber$Status)

#___________________________________________________________________________________________________________
#
####################### Results Expected #####################################################
#
# 1. Visually identify the most pressing problems for uber
# Hint: Create plots to visualise the frequency of requests that get cancelled or show 'no cars available'; 
#
# identify the most problematic types of requests (city to airport / airport to city etc.) 
# and the time slots (early mornings, late evenings etc.) using plots

# Problematic request types are "cancelled" and "No Cars Available"

problematic_Requests <- ggplot(subset(uber,Status!="Trip Completed"),aes(x=Pickup.point, fill=Status)) + 
                         geom_bar() +geom_text(stat="count",aes(label=..count..),
                         position = position_stack(vjust = 0.5),check_overlap = T) +
                         facet_grid(Pickup.point~Time_Slot) +
                         labs(title = "Problem Request Types From city to Airport/Airport to city") +
                         ylab("Number of Requests")+
                         theme(legend.position = "bottom",axis.text = element_text(angle = 45, hjust = 1))
problematic_Requests

#Observations from the plot:

# At Airport:
# Problematic timeslots at the airport --> Evening   
# Problematic Request types: "No Cars available"

# In City:
#Problematic timeslots in city --> Early Morning
# Problematic request types: "cancelled"


#2.Find out the gap between supply and demand and show the same using plots.
#(a)Find the time slots when the highest gap exists
#(b)Find the types of requests (city-airport or airport-city)
#for which the gap is the most severe in the identified time slots

# Demand- when cab is required(Trip completed,Cancelled,No Cars Available)
# Supply- when cab is booked (Trip completed)
# Gap=Demand-Supply (Cancelled+No Cars Available)

# Let's create a new dataframe uber_demandSupply_gap by converting status from narrow format to wide format
# and then creating 3 new columns named demand,supply,gap using mutate() in dplyr package
# to convert narrow format to wide format we use spread() from tidyr package

library(tidyr)
uber_demandSupply_gap <- uber %>% 
  group_by(Pickup.point,Time_Slot,Status) %>% 
  count() %>%               #row count
  spread(Status,n)       # spread the status variables( Narrow to wide format)
# View(uber_demandSupply_gap)  

uber_demandSupply_gap <- uber_demandSupply_gap %>%
  mutate(demand=sum(Cancelled,`No Cars Available`,`Trip Completed`),supply=sum(`Trip Completed`),
         gap=demand-supply) %>% 
  arrange(desc(gap))

View(uber_demandSupply_gap)

# write.csv(uber_demandSupply_gap,"demand-supplygap.csv") later used to plot graphs in Tableau

###########Solution to  Q2 #######
# solution to Q2(a) Find the time slots when the highest gap exists
head(uber_demandSupply_gap,2)

# it seems highest gap is during Evening where pickup.point is Airport and 
# Early Morning where pickup.point is City.

#  Pickup.point Time_Slot     Cancelled `No Cars Available` `Trip Completed` demand supply   gap
#  <fct>        <chr>             <int>               <int>            <int>  <int>  <int> <int>
#1 Airport      Evening              78                1067              312   1457    312  1145
#2 City         Early Morning       653                 309              373   1335    373   962

# Let's confirm it through plots Demand,Supply,Gap plots

demand_plot<- ggplot(uber_demandSupply_gap, aes(x=Time_Slot,y=demand,fill=Pickup.point))+
               geom_bar(stat = "identity", width = 0.5, position = "dodge")+
               ggtitle('Demand')+
               theme(legend.position = "bottom",axis.text = element_text(angle = 45, hjust = 1))+
               theme(plot.title = element_text(hjust=0.5))+ylab('Demand')
demand_plot

# Demand is highest at Airport in Evening and In city in Early Morning.

supply_plot<- ggplot(uber_demandSupply_gap, aes(x=Time_Slot,y=supply,fill=Pickup.point))+
               geom_bar(stat = "identity", width = 0.5, position = "dodge")+ggtitle('Supply')+
               theme(legend.position = "bottom",axis.text = element_text(angle = 45, hjust = 1))+
               theme(plot.title = element_text(hjust=0.5))+ylab('Supply')+ylim(0,1500)
supply_plot

gap_plot <- ggplot(uber_demandSupply_gap, aes(x=Time_Slot,y=gap,fill=Pickup.point))+
             geom_bar(stat = "identity", width = 0.5, position = "dodge")+
             ggtitle('Gap')+
             theme(legend.position = "bottom",axis.text = element_text(angle = 45, hjust = 1))+
             theme(plot.title = element_text(hjust=0.5))+ylab('Gap')+ylim(0,1500)
gap_plot

# Gap is highest in Evening at Airport and In Early Morning in City.

# Overall Demand Supply Gap plots

grid.arrange(demand_plot,supply_plot,gap_plot,ncol=3)


# By looking at these plots we can see that Gap is highest During Evening at Airport
# and Early Morning in city


# Q2(b)Find the types of requests (city-airport or airport-city) 
#for which the gap is the most severe in the identified time slots
# Gap is most severe is

# from city-Airport--- with city as Pickup.point 
# and Time_slots are during Early_Morning

# from Airport-City--- with Airport as Pickup.Point
# and Time_slots are during Evening

#uber_demandSupply_gap[,c("Pickup.point","Time_Slot","gap")]

EarlyMorning <- ggplot(subset(uber,Time_Slot=="Early Morning"),aes(x=Time_Slot,fill=Status))+
                      geom_bar()+
                      geom_text(stat = "count",aes(label=..count..),position=position_stack(vjust=0.5))+
                      facet_wrap(~Pickup.point)+ylab('Number of Requests')+
                      ggtitle('Status in Early Morning')+theme(plot.title=element_text(hjust=0.5))
EarlyMorning

# From plot we can see that status is "cancelled" during Early Morning in city which leads to huge gap

Evening <- ggplot(subset(uber,Time_Slot=="Evening"),aes(x=Time_Slot,fill=Status))+geom_bar()+
            geom_text(stat = "count",aes(label=..count..),position=position_stack(vjust=0.5))+
            facet_wrap(~Pickup.point)+ylab('Number of Requests')+ggtitle('Status in Evening')+
            theme(plot.title=element_text(hjust=0.5))

Evening

# From plot we can see that status is "No Cabs Available" at Airport during Evening that leads to huge Gap

# Summary for Q2

# Pickup.Point       TimeSlot       Type Of Request(leads to severe gap)
# Airport            Evening         No cabs Available
# City               Early Morning    cancelled


#Q3 What do you think is the reason for this issue for the supply-demand gap? 

# Supply-Demand gap is because of No cars Available/Cancellations.

uber_status <- ggplot(uber,aes(x = factor(Time_Slot))) + geom_bar(aes(fill = Pickup.point)) +
                facet_grid(Pickup.point~Status,  scales = "free_y") +
                geom_text(stat = "count",aes(label=..count..),position = position_stack(vjust = 0.5))+
                labs(title = "Supply Demand of Trips") +
                xlab("Timeslots") + ylab("Number of Requests") +
                theme(legend.position = "bottom") +
                theme(axis.text = element_text(angle = 45, hjust = 1))+
                ggtitle('Total Uber Status')+theme(plot.title = element_text(hjust=0.5))
uber_status

# By observing all these plots we can clearly state that the Problematic types for uber are: 

# At Airport: Status-"No cabs available" during  Evening 
# In City : Status-"cancelled" during Early Morning 

# The Reason for supply-Demand gap could be:

#Early Morning Time : During Early Morning Time, Flights leaving the airport are high and the flights  
# (4AM to 8AM)        coming to the airport(incoming flights) are very less.
#                     The driver who reaches the airport during this time either has to spend ideal time
#                     to bring a customer back to city or return empty seated which will result in loss 
#                     for drivers. So most of the drivers are cancelling the requests during Early Morning
#                     time slot which results in the huge Demand-Supply gap.

#                  That is why more cancellations in the city during Early Morning.Status-"cancelled".

# Evening Time      : During Evening Time, Incoming flights are more and outgoing flights are less 
#   (4PM to 8PM)      because of this the cabs coming to the airport are very less.
#                     More incoming flights during  Evening results in more passengers demand for cabs 
#                     but there are no sufficient cabs to meet the demand during Evening 
#                     which results in Demand-Supply gap 
#                  That is why "No Cabs Available" is status shown high during Evening at Airport.



# 4.Recommend some ways to resolve the supply-demand gap.

# 1. Employ more number of drivers to meet the demand as we have seen that supply is very less.
# 2. Hire drivers for 8 hour workshifts and rotate the shifts at regular intervals to ensure efficiency.
# 3. Inform drivers regarding arrivals and departures of flights in-advance.
# 4. Allow passengers to pre-book taxis for airports and if it is in the timeslot
#    where there are no other flights then increase the cost of the trip not too high
#    but in such a way that both passengers and cab drivers are benefitted.
# 5. Educate the drivers and give notifications regarding demand and location during timeslots.
# 6. Announce some points for the drivers who makes more trips during Early Morning from City to Airport
#    and during Evening from Airport to city and give incentives as per the points earned.
