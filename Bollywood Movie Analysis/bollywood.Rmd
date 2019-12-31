 
#	Import the Bollywood data set in Rstudio in a variable named bollywood

bollywood <- read.csv("bollywood.csv")
View(bollywood)


#	When you import a data set, R stores character vectors as factors (by default)
# You can check the structure of the data frame by using str()

str(bollywood)

# You can change the attribute 'Movie' from factor to character type using the given command

bollywood$Movie <- as.character(bollywood$Movie)


#Q1.
#	Access the last 10 movies (from the bottom of the Bollywood data frame) using column bollywood$Movie
# Store the names of those movies in last_10 vector (in the same order)

last_10 <-tail(bollywood$Movie,10) 
last_10

	  
#Q2.
#	Find out the total number of  missing values (NA) in the bollywood data frame.
# Store the result in na_bollywood vector
     
na_bollywood <- sum(is.na(bollywood))
na_bollywood
	  
	
#Q3
#	Write the command to find out which movie tops the list in terms of Total Collections
# Store the movie name in variable named top_movie
 
top_movie <- bollywood[which.max(bollywood$Tcollection),"Movie"]
top_movie

#Q4
#	Write the command to find out which movie comes second on the list in terms of Total Collections
# Store the movie name in variable named top_2_movie

x <- bollywood$Tcollection # taking the Tcollection column values into a variable x

y <-max(x[x!=max(x)]) # selecting the 2nd maximum value of x (Tcollection column value) and storing it in variable y

top_2_movie <-bollywood[bollywood$Tcollection==y,"Movie"] # finding the 2nd top movie in terms of Tcollection

top_2_movie


# Now let's find out the movies shot by Shahrukh, Akshay and Amitabh separately.
# subset() function is used for that. The code has already been written for you. 
	
shahrukh <- subset(bollywood, Lead == "Shahrukh")
akshay <- subset(bollywood, Lead == "Akshay")
amitabh <- subset(bollywood, Lead  == "Amitabh")

# You can view what the above data frames look like
View(shahrukh)
View(akshay)
View(amitabh)

		   
#Q5
#	What is the total collection of Shahrukh, Akshay and Amitabh movies individually?
# You can use	a column named 'Tcollection' for this 
 
shahrukh_collection <-sum(bollywood[bollywood$Lead=="Shahrukh","Tcollection"]) 

shahrukh_collection


akshay_collection <- sum(bollywood[bollywood$Lead=="Akshay","Tcollection"])

akshay_collection 


amitabh_collection <-sum(bollywood[bollywood$Lead=="Amitabh","Tcollection"]) 

amitabh_collection
	

#Q6  
# Write command/s to find out how many movies are in Flop, Average, Hit and Superhit categories in the entire Bollywood data set.

summary(bollywood$Verdict)


#You can use SAPPLY function if you want to apply a function specific columns in a data frame 
#You can write a command to find the maximum value of Ocollection, Wcollection, Fwcollecion and Tcollection using sapply
  

#Q7 
# Write a command to find the names of the movies which have the maximum Ocollection, Wcollection, Fwcollecion & Tcollection
# Store the names of 4 movies in same sequence in movie_result vector


Ocollection <-sapply(bollywood,function(x) x=bollywood[which.max(bollywood$Ocollection),"Movie"])

#Ocollection[1]


Wcollection <-sapply(bollywood,function(x) x=bollywood[which.max(bollywood$Wcollection),"Movie"])

#Wcollection[1]


Fwcollection <-sapply(bollywood,function(x) x=bollywood[which.max(bollywood$Fwcollection),"Movie"])

#Fwcollection[1]


Tcollection <-sapply(bollywood, function(x) x=bollywood[which.max(bollywood$Tcollection),"Movie"])

#Tcollection[1]

movie_result <-c(Ocollection[1],Wcollection[1],Fwcollection[1],Tcollection[1])

names(movie_result) <- c("Ocollection","Wcollection","Fwcollection","Tcollection")

movie_result
