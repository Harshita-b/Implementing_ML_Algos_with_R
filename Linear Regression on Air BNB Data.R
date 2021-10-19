#-------------------------AIR BNB DATASET-----------------

#importing the dataset
BnB = read.csv("C:/Users/DELL/OneDrive/Desktop/DATA SCIENCE COURSE (Datasets)/Air BnB.csv")
View(BnB)

#calling the necessary packages
library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)
library(psych)

#running basic functions to explore the dataframe
dim(BnB)
head(BnB)
describe(BnB) 
str(BnB)
summary(BnB)

#---------------------Data Cleaning-----------------

#Step 1: Dropping latitude and longitude column
BnB1 = select(BnB,-2:-3)
View(BnB1)
dim(BnB1)
str(BnB1)
summary(BnB1)
describe(BnB1)


#Step 2: Replacing null values

#checking all the null values
length(which(is.na(BnB1)))
lapply(BnB1,function(x) { length(which(is.na(x)))} )

#plotting histogram to check the skewness
hist(BnB$Min_Nights,col = "red")
hist(BnB$Host_Listing_Cnt,col = "orange")
hist(BnB$Price,col = "blue")

#histograms are positively skewed
#replacing all the null values with median
BnB1$Min_Nights[is.na(BnB1$Min_Nights)] <- median(BnB1$Min_Nights,na.rm = TRUE)
BnB1$Host_Listing_Cnt[is.na(BnB1$Host_Listing_Cnt)] <- median(BnB1$Host_Listing_Cnt,na.rm = TRUE)
BnB1$Price[is.na(BnB1$Price)] <- median(BnB1$Price,na.rm = TRUE)


#Step 3: Changing the data type to factor
BnB1$Boroughs <- as.factor(BnB1$Boroughs)
BnB1$Prop_Type <- as.factor(BnB1$Prop_Type)


#------------------------Exploratory Data Analysis---------------------

#------------boxplot------------
#boxplot for showing the distribution of response variable i.e Price 
#neighborhood wise and room type wise

#Observations :-
#1. There are a lot of outliers in each neighborhood
#2. Average price of Manhattan is comparatively higher than other neighborhoods
ggplot(BnB1,aes(x = Boroughs, y = Price, fill = Boroughs)) +
  geom_boxplot() + ylim(0,1000)

#Observations :-
#1. There are a lot of outliers in each room type
#2. Average price of EntireHome is comparatively higher than other room types
ggplot(BnB1,aes(x = Prop_Type, y = Price, fill = Prop_Type)) +
  geom_boxplot() + ylim(0,1000)


#-----------scatterplot--------
#scatterplot for observing the relationship of each column with response variable i.e Price

#scatterplot of min nights and price with a best fit line
#Observation :-
#1. The best fit line is nearly parallel to the x-axis which shows 
#   very less dependency of price on min nights
ggplot(BnB1,aes(Min_Nights,Price,colour = Boroughs)) + geom_point()+
  geom_jitter() + geom_smooth(colour = "red") + xlim(0,366)

#it has a weak correlation
cor(BnB1$Min_Nights,BnB1$Price)


#scatterplot of host listing and price with a best fit line
#Observation :-
#1. The best fit line is nearly parallel to the x-axis which shows 
#   very less dependency of price on host listing
ggplot(BnB1,aes(Host_Listing_Cnt,Price,colour = Boroughs)) + geom_point()+
  geom_jitter() + geom_smooth(colour="red")

#it has a negligible correlation
cor(BnB1$Host_Listing_Cnt,BnB1$Price)


#scatterplot of days available and price with a best fit line
#Observation :-
#1. The best fit line is parallel to the x-axis which shows very less 
#   dependency of price on days available
ggplot(BnB1,aes(Days_Available,Price,colour = Boroughs)) + geom_point()+
  geom_jitter() + geom_smooth(colour="red")

#it has a weak correlation
cor(BnB1$Days_Available,BnB1$Price)


#scatterplot of reviews count and price with a best fit line
#Observation :-
#1. The best fit line has a slightly negative slop which shows inverse relation of
#   review count and price
ggplot(BnB1,aes(Review_Cnt,Price,colour = Boroughs)) + geom_point()+
  geom_jitter() + geom_smooth(colour="red")

#it has a weak correlation
cor(BnB1$Review_Cnt,BnB1$Price)


#scatterplot of 30 days review and price with a best fit line
#Observation :-
#1. The best fit line has a slightly negative slop which shows inverse relation of
#   30 days review  and price
ggplot(BnB1,aes(Reviews30d,Price,colour = Boroughs)) + geom_point()+
  geom_jitter() + geom_smooth(colour="red")

#it has a weak correlation
#correlation of reviews count and 30 days review is nearly same
cor(BnB1$Reviews30d,BnB1$Price)


#-----------count analysis--------
#count analysis on the basis of neighbourhood
#Observations :- 
#1. Brooklyn has highest no of rooms with reviews greater than 100
#2. Staten Island and Bronx has quite less no of rooms with reviews greater than 100
countAnalysis <- BnB1 %>% filter(Review_Cnt > 100) %>% 
  group_by(Boroughs) %>% summarise(Count = n())

countrooms <- countAnalysis[with(countAnalysis,order(-Count)),]
countrooms <- countrooms[1:20,]

ggplot(countrooms, aes(x = reorder(Boroughs, Count), y = Count)) + 
  geom_bar(stat="identity",colour="black", fill = "tomato3") + 
  labs(title="Neighbourhoods with most Number of Rooms",subtitle = "Rooms with Number of Reviews greater than 100") + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5), legend.position = c(0.8, 0.5)) + xlab("") + ylab("No of rooms ") 


#count analysis on the basis on room type
#Observations :- 
#1. EntireHome and PrivateRoom has nearly same no of rooms with reviews greater than 100
#2. SharedRoom has quite less no of rooms with reviews greater than 100
countAnalysis <- BnB1 %>% filter(Review_Cnt > 100) %>% 
  group_by(Prop_Type) %>% summarise(Count = n())

countrooms <- countAnalysis[with(countAnalysis,order(-Count)),]
countrooms <- countrooms[1:20,]

ggplot(countrooms, aes(x = reorder(Prop_Type, Count), y = Count)) + 
  geom_bar(stat="identity",colour="black", fill = "tomato3") + 
  labs(title="Room Types with most Number of Rooms",subtitle = "Rooms with Number of Reviews greater than 100") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5), legend.position = c(0.8, 0.5)) + xlab("") + ylab("No of rooms") 


#--------Grouping by Boroughs---------
by_Boroughs <- BnB1 %>% 
  group_by(Boroughs) %>% summarise(Mean_Price_Boroughs = mean(Price))
                          

#barplot for comparing response variable i.e. Price
#Observations :-
#1. Average price of Manhattan is highest.
#2. Average price of Bronx is lowest.
ggplot(by_Boroughs, aes(x = reorder(Boroughs, -Mean_Price_Boroughs), y = Mean_Price_Boroughs)) + 
  geom_bar(stat="identity",colour="black", fill = "tomato3") + 
  labs(title="Average Price of Rooms in each Neighbourhood Group")+
  geom_text(aes(label = round(Mean_Price_Boroughs,2)),size = 4,position = position_dodge(width = 0.3), vjust = -0.25) +
  xlab(label = "Boroughs") +
  ylab(label = "Average Price")
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5), legend.position = c(0.8, 0.5)) 

  
#--------Grouping by Prop_Type---------
by_Prop <- BnB1 %>% 
  group_by(Prop_Type) %>% summarise(Mean_Price_Prop = mean(Price))
  
  
#barplot for comparing response variable i.e. Price
#Observations :-
#1. Average price of EntireHome is highest
#2. Average price of SharedRoom is lowest
#3. Average price of EntireRoom is almost double the average price of PrivateRoom
ggplot(by_Prop, aes(x = reorder(Prop_Type, -Mean_Price_Prop), y = Mean_Price_Prop)) + 
  geom_bar(stat = "identity",colour = "black", fill = "tomato3") + 
  labs(title = "Average Price of Rooms in each Neighbourhood Group")+
  geom_text(aes(label = round(Mean_Price_Prop,2)),size = 4,position = position_dodge(width = 0.3), vjust = -0.25) +
  xlab(label = "Prop_Type") +
  ylab(label = "Average Price")
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5), legend.position = c(0.8, 0.5)) 
  
  
#--------Grouping by Boroughs and Prop_Type---------
by_Boroughs_Prop <- BnB1 %>% 
  group_by(Boroughs,Prop_Type) %>% summarise(Mean_Price = mean(Price),
                                     Mean_Min_Nights = mean(Min_Nights),
                                     Mean_Host_Listing = mean(Host_Listing_Cnt),
                                     Mean_Days_Available = mean(Days_Available),
                                     Mean_Review = mean(Review_Cnt),
                                     Mean_Reviews_30d = mean(Reviews30d))
by_Boroughs_Prop  

#barplot for comparing mean price of each neighbourhood acc to type of room
#Observations :-
#1. Manhattan has the highest mean price for all the 3 room types
#2. EntireRoom has the highest mean price in all the neighborhoods
#3. Mean price of PrivateRoom and SharedRoom in Queens,Bronx and Staten Island is nearly same
ggplot(by_Boroughs_Prop, aes(x = reorder(Boroughs, -Mean_Price), y = Mean_Price, fill = Prop_Type)) + 
  geom_bar(stat = "identity",colour = "black",position = position_dodge()) + 
  labs(title = "Comparison of Mean Price of each Neighbourhood Group according to Type of The Room") + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5), legend.position = c(0.9, 0.8)) + xlab("") + ylab("Mean Price") 


#barplot for comparing minimum nights in each neighbourhood acc to type of room
#Observations :-
#1. Manhattan has the highest mean price for all the 3 room types
#2. EntireRoom has the highest mean price in all the neighbourhoods
#3. Mean price of PrivateRoom and SharedRoom is nearly same in Queens
ggplot(by_Boroughs_Prop, aes(x = reorder(Boroughs, -Mean_Min_Nights), y = Mean_Min_Nights, fill = Prop_Type)) + 
  geom_bar(stat = "identity",colour = "black",position = position_dodge()) + 
  labs(title = "Comparison of Minimum Nights in each Neighbourhood Group according to Type of The Room") + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5), legend.position = c(0.9, 0.8)) + xlab("") + ylab("Mean Min_Nights") 


#barplot for comparing host listing of each neighbourhood acc to type of room
#Observations :-
#1. SharedRoom has the highest host listing in each neighbourhood except Manhattan
#2. EntireHome has the lowest host listing in each neighbourhoods except
#3. Host Listing of EntireHome is higher than other room types in Manhattan
ggplot(by_Boroughs_Prop, aes(x = reorder(Boroughs, -Mean_Host_Listing), y = Mean_Host_Listing, fill = Prop_Type)) + 
  geom_bar(stat = "identity",colour = "black",position = position_dodge()) + 
  labs(title = "Comparison of Host Listing of each Neighbourhood Group according to Type of The Room") + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5), legend.position = c(0.9, 0.8)) + xlab("") + ylab("Mean Host_Listing") 


#barplot for comparing days available in each neighbourhood acc to type of room
#Observations :-
#1. Days available in Bronx is nearly same for all the 3 room types
#2. Days available in PrivateRooms in Staten Island is maximum
#3. Days available in SharedRooms is highest in Queens,Brooklyn and Manhattan
ggplot(by_Boroughs_Prop, aes(x = reorder(Boroughs, -Mean_Days_Available), y = Mean_Days_Available, fill = Prop_Type)) + 
  geom_bar(stat = "identity",colour = "black",position = position_dodge()) + 
  labs(title = "Comparison of Days Available in each Neighbourhood Group according to Type of The Room") + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5), legend.position = c(0.9, 0.8)) + xlab("") + ylab("Mean Days Available") 


#barplot for comparing reviews of each neighbourhood acc to type of room
#Observations :-
#1. Reviews of EntireHome and PrivateRoom is nearly same in Queens
#2. There is significant difference in Reviews of SharedRoom in Staten Island
#3. SharedRooms has lowest reviews in each neighbourhood
ggplot(by_Boroughs_Prop, aes(x = reorder(Boroughs, -Mean_Review), y = Mean_Review, fill = Prop_Type)) + 
  geom_bar(stat = "identity",colour = "black",position = position_dodge()) + 
  labs(title = "Comparison of Mean Reviews of each Neighbourhood Group according to Type of The Room") + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5), legend.position = c(0.9, 0.8)) + xlab("") + ylab("Mean Review count")  


#barplot for comparing 30 days review of each neighbourhood acc to type of room
#Observations :-
#1. 30 days Reviews of EntireHome and PrivateRoom is nearly same in Queens
#2. 30 days Reviews of EntireHome is highest in each neighbourhood except Manhattan
#3. 30 days Reviews of SharedRoom is lowest in each neighbourhood except Manhattan
#4. 30 days Reviews in Brooklyn is nearly same for all the room types
ggplot(by_Boroughs_Prop, aes(x = reorder(Boroughs, -Mean_Reviews_30d), y = Mean_Reviews_30d, fill = Prop_Type)) + 
  geom_bar(stat = "identity",colour = "black",position = position_dodge()) + 
  labs(title = "Comparison of 30 Days review of each Neighbourhood Group according to Type of The Room") + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5), legend.position = c(0.9, 0.8)) + xlab("") + ylab("Mean Review 30 days")


#------------------------Linear Regression Model---------------------
set.seed(20)
id <- sample(2,nrow(BnB1),prob = c(0.8,0.2),replace = TRUE)
id

#dividing the data into training and testing data
#training data = 80% of the data
#testing data = 20% of the data
training <- BnB1[id == 1,]
testing <- BnB1[id == 2,]
View(training)
View(testing)

#Model 1:
#value of R^2 is 0.1 which is quite less efficient
model1 <- lm(Price~.,data = training)
model1
summary(model1)

#there is huge difference between the actual and the predicted value
pred1 <- predict(model1,data = testing)
head(pred1)
head(testing$Price)


#Model 2:
#value of R^2 is 0.1 which is quite less efficient
model2 <- lm(Price~.-Reviews30d,data = training)
model2
summary(model2)

#there is huge difference between the actual and the predicted value
pred2 <- predict(model2,data = testing)
head(pred2)
head(testing$Price)


#Model 3:
#value of R^2 is 0.1 which is quite less efficient
model3 <- lm(Price~.-Reviews30d-Min_Nights,data = training)
model3
summary(model3)


#Model 4:
#value of R^2 is 0.08 which is negligible 
model4 <- lm(Price~.-Reviews30d-Min_Nights-Boroughs,data = training)
model4
summary(model4)


#Model 5:
#value of R^2 is 0.03 which is negligible
model5 <- lm(Price~.-Prop_Type-Min_Nights-Reviews30d-Host_Listing_Cnt,data = training)
model5
summary(model5)

