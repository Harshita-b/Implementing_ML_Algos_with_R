#-------------------------AIR QUALITY DATASET-----------------

#importing the dataset
air = read.csv("C:/Users/DELL/OneDrive/Desktop/DATA SCIENCE COURSE (Datasets)/Air quality dataset.csv")
View(air)

#importing the necessary packages
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(reshape2)
library(psych)

#running basic functions to explore the dataframe
dim(air)
head(air)
describe(air)
str(air)
summary(air)


#---------------------Data Cleaning-----------------

#Step 1: Replacing null values

#checking all the null values
length(which(is.na(air)))
lapply(air,function(x) { length(which(is.na(x)))} )

air_na = air[c(7,8,9,10,12)]  #created a dataframe containing columns with null values
View(air_na)

#using lapply function for plotting histograms of all the columns together
o <- par(mfrow = c(1,5)) #for plotting all the histograms side by side
lapply(seq(air_na),function(x){ hist(x = air_na[[x]],breaks = 20,col ="orange",xlab=names(air_na)[x],main=paste("Histogram of",names(air_na)[x]))})

#histograms are positively skewed
#replacing all the null values with median
air$so2[is.na(air$so2)] <- median(air$so2,na.rm=TRUE)
air$no2[is.na(air$no2)] <- median(air$no2,na.rm=TRUE)
air$rspm[is.na(air$rspm)] <- median(air$rspm,na.rm=TRUE)
air$spm[is.na(air$spm)] <- median(air$spm,na.rm=TRUE)
air$pm2_5[is.na(air$pm2_5)] <- median(air$pm2_5,na.rm=TRUE)

#replacing null values of categorical column with mode
air$type[is.na(air$type)] <- "Residential, Rural and other Areas"


#Step 2: Changing the data types of the columns
air$stn_code <- as.integer(air$stn_code)
air$date <- as.Date(air$date,'%Y-%m-%d')
air$state <- as.factor(air$state)
air$location <- as.factor(air$location)
air$agency <- as.factor(air$agency)
air$type <- as.factor(air$type)
air$location_monitoring_station <- as.factor(air$location_monitoring_station)


#Step 3: Merging all the Duplicate categories
table(air$type)
air$type[air$type %in% c("Sensitive Areas","Sensitive")] <- "Sensitive Area"
air$type[air$type %in% c("Industrial","Industrial Areas")] <- "Industrial Area"
air$type[air$type %in% c("Residential")] <- "Residential and others"
table(air$type)


#Step 4: Splitting the date column into year and month
Year = as.integer(format(air$date,format = "%Y"))
Month = as.integer(format(air$date,format = "%m"))

air$year <- Year
air$month <- Month



#------------------------Exploratory Data Analysis---------------------

#--------Grouping by state---------
by_state <- air%>%group_by(state)%>%summarise(Avg_So2_state = mean(so2,na.rm = TRUE),
                                                           Avg_No2_state = mean(no2,na.rm = TRUE),
                                                           Avg_Rspm_state = mean(rspm,na.rm = TRUE),
                                                           Avg_Spm_state = mean(spm,na.rm = TRUE))
by_state
 

#barplot for comparing So2 statewise
#Observations :-
#1. Uttaranchal and Jharkhand have high So2 content
#2. Arunachal Pradesh,Himachal Pradesh,Mizoram and Nagaland have low So2 content
#3. Majority of states have So2 content equal to its average
ggplot(by_state,aes(x = state,y = Avg_So2_state,fill = Avg_So2_state)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Average Sulphor DiOxide Content-State Wise") +
  xlab(label = "State") +
  ylab(label = "Average SO2 Content") + 
  geom_text(aes(label = round(Avg_So2_state,2)),size = 3,position = position_dodge(width = 0.3), vjust = -0.25) 
  labs(caption="Source :Ministry of Environment and Forests and Central Pollution Control Board of India")

  
#barplot for comparing No2 statewise
#Observations :-
#1. West Bengal and Delhi have high No2 content
#2. Arunachal Pradesh,Mizoram and Nagaland have low No2 content
#3. Majority of states have No2 content equal to its average
ggplot(by_state,aes(x = state,y = Avg_No2_state,fill = Avg_No2_state)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Average No2 Content-State Wise") +
  xlab(label = "State") + 
  ylab(label = "Average No2 Content") +
  geom_text(aes(label = round(Avg_No2_state,2)),size = 3,position = position_dodge(width = 0.3), vjust = -0.25) 
  labs(caption="Source :Ministry of Environment and Forests and Central Pollution Control Board of India")
  
  
#barplot for comparing Rspm statewise
#Observations :-
#1. Delhi,Jharkhand,Punjab and Uttar Pradesh have high Rspm content
#2. Sikkim and Mizoram have low Rspm content
#3. Majority of states have Rspm content equal to its average   
ggplot(by_state,aes(x = state,y = Avg_Rspm_state,fill = Avg_Rspm_state)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Average Rspm Content-State Wise") +
  xlab(label = "State") + 
  ylab(label = "Average Rspm Content") +
  geom_text(aes(label = round(Avg_Rspm_state,2)),size = 3,position = position_dodge(width = 0.3), vjust = -0.25) 
  labs(caption="Source :Ministry of Environment and Forests and Central Pollution Control Board of India")
  
  
#barplot for comparing Spm statewise
#Observations :-
#1. Delhi has high Spm content
#2. Sikkim has low Spm content
#3. Majority of states have Spm content equal to its average 
ggplot(by_state,aes(x = state,y = Avg_Spm_state,fill = Avg_Spm_state)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Average Spm Content-State Wise") +
  xlab(label = "State") +
  ylab(label = "Average Spm Content") +
  geom_text(aes(label = round(Avg_Spm_state,2)),size = 3,position = position_dodge(width = 0.8),vjust = -0.25)
  labs(caption = "Source :Ministry of Environment and Forests and Central Pollution Control Board of India")

#from the overall comparison it can be seen that Delhi has highest amount
#of pollution
  
  
#---------------Analysis on Delhi Trend w.r.t pollution-----------
#grouping the data by year and type
air$date <- as.POSIXct(air$date)
air$year <- year(air$date)
Delhi <-air%>%filter(state == "Delhi")%>%group_by(year,type)%>%summarise(Avg_So2_Delhi = mean(so2,na.rm=TRUE),
                                                                       Avg_No2_Delhi = mean(no2,na.rm=TRUE),
                                                                       Avg_Rspm_Delhi = mean(rspm,na.rm=TRUE),
                                                                       Avg_Spm_Delhi = mean(spm,na.rm=TRUE))
Delhi

#Line chart for visualising So2 content in Delhi
#Observations :-
#1. Graph is more peaked for year 1995 in Industrial and Residential,Rural and other Areas
#2. Level of So2 had started dropping around 1997 in Industrial and Residential,Rural and other Areas
#3. So2 trend in Residential and others shows a stagnant trend with a drop around 2007
ggplot(Delhi,aes(x = year,y = Avg_So2_Delhi)) +
  geom_line(size = 1,color = "darkred") +  
  geom_point()+
  facet_wrap(~type) +
  ggtitle("Delhi SO2 Content-Year Wise")+
  xlab("Year") +
  ylab("Average SO2")

#Piechart for comparing the So2 content in type for Delhi
#Observations :-
#1. Industrial Area has highest level of So2
#2. It comprises almost half of the amount of So2
#3. Residential Area has lowest level of So2
ggplot(Delhi,aes(x="",y = Avg_So2_Delhi,fill = type))+
  geom_bar(stat = "identity",width = 1)+
  coord_polar("y",start = 0)+
  ggtitle("Delhi So2 content type wise")


#Line chart for visualising No2 content in Delhi
#Observations :-
#1. No2 trend shows a steady trend in Industrial Area and Residential,Rural and other Areas
#2. It shows a drop in 2005 in all the 3 types
#3. Graph is more peaked around 2012 in Industrial Area and Residential,Rural and other Areas
#4. level of So2 shows a linear trend from 1990 to 2005 in Residential and others
ggplot(Delhi,aes(x = year,y = Avg_No2_Delhi)) +
  geom_line(size = 1,color = "darkred") +
  geom_point()+
  facet_wrap(~type) +
  ggtitle("Delhi No2 Content-Year Wise")+
  xlab("Year") +
  ylab("Average No2")

#Piechart for comparing the No2 content in type for Delhi
#Observations :-
#1. Industrial Area has highest level of No2
#2. It comprises almost half of the amount of No2
#3. Residential Area has lowest level of No2
ggplot(Delhi,aes(x="",y = Avg_No2_Delhi,fill = type))+
  geom_bar(stat = "identity",width = 1)+
  coord_polar("y",start = 0)+
  ggtitle("Delhi No2 content type wise")


#Line chart for visualising Rspm content in Delhi
#Observations :-
#1. Rspm level shows a stagnant trend from 1990 to 2002 in Industrial Area and Residential, Rural and other Areas
#2. It starts increasing around 2002
#3. Rspm level shows a steady trend from 1990 to 2005 in Residential and others 
#4. It starts increasing after 2005
ggplot(Delhi,aes(x = year,y = Avg_Rspm_Delhi)) +
  geom_line(size = 1,color = "darkred") +
  geom_point()+
  facet_wrap(~type) +
  ggtitle("Delhi Rspm Content-Year Wise")+
  xlab("Year") +
  ylab("Average Rspm")

#Piechart for comparing the Rspm content in type for Delhi
#Observations :-
#1. Industrial Area has highest level of Rspm
#2. It comprises almost half of the amount of Rspm
#3. Residential Area has lowest level of Rspm
ggplot(Delhi,aes(x="",y = Avg_Rspm_Delhi,fill = type))+
  geom_bar(stat = "identity",width = 1)+
  coord_polar("y",start = 0)+
  ggtitle("Delhi Rspm content type wise")


#Line chart for visualising Spm content in Delhi
#Observations :-
#1. Level of Spm content shows a lot of fluctuation in Industrial Area and Residential,Rural and other Areas
#2. Residential and others shows a decline in level of Spm from 1990 to 2005 and rises after that
ggplot(Delhi,aes(x = year,y = Avg_Spm_Delhi)) +
  geom_line(size = 1,color = "darkred") +
  geom_point()+
  facet_wrap(~type) +
  ggtitle("Delhi Spm Content-Year Wise")+
  xlab("Year") +
  ylab("Average Spm")

#Piechart for comparing the Spm content in type for Delhi
#Observations :-
#1. Industrial Area has highest level of Spm
#2. It comprises almost half of the amount of Spm
#3. Residential Area has lowest level of Spm
ggplot(Delhi,aes(x="",y = Avg_Spm_Delhi,fill = type)) +
  geom_bar(stat = "identity",width = 1) +
  coord_polar("y",start = 0) +
  ggtitle("Delhi Spm content type wise")

#From the above line charts it can be observed that Industrial Area and Residential,Rural and other Areas
#shows a similar behaviour.Piecharts shows that more than 50% pollution is caused by
#Industrial Areas


#from the overall comparison it can be seen that Mizoram has lowest amount
#of pollution

#---------------Analysis on Mizoram Trend w.r.t pollution-----------
air$date <- as.POSIXct(air$date)
air$year <- year(air$date)
Mizoram <- air%>%filter(state == "Mizoram")%>%group_by(year,type)%>%summarise(Avg_So2_Mizoram = mean(so2,na.rm=TRUE),
                                                                       Avg_No2_Mizoram = mean(no2,na.rm=TRUE),
                                                                       Avg_Rspm_Mizoram = mean(rspm,na.rm=TRUE),
                                                                       Avg_Spm_Mizoram = mean(spm,na.rm=TRUE))
Mizoram


#Line chart for visualising So2 content in Mizoram
#Observations :-
#1. There is a significant difference in the So2 level in Residential and others and Residential,Rural and others Areas
#2. So2 content in almost same in all years in Residential,Rural and other Areas with
#   a rise around 2013
#3. So2 content in Residential and others declines around 2006 and rises after.
#4. So2 content in Residential and others is stagnant after 2007
ggplot(Mizoram,aes(x = year,y = Avg_So2_Mizoram)) +
  geom_line(size = 1,color = "darkred") +
  geom_point()+
  facet_wrap(~type) +
  ggtitle("Mizoram SO2 Content-Year Wise")+
  xlab("Year") +
  ylab("Average SO2")

#Piechart for comparing the So2 content in type for Mizoram
#Observations :-
#1. Residential and others comprises almost three-fourth of the amount of So2 content
ggplot(Mizoram,aes(x="",y = Avg_So2_Mizoram,fill = type))+
  geom_bar(stat = "identity",width = 1)+
  coord_polar("y",start = 0)+
  ggtitle("Mizoram So2 content type wise")


#Line chart for visualising No2 content in Mizoram
#Observations :-
#1. There is a significant difference in the So2 level in Residential and others and Residential,Rural and others Areas
#2. No2 content in residential and others declines from 2005 to 2006.
#3. No2 content in residential rural and other areas does not show much difference.
ggplot(Mizoram,aes(x = year,y = Avg_No2_Mizoram)) +
  geom_line(size = 1,color = "darkred") +
  geom_point()+
  facet_wrap(~type) +
  ggtitle("Mizoram NO2 Content-Year Wise")+
  xlab("Year") +
  ylab("Average NO2")

#Piechart for comparing the No2 content in type for Mizoram
#Observations :-
#1. Residential and others comprises almost half of the amount of No2 content
ggplot(Mizoram,aes(x="",y = Avg_No2_Mizoram,fill = type))+
  geom_bar(stat = "identity",width = 1)+
  coord_polar("y",start = 0)+
  ggtitle("Mizoram No2 content type wise")


#Line chart for visualising Rspm content in Mizoram
#Observations :-
#1. There is a significant difference in the So2 level in Residential and others and Residential,Rural and others Areas
#2. Rspm content is more in Residential,Rural and other Areas
#3. Rspm content shows a huge difference in Residential,Rural and other Areas each year
#4. there is a rise in the So2 content in Residential and others after 2005
ggplot(Mizoram,aes(x = year,y = Avg_Rspm_Mizoram)) +
  geom_line(size = 1,color = "darkred") +
  geom_point()+
  facet_wrap(~type) +
  ggtitle("Mizoram Rspm Content-Year Wise")+
  xlab("Year") +
  ylab("Average Rspm")

#Piechart for comparing the Rspm content in type for Mizoram
#Observations :-
#1. Residential,Rural and other comprises almost three-fourth of the amount of Rspm content
ggplot(Mizoram,aes(x="",y = Avg_Rspm_Mizoram,fill = type))+
  geom_bar(stat = "identity",width = 1)+
  coord_polar("y",start = 0)+
  ggtitle("Mizoram Rspm content type wise")


#Line chart for visualising Spm content in Mizoram
#Observations :-
#1. There is a significant difference in the So2 level in Residential and others and Residential,Rural and others Areas
#2. Spm content is more in Residential,Rural and other Areas
#3. There is a sharp rise in Spm content in Residential,Rural and other Areas from 2010 to 2011
#4. Spm content is stagnant in Residential,Rural and other Areas after 2011
#5. Spm content in Residential and others declines after 2006
ggplot(Mizoram,aes(x = year,y = Avg_Spm_Mizoram)) +
  geom_line(size = 1,color = "darkred") +
  geom_point()+
  facet_wrap(~type) +
  ggtitle("Mizoram Spm Content-Year Wise")+
  xlab("Year") +
  ylab("Average Spm")

#Piechart for comparing the Spm content in type for Mizoram
#Observations :-
#1. Residential,Rural and other comprises almost three-fourth of the amount of Spm content
ggplot(Mizoram,aes(x="",y = Avg_Spm_Mizoram,fill = type))+
  geom_bar(stat = "identity",width = 1)+
  coord_polar("y",start = 0)+
  ggtitle("Mizoram Spm content type wise")

#from the above observations it can be said that Mizoram does not have Industrial Areas
#Therefore, pollution level is less

#--------Grouping by type---------
by_type <- air%>%group_by(type)%>%summarise(Avg_So2_type = mean(so2,na.rm = TRUE),
                                              Avg_No2_type = mean(no2,na.rm = TRUE),
                                              Avg_Rspm_type = mean(rspm,na.rm = TRUE),
                                              Avg_Spm_type = mean(spm,na.rm = TRUE))
by_type


#barplot for visualising So2 content Area-Wise
#Observations :-
#1. Industrial Area has highest So2 content.
#2. Residential and others and Residential,Rural and other Areas have nearly same So2 content
#3. Sensitive Area has lowest So2 content.
ggplot(by_type,aes(x = type,y = Avg_So2_type,fill = Avg_So2_type)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Average So2 Content Area-Wise") +
  xlab(label = "Type") +
  ylab(label = "Average SO2 Content") + 
  geom_text(aes(label = round(Avg_So2_type,2)),size = 3,position = position_dodge(width = 0.3), vjust = -0.25) 
  labs(caption="Source :Ministry of Environment and Forests and Central Pollution Control Board of India")

  
#barplot for visualising No2 content Area-Wise
#Observations :-
#1. RIRUO has highest No2 content.
#2. Residential and others and Residential,Rural and other Areas have nearly same No2 content
#3. Sensitive Area has lowest No2 content.  
ggplot(by_type,aes(x = type,y = Avg_No2_type,fill = Avg_No2_type)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Average No2 Content Area-Wise") +
  xlab(label = "Type") +
  ylab(label = "Average NO2 Content") + 
  geom_text(aes(label = round(Avg_No2_type,2)),size = 3,position = position_dodge(width = 0.3), vjust = -0.25) 
  labs(caption="Source :Ministry of Environment and Forests and Central Pollution Control Board of India")
  
  
#barplot for visualising Rspm content Area-Wise
#Observations :-
#1. Industrail Area has highest Rspm content
#2. Rest of the Areas have nearly same amount of Rspm content  
ggplot(by_type,aes(x = type,y = Avg_Rspm_type,fill = Avg_Rspm_type)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Average Rspm Content Area-Wise") +
  xlab(label = "Type") +
  ylab(label = "Average Rspm Content") + 
  geom_text(aes(label = round(Avg_Rspm_type,2)),size = 3,position = position_dodge(width = 0.3), vjust = -0.25) 
  labs(caption="Source :Ministry of Environment and Forests and Central Pollution Control Board of India")
  
  
#barplot for visualising Spm content Area-Wise
#Observations :-
#1. Industrial,Residentail and others,Sensitive Areas have nearly same amount of Spm content
#2. Residential,Rural and other Areas and RIRUO Areas have nearly same amount of Spm content
ggplot(by_type,aes(x = type,y = Avg_Spm_type,fill = Avg_Spm_type)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Average Spm Content Type-Wise") +
  xlab(label = "Type") +
  ylab(label = "Average Spm Content") + 
  geom_text(aes(label = round(Avg_Spm_type,2)),size = 3,position = position_dodge(width = 0.3), vjust = -0.25) 
  labs(caption="Source :Ministry of Environment and Forests and Central Pollution Control Board of India")
  
  
#--------Grouping by year and state---------
by_State_Year <- air%>%group_by(state,year) %>%summarise(So2_SY = mean(so2,na.rm=TRUE),
                                                          No2_SY = mean(no2,na.rm=TRUE),
                                                          Rspm_SY = mean(rspm,na.rm=TRUE),
                                                          spm_SY = mean(spm,na.rm=TRUE))
by_State_Year.long <-melt(State_Year_Wise,id=c("year","state"),measure = c("So2_SY","No2_SY","Rspm_SY","spm_SY"))
by_State_Year.long

#tiled bar chart state and year wise for visualising So2 content
#Observations :-
#1. States like Bihar,Goa,Puducherry and West Bengal shows a significant change in the level of So2 content
#2. For rest of the states level of So2 content is stagnant
ggplot(by_State_Year,aes(x = year,y = So2_SY)) +
  geom_bar(stat = "identity",position = "dodge") +
  facet_wrap(~state) +
  ggtitle("So2 content year - wise in every state")


#tiled bar chart state and year wise for visualising No2 content
#Observations :-
#1. States like Delhi,Rajasthan,Haryana and West Bengal shows a significant change in the level of No2 content
#2. For rest of the states level of No2 content is stagnant  
ggplot(by_State_Year,aes(x = year,y = No2_SY)) +
  geom_bar(stat = "identity",position = "dodge") +
  facet_wrap(~state) +
  ggtitle("No2 content year - wise in every state")


#tiled bar chart state and year wise for visualising Rspm content
#Observations :-
#1. States like Delhi,Haryana and Punjab shows a significant change in the level of Rspm content
#2. For rest of the states level of Rspm content is stagnant
ggplot(by_State_Year,aes(x = year,y = Rspm_SY)) +
  geom_bar(stat = "identity",position = "dodge") +
  facet_wrap(~state) +
  ggtitle("Rspm content year - wise in every state")


#tiled bar chart state and year wise for visualising Spm content
#Observations :-
#1. States like Delhi and Haryana shows a significant change in the level of Spm content
#2. For rest of the states level of Spm content is stagnant
ggplot(by_State_Year,aes(x = year,y = spm_SY)) +
  geom_bar(stat = "identity",position = "dodge") +
  facet_wrap(~state) +
  ggtitle("Spm content year - wise in every state")


#--------------heatmap for analyzing yearwise---------

#heatmap for analyzing magnitude of So2 content yearwise
#Observations :-
#1. West Bengal has high magnitude of So2 content in initial years and starts decreasing around 1998
#2. Goa shows high magnitude of So2 content around 1997
by_State_Year.long %>%
  filter(variable == "So2") %>%
  ggplot(aes(x = year,y = state,fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white",high="steelblue") +
  theme(legend.position = "right") +
  labs(title = "Heat Map for Average SO2 Content Yearwise")


#heatmap for analyzing magnitude of No2 content yearwise
#Observations :-
#1. West Bengal has high magnitude of No2 content
by_State_Year.long %>%
  filter(variable == "No2") %>%
  ggplot(aes(x = year,y = state,fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white",high="red") +
  theme(legend.position = "right") +
  labs(title ="Heat Map for Average No2 Content Yearwise")


#heatmap for analyzing magnitude of Rspm content yearwise
#Observations :-
#1. West Bengal shows high magnitude of Rspm content around 2004
#2. Delhi and Haryana shows high magnitude of Rspm content around 2009
by_State_Year.long %>%
  filter(variable == "Rspm") %>%
  ggplot(aes(x = year,y = state,fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white",high = "green") +
  theme(legend.position = "right") +
  labs(title ="Heat Map for Average Rspm Content Yearwise")


#heatmap for analyzing magnitude of Spm content yearwise
#Observations :-
#1. West Bengal,Uttar Pradesh and Delhi have high magnitude of Spm content
by_State_Year.long %>%
  filter(variable == "spm") %>%
  ggplot(aes(x = year,y = state,fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white",high = "orange") +
  theme(legend.position = "right") +
  labs(title ="Heat Map for Average Spm Content Yearwise")



#--------Grouping by month and state---------
#grouping monthwise to analyze seasonal trend
by_State_Month <- air%>%group_by(state,month) %>%summarise(So2_SM = mean(so2,na.rm=TRUE),
                                            No2_SM = mean(no2,na.rm=TRUE),
                                            Rspm_SM = mean(rspm,na.rm=TRUE),
                                            Spm_SM = mean(spm,na.rm=TRUE))
by_State_Month.long <-melt(by_State_Month,id=c("month","state"),measure = c("So2_SM","No2_SM","Rspm_SM","Spm_SM"))
by_State_Month.long


#tiled bar chart state and month wise for visualising So2 content
#Observations :-
#1.Seasonal trend can only be seen in West Bengal from march to september
ggplot(by_State_Month,aes(x = month ,y = So2_SM)) +
  geom_bar(stat = "identity",position = "dodge") +
  facet_wrap(~state) +
  ggtitle("So2 content month - wise in every state")


#tiled bar chart state and month wise for visualising No2 content
#Observations :-
#1.Seasonal trend can only be seen in West Bengal in all the months
ggplot(by_State_Month,aes(x = month ,y = No2_SM)) +
  geom_bar(stat = "identity",position = "dodge") +
  facet_wrap(~state) +
  ggtitle("No2 content month - wise in every state")


#tiled bar chart state and month wise for visualising Rspm content
#Observations :-
#1.Seasonal trend can only be seen in West Bengal and Delhi
ggplot(by_State_Month,aes(x = month ,y = Rspm_SM)) +
  geom_bar(stat = "identity",position = "dodge") +
  facet_wrap(~state) +
  ggtitle("Rspm content month - wise in every state")


#tiled bar chart state and month wise for visualising Spm content
#Observations :-
#1.Seasonal trend can only be seen in West Bengal,Delhi and Bihar
ggplot(by_State_Month,aes(x = month ,y = Spm_SM)) +
  geom_bar(stat = "identity",position = "dodge") +
  facet_wrap(~state) +
  ggtitle("Spm content month - wise in every state")



#-------------heatmap for analyzing monthwise----------

#heatmap for analyzing magnitude of So2 content monthwise
#Observations :-
#1. Uttaranchal,Jharkhand and Bihar have high magnitude of So2 content
by_State_Month.long %>%
  filter(variable == "So2_SM") %>%
  ggplot(aes(x = month,y = state,fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white",high="steelblue") +
  theme(legend.position = "right") +
  labs(title = "Heat Map for Average SO2 Content Monthwise")


#heatmap for analyzing magnitude of No2 content monthwise
#Observations :-
#1. West Bengal and Delhi have high magnitude of No2 content
by_State_Month.long %>%
  filter(variable == "No2_SM") %>%
  ggplot(aes(x = month,y = state,fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white",high="red") +
  theme(legend.position = "right") +
  labs(title ="Heat Map for Average No2 Content Monthwise")


#heatmap for analyzing magnitude of Rspm content monthwise
#Observations :-
#1. West Bengal,Jharkhand and Delhi have high magnitude of Rspm content
by_State_Month.long %>%
  filter(variable == "Rspm_SM") %>%
  ggplot(aes(x = month,y = state,fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white",high = "green") +
  theme(legend.position = "right") +
  labs(title ="Heat Map for Average Rspm Content Monthwise")


#heatmap for analyzing magnitude of Spm content monthwise
#Observations :-
#1. Delhi has high magnitude of So2 content
by_State_Month.long %>%
  filter(variable == "Spm_SM") %>%
  ggplot(aes(x = month,y = state,fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white",high = "orange") +
  theme(legend.position = "right") +
  labs(title ="Heat Map for Average Spm Content Monthwise")


#------------------------Linear Regression Model---------------------
set.seed(50)
id <- sample(2,nrow(air),prob = c(0.8,0.2),replace = TRUE)
id

#dividing the data into training and testing data
#training data = 80% of the data
#testing data = 20% of the data
training <- air[id == 1,]
testing <- air[id == 2,]
View(training)
View(testing)


#Model 1:
#taking so2 as response variable i.e target
#and rest of the variables as independent variable
model1 <- lm(so2~.,data = training)    
model1
summary(model1)  #agency and sampling_date has less dependency on the target variable

pred1 <- predict(model1,data = testing)
head(pred1)
head(testing$so2)


#Model 2:
#taking so2 as response variable i.e target
#and rest of the variables except agency and sampling_date as independent variable
model2 <- lm(so2~.-agency-sampling_date,data = training)    
model2
summary(model2)

pred2 <- predict(model2,data = testing)
head(pred2)
head(testing$so2)

