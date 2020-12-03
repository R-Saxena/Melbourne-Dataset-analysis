##Loading packages
require(tidyverse)
require(ISLR)
require(MASS)

#Step- 1
#Loading the Dataset

mel_data <- read.csv('melbourne_data.csv', header = T, stringsAsFactors = F)

mel_house_data <- data.frame(mel_data, stringsAsFactors = F)

class(mel_house_data)

#visualizing the dataset
head(mel_house_data)
View(mel_house_data)

##Seeing all the column names
colnames(mel_house_data)


#Seeing the Structure and Descriptive Summary of the dataset 

#finding out the structure of the melbourne dataset
str(mel_house_data)

#As we can see here, we are having lots of NA values in most of the attributes. like yearbuilt is not available for lots of the houses. So, first we need to clean our data for doing the EDA of dataset.

#Descriptive Analysis
summary(mel_house_data)

# with the help of descriptive analysis of the dataset we can get the statistical perspective. For example like, we can see that Building Areas can be of maximum 44515 but if you will the mean of all the house, it is 160 which means we have some outliers as well in the dataset. So, we need to preprocessing to clean the dataset.



### Data Preprocessing Step : Removing NA Values from the dataset :

#importing library 
library(ggplot2)

mel_house_data <- data.frame(lapply(mel_house_data,function(x) { gsub("#N/A", NA, x) }))
class(mel_house_data)


#Finding Count of Na values in each column
NA_count_of_each_col<-sapply(mel_house_data,function(x) sum(is.na(x)==TRUE))
NA_count_of_each_col                             

# Find percent of missing in each column

for(i in 1:ncol(mel_house_data)) {
  colName <- colnames(mel_house_data[i])
  pctNull <- sum(is.na(mel_house_data[,i]))/length(mel_house_data[,i])
  if (pctNull > 0.50) {
    print(paste("Column ", colName, " has ", round(pctNull*100, 3), "% of nulls"))
  }
}

#Droping all the columns which are having more than 50 percent NA values

mel_house_data[,c("BuildingArea","YearBuilt")]<-NULL


mel_house_data_clean<-na.exclude(mel_house_data)

mel_house_data_clean$Type<-as.factor(mel_house_data_clean$Type)
mel_house_data_clean$Propertycount<-as.numeric(mel_house_data_clean$Propertycount)
mel_house_data_clean$Regionname<-as.factor(mel_house_data_clean$Regionname)
mel_house_data_clean$Distance<-as.numeric(mel_house_data_clean$Distance)
mel_house_data_clean$Price<-as.numeric(mel_house_data_clean$Price)
mel_house_data_clean$Landsize <- as.numeric(mel_house_data_clean$Landsize)
mel_house_data_clean$Car <- as.numeric(mel_house_data_clean$Car)
mel_house_data_clean$Bathroom <- as.numeric(mel_house_data_clean$Bathroom)
mel_house_data_clean$Rooms <- as.numeric(mel_house_data_clean$Rooms)

head(mel_house_data_clean)

#treating the date variable
datetext <- as.Date(mel_house_data_clean$Date, "%d/%m/%Y")
#Split Date into the Day, Month, Year
df <- data.frame(Date = datetext,
                 year = as.numeric(format(datetext, format = "%Y")),
                 month = as.numeric(format(datetext, format = "%m")),
                 day = as.numeric(format(datetext, format = "%d")))

mel_house_data_clean$year <- as.factor(df$year)
mel_house_data_clean$month <- as.factor(df$month)
mel_house_data_clean$day <- df$day

#Remove Date
mel_house_data_clean$Date <- NULL

head(mel_house_data_clean)


#Boxplot to check the data and outliers

#making boxplot of price range in different regions
par(mar=c(3.1,12,4.1,2.1), mgp = c(11, 1, 0))
boxplot(mel_house_data_clean$Price ~ mel_house_data_clean$Regionname,horizontal = TRUE,  ylab = "Region Name", xlab = "Price of houses", main = "Boxplot of price of houses by region", las = 1)

#Boxplot of distance vs type of houses
boxplot(mel_house_data_clean$Distance ~ mel_house_data_clean$Type, horizontal = TRUE, ylab = "Type of House", xlab = "Distance", main = "Boxplot of distance vs type of houses", las = 1)

#As we can see, there are few outliers in the price range by different region boxplot graph. but Currently we are not removing any because it will not affect our EDA part but yes we can see the impact of it when we do some modelling on the dataset.



#Step -2
##Dataset has preprocessed So lets see the Statistics and summary of the Clean dataset

#columns in the clean dataset
colnames(mel_house_data_clean)

#structure of the clean dataset
str(mel_house_data_clean)

#descriptive Summary of the clean dataset
summary(mel_house_data_clean)

#Lets make some plots 

#pie chart
pie(table(mel_house_data_clean$Type),
    labels=table(mel_house_data_clean$Type),
    main="House type Breakdown",
    col=c("red","green","blue"),
    border="brown",
    clockwise=TRUE
)

#bar chart

# Distribution of Rooms in the houses:
ggplot(data =mel_house_data_clean) + 
  geom_bar(mapping = aes(x = Rooms),position = "dodge")

#Scatterplot
plot(x = mel_house_data_clean$Price,y = mel_house_data_clean$Landsize,
     xlab = "Price of the house",
     ylab = "LandSize of the house",
     main = "Price vs LandSize"
)

#Histograms
#seeing the distribution of all the variables
par(mfrow = c(2,2), mar=c(3.1,3.1,0.95,0))
hist(mel_house_data_clean$Distance, breaks = 40, xlim = c(0,50), ylim = c(0,800),xlab = "Distance", col = "Red", main = "Histogram of Distances", las =1)
hist(mel_house_data_clean$Rooms, breaks = 40, xlim = c(0,10), ylim = c(0,4000),xlab = "No. of Rooms", col = "Green", main = "Histogram of no. of Rooms in houses", las =1)
hist(mel_house_data_clean$Bathroom, breaks = 40, xlim = c(0,10), ylim = c(0,4000),xlab = "No. of Bathrooms", col = "Blue", main = "Histogram of no. of bathrooms in the houses", las =1)
hist(mel_house_data_clean$Car, breaks = 40, xlim = c(0,10), ylim = c(0,4000),xlab = "No. of Carslots", col = "Yellow", main = "Histogram of no. of carslots in houses", las =1)


#Step-3

#Show the histogram of the price variable. Describe it briefy. Include summary statistics like mean,median, and variance.


hist(mel_house_data_clean$Price, main = "Distribution of Price Variable")

# As we can see in the distribution of price variable, there are So many house at very low cost and very few house at very high cost. 
# average of all the house price is 1526 and for first 25% house average price is 609.
#Min.   :1.0  
#1st Qu.:609   
#Median :1726    
#Mean   :1526   
#3rd Qu.:2364   
#Max.   :2871

#We can divide house in different range like below 1000 - low cost, between 1000 to 2000 - medium cost, above 2000 - high cost house


#Group houses by some price ranges ( like low, medium, high,etc.) and summarise those groups separately
low <- mel_house_data_clean%>%filter(Price < 1000)
med <- mel_house_data_clean%>%filter(Price > 1000 & Price < 2000)
high <- mel_house_data_clean%>%filter(Price > 2000)

#Distribution of house ranges with different categories

hist(low$Price, main = "Distribution of low Price houses")

hist(med$Price, main = "Distribution of medium Price houses")

hist(high$Price, main = "Distribution of high Price houses")



#Explore prices for different house types. You might want to use the boxplot.


boxplot(mel_house_data_clean$Price ~ mel_house_data_clean$Type, horizontal = FALSE,  ylab = "Type of house", xlab = "Price of houses", main = "Boxplot of price of houses by Type", las = 1)

# we can see that h type house are having around 1.4k price, t type houses are having more than average 2k price and u type having around 1.9k price.
#But there are so many outliers in the u type houses


### Distribution of different Type of houses over the regions:
ggplot(data =mel_house_data_clean) + 
  geom_bar(mapping = aes(x = Regionname, fill = Type),position = "dodge")


#How different attributes are correlated with the price? Which of the variables are correlated the most with price?
library(corrplot)
cor_data<- as.data.frame(mel_house_data_clean[,c(2:7,9,12)])
corrplot(cor(as.matrix(cor_data)), method = "pie", type="lower")

# price is correlated with the number of the bathroom in the house and number of the rooms in the house


#step-4
#List the frequencies of houses for various types. Create 2 scatterplots and colour the house price by landsize and type.
table(mel_house_data_clean$Type)

# we can clearly see that we are having 13351 houses of h type, 1300 houses of t type and 3050 houses of u type.

plot(x = mel_house_data_clean$Price,y = mel_house_data_clean$Landsize,
     xlab = "Price of the house",
     ylab = "LandSize of the house",
     main = "Price vs LandSize",
     col = mel_house_data_clean$Type,
     legend = c('h','t','u')
)

#Scatter plot between price and LandSize and by type we can see the color as the type.





