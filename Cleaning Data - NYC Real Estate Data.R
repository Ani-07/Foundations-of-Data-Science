## Loading Libraries

library(xlsx)
library('plyr')
library(gdata)
library(ggplot2)
library(scales)

## Loading Data - Manhattan Real Estate Data

res.data <- read.xls("C:\\Users\\Anirudh Raghavan\\Desktop\\Stevens - Courses\\Financial Data Science - FE 582\\Assignment 1\\HW1_F19\\rollingsales_manhattan.xls", perl =  "C:\\Perl64\\bin\\perl5.28.1.exe", pattern = "BOROUGH")

## Let us now understand the data loaded in terms of variables provided and number of rows 
## of data

nrow(res.data)
ncol(res.data)
names(res.data)

## This shows us that there are 27,395 data points and we have upto 21 variables for each 
## data point (the word upto has been used because for some data points a few variables 
## may not be available)

## Let us now study a summary of the data available.

summary(res.data)

## As you would observe the summary function provides a summary of each variable in the 
## dataset.It is important to note that based on the type of data under each variable 
## the manner of disclosure of summary data shall vary. For example, the building class 
## cateogry being categorical data (stored as factors),the data is shown as number of 
## data points with respect to each category whereas the summary for Borough being 
## numerical data is provided in terms of median, mean and quartiles.

## Using the above, we can note that the details with respect to Sale Price, Land Square 
## Feet and Gross Square Feet have been stored in the form of factors i.e categorical data. 
## The same will now be converted into numeric data.

res.data$sale.price.new <- as.numeric(gsub("[^[:digit:]]","",res.data$SALE.PRICE))
res.data$LAND.SQUARE.FEET.new <- as.numeric(gsub("[^[:digit:]]","",res.data$LAND.SQUARE.FEET))
res.data$GROSS.SQUARE.FEET.new <- as.numeric(gsub("[^[:digit:]]","",res.data$GROSS.SQUARE.FEET))

## Now that we have converted all the data with respect to the above variables to numeric, 
## we can use the summary function to observe the data.

summary(res.data)

## As you would observe, the new data field with respect to sale price, land and gross square 
## feet now reflect the numerical characterisitics of the data. 

## It is interesting to note that with respect to sale price, the min and the 1st quartile is 
## 0, which shows that more than 25% of the data is zero i.e. the sale price is unavailable.

## Before we go ahead, with cleaning of the sales data, we can confirm if there are any 
## missing values our dataframe. We may observe from the summary table that the data 
## with respect to Building Class Category has a few blank values which could mean missing values.

count(is.na(res.data))

## We can see from the result of the above function that other than Easement, no other 
## variable has any missing values. This may seem contradictory to our observation about 
## missing values in Building Class Category. We can take a look at the categories under 
## the building class category to analyze the reasons for the same.

levels(res.data$BUILDING.CLASS.CATEGORY)

## As we can see from the above result, a ("") category has been assing to values with 
## missing items and this is the reason the 'na' function did not identify the above 
## discrepency. We can use the gsub function, to substitute such cells with NA values 
## in order to identify them easily.

res.data$BUILDING.CLASS.CATEGORY[res.data$BUILDING.CLASS.CATEGORY == "                                            "] <- NA

count(is.na(res.data$BUILDING.CLASS.CATEGORY))

## After Multiple Iterations, I had to open the file in excel and observe that those 
## blank cells under Building Category had a certain number of spaces contained in them. 
## Using the same, I identified those cells and replaced them with NA.

## Now let us go ahead with cleaning the sales data. As simple summary of the sales data 
## shows more than 25% of the data with zeroes and also the sales data has extremely high 
## values. To rescale the data, we can describe the data in thousands to provide a better 
## picture of the data and we can also remove the zeroes in the sale price as the same may 
## distort the relationship with other variables.

res.data$sale.price.new <- res.data$sale.price.new/1000
actual.sale <- res.data[res.data$sale.price.new != 0,]

summary(actual.sale$sale.price.new)

## While the summary of sale price details actually shows min value of 0.0, a further 
## detailed analysis of the data reveals that the same is not equal to zero but .001. 
## Refer the fivenum function belo.w

fivenum(actual.sale$sale.price.new)

## It is also interesting to note that in the summary data of the data frame that the 
## sale date has also been stored as factor data. We can change the same to the date 
## format using the below code. This will be useful in analyzing the data in terms of time.

actual.sale$sale.date.c <- as.Date(actual.sale$SALE.DATE)

## Next, with respect to sale price data, we can create a boxplot to observe if there 
## are any outliers.

boxplot(actual.sale$sale.price.new)

## You may observe from the boxplot that due to outliers, the data is looks very 
## dispersed. Removing the outliers may provide a better picture of the overall data. 
## We can remove the outliers using the following code.

outliers <- boxplot(actual.sale$sale.price.new)$out

actual.sale1 <- actual.sale[-which(actual.sale$sale.price.new %in% outliers),]

boxplot(actual.sale1$sale.price.new)

## We have created a new dataframe - "actual.sale1" after removing the outliers. 
## This ensures that in case we wish to analyze the data along with the outliers 
## we can do the same using the previous dataframe - "actual.sale"

## Now let us analyze the data available to understand the relationships between 
## the variables with respect to residential property. We will be including the 
## following: 1-, 2-, and 3-family homes, coops, and condos as residential property. 

Residential.Type <- c("FAMILY", "CONDOS", "COOPS")

residential.data <- actual.sale1[which(grepl(paste(Residential.Type, collapse = "|"),actual.sale1$BUILDING.CLASS.CATEGORY)),]
summary(residential.data$BUILDING.CLASS.CATEGORY)

## With respect to the same we shall analyze the following variables:

## 1) Relationship with sale date
## 2) Relationship between sale price and type of building
## 3) Relationship between sale price and Neighborhood

## 1) Relationship between sale price and time

hist(residential.data$sale.date.c, "months")

## On a individual basis, a histogram of the sale date shows us that a few months 
## seem to have a much higher number of sales such as Nov-Dec as compared to Jan-Feb.

ggplot(residential.data, aes(residential.data$sale.date.c,residential.data$sale.price.new))+geom_point()+
  scale_x_date(labels = date_format("%Y-%m"), breaks = "1 month")

## A scatterplot of sale date along with sale prices does not show any particularly 
## trend in sale prices over time. However, there is an indication that sale of costlier 
## houses seem to have happen around Dec-Jan. It is to be noted that this just may be a 
## coincidence due to higher number of sales in that period.

## We can take a closer look at sale of lower priced houses by limiting the y axis.

ggplot(residential.data, aes(residential.data$sale.date.c,residential.data$sale.price.new))+geom_point()+
  scale_x_date(labels = date_format("%Y-%m"), breaks = "1 month")+ylim(0,1500)

## A closer look at the price data of cheaper houses shows that a higher number of sales 
## is concentrated in Dec- Jan and between July - Aug

## 2) Relationship between sale price and type of building

ggplot(residential.data, aes(residential.data$BUILDING.CLASS.CATEGORY,residential.data$sale.price.new)) + geom_boxplot()+scale_x_discrete(labels = abbreviate)

## Using this boxplot we can observe the range, the concentration and the average of 
## the sale prices across different building categories to understand the sale price 
## distribution in detail. For example, YOu may observe how Condos are relatively 
## cheaper than family homes.

## 3) Relationship between sale price and Neighborhood

ggplot(residential.data, aes(residential.data$NEIGHBORHOOD,residential.data$sale.price.new))+geom_boxplot()+scale_x_discrete(labels = abbreviate(residential.data$NEIGHBORHOOD,minlength = 1, strict = FALSE))

## This plot allows us to understand the nature of sale prices across neighbourhoods 
## similar to the chart with respect to building types.

# Next let us try to analyze the data across boroughs. 

## We would first need to import data and then clean the data using the steps discussed above.

res.data.bx <- read.xls("C:\\Users\\Anirudh Raghavan\\Desktop\\Stevens - Courses\\Financial Data Science - FE 582\\Assignment 1\\HW1_F19\\rollingsales_bronx.xls", perl =  "C:\\Perl64\\bin\\perl5.28.1.exe", pattern = "BOROUGH")
res.data.st <- read.xls("C:\\Users\\Anirudh Raghavan\\Desktop\\Stevens - Courses\\Financial Data Science - FE 582\\Assignment 1\\HW1_F19\\rollingsales_statenisland.xls", perl =  "C:\\Perl64\\bin\\perl5.28.1.exe", pattern = "BOROUGH")
res.data.br <- read.xls("C:\\Users\\Anirudh Raghavan\\Desktop\\Stevens - Courses\\Financial Data Science - FE 582\\Assignment 1\\HW1_F19\\rollingsales_brooklyn.xls", perl =  "C:\\Perl64\\bin\\perl5.28.1.exe", pattern = "BOROUGH")
res.data.qu <- read.xls("C:\\Users\\Anirudh Raghavan\\Desktop\\Stevens - Courses\\Financial Data Science - FE 582\\Assignment 1\\HW1_F19\\rollingsales_queens.xls", perl =  "C:\\Perl64\\bin\\perl5.28.1.exe", pattern = "BOROUGH")

## Modify the formatting of Sale Price

res.data.bx$sale.price.new <- as.numeric(gsub("[^[:digit:]]","",res.data.bx$SALE.PRICE))
res.data.st$sale.price.new <- as.numeric(gsub("[^[:digit:]]","",res.data.st$SALE.PRICE))
res.data.br$sale.price.new <- as.numeric(gsub("[^[:digit:]]","",res.data.br$SALE.PRICE))
res.data.qu$sale.price.new <- as.numeric(gsub("[^[:digit:]]","",res.data.qu$SALE.PRICE))

## Change Sale Price data for ease of reference

res.data.bx$sale.price.new <- res.data.bx$sale.price.new/1000
actual.sale.bx <- res.data.bx[res.data.bx$sale.price.new != 0,]

res.data.br$sale.price.new <- res.data.br$sale.price.new/1000
actual.sale.br <- res.data.br[res.data.br$sale.price.new != 0,]

res.data.st$sale.price.new <- res.data.st$sale.price.new/1000
actual.sale.st <- res.data.st[res.data.st$sale.price.new != 0,]

res.data.qu$sale.price.new <- res.data.qu$sale.price.new/1000
actual.sale.qu <- res.data.qu[res.data.qu$sale.price.new != 0,]

## Remove outliers

outliers.bx <- boxplot(actual.sale.bx$sale.price.new)$out
actual.sale1.bx <- actual.sale.bx[-which(actual.sale.bx$sale.price.new %in% outliers.bx),]
boxplot(actual.sale1.bx$sale.price.new)

outliers.br <- boxplot(actual.sale.br$sale.price.new)$out
actual.sale1.br <- actual.sale.br[-which(actual.sale.br$sale.price.new %in% outliers.br),]
boxplot(actual.sale1.br$sale.price.new)

outliers.st <- boxplot(actual.sale.st$sale.price.new)$out
actual.sale1.st <- actual.sale.st[-which(actual.sale.st$sale.price.new %in% outliers.st),]
boxplot(actual.sale1.st$sale.price.new)

outliers.qu <- boxplot(actual.sale.qu$sale.price.new)$out
actual.sale1.qu <- actual.sale.qu[-which(actual.sale.qu$sale.price.new %in% outliers.qu),]
boxplot(actual.sale1.qu$sale.price.new)

## Extract details with respect to residential property

residential.data.bx <- actual.sale1.bx[which(grepl(paste(Residential.Type, collapse = "|"),actual.sale1.bx$BUILDING.CLASS.CATEGORY)),]

residential.data.br <- actual.sale1.br[which(grepl(paste(Residential.Type, collapse = "|"),actual.sale1.br$BUILDING.CLASS.CATEGORY)),]

residential.data.qu <- actual.sale1.qu[which(grepl(paste(Residential.Type, collapse = "|"),actual.sale1.qu$BUILDING.CLASS.CATEGORY)),]

residential.data.st <- actual.sale1.st[which(grepl(paste(Residential.Type, collapse = "|"),actual.sale1.st$BUILDING.CLASS.CATEGORY)),]

## Now let us analyze the data across the groups

library(ggpubr)

Manhattan <- ggplot(residential.data, aes(x = residential.data$BUILDING.CLASS.CATEGORY, y = residential.data$sale.price.new))+geom_boxplot()+scale_x_discrete(labels = abbreviate)
Brooklyn <- ggplot(residential.data.br, aes(x = residential.data.br$BUILDING.CLASS.CATEGORY, y = residential.data.br$sale.price.new))+geom_boxplot()+scale_x_discrete(labels = abbreviate)
Staten.Island <- ggplot(residential.data.st, aes(x = residential.data.st$BUILDING.CLASS.CATEGORY, y = residential.data.st$sale.price.new))+geom_boxplot()+scale_x_discrete(labels = abbreviate)
Bronx <- ggplot(residential.data.bx, aes(x = residential.data.bx$BUILDING.CLASS.CATEGORY, y = residential.data.bx$sale.price.new))+geom_boxplot()+scale_x_discrete(labels = abbreviate)
Queens <- ggplot(residential.data.qu, aes(x = residential.data.qu$BUILDING.CLASS.CATEGORY, y = residential.data.qu$sale.price.new))+geom_boxplot()+scale_x_discrete(labels = abbreviate)

ggarrange(Manhattan, Brooklyn, Staten.Island, Bronx, Queens)

## We can see from the above graphs the comparison of house prices across different boroughs.
## We can see that prices in Manhattan are much higher than the rest. With Brooklyn coming second
## and Queens, Bronx and Staten Island have comparable prices.