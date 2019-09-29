## First we shall load the data using the read.csv command

NYT.Day1 <- read.csv("nyt1.csv")

## The data provided to us is with respect to simulated details of online ad sales of New 
## York Times. We shall next, observe the details of the variables provided in the dataset

names(NYT.Day1)

## Create a new variable, age_group, that categorizes users as "<20", "20-29", "30-39","40-49", 
## "50-59", "60-69", and "70+".

NYT.Day1$AgeCat <- cut(NYT.Day1$Age, c(-Inf,20,29,39,49,59,69,Inf), right = TRUE, labels = c("<20","20-29","30-39","40-49","50-59","60-69","70<"))
summary(NYT.Day1$AgeCat)

## We next load the ggplot package to use plots in order to analyze the data provided.

library(ggplot2)

## Next we shal plot the number of impressions with respect to age categories to 
## understand the nature of distrubution

ggplot(NYT.Day1,aes(x = AgeCat, y = Impressions, fill = AgeCat))+geom_boxplot()

## A look at the graphs shows us that the distribution of impressions is simirlarly 
## distributed across age categories.

## Next we shall analyzie the click through rate ("CTR") which is the proportion of 
## clicks in comparison to the numer of impressions.

## Before we get down to analyzing the CTR, we would need to remove the rows that 
## do not have any clicks or impressions as the same may distort the data.

NYT.Day1$hasimp <- cut(NYT.Day1$Impressions,c(-Inf,0,Inf), labels = c("No", "Yes"))
NYT.Day1.Imp <- NYT.Day1[NYT.Day1$hasimp == "Yes",]
summary(NYT.Day1$hasimp)

## Now we create the CTR variable.

NYT.Day1.Imp$CTR <- NYT.Day1.Imp$Clicks/NYT.Day1.Imp$Impressions
summary(NYT.Day1.Imp$CTR)

## We shall now plot the boxplot of CTR as per different age categories. It is 
## important to note that the number of persons with zero cliks is very high 
## and this could distort the boxplot. We will create a boxplot with the zero 
## clicks and without zero clicks to show the difference.

ggplot(NYT.Day1.Imp, aes(x = AgeCat, y = CTR, fill = AgeCat))+geom_boxplot()

ggplot(subset(NYT.Day1.Imp, Clicks > 0), aes(x = AgeCat, y = CTR, fill = AgeCat))+geom_boxplot()

## As you would see from the above, the first plot is very difficult to read 
## because of the number of rows with zero clicks. Given the same, we shall 
## create a new dataframe with only clicks greater than zero in order to 
## study the same in detail.

NYT.Day1.Clicks <- NYT.Day1.Imp[NYT.Day1.Imp$Clicks >0,]

## Further, a look at the CTR plot (above) shows that similar to impressions, 
## the CTR distrubition is similar across age groups.However, it is to be 
## noted that the average CTR for people less than 20 seems to be more than 
## the remaining age categories.

## Now let us try to group users based on CTR. Alook at the CTR plot shows 
## us that the CTR rate is mostly concentrated between 0.25 to 0.1 and we 
## can use this cateogrisation to obtain a better idea about the data.

NYT.Day1.Imp$CTRCat <- cut(NYT.Day1.Imp$CTR, c(-Inf,0,0.1,0.15,0.20,0.25,0.30,.50,0.75,1,Inf))
summary(NYT.Day1.Imp$CTRCat)

## This data shows us that the CTR is concentrated between 0.1 to 0.2 with 
## the highest numer of CTR between 0.15 to 0.20.  

## Next we shall try to analyze the CTR between Gender Categories and Signed-in 
## and unsigned-in users. First let us check the type of the data under gender 
## before plotting

mode(NYT.Day1$Gender)

## As you can see the data under gender is currently numeric and we should turn 
## the same into factors. Further it is to be noted that the data is currently in 
## 0 and 1s. We can convert the same into the respective gender names for ease of 
## reference. Please note that in case, we plan to undertake numeric analysis for 
## gender then the type and name of variables should be maintained in numeric. 
## Given the same we shall create a separate variable - "Gender1" for plotting 
## of variables. It is also to be noted that the general principle is to denote
## females as 0 and males as 1, We shall accordingly convert the data.

NYT.Day1.Clicks$Gender1 <- as.factor(NYT.Day1.Clicks$Gender)
levels(NYT.Day1.Clicks$Gender1) <- c("Female", "Male")
summary(NYT.Day1.Clicks$Gender1)

## Now we shall plot the CTR of males and females to see if there are any observations.

ggplot(NYT.Day1.Clicks, aes(NYT.Day1.Clicks$CTR, fill = NYT.Day1.Clicks$Gender1))+geom_histogram()

ggplot(NYT.Day1.Clicks, aes(NYT.Day1.Clicks$CTR, colour = NYT.Day1.Clicks$Gender1))+geom_density()

## AS you can see from the above density plot and histogram females on an average 
## seem to have a higher click rate compared to men.

## If required, we can also zoom into the density plot to see the differences more 
## clearly using the xlim function.

ggplot(NYT.Day1.Clicks, aes(NYT.Day1.Clicks$CTR, colour = NYT.Day1.Clicks$Gender1))+geom_density()+xlim(0,.3)

## Similarly we can create respective graphs for signed in and non signed in users

mode(NYT.Day1.Clicks$Signed_In)

NYT.Day1.Clicks$Sign.Fac <- as.factor(NYT.Day1.Clicks$Signed_In) 

summary(NYT.Day1.Clicks$Sign.Fac)

## Generally, signed-in users are referenced to as 1 and anonymous users are denoted 
## by 0. We shall now convert the same to the appropriate labels.

levels(NYT.Day1.Clicks$Sign.Fac) <- c("Signed-In", "Annonymous")

## Now we shall plot the CTR of signed-in and anonymous users to see if there are 
## any observations.

ggplot(NYT.Day1.Clicks, aes(NYT.Day1.Clicks$CTR, fill = NYT.Day1.Clicks$Sign.Fac))+geom_histogram()

ggplot(NYT.Day1.Clicks, aes(NYT.Day1.Clicks$CTR, colour = NYT.Day1.Clicks$Sign.Fac))+geom_density()

ggplot(NYT.Day1.Clicks, aes(NYT.Day1.Clicks$CTR, colour = NYT.Day1.Clicks$Sign.Fac))+geom_density()+xlim(0,.3)

## The CTR for signed-in users and anonymous users are similar with signed in users 
## having more number of CTR in the <0.15 range while anonymous users are greater 
## in the 0.15< range.

## We can now load the data for the next 2 days and create a plot with respect to 
## the number of impressions occuring across the 2 days.

NYT.Day2 <- read.csv("nyt2.csv")

NYT.Day2.Clicks <- NYT.Day2[NYT.Day2$Clicks >0,]

NYT.Day3 <- read.csv("nyt3.csv")

NYT.Day3.Clicks <- NYT.Day3[NYT.Day3$Clicks >0,]

library(ggpubr)

Day1 <- ggplot(subset(NYT.Day1.Clicks, Clicks > 0), aes(NYT.Day1.Clicks$Impressions))+geom_density()

Day2 <- ggplot(subset(NYT.Day2, NYT.Day2$Clicks > 0), aes(NYT.Day2.Clicks$Impressions))+geom_density()

Day3 <- ggplot(subset(NYT.Day3, NYT.Day3$Clicks > 0), aes(NYT.Day3.Clicks$Impressions))+geom_density()

ggarrange(Day1, Day2, Day3)

## A look at the above graphs shows that the impressions behaviour of users has remained 
## similar across the 3 days.