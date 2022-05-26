# <- the hashtag # is the function for comments. comments will not be run in the console and are here to make your code understandable

# you can make a comment at any time, use it to remember what the code is doing in your own words
# there are always to less and never too much comments in a code!


###################################################
# Author: Fabian Kalleitner
# Title: R Intro 1 
# Purpose: Provide first insights into the functionality of R for Students
# Version: V2.0
# Date: 24-05-2022

###################################################
#install packages
###################################################

#rm(list = ls())     #clear/remove =rm everything at the start of a new session. Only needed when you want to ensure you start with a "clean" R environment 

# you only have to install packages when you are using it for the first time or when you want to update to a newer version

install.packages("ineq")      # package to easily calculate gini coefficients and to draw a lorenz-curve

install.packages("tidyverse") # package of packages This is Something like R 2.0. 
#It installs a lot of packages especially dplyr for data wrangling and ggplot for data vizualization I will use tidy and not "basic R" because it is more common now

install.packages("panelr") # package to easily handle panel data
# panel data = data from the same persons / countries has  been collected on multiple time points 
# cross sectional data = only data from one point in time
# repeated cross sectional data = ?

install.packages("srvyr") # package to handle survey weights


install.packages("stargazer") # export tables and regression results into word 
install.packages("modelsummary") # does the same but is more up to date

###################################################
#load packages
###################################################

# you always have to load packages at the start of a new R session (when you start R Studio or after you clean your environment (broomstick on the top right) -> )


library("ineq")
library("tidyverse"
) # R has no stop the line sign so it will automatically search for the end of the line and continue to the next line if it does not run into an unexpected symbol this is helpful if you want 
# to avoid to long lines of code that become completely unreadable

library("panelr")
library("srvyr")
library("modelsummary")
library("stargazer")

###################################################
#Import data
###################################################

#text data with delimiters (Begrenzer)

#read_delim
readr_example("mtcars.csv")

filepath <- readr_example("mtcars.csv")

?read_delim
read_delim(filepath, delim=",", col_names = TRUE)

data <- read_delim(filepath, delim=",", col_names = TRUE)

#double -> numeric vector that can store no characters
#character / char string -> character vector that understands values as strings "words"
#factor -> numeric vector with labels that understands values as nominal variables factors have both a numeric and a text value -> as.numeric(vector), levels(vector)

# you can also use the import dataset tool provided by Rstudio on the right (Button besides the broomstick)

#https://readr.tidyverse.org/reference/read_delim.html #load/import data from text


###################################################
#load data from foreign formats
###################################################

# SPSS datasets (.sav files) ##

#data = read_sav("./../data/raw/XYZ.sav")   # from haven package (don't forget to install the package beforehand and load it with library())
# . = the directory of the R file .. = go one directory downwards /data/raw  = the into the directorys data and raw/ get the file XYZ.sav

#data = read.spss("./../data/raw/XYZ.sav",to.data.frame = TRUE)  # same but slightly different function from the foreign package

# STATA datasets (.dta files) ##

#data = read_dta  for STATA data format one of the most popular statistic programs # from the haven package

#data = read.dta  for STATA data format one of the most popular statistic programs # from the foreign package

# EXCEL datasets (.xlsx files)  read_excel() # from the readxl package which is part of the tidyverse



###################################################
#View data
###################################################
#let's view our data
view(data) #attention this might load some time if you have large datasets

# list the variables in mydata
names(data)
# list the structure of mydata
str(data)

# print first 10 rows of mydata
head(data, n=10)

# print last 5 rows of mydata
tail(data, n=5)

#glimpse at the data - preferred with many collums
glimpse(data)

##################################################
#Data forms
##################################################

#the full data = data, data is a stored as a "dataframe" a dataframe is a matrix with labelled columns and maybe labelled rows, a matrix is a multirow vector, a vector is a single row of numbers or characters of the same type

# let's start with a vector

victor <- 2
victor

victor1 <- c(1,2,8)  # to combine several numbers or words use the function c() =  concatenate
victor1
class(victor1) # class of an object (numeric, matrix, data frame, etc) useful to determine the type - numeric factor character


victor2 <- c("hello","world","!") # instead of " you can also use the symbol ' to indicate strings
victor2
class(victor2)

# combine two vectors to a matrix 

matrix1<-cbind(victor1,victor2) #to combine row-wise use rbind
matrix1
class(matrix1)

# translate the matrix into a dataframe

df <- as.data.frame(matrix1)
df

# translate the columns to factor variables
df$victor1 <- as.factor(df$victor1)
df$victor2 <- as.factor(df$victor2)

##################################################
#Subsetting data
##################################################

df$victor1 # dataframe column names can be used to get a vector (column) of a dataframe
class(df$victor1) # the column has been imported as a factor -> we want a numeric column

as.numeric(df$victor1)

as.numeric(as.character(df$victor1))

df$victor1 <-as.numeric(levels(df$victor1)[df$victor1])

df$victor1[2] #to get a single value of a vector

df$victor1[2:3] #all values from 2 to 3 : works always you can also type in 2:5 and it will give the number tow to five

df[2,1] #row two column one

df[df$victor1==8,]  # take all rows in which victor 1 = 8   
# important logical operators in R are == which stands for = , 
# & which stands for and, 
# | which stands for or (bit more complex but also important %in% = identify if an element belongs to a vector)

df[c(FALSE,FALSE,TRUE),] 

################################################
#Let's use our new knowledge with our dataset
################################################

#let's view our variables again

names(data)

#variable names don't tell us much about the meaning of the the variables and the data they hold-> we need meta information about the data

#most common meta info for data are questionnaires(Fragebogen) for surveys and codebooks or data descriptions for surveys and other datasets

?mtcars # as mtcars is a test dataset R Itself provides a help file for the data

#THE CODEBOOK IS ESSENTIAL you always have to know what was actually measured and how the data was stored before you use the data -> search for your codebook until the next time we are meeting and ask questions

#lets look at two examples

#https://www.europeansocialsurvey.org/

#https://www.oecd.org/tax/taxing-wages-20725124.htm


# let's look again at the engine variable

data$vs

data$vs_factor<-as.factor(data$vs)

levels(data$vs_factor) <- c("V-shaped","straight")

data$vs_factor

#always check the recoding

mytable <- table(data$vs, data$vs_factor) # table makes a simple table. Vs will be rows vs_factor will be columns
mytable

#there exist several table functions: The most common are:

margin.table(mytable, 1) # row frequencies (summed over columns )
margin.table(mytable, 2) # column frequencies (summed over rows)

prop.table(mytable) # cell percentages --> 100% = 1.00
prop.table(mytable, 1) # row percentages
prop.table(mytable, 2) # column percentages 

# vs is a binary nominal variable you the mean of this variable has no immediate meaning except it can show the share of straight to V-shaped enginges

# simple essential  functions
#mean()   # calculate and show the mean of some variable

mean(data$mpg)  

mean(data$am)

#sum()    # calculate and show the sum of some variable

sum(data$am)

#length() # calculate and show the length of a vector

length(data$am)

sum(data$am)/length(data$am) # share of cars with automatic transmission

#is.na()  # is the content NA = Not available -> missing. 
#Missings in R are special values they do not count as 0. 
#Missing values often times have to be recoded or deleted if you want to calculate something.

is.na(data$am)

data$am2<-data$am
data$am2[12]<-NA
mean(data$am2)

# with na.rm = TRUE you can tell R to ignore missing values when calculating the mean value
mean(data$am2, na.rm=TRUE) 

#now lets make a plot with Base R#######################

#Scatterplot + trend line

plot(data$wt, data$mpg, main="Scatterplot Example",
     xlab="Car Weight ", ylab="Miles Per Gallon ", pch=19) 

abline(lm(data$mpg~data$wt), col="red") # add regression line (trend line) (y~x) 

myfirstregression <-lm(mpg ~ wt, data=data) # the ~ separates the dependent variable from the independent variable(s)

summary(myfirstregression)

#Barplot 

#where scatterplots don't look right
plot(data$am, data$mpg, main="Scatterplot Example 2",
     xlab="Car Transmission (0 = automatic, 1 = manual) ", ylab="Miles Per Gallon ", pch=19) 

plottable <- table(data$am)

barplot(plottable,
        main="Transmission of all cars",
        xlab="Car Transmission (0 = automatic, 1 = manual)",
        ylab="Count",
        col="blue"
)

#get averages miles per gallon across groups -> aggregate
mytable2 <- aggregate(data$mpg,by=list(data$am) , mean) # dont forget ,na.rm=TRUE if necessary
mytable2

barplot(mytable2$x) # misses information on what we can see here

colnames(mytable2) <- c("Car transmission","Average mpg")

mytable2$`Car transmission`<-as.factor(mytable2$`Car transmission`)
levels(mytable2$`Car transmission`)<-c("automatic","manual")

barplot(mytable2$'Average mpg',names=mytable2$'Car transmission',ylab = "average mpg")

t.test(data$mpg~data$am) # are group averages different from each other (on the sig. level p=0.05 yes)

