#----------------------------------------------------------------------------#
######### Programming for Data Analysis, Processing & Visualisation ##########
#----------------------------------------------------------------------------#
################ Assignment 1: Vignesh Muthumani (10385771) ##################
#----------------------------------------------------------------------------#

##############################################################################
###################### Section 1: Data Manipulation ##########################
##############################################################################

# (i) -----------------------------------------------------------------------#

# installing the package ggplot2
install.packages("ggplot2", dependencies = TRUE)

# loading the installed package ggplot2
library(ggplot2)

# loading the dataset "diamonds"
data(diamonds)

# [optional]: command to access the diamonds dataset specifically from ggplot
# package. This will prevent us from the possibility of loading a wrong
# diamonds dataset, if there were any from some other loaded R packages 
ggplot2::diamonds

# The command needed to see the top 6 rows of the dataset is
head(diamonds)

# (ii) ----------------------------------------------------------------------#

# Command used to access the description of the dataset is
?diamonds

# (iii) ---------------------------------------------------------------------#

# Creating a dataframe called best.colour by subsetting only the diamonds
# with colour D
best.color <- subset(diamonds, color == "D")

# Viewing the newly created data frame
View(best.color)

# (iv) ----------------------------------------------------------------------#

# Ordering best.colour by carat, in decreasing order
best.color <- best.color[order(-best.color$carat),]

# checking if it is sorted
View(best.color)

# (v) -----------------------------------------------------------------------#

# removing the column "color" from the data frame by setting it to NULL
best.color$color <- NULL

# checking if the column has been removed
View(best.color)

# exporting the modified data frame to my working directory in .csv format
write.csv(best.color, file = "Vignesh.csv")

# checking the location of my working directory to see where the .csv file
# was saved
getwd()

# Ans: The exported file was saved successfully in my working directory 

# (vi) ----------------------------------------------------------------------#

# checking the structure of the modified data frame to see the no. of
# observations and variables in it
str(best.color)

# or alternatively I can use nrow() and ncol() commands to find it
nrow(best.color)
ncol(best.color)

# Ans: It has got 6,775 rows and 9 columns.

# (vii) ---------------------------------------------------------------------#

# creating a function called countG to count the number of diamonds with 
# colour G in the first n observations of the diamonds dataset.
countG <- function(n) {
  
          # subsetting the rows 1:n and 3rd column 'color' in dataset
          d <- diamonds[1:n,3]
          
          #calculating the no. of diamonds with color G in rows 1:n
          nrow(d[d$color == "G",])
          
          }

# Testing the function by providing the result for "n" as 50 and 150

countG(50)
# Ans: 1 diamond with color G in the first 50 rows

countG(150)
# Ans: 18 diamonds with color G in the first 150 rows

# (viii) --------------------------------------------------------------------#

# creating a function called countG.and.VS2 to count the number of diamonds
# with colour G and clarity VS2 in the first n observations of the dataset.
countG.and.VS2 <- function(n) {
  
  # subsetting rows 1:n and 3rd & 4th columns (color & clarity) in the dataset
  d <- diamonds[1:n,c(3,4)]
  
  #calculating the no. of diamonds with color G and clarity VS2 in rows 1:n
  nrow(d[d$color == "G" & d$clarity == "VS2",])
  
  }

# Testing the function by providing the result for n as 50 and 150

countG.and.VS2(50)
# Ans: 0 diamonds with color G and clarity VS2 in the first 50 rows

countG.and.VS2(150)
# Ans: 5 diamonds with color G and clarity VS2 in the first 150 rows

##############################################################################
##################### Section 2: Data Analysis ###############################
##############################################################################

# (i) -----------------------------------------------------------------------#

# [method 1]:

# sorting the diamonds dataset by price in descending order
diamonds <- diamonds[order(-diamonds$price),]

# accessing the first row of the dataset, because the most expensive diamond
# will be on top of the sorted list
most_expensive_diamond <- diamonds[1,]

# accessing the last 3 columns ("x","y","z") of the most expensive diamond 
# to find its size (dimensions)
most_expensive_diamond[,8:10]

# from the output, I can see that the dimensions of the most expensive 
# diamond are:
# x (length in mm) : 8.50
# y (width  in mm) : 8.47
# z (depth  in mm) : 5.16

# [method 2]:

# finding the price of most expensive diamond using max fn
max(diamonds$price)

# implementing the above function in a subset function to access the row of 
# the most expensive diamond
most.exp.diamond <- subset(diamonds, price == max(diamonds$price))

# accessing the last 3 columns ("x","y","z") of the most expensive diamond 
# to find its size (dimensions)
most.exp.diamond[,8:10]

# from the output, I can see that the dimensions of the most expensive 
# diamond are:
# x (length in mm) : 8.50
# y (width  in mm) : 8.47
# z (depth  in mm) : 5.16

# (ii) ----------------------------------------------------------------------#

# accessing the row of the diamond that costs USD 1277 using subset function
USD1277.diamond <- subset(diamonds, price == 1277)

# accessing the last 3 columns ("x","y","z") of the diamond that costs 
# USD 1277 to find its size (dimensions)
USD1277.diamond[,8:10]

# from the output, I can see that the dimensions of the diamond that costs
# USD 1277 are:
# x (length in mm) : 5.10
# y (width  in mm) : 5.13
# z (depth  in mm) : 3.17

# (iii) ---------------------------------------------------------------------#

# subsetting the diamonds that are "Ideal" cut
idealcut <- subset(diamonds, cut == "Ideal")

# ordering the ideal cut diamonds by price in desc. order
expensive.idealcut <- idealcut[order(-idealcut$price),]

# accessing the prices of 7 most expensive Ideal cut diamonds by subsetting 
# the first 7 rows and the 7th column (price)
expensive.idealcut[1:7,7]

# Ans: 18806, 18804, 18791, 18787, 18780, 18779, 18768

# (iv) ----------------------------------------------------------------------#

# subsetting the diamonds that are Ideal cut, best colour and best clarity 
# using AND operator
best.diamonds <- subset(diamonds, cut=="Ideal" & color=="D" & clarity=="IF")

# finding the no. of diamonds that satisfy those 3 conditions
nrow(best.diamonds)

# Ans: 28 diamonds

# (v) -----------------------------------------------------------------------#

# creating a table of the frequency of the clarity (in the rows) and the 
# colour (in the columns) of the diamonds. 
freq.table <- table(diamonds$clarity, diamonds$color)

# checking the output of the created table
freq.table

# by identifying the max num from this table, we can derive the most common 
# combination of clarity and color 
max(freq.table)

# Output: 2470

# Ans: from the table, we can see that the output is in the position [4,2],
# which is a combination of clarity VS2 and color E, making it the most 
# common combination

# [optional method]:

# converting the freq table into a data frame to access the column headers
ft <- as.data.frame(freq.table)

# naming the columns for the created data frame
colnames(ft) <- c("clarity", "color", "combinations")

# subsetting the rows and columns to find the highest combination of diamonds
most.common.combination <- ft[ft$combinations == max(ft$combinations),]

# checking the output by subsetting the first 2 columns to get the answer
most.common.combination[,c(1,2)]

# Ans: the output shows the diamonds of most common combination - "color E"  
# and "clarity VS2" 

# (vi) ----------------------------------------------------------------------#

# accessing the no. of diamonds of clarity IF and color H by accessing the 
# 8th row "IF" and 5th column "H" from the freq table
n <- freq.table[8,5]

# calculating the total number of diamonds
total <- sum(freq.table)

# calculating the probability of obtaining a diamond of combination with
# color H and clarity IF
prob <- n/total

# checking the output
prob

# rounding the output to two decimal places to get the answer
round(prob, digits = 2)

# Ans: 0.01 (0.0055 rounded to two decimals)
# The probability of choosing a diamond of combination with color H and 
# clarity IF is 0.5% (1% as per the rounded decimals)

# [optional method]:

# using the dataframe "ft" created from the freq.table
ft

# subsetting the rows and columns to find the no. of combinations of diamonds
# with clarity IF and color H
n <- ft[ft$clarity == "IF" & ft$color == "H",3]

# calculating the total number of diamonds
total <- sum(ft$combinations)

# calculating the probability of obtaining a diamond of combination with
# color H and clarity IF
prob <- n/total

# checking the output
prob

# rounding the output to two decimal places
round(prob, digits = 2)

# Ans: 0.01 (0.0055 rounded to two decimals)
# The probability of choosing a diamond of combination with color H and 
# clarity IF is 0.5% (1% as per the rounded decimals)

##############################################################################
