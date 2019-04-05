#----------------------------------------------------------------------------#
######### Programming for Data Analysis, Processing & Visualisation ##########
#----------------------------------------------------------------------------#
################ Assignment 2: Vignesh Muthumani (10385771) ##################
#----------------------------------------------------------------------------#

##############################################################################
#################### Section 1: Data Visualisation I #########################
##############################################################################

# (i) -----------------------------------------------------------------------#

# installing the package MASS
install.packages("MASS")

# loading the installed package MASS
library(MASS)

# loading the dataset "quine"
data(quine)

# [optional]: command to access the quine dataset specifically from MASS
# package. This will prevent us from the possibility of loading a wrong
# quine dataset, if there were any from some other loaded R packages 
MASS::quine

# (ii) ----------------------------------------------------------------------#

# command used to access the description of the dataset is
?quine

# [or]
help(quine)

# (iii) ---------------------------------------------------------------------#

# command to display two charts side-by-side in a single panel. 
par(mfrow = c(1, 2))

# subsetting the dataset to obtain the data of the students who are aboriginal
# and not aboriginal

aboriginal <- subset(quine, Eth == 'A')

not.aboriginal <- subset(quine, Eth == 'N')

# visualizing the above subsetted datasets to see the days they were absent
# using histogram plot

hist(aboriginal$Days)

hist(not.aboriginal$Days)

# (iv) ----------------------------------------------------------------------#

# improvizing the histogram plots to make it look more sensible. In the  
# below commands, 'main' denotes the title of the plot. 'xlab' and 'ylab' 
# denote the x-axis and y-axis labels respectively. 'ylim' and 'xlim' denote
# the limits set to the y and x axis to visualize them in same scale. 'col'
# denotes the color set to bars. 'las=1' rotates the axes values to horizontal
# position

hist(aboriginal$Days, main = 'Absenteeism of aboriginal students',
     xlab = 'No. of days absent', ylab = 'No. of students', las = 1,
     ylim = c(0,50), xlim = c(0,100), col = 'Light Blue')

hist(not.aboriginal$Days, main = 'Absenteeism of not aboriginal students',
     xlab = 'No. of days absent', ylab = 'No. of students', las = 1,
     ylim = c(0,50), xlim = c(0,100), col = 'Pink')

# (v) -----------------------------------------------------------------------#

# 1. We can see that more no. of students of group 'not aboriginal' were
#    absent than the students of group 'aboriginal'. 
# 2. We can also see a significant difference in the no. of students who were
#    absent for less than ten days between the two groups.
# 3. The no. of students who were absent for less than 10 days of
#    'not aboriginal' group are nearly twice the no. of students of 
#    'aboriginal' group who were absent for the same no. of days.
# 4. It is to be also noted that no 'aboriginal' students were absent for
#    60-80 days, similarly no students of 'not-aboriginal' group were absent 
#    for 50-60 days.
# 5. We can also see that the 'Not-Aboriginal' students come to the school
#    more regularly than the aboriginal students. 

##############################################################################
#################### Section 2: Data Visualisation II ########################
##############################################################################

# (i) -----------------------------------------------------------------------#

# loading the installed package MASS
library(MASS)

# loading the dataset "crabs"
data(crabs)

# [optional]: command to access the crabs dataset specifically from MASS
# package. This will prevent us from the possibility of loading a wrong
# crabs dataset, if there were any from some other loaded R packages 
MASS::crabs

# reading the help file to see what the dataset contains
?crabs

# [or]
help(crabs)

# (ii) ----------------------------------------------------------------------#

# command to display two charts side-by-side in a single panel. 
par(mfrow = c(1, 2))

# subsetting male, female, blue and orange crabs from the dataset for plotting

male <- subset(crabs, sex == 'M')

female <- subset(crabs, sex == 'F')

blue <- subset(crabs, sp == 'B')

orange <- subset(crabs, sp == 'O')

# visualizing the subsetted datasets using boxplot

boxplot(male$RW, female$RW, main = 'Rear Width of Male and Female Crabs',
        col = c('lightblue','pink'), las = 1, ylim = c(6,21),  
        ylab = 'Rear Width', names = c('Male Crabs','Female Crabs'))

boxplot(blue$RW, orange$RW, main = 'Rear Width of Blue and Orange Crabs',
        col = c('darkblue','orange'), las = 1, ylim = c(6,21),
        ylab = 'Rear Width', names = c('Blue Crabs','Orange Crabs'))

# (iii) ---------------------------------------------------------------------#

# 1. The general relationship that can be observed between the two categorical 
#    variables (sex and species) is that there is a great similarity between
#    the distribution of the rear width of the male crabs and the blue crabs,
#    similarly between the female crabs and orange crabs.
# 2. The min, max, first quartile, third quartile, median values of the rear 
#    width of male and blue crabs are quite similar to each other.
# 3. Similarly, the min, max, first quartile, third quartile, median values of
#    the rear width of female and orange crabs are quite similar to each other
# 4. It can also be seen that the 1st quartile, median, 3rd quartile and max 
#    value of the rearwidth of the female crabs and orange crabs are higher  
#    than the rear width of male and blue crabs, although they all share a 
#    similar min value. 

# (iv) ----------------------------------------------------------------------#

# command to reset panel setup to one plot per panel
par(mfrow = c(1,1))

# subsetting RW of blue female, blue male, orange female & orange male crabs

blue.male <- subset(crabs, sp == 'B' & sex == 'M', 5)

blue.female <- subset(crabs, sp == 'B' & sex == 'F', 5)

orange.male <- subset(crabs, sp == 'O' & sex == 'M', 5)

orange.female <- subset(crabs, sp == 'O' & sex == 'F', 5)

# creating a list that comprises of the above 4 subsetted variables 
crabs.p <- c(blue.male, blue.female, orange.male, orange.female)

# plotting the crabs.p in a boxplot
boxplot(crabs.p, main = 'Rear Width of Crabs by Sex and Species',
        names = c('Blue Male', 'Blue Female', 'Orange Male', 'Orange Female'),
        col = 5:8, las = 1, ylab = 'Rear Width')

##############################################################################
#################### Section 3: Programming Structures #######################
##############################################################################

# (i) -----------------------------------------------------------------------#

# setting the values of x and y to 2 and 10 respectively 
x <- 2
y <- 10

# writing a while loop to print x+y, provided x<=40 or y<=1000
while(x<=40 & y<=1000) {
  print(x+y)
  x <- x^2
  y <- y^2
}

# the loop has stopped printing after 104 as the y value exceeded
# condition provided in the while loop

# (ii) ----------------------------------------------------------------------#

# creating a matrix of size 100x30 with data NA in it
m <- matrix(data=NA, nrow=100, ncol=30)

# writing a double 'for' loop to fill in values with the product of dimensions
# of its respective position
for (i in 1:100) {
  for (j in 1:30) {
    m[i,j] <- i*j
  } 
}

# checking the output
View(m)

# (iii) ---------------------------------------------------------------------#

# setting i to be 1
i <- 1

# writing a repeat loop to double the i until it is greater than 100
repeat{
  i <- i*2
  print(i)
  if(i>100) break
}

# [or]:
i <- 1
repeat{
  print(i)
  i <- i*2
  if(i>100) break
}

# Ans: In both the cases, the value of 'i' is 128 now. The loop stopped
# because 'i' has become greater than 100

##############################################################################
########################## Section 4: Web Scraping ###########################
##############################################################################

# checking the working directory
getwd()

# scanning the 'file3.txt' file from the working directory.
file3 <- scan('file3.txt', what = '', sep = '\n')
View(file3)

# finding the list of movies by looking for lines that start with 'title' 
movies <- file3[grep('^title',file3)]

# excluding the irrelevant lines at the top and bottom and accessing only from 
# line 6 to 255 to get the list of 250 movies
top250 <- movies[6:255]
top250 # checking the list of top 250 movies

# (i) -----------------------------------------------------------------------#

# finding the # of occurunces of the name 'Jodie FOster' in the movies list
length(grep('Jodie Foster', top250))

# Ans: Jodie Foster's name has appeared 2 times in the list

# (ii) ----------------------------------------------------------------------#

# accessing the lines in which Jodie Foster's name appears
Jodie <- top250[grep('Jodie Foster', top250)]
Jodie

# segregating the movie name separately from the other words in the lines
ind1 <- regexpr('>', Jodie)
ind2 <- regexpr('<', Jodie)

# printing the movies' names
Foster <- substr(Jodie, ind1+1, ind2-1)
Foster # checking the output

# Ans: From the output, we can see that the two movies she has acted in are: 
# 1. "Silence of the Lambs"
# 2. "Taxi Driver"

# (iii) ---------------------------------------------------------------------#

# finding the directors whose first name starts with 'S'
dir <- top250[grep('^title=\"S',top250)]

# segregating the directors' name separately from the other words in the lines
ind3 <- regexpr('\"',dir)
ind4 <- regexpr('(dir.)', dir)

dir.S <- substr(dir, ind3+1, ind4-3)
dir.S # checking the list

# counting the number of the directors whose first name started with S 
length(dir.S)

# Ans: 27 occurrences of directors' names that start with 'S'

# (iv) ----------------------------------------------------------------------#

# delimiting the file to separate the movie names from other information 
file3.m <- read.delim('file3.txt', sep='>', header=FALSE)
View(file3.m) # viweing the list

# accessing only the second column that contains the movie names
movies <- file3.m[,2]

# finding the movies that start with 'The'
movies1 <- movies[grep('^The',movies)]
movies1

# removing the irrelevant lines from the list to access only the movie names
movies2 <- movies1[5:59]
movies2

# segregating the movie names from the irrelevant characters attached to it 
ind5 <- regexpr('The', movies2)
ind6 <- regexpr('</a', movies2)

# printing the list of movies names starting with 'The'
movies.the <- substr(movies2, ind5, ind6-1)
movies.the

# counting the no. of movies that start with 'The' 
length(movies.the)

# Ans: 55 movie names start with 'The'

# (v) -----------------------------------------------------------------------#

# delimiting the file to separate the score from the other information 
rate <- read.delim('file3.txt', sep='<', header=FALSE)

# accessing only the second column that contains the score
score <- rate[,2]

# accessing the score by mentioning its prefix pattern
score250 <- score[grep('^strong title', score)]
score250

# removing the prefix pattern of score by substituting it with no space
rating <- gsub('strong title=', '', score250)
rating

# accessing only the first 3 charcters from the above list to get the scores
scores <- substr(rating, 1, 3)
scores

# coverting them to 'numeric' as they are currently stored as 'character'
scores.num <- as.numeric(scores)
scores.num

# subsetting to find out the scores that are greater than or equal to 8.4
scores8.4 <- scores.num[which(scores.num >= 8.4)]
scores8.4

# finding the count of ratings that are greater than or equal to 8.4
length(scores8.4)

# Ans: '68' movies have scores greater than or equal to 8.4

# (vi) ----------------------------------------------------------------------#

# using the 'score250' from the previous question to obtain the count of
# user ratings for each movie in the list
View(score250) # viewing it

# segregating the # of ratings separately from the other words in the lines
ind7 <- regexpr('based on', score250)
ind8 <- regexpr('user', score250)  

# printing the count of user ratings for each of the top 250 movies  
ratings <- substr(score250, ind7+9, ind8-2)
ratings 

# substituting the commas inbetween the characters with no space; to be able 
# to convert them into numbers
userratings <- gsub(',', '', ratings)
userratings

# coverting them to 'numeric' as they are currently stored as 'character'
ratings.num <- as.numeric(userratings)
ratings.num

# finding the sum of these count to determine the total no. of ratings used
# to define the top 250 movies
sum(ratings.num)

# Ans: Based on 113,418,965 user ratings, the top 250 movies were defined

##############################################################################
