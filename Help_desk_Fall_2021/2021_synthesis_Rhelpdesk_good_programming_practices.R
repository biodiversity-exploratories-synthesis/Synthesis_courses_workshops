####################################
#                                  #
# GOOD PROGRAMMING PRACTICES       #
#                                  #
####################################
# Biodiversity Exploratories Synthesis R helpdesk
# Noëlle Schenk 05.02.2021
#
# this file contains examples of practices to improve the
# readability of your code.
#
# Dependencies : 
#    The code in this script is not thought to be run, but rather
#    serves as an example. Therefore, no in- and output is needed
#    or generated.

library(data.table)
library(reshape2)
library(tidyr)
library(styler)

path_to_data <- "example_data_synthesis_helpdesk/"
path_to_output <- "R_outputs_and_plots/"

set.seed(12) # always generate the same random numbers, every time you run this script

###
# EXAMPLE - KEEP IT SIMPLE!  ####
#
# bad example : all in one line
info_data <- data.table::fread(paste(path_to_data, "/info_data.csv", sep = ""), header=T)
#
# good example : use many lines
info_data <- data.table::fread(
  paste(path_to_data, 
        "/info_data.csv", 
        sep = ""), 
  header=T)

# add structure to your file ####
# if you add four "#" in the end of the title, RStudio will allow
#   you to fold in and out the section

###
# EXAMPLE - relative paths over absolute paths ####
absolute_path <- "C:/user/yourname/Documents/Project1/Rscripts/data/info_data.csv"
relative_path <- "/data/info_data.csv"

# TRICK (Additional)
# Abbreviations in paths:
# "."  : refers to the folder where you are at the moment = the working directory
# the below paths point to the same directory
path_to_data <- "./example_data_synthesis_helpdesk/"
path_to_data <- "example_data_synthesis_helpdesk/"
# ".." : refers to the parent directory (1 level up) from where you are now (=from the working directory)
#   given your path structure looks like this : 
#   C:/user/yourname/Documents/PhD/Project1/Analysis/Rscripts/data/info_data.csv
#   and your working directory is "Rscripts"
#   both of the following path points to the folder "Analysis"
path_to_data <- "../"
path_to_data <- "C:/user/yourname/Documents/PhD/Project1/Analysis/"


###
# EXAMPLE - COMMENT YOUR CODE ####
#
# Bad example : comment the obvious
# read table example.csv
info_data <- data.table::fread("example.csv", header = T)
#
# Good example : comment the unexpected
# use package data.table to read in very large table "example.csv"
info_data <- data.table::fread("example.csv", header = T)


###
# EXAMPLE - PLAN YOUR PROGRAM  ####
#
# Describe your problem and the steps you need to do to solve it.
# Example problem : Plot samples from a normal distribution with mean = 0 and sd = 1.
# Example planning : 
#    - decide how many samples to take
#    - generate samples from a normal distribution with mean = 0 and sd = 1
#        - search internet which function can generate samples from normal distribution
#        - look up help of this function in R
#        - try out function with a small number of samples (e.g. 4)
#        - make vector x with normally distributed data
#    - make the plot
#        - search internet which function to use for plotting
#        - make title and axis labels

# we will take 40 samples
help(rnorm)
rnorm(4, mean = 0, sd = 1)
x <- rnorm(40, mean = 0, sd = 1) # first parameter : length of vector
plot(x)
# Additional : 
#    - evaluate your solution : are the points really from a normal distribution?

# plot histogram to evaluate distribution
hist(x) # looks reasonably normally distributed
# take mean and sd to compare with what was given (mean = 0 and sd = 1)
mean(x) # is around 0
sd(x)   # is around 1

# please note : rnorm will generate different samples every time, that means
#  the plots, mean and sd will look different every time.
#  If you wish to get the same results every time, you can "set a seed" with the
#  function set.seed().



###
# EXAMPLE - USE A CODE STYLE  ####
#
# examples taken from tidyverse code style 
# http://adv-r.had.co.nz/Style.html and https://style.tidyverse.org/syntax.html#long-lines

# Good
average <- mean(feet / 12 + inches, na.rm = TRUE)
# Bad
average<-mean(feet/12+inches,na.rm=TRUE)

# Good
do_something_very_complicated(
  something = "that",
  requires = many,
  arguments = "some of which may be long"
)
# Bad
do_something_very_complicated("that", requires, many, arguments,
                              "some of which may be long"
)

# Example use of styler with RStudio
# 1. copy bad example from above
# 2. Select the piece of code you wish to style
# 3. Go to RStudio Menu bar above > Addins > search for "styler" > click "Style selection"
average<-mean(feet/12+inches,na.rm=TRUE)
OneFile <- c(1,2,3) # styler does not correct variable names in CamelCase


###
# EXAMPLE - Test your code  ####
#
# create example data.frame
example <- data.frame(species = c("Erithacus rubecula", "Arion vulgaris", 
                                  "Plantago Lanceolata"),
                      abundance = c(2, NA, 10))
# action : recode NA value to 0
#    because Arion vulgaris was measured, but no individuals were found
example[which(example$species == "Arion vulgaris"), 2] <- 0
#
# test : did I do what I wanted?
sum(is.na(example$abundance)) # no NA values any more
sum(is.na(example$species))
min(example$abundance)        # min is 0 now, as expected
max(example$abundance)
View(example)
barplot(height = example$abundance, names.arg = example$species)
example$abundance


###
# EXAMPLE - NAMING THINGS  ####
#
# good names reduce comments
time_tolerance <- 60 # seconds
time_tolerance_in_seconds <- 60


###
# READ ERROR MESSAGES  ####
#
# read the error messages
x <- as.factor("A", "B", "C")
help("as.factor")
# the function encodes a vector, I gave the elements of a vector,
#    not the vector itself.
x <- as.factor(c("A", "B", "C"))