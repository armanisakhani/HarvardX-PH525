setwd("C:/Users/Arman/Dropbox/My courses/HarvardX PH525/01-Statistics and R for the Life Sciences/week 2 (introduction to random variable)")
rm(list = ls())
dat <- read.csv("femaleMiceWeights.csv")

mean(dat[13:24,2]) - mean(dat[1:12,2])



# Let's start by loading in the data that Rafa used in the video. Remember to click on "Raw" to download individual files from github.
# 
# dat = read.csv("femaleMiceWeights.csv")
# 
# The observed difference between high fat diet and control was calculated like so:
# 
# mean(dat[13:24,2]) - mean(dat[1:12,2])
# 
# Let's make a plot of these two groups: a strip chart of the weights. We're going to use some of the functions that we used from the assessments in Week 1, split() and soon sapply().

s = split(dat[,2], dat[,1])
stripchart(s, vertical=TRUE, col=1:2)

# Let's add the means to the plot as well:
  
  abline(h=sapply(s, mean), col=1:2)


sum(s[[2]]< mean(s[[1]]))
sum(s[[1]]> mean(s[[2]]))

highfat = s[["hf"]]
sum(highfat>30)/length(highfat)

population <- read.csv("femaleControlsPopulation.csv")
View(population)
population = population[,1]
mean(population)

dat <- as.data.table(dat)
dat
sum(dat[,Diet] == c("hf") & dat[,Bodyweight] > 30)/sum(dat[,Diet] == c("hf"))





# In the previous assessments, we created a vector of differences between means of random samples from the control population. This gives us a sense of the null distribution of differences if there is no true effect of a high fat diet. Let's recreate that vector:
# 
null = replicate(10000, mean(sample(population, 12)) - mean(sample(population, 12)))
# 
# The simple visualization of stacking the values which are close, in order to see the spread, is incredibly useful. Instead of actually writing the values on the plot, a more common visualization (which we will see again in Week 4), is a histogram. The histogram also puts the values into bins along the x-axis, but instead of writing the value, we just draw a vertical bar with the height equal to the number of values that fell in that bin:

hist(null)

# Let's return to the original difference we observed between the mice fed high fat diets and control mice:
  
  diff = mean(dat[13:24,2]) - mean(dat[1:12,2])

# Now what do we see when we add this difference to the histogram:
  
  abline(v=diff, col="red")

# If we look for the number of null distribution values to the right of the red line, we would say "we calculated the probability of observing a larger difference from the null distribution". This is sometimes called a "one-tailed" probability, because we only look at one "tail" of the histogram (the left and right sides where the bars become short). 
# 
# We can also add the negative of the difference:
  
  abline(v=-diff, col="red")

# By looking at the tails on both sides of the histogram, we can say "we calculated the probability of observing as extreme a difference from the null distribution". This is sometimes called a "two-tailed" probability. And as Rafa said in the video, this probability is commonly referred to as a p-value.

(sum(null>diff)+ sum(null<(-diff)))/length(null)





# In the video you just watched, Rafa looked at distributions of heights, and asked what was the probability of someone being shorter than a given height. In this assessment, we are going to ask the same question, but instead of people and heights, we are going to look at whole countries and the average life expectancy in those countries.
# 
# We will use the data set called "Gapminder" which is available as an R-package on Github. This data set contains the life expectancy, GDP per capita, and population by country, every five years, from 1952 to 2007. It is an excerpt of a larger and more comprehensive set of data available on Gapminder.org, and the R package of this dataset was created by the statistics professor Jennifer Bryan.
# 
# First, install the gapminder data set using the devtools R-package.

library(devtools)
install_github("jennybc/gapminder")

# Next, load the gapminder data set. To find out more information about the data set, use ?gapminder which will bring up a help file. To return the first few lines of the data set, use the function head().

library(gapminder)
data(gapminder)
head(gapminder)

# Create a vector 'x' of the life expectancies of each country for the year 1952. Plot a histogram of these life expectancies to see the spread of the different countries.
gapminder

data[]



