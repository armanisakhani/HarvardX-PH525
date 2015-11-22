data <- read.csv(file = "femaleControlsPopulation.csv")
names(data)
hist(data[,"Bodyweight"])
Bodyweight <- data[,"Bodyweight"]
set.seed(1)
means5 <- sapply(1:1000, function(x) mean(sample(Bodyweight,5)))
hist(means5)
set.seed(1)
means50 <- sapply(1:1000, function(x) mean(sample(Bodyweight,50)))
hist(means50)
d <- ecdf(means50)
d(25) - d(23)
pnorm(25,mean = 23.9,sd = 0.43) - pnorm(23,mean = 23.9,sd = 0.43)

## part 2
rm(list = ls())
library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/mice_pheno.csv"
filename <- basename(url)
download(url, destfile=filename)
dat <- read.csv(filename) 
dat <- na.omit( dat )
library(dplyr)
str(dat)
x <- unlist(select(filter(.data = dat, Sex == "M", Diet == "chow"),Bodyweight))
mean(x)
library(rafalib)
popsd(x)
set.seed(1)
x25 <- mean(sample(x, 25))

y <- unlist(select(filter(.data = dat, Sex == "M", Diet == "hf"),Bodyweight))
mean(y)
popsd(y)
set.seed(1)
y25 <- mean(sample(y, 25))
mean(y) - mean(x)
abs((y25 - x25) - (mean(y) - mean(x)))

x <- unlist(select(filter(.data = dat, Sex == "F", Diet == "chow"),Bodyweight))
mean(x)
library(rafalib)
popsd(x)
set.seed(1)
x25 <- mean(sample(x, 25))

y <- unlist(select(filter(.data = dat, Sex == "F", Diet == "hf"),Bodyweight))
mean(y)
popsd(y)
set.seed(1)
y25 <- mean(sample(y, 25))
mean(y) - mean(x)
abs((y25 - x25) - (mean(y) - mean(x)))

## Part 3

1 - pnorm(-1)*2
1 - pnorm(-2)*2
1 - pnorm(-3)*2

y <- unlist(select(filter(.data = dat, Sex == "M", Diet == "chow"),Bodyweight))
d <- ecdf(y)
d(mean(y) + popsd(y)) - d(mean(y) - popsd(y))
d(mean(y) + 2*popsd(y)) - d(mean(y) - 2*popsd(y))
d(mean(y) + 3*popsd(y)) - d(mean(y) - 3*popsd(y))

y <- filter(dat, Sex=="M" & Diet=="chow") %>% select(Bodyweight) %>% unlist
avgs <- replicate(10000, mean( sample(y, 25)))
mypar(1,2)
hist(avgs)
qqnorm(avgs)
qqline(avgs)
mean(avgs)
popsd(avgs)

## part 4
set.seed(1)
n <- 100
p <- 1/6
result <- replicate(10000, mean(sample(1:6, n, replace=TRUE)==6))
z = (result - p) / sqrt(p*(1-p)/n)
d <- ecdf(z)
1 - (d(2) - d(-2))

library(downloader)
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleMiceWeights.csv"
filename <- "femaleMiceWeights.csv"
if(!file.exists("femaleMiceWeights.csv")) download(url,destfile=filename)
dat <- read.csv(filename)
library(dplyr)
X <- filter(dat, Diet=="chow") %>% select(Bodyweight) %>% unlist
Y <- filter(dat, Diet=="hf") %>% select(Bodyweight) %>% unlist
mean(X)
library(rafalib)
popsd(X)/12
sd(X)*sqrt(11/12)
z <- sqrt(12)*2/sd(X)
(1 - pnorm(z))*2
SE <- sqrt(var(X)/12+var(Y)/12)
t <- (mean(Y)- mean(X))/SE
(1-pnorm(t))*2
t.test(X, Y)
