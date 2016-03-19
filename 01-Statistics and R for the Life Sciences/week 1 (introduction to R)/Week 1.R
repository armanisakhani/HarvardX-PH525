setwd("01-Statistics and R for the Life Sciences/week 1 (introduction to R)")

## part 1
mean(c(2.23, 3.45, 1.87, 2.11, 7.33, 18.34, 19.23))
sum((1:25)^2)
class(cars)
nrow(cars)
colnames(cars)[2]
mean(cars[,2])
which(x = (cars[,2]==85))
rm(list=ls())

## part 2
# library(downloader) 
# url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleMiceWeights.csv"
# filename <- "femaleMiceWeights.csv" 
# if(!file.exists(filename)){
#   download(url, destfile=filename)
# }

femailMiceWeights <- read.csv(filename)
femailMiceWeights[12,2]
names(femailMiceWeights)
femailMiceWeights$Bodyweight[11]
nrow(femailMiceWeights)
mean((femailMiceWeights[which(femailMiceWeights$Diet == "hf"),]$Bodyweight))
set.seed(1)
index <- sample(13:24 , size=1)
femailMiceWeights[index,]$Bodyweight

## part 3
rm(list=ls())
# library(downloader)
# url="https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/msleep_ggplot2.csv"
# filename <- basename(url)
# 
# if(!file.exists(filename)){
#   download(url, destfile=filename)
# }

tab = read.csv("msleep_ggplot2.csv")
class(tab)
library(dplyr)
Primates <- filter(.data = tab, order == "Primates")
class(Primates)
tab %>%
  filter(order == "Primates") %>%
  select(sleep_total) %>%
  class

tab %>%
  filter(order == "Primates") %>%
  select(sleep_total) %>%
  unlist() %>%
  mean()

tab %>%
  filter(order == "Primates") %>%
  summarize(mean(sleep_total))

## Previous Course
head(tab)
dim(tab)
colnames(tab)
tab[1,]
plot(tab$brainwt , tab$sleep_total)
plot(tab$brainwt , tab$sleep_total , log ="x")
summary(tab$sleep_total)
mean(tab$sleep_total[tab$sleep_total>18])
which(tab$sleep_total>18 & tab$sleep_rem<3)
sort(tab$sleep_total)
order(tab$sleep_total)
tab$sleep_total[ order(tab$sleep_total) ]
match(c("Cow","Owl monkey","Cheetah"), tab$name)
match("Cotton rat" , tab$name)


vec = c("red","blue","red","green","green","yellow","orange")
fac = factor(vec)
fac
levels(fac)
vec == "blue"
fac2 = factor(vec, levels=c("blue","green","yellow","orange","red"))
fac2
levels(fac2)

table(tab$order)

mean(split(tab$sleep_total , tab$order)[["Rodentia"]])

s
class(s)

lapply(s, sd)

