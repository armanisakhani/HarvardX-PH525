rm(list=ls())
setwd("C:/Users/Arman/Dropbox/My courses/HarvardX PH525/01-Statistics and R for the Life Sciences/week 1 (introduction to R)")
tab = read.csv("msleep_ggplot2.csv")
class(tab)
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

