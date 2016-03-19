 
library("data.table", lib.loc="~/R/win-library/3.2")
babies <- read.table("babies.txt" , header = T)
bwt.nonsmoke = babies$bwt[babies$smoke==0]
bwt.smoke = babies$bwt[babies$smoke==1]

mean(bwt.nonsmoke)-mean(bwt.smoke)
sd(bwt.nonsmoke)
sd(bwt.smoke)


t.test(bwt.nonsmoke[1:30] , bwt.smoke[1:30])



library(devtools)
install_github("jennybc/gapminder")

library(gapminder)
data(gapminder)
head(gapminder)
index <- (1:nrow(gapminder))[gapminder[,"year"]==1952]
x <- gapminder[index , "lifeExp"]
mean(x<=40 )
names(gapminder)
prop = function(q) {
  mean(x <= q)
}
prop(40)
qs = seq(from=min(x), to=max(x), length=20)
qs
props = sapply(qs, prop)
plot(qs, props)
props = sapply(qs, function(q) mean(x <= q))
plot(ecdf(x))
names(gapminder)
population <- gapminder[index , "pop"]
hist(population)
hist(log(population , 10))
sd(log(population , 10))
x <- log(population , 10)
qqnorm(x)
z <- (x-mean(x))/sd(x)
qqnorm(z)
abline(0,1)
max(z)
F = function(q) pnorm(q, mean=mean(x), sd=sd(x))
pnorm(1)
n = length(x)
n * (F(7) - F(6))
sum(x > 6 & x <= 7)

n <- length(x)
qnorm(0.5/n)
