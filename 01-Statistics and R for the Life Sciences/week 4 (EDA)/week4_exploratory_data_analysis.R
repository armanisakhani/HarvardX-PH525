## Part 2
load(file = "skew.RData")
dim(dat)
par(mfrow = c(3, 3))
str(dat)
for(i in 1:9){
      qqnorm(dat[,i])      
}

## Part 3
head(InsectSprays)
boxplot(split(InsectSprays[,"count"], InsectSprays[,"spray"]))
boxplot(InsectSprays[,"count"] ~ InsectSprays[,"spray"])
a <-split(InsectSprays, InsectSprays[,"spray"])
a

library(dplyr)
data(nym.2002, package="UsingR")
str(nym.2002)
boxplot(nym.2002[, "time"] ~ nym.2002[, "gender"])
par(mfrow = c(2,1))
hist(filter(nym.2002, gender == "Male") %>% select(time))
hist(filter(nym.2002, gender == "Female") %>% select(time) %>% unlist)
qqnorm(filter(nym.2002, gender == "Female") %>% select(time) %>% unlist)
qqnorm(filter(nym.2002, gender == "Male") %>% select(time) %>% unlist)

## Part 4

library(dplyr)
male <- filter(nym.2002, gender == "Male")
female <- filter(nym.2002, gender == "Female")
cor(male[, "time"], male[, "age"])
cor(female[, "time"], female[, "age"])
str(nym.2002)
par(mfrow = c(1,2))
groups_male <- split(male[,"time"], (as.integer(male[,"age"]/5)*5))
boxplot(groups_male)
groups_female <- split(male[,"time"], (as.integer(female[,"age"]/5)*5))
boxplot(groups_female)

## Part 5
time = sort(nym.2002$time)
head(time/median(time))
tail(time/median(time))

# 1) A plot of the ratio of times to the median time, with horizontal lines at twice
#    as fast as the median time, and twice as slow as the median time.

plot(time/median(time), ylim=c(1/4,4))
abline(h=c(1/2,1,2))

# 2) A plot of the log2 ratio of times to the median time. The horizontal lines indicate the same as above: twice as fast and twice as slow.

plot(log2(time/median(time)),ylim=c(-2,2))
abline(h=-1:1)


## Robust Summaries
#  part 1

data(ChickWeight)
head(ChickWeight)
plot( ChickWeight$Time, ChickWeight$weight, col=ChickWeight$Diet)
chick = reshape(ChickWeight, idvar=c("Chick","Diet"), timevar="Time",
                direction="wide")
head(chick)
chick = na.omit(chick)
w4 <- chick$weight.4
mean(c(w4, 3000)) / mean(w4)
median(c(w4, 3000)) / median(w4)
sd(c(w4, 3000)) / sd(w4)
mad(c(w4, 3000)) / mad(w4)
plot(x = chick$weight.4, y = chick$weight.21)
x = chick$weight.4; y = chick$weight.21
cor(c(x,3000), c(y, 3000), method = "spearman") / cor(x,y, method = "spearman")
cor(c(x,3000), c(y, 3000)) / cor(x,y)


# part 2
x <- chick[chick$Diet == 1, "weight.4"]
y <- chick[chick$Diet == 4, "weight.4"]
t.test(x,y)$p.value
t.test(c(x,200), y)$p.value
wilcox.test(x,y)
wilcox.test(c(x,200),y)

library(rafalib)
mypar(1,3)
boxplot(x,y)
boxplot(x,y+10)
boxplot(x,y+100)

t.test(x,y)$statistic
t.test(x,y+10)$statistic - t.test(x,y+100)$statistic

wilcox.test(1:3, 4:6)
wilcox.test(1:3, 4:6*100)
