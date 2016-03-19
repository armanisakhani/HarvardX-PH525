library(data.table)
library(dplyr)

babies = read.table("babies.txt", header=TRUE)
bwt.nonsmoke <- filter(babies, smoke==0) %>% select(bwt) %>% unlist 
bwt.smoke <- filter(babies, smoke==1) %>% select(bwt) %>% unlist
library(rafalib)
mean(bwt.nonsmoke)-mean(bwt.smoke)
popsd(bwt.nonsmoke)
popsd(bwt.smoke)

set.seed(1)
dat.ns <- bwt.nonsmoke[sample(x = (1:length(bwt.nonsmoke)), size = 25)]
dat.s  <- bwt.smoke[sample(x = (1:length(bwt.smoke)), size = 25)]
t.val <- t.test(dat.ns, dat.s, conf.level = 0.99)
t.val
pnorm(abs(t.val$statistic)) - pnorm(-abs(t.val$statistic))
sqrt( var(dat.s)/length(dat.s) + var(dat.ns)/length(dat.ns) ) * -qnorm(0.005)


## PART 2
babies = read.table("babies.txt", header=TRUE)
bwt.nonsmoke <- filter(babies, smoke==0) %>% select(bwt) %>% unlist 
bwt.smoke <- filter(babies, smoke==1) %>% select(bwt) %>% unlist
set.seed(1)
dat.ns <- bwt.nonsmoke[sample(x = (1:length(bwt.nonsmoke)), size = 25)]
dat.s  <- bwt.smoke[sample(x = (1:length(bwt.smoke)), size = 25)]

sqrt( var(dat.s)/length(dat.s) + var(dat.ns)/length(dat.ns) ) * -qt(0.005,df = 48)

set.seed(1)
dat.ns <- bwt.nonsmoke[sample(x = (1:length(bwt.nonsmoke)), size = 5)]
dat.s  <- bwt.smoke[sample(x = (1:length(bwt.smoke)), size = 5)]
t.test(dat.s, dat.ns)

N <- 90
set.seed(1)
p.vals <- replicate(1e4, {
      a <- t.test(bwt.nonsmoke[sample(x = (1:length(bwt.nonsmoke)), size = N)],
             bwt.smoke[sample(x = (1:length(bwt.smoke)), size = N)])
      a$p.value
})
sum(p.vals <= 0.01) / 1e4

## MONTE CARLO
set.seed(1)
X <- rnorm(n = 5)
sqrt(5)*mean(X)/sd(X)

B <- 1000
N <- 50
set.seed(1)
z <- replicate(B, {
      X <- rnorm(n = N)
      sqrt(N)*mean(X)/sd(X)
})
sum(z>2)/B

B=100; ps = seq(1/(B+1), 1-1/(B+1),len=B)
qqplot(z , qt(ps,df=N-1))

z <- replicate(n = 1000, {
      X <- rnorm(n = N)
      median(X)
})
hist(z)

sd(z)
1/sqrt(N)

## PART2 MONTE CARLO

N=10
set.seed(1)
nonsmokers <- sample(bwt.nonsmoke , N)
smokers <- sample(bwt.smoke , N)
obs <- mean(smokers) - mean(nonsmokers)

set.seed(1)
z <- replicate(n = 1000,
      {dat <- c(smokers,nonsmokers)
      shuffle <- sample( dat )
      smokersstar <- shuffle[1:N]
      nonsmokersstar <- shuffle[(N+1):(2*N)]
      mean(smokersstar)-mean(nonsmokersstar)
      }
)
obs
sum(abs(z) >abs(obs))/1e3
mean(abs(z) > abs(obs))

N=10
set.seed(1)
nonsmokers <- sample(bwt.nonsmoke , N)
smokers <- sample(bwt.smoke , N)
obs <- median(smokers) - median(nonsmokers)

set.seed(1)
z <- replicate(n = 1000,
               {dat <- c(smokers,nonsmokers)
               shuffle <- sample( dat )
               smokersstar <- shuffle[1:N]
               nonsmokersstar <- shuffle[(N+1):(2*N)]
               median(smokersstar) - median(nonsmokersstar)
               }
)
obs
mean(abs(z) > abs(obs))


## PART 3 MONTE CARLO

d = read.csv("assoctest.csv")
table(d)
