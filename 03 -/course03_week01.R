library(devtools)
load(file = "03 -/GSE5859Subset/data/GSE5859Subset.rda")
names(sampleInfo)
sum(as.Date(sampleInfo[,"date"]) == as.Date("2005-06-27"))
sum(geneAnnotation[!is.na(geneAnnotation[,"CHR"]), "CHR"] == "chrY")
index <- sampleInfo[,"date"] == as.Date("2005-06-10")
s <- sampleInfo[index, "filename"]
index <- geneAnnotation[!is.na(geneAnnotation[,"SYMBOL"]),"SYMBOL"] == "ARPC1A"
g <- geneAnnotation[index, "PROBEID"]
geneExpression[(1:nrow(geneExpression))[rownames(geneExpression) == g], (1:ncol(geneExpression))[colnames(geneExpression) == s]]

median(apply(geneExpression, 2, median))
test.group <- function(e, group){
  t.test(e[group == 0], e[group == 1])$p.value
}
g <- factor(sampleInfo$group)
min(apply(geneExpression, 1, FUN = function(x) test.group(x, g)))

## video 2
set.seed(1)
library(downloader)
url = "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleControlsPopulation.csv"
filename = "femaleControlsPopulation.csv"
if (!file.exists(filename)) download(url,destfile=filename)
population = read.csv(filename)
pvals <- replicate(1000,{
  control = sample(population[,1],12)
  treatment = sample(population[,1],12)
  t.test(treatment,control)$p.val
})
head(pvals)
hist(pvals)
sum(pvals <=0.05)/1000
sum(pvals <=0.01)/1000

set.seed(100)
pvals <- replicate(20, expr = {
  cases = rnorm(10,30,2)
  controls = rnorm(10,30,2)
  t.test(cases,controls)$p.val
})
sum(pvals < 0.05)

set.seed(100)
found <- replicate(1000, expr = {
  pvals <- replicate(20, expr = {
    cases = rnorm(10,30,2)
    controls = rnorm(10,30,2)
    t.test(cases,controls)$p.val
  })
  sum(pvals < 0.05)
})
mean(found)


set.seed(100)
had.found <- replicate(1000, expr = {
  pvals <- replicate(20, expr = {
    cases = rnorm(10,30,2)
    controls = rnorm(10,30,2)
    t.test(cases,controls)$p.val
  })
  sum(pvals < 0.05) > 0
})
mean(had.found)


source("http://bioconductor.org/biocLite.R")
biocLite("Biobase")

## FWER

alphas <- seq(0,0.25,0.01)
pvals <- runif(8793,0,1)

set.seed(1)
a <- 1-(1-0.05)^(1/8793)
z <- replicate(10000 , sum(runif(8793,0,1) <= a) )
mean(z)

set.seed(1)
a <- 0.05/8793
z <- replicate(10000 , sum(runif(8793,0,1) <= a) )
mean(z)

load("03 -/GSE5859Subset/data/GSE5859Subset.rda")
biocLite("genefilter")

library(genefilter)
?rowttests
a <- rowttests(x = geneExpression, fac = as.factor(c(rep(1,12), rep(0,12))))
class(a)
sum(a[,"p.value"] < 0.05)
sum(a[,"p.value"] < (0.05 /nrow(a) ))
sum(a[,"p.value"] < (1 - (1-0.05)^(1/8793)))

?p.adjust
sum(p.adjust(p = a[,"p.value"] , method = "fdr") < 0.05)
source("http://bioconductor.org/biocLite.R")
biocLite("qvalue")
library(qvalue)
?qvalue
c <- qvalue(p = a[,"p.value"] , fdr.level = 0.05)
names(c)
sum(c$qvalues < 0.05)
c$pi0

set.seed(1)
m0 <- 8793 - 500
n <- 24
m <- 8793
delta <- 2
positives <- 500
bonferroni <-c()
bonferroni.false.neg <- c()
alpha <- 0.05/m0
for(i in 1:1000){
  mat <- matrix(rnorm(n*m),m,n)
  mat[1:positives,1:(n/2)] <- mat[1:positives,1:(n/2)]+delta
  a <- rowttests(x = mat, fac = as.factor(c(rep(1,12), rep(0,12))))
  bonferroni <- c(bonferroni , sum(a[501:8793,"p.value"] < alpha ))
  bonferroni.false.neg <- c(bonferroni.false.neg , sum(a[1:500, "p.value"] >= alpha ))
}

mean(bonferroni.false.neg/500)


set.seed(1)
m0 <- 8793 - 500
n <- 24
m <- 8793
delta <- 2
positives <- 500
p.f.p <- c()
p.f.n <- c()
for(i in 1:1000){
  mat <- matrix(rnorm(n*m),m,n)
  mat[1:positives,1:(n/2)] <- mat[1:positives,1:(n/2)]+delta
  a <- rowttests(x = mat, fac = as.factor(c(rep(1,12), rep(0,12))))
  c <- qvalue(p = a[,"p.value"])
  p.f.p <- c(p.f.p, sum(p.adjust(p = a[, "p.value"], method = "fdr")[501:8792] < 0.05))
  p.f.n <- c(p.f.n, sum(p.adjust(p = a[, "p.value"], method = "fdr")[1:500] >= 0.05))
}
mean(p.f.p/m0)
mean(p.f.n/500)

set.seed(1)
m0 <- 8793 - 500
n <- 24
m <- 8793
delta <- 2
positives <- 500
q.f.p <- c()
q.f.n <- c()
for(i in 1:1000){
  mat <- matrix(rnorm(n*m),m,n)
  mat[1:positives,1:(n/2)] <- mat[1:positives,1:(n/2)]+delta
  a <- rowttests(x = mat, fac = as.factor(c(rep(1,12), rep(0,12))))
  c <- qvalue(p = a[,"p.value"])
  q.f.p <- c(q.f.p , sum(c$qvalues[500:8793] < 0.05))
  q.f.n <- c(q.f.n , sum(c$qvalues[1:500] >= 0.05))
  
}

mean(q.f.p/m0)
mean(q.f.n/500)

source("http://bioconductor.org/biocLite.R")
biocLite()

source("http://www.bioconductor.org/biocLite.R")
biocLite("SpikeInSubset")
library(SpikeInSubset)
data(mas133)

e <- exprs(mas133)
plot(e[,1],e[,2],main=paste0("corr=",signif(cor(e[,1],e[,2]),3)),cex=0.5)
k <- 3000
b <- 1000 #a buffer
polygon(c(-b,k,k,-b),c(-b,-b,k,k),col="red",density=0,border="red")

sum(e[,2] <= 3000 & e[,1] <= 3000)/nrow(e)

plot(log2(e[,1]),log2(e[,2]),main=paste0("corr=",signif(cor(log2(e[,1]),log2(e[,2])),2)),cex=0.5)
k <- log2(3000)
b <- log2(0.5)
polygon(c(b,k,k,b),c(b,b,k,k),col="red",density=0,border="red")

e <- log2(exprs(mas133))
plot((e[,1]+e[,2])/2,e[,2]-e[,1],cex=0.5)

log10(t.test(e[,1] , e[,2])$p.value)

nullpvals <- rowttests(e,c(0,0,0,1,1,1))$p.value
