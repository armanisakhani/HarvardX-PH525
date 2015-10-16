babies = read.table("babies.txt", header=TRUE)
bwt.nonsmoke = babies$bwt[babies$smoke==0]
bwt.smoke = babies$bwt[babies$smoke==1]

mytest <- t.test(s1, s2)
mytest$p.value
t <- mytest$conf.int

intervals <- matrix(rep(0 , 2000) , c(1000))

samp <- function(){
        sample.smoke <- sample(bwt.smoke , size = 30 ,replace = F)
        sample.nonsmoke <- sample(bwt.nonsmoke , size = 30 , replace = F)
        mytest <- t.test(sample.smoke, sample.nonsmoke)
        mytest$conf.int
}

for(i in 1:1000){
      intervals[i,] <- samp()
        
}
mean(intervals[,2] - intervals[,1])
intervals[1,] <- mytest$conf.int

popdiff = mean(bwt.nonsmoke) - mean(bwt.smoke)
mean(intervals[,2]<=popdiff & popdiff>=intervals[,1])
popdiff


