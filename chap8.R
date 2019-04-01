#---------------------Assignment6-----------------------------
library(UsingR)

satsco <- stud.recs$sat.m
length(satsco)

t.test(satsco, mu = 495, alternative = "two.sided")

library(distrEx)
library(BSDA)
tsum.test(mean.x = 4.03,s.x = 0.42, n.x  = 800, mu = 4.0,alternative = "greater")


xc<-c(80,90,81,99,100,75,88,89,77,87,91,67)
xi<-c(75,65,99,92,76,69,55,59)
t.test(xc, xi, var.equal = F, alternative = "greater")
?prop.test
prop.test(c(14,15), c(100, 125), alternative = "greater")
100*0.14*(1-0.14)
125*(15/125)*(1-15/125)
350*0.82*(1-0.82)
350*0.7*0.3

prop.test(c(350*0.82, 350*0.7), c(350,350), alternative = "two.sided")

fan<- father.son$fheight
son<- father.son$sheight
t.test(fan,son,paired = T, alternative = "less")

xu <- c(5.6,4.6,12.1,10.5,10.1,9.5,8.8,6.9)
xc <- c(13,10.4,14.3,12.9,6.9,15.7,17.5,8.4,16.7)

t.test(xu,xc,var.equal = F, alternative = "less")

x87<- c(234,257,242,215,113,287,316,228,193,205)
x92 <- c(236,239,228,225,118,297,350,242,187,208)

t.test(x87, x92,paired = T, alternative = "less")
rtime <- reaction.time$time
gen <- reaction.time$gender
mrtime <- rtime[gen == "M"]
frtime <- rtime[gen == "F"]
t.test(mrtime, frtime, var.equal = F, alternative = "two.sided")
