# Chapter 10

#1
murder <- c(63, 53, 50, 51, 55, 52, 56)
chisq.test(murder)

#2
library(UsingR)
mandms

prob <- mandms["milk chocolate",]
prob <- unlist(prob)
bagful <- c(15,34,7,19,29,24)
names(bagful) <- colnames(mandms)

chisq.test(bagful, p = prob/100)

prob1 <- mandms["Peanut",]
prob1 <- unlist(prob1)
bagful <- c(15,34,7,19,29,24)
names(bagful) <- colnames(mandms)

chisq.test(bagful, p = prob1/100)

#Checking Requirement
chisq.test(bagful, p = prob/100)$expected

#Example 3
hapiheal <- rbind(c(271, 261, 82, 20), c(247, 567, 231, 53), c(33, 103, 92,36))
chisq.test(hapiheal)
chisq.test(hapiheal)$expected

#Example 4
?samhda
#Remove missing value
levels(factor(samhda$amt.smoke))
newsamhda <- xtabs(~gender+amt.smoke, subset = amt.smoke < 98 & gender != 7, data = samhda)
newsamhda
#requirement(at least 80% > 5)
14*.8
chisq.test(newsamhda)$expected
chisq.test(newsamhda)
# if does not meet the requirement
chisq.test(newsamhda, simulate.p.value = T)


#Example 5
shop <- rbind(c(2,3,7), c(2,8,2))
rownames(shop) <- c("celexa", "placebo")
colnames(shop) <- c("worse", "same", "improved")
chisq.test(shop)$expected
set.seed(1)
chisq.test(shop, simulate.p.value = T)

#-------------------Assignment 9 ---------------------------
#1
die<- c(13, 17, 9, 17, 18, 26)
chisq.test(die)$expected
chisq.test(die)

#2
?pi2000
t1 <- table(pi2000)
chisq.test(t1)$expected
chisq.test(t1)

#3
?scrabble
scrabble
vowel<- c(28,39,23,22,11)
chisq.test(vowel)$expected
chisq.test(vowel)

#4
beltin <- rbind(c(12813, 647, 359, 42), c(65963, 4000, 2642, 303))
chisq.test(beltin)$expected
chisq.test(beltin)

#5
block<- rbind(c(18,15,5,8,4), c(10,5,7,18,10))
chisq.test(block)$expected
chisq.test(block)

#6
library(UsingR)
aq <- airquality[complete.cases(airquality),] 
aq <- transform(aq,
                te = cut(Temp, quantile(Temp)), 
                oz = cut(Ozone,quantile(Ozone))) 
table1<-xtabs(~ te + oz, data=aq)

chisq.test(table1)$expected
chisq.test(table1)

#7
mur <- rbind(c(53,42,51,45,36,37,65), c(63,53,50,51,55,52,56))

chisq.test(mur)$expected
chisq.test(mur)