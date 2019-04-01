library(UsingR)
#6
swt <- samhda$wt
salo <- samhda$alcohol
swtd <- swt[salo == 1]
length(swtd)
swtn <- swt[salo == 2]
length(swtn)
t.test(swtd, swtn, var.equal = F, alternative = "greater")

#Check the levels of the variables
levels(factor(salo))

attach(samhda)
wt_alcohol <- wt[alcohol == 1]
wt_noal <- wt[alcohol == 2]

#Check the requirement
length(wt_alcohol)
length(wt_noal)

t.test(wt_alcohol, wt_noal, var.equal = F, alternative = "greater")

#7
usize <- urchin.growth$size
usize
length(usize)
t.test(usize, conf.level = 0.9)

#8
#a)
1000*(104/1000)*(1-104/1000)
1200*(189/1200)*(1-189/1200)
prop.test(c(104,189),c(1000,1200), alternative = "two.sided")
#b)
prop.test(c(104,189),c(1000,1200), conf.level = 0.97)