#-------------Jessie------ Final-------------------

#5
library(UsingR)
team_sport <- rbind(c(39, 156, 25, 83), c(31, 98, 19, 75))
chisq.test(team_sport)$expected
chisq.test(team_sport)

#6
prob <- c(0.25, 0.4, 0.25, 0.05, 0.05)
grade_num <- c(36, 42, 60, 14, 8)
chisq.test(grade_num, p = prob)$expected
chisq.test(grade_num, p = prob)

#7
attach(fat)
perbody <- body.fat
hipc <- hip
body_hip.mod <- lm(perbody ~ hipc)

#Checking requirment
shapiro.test(resid(body_hip.mod))

print(summary(body_hip.mod))

new <- data.frame(hipc = 100)
predict(body_hip.mod, newdata = new)
confint(body_hip.mod, level = 0.98)

detach(fat)

#8
attach(diamonds)
#a)
levels(factor(cut))

diamond.mod <- lm(price ~ factor(cut), data = diamonds)
anova(diamond.mod)

#b)
diamond.mod1 <- lm(price ~ factor(cut)+ carat, data = diamonds)
anova(diamond.mod1)

#9
finals <- read_excel("finals.xlsx")
attach(finals)
dis <- dist_UCA
median(dis)
mean(dis)
sd(dis)
hist(dis)
boxplot(dis)

#10
a1 <- time_UCA[healt_conscious == "Yes"]

a2 <-  time_UCA[healt_conscious == "No"  || healt_conscious == "Maybe"]

t.test(a1,a2, alternative = "two.sided")

