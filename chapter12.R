#Chapter12

#Example 1
intrestR <- c(18.982, 17.967, 12.218, 8.612, 6.680, 5.150)
creditS <- c(545,595, 640, 675, 705, 750)

#b)
credit.mod <- lm(intrestR ~ creditS)
print(summary(credit.mod))

#d) Checking requirment
shapiro.test(resid(credit.mod))

#F)for credit score = 723
new <- data.frame(creditS = c(723, 750, 670))
predict(credit.mod, newdata = new)

#g)Construct 95% CI for beta1
confint(credit.mod, level = 0.95)

#Scatter Plot
plot(creditS,intrestR)
abline(credit.mod)

#Example 2
library(UsingR)
?galton
#get variable names
names(galton)
height.mod <- lm(child~parent, data = galton)
shapiro.test(resid(height.mod))
 
#a)
print(summary(height.mod))

#d)
new2 <- data.frame(parent = 70)
predict(height.mod, newdata = new2)

#e)
confint(height.mod, level = 0.99) 

#Scatter plot
plot(galton$parent,galton$child)
abline(height.mod)

# Example 3
#Separating data
attach(normtemp)
names(normtemp)
femal_temp <- temperature[gender == 2]
male_temp <- temperature[gender == 1]

t.test(femal_temp, male_temp, var.equal = F, alternative = "two.sided")

is.factor(gender)
temp.mod <- lm(temperature ~ factor(gender), data = normtemp)
print(summary(temp.mod))

detach(normtemp)


#-------------Assignment7---------------------
#1
age <- c(25,25,38,32,32,32,38,43,48,51,51,58,62,65)
chol <- c(180, 195, 186, 180, 210, 197, 239, 183, 204, 221, 243, 208, 228, 269)
tc.mod <- lm(chol ~ age)
print(summary(tc.mod))
plot(age, chol)
abline(tc.mod)

shapiro.test(resid(tc.mod))

new3 <- data.frame(age = 30)
predict(tc.mod, newdata = new3)

confint(tc.mod, level = 0.98)

#2
?normtemp
rhr <- normtemp$hr
rtemp <- normtemp$temperature
hrtemp.mod <- lm(rtemp ~ rhr)
plot(rhr, rtemp)
abline(hrtemp.mod)

print(summary(hrtemp.mod))
shapiro.test(resid(hrtemp.mod))

new4 <- data.frame(rhr = 80 )
predict(hrtemp.mod, newdata = new4)

confint(hrtemp.mod, level = 0.99)

