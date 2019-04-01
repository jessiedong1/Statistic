#-----------------ANOVA---------------------------
?mtcars
factor(mtcars$cyl)

#a)
#method 1 - using linear model
cars.mod1 <- lm(mpg ~ factor(cyl), data = mtcars)
anova(cars.mod1)
print(summary(cars.mod1))

#Method 2 - 
cars.mod2 <- aov(mpg ~ factor(cyl), data = mtcars)
print(summary(cars.mod2))

#b)
#method 1 - using linear model
cars.mod3 <- lm(mpg ~ factor(cyl) + wt, data = mtcars)
anova(cars.mod3)
#print(summary(cars.mod1))

#Method 2 - 
cars.mod4 <- aov(mpg ~ factor(cyl) + wt, data = mtcars)
print(summary(cars.mod4))


#Example 2
#a)
#method 1 - using linear model
factor(mtcars$am)
cars.mod5 <- lm(mpg ~ factor(am), data = mtcars)
anova(cars.mod5)
#print(summary(cars.mod5))

#Method 2 - 
cars.mod6 <- aov(mpg ~ factor(am), data = mtcars)
print(summary(cars.mod6))

#b)
#method 1 - using linear model
cars.mod7 <- lm(mpg ~ factor(am) + hp, data = mtcars)
anova(cars.mod7)
#print(summary(cars.mod1))

#Method 2 - 
cars.mod8 <- aov(mpg ~ factor(am) + hp, data = mtcars)
print(summary(cars.mod8))

#----------------ASSIGNMENT------------------------
#1
?female.inc
factor(female.inc$race)
female.mod <- lm(income ~ factor(race), data = female.inc)
anova(female.mod)

#2
?babies
factor(babies$smoke)
babies.mod <- lm(wt ~ factor(smoke) + wt1, data = babies)
anova(babies.mod)

#3
?carsafety
factor(carsafety$type)
carsa.mod <- lm(Driver.deaths ~ factor(type), data = carsafety)
anova(carsa.mod)

#4
library(UsingR)
?nym.2002
factor(nym.2002$gender)
nym.mod <- lm(time ~ factor(gender) + age, data = nym.2002)
anova(nym.mod)

