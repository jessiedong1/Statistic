#upsurvey1 <- read.csv("upsurvey1.csv", col_types = cols(class_meet = col_character()))

upsurvey1 <- read.csv("upsurvey1.csv")

choc <- upsurvey1$chocolate
sum(choc == "Milk")/66
prop.test(sum(choc == "Milk"),66, p = 0.4, alternative = "less") 

class12 <- upsurvey1$class_meet == "12:15 PM"


class2 <- upsurvey1$class_meet == "2:40 PM"
sum(class2)

names(upsurvey1)
attach(upsurvey1)
study_12 <- study[class_meet == "12:15 PM"]
study_2 <- study[class_meet == "2:40 PM"]
shapiro.test(study_12)

t.test(study_12, study_2, var.equal = F, alternative = "greater")

detach(upsurvey1)


attach(upsurvey1)
names(upsurvey1)
wt_j <- weight[j_cheese == "Yes"]
wt_jn <- weight[j_cheese == "Maybe" || j_cheese == "No"] 

levels(factor(TV))
tv_sam <- study[TV == "Samsung"]
tv_lg <- study[TV == "LG"]
tv_others <- study[TV == "Other" || TV == "Sony"]