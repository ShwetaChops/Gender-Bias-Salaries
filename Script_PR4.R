### Policy Report 4 - Engineering Students: Gender Bias in First Salary ###

#Set of libraries used throughout
library(readxl) #import data
library(lmSupport)
library(ggplot2)
library(lme4)
library(lattice)

#Clearing Working Environment
rm(list = ls())

#Setting Working Directory
setwd("/Users/shwetachopra/OneDrive - PennO365/Applied Linear Modeling/PR4")

#First Step is to Import the Dataset
good <- read_excel(("train.xlsx"))


#Extract data subset with relevant variables
economic.good <- cbind(good$ID,
                       good$Salary,
                       good$Gender,
                       good$g10percentage,
                       good$g12percentage,
                       good$Degree,
                       good$Specialization,
                       good$collegeGPA,
                       good$CollegeTier,
                       good$CollegeState,
                       good$GraduationYear,
                       good$English,
                       good$Logical,
                       good$Quant,
                       good$Domain)

#Set column names
colnames(economic.good) <- c("ID", "Salary", "Gender", "g10percentage", "g12percentage",
                             "Degree", "Specialization", "collegeGPA",
                             "CollegeTier", "CollegeState", "GraduationYear",
                             "English", "Logical", "Quant", "Domain")
economic.good <- as.data.frame(economic.good)
View(economic.good)

#Clean data
economic.good <- subset(economic.good, Degree == "B.Tech/B.E.")
economic.good$Gender <- ifelse(economic.good$Gender == "m", 0, ifelse(economic.good$Gender == "f", 1, "NA"))
economic.good$Domain <- ifelse(economic.good$Domain == -1, NA, as.character(economic.good$Domain))


#Fix data types
class(economic.good$Salary)
class(economic.good$Domain)
economic.good$Salary <- as.numeric(as.character(economic.good$Salary))
economic.good$Gender <- as.factor(economic.good$Gender)
economic.good$g10percentage <- as.numeric(as.character(economic.good$g10percentage))
economic.good$g12percentage <- as.numeric(as.character(economic.good$g12percentage))
economic.good$collegeGPA <- as.numeric(as.character(economic.good$collegeGPA))
economic.good$English <- as.numeric(as.character(economic.good$English))
economic.good$Logical <- as.numeric(as.character(economic.good$Logical))
economic.good$Quant <- as.numeric(as.character(economic.good$Quant))
economic.good$Domain <- as.numeric(economic.good$Domain)

#Convert Salary to dollar values at - $1 = Rs.70/- AND REMOVE OUTLIER SALARIES
economic.good <- subset(economic.good, Salary < 2000000)
economic.good$Salary <- (economic.good$Salary)/70
economic.good <- subset(economic.good, as.character(GraduationYear) != 0 || as.character(GraduationYear) !=  2007)

#Remove wrong GPA figures
economic.good <- subset(economic.good, collegeGPA > 10)

##PRELIMINARY ASSUMPTION DIAGNOSTICS
##Test for homoscedasticity in Simple Linear Model - BEFORE OUTLIER REMOVAL
lm_eng <- lm(economic.good$Salary ~ economic.good$Gender + economic.good$collegeGPA
             + factor(economic.good$CollegeTier) + factor(economic.good$GraduationYear) + economic.good$English +
               economic.good$Logical + economic.good$Quant + economic.good$Domain + factor(economic.good$Specialization))
plot(lm_eng) #Check residuals vs fitted values


#
#
#
#

##Test for Normality of Residuals in Simple Linear Model 1 - BEFORE CORRECTIONS
lm2 <- lm(economic.good$Salary ~ economic.good$Gender + economic.good$collegeGPA
          + factor(economic.good$CollegeTier) + factor(economic.good$GraduationYear) + economic.good$English +
            economic.good$Logical + economic.good$Quant + economic.good$Domain + factor(economic.good$Specialization))
good.res <- resid(lm2)
hist(good.res,10)

##Check for Multicollinearity - BEFORE CORRECTIONS 
lm_multi <- lm(economic.good$Salary ~ economic.good$Gender + economic.good$collegeGPA
               + factor(economic.good$CollegeTier) + factor(economic.good$GraduationYear) + economic.good$English +
                 economic.good$Logical + economic.good$Quant + economic.good$Domain + factor(economic.good$Specialization))
library(car)
vif(lm_multi)



## Analysis of Outliers
##-----------------------------##
#Outlier information for model 1, stored in new data frame
outliers <- na.omit(economic.good) #copy data regarding outliers

lm_out <- lm(outliers$Salary ~ outliers$Gender + outliers$collegeGPA
             + factor(outliers$CollegeTier) + factor(outliers$GraduationYear) + outliers$English +
               outliers$Logical + outliers$Quant + outliers$Domain + factor(outliers$Specialization))

outliers$cd <- cooks.distance(lm_out)

outliers$r <-rstudent(lm_out)
outliers$lev <- hatvalues(lm_out)
outliers$dffit<-dffits(lm_out)


# # #Discrepancy
# # #Our critical standardized residual values are 
# # #abs(standardized resid) > 2 (or 3)
# # #which seems more plausible?
# # #Let's plot to find out
# plot(outliers$r,
#      xlab="Index",
#      ylab="studentized residuals",
#      pch=19)
# # 
# # # Add fit lines
# # # regression line (y~x) 
# # abline(lm(r~ID, data=outliers), col="red")
# # 
# rejects <-subset(outliers,abs(r)>2)
# View(rejects)
# 
# outliers <-subset(outliers,abs(r)<2)
# # 
# # #LEVERAGE
# # #Our critical leverage value is
# # #leverage > (2k+2)/N 
# # #OR
# # # leverage > (2*15+2)/3699 (0.00865)
# # #Is this value too strict?
# # #Let's plot to find out
# plot(outliers$lev,
#      xlab="Index",
#      ylab="leverage",
#      pch=19)
# 
# rejects_lev <- subset(outliers,abs(lev) > 0.2)
# View(rejects_lev)
# 
# outliers <-subset(outliers,abs(lev) < 0.2)

##Influence - Cook's D
large_cd <- subset(outliers, cd > (4/3699))
View(large_cd)

library(Hmisc)
describe(large_cd$cd)
hist(large_cd$cd)
quantile(large_cd$cd, probs = seq(0, 1, 0.05))

##Mark outliers
good_out <- merge(economic.good, outliers[,c("ID", "cd")], by = "ID", all.x = TRUE)
good_out <- subset(good_out, is.na(cd) | cd < 4/3699) 
View(good_out) #New dataset without outliers




#Extract descriptive statistics
summary(good_out)


#Salary
print("Salary")
print(weighted.mean(good_out$Salary, na.rm = TRUE))
print(sd(good_out$Salary, na.rm = TRUE))
library(psych) #numeric
describe(good_out$Salary)
detach("package:psych", unload = TRUE)

#Gender
print("Gender")
count <- 0
for (i in 1:nrow(good_out)){
  if (good_out[i,4] == 0){
    count <- count + 1
  }
}
male_percentage <- count/nrow(good_out)
female_percentage <- 1 - male_percentage
library(Hmisc) #categorical 
describe(good_out$Gender)
detach("package:Hmisc", unload = TRUE)

#College Tier
print("College Tier")
count <- 0
for (i in 1:nrow(good_out)){
  if (good_out[i,9] == 1){
    count <- count + 1
  }
}
t1_percentage <- count/nrow(good_out)
t2_percentage <- 1 - t1_percentage

#collegeGPA
print("collegeGPA")
print(weighted.mean(good_out$collegeGPA, na.rm = TRUE))
print(sd(good_out$collegeGPA, na.rm = TRUE))
library(psych) #numeric
describe(good_out$collegeGPA)
detach("package:psych", unload = TRUE)

#English
print("English")
print(weighted.mean(good_out$English, na.rm = TRUE))
print(sd(good_out$English, na.rm = TRUE))
library(psych) #numeric
describe(good_out$English)
detach("package:psych", unload = TRUE)

#Logical
print("Logical")
print(weighted.mean(good_out$Logical, na.rm = TRUE))
print(sd(good_out$Logical, na.rm = TRUE))
library(psych) #numeric
describe(good_out$Logical)
detach("package:psych", unload = TRUE)

#Quant
print("Quant")
print(weighted.mean(good_out$Quant, na.rm = TRUE))
print(sd(good_out$Quant, na.rm = TRUE))
library(psych) #numeric
describe(good_out$Quant)
detach("package:psych", unload = TRUE)

#Domain
print("Domain")
print(weighted.mean(good_out$Domain, na.rm = TRUE))
print(sd(good_out$Domain, na.rm = TRUE))
library(psych) #numeric
describe(good_out$Domain)
detach("package:psych", unload = TRUE)

#CollegeState
print("CollegeState")
library(Hmisc) #categorical 
describe(good_out$CollegeState)
detach("package:Hmisc", unload = TRUE)

#Specialization
print("Specialization")
library(Hmisc) #categorical 
describe(good_out$Specialization)
detach("package:Hmisc", unload = TRUE)

### ASSUMPTION DIAGNOSTICS ###
#Checking for correlation between independent variables and the dependent variable
# cor(economic.good$Gender,economic.good$Salary)
# cor(economic.good$g10percentage,economic.good$Salary)
# cor(economic.good$g12percentage, economic.good$Salary)
# cor(economic.good$Specialization, economic.good$Salary)
# cor(economic.good$collegeGPA, economic.good$Salary)
# cor(economic.good$CollegeTier, economic.good$Salary)
# cor(economic.good$CollegeState, economic.good$Salary)
# cor(economic.good$English, economic.good$Salary)
# cor(economic.good$Logical, economic.good$Salary)
# cor(economic.good$Quant, economic.good$Salary)
# cor(economic.good$Domain, economic.good$Salary, use = "complete.obs")
# cor(economic.good$GraduationYear, economic.good$Salary)
# cor(economic.good, method = "pearson")
# 
# cor(economic.good$collegeGPA, economic.good$g10percentage)
# cor(economic.good$collegeGPA, economic.good$g12percentage)
# cor(economic.good$g10percentage, economic.good$g12percentage)

#Check for linearity of dependent variable versus all other variables
# ggplot(economic.good, aes(x = Gender, y = Salary)) +  geom_point(size = 0.6) +  xlab("Gender") +  ylab("Salary") +  theme_bw() +  geom_smooth(method = "loess")
# ggplot(economic.good, aes(x = g10percentage, y = Salary)) +  geom_point(size = 0.6) +  xlab("10th Percentage") +  ylab("Salary") +  theme_bw() +  geom_smooth(method = "loess")
# ggplot(economic.good, aes(x = g12percentage, y = Salary)) +  geom_point(size = 0.6) +  xlab("12th Percentage") +  ylab("Salary") +  theme_bw() +  geom_smooth(method = "loess")
# ggplot(economic.good, aes(x = Specialization, y = Salary)) +  geom_point(size = 0.6) +  xlab("Specialization") +  ylab("Salary") +  theme_bw() +  geom_smooth(method = "loess")
# ggplot(economic.good, aes(x = collegeGPA, y = Salary)) +  geom_point(size = 0.6) +  xlab("College GPA") +  ylab("Salary") +  theme_bw() +  geom_smooth(method = "loess")
# ggplot(economic.good, aes(x = CollegeTier, y = Salary)) +  geom_point(size = 0.6) +  xlab("College Tier") +  ylab("Salary") +  theme_bw() +  geom_smooth(method = "loess")
# ggplot(economic.good, aes(x = GraduationYear, y = Salary)) +  geom_point(size = 0.6) +  xlab("Graduation Year") +  ylab("Salary") +  theme_bw() +  geom_smooth(method = "loess")
# ggplot(economic.good, aes(x = English, y = Salary)) +  geom_point(size = 0.6) +  xlab("English score") +  ylab("Salary") +  theme_bw() +  geom_smooth(method = "loess")
# ggplot(economic.good, aes(x = Logical, y = Salary)) +  geom_point(size = 0.6) +  xlab("Logical score") +  ylab("Salary") +  theme_bw() +  geom_smooth(method = "loess")
# ggplot(economic.good, aes(x = Quant, y = Salary)) +  geom_point(size = 0.6) +  xlab("Quant score") +  ylab("Salary") +  theme_bw() +  geom_smooth(method = "loess")
# ggplot(economic.good, aes(x = Domain, y = Salary)) +  geom_point(size = 0.6) +  xlab("Domain score") +  ylab("Salary") +  theme_bw() +  geom_smooth(method = "loess")
# ggplot(economic.good, aes(x = CollegeState, y = Salary)) +  geom_point(size = 0.6) +  xlab("State") +  ylab("Salary") +  theme_bw() +  geom_smooth(method = "loess")



## Imputing missing data
# complete_good <- economic.good
# old.good <- economic.good

# #Replacing incomplete cases with Multiple Imputation#
# 
# # Create a dataframe for specification of the prior argument 
# #(row, column, min, max, confidence)
# Domainprior <- c(0,15,0,1,.99)
# 
# #We do not add for WICpreg as it does not make sense to impute a value for this variable (categorical + entire study depends on it)
# prior <- rbind(Domainprior)
# colnames(prior) <- c(1,2,3,4,5)
# prior <- as.matrix(prior)
# 
# bound_s <- c(15,0,1)
# bound <- rbind(bound_s)
# bound <- as.matrix(bound)
# 
# #By setting the seed we can reproduce our
# #results exactly
# set.seed(1000) 
# 
# good_out_multiple <- amelia( complete_good, m = 1, priors = prior, 
#                              boot.type = "none", bounds = bound, idvars = c("ID", "Gender", "Degree", "Specialization", "CollegeTier", "CollegeState", "GraduationYear",
#                                                             "g10percentage", "g12percentage", "collegeGPA", "English", "Logical", "Quant")) # Take note of the changes to the m = and parallel = argument.
# summary(good_out_multiple)
# 
# summary(good_out_multiple$imputations[[1]])
# 
# economic.good1 <- good_out_multiple$imputations[[1]] #Imputed dataset
# View(economic.good1)



#Check results
ggplot(good_out, aes(x = collegeGPA, y = Salary)) +  geom_point(size = 0.6) +  xlab("College GPA") +  ylab("Salary") +  theme_bw() +  geom_smooth(method = "loess")

##Test for Normality of Residuals in Simple Linear Model after removing outliers
lm3 <- lm(good_out$Salary ~ good_out$Gender + good_out$collegeGPA
          + factor(good_out$CollegeTier) + good_out$English +
            good_out$Logical + good_out$Quant + good_out$Domain + factor(good_out$GraduationYear) +
            factor(good_out$Specialization))
good.res <- resid(lm3)
hist(good.res,10)
plot(lm3)
boxplot(good.res,main="Boxplot of residuals") 
summary(lm3)

#Re-check for linearity of dependent variable versus all other variables
ggplot(good_out, aes(x = Gender, y = Salary)) +  geom_point(size = 0.6) +  xlab("Gender") +  ylab("Salary") +  theme_bw() +  geom_smooth(method = "loess")
ggplot(good_out, aes(x = g10percentage, y = Salary)) +  geom_point(size = 0.6) +  xlab("10th Percentage") +  ylab("Salary") +  theme_bw() +  geom_smooth(method = "loess")
ggplot(good_out, aes(x = g12percentage, y = Salary)) +  geom_point(size = 0.6) +  xlab("12th Percentage") +  ylab("Salary") +  theme_bw() +  geom_smooth(method = "loess")
ggplot(good_out, aes(x = Specialization, y = Salary)) +  geom_point(size = 0.6) +  xlab("Specialization") +  ylab("Salary") +  theme_bw() +  geom_smooth(method = "loess")
ggplot(good_out, aes(x = collegeGPA, y = Salary)) +  geom_point(size = 0.6) +  xlab("College GPA") +  ylab("Salary") +  theme_bw() +  geom_smooth(method = "loess")
ggplot(good_out, aes(x = CollegeTier, y = Salary)) +  geom_point(size = 0.6) +  xlab("College Tier") +  ylab("Salary") +  theme_bw() +  geom_smooth(method = "loess")
ggplot(good_out, aes(x = GraduationYear, y = Salary)) +  geom_point(size = 0.6) +  xlab("Graduation Year") +  ylab("Salary") +  theme_bw() +  geom_smooth(method = "loess")
ggplot(good_out, aes(x = English, y = Salary)) +  geom_point(size = 0.6) +  xlab("English score") +  ylab("Salary") +  theme_bw() +  geom_smooth(method = "loess")
ggplot(good_out, aes(x = Logical, y = Salary)) +  geom_point(size = 0.6) +  xlab("Logical score") +  ylab("Salary") +  theme_bw() +  geom_smooth(method = "loess")
ggplot(good_out, aes(x = Quant, y = Salary)) +  geom_point(size = 0.6) +  xlab("Quant score") +  ylab("Salary") +  theme_bw() +  geom_smooth(method = "loess")
ggplot(good_out, aes(x = Domain, y = Salary)) +  geom_point(size = 0.6) +  xlab("Domain score") +  ylab("Salary") +  theme_bw() +  geom_smooth(method = "loess")


#Fix non-linear transformations
good_out$collegeGPA2 <- good_out$collegeGPA ^ 2
good_out$Quant2 <- good_out$Quant ^ 2
good_out$Logical2 <- good_out$Logical ^ 2
good_out$Domain2 <- good_out$Domain ^ 2

## Assumption Diagnostics and Model Re-Specification - without scaling
lm4 <- lm(good_out$Salary ~ good_out$Gender + good_out$collegeGPA +
            good_out$collegeGPA2 + good_out$Quant2 + factor(good_out$CollegeTier) + good_out$English +
            good_out$Logical + good_out$Logical2 + good_out$Quant + good_out$Domain + good_out$Domain2 +
            factor(good_out$GraduationYear) +
            factor(good_out$Specialization))
summary(lm4)
good.res2 <- resid(lm4)
hist(good.res2,10)
plot(lm4)

library(car)
vif(lm4)


#Indirect effects
lmi1 <- lm(good_out$collegeGPA ~ good_out$Gender)
summary(lmi1)

lmi3 <- lm(good_out$English ~ good_out$Gender)
summary(lmi3)

lmi4 <- lm(good_out$Logical ~ good_out$Gender)
summary(lmi4)

lmi5 <- lm(good_out$Quant ~ good_out$Gender)
summary(lmi5)

lmi6 <- lm(good_out$Domain ~ good_out$Gender)
summary(lmi6)


#MALE DATASET
male <- subset(good_out, Gender == 0)

lm8 <- lm(male$Salary ~  male$collegeGPA +
            male$collegeGPA2 + male$Quant2 + factor(male$CollegeTier) + male$English +
            male$Logical + male$Logical2 + male$Quant + male$Domain + male$Domain2 +
            factor(male$GraduationYear) + factor(male$Specialization))
good.res8 <- resid(lm8)
hist(good.res8,10)
summary(lm8)
plot(lm8)

library(car)
vif(lm8)

#FEMALE DATASET
female <- subset(good_out, Gender == 1)

lm9 <- lm(female$Salary ~ female$collegeGPA +
            female$collegeGPA2 + female$Quant2 + factor(female$CollegeTier) + female$English +
            female$Logical + female$Logical2 + female$Quant + female$Domain + female$Domain2 +
            factor(female$GraduationYear) + factor(female$Specialization))
good.res9 <- resid(lm9)
hist(good.res9,10)
summary(lm9)
plot(lm9)

library(car)
vif(lm9)


lm4 <- lm(good_out$Salary ~ good_out$Gender + good_out$collegeGPA +
            good_out$collegeGPA2 + good_out$Quant2 + factor(good_out$CollegeTier) + good_out$English +
            good_out$Logical + good_out$Logical2 + good_out$Quant + good_out$Domain + good_out$Domain2 +
            factor(good_out$Specialization))

good.res2 <- resid(lm4)
hist(good.res2,10)
summary(lm4)
plot(lm4)
