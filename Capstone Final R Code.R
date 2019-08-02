
# Install the car package
install.packages("car")
library(car)
library(ggplot2)
library(dplyr)

#Read hd2017 and ef2017a_dist files
library(readxl)
hd2017 <- read_excel("IDS_Course/CapstoneProject/hd2017.xlsx")
ef2017a_dist <- read_excel("IDS_Course/CapstoneProject/ef2017a_dist.xlsx")


#Data Wrangling 

#The following tasks were performed to prepare a data set for exploratory data analysis:
#1)	Merge the two data sets by UNITID 
#2)	Subset the merged data set to select all students (undergraduate and graduate) and exclude for profit colleges and universities (CONTROL=3) 
#3)	Select schools of interst 
#4)	Calculate variables of interest 
#5) Recode missing values 

#Merge the two files by UNITID 
merged2017 <- merge(ef2017a_dist, hd2017, by="UNITID")
#Select all students (undergraduate and graduate) and exclude for profit colleges and universities (CONTROL=3) 
merged_all_levels <- subset(merged2017, EFDELEV == "1" & CONTROL < 3 )

#Calculate: a) Percent of students who are enrolled exclusively in online programs and b) percent of students taking some online classes; run descriptive statistics for these two metrics 
attach (merged_all_levels)
perc100online <- EFDEEXC/EFDETOT
perc_some_online <- EFDESOM/EFDETOT

#Add new variables. 
data_new <- cbind(merged_all_levels,perc100online, perc_some_online, perc_out, perc_int) 

#Select only the variables of interest 
data_new <- subset(data_new, select= c(CONTROL, INSTNM, UNITID, STABBR,EFDETOT, HBCU, LOCALE, perc100online, perc_some_online, perc_out, MEDICAL,HOSPITAL, LANDGRNT, UGOFFER, GROFFER, HLOFFER, perc_int))

#select small schools only
data_new2 = subset(data_new2, EFDETOT > 500  & EFDETOT  < 5000)

##Explore Outliers
OutVals = boxplot(data_new2$perc100online, plot=FALSE)$out
View(OutVals)

#eliminate schools that are predominantly online - 80% or more students in online classes 
data_new2 = subset(data_new2, perc100online < .80  & perc_some_online < .80)

str(data_new2)




#recode missing cases 
data_new2$HOSPITAL[data_new2$HOSPITAL < 0] <- NA
data_new2$LOCALE[data_new2$LOCALE < 0] <- NA
data_new2$UGOFFER[data_new2$UGOFFER < 0] <- NA
data_new2$GROFFER[data_new2$GROFFER < 0] <- NA

data_new2$MEDICAL[data_new2$MEDICAL < 0] <- NA
data_new2$ICLEVEL[data_new2$ICLEVEL < 0] <- NA
data_new2$CONTROL[data_new2$CONTROL < 0] <- NA
data_new2$LANDGRNT[data_new2$LANDGRNT < 0] <- NA
data_new2$EFDETOT[data_new2$EFDETOT < 0] <- NA

#Convert numeric variables to categorical
data_new2$online[data_new2$perc100online > 0] <- "online programs"
data_new2$online[data_new2$perc100online == 0] <- "no online programs"
# Convert the column to a factor
data_new2$online <- factor(data_new2$online)

data_new2$some_online[data_new2$perc_some_online> 0] <- "some online enrollment"
data_new2$some_online[data_new2$perc_some_online == 0] <- "no online enrollment"
# Convert the column to a factor
data_new2$some_online <- factor(data_new2$some_online)

data_new2$CONTROL[data_new2$CONTROL ==1] <- "public"
data_new2$CONTROL[data_new2$CONTROL ==2 ] <- "private"
# Convert the column to a factor
data_new2$CONTROL <- factor(data_new2$CONTROL)

data_new2$UGOFFER[data_new2$UGOFFER ==1] <- "undergraduate degree"
data_new2$UGOFFER[data_new2$UGOFFER ==2 ] <- "no undergraduate offering"
# Convert the column to a factor
data_new2$UGOFFER <- factor(data_new2$UGOFFER)

data_new2$GROFFER[data_new2$GROFFER ==1] <- "graduate degree"
data_new2$GROFFER[data_new2$GROFFER ==2 ] <- "no graduate offering"
# Convert the column to a factor
data_new2$GROFFER <- factor(data_new2$GROFFER)

data_new2$MEDICAL[data_new2$MEDICAL ==1] <- "Grants medical degree"
data_new2$MEDICAL[data_new2$MEDICAL ==2] <- "Does not grant medical degree"
# Convert the column to a factor
data_new2$MEDICAL <- factor(data_new2$MEDICAL)

data_new2$LANDGRNT[data_new2$LANDGRNT ==1] <- "Land Grant Institution"
data_new2$LANDGRNT[data_new2$LANDGRNT ==2] <- "Not a Land Grant Institution"
# Convert the column to a factor
data_new2$LANDGRNT <- factor(data_new2$LANDGRNT)

data_new2$HOSPITAL[data_new2$HOSPITAL ==1] <- "Has Hospital"
data_new2$HOSPITAL[data_new2$HOSPITAL ==2] <- "No Hospital"
# Convert the column to a factor
data_new2$HOSPITAL <- factor(data_new2$HOSPITAL)

data_new2$HBCU[data_new2$HBCU ==1] <- "HBCU"
data_new2$HBCU[data_new2$HBCU ==2] <- "Not an HBCU"
# Convert the column to a factor
data_new2$HBCU <- factor(data_new2$HBCU)

data_new2$LOCALE[data_new2$LOCALE >= 11 & data_new2$LOCALE <=13] <- "city"
data_new2$LOCALE[data_new2$LOCALE >= 21 & data_new2$LOCALE <=23] <- "suburb"
data_new2$LOCALE[data_new2$LOCALE >= 31 & data_new2$LOCALE <=33] <- "town"
data_new2$LOCALE[data_new2$LOCALE >= 41 & data_new2$LOCALE <=43] <- "rural"
# Convert the column to a factor
data_new2$LOCALE <- factor(data_new2$LOCALE)

#Exploratory Data Analysis 

library(ggplot2)
#Distriibutions of continuous variables 
ggplot(data_new2, aes(x=EFDETOT))+ geom_histogram(color="darkblue", fill="lightblue")
ggplot(data_new2, aes(x=EFDETOT, color=HBCU)) + geom_histogram(fill="white", alpha=0.5, position="identity")
ggplot(data_new2, aes(x=perc100online))+ geom_histogram(color="darkblue", fill="lightblue")
ggplot(data_new2, aes(x=perc_some_online))+ geom_histogram(color="darkblue", fill="lightblue")
ggplot(data_new2, aes(x=perc_out))+ geom_histogram(color="darkblue", fill="red")

#Exploratory Data Analysis 
library(ggplot2)
#Distriibutions of continuous variables 
ggplot(data_new2, aes(x=EFDETOT))+ geom_histogram(color="darkblue", fill="lightblue")
ggplot(data_new2, aes(x=EFDETOT, color=HBCU)) + geom_histogram(fill="white", alpha=0.5, position="identity")
ggplot(data_new2, aes(x=perc100online))+ geom_histogram(color="darkblue", fill="lightblue")
ggplot(data_new2, aes(x=perc_some_online))+ geom_histogram(color="darkblue", fill="lightblue")
ggplot(data_new2, aes(x=perc_out))+ geom_histogram(color="darkblue", fill="red")
ggplot(data_new2, aes(x=perc_int))+ geom_histogram(color="darkblue", fill="red")


#Explore scatterplots for relationships between continuous variables  
ggplot(data_new2, aes(x = EFDETOT, y = perc100online)) + 
  geom_smooth(method = "lm")  + 
  geom_point() 

ggplot(data_new2, aes(x = EFDETOT, y = perc_some_online)) + 
  geom_smooth(method = "lm")  + 
  geom_point() 

##Explore differences in dependent variables by HBCU status 
ggplot(data= data_new2) + aes(x = HBCU, y = perc100online) +
  stat_summary(aes(fill = HBCU), fun.y=median, geom="bar")+
  stat_summary(aes(label=round(..y..,2)), fun.y=median, geom="text", size=3,
               vjust = -0.5)
ggplot(data= data_new2) + aes(x = HBCU, y = perc_some_online) +
  stat_summary(aes(fill = HBCU), fun.y=median, geom="bar")+
  stat_summary(aes(label=round(..y..,2)), fun.y=median, geom="text", size=3,
               vjust = -0.5)

##Explore differences in dependent variables by GROFFER status 
ggplot(data= data_new2) + aes(x = GROFFER, y = perc100online) +
  stat_summary(aes(fill = GROFFER), fun.y=median, geom="bar")+
  stat_summary(aes(label=round(..y..,2)), fun.y=median, geom="text", size=3,
               vjust = -0.5)

ggplot(data= data_new2) + aes(x = GROFFER, y = perc_some_online) +
  stat_summary(aes(fill = GROFFER), fun.y=median, geom="bar")+
  stat_summary(aes(label=round(..y..,2)), fun.y=median, geom="text", size=3,
               vjust = -0.5)

##Explore differences in dependent variables by UGOFFER
ggplot(data= data_new2) + aes(x = UGOFFER, y = perc100online) +
  stat_summary(aes(fill = UGOFFER), fun.y=median, geom="bar")+
  stat_summary(aes(label=round(..y..,2)), fun.y=median, geom="text", size=3,
               vjust = -0.5)

ggplot(data= data_new2) + aes(x = UGOFFER, y = perc_some_online) +
  stat_summary(aes(fill = UGOFFER), fun.y=median, geom="bar")+
  stat_summary(aes(label=round(..y..,2)), fun.y=median, geom="text", size=3,
               vjust = -0.5)


##Explore differences in dependent variables by CONTROL
ggplot(data= data_new2) + aes(x = CONTROL, y = perc100online) +
  stat_summary(aes(fill = CONTROL), fun.y=median, geom="bar")+
  stat_summary(aes(label=round(..y..,2)), fun.y=median, geom="text", size=3,
               vjust = -0.5)

ggplot(data= data_new2) + aes(x = CONTROL, y = perc_some_online) +
  stat_summary(aes(fill = CONTROL), fun.y=median, geom="bar")+
  stat_summary(aes(label=round(..y..,2)), fun.y=median, geom="text", size=3,
               vjust = -0.5)

##Explore differences in dependent variables by MEDICAL
ggplot(data= data_new2) + aes(x = MEDICAL, y = perc100online) +
  stat_summary(aes(fill = MEDICAL), fun.y=median, geom="bar")+
  stat_summary(aes(label=round(..y..,2)), fun.y=median, geom="text", size=3,
               vjust = -0.5)

ggplot(data= data_new2) + aes(x = MEDICAL, y = perc_some_online) +
  stat_summary(aes(fill = MEDICAL), fun.y=median, geom="bar")+
  stat_summary(aes(label=round(..y..,2)), fun.y=median, geom="text", size=3,
               vjust = -0.5)

##Explore differences in dependent variables by HOSPITAL
ggplot(data= data_new2) + aes(x = HOSPITAL, y = perc100online) +
  stat_summary(aes(fill = HOSPITAL), fun.y=median, geom="bar")+
  stat_summary(aes(label=round(..y..,2)), fun.y=median, geom="text", size=3,
               vjust = -0.5)

ggplot(data= data_new2) + aes(x = HOSPITAL, y = perc_some_online) +
  stat_summary(aes(fill = HOSPITAL), fun.y=median, geom="bar")+
  stat_summary(aes(label=round(..y..,2)), fun.y=median, geom="text", size=3,
               vjust = -0.5)

##Explore differences in dependent variables by LANDGRNT
ggplot(data= data_new2) + aes(x = LANDGRNT, y = perc100online) +
  stat_summary(aes(fill = LANDGRNT), fun.y=median, geom="bar")+
  stat_summary(aes(label=round(..y..,2)), fun.y=median, geom="text", size=3,
               vjust = -0.5)

ggplot(data= data_new2) + aes(x = LANDGRNT, y = perc_some_online) +
  stat_summary(aes(fill = LANDGRNT), fun.y=median, geom="bar")+
  stat_summary(aes(label=round(..y..,2)), fun.y=median, geom="text", size=3,
               vjust = -0.5)


##Explore differences in dependent variables by LOCALE
ggplot(data= data_new2) + aes(x = LOCALE, y = perc100online) +
  stat_summary(aes(fill = LOCALE), fun.y=median, geom="bar")+
  stat_summary(aes(label=round(..y..,2)), fun.y=median, geom="text", size=3,
               vjust = -0.5)

ggplot(data= data_new2) + aes(x = LOCALE, y = perc_some_online) +
  stat_summary(aes(fill = LOCALE), fun.y=median, geom="bar")+
  stat_summary(aes(label=round(..y..,2)), fun.y=median, geom="text", size=3,
               vjust = -0.5)

##Explore differences in dependent variables by HLOFFER
ggplot(data= data_new2) + aes(x = factor(HLOFFER), y = perc100online) +
  stat_summary(aes(fill = factor(HLOFFER)), fun.y=median, geom="bar")+
  stat_summary(aes(label=round(..y..,2)), fun.y=median, geom="text", size=3,
               vjust = -0.5)

ggplot(data= data_new2) + aes(x = factor(HLOFFER), y = perc_some_online) +
  stat_summary(aes(fill = factor(HLOFFER)), fun.y=median, geom="bar")+
  stat_summary(aes(label=round(..y..,2)), fun.y=median, geom="text", size=3,
               vjust = -0.5)

#Machine Learning Approaches
#1) Logistic Regression Model
#We split the data into two sets: training and test. The training set will be used to fit our model which we will be testing over the test set.
install.packages ("caTools")
library(caTools)
set.seed (88)
split=sample.split(data_new2$some_online, SplitRatio=.85) 
train = subset(data_new2, split ==TRUE)
test= subset(data_new2, split ==FALSE)

#Run logistic model and save results to mod1
mod1 <- glm(some_online ~ CONTROL + MEDICAL + HOSPITAL + LANDGRNT + UGOFFER + GROFFER + HBCU +EFDETOT + LOCALE + HLOFFER, data=train, family=binomial)
summary (mod1)
#transform the coefficients to make them easier to interpret
mod1c <- coef(summary(mod1))
mod1c[, "Estimate"] <- exp(coef(mod1))
mod1c


# Compute Area under ROC Curve (AUC) for to assess how well the model predicts
library(ROCR)
prob <- predict(mod1, newdata=test, type="response")
pred <- prediction(prob, test$some_online)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf) 
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
auc


#2) Linear Regression Model to test the importance of some_online
Lm1 <- lm(EFDETOT ~ CONTROL + MEDICAL + HOSPITAL + LANDGRNT + UGOFFER + GROFFER + HBCU +EFDETOT + LOCALE + HLOFFER +some_online, data=train)
summary (lm1)

## Examine Prediction Accuracy and Error Rate for the Linear Regression Model 
OnlinePred <- predict(lm1, data=test)
# make actuals_predicteds dataframe.
actuals_preds <- data.frame(cbind(actuals=test$EFDETOT, predicteds=OnlinePred)) 

min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))
min_max_accuracy


