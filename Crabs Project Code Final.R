library(tidyverse)
library(leaps)
library(MASS)
library(faraway)

Data <- crabs

#ELIMINATING A COLUMN THAT DOES NOT MAKE MUCH SENSE
Data <- subset(Data, select = -c(index) )

#GETTING BASIC INFO ABOUT DATA AND THE PREDICTORS
class(Data$sex)
class(Data$sp)

summary(Data)

pairs(Data) #all of them seem very linear 

#SEEING WHICH MODEL IT SUGGESTS TO USE WITH TESTS
allreg <- regsubsets(CW ~ ., data=Data, nbest = 2)
summary(allreg)

which.max(summary(allreg)$adjr2) #7

which.min(summary(allreg)$cp) #7

which.min(summary(allreg)$bic) #7

regnull <- lm(CW ~ 1, data = Data)
regfull <- lm(CW ~ ., data = Data)

step(regfull, scope = list(lower=regnull, upper = regfull), direction = "backward")
#all suggest lm(CW ~ sp + FL + RW + CL, data = Data)

#Linear Regression 
#EVALUATING MODEL WITH TESTS/SEEING IF NEED TO TRANSFORM
model1 <- lm(CW ~ sp + FL + RW + CL, data = Data)
summary(model1) 
#F stat is high and p-value low so passes F test
#p-values for each predictor are low so significant (do not need to drop)

vif(model1)
#Suggests multicollinearity so we remove CL (FL has higher score) 

model2 <- lm(CW ~ sp + FL + RW, data = Data)
summary(model2)
#RW has a high p-value (above .05) so we remove from model

model3 <- lm(CW ~ sp + FL, data = Data)
summary(model3)
#p-values and f-stat loog good so we move forward

contrasts(Data$sp)

vif(model3)
#no multicollinearity

boxcox(model3)
#1 is in interval but we will plot residuals to make sure

yhat <- (model3$fitted.values)
res <- model3$residuals
Data1 <- data.frame(Data, yhat, res)

ggplot(Data1, aes(x=yhat, y= res))+
  geom_point()+
  geom_hline(yintercept = 0, color = "red")
#residual plot looks really nice

acf(res, main = "ACF Plot of Residuals") # not appropriate as this is not a timeseries dataset - this will depend on the order the data is in. 
qqnorm(res) 
qqline(res, col="red") # QQ plot is clean. RLA #4 is met. 

#LEVERAGE AND OUTLIERS
n<-dim(Data)[1]
p<-2
crit<-qt(1-0.05/(2*n), n-1-p)
ext.student.res<-rstudent(model3)
ext.student.res[abs(ext.student.res)>crit]
#no outliers 
lev<-lm.influence(model3)$hat
lev[lev>2*p/n]
#lots of points with leverage

DFFITS<-dffits(model3)
DFFITS[abs(DFFITS)>2*sqrt(p/n)]
#lots have high DFFITS
COOKS<-cooks.distance(model3)
COOKS[COOKS>qf(0.5,p,n-p)]
#none


#LINEAR MODEL IF ONLY NEW SPECIES/ COLOR
model7 <- lm(CW ~ sp + sex, data = Data)
summary(model7)

model8 <- lm(CW ~ sp, data = Data)
summary(model8)
#R squared is so small (no numbers is why)

boxcox(model8)
#1 is in interval but we will plot residuals to make sure

yhat1 <- (model3$fitted.values)
res1 <- model3$residuals
Data2 <- data.frame(Data, yhat1, res1)

ggplot(Data1, aes(x=yhat1, y= res1))+
  geom_point()+
  geom_hline(yintercept = 0, color = "red")
#residual plot looks really nice

#LEVERAGE AND OUTLIERS
n1<-dim(Data)[1]
p1<-1
crit1<-qt(1-0.05/(2*n), n1-1-p1)
ext.student.res1<-rstudent(model8)
ext.student.res1[abs(ext.student.res1)>crit1]
#no outliers 
lev1<-lm.influence(model8)$hat
lev1[lev1>2*p1/n1]
#all the blue crabs have high leverage(since only using 1 predictor)

DFFITS1<-dffits(model8)
DFFITS1[abs(DFFITS1)>2*sqrt(p1/n1)]
#lots have high DFFITS
COOKS1<-cooks.distance(model8)
COOKS1[COOKS1>qf(0.5,p1,n1-p1)]
#none




#Logistic Regression
#logreg exploratory data analysis

#Logistic Regression Model with CW and CL as predictors for sp
model4 <- glm(sp ~ CW+CL, family = "binomial", data=train)
preds<- predict(model4, newdata = test, type = "response")
rates <- prediction(preds, test$sp)
roc_result <- performance(rates,measure="tpr", x.measure = "fpr")
plot(roc_result,main="ROC Curve")
lines(x=c(0,1), y=c(0,1), col = "red")
auc <- performance(rates,measure="auc")
auc@y.values
table(test$sp, preds >.5)
summary(model4)

#Logistic Regression Model with CW and SEX as predictors for sp
model5 <- glm(sp ~ CW+sex, family = "binomial", data=train)
preds1<- predict(model5, newdata = test, type = "response")
rates1 <- prediction(preds1, test$sp)
roc_result1 <- performance(rates1,measure="tpr", x.measure = "fpr")
plot(roc_result1,main="ROC Curve")
lines(x=c(0,1), y=c(0,1), col = "red")
auc1 <- performance(rates1,measure="auc")
auc1@y.values
table(test$sp, preds1 >.5)
summary(model5)

#Logistic Regression Model with CW and sp as predictors for sex
model6 <- glm(sex ~ CW + sp, family = "binomial", data=train)
preds2<- predict(model6, newdata = test, type = "response")
rates2 <- prediction(preds2, test$sex)
roc_result2 <- performance(rates2,measure="tpr", x.measure = "fpr")
plot(roc_result2,main="ROC Curve")
lines(x=c(0,1), y=c(0,1), col = "red")
auc2 <- performance(rates2,measure="auc")
auc2@y.values
table(test$sex, preds2 >.5)
summary(model6)

deltaG2<-model4$null.deviance-model4$deviance
deltaG2

1-pchisq(deltaG2,2)



# box plot
ggplot(train, aes(x =  sp, y = FL)) +
  geom_boxplot() +
  labs(title = "Crab Frontal Lobe Size and Species", x = "Species", y = "Frontal Lobe Size (mm)") + 
  theme(plot.title = element_text(hjust = 0.5))

# density plot
ggplot(train, aes(color = sp, x = FL)) +
  geom_density() + 
  labs(title = "Density Plot of Crab Frontal Lobe Size and Species", 
       color = "Species", x = "Frontal Lobe Size (mm)") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = c("cyan4", "chocolate2"))

# Rear Width vs. Species

# box plot
ggplot(train, aes(x =  sp, y = RW)) +
  geom_boxplot() +
  labs(title = "Crab Rear Width and Species", x = "Species", y = "Rear Width (mm)") + 
  theme(plot.title = element_text(hjust = 0.5))

# density plot
ggplot(train, aes(color = sp, x = RW)) +
  geom_density() + 
  labs(title = "Density Plot of Crab Rear Width and Species", 
       color = "Species", x = "Rear Width (mm)") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = c("cyan4", "chocolate2"))

# Carapace Length vs. Species

# box plot
ggplot(train, aes(x =  sp, y = CL)) +
  geom_boxplot() +
  labs(title = "Crab Carapace Length and Species", x = "Species", y = "Carapace Length (mm)") + 
  theme(plot.title = element_text(hjust = 0.5))

# density plot
ggplot(train, aes(color = sp, x = CL)) +
  geom_density() + 
  labs(title = "Density Plot of Crab Carapace Length and Species", 
       color = "Species", x = "Carapace Length (mm)") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = c("cyan4", "chocolate2"))

# Carapace Width vs. Species

# box plot
ggplot(train, aes(x =  sp, y = CW)) +
  geom_boxplot() +
  labs(title = "Crab Carapace Width and Species", x = "Species", y = "Carapace Width (mm)") + 
  theme(plot.title = element_text(hjust = 0.5))

# density plot
ggplot(train, aes(color = sp, x = CW)) +
  geom_density() + 
  labs(title = "Density Plot of Crab Carapace Width and Species", 
       color = "Species", x = "Carapace Width (mm)") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = c("cyan4", "chocolate2"))

# Body Depth vs. Species

# box plot
ggplot(train, aes(x =  sp, y = BD)) +
  geom_boxplot() +
  labs(title = "Crab Body Depth and Species", x = "Species", y = "Body Depth (mm)") + 
  theme(plot.title = element_text(hjust = 0.5))

# density plot
ggplot(train, aes(color = sp, x = BD)) +
  geom_density() + 
  labs(title = "Density Plot of Crab Body Depth and Species", 
       color = "Species", x = "Body Depth (mm)") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = c("cyan4", "chocolate2"))

# Carapace Width vs. Sex

# box plot
ggplot(train, aes(x =  sex, y = FL)) +
  geom_boxplot() +
  labs(title = "Crab Frontal Lobe Size and Sex", x = "Sex", y = "Frontal Lobe Size (mm)") + 
  theme(plot.title = element_text(hjust = 0.5))

# density plot
ggplot(train, aes(color = sex, x = FL)) +
  geom_density() + 
  labs(title = "Density Plot of Crab Frontal Lobe Size and Sex", 
       color = "Sex", x = "Frontal Lobe Size (mm)") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = c("chocolate2", "cyan4"))

# Carapace Width vs. Sex

# box plot
ggplot(train, aes(x =  sex, y = RW)) +
  geom_boxplot() +
  labs(title = "Crab Rear Width and Sex", x = "Sex", y = "Rear Width (mm)") + 
  theme(plot.title = element_text(hjust = 0.5))

# density plot
ggplot(train, aes(color = sex, x = RW)) +
  geom_density() + 
  labs(title = "Density Plot of Crab Rear Width and Sex", 
       color = "Sex", x = "Rear Width (mm)") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = c("chocolate2", "cyan4"))

# Carapace Width vs. Sex

# box plot
ggplot(train, aes(x =  sex, y = CL)) +
  geom_boxplot() +
  labs(title = "Crab Carapace Length and Sex", x = "Sex", y = "Carapace Length (mm)") + 
  theme(plot.title = element_text(hjust = 0.5))

# density plot
ggplot(train, aes(color = sex, x = CL)) +
  geom_density() + 
  labs(title = "Density Plot of Crab Carapace Length and Sex", 
       color = "Sex", x = "Carapace Length (mm)") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = c("chocolate2", "cyan4"))

# Carapace Width vs. Sex

# box plot
ggplot(train, aes(x =  sex, y = CW)) +
  geom_boxplot() +
  labs(title = "Crab Carapace Width and Sex", x = "Sex", y = "Carapace Width (mm)") + 
  theme(plot.title = element_text(hjust = 0.5))

# density plot
ggplot(train, aes(color = sex, x = CW)) +
  geom_density() + 
  labs(title = "Density Plot of Crab Carapace Width and Sex", 
       color = "Sex", x = "Carapace Width (mm)") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = c("chocolate2", "cyan4"))

# Carapace Width vs. Sex

# box plot
ggplot(train, aes(x =  sex, y = BD)) +
  geom_boxplot() +
  labs(title = "Crab Body Depth and Sex", x = "Sex", y = "Bpdy Depth (mm)") + 
  theme(plot.title = element_text(hjust = 0.5))

# density plot
ggplot(train, aes(color = sex, x = BD)) +
  geom_density() + 
  labs(title = "Density Plot of Crab Body Depth and Sex", 
       color = "Sex", x = "Body Depth (mm)") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = c("chocolate2", "cyan4"))
set.seed(1) ##for reproducibility
sample<-sample.int(nrow(Data), floor(.70*nrow(Data)), replace = F)
train<-Data[sample, ] ##training data frame
test<-Data[-sample, ] ##test data frame











