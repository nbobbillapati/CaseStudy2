#Reading in data and review high level summary
dat <- read.csv("CaseStudy2-data.csv",na.strings=c(""))
head(dat)
summary(dat)
sapply(dat,function(x) sum(is.na(x))) #check if there are any missing values

#Remove the columns that do not give any useful information.
dat <- subset(dat, select = -c(Over18,EmployeeCount,StandardHours,na.strings)) 

# Create variable for age group
#attach(dat)
#dat$AgeGroup[Age <= 24] <- "18-24"
#dat$AgeGroup[Age > 24 & Age <=34] <- "25-34"
#dat$AgeGroup[Age > 34 & Age <=44] <- "35-44"
#dat$AgeGroup[Age > 44 & Age <=54] <- "45-54"
#dat$AgeGroup[Age > 54 & Age <=64] <- "55-64"
#dat$AgeGroup[Age >= 65] <- "65+"
#detach(dat)
dat$Att <- ifelse(dat$Attrition == "Yes", 1,0)
dat$Gender <- ifelse(dat$Gender == "Male", 1,0)

dat$EnvironmentSatisfaction <- factor(dat$EnvironmentSatisfaction)
dat$JobInvolvement<- factor(dat$JobInvolvement)
dat$JobLevel <- factor(dat$JobLevel)
dat$JobSatisfaction <- factor(dat$JobSatisfaction)
dat$PerformanceRating <- factor(dat$PerformanceRating)
dat$RelationshipSatisfaction <- factor(dat$RelationshipSatisfaction)
dat$StockOptionLevel <- factor(dat$StockOptionLevel)
dat$WorkLifeBalance <- factor(dat$WorkLifeBalance)
dat$NumCompaniesWorked <- factor(dat$NumCompaniesWorked)


#Exploration via boxplots (only left in the ones that had significant differences - others are noted in the Notes file)
#Looks like the mean age of those the did leave companies is lower than those who didn't.
ggplot(dat, aes(x=Attrition, y=Age))+geom_boxplot()+scale_x_discrete(name = "Attrition") +
  scale_y_continuous(name = "Age") + ggtitle("Attrition by Age")+theme_bw()

#The spread of these is about the same, but the average and interquartile spread are higher for those who left (meaning longer commutes may indicate higher attrition)
ggplot(dat, aes(x=Attrition, y=DistanceFromHome))+geom_boxplot()+scale_x_discrete(name = "Attrition") +
  scale_y_continuous(name = "Distance From Home") + ggtitle("Attrition by Commute Distance")+theme_bw()

#People seem more likely to leave if they have been working for fewer years
ggplot(dat, aes(x=Attrition, y=TotalWorkingYears))+geom_boxplot()+scale_x_discrete(name = "Attrition") +
  scale_y_continuous(name = "Total Years Working") + ggtitle("Attrition by Work Experience (Years)")+theme_bw()

#Looks like attrition might be more likely when there's less training time in the previous year
ggplot(dat, aes(x=Attrition, y=TrainingTimesLastYear))+geom_boxplot()+scale_x_discrete(name = "Attrition") +
  scale_y_continuous(name = "Training Times Last Year") + ggtitle("Attrition by Training (Instances in Previous Year)")+theme_bw()+geom_jitter()

#Really skewed, looks like fewer years at the company might be an indicator for attrition
ggplot(dat, aes(x=Attrition, y=YearsAtCompany))+geom_boxplot()+scale_x_discrete(name = "Attrition") +
  scale_y_continuous(name = "Years At Company") + ggtitle("Attrition by Years At Company")+theme_bw()

#Fewer years in role, more likely to leave 
ggplot(dat, aes(x=Attrition, y=YearsInCurrentRole))+geom_boxplot()+scale_x_discrete(name = "Attrition") +
  scale_y_continuous(name = "Years In Current Role") + ggtitle("Attrition by Years In Current Role")+theme_bw()

#Fewer years with current manager, more likely to leave
ggplot(dat, aes(x=Attrition, y=YearsWithCurrManager))+geom_boxplot()+scale_x_discrete(name = "Attrition") +
  scale_y_continuous(name = "Years With Current Manager") + ggtitle("Attrition by Years With Current Manager")+theme_bw()


#Histograms showing possible needs for transformation:
library(ggplot2)
ggplot(dat, aes(x=DistanceFromHome))+geom_histogram()+
  scale_x_continuous(name="Commute Mileage")+
  ggtitle("Distribution of Commute Mileage")
ggplot(dat, aes(x=MonthlyIncome))+geom_histogram()+
  scale_x_continuous(name="Monthly Income")+
  ggtitle("Distribution of Monthly Income")
ggplot(dat, aes(x=NumCompaniesWorked))+geom_histogram()+
  scale_x_continuous(name="Number of Companies Worked For")+
  ggtitle("Distribution of Number of Employers")
ggplot(dat, aes(x=TotalWorkingYears))+geom_histogram()+
  scale_x_continuous(name="Total Working Years") +
  ggtitle("Distribution of Working Years")
ggplot(dat, aes(x=YearsAtCompany))+geom_histogram()+
  scale_x_continuous(name="Years at Company")+ 
  ggtitle("Distribution of Employment Tenure")
ggplot(dat, aes(x=YearsSinceLastPromotion))+geom_histogram()+ 
  scale_x_continuous(name="Years Since Last Promotion")+
  ggtitle("Distribution of Years since Last Promotion")


#first create a subset of just the continuous variables
cont <- subset(dat, select=c(Att, Age,DailyRate,DistanceFromHome,HourlyRate,MonthlyIncome,MonthlyRate,NumCompaniesWorked,PercentSalaryHike,TotalWorkingYears,TrainingTimesLastYear,YearsAtCompany,YearsInCurrentRole,YearsSinceLastPromotion,YearsWithCurrManager))
library(Hmisc)
library(corrplot)
#Run correlation matrix
res <- rcorr(as.matrix(cont))
#Plot the correlations to see which ones are highly important
corrplot(res$r,type="upper",p.mat=res$P, sig.level=0.1, insig="blank", tl.cex=0.5,tl.col="black")
# Variables related to years are all significantly correlated (TotalWorkingYears,YearsAtCompany,YearsInCurrentRole,YearsSinceLastPromotion,YearsWithCurrManager).
# Age is significantly correlated with TotalWorkingYears and MonthlyIncome.
# MonthlyIncome is significantly correlated with the variables related to years (see above comment for list).
# By adding Att to this, it's possible to see which continous variables are correlated to Attrition

# This might not be needed (since "Att" can be treated as continuous in the above correlation matrix)
#Diving in to testing the continous variables impact on Attrition
#Run an ANOVA to test if continuous variable impacts categorical outcome
AgeTest <- aov(Age~Attrition,data=dat)
summary(AgeTest)
#Formula to get a correlation between the categorical "Attrition" and a continuous variable 
#install.packages("ICC")
library(ICC)
AgeEffect <- ICCbare(Attrition,Age, dat)
#We could probably turn this into a loop to run all of these tests and read them out as a matrix


#Examining correlations with Categorical variables
#Create a function to look at Kramer correlations between categorical variables
catcorrm <- function(vars, dat) sapply(vars, function(y) sapply(vars, function(x) assocstats(table(dat[,x], dat[,y]))$cramer))
#Create a vector of the names of the categorical variables from the data set
catvars <- c("Attrition","BusinessTravel","Department","EducationField","Gender","JobRole","MaritalStatus","OverTime","EnvironmentSatisfaction","JobInvolvement","JobLevel","JobSatisfaction","PerformanceRating","RelationshipSatisfaction","StockOptionLevel","WorkLifeBalance","NumCompaniesWorked")
#Run the Kramer correlation function
mat <- catcorrm(vars=catvars,dat)
corrplot(mat, type="upper", tl.cex=0.5, tl.col="black")

#----------Logistic Regression Analysis (may not include) ----------
#After working through initial correlation analysis, logistic models can be used to determine which factors work well TOGETHER
library(aod) #needed for the wald.test function

#logistic regression, stepwise model selection
model.null <- glm(Attrition~1,data=dat, family=binomial(link="logit")) #Create the baseline null model
model.full <- glm(Attrition~.-EmployeeNumber,data=dat, family=binomial(link="logit")) #Create the full model (I don't think this included interactions, haven't figured that out yet)
step(model.null,scope=list(upper=model.full),direction="both",test="Chisq",data=dat) #step model 
#This model results in 20 explantory variables (far too many!). The first ones to be added to the model are OverTime, JobRole, StockOptionLevel, JobLevel, EnvironmentSatisfaction and BusinessTravel.

#First model after running stepwise. The variables included were the top correlated variables from the correlation exploration above.
model <- glm(Attrition~JobRole + OverTime +JobLevel+StockOptionLevel+EnvironmentSatisfaction+BusinessTravel+Age,family=binomial(link='logit'),data=dat)
summary(model) #aic = 1012

#Two ways to test significance of the model: ANOVA and Likelihood Ratio Test
anova(model,model.null,test="Chisq")
library(lmtest)
lrtest(model)
#Both show overall significance in the model

#The model shows that EnvironmentSatisfaction and OverTime are definitely significant. Further testing is needed for the other variables (because they are dummy variables).
#Wald tests on the other variables
wald.test(b=coef(model), Sigma=vcov(model),Terms=2:9) #test on JobRole; small P value, significant
wald.test(b=coef(model), Sigma=vcov(model),Terms=11:14) #test on JobLevel; small P value, significant
wald.test(b=coef(model), Sigma=vcov(model),Terms=15:17) #test on StockOptionLevels; small P value, significant
wald.test(b=coef(model), Sigma=vcov(model),Terms=21:22) #test on BusinessTravel; small P value, significant.

#Plot residuals
plot(fitted(model),rstandard(model))

#Doing Comparisons between different models (looking at BIC)
model.1 <- glm(Attrition~JobRole + OverTime +JobLevel,family=binomial(link='logit'),data=dat)
model.2 <- glm(Attrition~JobRole + OverTime +StockOptionLevel,family=binomial(link='logit'),data=dat)
model.3 <- glm(Attrition~JobRole +JobLevel+StockOptionLevel,family=binomial(link='logit'),data=dat)
model.4 <- glm(Attrition~OverTime +JobLevel+StockOptionLevel,family=binomial(link='logit'),data=dat)
model.5 <- glm(Attrition~OverTime +JobLevel+EnvironmentSatisfaction,family=binomial(link='logit'),data=dat)

library(rcompanion)
compareGLM(model.1,model.2,model.3,model.4, model.5)
#Model 4 returns the lowest metrics (AIC, AICc=1104, BIC)