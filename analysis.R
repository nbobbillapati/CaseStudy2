#Reading in data and review high level summary
dat <- read.xlsx("CaseStudy2-data.xlsx", sheetName="HR-employee-attrition Data")
head(dat)
summary(dat)

#Remove the columns that do not give any useful information.
dat <- subset(dat, select = -c(Over18,EmployeeCount,StandardHours)) 

#Exploration via boxplots (only left in the ones that had significant differences - others are noted in the Notes file)
boxplot(Age~Attrition, data=dat)#Looks like the mean age of those the did leave companies is lower than those who didn't.
boxplot(DailyRate~Attrition, data=dat) #The spread of these is about the same, but the average and interquartile spread are lower for thos who left (meaning lower daily rates may indicate higher attrition)
boxplot(DistanceFromHome~Attrition, data=dat)#The spread of these is about the same, but the average and interquartile spread are higher for those who left (meaning longer commutes may indicate higher attrition)
boxplot(NumCompaniesWorked~Attrition, data=dat) #Looks like maybe people are more likely to leave if they've only worked at a small number of companies
boxplot(TotalWorkingYears~Attrition, data=dat) #People seem more likely to leave if they have been working for fewer years
boxplot(TrainingTimesLastYear~Attrition, data=dat) #Looks like attrition might be more likely when there's less training time in the previous year
boxplot(YearsAtCompany~Attrition, data=dat) #Really skewed, looks like fewer years at the company might be an indicator for attrition
boxplot(YearsInCurrentRole~Attrition,data=dat) #Fewer years in role, more likely to leave 
boxplot(YearsWithCurrManager~Attrition,data=dat) #Fewer years with current manager, more likely to leave

#Histograms showing possible needs for transformation:
ggplot(dat, aes(x=DistanceFromHome))+geom_histogram() 
ggplot(dat, aes(x=MonthlyIncome))+geom_histogram()
ggplot(dat, aes(x=NumCompaniesWorked))+geom_histogram()
ggplot(dat, aes(x=TotalWorkingYears))+geom_histogram()
ggplot(dat, aes(x=YearsAtCompany))+geom_histogram()
ggplot(dat, aes(x=YearsSinceLastPromotion))+geom_histogram()




#looking at correlation between potential confounders (does not look at impact on Attrition)

#first create a subset of just the continuous variables
cont <- subset(dat, select=c(Age,DailyRate,DistanceFromHome,HourlyRate,MonthlyIncome,MonthlyRate,NumCompaniesWorked,PercentSalaryHike,TotalWorkingYears,TrainingTimesLastYear,YearsAtCompany,YearsInCurrentRole,YearsSinceLastPromotion,YearsWithCurrManager))
install.packages("Hmisc")
library(Hmisc)
#Run correlation matrix
res <- rcorr(as.matrix(cont))
#Plot the correlations to see which ones are highly important
corrplot(res$r,type="upper",p.mat=res$P, sig.level=0.1, insig="blank", tl.cex=0.5)





#Diving in to testing the continous variables impact on Attrition

#Run an ANOVA to test if continuous variable impacts categorical outcome
AgeTest <- aov(Age~Attrition,data=dat)
summary(AgeTest)

#Formula to get a correlation between the categorical "Attrition" and a continuous variable 
install.packages("ICC")
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