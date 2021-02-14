#project2
#11-29-20

#Variables : BMI, Life expectancy, Adult morality, Status,.Alcohol
#y : Life expectancy
#x: BMI, Status, Alcohol, GDP, schooling



setwd("C:/Users/Arturo/Documents/R/rstudio/project4")

Life_Expectancy <- read.csv(file.path("LifeExpectancyData.csv"), sep = ",")
str(Life_Expectancy)
Life_Expectancy <- as.data.frame(Life_Expectancy)
Fixed_Expectancy <- data.frame(GDP = Life_Expectancy$GDP,status = Life_Expectancy$Status,Alcohol.Mortality = Life_Expectancy$Alcohol,BMI = Life_Expectancy$BMI, Life.Expectancy = Life_Expectancy$Life.expectancy, Education = Life_Expectancy$Schooling)
Life_Expectancy$Status <- as.factor(Life_Expectancy$Status)
Life_Expectancy[is.na(Life_Expectancy)] <- 0
#Life_Expectancy$GDP2 <- which(Life_Expectancy$GDP != NULL)
GDP_dataframe <- subset(Life_Expectancy, Life_Expectancy$GDP > 0)
Fixed_Expectancy$GDP[is.na(Fixed_Expectancy$GDP)] <-  mean(GDP_dataframe$GDP)
Fixed_Expectancy[is.na(Fixed_Expectancy)] <- 0

par(mfrow = c(2,2))


#BMI
#H0: B1 = 0
#H1: B1 != 0
BMIline <- lm(Fixed_Expectancy$Life.Expectancy ~ Fixed_Expectancy$BMI, data = Fixed_Expectancy)
BMIline
summary(BMIline)
plot(Fixed_Expectancy$BMI, Fixed_Expectancy$Life.Expectancy)
abline(BMIline, col = 'red')
t.test(Fixed_Expectancy$BMI, Fixed_Expectancy$Life.Expectancy, alternative = "two.sided")
#t = -73.911, df = 4356.3, p-value < 2.2e-16



plot(lm(Fixed_Expectancy$Life.Expectancy ~ Fixed_Expectancy$BMI))
plot(Fixed_Expectancy$Life.Expectancy~Fixed_Expectancy$BMI, xlab = "BMI")
cor(Fixed_Expectancy$Life.Expectancy, Fixed_Expectancy$BMI)
cor.test(Fixed_Expectancy$Life.Expectancy, Fixed_Expectancy$BMI)

regline1 <- lm(Fixed_Expectancy$Life.Expectancy~Fixed_Expectancy$BMI, data = Fixed_Expectancy)
summary(regline1)
abline(regline1, col = "red")

resid(regline1)

plot(Fixed_Expectancy$BMI, resid(regline1))
abline(h=0)


#Status
#H0: B2 = 0
#H1: B2 != 0
statusline <- lm(Fixed_Expectancy$Life.Expectancy ~ Fixed_Expectancy$status, data = Fixed_Expectancy)
summary(statusline)
plot(statusline)
anova(lm(Fixed_Expectancy$Life.Expectancy~Fixed_Expectancy$status, data = Fixed_Expectancy))
#status    1  64619   64619  763.06 < 2.2e-16 ***

status <- as.factor(Fixed_Expectancy$status)
plot(status, Fixed_Expectancy$Life.Expectancy)



#Alcohol
#H0: B3 = 0
#H1: B4 != 0

alcline <- lm(Fixed_Expectancy$Life.Expectancy ~ Fixed_Expectancy$Alcohol.Mortality, data = Fixed_Expectancy)
summary(alcline)
plot(alcline)
t.test(Fixed_Expectancy$Alcohol.Mortality, Fixed_Expectancy$Life.Expectancy, alternative = "two.sided")
#t = -315.78, df = 3831.9, p-value < 2.2e-16

plot(Fixed_Expectancy$Alcohol.Mortality, Fixed_Expectancy$Life.Expectancy)
abline(alcline, col = 'red')



#GDP
#H0: B4 = 0
#H1: B4 != 0

Gdpline <- lm(Fixed_Expectancy$Life.Expectancy ~ Fixed_Expectancy$GDP, data = Fixed_Expectancy)
summary(Gdpline)
plot(Gdpline)
#t = 30.591, df = 2937, p-value < 2.2e-16
t.test(Fixed_Expectancy$GDP, Fixed_Expectancy$Life.Expectancy, alternative = "two.sided")

plot(Fixed_Expectancy$GDP, Fixed_Expectancy$Life.Expectancy)
abline(Gdpline, col = 'red')


#Since the P value is t0o low and lower than .05, we reject the null hypothesis meaning there is strong evidence
# on having correlation between GDP and life expectancy
#F-statistic: 562.9 on 1 and 2936 DF,  p-value: < 2.2e-16


#Schooling
#H0: B4 = 0
#H1: B4 != 0

schoolingline <- lm(Fixed_Expectancy$Life.Expectancy ~ Fixed_Expectancy$Education, data = Fixed_Expectancy)
summary(schoolingline)
plot(schoolingline)
t.test(Fixed_Expectancy$Education, Fixed_Expectancy$Life.Expectancy, alternative = "two.sided")
#t = -279.72, df = 3910.8, p-value < 2.2e-16

plot(Fixed_Expectancy$Education, Fixed_Expectancy$Life.Expectancy)
abline(schoolingline, col = 'red')


#2
par(mfrow = c(2,2))
GDP_Edu <- lm(Fixed_Expectancy$Education ~ Fixed_Expectancy$GDP)
plot(Fixed_Expectancy$GDP, Fixed_Expectancy$Education)
abline(GDP_Edu, col = 'red')
plot(residuals(GDP_Edu))
t.test(Fixed_Expectancy$GDP, alternative = "two.sided")

plot(GDP_Edu)
educor<- cor(Fixed_Expectancy$GDP, Fixed_Expectancy$Education)
cor.test(Fixed_Expectancy$GDP, Fixed_Expectancy$Education)

regline1 <- lm(Fixed_Expectancy$GDP~Fixed_Expectancy$Education, data = Fixed_Expectancy)
regline1 <- lm(Fixed_Expectancy$GDP~Fixed_Expectancy$Education, data = Fixed_Expectancy)

summary(regline1)
abline(regline1, col = "red")

resid(regline1)

plot(Fixed_Expectancy$GDP, resid(regline1))
abline(h=0)


#Anova
#H_0: Mean_developed = mean_developing
# H0: B1 = B2 = B3 = B4 = B5 = 0
# H1: at least one of those does not equal 


#3 
developed_dataframe <- subset(Fixed_Expectancy, Fixed_Expectancy$status == 'Developed')
developing_dataframe <- subset(Fixed_Expectancy, Fixed_Expectancy$status == 'Developing')

multipleregression_developing <- lm(developing_dataframe$Life.Expectancy ~ developing_dataframe$BMI+developing_dataframe$GDP+developing_dataframe$Alcohol.Mortality+developing_dataframe$Education, data = developing_dataframe)
multipleregression_developed <- lm(developed_dataframe$Life.Expectancy ~ developed_dataframe$BMI+developed_dataframe$GDP+developed_dataframe$Alcohol.Mortality+developed_dataframe$Education, data = developed_dataframe)
anova(multipleregression_developing, multipleregression_developed)

statusaov <- aov(Fixed_Expectancy$Life.Expectancy~Fixed_Expectancy$status, data = Fixed_Expectancy)
summary(statusaov)
# or
anova(lm(Fixed_Expectancy$Life.Expectancy~Fixed_Expectancy$status, data = Fixed_Expectancy))

statustukey <- TukeyHSD(x = statusaov, conf.level = .95)
plot(statustukey)




