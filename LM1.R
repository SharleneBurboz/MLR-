data <- read.csv("Concrete_Data1.csv")
View(data)
library(tidyr)
dataset<- drop_na(data)
str(dataset) #All variables are of numeric type except Age which is an integer
summary(dataset) #Used to find Minimum, 1st Quartile, Median, and Quartile and Maximum
cor(dataset) #Multicollinearity is absent

#Fitting the model
#Fitting model by using variables having positive correlation only
model1<-lm(Concrete.compressive.strength~Cement+Blast.Furnace.Slag+Superplasticizer+Age.in.days,data=dataset)
summary(model1)
#The value of Adjusted R squared is 54%

#Fitting model by taking 4 positive correlation values and 2 negative correlation values namely Fine Aggregate and Water
model2 <- lm(Concrete.compressive.strength~Cement+Blast.Furnace.Slag+Superplasticizer+Age.in.days+Fine.Aggregate+Water,data=dataset)
summary(model2)
#The value of Adjusted R square is 58%

#Fitting the model by including all independent variables
model3<-lm(Concrete.compressive.strength~Cement+Blast.Furnace.Slag+Fly.Ash+Water+Superplasticizer+Coarse.Aggregate+Fine.Aggregate+Age.in.days,data=dataset)
summary(model3)
#The value of Adjusted R squared is 61%. so 61% error in the dataset can be explained by our model p-value is less than 0.05 so data is significant.
#Here we can observe that model3 has a greater value of Adjusted R squared as compared with above models.Hence it is a better fit than 2 other models.

#Graphical Interpretation
plot(model3)
#From Residual VS Fitted plot we can observe an almost straight horizontal line with equally spaced residuals.
#Hence it is safe to assume that linearity assumption is satisfied
#From Normal QQ Plot we can observe that most observations lie on line. Hence normality assumption is satisfied
#From Scale Location plot we can observe an almost straight horizontal line. However the residuals are not equally spaced.
#Hence homoscedasticity assumption is not satisfied  (Log transformation or square root method can be used to remove heteroscedasticity)
#From Residual VS Leverage plot we can observe 2 observations 611 and 225 lie beyond Cooke's distance and exceed -3 standard deviation

#Test for autocorrelation
library(car)
durbinWatsonTest(model3)
#We get p value zero indicating that autocorelation is present 3

#Testing for normality
hist(model3$residuals, main= "Histogram")
library(olsrr)
ols_test_normality(model3)
# Since we have 1030 observations, we will use Kolmogorov smirnov test.
#Since p value is less than 0.05, we reject Ho. Our data is normal

#Checking for heteroscedasticity
library(lmtest)
lmtest::bptest(model3)
#Since p value is less than 0.05, it indicates that residuals are not homoscedastic
plot(fitted(model3), resid(model3), xlab='Fitted Values', ylab='Residuals')

 
#Using weighted least squares to eliminate heteroscedasticity
wt <- 1 / lm(abs(model3$residuals) ~ model3$fitted.values)$fitted.values^2
#Performing weighted least squares regression
model4<-lm(Concrete.compressive.strength~Cement+Blast.Furnace.Slag+Fly.Ash+Water+Superplasticizer+Coarse.Aggregate+Fine.Aggregate+Age.in.days,data=dataset,weights=wt)
summary(model4)
plot(model4)
# After using weighted regression we can see that the overall R square increaed from 61% to 65% 
# Also from plot it is clear that homoscedasticity is present
library(lmtest)
lmtest::bptest(model4)
#Since p value is greater than 0.05, it indicates that residuals are homoscedastic

anova(model3)