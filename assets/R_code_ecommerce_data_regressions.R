#----------------------------------------
# Import and set up data
#----------------------------------------
data <-read.csv("./R PROJECTS/data/ecommerce-users")

str(data)
summary(data)

#----------------------------------------
# Creating plots and search form insights
#----------------------------------------

library(ggplot2)

# Is there a correlation between time spent on a website 
# and a yearly amount spent?

ggplot(data, aes(x=Time.on.Website, y=Yearly.Amount.Spent)) +
  geom_point(color="red") +
  ggtitle("Time spent on the website against Yearly amount spent") +
  xlab("Time on website") +
  ylab("Yearly amount spent")

# Correlation between average session length vs Yearly amount spent

ggplot(data, aes(x=Avg..Session.Length, y=Yearly.Amount.Spent)) +
  geom_point(color="red") +
  ggtitle("Average session length against Yearly amount spent") +
  xlab("Average session length") +
  ylab("Yearly amount spent")

# Pair plot of all continuous variables

pairs(data[c("Avg..Session.Length",
             "Time.on.App",
             "Time.on.Website",
             "Length.of.Membership",
             "Yearly.Amount.Spent")],
      col="orange",
      pch=16,
      main="Pairplot of all continuous variables"
      )   

#----------------------------------------
# Exploring the selected variable
#----------------------------------------

# Is the variable normally distributed?

hist(data$Length.of.Membership)
ggplot(data, aes(x=Length.of.Membership)) +
  geom_histogram(
    color="white",
    fill="red",
    binwidth=0.5
  )
 boxplot(data$Length.of.Membership)  
ggplot(data, aes(x=Length.of.Membership)) + 
  geom_boxplot(fill="blue")


#----------------------------------------
# Fitting a linear model
#----------------------------------------

attach(data)
lm.fit1 <-lm(Yearly.Amount.Spent~Length.of.Membership)

summary(lm.fit1)
plot(Yearly.Amount.Spent~Length.of.Membership)
abline(lm.fit1, col="red")

#----------------------------------------
# Residuals analysis
#----------------------------------------

hist(residuals(lm.fit1))

qqnorm(residuals(lm.fit1))

qqline(residuals(lm.fit1), col="red")

shapiro.test(residuals(lm.fit1))

#----------------------------------------
# Evaluation of the model
#----------------------------------------

set.seed(1)
row.number<- sample(1:nrow(data), 0.8*nrow(data))

train <- data[row.number,]
test <- data[-row.number,]

#Estimating the linear fit with the training set

lm.fit0.8 <-lm(Yearly.Amount.Spent~Length.of.Membership, data=train)
summary(lm.fit0.8)

#Predicting in the test data set

prediction0.8 <- predict(lm.fit0.8, newdata=test)

error0.8 <- prediction0.8 - test$Yearly.Amount.Spent

#Root mean square error

rmse <- sqrt(mean(error0.8^2))

# Mean absolute percentage error

mape <- mean(abs(error0.8/test$Yearly.Amount.Spent))

c(RMSE=rmse, mape=mape, R2=summary(lm.fit0.8)$r.squared)

#----------------------------------------
# MULTIPLE REGRESSION
#----------------------------------------

attach(data)

lm.fit <- lm(Yearly.Amount.Spent~Avg..Session.Length + Time.on.App + Time.on.Website + Length.of.Membership)

summary(lm.fit)

#----------------------------------------
# EVALUATION OF THE MULTIPLE REGRESSION
#----------------------------------------

set.seed(1)
row.number<- sample(1:nrow(data), 0.8*nrow(data))

train <- data[row.number,]
test <- data[-row.number,]

#Estimating the linear fit with the training set

multi.lm.fit0.8 <- lm(Yearly.Amount.Spent~Avg..Session.Length + Time.on.App + Time.on.Website + Length.of.Membership,
                data = train)
summary(multi.lm.fit0.8)

#Predicting in the test data set

prediction0.8 <- predict(multi.lm.fit0.8, newdata=test)

error0.8 <- prediction0.8 - test$Yearly.Amount.Spent

#Root mean square error

rmse <- sqrt(mean(error0.8^2))

# Mean absolute percentage error

mape <- mean(abs(error0.8/test$Yearly.Amount.Spent))

c(RMSE=rmse, mape=mape, R2=summary(lm.fit0.8)$r.squared)









