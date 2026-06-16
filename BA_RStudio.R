setwd("D:/BA R_STUDIO WORK")
getwd()

EduStates_New <-read.csv("EduStates_New.csv")
print(EduStates_New)

#TASK 03

#minimum of state spending on public education per student
min(EduStates_New$dollars)

#maximum of state spending on public education per student
max(EduStates_New$dollars)

#mean value of state spending on public education per student
mean(EduStates_New$dollars)


#median value of state spending on public education per student
median(EduStates_New$dollars)

#the 1st most frequent mode value of state spending on public education per student
mode= function(){
  #calculate mode of dollar column i table EduStates_New
  return(names(sort(-table(EduStates_New$dollars)))[1])
}
mode()

#TASK 04

#summary statistics of state spending on public education per student
summary(EduStates_New$dollars)

# Create a bar plot for minimum and maximum values
min_value <- min(EduStates_New$dollars)
max_value <- max(EduStates_New$dollars)

values <- c(min_value, max_value)
labels <- c("Minimum", "Maximum")

#plot the bar chart
barplot( values, names.arg=labels,
         xlab="Values",
         ylab="Dollars per Student",col='darkblue',
         main="Minimum and Maximum State Spending on Education",border="white")

#TASK 05

#Bell curve for Percentage of graduating high-school students in the state who took the SAT exam
hist(EduStates_New$percent, probability = TRUE, col = "red", 
     xlab = "Percentage of students", ylab = "Frequency", 
     main = "Percentage of high school graduates in the state who took the SAT.")

curve(dnorm(x,mean = mean(EduStates_New$percent),sd = sd(EduStates_New$percent)), add = TRUE)

#standard deviation for Percentage of graduating high-school students in the state who took the SAT exam
sd(EduStates_New$percent)

#H0 : Data set is normally distributed
#H1 : Data set is not normally distributed
#Perform the Shapiro-wilk test for EduStates_New$percent
shapiro.test(EduStates_New$percent)


#Bell curve for state spending on public education per student
hist(EduStates_New$dollars, probability = TRUE, col = "lightpink", 
     xlab = "Spending", ylab = "Frequency", 
     main = "State spending per student on public education.")

curve(dnorm(x,mean = mean(EduStates_New$dollars),sd = sd(EduStates_New$dollars)), add = TRUE)

#standard deviation of state spending on public education per student
sd(EduStates_New$dollars)

#H0 : Data set is normally distributed
#H1 : Data set is not normally distributed
#Perform the Shapiro-wilk test for EduStates_New$dollars
shapiro.test(EduStates_New$dollars)


#Bell curve for average teacher's salary in the state
hist(EduStates_New$pay, probability = TRUE, col = "lightblue", 
     xlab = "Average Teacher Salary", ylab = "Frequency", 
     main = "Average teacher's salary in the state")

curve(dnorm(x,mean = mean(EduStates_New$pay),sd = sd(EduStates_New$pay)), add = TRUE)


#standard deviation of average teacher's salary in the state
sd(EduStates_New$pay)

#H0 : Data set is normally distributed
#H1 : Data set is not normally distributed
#Perform the Shapiro-wilk test for EduStates_New$pay
shapiro.test(EduStates_New$pay)


#Bell curve for population of states
hist(EduStates_New$pop, probability = TRUE, col = "purple", 
     xlab = "Population of each state", ylab = "Frequency", 
     main = "Population of different states of the United States")

curve(dnorm(x,mean = mean(EduStates_New$pop),sd = sd(EduStates_New$pop)), add = TRUE)

#standard deviation of population of states
sd(EduStates_New$pop)

#H0 : Data set is normally distributed
#H1 : Data set is not normally distributed
#Perform the Shapiro-wilk test for EduStates_New$pop
shapiro.test(EduStates_New$pop)

#TASK 06
# Vertical boxplot dor State Spending on Public Education per Student by Region
boxplot(dollars ~ region, data = EduStates_New,
        main = "State Spending on Public Education per Student by Region",
        xlab = "Region",
        ylab = "Spending per Student ($)",
        col = "lightblue",
        border = "darkblue",
        las = 2)

#one-way ANOVA for the task
anova <- aov(dollars ~ region, data = EduStates_New)
summary(anova)

#TASK 07
#correlation of Percentage of graduating high-school students in the state who took the SAT exam and state spending on public education per student
cor.test(EduStates_New$percent,EduStates_New$dollars)

dollars = EduStates_New$dollars
percent = EduStates_New$percent

#the scatter plot for Percentage of graduating high-school students in the state
#who took the SAT exam and state spending on public education per student

plot(dollars, percent,col="navyblue", main="Scatter plot",
     xlab="State Spending on Public Education per Student ($)",
     ylab="Percentage of Students Taking SAT",cex=1.3, pch=16)

#the regression line for the scatter plot
abline(lm( percent~dollars, data = data), col="red")

#TASK 08
#correlation of Percentage of graduating high-school students in the state who 
#took the SAT exam and average teacher's salary in the state
cor.test(EduStates_New$percent,EduStates_New$pay)

percent = EduStates_New$percent
pay = EduStates_New$pay

#the scatter plot for Percentage of graduating high-school students in the state who 
#took the SAT exam and average teacher's salary in the state

plot(pay,percent,col="darkgreen", main="Scatter plot",
     xlab="Average teacher's salary in the state",
     ylab="Percentage of Students Taking SAT",cex=1.3, pch=16)

#the regression line for the scatter plot
abline(lm( percent~pay, data = data), col="red")

#TASK 09
#correlation of Percentage of graduating high-school students in the state who 
#took the SAT exam and population
cor.test(EduStates_New$pop,EduStates_New$percent)

percent = EduStates_New$percent
pop = EduStates_New$pop

#the scatter plot for Percentage of graduating high-school students in the state who 
#took the SAT exam and population

plot(pop,percent,col="purple", main="Scatter plot",
     xlab="Population of states",
     ylab="Percentage of Students Taking SAT",cex=1.3, pch=16)

#the regression line for the scatter plot
abline(lm( percent~pop, data = data), col="red")

#TASK 10
#applying lm() function for percent vs dollars
relation <- lm(dollars~percent)
print(relation)
summary(relation)
#linear regression equation for percent vs dollars
#y = a + bx
#Dependent variable = Coefficients (Intercept, Slope) + Coefficients (Intercept, Slope) x Independent Variable
#Spending per Student=6.80368+0.040652×Percent Taking SAT


#prediction of the values of the dependent variable based on the linear regression model
relation <- lm(dollars ~ percent)
result <- predict(relation)
print(result)


#applying lm() function for pay vs percent
relation <- lm(pay~percent)
print(relation)
summary(relation)
#linear regression equation for pay vs dollars
#y = a + bx
#Dependent variable = Coefficients (Intercept, Slope) + Coefficients (Intercept, Slope) x Independent Variable
#Average Salary=26.008+0.1462×Percent Taking SAT

#prediction of the values of the dependent variable based on the linear regression model
relation <- lm(pay ~ percent)
result <- predict(relation)
print(result)

#applying lm() function for pop vs percent
relation <- lm(pop~percent)
print(relation)
summary(relation)
#linear regression equation for pop vs dollars
#y = a + bx
#Dependent variable = Coefficients (Intercept, Slope) + Coefficients (Intercept, Slope) x Independent Variable
#Population=3275.02+47.46×Percent Taking SAT

#prediction of the values of the dependent variable based on the linear regression model
relation <- lm(pop ~ percent)
result <- predict(relation)
print(result)
