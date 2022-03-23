# Prep for Econometrics R Midterm Spring 2022

# load dataset
df_401k <- read.csv("../ECON 620 Econometrics/401k.csv", stringsAsFactors = TRUE)
wage_data <- read.csv("../ECON 620 Econometrics/wage1_S2022.csv", stringsAsFactors = TRUE)
df_sleep <- read.csv("../ECON 620 Econometrics/sleep75A.csv", stringsAsFactors = TRUE)

# find the average of a variable
mean(df_401k$mrate)

# find the min and max values of a variable
min(df_401k$mrate)
max(df_401k$mrate)

# adjust wage for inflation (1976 dollars) using CPI
# CPI for 1976 = 56.9
# CPI for 2018 = 251.1
avg_hrly_wage_2018 = mean(wage_data$ï..wage) * (251.1/56.9)
avg_hrly_wage_2018

# find value counts of binary variable
sum(wage_data$female, na.rm=TRUE)
sum(!wage_data$female, na.rm=TRUE)

# create a student data df
Student <- c(1, 2, 3, 4, 5, 6, 7, 8)
GPA <- c(2.8, 3.4, 3.0, 3.5, 3.6, 3.0, 2.7, 3.7)
ACT <- c(21, 24, 26, 27, 29, 25, 25, 30)
student_data <- data.frame(Student, GPA, ACT)

# estimate GPA using ACT with OLS, print the slope & intercept
regression1 <- lm(GPA ~ ACT, data = student_data)
regression1

# plot the relationship including the line of best fit
plot(student_data$ACT, student_data$GPA)
abline(lm(GPA ~ ACT, data = student_data))

# how much higher is GPA predicted to be if ACT rises by 5 points
change <- .1022*5
change

# compute the fitted values and residuals for each observation
fit1 <- fitted(regression1)
fit1
resid(regression1)

# prove that the residuals sum approximately to zero
sum(resid(regression1))

# What is the predicted value of GPA when ACT=20?
answer2 <- 0.5681+0.1022*20
answer2

# print regression summary
regression1 <- lm(df_401k$prate ~ df_401k$mrate)
summary(regression1)

# find R^2
summary(lm(df_401k$prate ~ df_401k$mrate))$r.squared

# run a new regression 
regression2 <- lm(df_sleep$sleep ~ df_sleep$totwrk)
summary(regression2)

# if totwrk increases by 2 hours, how much will sleep fall?
change_in_sleep <- 120*(-0.15075)
change_in_sleep

# calculate R^2, SER, RSS, SST, SSE
ssr <- sum((df_sleep$sleep - fitted(regression2))^2)
sse <- sum((mean(df_sleep$sleep) - fitted(regression2))^2)
sst <- ssr+sse

r_squared <- (sse/sst)
r_squared

ser <- sqrt(ssr/704)
ser

