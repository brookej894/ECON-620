
#1
# See end of document for attached work. 

#2
df_401k <- read.csv("../ECON 620 Econometrics/401k.csv", stringsAsFactors = TRUE)

#2a.
mean(df_401k$mrate)
mean(df_401k$prate)

#2b.
regression1 <- lm(df_401k$prate ~ df_401k$mrate)
summary(regression1)

# coefficient: 5.861, intercept: 83.0755, sample size: 1533 (degrees of freedom + 1), R^2: 0.0747

#2c.
# When the match rate (or generosity of the plan) is zero, 83% of employees are predicted to participate in having an active account.
# An increase in the match rate of $1 is associated with a 5.86% increase in the plan participation rate. 

#2d.
prate <- 83.0755 + 5.861*3.5
prate
# The predicted precent of enrolled workers is 104%, this prediction therefore does not make sense since it estimates more than the available number of works. 
# Since the regression model is a line that has not been given parameters of the independent variable for which to estimate, it will provide estimations beyond reasonable interpretation (i.e. predict an estimate over 100%)

#2e.
# The R^2 is: 
summary(lm(df_401k$prate ~ df_401k$mrate))$r.squared
# .07 or 7% of the variation in particpation rate is explained by the match rate. This is not a great model because it only explains a very low amount of variation in our variable of interest. 

#3
df_sleep <- read.csv("../ECON 620 Econometrics/sleep75A.csv", stringsAsFactors = TRUE)

#3a.
regression2 <- lm(df_sleep$sleep ~ df_sleep$totwrk)
summary(regression2)

#3b.
# coefficient: -0.15075, intercept:3586.37695, sample size: 705, R^2: 0.1033
# The intercept tells us that when total time spent in paid work is zero the expected amount of sleep measured in minutes is 3,586 minutes (or 58 hours)
# In this instance, the intercept does not tell us something helpful, since its impossible to spend 58 hours a night sleeping. 
# This is a limitation in interpretting the model. 

#3c.
change_in_sleep <- 120*(-0.15075)
change_in_sleep
# If total amount of work increasesd by 2 hours we would predict a decrease of 18.09 minutes in amount of time sleeping. 
# This is not a very large affect, trading 18.09 sleeping minutes for 120 working minutes. 

#3d. 
sse <- sum((df_sleep$sleep - fitted(regression2))^2)
ssr <- sum((mean(df_sleep$sleep) - fitted(regression2))^2)
sst <- ssr+sse

r_squared <- (ssr/sst)
r_squared

ser <- sqrt(sse/704)
ser

#3e. 

#4
regression3 <- lm(df_sleep$sleep ~ df_sleep$totwrk + df_sleep$Educ + df_sleep$age)
summary(regression3)

#4a. 
# When adults trade off sleep for work the coefficient on beta 1 is negative, meaning that if they sleep less they will work more, or work less and sleep more. 
# The relationship between the outcome variable and x1 is negative. 

#4b. 
# One could reasonably assume that the relationship between education and sleep is also negative, such that a decrease in time spent in education should result in an increase in sleep, and vice versa.
# The sign on age could difficult to guess, one hand young people require more hours sleeping to maintain health brain activity but often actually acheive less hours of sleep due to life style choices and environmental factors. 
# I would use the background assumption that people require more sleep at a younger age to estimate that the relationship between age and sleep will be negative such that as one ages the amount of sleep they acheive decreases. 
# Therefore the coefficients on age and education (beta 2 & beta 3) should be negative. 

#4c. 
# If someone works 5 hours more (or 300 minutes), we would predict that their amount of sleep would fall by ...
change_in_sleep_2 <- 300*(-.14779)
change_in_sleep_2
# ... 44.337 mintutes or .739 hours. This is not too large of a tradeoff, 300 working minutes for 44 sleeping minutes. 

#4d. 
# The negative sign on the coefficient for education makes sense because of the expanation given in 4b - that is time spent in education is less time sleeping. 
# The magnitude is much larger than hours working through, suggesting there is a bigger cost of sleep for education than work. 1 hour decrease in the amount of time in education will result in a 658.2 (10.79*60) minute increase in amount of time sleeping. 

#4e. 
# The R-squared in this model = 0.1138 or 11.38%, meaning that 11% of the variation in sleep is explained by observing these three variables in our model. 
# this result has a lot of room for improvement. 11% explained variation is not much. There is likely still variation in hours spent sleeping being accumulated in the error term, meaning that we are missing significant variables that predict amount of time sleeping. 
# Examples of possible missing variables include whether the individual has a  young kid, whether they are married, time spent doing leisure activities, etc. 
# These variables could be correlated with total working hours. For example, if an individual has a young child they may have to spend less time working into order to care for their kid like leaving work early to pick them up from school. 
# Any variable that infringes on the amount of hours an individual has available to sleep will also risk coinciding with an affect on work since total available hours could be allocated to either activity. 

#5
# see end of document for attached work. 

#6 

