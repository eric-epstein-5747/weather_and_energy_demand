## EXPLORE OUR DATA & FIT AND ASSESS A SUITABLE MODEL
# Hypothesis: energy demand is driven most significantly by use of heating and AC, which
# is greatest in the hottest summer months and coldest winter months. 

## Plan: 
# 1. Examine some preliminary visualizations:
#    a) Decide what variables to examine.
#    b) Get a sense of what might be a good kind of model.
# 2. Compare and settle on parameters for model of that type. 
# 3. Double check whether any ignored variables might be important after all.

## 1. Exploratory analysis----
# Import necessary packages & data

library(readr)
library(ggplot2)
library(boot)

# Import the result of joining aggregated weather data with energy sales data: 
sales_weather <- read_csv("sales_weather.csv")

# Create some initial visualizations. 

# Look at how energy demand relates to snowfall, preexisting snow depth, and precipitation:
ggplot(sales_weather, aes(x=prcp, y= all_sectors)) + geom_point(color="blue", alpha=0.4)
ggplot(sales_weather, aes(x=snwd, y= all_sectors)) + geom_point(color="blue", alpha=0.4)
ggplot(sales_weather, aes(x=snow, y= all_sectors)) + geom_point(color="blue", alpha=0.4)
# No obvious patterns here. We can return and reconsider these variables later, but let's 
# move on for now.

# Look at how energy demand relates to temperature. 
# Plot energy demand against the average of min and max temp:
ggplot(sales_weather, aes(x=tmax, y= all_sectors)) + geom_point(color="blue", alpha=0.4)
ggplot(sales_weather, aes(x=tmin, y= all_sectors)) + geom_point(color="blue", alpha=0.4)
# This looks like a polynomial relationship in both cases, so let's plan to consider several polynomial models.


# Decide which temperature variables to examine.
# Min and max temp are likely to be highly correlated, so we should probably only look at one. 
# Let's verify and make a decision. (We could look at some combination of tmin and tmax, but let's keep
# things simple and intelligible.)
cor(cbind(tmax=sales_weather$tmax, tmin=sales_weather$tmin, demand=sales_weather$all_sectors))
# tmin and tmax are *highly* correlated, each more to the other than to demand. But at ~0.59 tmin is more
# closely related to demand than tmax is (at 0.53), so let's look at tmin.



## 2. Identify the best degree for a polynomial model on tmin----
# First use F-tests:
fit.1 <- lm(all_sectors~tmin, data=sales_weather)
fit.2 <- lm(all_sectors~poly(tmin,2), data=sales_weather)
fit.3 <- lm(all_sectors~poly(tmin,3), data=sales_weather)
fit.4 <- lm(all_sectors~poly(tmin,4), data=sales_weather)
fit.5 <- lm(all_sectors~poly(tmin,5), data=sales_weather)
fit.6 <- lm(all_sectors~poly(tmin,6), data=sales_weather)
anova(fit.1,fit.2,fit.3,fit.4,fit.5,fit.6)

# The p-value comparing the linear fit.1 to the quadratic fit.2 is essentially zero 
# (<10^(-15)), indicating that a linear fit is not sufficient. Similarly the p-value comparing 
# the quadratic fit.2 to the cubic fit.3 is significant (~0.007), so the quadratic fit 
# is also insufficient. The 4, 5, and 6 degree  polynomials (models 4, 5, and 6) seem 
# unnecessary because their p-values are ~10%, ~13%, and ~10%, respectively. Hence, a cubic 
# polynomial appears to provide a reasonable fit to the data, but lower- or higher-order
# models are not justified.


# Now compare results from cross-validation:
# We will use the one-standard-error rule: We'll find the degree i with the smallest 
# estimated RMSE, R, (estimated via cross validation), and the standard error E for R.
# Then find the smallest degree m such that models of degree m have estimated RMSE 
# within E of R. 

# NOTE: Since this is time-series data, cross validation (CV) has to be implemented carefully: 
# the basic idea is, avoid training on the future to predict the past. Also, CV assumes that
# the data points are independent (since the data is partitioned randomly). But this tends not
# to be true of time-series data. Accordingly, I implement the method of "rolling through" the 
# data, as described by Rob J. Hyndman here: https://robjhyndman.com/hyndsight/crossvalidation/
# The goal is to respect the data's temporal structure. 

library(dplyr)

## Split data into 18 training and testing sets
train_sets <- list()
test_sets <- list()

for (yr in (1:18)) {
  
  train_sets[[yr]] <- data.frame(filter(sales_weather, year<=(2000+yr))) # Each training set is the data
                                                                         # for years <= yr.
  
  test_sets[[yr]] <- data.frame(filter(sales_weather, year==(2001+yr))) # Each test set is the data
                                                                        # for the year yr+1.
}

## Initialize vectors cv.rmse and cv.se, and a matrix, errs:
# a) cv.rmse[i] will give, for each polynomial degree i, the root mean squared error for models of 
#    degree i, estimated by cross validation across 18 training sets (folds). 
# b) cv.se[i] will give the standard error of cv.rmse[i], estimated over the 18 training sets. 
# c) The matrix errs gives, for each degree i and training set yr, the estimated RMSE of a degree
#    i model trained on yr. 
#
# NOTE 1: cv.rmse[i] will be calculated as the mean of errs[i,], and cv.se[i] will be the standard 
#         deviation of errs[i,]. With these vectors in hand, we will use the one standard error rule to 
#         choose the degree of our polynomial. 
#
# NOTE 2: If we wanted to ensure that the process of choosing a degree for the polynomial is less
#         vulnerable to outliers, we could instead look at average absolute value of error rather 
#         than RMSE. I haven't explored that yet in this case, but I suspect that the results would be 
#         very similar. 
cv.rmse <- rep(NA,6)
cv.se <- rep(NA,6)
errs <- matrix(NA,6,18)

## For each degree i, cross-validate an ith degree polynomial model, using our 18 training sets
# as the cv folds, calculating & storing estimated test RMSEs for each degree, & their SEs

for (i in (1:6)) {
  
  # For each training set yr, fit a degree i model on yr, which comprises all years up to and including
  # the year represented by yr. Test the model on the designated test set and store the test RMSE.
  #
  # NOTE: Normally, for such a small dataset (small n) it would be prudent to repeat this process and 
  #       average the results, to get a more accurate estimate of test RMSE. However, since we are 
  #       using the "rolling" cross validation method, the training sets are not chosen randomly. Hence 
  #       repeating the process would be pointless; we'd get the same results every time, since we'd
  #       be training on exactly the same 18 training sets in each trial. 
  for (yr in (1:18)) {
    
    # Fit an i degree polynomial model on yr. 
    glm.fit <- glm(all_sectors~poly(tmin,i),data=train_sets[[yr]])
    
    # Calculate & store the RMSE using the test dataset comprising yr + 1. 
    errs[i,yr] <- sqrt(mean((predict(glm.fit, test_sets[[yr]]) - test_sets[[yr]]$all_sectors)^2))
  }
  
  # The cv estimated RMSE for each polynomial degree is the average RMSE of models of that degree over
  # all test sets. 
  cv.rmse[i] <- mean(errs[i,])
  
  # For each degree i, the standard error of the cv estimated RMSE for models of degree i is the standard 
  # deviation of estimated RMSEs for models of that degree, across all trials. (Here we assume that these
  # estimated RMSEs follow a normal distribution. But even if not, our answer won't be far off.) 
  cv.se[i] <- sd(errs[i,])
}

## Implement the one standard error rule to choose the degree for our our polynomial model
# We'll identify the degree i with smallest estimated MSE, and the standard error se for 
# estimated cross validation MSE for models of degree i. Then we'll pick the degree whose 
# estimated MSE is within se of i's estimated cross validation MSE. 
cv.rmse
cv.se
# Here the degree with smallest estimated RMSE is 3, with RMSE ~54.88. The standard error for RMSE for 
# models of degree 3 is ~19.04. The smallest degree with MSE within 19.04 of 55 is  2, with RMSE of 55.41. 
# So by the one standard error rule, we should go with a quadratic model. 

# Plot a quadratic model against our data points and see if it looks reasonable to the naked eye:
glm.fit <- glm(all_sectors~poly(tmin,2),data=sales_weather)
ggplot(data=sales_weather, aes(x=tmin, y=all_sectors)) + geom_point(color="blue",alpha=0.4) +
  geom_line(aes(x=tmin,y=predict(glm.fit)),color="red")
# This plot looks intuitively like a good fit for the data. 
plot(glm.fit)
# The residual plot looks very good.

# QUESTION: Why privilege the one standard error rule over the anova() results?
# ANSWER: The models tested in anova() were fit on the entire dataset, so they
# are more vulnerable to overfitting and favoring a more flexible model as better. 

## 3. Double check to make sure we didn't ignore any important variables other than temperature----
glm.more <- glm(all_sectors~poly(tmin,3)+prcp+snow+snwd, data=sales_weather)
summary(glm.more)
# The p values for precipitation, snow, and snow depth are all greater than 0.47, so we can be
# confident that none of these variables is very important. 