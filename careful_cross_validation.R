## In this update, I perform 10-fold cross-validation on my polynomial model 
# in order to select the correct degree of polynomial. Since this is time-
# series data, cross validation has to be implemented carefully: the basic idea
# is, avoid training on the future to predict the past. To avoid this issue, 
# I implement the method of "rolling through" the data, as described by Rob 
# J. Hyndman here: https://robjhyndman.com/hyndsight/crossvalidation/

library(dplyr)
library(readr)
library(ggplot2)
library(boot)

## Import the result of joining aggregated weather data with energy sales data----
sales_weather <- read_csv("sales_weather.csv")


## Split data into 18 training and testing sets----
train_sets <- list()
test_sets <- list()

for (yr in (1:18)) {
  
  train_sets[[yr]] <- data.frame(filter(sales_weather, year<=(2000+yr))) # Each training set is the data
                                                                         # for years <= yr.
  
  test_sets[[yr]] <- data.frame(filter(sales_weather, year==(2001+yr))) # Each test set is the data
                                                                        # for the year yr+1.
}


## Initialize vectors cv.rmse and cv.se, and a matrix, errs----
# 1) cv.rmse[i] will give, for each polynomial degree i, the root mean squared error for models of 
#    degree i, estimated by cross validation across 18 training sets (folds). 
# 2) cv.se[i] will give the standard error of cv.rmse[i], estimated over the 18 training sets. 
# 3) The matrix errs gives, for each degree i and training set yr, the estimated RMSE of a degree
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
cv.rmse <- rep(0,6)
cv.se <- rep(0,6)
errs <- matrix(NA,6,18)

## For each degree i, cross-validate an ith degree polynomial model, using our 18 training sets
# as the cv folds, calculating & storing estimated test RMSEs for each degree, & their SEs----

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

## Implement the one standard error rule to choose the degree for our our polynomial model----
# We'll identify the degree i with smallest estimated MSE, and the standard error se for 
# estimated cross validation MSE for models of degree i. Then we'll pick the degree whose 
# estimated MSE is within se of i's estimated cross validation MSE. 
cv.rmse
cv.se
# Here the degree with smallest estimated RMSE is 3, with RMSE ~54.88. The standard error for RMSE for 
# models of degree 3 is ~19.04. The smallest degree with MSE within 19.04 of 55 is  2, with RMSE of 55.41. 
# So by the one standard error rule, we should go with a quadratic model. 