## EXPLORE OUR DATA & FIT AND ASSESS A SUITABLE MODEL
# Hypothesis: energy demand is driven most significantly by use of heating and AC, which
# is greatest in the hottest summer months and coldest winter months. 

## Plan: 
# 1. Examine some preliminary visualizations, get a sense of what might be a good kind of model.
# 2. Fit a model of that type. 
# 3. Compare and settle on parameters for model. 
# 4. Double check whether any ignored variables might actually be important.


# Import necessary packages & data----

library(readr)
library(ggplot2)
library(boot)

# Import the result of joining aggregated weather data with energy sales data: 
sales_weather <- read_csv("sales_weather.csv")



## Exploratory data analysis----
# Look at how energy demand relates to temperature
# Plot energy demand against the average of min and max temp:
ggplot(sales_weather, aes(x=tmax, y= all_sectors)) + geom_point(color="blue", alpha=0.4)
ggplot(sales_weather, aes(x=tmin, y= all_sectors)) + geom_point(color="blue", alpha=0.4)
# This looks like a polynomial relationship in both cases, so let's plan to consider several polynomial models.


## Decide which temperature variables to examine----
# Min and max temp are likely to be highly correlated, so we should probably only look at one. 
# Let's verify and make a decision. (We could look at some combination of tmin and tmax, but let's keep
# things simple and intelligible.)
cor(cbind(tmax=sales_weather$tmax, tmin=sales_weather$tmin, demand=sales_weather$all_sectors))
# tmin and tmax are *highly* correlated, each more to the other than to demand. But at ~0.59 tmin is more
# closely related to demand than tmax is (at 0.53), so let's look at tmin.



## Identify the best degree for a polynomial model----
# First use analysis of variance:
fit.1 <- lm(all_sectors~tmax, data=sales_weather)
fit.2 <- lm(all_sectors~poly(tmin,2), data=sales_weather)
fit.3 <- lm(all_sectors~poly(tmin,3), data=sales_weather)
fit.4 <- lm(all_sectors~poly(tmin,4), data=sales_weather)
fit.5 <- lm(all_sectors~poly(tmin,5), data=sales_weather)
fit.6 <- lm(all_sectors~poly(tmin,6), data=sales_weather)
anova(fit.1,fit.2,fit.3,fit.4,fit.5,fit.6)

# The p-value comparing the linear Model 1 to the quadratic Model 2 is essentially zero 
# (<10^(-15)), indicating that a linear fit is not sufficient. Similarly the p-value comparing 
# the quadratic Model 2 to the cubic Model 3 is significant (~0.007), so the quadratic fit 
# is also insufficient. The 4, 5, and 6 degree  polynomials (Models 4, 5, and 6) seem 
# unnecessary because their p-values are ~10%, ~12%, and ~10%, respectively. Hence, a cubic 
# polynomial appears to provide a reasonable fit to the data, but lower- or higher-order
# models are not justified.


# Now compare results from cross-validation:
# We will use the one-standard-error rule: We'll find the degree q with the smallest 
# estimated prediction error CV(q) (estimated via cross validation), and the standard error E 
# for CV(q). Then find the smallest degree m such that models of degree m have CV(m) within E
# of CV(q).


# Initialize the vectors we're going to examine
set.seed(17)
cv.error.6 <- rep(0,6)
cv.se.6 <- rep(0,6)

# For each degree i we're considering, cross validate models of that degree and store the 
# cv error (that is, the estimated prediction error for models of degree i, estimated by cross validation)
# in cv.error.6[i]. Also, estimate the standard error of the cv error for i and store it in cv.se.6[i].
for (i in 1:6) {
  glm.fit <- glm(all_sectors~poly(tmin,i),data=sales_weather)
  cv.error.6[i] <- cv.glm(sales_weather,glm.fit,K=10)$delta[1]
  # Run cv.glm 100 times to obtain samples for estimated prediction error (CV). 
  # This is so we can estimate the standard error of the estimated prediction error. 
  errs <- rep(1:100)
  for (j in 1:100) {
    glm.fit <- glm(all_sectors~poly(tmin,i),data=sales_weather)
    errs[j] <- cv.glm(sales_weather,glm.fit,K=10)$delta[1]
  }
  cv.se.6[i] <- sd(errs) # Here we assume that the distribution
                         # of estimated prediction errors is Gaussian.
}
cv.error.6
cv.se.6

# Here the degree with lowest cross validation error (at ~3011) is 3. The cv error for 2nd degree models
# is ~3126, which is more than one standard error away from 3011. (This standard error, ~25, in this case,
# is the standard error for the estimated prediction error (the cv error) for 3rd degree polynomial models).
# So, the one standard error rule tells us we must stick with a cubic model. 



## Plot our model against data points & examine residual plots----

# Visualize a cubic model to verify that it looks right.
ggplot(sales_weather, aes(x=tmin, y= all_sectors)) + geom_point(color="blue", alpha=0.4) +
  geom_line(aes(x=sales_weather$tmin,y=glm.fit$fitted.values), color="red")
# This looks reasonable. 

# Check the residual plot: 
plot(glm.fit)
# Residuals vs. fitted values plot looks very good. Best fit line here is beautifully flat.



## Double check to make sure we didn't ignore any important variables other than temperature----
glm.more <- glm(all_sectors~poly(tmin,3)+prcp+snow+snwd, data=sales_weather)
summary(glm.more)
# The p values for precipitation, snow, and snow depth are all greater than 0.45, so we can be
# confident that none of these variables is very important. 