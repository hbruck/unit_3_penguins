## Heather Bruck 
## MSCI 709 Class Notes 
## Unit 3 - Lesson 5 - Linear Models 

# Linear regressions 
  # m and b are your parameters 
  # any NA's need to be dropped !!! 

# assumptions of linear regressions 
  # can vary, so take them with a grain of salt (and these rules don't need to be followed exact) 
    # but it still is helpful to check these to have better science and make your own judgements on what should be followed or not 
  # main assumptions: 
    # there is a linear relationship 
    # normality of the model residuals (residual is the true y that was fed into the model - estimated y) 
    # no or little multicollinearity (no multiple dependent variables)
    # no auto-correlation (samples are independent) 
      # this can be a problem with time series 
    # homoscedasticity (homogeneity of variance in residuals across the independent variables) 
  # want at least 20 cases per independent variable 


# plots to look at before doing a linear regression 
 # Scatter plot: Visualise the linear relationship between the predictor and response, check for auto-correlation (if needed) and heteroscedasticity
 # Box plot: To spot any outlier observations in the variable. Having outliers in your predictor can drastically affect the predictions as they can affect the direction/slope of the line of best fit.
 # Histogram / density plot: To see the distribution of the predictor variable. Ideally, a close to normal distribution (a bell shaped curve), without being skewed to the left or right is preferred.
 # Q-Q plot: Also good for checking normality of your predictor variable
 # Correlation matrix (like `GGally::ggpairs()`) to check for multicollinearity in your explanatory variables


library(tidyverse)
library(palmerpenguins)
library(GGally) # ggPairs()
library(broom)  # tidy() augment() #does NOT load with tidyverse


# bill depth ~ bill length (bill depth as a function of bill length) 
penguins %>%
  select(species, bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g) %>%
  GGally::ggpairs(aes(color = species))

penguins %>%
  select(species, bill_depth_mm, bill_length_mm) %>% 
  GGally::ggpairs(aes(color = species))

# linear model (this one is meant to be a failure, don't separate by species) 
lm_1 <- lm(bill_depth_mm ~ bill_length_mm, data = penguins)
  class(lm_1)
  summary(lm_1)
  # the R squared value is 0.05525 
    # this says that bill length can explain about 5% of the variation of bill depth 
  # adjusted R squared value is 0.05247 

  # to check that you did a linear model right (and are capturing the full complexity of the model), make a scatterplot and plot linear model 
    # it makes it clear that you are missing a lot of data in the negative slope and it really doesn't do the data justice
ggplot(data = penguins, aes(x = bill_length_mm, y = bill_depth_mm)) + 
  geom_point() + 
  geom_smooth(method = 'lm') # method specifies it is a linear model 
  # having aes in the ggplot call makes those aesthetics go across multiple plots (like scatterplot and smooth with the same data) 

plot(lm_1)
  # doing the base plot with a linear model makes a bunch of different plots that you hit 'enter' in the console to see them in a row 
    # 1st one: true y compared to predicted y (way to check for homoscedasticity) 
      # you want it to look like a scatter plot with the smoothing line flat 
      # this curvy smoothing plot means that there is a lot of error when the beaks are smaller 
      # this curve is not homoscedasticity
    # 2nd one: QQ plot 
      # a little bit of tail action but it's not too bad
    # 3rd one: square root of the residuals as a function of y 
      # implies that model error is less when the beak is larger (again not homoscedasticity) 
    # 4th one: residuals vs leverage 
      # leverage: how much weight each data point has on the model estimated parameters
      # you want to see something that goes from wide scattered points to narrow, and as it narrows you want it to be close to the red line 



### split the data by species 
gentoo <- penguins %>% 
  filter(species == 'Gentoo')

gentoo %>% 
  select(bill_depth_mm, bill_length_mm) %>%
  ggpairs()

lm_2 <- lm(bill_depth_mm ~ bill_length_mm, data = gentoo) 
  summary(lm_2)

plot(lm_2) 
  
ggplot(data = gentoo, aes(x = bill_length_mm, y = bill_depth_mm)) + 
  geom_point() + 
  geom_smooth(method = 'lm') 
  ### the same way of writing this is: 
  ggplot() + 
   geom_point(data = gentoo, aes(x = bill_length_mm, y = bill_depth_mm)) + 
   geom_smooth(data = gentoo, aes(x = bill_length_mm, y = bill_depth_mm), method = 'lm') 

# can plot multiple species with aes 
  ggplot(data = gentoo, aes(x = bill_length_mm, y = bill_depth_mm)) + 
    geom_point() + 
    geom_smooth(method = 'lm') + 
    geom_point(data = penguins %>% filter(species == 'Adelie'),
               aes(x = bill_length_mm, y = bill_depth_mm), color = 'red') 
  

  
# plot each species lm on the same plot with the combined lm in black 
  ggplot(data = penguins) + 
    geom_point(aes(x = bill_length_mm, y = bill_depth_mm, color = species)) + 
    geom_smooth(aes(x = bill_length_mm, y = bill_depth_mm, color = species), 
                method = 'lm') + 
    geom_smooth(aes(x = bill_length_mm, y = bill_depth_mm),
                method = 'lm', 
                color = 'black') + 
    theme_bw()
  
  
  #########################
  # Exercise 5.1 
  # Build a linear model predicting Gentoo bill depth as a function of flipper length. Plot the predictions. Which explanatory variable (bill length vs. flipper length) does a better job of predicting bill depth? What is your evidence?
  
  
  
  gentoo %>%
    select(flipper_length_mm, bill_depth_mm) %>% 
    GGally::ggpairs()

  gentoo_lm <- lm(bill_depth_mm ~ flipper_length_mm,
                  data = gentoo)  
    summary(gentoo_lm)

  ggplot(data = gentoo, aes(x = flipper_length_mm, y = bill_depth_mm)) + 
    geom_point() + 
    geom_smooth(method = 'lm')
  
  gentoo_lm2 <- lm(bill_depth_mm ~ bill_length_mm, 
                   data = gentoo) 
    summary(gentoo_lm2)  

