## Heather Bruck 
## 2023 - 02 - 28


### multiple regression 


# model philosophy 
  # reasons to model
    # model for understanding (what we are doing in this class)
    # model for prediction 
  # two major variables being used 
    # continuous: any number within range is possible, eg. 15.111, 151112...
    # categorical: 
      # also called...
        # factor (assign categorical as this in R)
          # eg. as.factor(penguins$year) makes R treat year in penguins dataset as a factor instead of continuous 
        # nominal (name instead of a number)
        # discrete 


library(palmerpenguins)
library(tidyverse)

head(penguins)

# creating special data frame just for the model 
penguins_lm_3 <- penguins %>%
  filter(!is.na(bill_depth_mm),
         !is.na(bill_length_mm), 
         !is.na(species))
  # check the new dataset 
  head(penguins_lm_3)

# building the model lm_3 
  lm_3 <- lm(bill_depth_mm ~ bill_length_mm + species, data = penguins_lm_3)
    # make bill depth a function of bill length AND species
  class(lm_3)
  summary(lm_3)
    # (Intercept) bill_length_mm is for the Adelie penguins 
    # significant p-values mean that all of the coefficients are important for building an accurate model (they are all significant) 
    # never write these down or copy them by typing them in myself bc there will be mistakes or when you make an adjustment to the model your numbers you wrote down will be wrong 

# ways to access the lm data easily: 
  # makes it easy to assign it as a variable so the models are always accurate 

  coef(lm_3)
    # coef() creates a vector of estimates of all the coefficients in the model 
      # (Intercept) is the y intercept 
  anova(lm_3)
    # analytically the same as a linear model 
    # usually have categorical variable as x 
    # would be helpful for species + island 
  my_results <- broom::tidy(lm_3)
    # need to do broom:: bc it is not automatically loaded up with tidyverse 
    my_results$estimate
    my_results$std.error
    
    # can add parameters to the broom function too 
      broom::tidy(lm_3, conf.int = T) 
        # creates confidence intervals (level is by default 95%) 
      broom::tidy(lm_3, conf.int = T, conf.level = 0.95) 
        # can adjust confidence level 
    
    # can mutate the columns so they are rounded 
      my_results <- broom::tidy(lm_3, conf.int = T, conf.level = 0.95) %>%
        mutate_if(is.numeric, round, 2) # if there is a numeric number, mutate it to be rounded to the 2nd decimal point
    

# visualizing the model 
  library(ggiraph)
  library(ggiraphExtra)  

ggPredict(lm_3, se = T, interactive = T)
  # creates a predictive model graph     
  # se = T adds standard error lines
  # interactive = T means if you hover a mouse over a point it will tell me the exact length and depth of that data point
  # can't go too far with specifications of how the model is plotted or works, this is just an intro thing 

### 3 different ways to visualize the model 

# 1. base R 
  lm_3_predictions <- predict(lm_3, interval = 'confidence', level = 0.95)
    head(lm_3_predictions)  
    # prints out predictions of bill depth for each individual penguin(you figure out what it prints out by reading help file for predict) 
    # 1 is the same as penguin #1 in penguins_lm_3 (it is the prediction of the bill depth for penguin 1 in the data) 
    
  # combine model prediction with the original data 
    # this is all of the data we need to generate the model plot 
  penguins_lm_3_predict <- cbind(penguins_lm_3, lm_3_predictions)
    head(penguins_lm_3_predict)
      # adds the predicted bill depth (fit) with upper/lower CI from prediction 
  
  ggplot(data = penguins_lm_3_predict, aes(x = bill_length_mm, y = bill_depth_mm, color = species)) + 
    geom_point() + 
    geom_line(aes(y = fit)) + # y = fit means to plot the model fit (from lm_3_predictions) on y axis
    geom_ribbon(aes(ymin = lwr, ymax = upr, fill = species, color = NULL),
                alpha = 0.1) # this adds the confidence intervals from lm_3_predictions 
      # color = NULL removes the colored outline from the CI lines 
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
    