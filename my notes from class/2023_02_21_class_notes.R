## Heather Bruck 
## 2023 - 02 - 21 
## Unit 3 - Lesson 3 - t.tests

# t-test is used for comparing the MEAN between 2 things 
  # one-sample t-test: compare the sample against a single reference mean 
  # independent sample t-test: comparing 2 data sets or rows to one another
  # paired sample t-test: there is a link between the data in each row (eg. comparing weights of penguins over time of the sample individuals)

# assumptions with t tests 
  # data is normally distributed (check with histogram) 
    # central limit theorem: if you have enough numbers (usually 30), then you can be more lax about normal distribution (it should still be relatively normal but doesn't need to be exact) 
  # no outliers 
  # with 2 sample t tests: 
    # variances are equal 
    # check with Levene test
    # if variance is not equal, then use Welch's t test 
    # if variance is equal, use Student's t test 


# Encyclopedia of Life 
  # search 'gentoo penguin' 
  # click on the organism 
  # click on '51 attributes' 
  # look for desired attribute, for this it is 'body mass' 
    # there is a list of manuscripts with their calculated data 
    # if this was for real research, delve into the manuscripts more to see methods 

library(palmerpenguins)
library(rstatix) # for levene_test() and t_test() 
library(knitr) # prints pretty tables with RMarkdown 
library(tidyverse)


head(penguins)

ggplot(data = penguins) + 
  geom_histogram(aes(x = body_mass_g, 
                     fill = species))

# QQ plot to test if data is normally distributed 
    # want a fairly straight line 
    # a straight line is a normal distribution 
  ggplot(data = gentoo) + 
    stat_qq(aes(sample = body_mass_g))
  
  # calculate mean and standard dev of gentoo 
gentoo %>% 
  summarize(mean_body_mass_g = mean(body_mass_g, na.rm = TRUE), 
            sd_body_mass_g = sd(body_mass_g, na.rm = TRUE))
  

# one sample t test 
gentoo <- penguins %>%
  filter(species == 'Gentoo')

head(gentoo)

ggplot(data = gentoo) + 
  geom_histogram(aes(x = body_mass_g))

  # run t-test 
  t.test(gentoo$body_mass_g,
         mu = 5500) # literature derived value that our data is compared against 
    # very low p-value means that the mean of our data is very different from literature derived value 
  
  
  # another way of running t.test (can make this saved into a table with multiple t-test outputs)
  t_test_results <- gentoo %>%
    t_test(body_mass_g ~ 1, # ~1 means it is a one sample t test 
           mu = 5500)

  
# two sample t test 
  data_for_t_test <- penguins %>%
    filter(species %in% c('Gentoo', 'Adelie'), 
           !is.na(body_mass_g)) %>%
    select(species, body_mass_g) %>% # selects for only specific columns (drops other columns) 
    droplevels() # drops any factor variable that is no longer represented in data (eg. Chinstrap because we didn't filter for them) 
    
    
  head(data_for_t_test)
  summary(data_for_t_test)  

    # creates mean for all of the penguins regardless of species 
  data_for_t_test %>%
    summarize(mean = mean(body_mass_g))
    
    # creates mean within each species 
  data_for_t_test %>%
    group_by(species) %>%
    summarize(mean = mean(body_mass_g), 
              sd = sd(body_mass_g))

  # QQ plot 
  ggplot(data = data_for_t_test) + 
    stat_qq(aes(sample = body_mass_g)) + 
    facet_wrap(~species, 
               scales = 'free') # with QQ plots, make the scales free (makes the scale not the same between species facet wrap) 
  
  
  # Levene test
    # to check equality of variance assumption 
  data_for_t_test %>%
    levene_test(body_mass_g ~ species)
   # p-value is larger than 0.5, so we can say that the variance is similar 
    # a small p-value (<0.05), then we say the variance is not similar 
  
  
  t.test(data_for_t_test$body_mass_g ~ data_for_t_test$species)
    # this ran a Welch's t test
    # can still trust it, Welch's t test is more relaxed so if it is significant with this then it is definetly significant with Student's t test 
  
  # student's t test 
  t.test(data_for_t_test$body_mass_g ~ data_for_t_test$species, var.equal = TRUE)
  
################################################################################
  # Unit 3 - Lesson 4 - Correlations 

# t test asks to compare the same variable between 2 specified things (eg. species) 
# correlation asks if one variable is similar/different to another variable
  
ggplot(data = gentoo) + 
    geom_point(aes(x = bill_length_mm, 
                   y = bill_depth_mm))

# Pearson's R correlation statistic   
  # assume data is normal 
    ggplot(data = gentoo) + 
      stat_qq(aes(sample = bill_length_mm))
    ggplot(data = gentoo) + 
      stat_qq(aes(sample = bill_depth_mm))
  
 cor(x = gentoo$bill_length_mm, 
     y = gentoo$bill_depth_mm, 
     use = 'complete.obs') # tells it to only use rows where none of the columns have NA  
  # correlation near 1 means a strong positive correlation 
  # correlation near -1 menas a strong negative correlation 
 
 # different way of running the same thing, makes output a more complete view of stats 
 cor.test(x = gentoo$bill_length_mm, 
          y = gentoo$bill_depth_mm, 
          use = 'complete.obs')
  
  # another way of running Pearson's correlation test 
 gentoo %>% 
   cor_test(bill_length_mm, bill_depth_mm)

### DO THIS FOR HOLLINGS DATA 
# correlation matrix 
 # run a bunch of tests to look for correlations among a bunch of data 
 cor(gentoo[ ,c(3:6)], # this asks for all of the rows but only columns 3-6 
     use = 'complete.obs')
  
 library(GGally)  

 penguins %>%
   select(species, bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g) %>%
   GGally::ggpairs(aes(color = species))
  
