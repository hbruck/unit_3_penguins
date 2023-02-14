## Heather Bruck 
# 2023 - 02 - 14

### practice for loops: 
# 1st tutorial talking about R studio 
# there's a free online textbook for R users 
# that textbook should have exercises with for loops 

# Help -> Cheat Sheets -> there's a bunch of cheat sheet stuff for different R things 

# stackoverflow is a great resource for modifying/making aesthetics of ggplot nice 

##########################################################
# Unit 3 - Lesson 2 - ggplot 

library(tidyverse)
library(palmerpenguins)

# to find if something in a package is overrulling something else, do this... 
  find('filter')
    # filter is an example, because the dplyr loaded it up and overrode the base stats package 
    # so whenever I call filter(), it will use the dplyr one not the states one 
    # to fix the override... 
      stats::filter() 
        # identify the package first, then give :: it will use that specific package for that function 
      dplyr::select() 
        # there are a lot of select() in other packages, so it's a good habit to always put dplyr before it

head(penguins)

# ggplot: scatter plots 
  # removing NAs 
  penguins_without_nas <- penguins %>%
    filter(!is.na(flipper_length_mm))

  my_plot <- ggplot(data = penguins_without_nas) + 
  geom_point(aes(x = flipper_length_mm, # aes() is where I can map the variables to various aesthetics in the plot 
                 y = body_mass_g, 
                 color = species, 
                 shape = sex)) + # shape makes the sexes all different shapes on plot 
    theme_bw() + 
    geom_smooth(aes(x = flipper_length_mm,
                    y = body_mass_g)) + # by default, it uses a LS smoother (can change it to be a linear model)
    xlab('Flipper Length (mm)') + 
    ylab('Body Mass (g)') + 
    ggtitle('penguins are cute')

  # saving the figure we created above
    # by default, ggsave plots the most recent plot created 
    # to be extra safe, can name the plot and call directly to it with ggsave
    # save these as png or tiff for the best quality (they are a loss-less compression format)  
  ggsave(my_plot, filename = 'figures/flipper_v_mass.png', 
         width = 7, 
         height = 5, 
         units = 'in', 
         dpi = 300) # dpi is the quality of the image, 300 is good for posters and presentations 
  

# ggplot: lines/ timeseries
  penguin_ts <- penguins %>%
    group_by(year, species) %>% # treat the data separately by year (categorize them by year/species, THEN summarize)
    summarize(num_penguins = n()) # n() counts the number of rows for each year grouping 
  
  ### %>% vs + 
    # %>% feeds data from the previous line into the next 
    # + stacks the provided info together (data is not funneled with each other)
  
  ggplot(data = penguin_ts) + 
    geom_line(aes(x = year, 
                  y = num_penguins, 
                  color = species))
  
# ggplot: histogram 
    # great way to look at 1 dimension of your data (especially DISTRIBUTION)
  # stacked histogram (adds overlapping data up y axis)
  ggplot(data = penguins) + 
    geom_histogram(aes(x = flipper_length_mm,
                       fill = species))
  # un-stack the histogram, don't add them up but instead just lay them onto the x axis 
  ggplot(data = penguins) + 
    geom_histogram(aes(x = flipper_length_mm,
                       fill = species), 
                   position = 'identity')
  # making the bars semi-transparent so you can see everything 
  ggplot(data = penguins) + 
    geom_histogram(aes(x = flipper_length_mm,
                       fill = species,
                       color = species), 
                   position = 'identity', 
                   alpha = 0.5)

# ggplot: changing the colors from default to what I want 
  ggplot(data = penguins) + 
    geom_histogram(aes(x = flipper_length_mm,
                       fill = species,
                       color = species), 
                   position = 'identity', 
                   alpha = 0.5) + 
    scale_fill_manual(values = c('blue', 'pink', 'orange')) + 
    scale_color_manual(values = c('blue', 'pink', 'orange'))

# ggplot: boxplots 
  # basic boxplot 
  ggplot(data = penguins) + 
    geom_boxplot(aes(y = flipper_length_mm, 
                     x = species))
  
  # a better boxplot that shows all of the data points and the distribution 
  ggplot(data = penguins) + 
    geom_boxplot(aes(y = flipper_length_mm, 
                     x = species)) + 
    geom_jitter(aes(y = flipper_length_mm, # jitter randomly spreads out the points a little over x axis (y remains the same value) so they don't all overlap
                    x = species, 
                    color = species), 
                alpha = 0.5, 
                width = 0.2) # width specifies how wide each jitter is 

# ggplot: bar charts 
  sex_na_removed <- penguins %>%
    filter(!is.na(sex))
  
  ggplot(data = sex_na_removed) + 
    geom_bar(aes(x = sex, 
                 fill = species)) + 
    facet_wrap(~species)

  ggplot(data = penguins) + 
    geom_bar(aes(x = island, 
                 fill = species)) + 
    facet_wrap(~species, nrow = 3) + 
    coord_flip() # changes bar plots from up and down to side to side



















