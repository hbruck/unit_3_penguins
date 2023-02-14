## Heather Bruck  
## 2023-02-09 
## Unit 3 - Lesson 1 - Intro to dplyr

library(tidyverse)

tidyverse_packages() # makes a list of all the packages tidyverse includes 
  # only the ones that show up in 'attaching packages' when loading package are loaded 
  # the other ~21 of them would need to be loaded in addition to library(tidyverse) 

library(palmerpenguins) # loads up penguin data 

head(penguins)

summary(penguins)
  # summary includes how many NA in each variable (at the bottom) 

glimpse(penguins)
  # looks at the first few rows of all columns, transposes them so it goes left to right instead of up to down 

class(penguins)
  # tibble (tbl) = a version of a data frame, basically the same thing as data frame 
  # tbl_df means it can be tibble or data frame 
  
  # to change a tibble to a data frame (if given error...) 
    # as.data.frame() 

mean(penguins$bill_depth_mm)
  # says NA because there is a NA somewhere in it 
  mean(penguins$bill_depth_mm, na.rm = TRUE)
    # removes NA from the mean analysis 
  
# filter by species 
  gentoo <- filter(.data = penguins, species == 'Gentoo')
    head(gentoo)  
    summary(gentoo)  

  gentoo_ladies <- filter(penguins, species == 'Gentoo', sex == 'female')
    summary(gentoo_ladies)
  
### pipes 
  # %>% 
  # says take the info from before the pipe and send it to after the pipe 
  # round(mean((data)^2)) has basic outline of h(g(f(x))) where h, g, and f are functions of x 
    # can be changed to x %>% (read going down rather than nested) 
                      # f() %>% 
                      # g() %>% 
                      # h() 
  # piping takes whatever variable precedes it and makes it into the parameter for the next function 
  
gentoo_ladies = penguins %>%
  filter(species == 'Gentoo') %>% 
  filter(sex == 'female') 
  
# CHANGE THESIS CODE TO USE PIPING INSTEAD OF BASIC R 
# finding mean body mass of all female penguins 
penguins %>%
  filter(sex == 'female') %>%
  summarize(mean_mass_g = mean(body_mass_g)) 
  
  # base r version of the same thing 
  mean(penguins$body_mass_g[penguins$sex == 'female'], na.rm = TRUE)
    # na.rm = TRUE is the parameter for base R functions (not consistent) 

### grouping 
  # group by specified condition, then provide me the output I want (like mean) 
  species_sex_mass <- penguins %>% 
    filter(!is.na(sex)) %>% # when using tidyverse use is.na(x) to remove na's before analyses 
    group_by(species, sex) %>% # provides mean body mass for each sex and species 
    summarize(mean_mass_g = mean(body_mass_g))

# create a csv table of the table created (helpful for putting summary statistics together)
  write_csv(x = species_sex_mass, file = 'data/species_sex_mass.csv')

# count () 
  # count species and sex 
  species_sex_count <- penguins %>% 
    filter(!is.na(sex)) %>% 
    group_by(species, sex) %>% 
    summarize(count = n()) # count asks for the number in specified groups 
  
  # count species (not sex) 
  species_count <- penguins %>% 
    # filter(!is.na(sex)) %>%   # remove this filter because what if sex wasn't determined 
    group_by(species) %>% 
    summarize(count = n())
    

# mutate () 
  # conversion of body mass from lbs to g 
  penguins_for_america <- penguins %>% 
    mutate(body_mass_lb = body_mass_g * 0.0022) 
  head(penguins_for_america)  
  glimpse(penguins_for_america)  
  
# distinct() 
  # give me a specific list 
  penguins %>% 
    distinct(island) 

# select() 
  # reduces tibble to include only parts that you want to see 
  penguins %>% 
    select(species, sex) 

  bill_removed <- penguins %>% 
    select(-bill_length_mm, -bill_depth_mm) # putting - before column names in select removes them
  
# arrange()
  # sorting (can do smallest to largest penguin), ascending: 
  penguins %>% 
    arrange(body_mass_g)
  # descending order: 
  penguins %>% 
    arrange(desc(body_mass_g))

##### EXERCISE 1.3 
      # What is the mean bill length (in inches) of Adelie penguins found on either Dream island or Biscoe island? What is the standard deviation? Is the mean larger or smaller than the mean bill length of Adelie penguins found on Torgersen island?

  mean_bill_length_in <- penguins %>% 
    filter(species == 'Adelie', 
           island %in% c('Biscoe','Dream'), 
           !is.na(bill_length_mm)) %>%
    mutate(bill_length_in = bill_length_mm * 0.039) %>% # 0.039 in/mm
    summarize(mean_bill_length_in = mean(bill_length_in), 
              sd(bill_length_in)) 
  
  mean_bill_length_in_torgersen <- penguins %>% 
    filter(species == 'Adelie', 
           island == 'Torgersen', 
           !is.na(bill_depth_mm)) %>% 
    mutate(bill_length_in = bill_length_mm * 0.039) %>% # 0.039 in/mm
    summarize(mean_bill_length_in_torgersen = mean(bill_length_in))
  
    