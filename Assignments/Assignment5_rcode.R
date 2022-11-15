#Assignment 5
#Madison Herrboldt
#October 2022

#load required packages
library(tidyverse)
library(dplyr)

#load in the two required csv files from an html format with read_csv
part1 <- read_csv("https://raw.githubusercontent.com/mbtoomey/Biol_7263/main/Data/assignment6part1.csv")
part2 <- read_csv("https://raw.githubusercontent.com/mbtoomey/Biol_7263/main/Data/assignment6part2.csv")


#Here we are dealing with the first dataset, which I am calling part1. First I want to take the column names and separate them into three different columns.
#(this makes the matrix longer so I use pivot_longer)
#Then, for the sample column, I just wanted the sample number so I remove the word sample and transform these values into integers. I separate the names by
#using the _ between them in the original column name and then I move the data to a column called count. After that I want to make the table wider and
#add body length and age as columns in the matrix.
part1a <- part1 %>% pivot_longer(cols = starts_with("Sample"), names_to = c("Sample", "Gender", "Treatment"), 
                                 names_prefix = "Sample", names_transform = list(Sample = as.integer), 
                                 names_sep = "_", values_to = "count") %>% 
                                  pivot_wider(names_from = ID, values_from = count)


#For the second data set I first take the columns and move them to one column named SampleTreatment. Then I remove the prefix of sample and turn the sample
#numbers into integers. I then move all the values associated with the former columns to a column called count. I pipe this tibble into a separate function
#where I separate the SampleTreatment columns into two separate columns: sample and treatment. I use pivot_wider to make the count column a mass column.
#Finally I get rid of the Treatment column because next I will combine the two tibbles and part1a already has a treatment column.
part2a <- part2 %>% pivot_longer(cols = starts_with("Sample"), names_to = c("SampleTreatment"), 
                       names_prefix = "Sample", names_transform = list(Sample = as.integer), 
                       values_to = "count") %>% separate(SampleTreatment, into = c("Sample", "Treatment"), convert = TRUE) %>% 
                        pivot_wider(names_from = ID, values_from = count) %>% select(-Treatment)

#I use the commany full join to join the two tibbles by the sample column
part1a %>% full_join(part2a, by = "Sample") -> final

#Write my combined tibbles to a csv file
write_csv(final, "Results/experiment_data.csv")

#Here I pipe my final tibble into a transmute function where I create a new tibble with gender and treatment info as well as a calculated residual mass
#column. Then I group the data by Treatment and Gender so that I can use the summarize function to make a tibble that calculates the mean mass and standard
#deviation for each gender within each treatment. I use na.rm to remove NAs before calculating these stats. I name this final tibble to the variable 
#summarystats
final %>% transmute(Gender = Gender, Treatment = Treatment, resid_mass = mass / body_length) %>% 
  group_by(Treatment, Gender) %>% 
  summarize(mean_mass = mean(resid_mass, na.rm = TRUE), SD_mass = sd(resid_mass, na.rm = TRUE)) -> summarystats

#I write my final tibble (summarystats) to a csv file
write_csv(final, "Results/summary_stats.csv")




                                                                                      