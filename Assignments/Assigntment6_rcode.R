#load required packages
library(tidyverse)
library(ggplot2)
library(patchwork)

#load in dataset 
Matt_ebird <- read_csv("https://github.com/mbtoomey/Biol_7263/blob/main/Data/MBT_ebird.csv?raw=true")
view(Matt_ebird)

options(dplyr.summarise.inform = FALSE) #summarise recognizes all groups

#calculate the total number of species seen each month of each year in each location
count_data <- Matt_ebird %>% #create new variable
  group_by(month, year, location) %>% #group data by month, year, and location
  count(scientific_name) %>% #get the unique species
  summarise(number_species=n()) #get counts of number of species found per month, year, and location
view(count_data)

#plot
plot1 <- ggplot(data = count_data) + #tell it the data set
  aes(as.factor(month), number_species) + #reference the x and y variables
  geom_point(aes(color = year), size = 2) + #color points by year 
  facet_wrap(~location) + #wrap the plots by locations
  xlab("Month") + #add x axis label
  ylab("Number of Species") + #add y axis label
  ggtitle("Number of Observed Species by State per Year") #add plot title
plot1

#plot mass by treatment
mass_data <- read.csv("Results/summary_stats.csv") #load in data

plot2 <- ggplot(data = mass_data, aes(Treatment,mass)) + #give it the data set and assign x and y 
  geom_jitter(size = 3, aes(Treatment, mass, shape = Gender)) + #plot mass by treatment and sex (shape of points)
  xlab("Treatment Group") + #add x axis label
  ylab("Mass") + #add y axis label
  stat_summary(fun = mean, geom = "crossbar", width = 0.5, color = "blue") + #add the mean
  stat_summary(geom = "errorbar", width = 0.3) + #add error bars
  labs(shape = "Sex") #add the key label
plot2

#plot of mass with age
plot3 <- ggplot(data = mass_data, aes(age, mass)) + #give it the data set and assign x and y
  geom_point(size = 3, aes(age, mass, shape = Treatment)) + #plot mass by age with point shapes differing by treatment
  xlab("Age") + #add x axis label
  ylab("Mass") + #add y axis label
  geom_smooth(size = 2, method = lm, aes(color = Treatment, group = Treatment), se=FALSE) + #add trend lines without confidence interval
  labs(shape = "Treatment") #add key label
plot3

#combine the plots
plot2+plot3+plot_annotation(title = "Mass Across Treatments, Sex, and Age",
                            tag_levels = "A") 



