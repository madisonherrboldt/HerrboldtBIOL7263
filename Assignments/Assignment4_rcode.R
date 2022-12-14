#load required library
library(tidyverse)

#load in dataset
Matt_ebird <- read_csv("https://github.com/mbtoomey/Biol_7263/blob/main/Data/MBT_ebird.csv?raw=true")
view(Matt_ebird)

#we want to know in what year the most birds were observed and how many so we will need to group by year and summarize 
year_count <- Matt_ebird %>% #created new variable that will be just the year and counts
  group_by(year) %>% #group the data by year
  summarize(total_birds = sum(count)) #get the sum of the counts for each year
view(year_count)
#arrange into descending order to make it easier to see what year had the most birds
arrange(year_count, by = desc(total_birds))

#now we want to know how many species of birds were observed in 2014, so we will need to filter the original data set
#to be 2014 and then count to get all of the species of birds
species_2014 <- Matt_ebird %>% #create new variable
  filter(year == 2014) %>% #filter by only the year 2014
  count(scientific_name) #list each species and their observed counts
view(species_2014)
nrow(species_2014) #output the number of rows (number of different species observed)

#to find which state has the highest Red-winged blackbird we will filter by the common name and then 
#count number of birds per location (state)
RWBB <- Matt_ebird %>% #create new variable
  filter(common_name == "Red-winged Blackbird") %>% #filter by common name
  count(location, sort = TRUE) #count how many birds were seen in each location and then sort by high to low
view(RWBB)

#here we are calculating the mean rate per checklist that species were encountered each year, honestly not sure if
#I did this right
duration_filter <- Matt_ebird %>%
  filter(duration >= 5 & duration <= 200) %>% #filter by duration to be equal to/between 5 and 200
  group_by(list_ID) %>% #group by checklist
  summarise(duration = first(duration), year = first(year), count = n_distinct(common_name)) #summarize duration and species
#count per checklist and also include the year, not sure if my count function is actually doing what I want here
rate_birds <- mutate(duration_filter, rate = (count/duration)) %>% #calculate the rate per checklist
  group_by(year) %>% #group by year
  summarise(mean_rate = mean(rate)) #calculate the mean rate per year
  
#observation data for the 10 most frequently observed species
bird_freq <- Matt_ebird %>%  
  group_by(scientific_name) %>%  #group by species
  mutate(frequency = sum(count)) %>% #calculate frequency per species
  slice(which.max(frequency)) #index by most frequently seen

Top_10_data <- head(arrange(bird_freq, by = desc(frequency)),10) #create new table arranged by the top ten most 
#frequently seen bird in descending order of frequency

write_csv(Top_10_data, "Results/Top_10_data.csv")



