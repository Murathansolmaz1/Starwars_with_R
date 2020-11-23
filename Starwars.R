install.packages("tidyverse")

library(tidyverse)
library(dplyr)

starwars <- dplyr::starwars
starwars

dplyr::glimpse(starwars)


#Question 1
#How many unique species per their homeworld are there?

numberofunique <- length(unique(starwars$species))
numberofunique

uniques <- %>%
  starwars %>%
  group_by(homeworld) %>%
  summarise(count_species = n_distinct(species))
  

#Question 2
#Which character(s) did play in the Star Wars movies most?

starwars %>% 
  select(name, films) %>% 
  arrange(desc(lengths(films)))%>% 
  summarise(name, films = lengths(films)) %>%
  filter(films==max(films))


#Question 3
#According to the data available, what were the average height value and the average mass value across each species?


starwars %>%
  group_by(species) %>%
  summarise(
            count=n(),
            mean_height = mean(height, na.rm = TRUE),
            mean_mass = mean(mass, na.rm = TRUE)
  )


#Question 4
#Create a new data set by adding a new observation to this data. 
#This observation should be based on your own character (your name or nickname, your weight and height, your homeworld, your starships etc). 
#Note that you can pick one or more than one Star Wars films in which you played as a movie star.

starwars <- %>%
  rbind(starwars,list("Andromeda",220,180,"Yoda's~","brown","white","blue",320,"male","mascu~","Tatooine","Human","Return of the jedi","snowspeeder","jedi starfighter"))


#Question 5
#Calculate the body mass index (BMI) values (dividing the mass value in kg to the square of the height value in meter) 
#for all observations and create a new data set including BMI values and the variables titled as name, mass, height, 
#species, hair color, skin color, eye color, sex and gender.

starwars2 <- (starwars %>%
  select(name,mass,height,species,hair_color,skin_color,eye_color,sex,gender) %>%
  mutate(starwars2, BMI =(mass/((height/100)^2))))

starwars2


#Question 6
#With using this new dataset, categorize the observations as underweight 
#(BMI below 18.5), healthy (BMI between 18.5-24.99), overweight (BMI between 25.0-29.99) 
#and obese (BMI above 30.0). Find the counts of these categories with respect to species.


weight_starwars2 <- starwars2 %>% 
  mutate(BMI_Observation = cut(BMI,breaks=c(-Inf , 18.5 , 25.0 ,  30.0 , Inf),labels=c("underweight","healthy","overweight","obese"))) %>%
  select(BMI,BMI_Observation)
weight_starwars2

#Question 7
#Plot the distribution of BMI according to sex and gender.

ggplot(data = starwars2, aes(x = sex, y = gender, color = BMI)) +
  geom_point() +
  labs(title = "Sex-Gender Relationship in BMI", 
       y = "Gender")



