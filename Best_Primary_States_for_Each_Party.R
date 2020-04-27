rm(list=ls())
#reading in demographic and voter data for the 2012 and 2016 elections
#for some reason Alaska is missing, but it only has 3 electoral votes and has been consistantly republican so it's not too important
library(readr)
library(ggplot2)
library(tidyverse)
library(ggspatial)
library(sf)
#Reading in State voting and Deomgraphics Data
Demographics = read.csv("https://raw.githubusercontent.com/MEDSL/2018-elections-unoffical/master/election-context-2018.csv")
#Removing variables for state elections
Demographics = Demographics %>% select(-demgov14,-demgov16,-demsen16,-demhouse16,-repgov14,-repgov16,-repsen16,-rephouse16,-othersen16,-otherhouse16,-othergov16,-othergov14)
#Removing three counties with no data
Demographics = na.omit(Demographics)

#Because the data comes at the county level, but we're interested in states we need to aggregate the variables to the state level.
#Here we sum all voter numbers and average demographic data weighted by population
State_Data = Demographics %>% group_by(state) %>% mutate() %>%
  summarize(Republican_Votes16 = sum(trump16),
            Democrat_Votes16=sum(clinton16),
            Percent_Republican16 = sum(trump16)/(sum(trump16)+sum(clinton16)),#Ignores 3rd party votes
            Republican_Votes12 = sum(romney12),
            Democrat_Votes12=sum(obama12),
            Percent_Republican12 = sum(romney12)/(sum(romney12)+sum(obama12)),
            white_pct = mean(white_pct*total_population)/mean(total_population),
            black_pct = mean(black_pct*total_population)/mean(total_population),
            hispanic_pct = mean(hispanic_pct*total_population)/mean(total_population),
            nonwhite_pct = mean(nonwhite_pct*total_population)/mean(total_population),
            foreignborn_pct = mean(foreignborn_pct*total_population)/mean(total_population),
            female_pct = mean(female_pct*total_population)/mean(total_population),
            age29andunder_pct = mean(age29andunder_pct*total_population)/mean(total_population),
            age65andolder_pct = mean(age65andolder_pct*total_population)/mean(total_population),
            average_median_hh_inc = mean(median_hh_inc), #median income is now the average of county median incomes
            clf_unemploy_pct = mean(clf_unemploy_pct*total_population)/mean(total_population),
            lesshs_pct = mean(lesshs_pct*total_population)/mean(total_population),
            lesscollege_pct = mean(lesscollege_pct*total_population)/mean(total_population),
            lesshs_whites_pct = mean(lesshs_whites_pct*total_population)/mean(total_population),
            lesscollege_whites_pct = mean(lesscollege_whites_pct*total_population)/mean(total_population),
            rural_pct = mean(rural_pct*total_population)/mean(total_population),
            ID = mean(fips)%/%1000)

#Adding Electoral votes column
ECVotes = read.csv("https://raw.githubusercontent.com/PitchInteractiveInc/tilegrams/master/data/us/electoral-college-votes-by-state.csv",header = FALSE,col.names = c("ID","ECvotes"))
ECVotes = ECVotes %>% slice(-2) #removing Alaska
State_Data = State_Data %>% left_join(ECVotes, by = "ID")

#Here we create an importance rating based on the how many votes the state has, and how close it's election was
#Formula for closeness puts states on an S curve based on how close they were to 50/50
#2016 closeness weighted to 67% 2012 weighted to 33%
State_Data = State_Data %>% mutate(Closeness16 = (.001/(.5-Percent_Republican16)^4)/((.001/(.5-Percent_Republican16)^4)+20),
                                   Closeness12 = (.001/(.5-Percent_Republican12)^4)/((.001/(.5-Percent_Republican12)^4)+20)) %>% 
  mutate(Importance = (Closeness16*2/3+Closeness12/3)*ECvotes)

#plots of imporance and closeness scores between the two elections
State_Data %>% ggplot(aes(x=Importance)) + geom_histogram()
State_Data %>% ggplot(aes(x=Closeness16,y=Closeness12)) + geom_point()

#Changing Non-white to other
State_Data = State_Data %>% mutate(nonwhite_pct = nonwhite_pct-hispanic_pct-black_pct) %>% rename("other_pct" = nonwhite_pct)

#finding the median statistics weighted by importance
Ideal_State = State_Data %>% 
  summarize(white_pct = mean(white_pct*Importance)/mean(Importance),
            black_pct = mean(black_pct*Importance)/mean(Importance),
            hispanic_pct = mean(hispanic_pct*Importance)/mean(Importance),
            other_pct = mean(other_pct*Importance)/mean(Importance),
            foreignborn_pct = mean(foreignborn_pct*Importance)/mean(Importance),
            female_pct = mean(female_pct*Importance)/mean(Importance),
            age29andunder_pct = mean(age29andunder_pct*Importance)/mean(Importance),
            age65andolder_pct = mean(age65andolder_pct*Importance)/mean(Importance),
            average_median_hh_inc = mean(average_median_hh_inc*Importance)/mean(Importance),
            clf_unemploy_pct = mean(clf_unemploy_pct*Importance)/mean(Importance),
            lesshs_pct = mean(lesshs_pct*Importance)/mean(Importance),
            lesscollege_pct = mean(lesscollege_pct*Importance)/mean(Importance),
            lesshs_whites_pct = mean(lesshs_whites_pct*Importance)/mean(Importance),
            lesscollege_whites_pct = mean(lesscollege_whites_pct*Importance)/mean(Importance),
            rural_pct = mean(rural_pct*Importance)/mean(Importance))


#-----------------------------------------------------part 2-----------------------------------------------------
State_Score = State_Data %>%
  select(state, white_pct:rural_pct)

#Subtracting each demographic observation from the ideal observation for each state
i=2
j=1
for(i in 2:length(Ideal_State)){
  for(j in 1:nrow(State_Score)){
    State_Score[j,i]= abs(State_Score[j,i]-Ideal_State[i-1])
  }
}

#Adjusting the "average_median_hh_inc" variable
State_Score = State_Score %>%
  mutate(average_median_hh_inc = average_median_hh_inc/3000)

#Creating "score" variable (combining all of the absolute values of the subtracted demographic observations for each state)
State_Score$score = rowSums(State_Score[,2:16])

#The top 5 best primary states in general (the lower score means less difference from the ideal state demographic)
State_Score %>%
  select(state, score) %>%
  arrange(score) %>%
  slice(1:5) #Illinois, New York, New Jersey, Washington, Delaware

#-----------------------------------Republican Party-----------------------------------
Republican_State_Score = State_Data %>%
  select(state, white_pct:rural_pct)

#Weightings
#Increasing prefered demographics republicans have trouble with: Black, hispanic, female, youth
Republican_Ideal_State = Ideal_State %>% mutate(black_pct = black_pct + sd(State_Score$black_pct),
                                                hispanic_pct = hispanic_pct + sd(State_Score$hispanic_pct),
                                                other_pct = other_pct + sd(State_Score$other_pct),
                                                female_pct = female_pct + sd(State_Score$female_pct),
                                                age29andunder_pct = age29andunder_pct - sd(State_Score$age29andunder_pct),
                                                lesshs_pct = lesshs_pct - sd(State_Score$lesshs_pct),
                                                lesscollege_pct = lesscollege_pct - sd(State_Score$lesscollege_pct),
                                                lesshs_whites_pct = lesshs_whites_pct - sd(State_Score$lesshs_whites_pct),
                                                lesscollege_whites_pct = lesscollege_whites_pct - sd(State_Score$lesscollege_whites_pct),
                                                rural_pct = rural_pct - sd(State_Score$rural_pct))

#Subtracting each demographic observation from the ideal observation for each state
i=2
j=1
for(i in 2:length(Republican_Ideal_State)){
  for(j in 1:nrow(Republican_State_Score)){
    Republican_State_Score[j,i]= abs(Republican_State_Score[j,i]-Republican_Ideal_State[i-1])
  }
}

#Adjusting the "average_median_hh_inc" variable
Republican_State_Score = Republican_State_Score %>%
  mutate(average_median_hh_inc = average_median_hh_inc/3000, #bringing importance income in line with other variables
         foreignborn_pct = foreignborn_pct*.45)# only 45% of the foriegn born population can vote, so we weight this lower -- https://www.pewresearch.org/fact-tank/2019/06/17/key-findings-about-u-s-immigrants/

#Creating "score" variable (summing all of the absolute values of the subtracted demographic observations for each state)
Republican_State_Score$score = rowSums(Republican_State_Score[,2:16])

#The top 5 best states in Republican Party (the lower score means less difference from the ideal state demographic)
Republican_State_Score %>%
  select(state, score) %>%
  arrange(score) %>%
  slice(1:20) #Illinois, New Jersey, New York, Connecticut, Virginia

#--------------------Republican-Map---------------------
states_location = map_data("state") #doesn't have Hawaii
states_location_for_texts = read.csv("statelatlong.csv")

states_location$region = str_to_title(states_location$region)
states_location_for_texts = states_location_for_texts %>%
  slice(-2)

Republican_State_Score_map = Republican_State_Score %>% 
  left_join(states_location, by = c("state" = "region")) %>%
  left_join(states_location_for_texts, by = c("state" = "City"))

ggplot(Republican_State_Score_map, aes(x = long, y = lat)) + 
  geom_polygon(aes(group = group, fill = score), color = "black") +
  geom_polygon(data = Republican_State_Score_map %>% 
                 filter(state %in% c("Illinois", "New Jersey", "New York", "Connecticut", "Virginia")),
               aes(group = group, fill = score), size = 2, color = "black") +
  scale_fill_gradient(low = "red", high = "white", na.value = NA) +
  geom_text(data = Republican_State_Score_map %>% 
              filter(!state %in% c("New York", "New Hampshire", "Vermont", "Massachusetts", "Rhode Island", "Connecticut", "New Jersey", "Delaware", "Maryland", "District of Columbia", "West Virginia",
                                   "Florida", "Idaho", "Nevada", "Oklahoma", "Texas", "Louisiana", "Minnesota", "Michigan", "Illinois", "North Carolina", "Virginia", "Hawaii", "Washington")),
            aes(x = Longitude, y = Latitude, label = State), 
            vjust = 0 , size = 5) +
  geom_text(data = Republican_State_Score_map %>% 
              filter(!state %in% c("New York", "New Hampshire", "Vermont", "Massachusetts", "Rhode Island", "Connecticut", "New Jersey", "Delaware", "Maryland", "District of Columbia", "West Virginia",
                                   "Florida", "Idaho", "Nevada", "Oklahoma", "Texas", "Louisiana", "Minnesota", "Michigan", "Illinois", "North Carolina", "Virginia", "Hawaii", "Washington")),
            aes(x = Longitude, y = Latitude, label = round(score, digits = 0)), 
            vjust = + 1.5 , size = 5, color = "black", family = "Times") +
  #-----------------------Abb. state names manually entered-----------------------------
  annotate(geom = "text", x = -75, y = 44, label = "NY", size = 5, fontface = "bold") +
  annotate(geom = "text", x = -71.5, y = 44, label = "NH", size = 5, fontface = "bold") +
  annotate(geom = "text", x = -72.5, y = 44.5, label = "VT", size = 5, fontface = "bold") +
  annotate(geom = "text", x = -69.5, y = 42.5, label = "MA", size = 5, fontface = "bold") +
  annotate(geom = "text", x = -70.5, y = 41, label = "RI", size = 5, fontface = "bold") +
  annotate(geom = "text", x = -72, y = 40.5, label = "CT", size = 5, fontface = "bold") +
  annotate(geom = "text", x = -73, y = 39.5, label = "NJ", size = 5, fontface = "bold") +
  annotate(geom = "text", x = -72.5, y = 38.4, label = "DE", size = 5, fontface = "bold") +
  annotate(geom = "text", x = -74, y = 38, label = "MD", size = 5, fontface = "bold") +
  annotate(geom = "text", x = -74.5, y = 36.9, label = "DC", size = 5, fontface = "bold") +
  annotate(geom = "text", x = -80.5, y = 39, label = "WV", size = 5, fontface = "bold") +
  annotate(geom = "text", x = -81.7, y = 29, label = "FL", size = 5, fontface = "bold") +
  annotate(geom = "text", x = -114.5, y = 44.5, label = "ID", size = 5, fontface = "bold") +
  annotate(geom = "text", x = -117, y = 40, label = "NV", size = 5, fontface = "bold") +
  annotate(geom = "text", x = -97.5, y = 36, label = "OK", size = 5, fontface = "bold") +
  annotate(geom = "text", x = -100, y = 32, label = "TX", size = 5, fontface = "bold") +
  annotate(geom = "text", x = -92.5, y = 31.5, label = "LA", size = 5, fontface = "bold") +
  annotate(geom = "text", x = -94.5, y = 46.5, label = "MN", size = 5, fontface = "bold") +
  annotate(geom = "text", x = -85, y = 43.5, label = "MI", size = 5, fontface = "bold") +
  annotate(geom = "text", x = -89, y = 41, label = "IL", size = 5, fontface = "bold") +
  annotate(geom = "text", x = -79, y = 36, label = "NC", size = 5, fontface = "bold") +
  annotate(geom = "text", x = -78.5, y = 38, label = "VA", size = 5, fontface = "bold") +
  annotate(geom = "text", x = -120, y = 48, label = "WA", size = 5, fontface = "bold") +
  annotate(geom = "text", x = -115, y = 26, label = "HI", size = 5, fontface = "bold") +
  #--------------------------Score manually entered-----------------------------------
  annotate(geom = "text", x = -75, y = 43.3, label = "48", size = 5, fontface = "bold") +
  annotate(geom = "text", x = -71.5, y = 43.3, label = "138", size = 5, fontface = "bold") +
  annotate(geom = "text", x = -72.5, y = 43.8, label = "163", size = 5, fontface = "bold") +
  annotate(geom = "text", x = -69.5, y = 41.8, label = "66", size = 5, fontface = "bold") +
  annotate(geom = "text", x = -70.5, y = 40.3, label = "73", size = 5, fontface = "bold") +
  annotate(geom = "text", x = -72, y = 40, label = "56", size = 5, fontface = "bold") +
  annotate(geom = "text", x = -73, y = 39, label = "44", size = 5, fontface = "bold") +
  annotate(geom = "text", x = -72.5, y = 37.8, label = "64", size = 5, fontface = "bold") +
  annotate(geom = "text", x = -74, y = 37.5, label = "74", size = 5, fontface = "bold") +
  annotate(geom = "text", x = -74.5, y = 36.3, label = "172", size = 5, fontface = "bold") +
  annotate(geom = "text", x = -80.7, y = 38.3, label = "189", size = 5, fontface = "bold") +
  annotate(geom = "text", x = -81.7, y = 28.3, label = "66", size = 5, fontface = "bold") +
    annotate(geom = "text", x = -114.5, y = 43.8, label = "121", size = 5, fontface = "bold") +
  annotate(geom = "text", x = -117, y = 39.3, label = "81", size = 5, fontface = "bold") +
  annotate(geom = "text", x = -97.5, y = 35.3, label = "101", size = 5, fontface = "bold") +
  annotate(geom = "text", x = -100, y = 31.3, label = "102", size = 5, fontface = "bold") +
  annotate(geom = "text", x = -92.5, y = 30.8, label = "119", size = 5, fontface = "bold") +
  annotate(geom = "text", x = -94.5, y = 45.8, label = "100", size = 5, fontface = "bold") +
  annotate(geom = "text", x = -85, y = 42.8, label = "97", size = 5, fontface = "bold") +
  annotate(geom = "text", x = -89, y = 40.3, label = "43", size = 5, fontface = "bold") +
  annotate(geom = "text", x = -79, y = 35.3, label = "84", size = 5, fontface = "bold") +
  annotate(geom = "text", x = -78.5, y = 37.3, label = "56", size = 5, fontface = "bold") +
  annotate(geom = "text", x = -120, y = 47.3, label = "62", size = 5, fontface = "bold") +
  annotate(geom = "text", x = -115, y = 25.3, label = "155", size = 5, fontface = "bold") +
  #annotation_scale() +
  #annotation_north_arrow() +
  theme_bw() +
  labs(fill = "State Scores",
       title = "Scores by State") +
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0.5, size = 15, face = "bold")) +
  scale_y_continuous(breaks=c()) +
  scale_x_continuous(breaks=c())
  #theme_void()


#-----------------------------------Democratic Party-----------------------------------
Democrat_State_Score = State_Data %>%
  select(state, white_pct:rural_pct)

#Weightings
#Increasing prefered demographics democrats have trouble with: Lower Education, white, rural
Democrat_Ideal_State = Ideal_State %>% mutate(black_pct = black_pct - sd(State_Score$black_pct),
                                              hispanic_pct = hispanic_pct - sd(State_Score$hispanic_pct),
                                              other_pct = other_pct - sd(State_Score$other_pct),
                                              female_pct = female_pct - sd(State_Score$female_pct),
                                              age29andunder_pct = age29andunder_pct - sd(State_Score$age29andunder_pct),
                                              lesshs_pct = lesshs_pct + sd(State_Score$lesshs_pct),
                                              lesscollege_pct = lesscollege_pct + sd(State_Score$lesscollege_pct),
                                              lesshs_whites_pct = lesshs_whites_pct + sd(State_Score$lesshs_whites_pct),
                                              lesscollege_whites_pct = lesscollege_whites_pct + sd(State_Score$lesscollege_whites_pct),
                                              rural_pct = rural_pct + sd(State_Score$rural_pct))

#Subtracting each demographic observation from the ideal observation for each state
i=2
j=1
for(i in 2:length(Democrat_Ideal_State)){
  for(j in 1:nrow(Democrat_State_Score)){
    Democrat_State_Score[j,i]= abs(Democrat_State_Score[j,i]-Democrat_Ideal_State[i-1])
  }
}

#Adjusting the "average_median_hh_inc" variable
Democrat_State_Score = Democrat_State_Score %>%
  mutate(average_median_hh_inc = average_median_hh_inc/3000, #bringing importance income in line with other variables
         foreignborn_pct = foreignborn_pct*.45)# only 45% of the foriegn born population can vote, so we weight this lower -- https://www.pewresearch.org/fact-tank/2019/06/17/key-findings-about-u-s-immigrants/

#Creating "score" variable (summing all of the absolute values of the subtracted demographic observations for each state)
Democrat_State_Score$score = rowSums(Democrat_State_Score[,2:16])

#The top 5 best states in Republican Party (the lower score means less difference from the ideal state demographic)
Democrat_State_Score %>%
  select(state, score) %>%
  arrange(score) %>%
  slice(1:20) #Rhode Island, Delaware, Indiana, Pennsylvania, Oklahoma

#--------------------Democratic-Map---------------------
states_location = map_data("state") #doesn't have Hawaii
states_location_for_texts = read.csv("statelatlong.csv")

states_location$region = str_to_title(states_location$region)
states_location_for_texts = states_location_for_texts %>%
  slice(-2)

Democratic_State_Score_map = Democrat_State_Score %>% 
  left_join(states_location, by = c("state" = "region")) %>%
  left_join(states_location_for_texts, by = c("state" = "City"))

ggplot(Democratic_State_Score_map, aes(x = long, y = lat)) + 
  geom_polygon(aes(group = group, fill = score), color = "black") +
  geom_polygon(data = Democratic_State_Score_map %>% 
                 filter(state %in% c("Rhode Island", "Delaware", "Indiana", "Pennsylvania", "Oklahoma")),
               aes(group = group, fill = score), size = 2, color = "black") +
  scale_fill_gradient(low = "blue3", high = "white", na.value = NA) +
  geom_text(data = Democratic_State_Score_map %>% 
              filter(!state %in% c("New York", "New Hampshire", "Vermont", "Massachusetts", "Rhode Island", "Connecticut", "New Jersey", "Delaware", "Maryland", "District of Columbia", "West Virginia",
                                   "Florida", "Idaho", "Nevada", "Oklahoma", "Texas", "Louisiana", "Minnesota", "Michigan", "Illinois", "North Carolina", "Virginia", "Hawaii", "Washington")),
            aes(x = Longitude, y = Latitude, label = State), 
            vjust = 0 , size = 5) +
  geom_text(data = Democratic_State_Score_map %>% 
              filter(!state %in% c("New York", "New Hampshire", "Vermont", "Massachusetts", "Rhode Island", "Connecticut", "New Jersey", "Delaware", "Maryland", "District of Columbia", "West Virginia",
                                   "Florida", "Idaho", "Nevada", "Oklahoma", "Texas", "Louisiana", "Minnesota", "Michigan", "Illinois", "North Carolina", "Virginia", "Hawaii", "Washington")),
            aes(x = Longitude, y = Latitude, label = round(score, digits = 0)), 
            vjust = + 1.5 , size = 5, color = "black", family = "Times") +
  #-----------------------Abb. state names manually entered-----------------------------
annotate(geom = "text", x = -75, y = 44, label = "NY", size = 5, fontface = "bold") +
  annotate(geom = "text", x = -71.5, y = 44, label = "NH", size = 5, fontface = "bold") +
  annotate(geom = "text", x = -72.5, y = 44.5, label = "VT", size = 5, fontface = "bold") +
  annotate(geom = "text", x = -69.5, y = 42.5, label = "MA", size = 5, fontface = "bold") +
  annotate(geom = "text", x = -70.5, y = 41, label = "RI", size = 5, fontface = "bold") +
  annotate(geom = "text", x = -72, y = 40.5, label = "CT", size = 5, fontface = "bold") +
  annotate(geom = "text", x = -73, y = 39.5, label = "NJ", size = 5, fontface = "bold") +
  annotate(geom = "text", x = -72.5, y = 38.4, label = "DE", size = 5, fontface = "bold") +
  annotate(geom = "text", x = -74, y = 38, label = "MD", size = 5, fontface = "bold") +
  annotate(geom = "text", x = -74.5, y = 36.9, label = "DC", size = 5, fontface = "bold") +
  annotate(geom = "text", x = -80.5, y = 39, label = "WV", size = 5, fontface = "bold") +
  annotate(geom = "text", x = -81.7, y = 29, label = "FL", size = 5, fontface = "bold") +
  annotate(geom = "text", x = -114.5, y = 44.5, label = "ID", size = 5, fontface = "bold") +
  annotate(geom = "text", x = -117, y = 40, label = "NV", size = 5, fontface = "bold") +
  annotate(geom = "text", x = -97.5, y = 36, label = "OK", size = 5, fontface = "bold") +
  annotate(geom = "text", x = -100, y = 32, label = "TX", size = 5, fontface = "bold") +
  annotate(geom = "text", x = -92.5, y = 31.5, label = "LA", size = 5, fontface = "bold") +
  annotate(geom = "text", x = -94.5, y = 46.5, label = "MN", size = 5, fontface = "bold") +
  annotate(geom = "text", x = -85, y = 43.5, label = "MI", size = 5, fontface = "bold") +
  annotate(geom = "text", x = -89, y = 41, label = "IL", size = 5, fontface = "bold") +
  annotate(geom = "text", x = -79, y = 36, label = "NC", size = 5, fontface = "bold") +
  annotate(geom = "text", x = -78.5, y = 38, label = "VA", size = 5, fontface = "bold") +
  annotate(geom = "text", x = -120, y = 48, label = "WA", size = 5, fontface = "bold") +
  annotate(geom = "text", x = -115, y = 26, label = "HI", size = 5, fontface = "bold") +
  #--------------------------Score manually entered-----------------------------------
annotate(geom = "text", x = -75, y = 43.3, label = "87", size = 5, fontface = "bold") +
  annotate(geom = "text", x = -71.5, y = 43.3, label = "123", size = 5, fontface = "bold") +
  annotate(geom = "text", x = -72.5, y = 43.8, label = "147", size = 5, fontface = "bold") +
  annotate(geom = "text", x = -69.5, y = 41.8, label = "79", size = 5, fontface = "bold") +
  annotate(geom = "text", x = -70.5, y = 40.3, label = "58", size = 5, fontface = "bold") +
  annotate(geom = "text", x = -72, y = 40, label = "79", size = 5, fontface = "bold") +
  annotate(geom = "text", x = -73, y = 39, label = "90", size = 5, fontface = "bold") +
  annotate(geom = "text", x = -72.5, y = 37.8, label = "65", size = 5, fontface = "bold") +
  annotate(geom = "text", x = -74, y = 37.5, label = "110", size = 5, fontface = "bold") +
  annotate(geom = "text", x = -74.5, y = 36.3, label = "205", size = 5, fontface = "bold") +
  annotate(geom = "text", x = -80.7, y = 38.3, label = "125", size = 5, fontface = "bold") +
  annotate(geom = "text", x = -81.7, y = 28.3, label = "74", size = 5, fontface = "bold") +
  annotate(geom = "text", x = -114.5, y = 43.8, label = "80", size = 5, fontface = "bold") +
  annotate(geom = "text", x = -117, y = 39.3, label = "71", size = 5, fontface = "bold") +
  annotate(geom = "text", x = -97.5, y = 35.3, label = "66", size = 5, fontface = "bold") +
  annotate(geom = "text", x = -100, y = 31.3, label = "113", size = 5, fontface = "bold") +
  annotate(geom = "text", x = -92.5, y = 30.8, label = "89", size = 5, fontface = "bold") +
  annotate(geom = "text", x = -94.5, y = 45.8, label = "95", size = 5, fontface = "bold") +
  annotate(geom = "text", x = -85, y = 42.8, label = "71", size = 5, fontface = "bold") +
  annotate(geom = "text", x = -89, y = 40.3, label = "69", size = 5, fontface = "bold") +
  annotate(geom = "text", x = -79, y = 35.3, label = "76", size = 5, fontface = "bold") +
  annotate(geom = "text", x = -78.5, y = 37.3, label = "88", size = 5, fontface = "bold") +
  annotate(geom = "text", x = -120, y = 47.3, label = "71", size = 5, fontface = "bold") +
  annotate(geom = "text", x = -115, y = 25.3, label = "172", size = 5, fontface = "bold") +
  #annotation_scale() +
  #annotation_north_arrow() +
  theme_bw() +
  labs(fill = "State Scores",
       title = "Scores by State") +
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0.5, size = 15, face = "bold")) +
  scale_y_continuous(breaks=c()) +
  scale_x_continuous(breaks=c())
#theme_void()


