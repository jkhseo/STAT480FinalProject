rm(list=ls())
#reading in demographic and voter data for the 2012 and 2016 elections
#for some reason Alaska is missing, but it only has 3 electoral votes and has been consistantly republican so it's not too important
library(readr)
library(ggplot2)
library(tidyverse)
Demographics = read.csv("https://raw.githubusercontent.com/MEDSL/2018-elections-unoffical/master/election-context-2018.csv")


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

#finding average swing-state weighted on imporance
#Adding missing race data on Missouri, Virginia, and South Dakota from 2010 Census data
#Unemployment pulled from 2010 numbers, age was under 18, so extrapolated to under 29, last 3 still missing
#https://www.census.gov/quickfacts/SD
State_Data$white_pct[41] = 84.4
State_Data$black_pct[41] = 2.4
State_Data$hispanic_pct[41] = 4.1
State_Data$nonwhite_pct[41] = 100-84.4
State_Data$foreignborn_pct[41] = 3.5
State_Data$female_pct[41] = 49.5
State_Data$age29andunder_pct[41] = 24.7/17*28
State_Data$age65andolder_pct[41] = 16.6
State_Data$average_median_hh_inc[41] = 56499
State_Data$clf_unemploy_pct[41] = 5
State_Data$lesshs_pct[41] = 100-91.7
State_Data$lesshs_whites_pct[41] = NA
State_Data$lesscollege_whites_pct[41] = NA
State_Data$rural_pct[41] = NA

#https://www.census.gov/quickfacts/fact/table/VA/PST045219
State_Data$white_pct[46] = 69.5
State_Data$black_pct[46] = 19.9
State_Data$hispanic_pct[46] = 96
State_Data$nonwhite_pct[46] = 100-69.5
State_Data$foreignborn_pct[46] = 12.3
State_Data$female_pct[46] = 50.8
State_Data$age29andunder_pct[46] = 22/17*28
State_Data$age65andolder_pct[46] = 15.4
State_Data$average_median_hh_inc[46] = 71564
State_Data$clf_unemploy_pct[46] = 7.5
State_Data$lesshs_pct[46] = 100-89.3
State_Data$lesshs_whites_pct[46] = NA
State_Data$lesscollege_whites_pct[46] = NA
State_Data$rural_pct[46] = NA

#https://www.census.gov/quickfacts/fact/table/MO/PST045219
State_Data$white_pct[25] = 83
State_Data$black_pct[25] = 11.8
State_Data$hispanic_pct[25] = 4.3
State_Data$nonwhite_pct[25] = 100-83
State_Data$foreignborn_pct[25] = 4.1
State_Data$female_pct[25] = 50.9
State_Data$age29andunder_pct[25] = 22.5/17*28
State_Data$age65andolder_pct[25] = 16.9
State_Data$average_median_hh_inc[25] = 53560
State_Data$clf_unemploy_pct[25] = 9.6
State_Data$lesshs_pct[25] = 100-89.6
State_Data$lesshs_pct[25] = NA
State_Data$lesscollege_whites_pct[25] = NA
State_Data$rural_pct[25] = NA

#finding the median statistics weighted by importance
Ideal_state = State_Data %>% 
  summarize(white_pct = mean(white_pct*Importance)/mean(Importance),
            black_pct = mean(black_pct*Importance)/mean(Importance),
            nonwhite_pct = mean(nonwhite_pct*Importance)/mean(Importance),
            foreignborn_pct = mean(foreignborn_pct*Importance)/mean(Importance),
            female_pct = mean(female_pct*Importance)/mean(Importance),
            age29andunder_pct = mean(age29andunder_pct*Importance)/mean(Importance),
            age65andolder_pct = mean(age65andolder_pct*Importance)/mean(Importance),
            average_median_hh_inc = mean(average_median_hh_inc*Importance)/mean(Importance),
            clf_unemploy_pct = mean(clf_unemploy_pct*Importance)/mean(Importance),
            lesshs_pct = mean(lesshs_pct*Importance)/mean(Importance))

