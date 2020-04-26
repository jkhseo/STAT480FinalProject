rm(list=ls())
library(readr)
library(ggplot2)
library(tidyverse)
library(logistf)
library(matrixStats)
#Reading in State voting and Deomgraphics Data
Demographics <- read.csv("https://raw.githubusercontent.com/MEDSL/2018-elections-unoffical/master/election-context-2018.csv")
#Removing variables for state elections
Demographics <- Demographics %>% select(-demgov14,-demgov16,-demsen16,-demhouse16,-repgov14,-repgov16,-repsen16,-rephouse16,-othersen16,-otherhouse16,-othergov16,-othergov14)
#Removing three counties with no data
Demographics <- na.omit(Demographics)

#Grouping the data by State
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

#Data Manipulation
#Finding total number of voters
#Creating a variable for the winner 1 = republican, 0 = democrat
State_Data <- State_Data %>% mutate(total_voters = Republican_Votes16 + Democrat_Votes16)
State_Data <- State_Data %>% mutate(winner = ifelse(Percent_Republican16 > .5,1,0))

#Represents the demographic of the voters in the entire country
#Weighted averages by population per state
US_Data <- State_Data %>%
  summarize(
  white_pct = sum(white_pct * total_voters)/sum(total_voters),
  black_pct = sum(black_pct * total_voters)/sum(total_voters),
  hispanic_pct = sum(hispanic_pct * total_voters)/sum(total_voters),
  nonwhite_pct = sum(nonwhite_pct * total_voters)/sum(total_voters),
  foreignborn_pct = sum(foreignborn_pct * total_voters)/sum(total_voters),
  female_pct = sum(female_pct * total_voters)/sum(total_voters),
  age29andunder_pct = sum(age29andunder_pct * total_voters)/sum(total_voters),
  age65andolder_pct = sum(age65andolder_pct * total_voters)/sum(total_voters),
  average_median_hh_inc = sum(average_median_hh_inc * total_voters)/sum(total_voters),
  clf_unemploy_pct = sum(clf_unemploy_pct * total_voters)/sum(total_voters),
  lesshs_pct = sum(lesshs_pct * total_voters)/sum(total_voters),
  lesscollege_pct = sum(lesscollege_pct * total_voters)/sum(total_voters),
  lesshs_whites_pct = sum(lesshs_whites_pct * total_voters)/sum(total_voters),
  lesscollege_whites_pct = sum(lesscollege_whites_pct * total_voters)/sum(total_voters),
  rural_pct = sum(rural_pct * total_voters)/sum(total_voters)
)

#Logistic Regression to see the weights/coefficients of each variable in the election
# 1 = Republican and 0 = Democrat
logitreg <- logistf(winner ~ white_pct + black_pct + hispanic_pct + nonwhite_pct + foreignborn_pct +
                                 female_pct + age29andunder_pct + age65andolder_pct + average_median_hh_inc +
                                 clf_unemploy_pct + lesshs_pct + lesscollege_pct + lesshs_whites_pct + 
                                 lesscollege_whites_pct + rural_pct, data = State_Data, family = "binomial")
summary(logitreg)
logitreg_coef <- logitreg$coefficients
logitreg_coef

#Normalized the coefficients on a scale from 0 to 1, which serves as the rank of each of the variables
normalized_logitreg_coef <- (logitreg_coef-min(logitreg_coef))/ (max(logitreg_coef) - min(logitreg_coef))
normalized_logitreg_coef <- normalized_logitreg_coef[-1]
normalized_logitreg_coef

#helper function that calculates percent diff
percent_diff <- function(n1, n2){
  p_d <- (abs(n1 - n2) / ((n1 + n2) /2)) * 100
  return(p_d)
}

#Calculating percent difference for each of the variables between each state and the entire country
pre_data <-State_Data %>% 
  mutate(
    white_pct_diff = percent_diff(white_pct, US_Data$white_pct),
    black_pct_diff = percent_diff(black_pct, US_Data$black_pct),
    hispanic_pct_diff = percent_diff(hispanic_pct, US_Data$hispanic_pct),
    nonwhite_pct_diff = percent_diff(nonwhite_pct, US_Data$nonwhite_pct),
    foreignborn_pct_diff = percent_diff(foreignborn_pct, US_Data$foreignborn_pct),
    female_pct_diff = percent_diff(female_pct, US_Data$female_pct),
    age29andunder_pct_diff = percent_diff(age29andunder_pct, US_Data$age29andunder_pct),
    age65andolder_pct_diff = percent_diff(age65andolder_pct, US_Data$age65andolder_pct),
    average_median_hh_inc_diff = percent_diff(average_median_hh_inc, US_Data$average_median_hh_inc),
    clf_unemploy_pct_diff = percent_diff(clf_unemploy_pct, US_Data$clf_unemploy_pct),
    lesshs_pct_diff = percent_diff(lesshs_pct, US_Data$lesshs_pct),
    lesscollege_pct_diff = percent_diff(lesscollege_pct, US_Data$lesscollege_pct),
    lesshs_whites_pct_diff = percent_diff(lesshs_whites_pct, US_Data$lesshs_whites_pct),
    lesscollege_whites_pct_diff = percent_diff(lesscollege_whites_pct, US_Data$lesscollege_whites_pct),
    rural_pct_diff = percent_diff(rural_pct, US_Data$rural_pct)) 

#selecting the percent difference for every variable for all the states
percent_diff <- pre_data %>%
  select(state, white_pct_diff, black_pct_diff, hispanic_pct_diff, nonwhite_pct_diff,
         foreignborn_pct_diff, female_pct_diff, age29andunder_pct_diff, age65andolder_pct_diff,
         average_median_hh_inc_diff, clf_unemploy_pct_diff, lesshs_pct_diff, lesscollege_pct_diff,
         lesshs_whites_pct_diff, lesscollege_whites_pct_diff, rural_pct_diff)

#Calculating the score S (which is the weighted average of the percent diff) 
weightedMean <- rowWeightedMeans(data.matrix(percent_diff[-1]), normalized_logitreg_coef)

#Making a new data frame that only contains the state and their score S
state_score <- percent_diff %>% mutate(score = weightedMean) %>% select(state, score) %>% arrange(score)

#Bar plot of the state and their respective scores
state_score %>% arrange(score) %>% ggplot(aes(x =reorder(state, score), y = score)) + geom_bar(stat = "identity") +
  coord_flip() + labs(x = "State")


