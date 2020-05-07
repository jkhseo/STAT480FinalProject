### Data and Packages
Election <- read.csv("https://raw.githubusercontent.com/MEDSL/2018-elections-unoffical/master/election-context-2018.csv")
library(caret)
library(leaps)
library(MASS)
library(tidyverse)
library(maps)
library(RColorBrewer)

County <- map_data("county")
County$state <- County$region
County$county <- County$subregion
County <- County %>% dplyr::select(long:order, state:county)

Election$state <- tolower(Election$state)
Election$county <- tolower(Election$county)

### State Demographic Variable Manipulation

Election <- Election %>% mutate(white_pop= total_population*white_pct) %>% 
  mutate(black_pop= total_population*black_pct) %>% 
  mutate(hispanic_pop = total_population*hispanic_pct) %>% 
  mutate(nonwhite_pop = total_population*nonwhite_pct) %>% 
  mutate(foreignborn_pop= total_population*foreignborn_pct) %>% 
  mutate(female_pop = total_population*female_pct) %>% 
  mutate(age29andunder_pop = age29andunder_pct*total_population) %>% 
  mutate(age65andolder_pop = total_population*age65andolder_pct) %>% 
  mutate(clf_unemploy_pop = total_population*clf_unemploy_pct) %>% 
  mutate(lesshs_pop = total_population*lesshs_pct) %>% 
  mutate(lesscollege_pop = total_population*lesscollege_pct) %>% 
  mutate(lesshs_whites_pop = total_population*lesshs_whites_pct) %>% 
  mutate(lesscollege_whites_pop= total_population*lesscollege_whites_pct) %>% 
  mutate(rural_pop= total_population*rural_pct) 

StateDem <- Election %>% group_by(state) %>% 
  summarise(white_pop=sum(white_pop), 
  black_pop=sum(black_pop),
  hispanic_pop=sum(hispanic_pop),
  nonwhite_pop=sum(nonwhite_pop),
  foreignborn_pop=sum(foreignborn_pop), 
  female_pop=sum(female_pop), 
  age29andunder_pop=sum(age29andunder_pop), 
  age65andolder_pop=sum(age65andolder_pop), 
  clf_unemploy_pop=sum(clf_unemploy_pop), 
  lesshs_pop=sum(lesshs_pop), 
  lesscollege_pop=sum(lesscollege_pop), 
  lesshs_whites_pop=sum(lesshs_whites_pop),
  lesscollege_whites_pop=sum(lesscollege_whites_pop), 
  rural_pop=sum(rural_pop),
  otherpres12=sum(otherpres12),
  otherpres16=sum(otherpres16))

StateDemPercent <- Election %>% drop_na(total_population) %>% 
  group_by(state) %>% 
  summarise(black_pct= sum(black_pop)/sum(total_population), 
  white_pct= sum(white_pop)/sum(total_population), 
  hispanic_pct=sum(hispanic_pop)/sum(total_population), 
  nonwhite_pct=sum(nonwhite_pop)/sum(total_population),
  foreignborn_pct=sum(foreignborn_pop)/sum(total_population),
  female_pct=sum(female_pop)/sum(total_population),
  age29andunder_pct=sum(age29andunder_pop)/sum(total_population),
  age65andolder_pct=sum(age65andolder_pop)/sum(total_population),
  clf_unemploy_pct=sum(clf_unemploy_pop)/sum(total_population),
  lesshs_pct=sum(lesshs_pop)/sum(total_population), 
  lesscollege_pct=sum(lesscollege_pop)/sum(total_population),
  lesshs_whites_pct=sum(lesshs_whites_pop)/sum(total_population),
  lesscollege_whites_pct=sum(lesscollege_whites_pop)/sum(total_population), 
  rural_pct=sum(rural_pop)/sum(total_population), 
  otherpres12=sum(otherpres12)/sum(total_population) *100,
  otherpres16=sum(otherpres16)/sum(total_population)*100)






### Map of Third Party Votes 2012


ThirdMap2012 <- Election %>% dplyr::select(state:fips, otherpres12, total_population) %>% 
  drop_na() %>% group_by(state) %>% 
  summarise(PercentThird = sum(otherpres12)/sum(total_population) *100) 

ThirdMap2012 %>% arrange(desc(PercentThird)) %>% head(10)

ThirdMap2012 <- ThirdMap2012 %>% left_join(County, by="state")



ggplot(ThirdMap2012, aes(x=long, y= lat, fill=PercentThird)) + geom_polygon(aes(group=group)) + scale_fill_gradient2(low= "red", mid="white" , high="blue", guide= "colourbar") +
  labs(fill= "Percent Third Party")

### Third Party Model for 2012

model2012 <- StateDemPercent %>% dplyr::select(black_pct:otherpres12) 

full.model2012 <- lm(otherpres12 ~., data=model2012)

summary(full.model2012)

step.model2012  <- stepAIC(full.model2012, direction="both", trace=FALSE)

summary(step.model2012)


plot(step.model2012)

### Which States are the Best 2012

BestModel2012 <- StateDemPercent %>% 
  mutate(Prediction = 9.56 +(.012 * white_pct)+
  (.035*foreignborn_pct)+(-.017*lesscollege_whites_pct)+
  (.011*rural_pct)+ (-.179 * female_pct)) %>% dplyr::select(state, Prediction) %>% 
  dplyr::arrange(desc(Prediction))

BestModel2012Map <- BestModel2012 %>% left_join(County, by="state")

ggplot(BestModel2012Map, aes(x=long, y= lat, fill=Prediction)) + geom_polygon(aes(group=group)) + scale_fill_gradient2(low= "red", mid="white" , high="blue", guide="colourbar") + 
  labs(fill= "Percent Third Party")

### How Good is 2012 Model?
Test2012 <- Election %>% dplyr::select(state:fips, otherpres12, total_population) %>% 
  drop_na() %>% group_by(state) %>% 
  summarise(PercentThird = sum(otherpres12)/sum(total_population) *100) 


Test2012 <- Test2012 %>% left_join(BestModel2012, by="state")

Test2012 <- Test2012 %>% mutate(Error = abs(PercentThird-Prediction)/Prediction*100)

### Map of Third Pary Votes 2016 

ThirdMap2016 <- Election %>% select(state:fips, otherpres16, total_population) %>% 
  drop_na() %>% group_by(state) %>% 
  summarise(PercentThird = sum(otherpres16)/sum(total_population) * 100)

ThirdMap2016 %>% arrange(desc(PercentThird)) %>% head(10)


ThirdMap2016 <- ThirdMap2016 %>% left_join(County, by="state")

ggplot(ThirdMap2016, aes(x=long, y= lat, fill=PercentThird)) + geom_polygon(aes(group=group)) + scale_fill_gradient2(low= "red", mid="white" , high="blue", guide="colourbar") + 
  labs(fill= "Percent Third Party")

### Third Party Model for 2016

model2016 <- StateDemPercent %>% dplyr::select(black_pct:rural_pct, otherpres16) 

full.model2016 <- lm(otherpres16 ~., data=model2016)

summary(full.model2016)

step.model2016  <- stepAIC(full.model2016, direction="both", trace=FALSE)

summary(step.model2016)

plot(step.model2016)

### Best States for 2016 Model

BestModel2016 <- StateDemPercent %>% mutate(Prediction = -8.91 + 
  (-.098*black_pct) + (.454*age29andunder_pct)+ 
  (.534*clf_unemploy_pct)+ (-.293*lesshs_pct)+
  (-.093*lesscollege_pct)+ (.048*rural_pct)) %>%  
  dplyr::select(state, Prediction) %>% arrange(desc(Prediction))

BestModel2016Map <- BestModel2016 %>% left_join(County, by="state")

ggplot(BestModel2016Map, aes(x=long, y= lat, fill=Prediction)) + geom_polygon(aes(group=group)) + scale_fill_gradient2(low= "red", mid="white" , high="blue", guide="colourbar") + 
  labs(fill= "Percent Third Party")

### How Good is the 2016 Model
Test2016 <- Election %>% dplyr::select(state:fips, otherpres16, total_population) %>% 
  drop_na() %>% group_by(state) %>% 
  summarise(PercentThird = sum(otherpres16)/sum(total_population) *100) 


Test2016 <- Test2016 %>% left_join(BestModel2016, by="state")

Test2016 <- Test2016 %>% mutate(Error = abs(PercentThird-Prediction)/Prediction*100)
