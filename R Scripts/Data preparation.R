library(tidyverse)
# read the data from local directory
data<- read_csv("data/owid-covid-data.csv")
data_Europe<-filter(data,data$continent=="Europe")
dim(data_Europe)
names(data_Europe)

summary(data_Europe$date)
data_Europe<-data_Europe%>%
  dplyr::mutate(year = lubridate::year(date),
                month = lubridate::month(date),
                day = lubridate::day(date))
head(data_Europe) # looking at first 6 rows of the data
write_csv(data_Europe,file="data/Europe_covid.csv")
# creating quarterly data for modelling

summary(data_Europe$month)
summary(data_Europe$gdp_per_capita)

# replacing missing values with 0
data_Europe$new_cases_per_million[is.na(data_Europe$new_cases_per_million)]<-0
data_Europe$new_deaths_per_million[is.na(data_Europe$new_deaths_per_million)]<-0

data_Europe<-read_csv("data/Europe_covid.csv")
data_Europe_Q1<-filter(data_Europe,year==2020,month==1:4)

new_cases_per_million<-c()
new_deaths_per_million<- c()
gdp_per_capita<-c()
aged_65_older<-c()
hospital_beds_per_thousand<-c()
population_density<-c()
for(i in 1:length(unique(data_Europe_Q1$location)))
{
  new_cases_per_million[i]<- sum(data_Europe_Q1$new_cases_per_million[data_Europe_Q1$location==
                                                                    unique(data_Europe_Q1$location)[i]])
  new_deaths_per_million[i]<- sum(data_Europe_Q1$new_deaths_per_million[data_Europe_Q1$location==
                                                                          unique(data_Europe_Q1$location)[i]])
  gdp_per_capita[i]<-mean(data_Europe_Q1$gdp_per_capita[data_Europe_Q1$location==
                                                          unique(data_Europe_Q1$location)[i]])
  aged_65_older[i]<-mean(data_Europe_Q1$aged_65_older[data_Europe_Q1$location==
                                                        unique(data_Europe_Q1$location)[i]])
  hospital_beds_per_thousand[i]<-mean(data_Europe_Q1$hospital_beds_per_thousand[data_Europe_Q1$location==
                                                                                  unique(data_Europe_Q1$location)[i]])
  population_density[i]<-mean(data_Europe_Q1$population_density[data_Europe_Q1$location==
                                                                  unique(data_Europe_Q1$location)[i]])
}

data_Europe_Q1_2020<-data.frame(location=unique(data_Europe_Q1$location),
                               new_cases_per_million,
                               new_deaths_per_million,
                               gdp_per_capita,
                               aged_65_older,
                               hospital_beds_per_thousand,
                               population_density)
head(data_Europe_Q1_2020)




data_Europe_Q2<-filter(data_Europe,year==2020,month==5:8)

new_cases_per_million<-c()
new_deaths_per_million<- c()
new_tests_per_thousand<-c()
total_tests<-c()
gdp_per_capita<-c()
aged_65_older<-c()
hospital_beds_per_thousand<-c()
population_density<-c()
for(i in 1:length(unique(data_Europe_Q2$location)))
{
  new_cases_per_million[i]<- sum(data_Europe_Q2$new_cases_per_million[data_Europe_Q2$location==
                                                                        unique(data_Europe_Q2$location)[i]])
  new_deaths_per_million[i]<- sum(data_Europe_Q2$new_deaths_per_million[data_Europe_Q2$location==
                                                                          unique(data_Europe_Q2$location)[i]])
  new_tests_per_thousand[i]<-sum(data_Europe_Q2$new_tests_per_thousand[data_Europe_Q2$location==
                                                                         unique(data_Europe_Q2$location)[i]])
  total_tests[i]<-sum(data_Europe_Q2$total_tests[data_Europe_Q2$location==
                                               unique(data_Europe_Q2$location)[i]])
  gdp_per_capita[i]<-mean(data_Europe_Q2$gdp_per_capita[data_Europe_Q2$location==
                                                          unique(data_Europe_Q2$location)[i]])
  aged_65_older[i]<-mean(data_Europe_Q2$aged_65_older[data_Europe_Q2$location==
                                                        unique(data_Europe_Q2$location)[i]])
  hospital_beds_per_thousand[i]<-mean(data_Europe_Q2$hospital_beds_per_thousand[data_Europe_Q2$location==
                                                                                  unique(data_Europe_Q2$location)[i]])
  population_density[i]<-mean(data_Europe_Q2$population_density[data_Europe_Q2$location==
                                                                  unique(data_Europe_Q2$location)[i]])
}

data_Europe_Q2_2020<-data.frame(location=unique(data_Europe_Q2$location),
                                new_cases_per_million,
                                new_deaths_per_million,
                                new_tests_per_thousand,
                                new_tests,
                                gdp_per_capita,
                                aged_65_older,
                                hospital_beds_per_thousand,
                                population_density)

data_Europe[data_Europe$location=="Czechia",]

# accumulating test information
test<-read_csv("data/owid-covid-data-testing.csv")
test_Europe<-filter(test,test$continent=="Europe")
dim(test_Europe)
names(test_Europe)

summary(test_Europe$date)
test_Europe<-test_Europe%>%
  dplyr::mutate(year = lubridate::year(date),
                month = lubridate::month(date),
                day = lubridate::day(date))
head(test_Europe) # looking at first 6 rows of the test
write_csv(test_Europe,file="data/Europe_covid-test.csv")
# creating quarterly test for modelling

summary(test_Europe$month)
summary(test_Europe$gdp_per_capita)

# replacing missing values with 0


test_Europe_Q1<-filter(test_Europe,year==2020,month==1:4)

test_Europe_Q1$new_tests_per_thousand[is.na(test_Europe_Q1$new_tests_per_thousand)]<-0
test_Europe_Q1$new_tests[is.na(test_Europe_Q1$new_tests)]<-0
new_tests<-c()
new_tests_per_thousand<-c()

for(i in 1:length(unique(test_Europe_Q1$location)))
{

  new_tests_per_thousand[i]<-sum(test_Europe_Q1$new_tests_per_thousand[test_Europe_Q1$location==
                                                                         unique(test_Europe_Q1$location)[i]])
  new_tests[i]<-sum(test_Europe_Q1$new_tests[test_Europe_Q1$location==
                                               unique(test_Europe_Q1$location)[i]])
}

test_Europe_Q1_2020<-data.frame(location=unique(test_Europe_Q1$location),
                                new_tests_per_thousand)



# Analysis 1, Quarter 1, Europe, new cases vs aged and pop density
library(dplyr)
Europe_Covid_test_aged_anal1_data<-inner_join(data_Europe_Q1_2020,test_Europe_Q1_2020,by="location")

# excluding rows with missing values
Europe_Covid_test_aged_anal1_data<-Europe_Covid_test_aged_anal1_data[-46,]
write_csv(Europe_Covid_test_aged_anal1_data,file="data/Europe_COVID_1")
