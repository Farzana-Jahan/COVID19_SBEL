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
head(data_Europe)

# looking at first 6 rows o f the data
