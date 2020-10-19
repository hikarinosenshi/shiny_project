# This file was used to preprocess the data files used in this project.
library(tidyverse)
library(lubridate)
library(scales)
#library(leaflet)
#library(maps)
#library(sp)
#library(rgdal)
############################################################
# 1. Covid 19 Cases by Date in Massachusetts
# source https://www.mass.gov/info-details/covid-19-response-reporting
cases_by_date <- read.csv(file = './data/covid-19-dashboard-10-13-2020/CasesByDate.csv')
str(cases_by_date)

class(cases_by_date[,1])

month(mdy("1/29/2020"),label = T)

format(12345,big.mark = ',')
sprintf()

new_cases_by_month <- cases_by_date %>%
  mutate(month = month(mdy(Date), label = T)) %>% 
  group_by(month) %>% 
  summarise(total.cases = sum(Positive.New))

new_cases_by_month %>% 
  ggplot(aes(x = month, y = total.cases)) +
  geom_bar(stat = 'identity', color = 'blue', fill = 'blue') +
  labs(title = 'Reported COVID-19 Cases in the State of Massachusetts in 2020', x = 'Month', y = 'Number of Cases') +
  geom_text(aes(label=format(total.cases,big.mark = ',')), position=position_dodge(width=0.9), vjust=-0.25) +
  scale_y_continuous(labels = comma)

write_csv(x = new_cases_by_month,path = './CasesByDate.csv')
############################################################
# Loading listings from Jan to August
filenames <- list.files(path = './data', pattern = '^2019.*csv$', full.names = T)

listings <- sapply(filenames, read.csv, simplify = F, USE.NAMES = T)

names(listings) <- ymd(gsub(x = gsub(x = names(listings),pattern = './data/',replacement = ''), pattern = '_listings.csv', replacement = ''))

listings

str(listings[[1]])

listings2020 <- bind_rows(listings, .id = 'last_scrape_date')
listings2019 <- bind_rows(listings, .id = 'last_scrape_date')
str(listings2020)
str(listings2019)
listings2020[1:10,]
listings2019[1:10,]

listings

############################################################
# Number of listings by last scrape date
# 2020
listings_2020 <- listings2020 %>% 
  group_by(last_scrape_date) %>% 
  summarise(total_listings = n())

listings_2020 %>%
  ggplot(aes(x = last_scrape_date, y = total_listings)) +
  geom_line(aes(group = 1), color = 'red') +
  labs(title = 'Number of Listings in the city of Boston', x = 'Date data was gathered', y = 'Number of Listings')+
  scale_y_continuous(labels = comma)

write_csv(x = listings_2020, path = './www/listings_2020.csv')
# 2019
listings_2019 <- listings2019 %>% 
  group_by(last_scrape_date) %>% 
  summarise(total_listings = n())
par(mfrow=c(1,2))
listings_2019 %>%
  ggplot(aes(x = last_scrape_date, y = total_listings)) +
  geom_line(aes(group = 1), color = 'red') +
  labs(title = 'Number of Listings in the city of Boston', x = 'Date data was gathered', y = 'Number of Listings')+
  scale_y_continuous(labels = comma)
listings_2020 %>%
  ggplot(aes(x = last_scrape_date, y = total_listings)) +
  geom_line(aes(group = 1), color = 'red') +
  labs(title = 'Number of Listings in the city of Boston', x = 'Date data was gathered', y = 'Number of Listings')+
  scale_y_continuous(labels = comma)

write_csv(x = listings_2019, path = './listings_2019.csv')
############################################################
# number of reviews by last scrape date
reviews_2020 <- listings2020 %>% 
  group_by(last_scrape_date) %>% 
  summarise(total_reviews_count = sum(number_of_reviews))

reviews_2020 %>% 
  ggplot(aes(x = last_scrape_date, y = total_reviews_count)) +
  geom_line(aes(group = 1), color = 'blue') +
  labs(title = 'Total Number of Reviews', x = 'Date data was gathered', y = 'Number of Reviews')+
  scale_y_continuous(labels = comma)

write_csv(x = reviews_2020, path = './www/reviews_2020.csv')
############################################################
# make sure to only include listings that were present from Jan - Aug
listings_present_2020 <- listings2020 %>%
  group_by(id) %>% 
  summarise(n = n()) %>% 
  filter(n > 7) %>% 
  select(id)
# number of reviews by last scrape date
reviews2_2020 <-listings2020 %>%
  filter(id %in% listings_present_2020$id) %>% 
  group_by(last_scrape_date) %>% 
  summarise(total_reviews_count = sum(number_of_reviews))

reviews2_2020 %>% 
  ggplot(aes(x = last_scrape_date, y = total_reviews_count)) +
  geom_line(aes(group = 1), color = 'blue') +
  labs(title = 'Total Number of Reviews', x = 'Date data was gathered', y = 'Number of Reviews')+
  scale_y_continuous(labels = comma)

write_csv(x = reviews2_2020, path = './www/reviews2_2020.csv')
############################################################
# number of new reviewsreviews
newreviews_2020 <- listings2020 %>%
  filter(id %in% listings_present_2020$id) %>% 
  group_by(last_scrape_date) %>% 
  summarise(total_reviews_count = sum(number_of_reviews)) %>%
  mutate(new_reviews = total_reviews_count - first(total_reviews_count)) %>% 
  mutate(new_reviews2 = (c(new_reviews[1:2],new_reviews[3:8] - nth(new_reviews,2)))) %>% 
  mutate(new_reviews3 = (c(new_reviews2[1:3],new_reviews2[4:8] - nth(new_reviews2,3)))) %>% 
  mutate(new_reviews4 = (c(new_reviews3[1:4],new_reviews3[5:8] - nth(new_reviews3,4)))) %>% 
  mutate(new_reviews5 = (c(new_reviews4[1:5],new_reviews4[6:8] - nth(new_reviews4,5)))) %>% 
  mutate(new_reviews6 = (c(new_reviews5[1:6],new_reviews5[7:8] - nth(new_reviews5,6)))) %>% 
  mutate(new_reviews7 = (c(new_reviews6[1:7],new_reviews6[8:8] - nth(new_reviews6,7)))) %>% 
  mutate(new_reviews = new_reviews7) %>% 
  slice(2:n()) %>% 
  select(last_scrape_date, new_reviews)


newreviews_2020 %>% 
  ggplot(aes(x = last_scrape_date, y = new_reviews)) +
  geom_line(aes(group = 1), color = 'blue') +
  labs(title = 'Total Number of New Reviews', x = 'Date data was gathered', y = 'Number of Reviews')+
  scale_y_continuous(labels = comma)

write_csv(x = newreviews_2020, path = './www/newreviews_2020.csv')

#2019
listings_present_2019 <- listings2019 %>%
  group_by(id) %>% 
  summarise(n = n()) %>% 
  filter(n > 7) %>% 
  select(id)


newreviews_2019 <- listings2019 %>%
  filter(id %in% listings_present_2019$id) %>% 
  group_by(last_scrape_date) %>% 
  summarise(total_reviews_count = sum(number_of_reviews)) %>%
  slice(1:8) %>% 
  mutate(new_reviews = total_reviews_count - first(total_reviews_count)) %>% 
  mutate(new_reviews2 = (c(new_reviews[1:2],new_reviews[3:8] - nth(new_reviews,2)))) %>% 
  mutate(new_reviews3 = (c(new_reviews2[1:3],new_reviews2[4:8] - nth(new_reviews2,3)))) %>% 
  mutate(new_reviews4 = (c(new_reviews3[1:4],new_reviews3[5:8] - nth(new_reviews3,4)))) %>% 
  mutate(new_reviews5 = (c(new_reviews4[1:5],new_reviews4[6:8] - nth(new_reviews4,5)))) %>% 
  mutate(new_reviews6 = (c(new_reviews5[1:6],new_reviews5[7:8] - nth(new_reviews5,6)))) %>% 
  mutate(new_reviews7 = (c(new_reviews6[1:7],new_reviews6[8:8] - nth(new_reviews6,7)))) %>% 
  mutate(new_reviews = new_reviews7) %>% 
  slice(2:n()) %>% 
  select(last_scrape_date, new_reviews)

newreviews_2019 %>% 
  ggplot(aes(x = last_scrape_date, y = new_reviews)) +
  geom_line(aes(group = 1), color = 'blue') +
  labs(title = 'Total Number of New Reviews', x = 'Date data was gathered', y = 'Number of Reviews')+
  scale_y_continuous(labels = comma)

write_csv(x = newreviews_2019, path = './newreviews_2019.csv')
