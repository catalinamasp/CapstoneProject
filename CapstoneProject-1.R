library(httr)
library(jsonlite)
library(tidyverse)
library(ckanr)
library(rvest)
library(RCurl)


#Roadmap

#1. SELECT INFORMATION FROM BUENOS AIRES, MILAN AND LONDON
#2. ORGANIZE & TIDY UP THE INFORMATION
#3. TRANSLATE THE INFORMATION? 
#4. GROUP CRIMES BY CATEGORIES & COMAPRE --> VISUALIZATIONS
#5. COMPARE CRIMES IN TIME FRAMES  --> VISUALIZATIONS
#6. CRIME RATE / INHABITANTS
#7. CONCLUSIONS

#DOWNLOAD CRIMES IN BUENOS AIRES

url <- paste0("https://data.buenosaires.gob.ar/api/3/action/",
              "package_show?",
              "id=delitos")

page <- GET(url) # API request
status_code(page) # # Check that the call is successful

datalist <- fromJSON(url) 
data <- datalist$result$resources$url
(data) 

datasets <- as.vector(unlist(data))
(datasets)

Security_BA<- read_csv(data[[4]]) 

download.file("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/ministerio-de-justicia-y-seguridad/delitos/delitos_2019.csv", "Baires.csv")

Security_BA <- read_csv("Baires.csv")

#SEE PROBLEMS

Problems <- problems(Security_BA)

Security_BA
Security_BA%>% 
  as.data.frame(Security_BA)
head(Security_BA)



#to run till line 52

#TIMEFRAME TO REVIEW
Security_BA %>% 
  group_by(franja_horaria) %>% 
  count()


#Date format --> data belongs to 2019, therefore the column year is nonsense

library(lubridate)

is.character(Security_BA$fecha)


Security_BA$fecha <- as.Date.character(Security_BA$fecha, format = c("%d-%m-%Y"))

Security_BA <- Security_BA %>%
  separate(fecha, sep="-", into = c("year", "month", "day"))

is.character(Security_BA$month) 


Security_BA$year <- year(ymd(Security_BA$fecha))
Security_BA$month <- month(ymd(Security_BA$fecha)) 
Security_BA$day <- day(ymd(Security_BA$fecha))

Security_BA

#Visualization 1 | Distributi

library(ggplot2)

Plot_Distribution <-  Security_BA %>%
  group_by(month, comuna, barrio) %>% 
  count()

Plot_Distribution

Plot_Distribution %>% 
  ggplot(aes(x = month, y = n, fill = comuna, color = comuna))+
  geom_col(position="dodge")+
  facet_wrap(~ comuna)+
  theme_bw()

#maybe it makes more sense in a boxplot?

Plot_Distribution %>% 
  ggplot(aes(x = month, y = n, fill = comuna, color = comuna))+
  geom_boxplot()
theme_bw()


#NUMBER OF CRIMES X COMUNA 

Security_BA %>% 
  group_by(comuna, barrio) %>%  view()

#frequency by time frame

Distribution_TimeFrame <-  Security_BA %>%
  group_by(franja_horaria, comuna, barrio) %>% 
  count()
Distribution_TimeFrame %>% 
  ggplot(aes(x = comuna, y = n, fill = barrio, color = barrio))+
  geom_col(position="dodge")+
  facet_wrap(~ franja_horaria)+
  theme_bw()



# MILANO

url2 <- ("https://dati.comune.milano.it/api/3/action/datastore_search?q=2019&resource_id=8b03b9f2-f2d7-4408-b439-bc6efc093cff")

page2 <- GET(url2) # API request
status_code(page2) # # Check that the call is successful

datalist <- fromJSON(url2) 
Milano_2019 <- datalist$result$records
view(Milano_2019) 
as.data.frame(Milano_2019)



#LONDON

install.packages("ukpolice")
library("ukpolice")

Ukdata <- ukpolice::ukc_available() 

df <- ukc_forces()
df
ukc_forces
Ukdata

Ukdata %>% 
  ifelse(grepl("2019", Ukdata$date), "inside")

library(lubridate)

Crime <- Ukdata 

LondonCrimes <- ukc_crime_no_location(force = "city-of-london", "metropolitan") 
LondonCrimes

LondonCrimes$month <- ifelse(grepl("2019",LondonCrimes$month),'inside','outside')

LondonCrimes$month

LondonCrimes <- LondonCrimes 
view(LondonCrimes)
separate(LondonCrimes$month, c("Year", "Month"), sep = "-")

as.Date(LondonCrimes$month) 

as.Date.character(LondonCrimes$month)

install.packages("readr")
library(readr)
library(d
        as.numeric(LondonCrimes$month) %>% 
          separate(LondonCrimes$month, c("Year", "Month"), sep = "-")
        
        
        
        ukc_date_processing <- function(2019-01) {
          if (is.null(2019-01)) {
            date_query <- NULL
          } else {
            if (nchar(as.character((2019-01)) > 7) {
              date <- substr(as.Date((2019-01), 1, 7)
            }
            
            date_query <- paste0("&date=", date)
          }
          
          date_query
        }
        
        
        #NewYork
        
        url4 <- ("https://data.cityofnewyork.us/Public-Safety/NYPD-Complaint-Data-Historic/qgea-i56i")
        
        https://opendata.cityofnewyork.us/how-to/
          
          https://www1.nyc.gov/site/nypd/stats/crime-statistics/citywide-crime-stats.page
        https://data.cityofnewyork.us/Public-Safety/NYPD-Complaint-Data-Historic/qgea-i56i
        
        NYC <- datalist$result$records
        view(Milano_2019) 
        as.data.frame(Milano_2019)
        
        
        
        