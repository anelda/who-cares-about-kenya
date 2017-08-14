###
# This script was originally downloaded from the tutorial: http://www.interhacktives.com/2017/01/25/scrape-tweets-r-journalists/
# Many modifications has been made to it from a variety of help documents and fora Q&A specifically from https://stackoverflow.com/
# Tutorial for displaying two datasets: https://allthisblog.wordpress.com/2016/10/12/r-311-with-leaflet-tutorial/
# Tutorial for grouping markers: https://rstudio.github.io/leaflet/markers.html
### 

#0) Do this first bit the first time, but only need to do once to install#
#install.packages("stringr")
#install.packages("twitteR")
#install.packages("purrr")
#install.packages("tidytext")
#install.packages("dplyr")
#install.packages("tidyr")
#install.packages("lubridate")
#install.packages("scales")
#install.packages("broom")
#install.packages("ggplot2")
#install.packages("base64enc") # added based on error received
#install.packages("dismo") # from https://rizkashifs.wordpress.com/2016/09/17/twitter-data-exploration-using-r/
#install.packages("maps") # from https://rizkashifs.wordpress.com/2016/09/17/twitter-data-exploration-using-r/
#install.packages("leaflet") # from https://opensource.com/article/17/6/collecting-and-mapping-twitter-data-using-r
#install.packages("googlesheets")
install.packages("countrycode")

#1) Do this whenever you need to start a session#
library(stringr)
library(twitteR)
library(purrr)
library(tidytext)
library(dplyr)
library(tidyr)
library(lubridate)
library(scales)
library(broom)
library(ggplot2)
library(base64enc) #added based on error received
library (maps) # from https://opensource.com/article/17/6/collecting-and-mapping-twitter-data-using-r
library (dismo) # from https://opensource.com/article/17/6/collecting-and-mapping-twitter-data-using-r
library(leaflet) # from https://opensource.com/article/17/6/collecting-and-mapping-twitter-data-using-r
library(googlesheets)
library(countrycode)

# 2) Access Google sheet with data

kenya_doc <- gs_title("KenyaPresident")

kenya_sheet <- gs_read(kenya_doc, ws = "Copy of #KenyaPresident", verbose = TRUE)

kenya_df <- as.data.frame(kenya_sheet)

# Create new columns containing latitude and longitude estimated from "Location" column
# Allow only one result per location even if more are found

kenya_locations <- geocode(kenya_df$Location, oneRecord = TRUE)

# write results from geocoding to file so that I don't have to run that over and over (time consuming and relies on internet access)

write.csv(kenya_locations, file = "kenya_tweet_geocoding.csv")

# Add columns to the dataframe containing the longitude and latitude as well as interpreted country name and 3-letter ISO code

kenya_locations$interpretedPlace <- as.character(kenya_locations$interpretedPlace)

kenya_df$long <- kenya_locations$longitude
kenya_df$lat <- kenya_locations$latitude
kenya_df$interpreted_place <- kenya_locations$interpretedPlace
kenya_df$interpreted_country <- sub('.*,\\s*', '', kenya_df$interpreted_place)
kenya_df$interpreted_country <- str_to_upper(kenya_df$interpreted_country, locale = "en")
kenya_df$interpreted_country_code <- countrycode(kenya_df$interpreted_country, "country.name", "iso3c")


# Draw map using coordinates and leaflet package

kenya_map <- leaflet(kenya_df) %>% addTiles() 

kenya_map %>% addAwesomeMarkers(lng = ~long, lat = ~lat, layerId = "entr",
                             clusterOptions = markerClusterOptions(zoomToBoundsOnClick = TRUE), 
                             clusterId = NULL) 

# Read in list of countries with relationships to Kenya

foreign_doc <- gs_title("Kenya-ForeignRelationships")

foreign_sheet <- gs_read(foreign_doc, ws = "AllCountries", verbose = TRUE)

foreign_df <- as.data.frame(foreign_sheet)

# Categorise countries as either with or without foreign relationships to Kenya

kenya_new <- kenya_df

kenya_new <- rename(kenya_new, Handle = ScreenName)
handle_country_df <- kenya_new %>% distinct(Handle, 
              interpreted_country_code)

kenya_doc <- kenya_doc %>% 
  gs_ws_new(ws_title = "Twitter Data with ScreenName + Country Code", input = handle_country_df, 
            verbose = TRUE, trim=TRUE)

write.csv(handle_country_df, file = "handle_country.csv")

# Write new sheet containing three new columns to original Google Sheet

kenya_doc <- kenya_doc %>% 
  gs_ws_new(ws_title = "Twitter Data with coordinates and country", input = kenya_df, 
            verbose = TRUE, trim=TRUE)


str(countrycode_data)

iso
