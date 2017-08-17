###
# This script was originally downloaded from the tutorial: http://www.interhacktives.com/2017/01/25/scrape-tweets-r-journalists/
# Many modifications has been made to it from a variety of help documents and fora Q&A specifically from https://stackoverflow.com/
# Tutorial for displaying two datasets: https://allthisblog.wordpress.com/2016/10/12/r-311-with-leaflet-tutorial/
# Tutorial for grouping markers: https://rstudio.github.io/leaflet/markers.html
# Tufte in R http://motioninsocial.com/tufte/#dot-dash-plot
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
install.packages("leaflet") # from https://opensource.com/article/17/6/collecting-and-mapping-twitter-data-using-r
#install.packages("googlesheets")
install.packages("countrycode")
install.packages("ggthemes")
install.packages("lattice")

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
library(leaflet) # from https://opensource.com/article/17/6/collecting-and-mapping-twitter-data-using-r
library(googlesheets)
library(countrycode)
library(ggthemes)
library(lattice)


# 2) Access Google sheet with data

kenya_doc <- gs_title("KenyaPresident")

kenya_sheet <- gs_read(kenya_doc, ws = "Copy of #KenyaPresident", verbose = TRUE)

kenya_df <- as.data.frame(kenya_sheet)

# Create new columns containing latitude and longitude estimated from "Location" column
# Allow only one result per location even if more are found

kenya_locations <- geocode(kenya_df$Location, oneRecord = TRUE)

# write results from geocoding to file so that I don't have to run that over and over (time consuming and relies on internet access)

write.csv(kenya_locations, file = "kenya_tweet_geocoding.csv")

# Add columns to the dataframe containing the longitude and latitude as well as 
# interpreted country name and 3-letter ISO code

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

# Draw map showing different colours for countries with relationship with Kenya and those without

kenya_rel <- gs_title("KenyaPresident")

kenya_rel_sheet <- gs_read(kenya_rel, ws = "6: All Countries with ISO Code + Relation", verbose = TRUE)

kenya_rel_df <- as.data.frame(kenya_rel_sheet)

# Clean up the interpreted data to prepare for a dataframe merge with the original kenya dataframe

kenya_rel_df <- rename(kenya_rel_df, interpreted_country_code = CountryCode)
kenya_rel_df <- select(kenya_rel_df, -NumberTweets)
kenya_rel_df <- select(kenya_rel_df, -Country)

kenya_rel_df <- kenya_rel_df %>% dplyr::filter(interpreted_country_code != "KEN") %>%
  dplyr::filter(interpreted_country_code != "NA")

# Remove tweets from Kenya and those which has no country code associated)

kenya_new <- kenya_new %>% dplyr::filter(interpreted_country_code != "KEN") %>%
  dplyr::filter(interpreted_country_code != "NA")

# Add column with relationship information to dataframe

kenya_new_with_rel <- left_join(kenya_new, kenya_rel_df, by = "interpreted_country_code") 
kenya_new_with_rel$HasForeignRelation <- as.factor(kenya_new_with_rel$HasForeignRelation)

# Change levels so that legend will display properly

kenya_new_with_rel$HasForeignRelation <- recode(kenya_new_with_rel$HasForeignRelation, "TRUE" = "Tweets from countries with foreign relationship with Kenya",
                                 "FALSE" = "Tweets from countries with no foreign relationship with Kenya")

# Create two samples for mapping - tweets from countries with relationship vs tweets from countries without relationship"

mypal <- colorFactor(c("red","blue"), domain = kenya_new_with_rel$HasForeignRelation)

combined_map <-  leaflet() %>%
  addTiles()
combined_map %>% addCircleMarkers(lng = kenya_new_with_rel$long, lat = kenya_new_with_rel$lat, 
                                  radius = 2,
                                  fillOpacity = 0.75, color = mypal(kenya_new_with_rel$HasForeignRelation)) %>%
  addLegend("bottomright", pal = mypal, values = kenya_new_with_rel$HasForeignRelation,
            title = "#KenyaPresident Tweets",
            opacity = 1)

# Create Bubble Chart 

bubble_doc <- gs_title("KenyaPresident Plot")

bubble_sheet <- gs_read(bubble_doc, ws = "10: Number of Twitter Users per Country", verbose = TRUE)

bubble_df <- as.data.frame(bubble_sheet)


kenya_rel_df <- kenya_rel_df %>% dplyr::filter(interpreted_country_code != "KEN") %>%
  dplyr::filter(interpreted_country_code != "NA")

# Remove tweets from Kenya and those which has no country code associated)

bubble_df <- bubble_df %>% dplyr::filter(CountryCode != "KEN") %>%
  dplyr::filter(CountryCode != "NA")

# Set colours for plot

my_colours <- c("Existing Relationship" = "red", "No Relationship" = "blue")

#Draw Bubbleplot

#bubble_plot <- ggplot(data = bubble_df, 
#                      aes(x = NumberUsers, y = NumberTweets)) +
#  geom_point(shape = 16, size = 5, aes(colour = HasRelationshipWithKenya)) +
#  scale_x_log10("Number of Active #KenyaPresident Twitter Users (Log)") +
#  scale_y_log10("Number of Tweets with #KenyaPresident (Log)") +
#  ggtitle("Twitter Activity per Country for #KenyaPresident from 11 - 13 August 2017") +
#  theme_classic() + 
#  scale_colour_manual(values = my_colours, name = "Country Relationship with Kenya")

#bubble_plot

# Create chart according to Tufte's rules

# Change Countrycode and relationship status to factors for analysis

bubble_df$CountryCode <- as.factor(bubble_df$CountryCode)
bubble_df$HasRelationshipWithKenya <- as.factor((bubble_df$HasRelationshipWithKenya))

# Create plot

tufte <- ggplot(bubble_df, aes(NumberUsers, NumberTweets, 
                      color = HasRelationshipWithKenya)) + 
  geom_point() + geom_rug() +
  theme_tufte(ticks=F) + 
  xlab("Number of Tweets") + ylab("Number of Tweeters") + 
  theme(axis.title.x = element_text(vjust=-0.5), 
        axis.title.y = element_text(vjust=1)) +
  scale_colour_manual(values = my_colours, name = "")

tufte
