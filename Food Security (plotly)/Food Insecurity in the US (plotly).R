library(tidyverse)

#Set Working Directory#
setwd("C:/Users/User/Desktop/Software Cheats/R/Projects/Food Security (plotly)")

#Importing Datasets#
library(readr)
Health2012 <- read_csv("Health 2012.csv")
Health2016 <- read_csv("Health 2016.csv")
us_states <- read_csv("Data.csv")

#Dropping Irrelevant Variables#
library(reshape2)
H2012 <- Health2012[-1, c(1:3, 4:5, 36)]
H2016 <- Health2016[-1, c(1:3, 5:6, 47)]

#Renaming Variables#
colnames(H2012) <- c("CS", "County", "FIPS", "StateC", "CountyC", "Perc_Insecure12")
colnames(H2016) <- c("CS", "County", "FIPS", "StateC", "CountyC", "Perc_Insecure16")

#Preparing 2012 Dataset#
?separate
H2012 <- separate(H2012, CS, into = c("County2", "State"), ", ", remove = FALSE)
H2012$Perc_Insecure12 <- as.numeric(H2012$Perc_Insecure12)
str(H2012)
apply(is.na(H2012), 2, sum)

#Replacing 2012 NAs with 2013 Data#
Health2013 <- read_csv("Health 2013.csv")
H2013 <- Health2013[-1, c(1, 46)]
colnames(H2013) <- c("CS","Perc_Insecure13")
H2013$New <- str_detect(H2013$CS, "County")
table(H2013$New)
str(H2013)

H2012$Perc_Insecure12 <- if_else(is.na(H2012$Perc_Insecure12), as.numeric(H2013$Perc_Insecure13), H2012$Perc_Insecure12)
H2012$Perc_Insecure12 <- round(H2012$Perc_Insecure12, 2)
apply(is.na(H2012), 2, sum)

#Preparing 2016 Dataset#
library(stringr)
?str_replace_all
H2016$CS <- str_replace_all(H2016$CS, "Parish", "County")
H2016 <- separate(H2016, CS, into = c("County2", "State"), ", ", remove = FALSE)
H2016$County <- str_replace_all(H2016$County, "Parish", "County")
H2016$County <- str_replace_all(H2016$County, " County", "")
H2016$Perc_Insecure16 <- as.numeric(H2016$Perc_Insecure16)
str(H2016)
apply(is.na(H2016), 2, sum)
H2016$Perc_Insecure16 <- round(H2016$Perc_Insecure16, 2)

#Combining Health Datasets#
Food_insecure <- merge(x = H2012, y = H2016[, c("CS", "Perc_Insecure16")], by = "CS", all.x = TRUE, all.y = TRUE)
apply(is.na(Food_insecure), 2, sum)

head(Food_insecure)
str(Food_insecure)

#Converting 2 Variables to Numeric#
Food_insecure <- Food_insecure %>%
  mutate_at(c("StateC", "CountyC"), as.numeric)
str(Food_insecure)

#Merging Health Datasets with US State Codes#
Food_insecure <- merge(x = Food_insecure, y = us_states[ , c("State", "Code")], by = "State", all.x = TRUE)

#State Map Graphing#
library(plotly)
#2012 State Map#
S2012 <- Food_insecure %>%
  group_by(Code) %>%
  summarise(Perc_Ins = mean(Perc_Insecure12)) %>%
  mutate(Perc_Ins = round(Perc_Ins,2)) %>%
  plot_geo(locationmode = 'USA-states') %>%
  add_trace(z= ~Perc_Ins, locations = ~Code, reversescale = TRUE, zmin = 0, zmax = 25, name = 'Year<br>2012') %>%
  layout(geo = list(scope = 'usa'),
         title = '2012 US Food Insecurity by State') %>%
  colorbar(title = "Food Insecure (%)") 

S2012

#2016 State Map#
S2016 <- Food_insecure %>%
  group_by(Code) %>%
  summarise(Perc_Ins = mean(Perc_Insecure16)) %>%
  mutate(Perc_Ins = round(Perc_Ins,2)) %>%
  plot_geo(locationmode = 'USA-states') %>%
  add_trace(z= ~Perc_Ins, locations = ~Code, reversescale = TRUE, zmin = 0, zmax = 25, name = 'Year<br>2016') %>%
  layout(geo = list(scope = 'usa'),
         title = '2016 US Food Insecurity by State') %>%
  colorbar(title = "Food Insecure (%)")

S2016

#State Change Map#
S <- Food_insecure %>%
  group_by(Code) %>%
  summarise(Perc_Ins12 = mean(Perc_Insecure12), Perc_Ins16 = mean(Perc_Insecure16)) %>%
  mutate(Perc_Ins = Perc_Ins16 - Perc_Ins12) %>%
  mutate(Perc_Ins = round(Perc_Ins,2)) %>%
  plot_geo(locationmode = 'USA-states') %>%
  add_trace(z= ~Perc_Ins, locations = ~Code, reversescale = TRUE, name = '2012-<br>2016') %>%
  layout(geo = list(scope = 'usa'),
         title = 'US Food Security Improvements by State <br> 2012 - 2016') %>%
  colorbar(title = "Change in Food<br>Insecure Population (%)")

S

#Subplot Combining 3 State Maps#
combined_states <- subplot(S2012, S2016, S, nrows = 3) %>%
  layout(title = "Food Security in the United States",
         showlegend = FALSE)

combined_states

#County Map Graphing#
#Pulling in County Geo Info from JSON#
library(rjson)
county_geo = 'https://raw.githubusercontent.com/plotly/datasets/master/geojson-counties-fips.json'
json_file <- rjson::fromJSON(file=county_geo)

#Checking Max Values for Scale# 
max(Food_insecure$Perc_Insecure12) #70.74#
max(Food_insecure$Perc_Insecure16) #72.27#

#2012 County Map#
C2012 <- plot_ly() 
C2012 <- C2012 %>% add_trace(
  type="choroplethmapbox",
  geojson=json_file,
  locations=Food_insecure$FIPS,
  z=Food_insecure$Perc_Insecure12,
  colorscale="Viridis",
  reversescale = TRUE,
  zmin=0,
  zmax=75,
  hoverinfo = "text",
  text = ~paste(Food_insecure$CS,"<br>",
              "Food Insecure Pop.:", Food_insecure$Perc_Insecure12,"%"),
  marker=list(line=list(
    width=0),
    opacity=0.5))

C2012 <- C2012 %>%
  layout(
  mapbox=list(
    style="carto-darkmatter",
    zoom = 2,
    center= list(lon= -125, lat=49)))

C2012 <- C2012 %>%
  layout(title = "2012 US Food Insecurity by County")

htmlwidgets::saveWidget(as_widget(C2012), "2012.html") #Saving the file as a .html file
browseURL(paste('file://', getwd(),'2012.html', sep='/')) #Opening the file in default browser

#2016 County Map#
C2016 <- plot_ly() 
C2016 <- C2016 %>% add_trace(
  type="choroplethmapbox",
  geojson=json_file,
  locations=Food_insecure$FIPS,
  z=Food_insecure$Perc_Insecure16,
  colorscale="Viridis",
  reversescale = TRUE,
  zmin=0,
  zmax=75,
  hoverinfo = "text",
  text = ~paste( Food_insecure$CS,"<br>",
                "Food Insecure Pop.:", Food_insecure$Perc_Insecure16,"%"),
  marker=list(line=list(
    width=0),
    opacity=0.5))

C2016 <- C2016 %>%
  layout(
    mapbox=list(
      style="carto-darkmatter",
      zoom = 2,
      center= list(lon= -125, lat=49)))

C2016 <- C2016 %>%
  layout(title = "2016 US Food Insecurity by County")

htmlwidgets::saveWidget(as_widget(C2016), "2016.html") #Saving the file as a .html file
browseURL(paste('file://', getwd(),'2016.html', sep='/')) #Opening the file in default browser

#County Change Map#
A <- Food_insecure %>%
  mutate(Perc_Ins = Perc_Insecure16 - Perc_Insecure12) %>%
  mutate(Perc_Ins = round(Perc_Ins,2))

#Checking Max & Min for Scale#
max(A$Perc_Ins) #40.61
min(A$Perc_Ins) #-56.33#

C <-  plot_ly() 

C <- C %>% add_trace(
  type="choroplethmapbox",
  geojson=json_file,
  locations=Food_insecure$FIPS,
  z=A$Perc_Ins,
  colorscale="Viridis",
  reversescale = TRUE,
  zmin=-60,
  zmax=40,
  hoverinfo = "text",
  text = ~paste( Food_insecure$CS,"<br>",
                 "Change in Food Insecure Pop.:", A$Perc_Ins,"%"),
  marker=list(line=list(
    width=0),
    opacity=0.5))

C <- C %>%
  layout(
    mapbox=list(
      style="carto-darkmatter",
      zoom = 2,
      center= list(lon= -125, lat=49)))

C <- C %>%
  layout(title = "US Food Security Improvements by County (2012 - 2016)")

htmlwidgets::saveWidget(as_widget(C), "2012-2016.html") #Saving the file as a .html file
browseURL(paste('file://', getwd(),'2012-2016.html', sep='/')) #Opening the file in default browser


