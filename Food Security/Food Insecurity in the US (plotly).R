# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
#  Food Insecurity Visualizations #
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

## Load packages ----
source('Food Security/settings.R')
library(stringr)
library(plotly)
library(rjson)


## Load datasets ----
Health2012 <- fread(paste0(dir$data,'Health 2012.csv'))
Health2013 <- fread(paste0(dir$data,'Health 2013.csv'))
Health2016 <- fread(paste0(dir$data,'Health 2016.csv'))
us_states <- fread(paste0(dir$data,'states.csv'))

# Keep only relevant variables
H2012 <- Health2012[, c('Qualifying Name', 'Name of Area', 'FIPS'
                       , 'State', 'County'
                       , 'Percent of Persons with Limited Access to Healthy Foods')]

H2013 <- Health2013[, c('Qualifying Name'
                        , 'Percent of Persons with Limited Access to Healthy Foods')]

H2016 <- Health2016[,c('Qualifying Name', 'Name of Area', 'FIPS'
                       , 'State', 'County'
                       , 'Percent of Persons with Limited Access to Healthy Foods')]

rm(Health2016,Health2012,Health2013)

# Rename variables 
names(H2012) <- c('CS', 'County', 'FIPS', 'StateC', 'CountyC', 'Perc_Insecure12')
names(H2013) <- c('CS','Perc_Insecure13')
names(H2016) <- c('CS', 'County', 'FIPS', 'StateC', 'CountyC', 'Perc_Insecure16')


## Preparing 2012 Dataset ----
H2012 <- separate(H2012, CS, c('County2', 'State'), ", ", remove = FALSE)
H2012[, Perc_Insecure12 := as.numeric(Perc_Insecure12)]
str(H2012)
apply(is.na(H2012), 2, sum)

# Replacing 2012 NAs with 2013 Data
H2013[, New := str_detect(CS, 'County')]
table(H2013$New)
str(H2013)

H2012 <- left_join(H2012, H2013[,.(CS, Perc_Insecure13)], by = 'CS')
H2012 <- setDT(H2012)
H2012[, Perc_Insecure12 := fifelse(is.na(Perc_Insecure12)
                                   , as.numeric(Perc_Insecure13)
                                   , H2012$Perc_Insecure12)]
H2012[, Perc_Insecure12 := round(Perc_Insecure12, 2)]
H2012 <- H2012[,-'Perc_Insecure13']

# Verifying that no NAs remain
apply(is.na(H2012), 2, sum)


## Preparing 2016 Dataset ----
H2016[, CS := str_replace_all(CS, 'Parish', 'County')]
H2016 <- separate(H2016, CS, into = c('County2', 'State')
                  , ", ", remove = FALSE)
H2016[, County := str_replace_all(County, 'Parish', 'County')]
H2016[, County := str_replace_all(County, " County", "")]
H2016[, Perc_Insecure16 := as.numeric(Perc_Insecure16)]
H2016[, Perc_Insecure16 := round(Perc_Insecure16, 2)]
str(H2016)

# Verifying no NAs exist
apply(is.na(H2016), 2, sum)


## Combine Datasets ----
Food_insecure <- left_join(x = H2012
                       , y = H2016[, c('FIPS', 'Perc_Insecure16')]
                       , by = 'FIPS')
apply(is.na(Food_insecure), 2, sum)

head(Food_insecure)
str(Food_insecure)

# Converting certain variables
Food_insecure <- Food_insecure %>%
  mutate_at(c('StateC', 'CountyC'), as.numeric) %>%
  setDT()
str(Food_insecure)

# Amending FIPS to add zeroes 
Food_insecure[, FIPS := formatC(FIPS, width = 5, format = "d", flag = "0")]

# Join US State Codes
Food_insecure <- merge(x = Food_insecure
                       , y = us_states[ , c("State", "Code")]
                       , by = "State", all.x = TRUE)


## State Maps ----
# 2012 State Map
S2012 <- state_map(dat=Food_insecure, var='Perc_Insecure12', year=2012
                   ,change=FALSE, min=0, max=25)
S2012

# Save to directory
htmlwidgets::saveWidget(as_widget(S2012)
                        , paste0(dir$out,'State_Insecurity_2012.html')) 

# 2016 State Map
S2016 <- state_map(dat=Food_insecure, var='Perc_Insecure16', year=2016
                   ,change=FALSE, min=0, max=25)
S2016

# Save to directory
htmlwidgets::saveWidget(as_widget(S2016)
                        , paste0(dir$out,'State_Insecurity_2016.html')) 

# State Change Map
Food_insecure[, change := Perc_Insecure16-Perc_Insecure12]
state_change <- state_map(dat=Food_insecure, var='change', year=NULL
                   ,change=TRUE, min=NULL, max=NULL)
state_change

# Save to directory
htmlwidgets::saveWidget(as_widget(state_change)
                        , paste0(dir$out,'State_Insecurity_Improvement.html')) 


# Plot all maps in one
combined_states <- subplot(S2012, S2016, state_change, nrows = 3) %>%
  layout(title = 'Food Security in the United States', showlegend = FALSE)

combined_states

# Save to directory
htmlwidgets::saveWidget(as_widget(combined_states)
                        , paste0(dir$out,'Combined_State_Insecurity.html'))


## County Maps ----
# Loading JSON data
county_geo = 'https://raw.githubusercontent.com/plotly/datasets/master/geojson-counties-fips.json'
json_file <- rjson::fromJSON(file=county_geo)

# Checking Max Values for Scale
max(Food_insecure$Perc_Insecure12) #70.74
max(Food_insecure$Perc_Insecure16) #72.27

# 2012 County Map 
C2012 <- county_map(Food_insecure, var='Perc_Insecure12',year=2012
                    ,change=FALSE, min=0, max=75)

# Saving output as an .html
htmlwidgets::saveWidget(as_widget(C2012)
                        , paste0(dir$out,'County_Insecurity_2012.html'))

# Open the file in default browser
browseURL(paste0('file:///', dir$out,'County_Insecurity_2012.html')) 

# 2016 County Map 
C2016 <- county_map(Food_insecure, var='Perc_Insecure16',year=2016
                    ,change=FALSE, min=0, max=75)

# Saving output as an .html
htmlwidgets::saveWidget(as_widget(C2016)
                        , paste0(dir$out,'County_Insecurity_2016.html'))

# Open the file in default browser
browseURL(paste0('file:///', dir$out,'County_Insecurity_2016.html')) 

# County Change Map 
# Checking Max & Min for Scale
max(Food_insecure$change) #40.61
min(Food_insecure$change) #-56.33

county_change <- county_map(Food_insecure, var='change',year=NULL
                    ,change=TRUE, min=-60, max=40)

# Saving output as an .html
htmlwidgets::saveWidget(as_widget(county_change)
                        , paste0(dir$out,'County_Insecurity_Improvement.html'))

# Open the file in default browser
browseURL(paste0('file:///', dir$out,'County_Insecurity_Improvement.html')) 

# END ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
