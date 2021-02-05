# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# Food Security Settings & Functions #
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

# Packages ----
library(tidyverse)
library(data.table)


# Directories ----
dir <- list()
dir$project <- 'C:/Users/User/Documents/GitHub/R-Projects/Food Security/' 
dir$out     <- paste0(dir$project,'output/')
dir$data    <- paste0(dir$project,'data/')

# State Map Function
state_map <- function(dat, var, year){
  
  var <- ensym(var)
  map <- dat %>%
    group_by(Code) %>%
    summarise(Perc_Ins = mean(!!var)) %>%
    mutate(Perc_Ins = round(Perc_Ins,2)) %>%
    plot_geo(locationmode = 'USA-states') %>%
    add_trace(z= ~Perc_Ins, locations = ~Code, reversescale = TRUE
              , zmin = 0, zmax = 25, name = paste0('Year ',year)) %>%
    layout(geo = list(scope = 'usa'),
           title = paste0(year,' US Food Insecurity by State')) %>%
    colorbar(title = 'Food Insecure (%)') 
  
  return(map)
}


# County Map Function
county_map <- function(dat, var, year){
  
  dat <- setDT(dat)
  var <- pull(dat, var)
  fips <- pull(dat,'FIPS')
  location <- pull(dat,'CS')
  
  map <- plot_ly() 
  map <- map %>%
    add_trace(
      type='choroplethmapbox',
      geojson=json_file,
      locations=fips,
      z=var,
      colorscale='Viridis',
      reversescale = TRUE,
      zmin=0,
      zmax=75,
      hoverinfo = 'text',
      text = ~paste(location,'<br>',
                    'Food Insecure Pop.:', var,'%'),
      marker=list(line=list(
        width=0),
        opacity=0.5))
  
  map <- map %>%
    layout(
      mapbox=list(
        style='carto-darkmatter',
        zoom = 2,
        center= list(lon= -125, lat=49)))
  
  map <- map %>%
    layout(title = paste0(year,' US Food Insecurity by County'))
  return(map)
}