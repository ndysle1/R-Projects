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
state_map <- function(dat, var, year, change, min, max){
  
  var <- ensym(var)
  title1 <- ifelse(change==FALSE, paste0(year,' US Food Insecurity by State')
                   ,'US Food Security Improvements by State <br> 2012 - 2016')
  title2 <- ifelse(change==FALSE, 'Food Insecure Pop (%)'
                   , 'Change in Food<br>Insecure Pop (%)')
  name <- ifelse(change==FALSE, paste0('Year ',year), '2012-2016')
  
  map <- dat %>%
    group_by(Code) %>%
    summarise(Perc_Ins = mean(!!var)) %>%
    mutate(Perc_Ins = round(Perc_Ins,2)) %>%
    plot_geo(locationmode = 'USA-states') %>%
    add_trace(z= ~Perc_Ins, locations = ~Code, reversescale = TRUE
              , zmin = min, zmax = max, name = name) %>%
    layout(geo = list(scope = 'usa'),
           title = paste0(title1)) %>%
    colorbar(title = paste0(title2)) 
  
  return(map)
}

var <- 'Perc_Insecure12'
# County Map Function
county_map <- function(dat, var, year, change, min, max){
  
  dat <- setDT(dat)
  var <- pull(dat, var)
  fips <- pull(dat,'FIPS')
  #location <- pull(dat,'CS')
  title <- ifelse(change==FALSE, paste0(year,' US Food Insecurity by County')
                   ,'US Food Security Improvements by County (2012 - 2016)')
  dat[, text := ifelse(change==FALSE
         , paste(CS,'<br>',
                 'Food Insecure Pop.:', var,'%')
         , paste(CS,'<br>',
                 'Change in Food Insecure Pop.:', var,'%'))]
  text <- pull(dat,'text')
  
  map <- plot_ly() 
  map <- map %>%
    add_trace(
      type='choroplethmapbox',
      geojson=json_file,
      locations=fips,
      z=var,
      colorscale='Viridis',
      reversescale = TRUE,
      zmin=min,
      zmax=max,
      hoverinfo = 'text',
      text = ~text,
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
    layout(title = title)
  
  return(map)
}
