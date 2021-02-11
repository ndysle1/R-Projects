# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# Food Security Settings & Functions #
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

## Packages ----
library(tidyverse)
library(data.table)


## Directories ----
dir <- list()
dir$project <- 'C:/Users/User/Documents/GitHub/R-Projects/Food Security/' 
dir$out     <- paste0(dir$project,'output/')
dir$data    <- paste0(dir$project,'data/')


## Functions ----
# State Map Function
state_map <- function(dat, var, year, change, min, max){
  
  var <- ensym(var)
  title1 <- ifelse(change==FALSE, paste0(year,' US Food Insecurity by State')
                   ,'US Food Security Improvements by State <br> 2012 - 2016')
  title2 <- ifelse(change==FALSE, 'Food Insecure<br>Pop (%)'
                   , 'Change in Food<br>Insecure Pop (%)')
  name <- ifelse(change==FALSE, paste0('Year ',year), '2012-2016')
  
  caption <- ifelse(change==FALSE, "",'NOTE: A change that is negative represents 
                    a decrease in the number of food insecure individuals')
  
  map <- dat %>%
    group_by(Code) %>%
    summarise(Perc_Ins = mean(!!var)) %>%
    mutate(Perc_Ins = round(Perc_Ins,2)) %>%
    plot_geo(locationmode = 'USA-states') %>%
    add_trace(z= ~Perc_Ins, locations = ~Code, reversescale = TRUE
              , zmin = min, zmax = max, name = name) %>%
    layout(geo = list(scope = 'usa')
           ,title = paste0(title1)
           ,annotations = 
             list(x = 1.1, y = -0.1, text = paste0(caption), 
                  showarrow = F, xref='paper', yref='paper', 
                  xanchor='right', yanchor='auto', xshift=0, yshift=0,
                   font=list(size=10, color="dark gray"))) %>%
    colorbar(title = paste0(title2)
             ,thickness=10
             ,len=.3)
  
  return(map)
}


# County Map Function
county_map <- function(dat, var, year, change, min, max){
  
  dat <- setDT(dat)
  var <- pull(dat, var)
  fips <- pull(dat,'FIPS')
  title <- ifelse(change==FALSE, paste0(year,' US Food Insecurity by County')
                  ,'US Food Security Improvements by County (2012 - 2016)')
  title2 <- ifelse(change==FALSE, 'Food Insecure<br>Pop (%)'
                   , 'Change in Food<br>Insecure Pop (%)')
  caption <- ifelse(change==FALSE, "",'NOTE: A change that is negative represents 
                    a decrease in the number of food insecure individuals')
  text <- ifelse(change==FALSE, 'Food Insecure Pop.:','Change in Food<br>Insecure Pop.:')
  dat[, text1 := paste0(text)]
  dat[, tooltip := paste(CS,'<br>',text, var,'%')]
  tooltip <- pull(dat,'tooltip')
  
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
      text = ~tooltip,
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
  
  map <- map %>%
    layout(annotations = 
             list(x = 1.1, y = -0.1, text = paste0(caption), 
                  showarrow = F, xref='paper', yref='paper', 
                  xanchor='right', yanchor='auto', xshift=0, yshift=0,
                  font=list(size=10, color="dark gray"))) %>%
    colorbar(title = title2
             ,thickness=10
             ,len=.3)
  
  return(map)
}
