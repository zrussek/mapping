
################
# Molly's List #
################

  ## "Pins" at the sites of incidents
  ## Different colors for pins where homicides occurred 
  ## Labeling pins with the address, date & time of incident, and number of victims 
  # Displaying connections between incidents (thank you for looking into this!)
  # Summary tables (if needed, we can easily create this in Excel as well)
  ## Target area boundaries and district boundaries 


###################################
# Clear, load packages, load data #
###################################
  
  # clear 
  rm(list = ls())
  
  # load packages
  library(leaflet)
  library(data.table)
  library(readxl)
  library(ggmap)
  library(htmltools)
  library(magrittr)
  
  # load dataset
  sample <- data.table(read_excel("INVC/maps/sample_incidents.xlsx"))

#############
# edit data #
#############
  
  # change colnames to make them useful
  colnames(sample) <- gsub(" ","_",x = tolower(colnames(sample)))
  
  # fix time 
  sample [, formatted_time := format(time,"%I:%M %p")]
  
  # add city and state information so lat and long are correct
  sample [, city_state := ", chicago, il"]
  
  # add lat and long
  sample [, lat := ggmap::geocode(paste0(location,city_state), source = "google")$lat]
  sample [, long := ggmap::geocode(paste0(location,city_state), source = "google")$lon]
  
  # add indicator variable for himicide or not
  sample [, homicide := ifelse(number_of_victims_that_were_homicides>0, yes = "Homicide", no = "Not a homicide")]

  # # add pretend connections
  # sample [, connect := c("a","b","a","b","b","c","d","e","f")]
  
  # create info for the markers
  # Labeling pins with the address, date & time of incident, and number of victims
  sample [, popup := paste0("<center><b>",location,"</b><br/>", format(date,"%m/%d/%Y"), "<br/>",formatted_time,"<br/>",
                            paste0("Number of Victims: ",number_of_victims),"</center>")]
  sample [number_of_victims_that_were_homicides > 0, popup := paste0(popup,"Number of Homicide Vicitms: ", number_of_victims_that_were_homicides)]
  
##################################
# create elements needed for map #
##################################
    
  # Create a palette that maps factor levels to colors
  palette <- colorFactor(c("red", "navy"), domain = c("Homicide", "Not a homicide"))
  
  # target area boundaries: W Chicago Ave, N Cicero, W Jackson, S Central. 
  target_lat1 <- ggmap::geocode("4825 west Chicago Avenue, Chicago, IL", source = "google")$lat
  target_lat2 <- ggmap::geocode("4828 west Jackson Boulevard, Chicago, IL", source = "google")$lat

  target_lon1 <- ggmap::geocode("547 North Cicero Avenue, Chicago, IL", source = "google")$lon
  target_lon2 <- ggmap::geocode("105 S Central avenue, Chicago, IL", source = "google")$lon

  # The 15th district spans: W Division, N Cicero, W Roosevelt, N Austin
  dist_lat1 <- ggmap::geocode("5032 W Division, Chicago, IL", source = "google")$lat
  dist_lat2 <- ggmap::geocode("5918 W Roosevelt, Chicago, IL", source = "google")$lat

  dist_lon1 <- ggmap::geocode("547 North Cicero Avenue, Chicago, IL", source = "google")$lon
  dist_lon2 <- ggmap::geocode("111 N Austin Boulevard, Chicago, IL", source = "google")$lon

  
#############
# build map #
#############
  
  map <- leaflet(sample) %>%
    
    # Add default OpenStreetMap map tiles
    addTiles() %>%  
    
    # add a legend
    addLegend("bottomright", pal = palette, values = ~homicide,
              title = "Homicide Indication",opacity = 1
    ) %>%
    
    # # add connections
    # addPolylines(lng = ~long, lat = ~lat, weight = .5,
    #              group = ~connect) %>%
    
    # add rectagle 
    addRectangles(
      lng1=dist_lon1, lat1=dist_lat1,
      lng2=dist_lon2, lat2=dist_lat2,
      fillColor = "transparent",
      popup = "<center><b>15th District</b><br/>boardered by<br/>W Division St, N Cicero Ave,<br/>W Roosevelt Rd, and N Austin Blvd</center>",
      color = "blue"
    ) %>%
    
    # add rectagle 
    addRectangles(
      lng1=target_lon1, lat1=target_lat1,
      lng2=target_lon2, lat2=target_lat2,
      fillColor = "transparent",
      popup = "<center><b>Target Area</b><br/>bordered by<br/>W Chicago Ave, N Cicero Ave,<br/>W Jackson Blvd, and S Central Ave</center>",
      color = "green"
    ) %>%
    
    # add markers for incidents
    addCircleMarkers(lng=~long, lat=~lat, 
                     popup=~popup,
                     radius = ~ifelse(homicide == "y", 6, 4),
                     color = ~palette(homicide),
                     stroke = FALSE, fillOpacity = 1)
  
  # print the map
  map
  
  


