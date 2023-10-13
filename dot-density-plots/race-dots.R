library(tidycensus)
library(sf)
library(tidycensus)
library(leaflet)
library(dplyr)
library(stringr)
library(mgsub)
library(data.table)
library(shiny)
library(cartography)
library(reactable)
library(tidyr)
library(tidyr)
library(deckgl)

clean_col_names <- function(sf_frame, y = 2019){
  
  acs_2019 <- load_variables(year = y, dataset = "acs5")
  sf_frame_f <- sf_frame[,!grepl(colnames(sf_frame), pattern = "M$")]
  
  logi_vector <- acs_2019$name %in% gsub(colnames(sf_frame_f)[3:ncol(sf_frame_f)], 
                                         pattern = "E$", replacement = "") # getting into final format of B01001_001 from B01001_001E
  
  col_names_dirty <- acs_2019$label[logi_vector]
  
  
  clean_names <- mgsub::mgsub(col_names_dirty, 
                              pattern = c("Estimate!!Total:!!"," ", ":!!", "Estimate!!",":"), 
                              replacement = c('', '_','_','',''))
  print(clean_names)
  
  colnames(sf_frame_f)[3:ncol(sf_frame_f)] <- c(clean_names, "geometry")
  message("renaming")
  return(sf_frame_f[!st_is_empty(sf_frame_f),])
}







bloc_groups <- get_acs(table = "B02001", geography = "block group", year = 2019,
                       state = "17", county = c("097","031","043","197", "089","111"),
                       geometry =T, output = "wide")

cleaned <- clean_col_names(bloc_groups)
cleaned_geom <- cleaned[,1]
cleaned <- st_drop_geometry(cleaned[ , -c(11,12)])

cleaned_long <- cleaned %>% pivot_longer(cols = 4:10, 
                                         names_to = "Type", 
                                         values_to = "Estimate")
cleaned_final <- st_transform(st_sf(left_join(cleaned_long, cleaned_geom, by = "GEOID")), 4326)

cleaned_dots <- as_dot_density(cleaned_final, value = "Estimate", 
                            values_per_dot = 10, group = "Type")

cleaned_dots$Type <- gsub(cleaned_dots$Type, pattern = "_", replacement = " ")


 for(i in 1:nrow(cleaned_dots)){
   if(i %% 100 == 0) {
     print(i)
   }
   
   cleaned_dots$lng[i] <- cleaned_dots$geometry[[i]][1] 
   cleaned_dots$lat[i] <- cleaned_dots$geometry[[i]][2] 
 }


properties <- list(
  getPosition = ~lng + lat, #[165, 42,42]
  getRadius = 25, #JS("data => Math.sqrt(data.exits)"),
  getColor = JS("data => data.Type === 'White alone' ? [10, 161, 221]: 
                data.Type === 'Black or African American alone' ? [246, 107, 14] : 
                data.Type === 'Some other race alone' ? [6,255,0]: 
                data.Type === 'Asian alone' ? [249, 7, 22] :
                data.Type === 'Two or more races' ? [246, 107, 14] :
                data.Type === 'American Indian and Alaska Native alone' ? [255, 23, 0] :
                data.Type === 'Native Hawaiian and Other Pacific Islander alone' ? [147, 255, 216] : [24, 10, 10]"),
  tooltip = "GEOID : {{Type}}"
)

 deckgl(zoom = 11, latitude = 41.889673, longitude = -87.634711, pitch = 30) %>%
  add_scatterplot_layer(data = cleaned_dots, properties = properties , id = "1")  %>%
  add_basemap(style = use_carto_style("positron")) 