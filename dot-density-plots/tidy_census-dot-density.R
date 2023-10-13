library(tidycensus)
library(sf)
library(tigris)
library(tidyr)
library(data.table)
library(dplyr)
library(deckgl)
library(leaflet)
library(leafgl)
library(deckgl)
library(RColorBrewer)
library(viridis)
library(htmlwidgets)
library(fst)



######################################## FUNCTIONS AND HOUSEKEEPING #####################################
options(tigris_use_cache = TRUE)


#######from tigris package, just editted to see what group it is working on 
as_dot_density <- function (input_data, value, values_per_dot, group = NULL, erase_water = TRUE,
                            area_threshold = 0.85){
  if (!"terra" %in% installed.packages()) {
    stop("`as_dot_density()` uses the terra package to generate dots. Please install terra first then re-run.",
         call. = FALSE)
  }
  if (erase_water) {
    if (is.null(area_threshold)) {
      area_threshold = 0.75
    }
    input_data <- tigris::erase_water(input_data, area_threshold = area_threshold)
  }
  else {
    if (!is.null(area_threshold)) {
      message("`area_threshold` is to be used when erasing water from input polygons; 
              ignoring as `erase_water` is currently `FALSE`.")
    }
  }
  if (!is.null(group)) {
    groups <- unique(input_data[[group]])
    output_dots <- purrr::map_dfr(groups, function(g) {
      message(g)
      group_data <- input_data[input_data[[group]] == g,]
      
      group_data %>% terra::vect() %>% terra::dots(field = value,
                                                   size = values_per_dot) %>% sf::st_as_sf()
    }) %>% dplyr::slice_sample(prop = 1)
  }
  else {
    output_dots <- input_data %>% terra::vect() %>% terra::dots(field = value,
                                                                size = values_per_dot) %>% sf::st_as_sf()
  }
  output_dots <- sf::st_filter(output_dots, input_data)
  return(output_dots)
}

####### cleaning column names 
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

###### returning dataframe with geometry
create_dot_density <- function(sf_frame, long_vector = 4:10, meta = 1:3, scale = 100){
  
  sf_frame_f <- st_drop_geometry(sf_frame[, c(meta,long_vector)] )
  sf_frame_f <- sf_frame_f %>%
    pivot_longer(col = long_vector,
                 names_to = "Type",
                 values_to = "estimate")
  sf_frame_geom <- st_sf(left_join(sf_frame_f, sf_frame[,1], by = "GEOID"))
  message("Creating Dots")
  sf_dot <- as_dot_density( sf_frame_geom, values_per_dot = scale, value = "estimate", group = "Type")
  message("Finished Dots")
  for( i in 1:nrow(sf_dot)){
    if(i %% 100 == 0){
      print(i)
    }
    sf_dot$lng[i] <- dot_race$geometry[[i]][1]
    sf_dot$lat[i] <- dot_race$geometry[[i]][2]
    
  }
  
  
  
  
  
  
  
  return(sf_dot)
}





x <- load_variables(year = 2020, dataset = "acs5")


write_fst(cleaned_dots, "Age-dots-data.fst")




race <- clean_col_names(get_acs(table = "B02001", geography = "block group", output = "wide",
                geometry = T, year = 2019, state = "17", county = c("031")))

dot_race <- create_dot_density(race,scale = 10)




#split_race_dot <- split(x = dot_race, f = factor(dot_race$Type) )


# pal <- colorFactor(viridis(7, alpha = 1, begin = 0, 
#         end = 0.7, direction = 1, option = "A"),
#         domain = dot_race$Type)
# 
# 
# 
# 
# ###leaflet solution
# leaflet() %>%
#   addProviderTiles(providers[[110]]) %>%
#   setView(zoom = 10.5, lat = 41.889673, lng = -87.634711) %>%
#   addGlPoints(data = dot_race, radius = 2,
#               fillColor = ~pal(Type), popup = dot_race$Type )



###deckgl solution 

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

deck <- deckgl(zoom = 11, latitude = 41.889673, longitude = -87.634711, pitch = 30) %>%
  add_scatterplot_layer(data = dot_race, properties = properties , id = "1")  %>%
  add_basemap(style = use_carto_style("positron")) 
 
if (interactive()) deck

cleaned_dots <- st_drop_geometry(cleaned_dots)
write_fst(cleaned_dots,"Age-dots-chicago.fst")

saveWidget(deck, "racial-dot-white.html")




























