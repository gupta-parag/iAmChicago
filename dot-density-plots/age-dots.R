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
analyse_age <- function(sf_frame){
  df <- setDT(st_drop_geometry(sf_frame))
  y <- df[ ,`:=`(GEOID = GEOID,Total = Total, Male = Male, Female = Female, 
                 Under_5 = Male_Under_5_years + Female_Under_5_years,
                 bw_5_17 = Male_5_to_9_years + Male_10_to_14_years+ Male_15_to_17_years+
                   Female_5_to_9_years + Female_10_to_14_years+Female_15_to_17_years,
                 
                 bw_18_24 = Female_18_and_19_years + Female_20_years +Female_21_years +Female_22_to_24_years+
                   Male_18_and_19_years +Male_20_years+Male_21_years  + Male_22_to_24_years , 
                 
                 bw_25_34 =Male_25_to_29_years+Male_30_to_34_years +
                   Female_25_to_29_years+Female_30_to_34_years,
                 
                 bw_35_64 = Male_35_to_39_years+Male_40_to_44_years+ Male_45_to_49_years+Male_50_to_54_years+
                   Male_55_to_59_years+Male_60_and_61_years+Male_62_to_64_years +
                   Female_35_to_39_years+Female_40_to_44_years+ Female_45_to_49_years+Female_50_to_54_years+
                   Female_55_to_59_years+Female_60_and_61_years+Female_62_to_64_years,
                 
                 bw_65_P = Female_65_and_66_years+Female_67_to_69_years+Female_70_to_74_years+
                   Female_75_to_79_years+Female_80_to_84_years+Female_85_years_and_over + Male_65_and_66_years+
                   Male_67_to_69_years+Male_70_to_74_years+Male_75_to_79_years+Male_80_to_84_years+Male_85_years_and_over
  ) ,]
  y_geom <- st_sf(left_join(y, sf_frame[,1], by = "GEOID"))
  
  return(y_geom)
}

bloc_groups <- get_acs(table = "B01001", geography = "block group", year = 2019,
                       state = "17", county = c("097","031","043","197", "089","111"),
                       geometry =T, output = "wide")

cleaned <-   analyse_age(clean_col_names(bloc_groups))
cleaned_geom <- cleaned[,1]
cleaned <- st_drop_geometry(cleaned[ , c(1:3,52:57)])

cleaned_long <- cleaned %>% pivot_longer(cols = 4:9, 
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