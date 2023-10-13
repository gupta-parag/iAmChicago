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
library(fst)
library(purrr)


setwd("D:\\PASSION_PROJECTS\\iqc\\national-us-racial-dot")

state_choices<- unique(fips_codes$state_code)[-56]
names(state_choices) <- unique(fips_codes$state_name)[-56]
county_choices <- setDT(fips_codes[ fips_codes$state_code %in% state_choices, c(1,2,4,5) ])
# 
 # bgs <-  get_decennial(geography = "block group", variables =  census_2020$name[6:12], state = "01",
 #                       county = "003",year = 2020, geometry = T)
 # bgs <- bgs[!st_is_empty(bgs),]
 # bgs$value[bgs$value >= 1 & bgs$value <10] <- 10
 # zero <- data.frame(GEOID = bgs$GEOID[bgs$value == 0])
 # zero_values_replaced <<- rbind(zero_values_replaced,zero)
 # bgs$value[bgs$value == 0] <- 10
 # 
 # print("creating dots")
 # bg_dots <- as_dot_density(bgs, value = "value",
 #                           values_per_dot = 10, group = "variable")
 # message("Finished")

census_2020 <- load_variables(year = 2020, dataset = "pl")


# for( i in 1:length(state_choices)){
#  state_bg <-  lapply(county_choices[state_code == state_choices[i], county_code],
#          function(x){
#            get_decennial(geography = "block group", table = "P1", state = state_choices[i], 
#                          county = x ,year = 2020, geometry = T, output = "wide")
#          })
#  
#  
#  
#  
# }




main_function <- function(x){
    print("------------------------------------------------------------------------")
    current_state <- unique(county_choices[state_code == state_choices[i], state])
    current_state_fips <- unique(county_choices[state_code == state_choices[i], state_code])
    current_county <- county_choices[ state == current_state &
                                        county_code == x,  county]
    
    print(paste0("State Choice : ", current_state," (", current_state_fips, ")",
                 ", County : ", current_county, " (", x , ") : " , counter  , " of " ,
                 nrow(county_choices[ state == current_state,]) ))
    counter <<- counter + 1
    
    bgs <-  get_decennial(geography = "block group", variables =  census_2020$name[6:12], 
                          state = state_choices[i], county = x ,year = 2020, 
                          geometry = T)
    bgs <- bgs[!st_is_empty(bgs),-2]
    print("Got Data")
    message("replacing values less than 10")
    bgs$value[bgs$value >= 1 & bgs$value <10] <- 10
    zero <- data.frame(GEOID = bgs$GEOID[bgs$value == 0])
    zero_values_replaced <<- rbind(zero_values_replaced,zero)
    bgs$value[bgs$value == 0] <- 10
    
    print("creating dots")
    bg_dots <- as_dot_density(bgs, value = "value", 
                              values_per_dot = 10, group = "variable")
    message("Finished")
    print("replacing geometry with columns")
    
    for( i in 1:nrow(bg_dots)){
      
      if(i %% 1000 == 0) {
        print(paste0(i, " of ", nrow(bg_dots)))
      }
      
      bg_dots$lng[i] <- bg_dots$geometry[[i]][1] 
      bg_dots$lat[i] <- bg_dots$geometry[[i]][2] 
    }
    message("Dropping geometry")
    bg_dots <- st_drop_geometry(bg_dots)
    print("sending")
    print("------------------------------------------------------------------------")
    return(bg_dots)
  }




for( i in state_choices[14] ){ #1:length(state_choices)
  counter <- 1
  zero_values_replaced <- data.frame(GEOID = "1")
  counties <- county_choices[state_code == state_choices[i], county_code]
  state_bg <-  map(counties,possibly(main_function, otherwise = NA_real_))
  names(state_bg) <- county_choices[state_code == state_choices[i], county_code]
  final_df <- do.call(rbind.data.frame, state_bg)
  
  
  if( is.na(state_bg)){
    final_df <- final_df[!is.na(final_df$value),]
    nocensusCounties <- as.data.frame( which(is.na(state_bg)))
    nocensusCounties$State <- state_choices[i]
    colnames(nocensusCounties) <- c("county", "index","state")
    write.csv(nocensusCounties, str_c( "NoCensusCounties_",state_choices[i],"_bg.csv"))
  }
  
  write_fst(final_df, str_c(names(state_choices)[i],"_",state_choices[i], ".fst") )
  write_fst(zero_values_replaced, str_c(names(state_choices)[i],"_",state_choices[i],
                                        "zero_values_block_groups.fst") )
  
  print(paste0("State Choice : ", names(state_choices)[i],
               " (", state_choices[i], ")"))
  print(" *************(Finished the above state. Begin New One) *******************")
  
}

# bgs <-  get_decennial(geography = "tract", variables =  census_2020$name[6:12], 
#                       state = "02", county = "201" ,year = 2020, 
#                       geometry = T, output = "wide")

