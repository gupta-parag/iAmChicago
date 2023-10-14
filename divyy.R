library(data.table)
library(leafgl)
library(dplyr)
library(leaflet)
library(sf)
library(tidycensus)
library(RMySQL)
library(odbc)
library(RODBC)
library(lubridate)
library(purrr)
library(DBI)
library(echarts4r)
library(tidyr)
library(htmlwidgets)
library(stringr)
library(gmapsdistance)

setwd("C:\\Users\\pgupta\\Downloads")
# set.api.key()
# 
# ######################################## trip matrices ##########################
# 
# 
# y_2017_july <- y_P[MONTH_START == 7 & YEAR_START == 2017, ]
# 
# race <- get_acs(table = "B02001", year = 2019, geography = "tract",
#                 state = "17", county = "031")
# july <- y_2017_july[!is.na(FROM_LONGITUDE) , 
#             .(TRIP_ID = TRIP_ID,
#               FROM_LOCATION = st_as_sfc(FROM_LOCATION),
#               FROM_STATION_ID,TO_STATION_ID,
#               TO_LOCATION = st_as_sfc(TO_LOCATION)) ,]
# 
# census_tract <- st_read(dsn = "C:\\Users\\pgupta\\Documents\\DS\\cta\\tl_2019_17_tract",
#                         layer = "tl_2019_17_tract")
# 
# block_group <- st_read("C:\\Users\\pgupta\\Documents\\DS\\cta\\tl_2019_17_bg",
#                        layer = "tl_2019_17_bg")
# 
# leaflet() %>% addTiles() %>% 
#   addPolygons(data = 
#   st_transform(census_tract[ census_tract$COUNTYFP == "031",], 
#                crs = 4326), 
#               weight = 1, group = "tract", fillOpacity = 0 , color = "black") %>%
#   addPolygons(data = 
#                 st_transform(census_tract[ census_tract$GEOID == "17031320100" | 
#                                              census_tract$GEOID == "17031330100",], 
#                              crs = 4326), 
#               weight = 1, group = "round", fillOpacity = 0.7 , color = "black",
#               fillColor = "green") %>%
#   addPolygons(data = 
#       st_transform(block_group[ block_group$COUNTYFP == "031", ], 
#                    crs = 4326), color = "green", group = "BG", weight = 1, fillOpacity = 0.2,
#       popup = block_group[ block_group$COUNTYFP == "031", "GEOID"]) %>%
#   addGlPoints( data = july$FROM_LOCATION, popup = july$FROM_STATION_ID) %>% 
#   addLayersControl(overlayGroups = c("BG", "tract", "round"))
#  
# july_sf <- st_sf(july)
# ct <- st_transform(census_tract[ , 4], 4326)
# st_crs(july_sf) = 4326
# 
# tracts_from <- st_join(july_sf[ , c(1,2)],  ct, join = st_within)
# july_sf <- st_as_sf(st_drop_geometry(july_sf[ , c(1,3,5)]))
# tracts_to <- st_drop_geometry(st_join(july_sf, ct, join = st_within))
# 
# 
# 
# matrix_long <-  left_join(tracts_from, tracts_to, by = "TRIP_ID")
# matrix_long <- setDT(st_drop_geometry(matrix_long))
# matrix_sf <- matrix_long[ , .(trips = .N), by = c("GEOID.x","GEOID.y")]
# colnames(matrix_sf) <- c("Origin", "Destination", "Trips")
# 
# 
# final_matrix_ct <- as.matrix(pivot_wider(matrix_sf, 
#                 names_from = "Destination", values_from =  "Trips"))
# 
# 
# for( i in 1:nrow(final_matrix_ct)){
#   for(j in 1:ncol(final_matrix_ct)){
#          if(is.na(final_matrix_ct[i,j])){
#            final_matrix_ct[i,j] <- 0
#          }
#   }
# }
# 
# 
# 
# ###########################################################################


 ############################## ggmap #######################
# y_17_july <-  y_2017[ YEAR_START == 2017 & MONTH_START == 7, .(TRIP_ID, FROM_LONGITUDE , FROM_LATITUDE,
#             TO_LONGITUDE,TO_LATITUDE, START_TIME )  , ][,`:=`(OR = paste0(FROM_LATITUDE,"+",FROM_LONGITUDE),
#                                              DES = paste0(TO_LATITUDE, "+", TO_LONGITUDE ))] 
# y_17_july_sep <-  y_17_july %>% separate(sep = " ", into = c("START_DATE","START_TIME"), col = START_TIME) 
# k <- 1:2
# 
# 
# trip_length <- NULL
# 
# for( k in 1:100000){
#   print(k)
#   trip_length[[k]] <- gmapsdistance( y_17_july_sep$OR[k], y_17_july_sep$DES[k], 
#                                 mode = "bicycling", 
#                                 shape = "long",combinations = "pairwise",
#                                 dep_date = gsub(y_17_july_sep$START_DATE[k], pattern = "2017", replacement = "2022"),
#                                 dep_time =   y_17_july_sep$START_TIME[k] , 
#                                 key = "AIzaSyBxWwmVyZm30xU1ZZC85xuKlk9O8CO8DTg")
# }
#######################################################################



# con <- dbConnect(odbc::odbc(), .connection_string = "Driver={MySQL ODBC 8.0 Unicode Driver};", 
#                  Server = "localhost", Database = "divy_trips",
#                  UID = "root", PWD = "Sna!loPso217", Port = 3306)
# dbSendQuery(con, "USE divy_trips")
# dbWriteTable(con, "p_divy", y_P)

  #mydb = dbConnect(MySQL(), user='user', password='password', dbname='database_name', host='host')


#GTFS data
#DIvyybikes trips by month, day of week, time of day, quarterly ansd yearly
##age of people shared in the data and census tract data from 
#census median property values, rent, homeownershop,vehicle owenrship, jobs map,
# odbcConnect()
# 
#from census tract to census tract
# 
# 
# 


#con <- dbConnect(odbc(), "def")

#####################################################################

#y_P <-fread("C:\\Users\\pgupta\\Documents\\DS\\cta\\Divvy_Trips.csv") 
y_P <-fread("Divvy_Trips.csv") 
# sample_year <- 2015
# 
# 
# 
# 
# y_P <- x[, `:=`(`START TIME` = mdy_hms(`START TIME`),
#                 YEAR_START = year(`START TIME`)),][,YEAR_START == sample_year ]

# race <- get_acs(geography = "tract", state = "17", county = "031", table = "B02001", year = 2019,
#                 geometry = T, output = "wide")
# race <- st_transform(race[,1], crs = '+proj=longlat +datum=WGS84')





########################################################################################


######computing stats#######
y_P[ , `:=`(`START TIME` = mdy_hms(`START TIME`,tz = "US/Central"),
          `STOP TIME` = mdy_hms(`STOP TIME`,tz = "US/Central"),
          `FROM LATITUDE` = as.numeric(`FROM LATITUDE`),
          `FROM LONGITUDE`  = as.numeric(`FROM LONGITUDE`),
          `TO LATITUDE` = as.numeric(`TO LATITUDE`),
          `TO LONGITUDE`  = as.numeric(`TO LONGITUDE`),
          `TRIP ID` = as.character(`TRIP ID`)
          ), ]
y_P[,`:=`(HOUR_START = hour(`START TIME`), #hour of start
        DAY_NAME = weekdays(`START TIME`),#name of day
        WDAY_START = wday(`START TIME`), #DAY OF WEEK
        WEEK_START_MONTH = ceiling(day(`START TIME`)/7), # week of month]
        WEEK_START = week(`START TIME`), # week of year
        MONTH_START = month(`START TIME`), #month of of year
          YEAR_START = year(`START TIME`),
        HOUR_TO = hour(`STOP TIME`), #hour of TO
        DAY_NAME_TO = weekdays(`STOP TIME`),#name of day
        WDAY_TO = wday(`STOP TIME`), #DAY OF WEEK
        WEEK_TO_MONTH = ceiling(day(`STOP TIME`)/7), # week of month]
        WEEK_TO = week(`STOP TIME`), # week of year
        MONTH_TO = month(`STOP TIME`), #month of of year
        YEAR_TO = year(`STOP TIME`)
        )]
 y_P[HOUR_START >= 6 & HOUR_START <= 9,`:=`(TOD = "AM_PEAK")]
 y_P[HOUR_START >= 10 & HOUR_START <= 16,`:=`(TOD = "MD")][HOUR_START > 16 & HOUR_START <= 19,
      `:=`(TOD = "PM_PEAK")][HOUR_START >= 20 & HOUR_START <= 24,
      `:=`(TOD = "NT")][HOUR_START >= 0 & HOUR_START <= 3,`:=`(TOD = "NT")][HOUR_START >= 4 & HOUR_START < 6,`:=`(TOD = "EA")]
colnames(y_P) <- gsub(colnames(y_P), pattern = " ", replacement = "_")



library(data.table)
library(tidycensus)
library(dplyr)
library(leaflet)
library(sf)
library(fst)


# setwd("D:\\INRIX")
# 
# trips <- read_fst("trips.fst")
race <- get_acs(table = "B02001", geometry = T, state = "17", 
                county = c("031", "097", "197","111",
                           "043", "089"), output = "wide",
                year = 2020, geography = "tract")



# trips_f <- trips[ 1:100, ]

do_spatial_join <- function(trips_data,sf_frame, type = "origin"){
  
  message("Started One Month")
  list_of_points <- lapply(1:nrow(trips_data), sum)
  message("Created List, making points")
  
  if (type == "origin"){
    for( i in 1:nrow(trips_data)){
      
      if(i %% 100000 == 1){
        print(i)
      }
      
      if(is.na(trips_data$FROM_LONGITUDE[i])) {
        list_of_points[[i]] <-  st_point(c( -80.13174232480651,
                                            25.776534157486157))
      } else{
        list_of_points[[i]] <- st_point(c(trips_data$FROM_LONGITUDE[i],
                                          trips_data$FROM_LATITUDE[i])) 
      }
      
    }
  } else  {
    for( i in 1:nrow(trips_data)){
      if(i %% 100000 == 1){
        print(i)
      }
      
      if(is.na(trips_data$TO_LATITUDE[i])) {
        list_of_points[[i]] <-  st_point(c( -80.13174232480651,
                                            25.776534157486157))
      } else{
        list_of_points[[i]] <- st_point(c(trips_data$TO_LONGITUDE[i],
                                          trips_data$TO_LATITUDE[i]))
      }
    }
  }
  
  
  message("Finished points")
  print(list_of_points)
  trips_data$geometry <- st_sfc(list_of_points)
  trips_data <- st_sf(trips_data)
  st_crs(trips_data) = 4326
  sf_frame <- st_transform(sf_frame,4326)
  print(class(trips_data))
  print(class(sf_frame))
  message("joining")
  spatial_joined <- st_join(trips_data, sf_frame  ,join = st_within)
  message("finished ")
  
  return(spatial_joined)
}


origin <- do_spatial_join(trips_data =  y_P, sf_frame =  st_transform(race[,1],4326))
rm(y_P)
gc(reset = T)

origin_f <- st_drop_geometry(origin)
write_fst(orgin_f,"Origin_mapped.fst", compress=99)

rm(origin)
gc(reset = T)

dest <- do_spatial_join(trips_data =  origin_f, sf_frame =  st_transform(race[,1],4326), 
                        type = "dest")

rm(origin_f)
gc(reset = T)
write_fst(dest,"Dest_Mapped.fst", compress=99)


























 #y_2013 <- y_P[ YEAR_START == 2013,,]
# y_2014 <- y_P[ YEAR_START == 2014,,]
# y_2015 <- y_P[ YEAR_START == 2015,,]
# y_2016 <- y_P[ YEAR_START == 2016,,]
# y_2017 <- y_P[ YEAR_START == 2017,,]
# y_2018 <- y_P[ YEAR_START == 2018,,]
# y_2019 <- y_P[ YEAR_START == 2019,,]


#trips by year
trips_by_year <- y_P[ , .(Trips = .N), by = c("YEAR_START")]

trips_by_year%>%
  mutate(YEAR_START = factor(YEAR_START)) %>%
  filter(YEAR_START != 2013) %>%
  arrange(YEAR_START) %>%
  e_charts(YEAR_START) %>%
  e_x_axis( YEAR_START,type = 'category', axisTick = list(alignWithLabel = TRUE), 
              nameTextStyle = list(fontSize = 22), axisLabel = list(fontSize = 18), 
            nameLocation = 'middle', inverse = TRUE, nameGap = 30) %>%
  # e_area(Trips, stack = "grp1") %>%
    e_bar(Trips,  showBackground = T,
          barwidth = "25%", itemStyle = list(borderRadius = 5)) %>% 
  e_y_axis( nameTextStyle = list(fontSize = 20), axisLabel = list(fontSize = 14), nameLocation = 'center',
            inverse = TRUE, nameGap = 80, minorTick = list(show = TRUE), splitNumber = 6,legend = FALSE) %>%
  
  e_visual_map( min = 2400000, max = 4000000, text = c("Max trips",
          "Min trips"), inRange = list(color = c( '#4FD3C4', '#488FB1', '#533E85')), 
           align = 'bottom', orient = 'vertical',left = 'right', top = 'center') %>%
  
  e_axis_labels( x = "Year", y = "Total Trips" ) %>% 
  #e_tooltip(trigger = "axis")%>%
  e_title("Total Number of Divvy Trips by Year in Chicago", 
          textStyle = list(fontSize = 30), left = 'center') %>%
  # e_theme("infographic") %>%
  e_labels( position = "insideBottom", rotate = 90, align = "left", verticalAlign = "middle",
            distance = 30, fontFamily = 'sans-serif',fontSize = 25,
      formatter = htmlwidgets::JS("
                             function(params){
                            return( echarts.format.addCommas(params.value[1]) + `  trips`) }")) %>%
  e_legend(show = FALSE)






#trips by time of day
trips_by_year_tod <- y_P[ , .(trips = .N), by = c("YEAR_START","TOD")][ ,
              BY_YEAR := sum(trips),by = "YEAR_START"][,TRIPS_P := round(trips* 100/BY_YEAR,1),]
trips_by_year_tod <-  pivot_wider(trips_by_year_tod[ , c(1,2,5)],names_from = "TOD",values_from = TRIPS_P)
trips_by_year_tod$TOTAL <- rowSums(trips_by_year_tod[,c(2:6)])

trips_by_year_tod %>%
  mutate(YEAR_START = factor(YEAR_START)) %>%
  arrange(YEAR_START) %>%
  filter(YEAR_START != 2013) %>%
  e_charts(YEAR_START) %>%
  # e_line(TOTAL, stack = "grp") %>%
  e_theme("infographic") %>%
  e_bar(EA, stack = "grp", emphasis = list(focus = 'series')) |> 
  e_bar(AM_PEAK, stack = "grp", emphasis = list(focus = 'series')) |> 
  e_bar(MD, stack = "grp", emphasis = list(focus = 'series')) |> 
  e_bar(PM_PEAK, stack = "grp", emphasis = list(focus = 'series')) %>%
  e_bar(NT, stack = "grp", emphasis = list(focus = 'series')) %>%
  # e_y_axis( serie = TOTAL) %>%#, margin = 759788
  # # ) %>%
   e_flip_coords() %>%
  e_tooltip(trigger = "axis")%>%
  e_title("Total Trips by Time of Day and Year", "Divy Trips in Chicago") %>%
  e_legend(right = 0) %>%
 e_labels(formatter = htmlwidgets::JS("
                         function(params){
                         return(params.value[0]+`%`) }") ) 


# e_labels(
#   formatter = htmlwidgets::JS("
#                          function(params){
#                         return( echarts.format.addCommas(params.value[0])) }")) 





#trips by gender
trips_by_year_gen <- y_P[ , .(trips = .N), by = c("YEAR_START","GENDER")][ ,
                  BY_YEAR := sum(trips),by = "YEAR_START"][,TRIPS_P := round(trips* 100/BY_YEAR,1),]

trips_by_year_gen$GENDER[trips_by_year_gen$GENDER == ""] <- "Unknown"

trips_by_year_gen <- pivot_wider(trips_by_year_gen[ , c(1,2,5)],
                                 names_from = "GENDER",values_from = TRIPS_P)

trips_by_year_gen %>%
  mutate(YEAR_START = factor(YEAR_START)) %>%
  arrange(YEAR_START) %>%
  filter(YEAR_START != 2013) %>%
  e_charts(YEAR_START) %>%
  e_bar(Male, stack = "grp") %>%
  e_bar(Female, stack = "grp") |> 
  e_bar(Unknown, stack = "grp") |> 
e_flip_coords() %>%
  e_tooltip(trigger = "axis") %>%
  e_title("Trips by Gender", "Divy Trips in Chicago") %>%
  e_theme("bee-insipired") %>%
  e_labels(formatter = htmlwidgets::JS("
                         function(params){
                         return(params.value[0]+`%`) }")) %>%
  e_legend(right = 0) 


# e_labels(
#   formatter = htmlwidgets::JS("
#                          function(params){
#                         return( echarts.format.addCommas(params.value[0])) }")) %>%





#trips by customer type
trips_by_year_use <- y_P[,.(trips = .N), 
                         by = c("YEAR_START","USER_TYPE")]
trips_by_year_use <- pivot_wider(trips_by_year_use,
                                 names_from = "USER_TYPE",values_from = trips)

trips_by_year_use %>%
  mutate(YEAR_START = factor(YEAR_START)) %>%
  arrange(YEAR_START) %>%
  e_charts(YEAR_START) %>%
  e_bar(Subscriber, stack = "grp") |> 
  e_bar(Customer, stack = "grp") |> 
  e_bar(Dependent, stack = "grp") |> 
  e_flip_coords() %>%
  e_theme("bee-insipired") %>%
  e_labels(
    formatter = htmlwidgets::JS("
                         function(params){
                        return( echarts.format.addCommas(params.value[0])) }")) %>%
  e_tooltip(trigger = "axis")

###  AVERAGE TRIPS BY WEEKEDAY ANDD WEKKEND

trips_by_year_day <- y_P[ , .(trips = .N), by = c("YEAR_START","DAY_NAME","TOD")]

trips_by_year_day[DAY_NAME %in% c("Thursday","Wednesday",
                                  "Tuesday","Friday","Monday")  , DAY_TYPE := "WEEKDAY" ,][DAY_NAME %in% c("Sunday","Saturday"),
                                                                                           DAY_TYPE := "WEEKEND"]

tbyday_type_2 <- trips_by_year_day[ , .(MEAN_TRIPS = round(mean(trips,na.rm = T),0)), 
                                    by = c("DAY_TYPE", "YEAR_START")]
tbyday_type_2 <- pivot_wider(tbyday_type_2,
                           names_from = "DAY_TYPE",values_from = MEAN_TRIPS)

tbyday_type_2 %>%
  mutate(YEAR_START = factor(YEAR_START)) %>%
  arrange(YEAR_START) %>%
  e_charts(YEAR_START) %>%
  e_bar(WEEKDAY,stack = "grp") %>%
  e_bar(WEEKEND, stack = "grp2") %>%
  e_labels(
    formatter = htmlwidgets::JS("
                         function(params){
                        return( echarts.format.addCommas(params.value[1])) }")) %>% 
  # e_y_axis(Total_Mean) %>%
  e_tooltip(trigger = "axis")%>%
  e_title("Mean Trips by Day and Year", "Divy Trips in Chicago") %>%
  e_legend(show = FALSE)






tbyday_type <- trips_by_year_day[ , .(MEAN_TRIPS = round(mean(trips,na.rm = T),0),
                                      T_TRIPS = sum(trips,na.rm = T)), by = c("DAY_TYPE", "YEAR_START","TOD")] 
tbyday_type <- pivot_wider(tbyday_type[,c(1,2,3,4)],
                           names_from = "TOD",values_from = MEAN_TRIPS)
tbyday_type$Total_Mean <- round(rowMeans(tbyday_type[,c(3:7)]),0)

tbyday_type %>%
  mutate(YEAR_START = factor(YEAR_START)) %>%
  arrange(YEAR_START) %>%
  filter(YEAR_START != 2013) %>%
  group_by(DAY_TYPE) %>%
  e_charts(YEAR_START, timeline = T) %>%
   e_line(Total_Mean,stack = "grp") %>%
  e_bar(EA, stack = "grp") |> 
  e_bar(AM_PEAK, stack = "grp") |> 
  e_bar(MD, stack = "grp") |> 
  e_bar(PM_PEAK, stack = "grp") |>
  e_bar(NT, stack = "grp") |> 
  e_flip_coords() %>%
  # e_y_axis(Total_Mean) %>%
  e_tooltip(trigger = "axis")%>%
  e_title("Mean Trips by Day Type and Year", "Divy Trips in Chicago") %>%
   e_legend(right = 0) %>%  
   e_labels(
     formatter = htmlwidgets::JS("
                          function(params){
                        return( echarts.format.addCommas(params.value[0]))}")) %>%
  e_x_axis(show = F)


###  AVERAGE TRIP duration, BY WEEKEDAY ANDD WEKKEND,TOD
tripdur_by_year_day <- y_P[ , .(DUR = mean(TRIP_DURATION,na.rm = T)), by = c("YEAR_START","DAY_NAME","TOD")]

tripdur_by_year_day[DAY_NAME %in% c("Thursday","Wednesday",
                                  "Tuesday","Friday","Monday")  , DAY_TYPE := "WEEKDAY" ,][DAY_NAME %in% c("Sunday","Saturday"),
                                                                                           DAY_TYPE := "WEEKEND"]

tdurby_year_day <- tripdur_by_year_day[ , .(MEAN_TRIPS_DUR = round(mean(DUR,na.rm = T)/60,0)), 
                                    by = c("DAY_TYPE", "YEAR_START","TOD")]
tdurby_year_day <- pivot_wider(tdurby_year_day,
                             names_from = "TOD",values_from = MEAN_TRIPS_DUR)
tdurby_year_day$Total_Mean <- round(rowMeans(tdurby_year_day[,c(3:7)]),0)

tdurby_year_day %>%
  mutate(YEAR_START = factor(YEAR_START)) %>%
  arrange(YEAR_START) %>%
  group_by(DAY_TYPE) %>%
  e_charts(YEAR_START, timeline = T) %>%
  e_line(Total_Mean,stack = "grp") %>%
  e_bar(EA, stack = "grp") |> 
  e_bar(AM_PEAK, stack = "grp") |> 
  e_bar(MD, stack = "grp") |> 
  e_bar(PM_PEAK, stack = "grp") |>
  e_bar(NT, stack = "grp") |> 
  e_flip_coords() %>%
  # e_y_axis(Total_Mean) %>%
  e_tooltip(trigger = "axis")%>%
  e_title("Mean Trip Duration (mins) by Day Type and Year", "Divy Trips in Chicago") %>%
  e_legend(right = 0) %>% 
  e_labels() %>%
  e_x_axis(show = F)
 # e_labels(
    # formatter = htmlwidgets::JS("
    #                       function(params){
    #                     return( echarts.format.addCommas(params.value[0]))}"))










#trips by age
trips_by_year_age <- y_P[,.(trips = .N,
                            AGE_MEAN = mean(BIRTH_YEAR,na.rm = T),
                            AGE_MEDIAN = median(BIRTH_YEAR,na.rm = T)), 
                         by = c("YEAR_START")]
trips_by_year_age$AGE_MEAN <- ceiling(trips_by_year_age$AGE_MEAN)
trbya <-  trips_by_year_age[ , .(YEAR = YEAR_START,
                       AGE_MEAN = 2022- AGE_MEAN,
                       AGE_MEDIAN = 2022 - AGE_MEDIAN)]
trbya %>%
  mutate(YEAR = factor(YEAR)) %>%
  arrange(YEAR) %>%
  e_charts(YEAR) %>%
  e_step(AGE_MEAN, stack = "1") %>%
 
  e_bar(AGE_MEDIAN, stack = "2") %>%
  e_labels(formatter = htmlwidgets::JS("
                         function(params){
                         return(params.value[0] +` Yrs`) }")) %>%
  e_tooltip(trigger = "axis") %>%

  e_legend(bottom = 0,right = 0)  %>%
  e_theme("dark") %>%
  e_title("Average and Median Age by Year", "Divy Trips in Chicago") %>%
  e_flip_coords()


####trips by age and TOD


trips_by_year_age_tod <- y_P[,.(trips = .N,
                            AGE_MEAN = mean(BIRTH_YEAR,na.rm = T),
                            AGE_MEDIAN = median(BIRTH_YEAR,na.rm = T)), 
                         by = c("YEAR_START","TOD")]
trips_by_year_age_tod$AGE_MEAN <- ceiling(trips_by_year_age_tod$AGE_MEAN)
trbya_t_p <-  trips_by_year_age_tod[ , .(YEAR = YEAR_START,
                                 TOD = TOD,
                                 AGE_MEAN = 2022- AGE_MEAN,
                                 AGE_MEDIAN = 2022 - AGE_MEDIAN)]
trbya_t <- pivot_wider(trbya_t_p[,c(1:3)], names_from = "TOD", values_from = AGE_MEAN)
trbya_t_median <- pivot_wider(trbya_t_p[,c(1,2,4)], names_from = "TOD", values_from = AGE_MEDIAN)



trbya_t %>%
  mutate(YEAR = factor(YEAR)) %>%
  arrange(YEAR) %>%
  e_charts(YEAR) %>%
  e_area(EA, stack = "1") %>%
  e_area(AM_PEAK, stack = "1") %>%
  e_area(MD, stack = "1") %>%
  e_area(PM_PEAK, stack = "1") %>%
  e_area(NT, stack = "1") %>%
  e_labels(formatter = htmlwidgets::JS("
                         function(params){
                         return(params.value[1] +` Yrs`) }")) %>%
  e_title("AVERAGE AGE BY TIME OF DAY") %>%
  e_tooltip(trigger = "axis") %>%
  e_y_axis(show=F) 
  

# 
# trbya_t_median %>%
#   mutate(YEAR = factor(YEAR)) %>%
#   arrange(YEAR) %>%
#   e_charts(YEAR) %>%
#   e_area(EA, stack = "1") %>%
#   e_area(AM_PEAK, stack = "1") %>%
#   e_area(MD, stack = "1") %>%
#   e_area(PM_PEAK, stack = "1") %>%
#   e_area(NT, stack = "1") %>%
#   e_labels() %>%
#   e_title("MEDIAN AGE BY TIME OF DAY") %>%
#   e_tooltip(trigger = "axis")



#average duration by TOD AND year(not needed)
# trips_by_year_tod_dur <- y_P[ , .(MEAN_DURATION = mean(TRIP_DURATION,na.rm = T)), by = c("YEAR_START","TOD")][ ,MEAN_DURATION := 
#                                         MEAN_DURATION / 60][,MEAN_DURATION := round(MEAN_DURATION,1)]
# trips_by_year_tod_dur <-  pivot_wider(trips_by_year_tod_dur,names_from = "TOD",values_from = MEAN_DURATION)
# trips_by_year_tod_dur$TOTAL_AVERAGE <- round(rowMeans(trips_by_year_tod_dur[ , 2:6]),1)
# 
# trips_by_year_tod_dur %>%
#   mutate(YEAR_START = factor(YEAR_START)) %>%
#   arrange(YEAR_START) %>%
#   e_charts(YEAR_START) %>%
#   e_line(TOTAL_AVERAGE,stack = "grp") %>%
#   e_bar(EA, stack = "grp") |> 
#   e_bar(AM_PEAK, stack = "grp") |> 
#   e_bar(MD, stack = "grp") |> 
#   e_bar(PM_PEAK, stack = "grp") %>%
#   e_bar(NT, stack = "grp") %>%
#   e_labels(formatter = htmlwidgets::JS("
#                          function(params){
#                          return(params.value[0] +` Min`) }")) %>%
#   e_flip_coords() %>%
#   e_tooltip(trigger = "axis")%>%
#   e_title("Mean Duration (mins.) by Time  of Day and Year", "Divy Trips in Chicago") %>%
#   e_legend(right = 0)
  # e_line(TOTAL_MEAN,stack = "grp") %>%





########################################################################################



y_2017 <- y_P[ YEAR_START == 2017,]

	 months <- data.frame(MONTH = c("January","February",	"March","April"	,	"May"	,
	"June","July"	,"August"	,"September",	"October"	,	
	"November","December"),ID = 1:12)










total_stats <- y_2017[, .(trips = .N,
                          mean_duration = mean(TRIP_DURATION,na.rm = T),
                          median_duration = median(TRIP_DURATION,na.rm = T),
                          median_age  = median(BIRTH_YEAR,na.rm = T),
                          mean_age = mean(BIRTH_YEAR, na.rm = T)), 
                      by = c( "GENDER","USER_TYPE","HOUR_START","TOD", "DAY_NAME",
                              "WEEK_START_MONTH", "MONTH_START","YEAR_START" )]
total_stats_month <- total_stats[ , .(trips = sum(trips,na.rm = T)), by = c("MONTH_START","TOD")]
total_stats_month <- pivot_wider(total_stats_month, names_from = "TOD", values_from = "trips")
total_stats_month$Total_Trips <- rowSums(total_stats_month[, c(2:6)])

total_stats_month <-  left_join(months, total_stats_month, by = c("ID" = "MONTH_START"))
total_stats_month %>% 
  e_charts(MONTH) %>%
  e_line(Total_Trips,stack = "g1") %>%
  # e_add_nested("label",Total_Trips) %>%
  e_bar(EA, stack = "g2") %>%
  e_bar(AM_PEAK, stack = "g2") %>%
  e_bar(MD, stack = "g2") %>%
  e_bar(PM_PEAK, stack = "g2") %>%
   e_bar(NT, stack = "g2") %>%
  e_tooltip(trigger = "axis") %>%
  e_labels(
    formatter = htmlwidgets::JS("
                             function(params){
                            return( echarts.format.addCommas(params.value[0]) + ` trips`) }"),
    position = "top") %>%
  e_x_axis( position = "top") %>%
  e_flip_coords() %>%
  e_title("Total Trips of Year 2017 by Month and TOD ")

#july by day # put days average trips also like monday trips. tuiesday trips etc 
total_stats_july <- y_2017[MONTH_START == 7, .(trips = .N,
                          mean_duration = mean(TRIP_DURATION,na.rm = T),
                          median_duration = median(TRIP_DURATION,na.rm = T),
                          median_age  = median(BIRTH_YEAR,na.rm = T),
                          mean_age = mean(BIRTH_YEAR, na.rm = T)), 
                      by = c(  "DAY_NAME","TOD" )]
total_stats_july <- pivot_wider(total_stats_july[ , c(1:3)], 
                                names_from = "TOD", values_from = "trips")
total_stats_july$Total_Trips <- rowSums(total_stats_july[, c(2:6)])
total

days <- data.frame(ID = 1:7,
                   day = c("Monday", "Tuesday", "Wednesday",
                           "Thursday","Friday","Saturday","Sunday"))
 total_stats_july <-  left_join(days, total_stats_july, 
                                by = c("day" = "DAY_NAME"))


total_stats_july %>% 
  mutate(DAY_NAME = factor(day)) %>%
  e_charts(DAY_NAME) %>%
  e_line(Total_Trips,stack = "g1") %>%
  # e_add_nested("label",Total_Trips) %>%
  e_bar(EA, stack = "g2") %>%
  e_bar(AM_PEAK, stack = "g2") %>%
  e_bar(MD, stack = "g2") %>%
  e_bar(PM_PEAK, stack = "g2") %>%
  e_bar(NT, stack = "g2") %>%
  e_tooltip(trigger = "axis") %>%
  e_labels(
    formatter = htmlwidgets::JS("
                             function(params){
                            return( echarts.format.addCommas(params.value[0]) + ` trips`) }"),
    position = "top") %>%
  e_x_axis( position = "top") %>%
  e_flip_coords() %>% 
  e_title("Total July Trips by Day of Week",
          "Divy Trips in Chicago")




#hour by day - saturday july

  
total_stats_july_sat <- y_2017[MONTH_START == 7 & DAY_NAME == "Saturday", .(trips = .N,
                                               Mean_Duration = mean(TRIP_DURATION,na.rm = T),
                                               median_duration = median(TRIP_DURATION,na.rm = T),
                                               median_age  = median(BIRTH_YEAR,na.rm = T),
                                               mean_age = mean(BIRTH_YEAR, na.rm = T)), 
                           by =   "HOUR_START" ]
  
total_stats_july_sat %>% 
  mutate(HOUR_START = factor(HOUR_START)) %>%
  arrange(HOUR_START) %>%
  e_charts(HOUR_START) %>%
  e_bar(trips, stack = "g2") %>%
  e_tooltip(trigger = "axis") %>%
  e_labels(
    formatter = htmlwidgets::JS("
                             function(params){
                            return( echarts.format.addCommas(params.value[1])) }")) %>%
  e_title("Hourly Trips of All Saturdays in July")

total_stats_july_sat[ , Mean_Duration := round(Mean_Duration/60,1)] %>% 
  mutate(HOUR_START = factor(HOUR_START)) %>%
  arrange(HOUR_START) %>%
  e_charts(HOUR_START) %>%
  e_bar(Mean_Duration, stack = "g2") %>%
  e_tooltip(trigger = "axis") %>%
  e_title("Mean Duration of trips all Saturdays in July") %>%
  e_labels(
    formatter = htmlwidgets::JS("
                             function(params){
                            return( echarts.format.addCommas(params.value[1])) }")) 





# y_2015 <- st_as_sf(y_2015[ , .(FROM_LOCATION = st_as_sfc(FROM_LOCATION),
#                                TRIP_ID = TRIP_ID)])
# 
# leaflet() %>% addTiles() %>% addGlPoints(data = y_2013$FROM_LOCATION) %>% addPolygons(data = race, weight = 2,
#                                                                                       fillOpacity = 0.1)



#mean trip lengths for July and days,TOd
#mean trip durations for July, days and TOD











y_from <- spatial_map(y_2015) 
y_to <- spatial_map(y_2015, name_column = "TO_LOCATION")

y_joined <- y_from[y_to, on = "TRIP_ID"]

trips_by_geoid <- y_joined[ ,.(trips = .N, 
                               mean_duration = mean(TRIP_DURATION,na.rm = T)/60,
                               median_duration = median(TRIP_DURATION, na.rm = T)/60),
                            by = c("GEOID", "i.GEOID","TOD", "DAY_NAME", "MONTH_START")][ , PERCENT_TRIPS :=
                                                                                            trips *  100/sum(trips),]

trips_o <- y_joined[ ,.(trips = .N, 
                   mean_duration = mean(TRIP_DURATION,na.rm = T)/60,
                   median_duration = median(TRIP_DURATION, na.rm = T)/60)
                , by = c("GEOID","TOD","DAY_NAME", "MONTH_START")]



trips_o_geom <- st_as_sf(left_join(trips_o, race, by="GEOID"))
plot_sf <-  trips_o_geom[trips_o_geom$TOD == "AM_PEAK" & trips_o_geom$MONTH_START == 7,]

leaflet() %>% addTiles() %>% addPolygons(data = plot_sf)


trips_d <- y_joined[ ,.(trips = .N, 
                        mean_duration = mean(TRIP_DURATION,na.rm = T)/60,
                        median_duration = median(TRIP_DURATION, na.rm = T)/60)
                     , by = c("i.GEOID","TOD","DAY_NAME","MONTH_START")]




y_from_geom <-  y_from_geom[!is.na(y_from_geom$TRIP_ID),]








y_to <-  st_as_sf(y[ , .(`TRIP ID`, `TO LOCATION`)][ , .(`TO LOCATION` = st_as_sfc(`TO LOCATION`),
                                                `TRIP ID` = `TRIP ID`)])
y_to <- y[ , c()]
y[ , `:=`(`FROM LOCATION` = st_as_sfc(`FROM LOCATION`), 
          `TO LOCATION` = st_as_sfc(`TO LOCATION`)),]
y <- st_as_sf(y)
st_crs(y) <- '+proj=longlat +datum=WGS84'

t_from <- setDT(st_drop_geometry(st_join(y[ , c(1,24)],  race, join = st_within)))
t_to <- setDT(st_drop_geometry(st_join(y[ , c(1,25)],  race, join = st_within)))




z <- NULL
z$lines <- map(1:nrow(y), function(x){
  st_linestring(rbind(c(y$`FROM LONGITUDE`[x], y$`FROM LATITUDE`[x]),
                      c(y$`TO LONGITUDE`[x],y$`TO LATITUDE`[x])))
}) 

k <- st_as_sf(data.frame(geom = st_as_sfc(z[["lines"]]),
                      trip_id = y$`TRIP ID`))
  
  y[ ,
              .(geom = st_linestring(rbind(c(`FROM LONGITUDE`, `FROM LATITUDE`),
                                           c(`TO LONGITUDE`,`TO LATITUDE`)))
                  )
              ,]
# routes_line <- st_linestring(rbind(c(y$`FROM LONGITUDE`[1], y$`FROM LATITUDE`[1]),
#                     c(y$`TO LONGITUDE`[1],y$`TO LATITUDE`[1])))

y[ , .(trips_by_day = .N), by = "DAY_NAME"] #BY DAY
y[ , .(trips_by_month = .N), by = "MONTH_START"] #MONTH OF YEAR
y[ , .(trips_by_hour = .N), by = "HOUR_START"] # BY HOUR
y[ , .(trips_by_hour = .N), by = c("HOUR_START","DAY_NAME")]
y[ , .(trips_by_hour = .N), by = c( "FROM STATION ID","HOUR_START","DAY_NAME")]
y[ , .(trips_by_hour = .N), by = c( "FROM STATION ID","HOUR_START","DAY_NAME","MONTH_START")]

y[ , .(trips_by_week_month = .N), by = "WEEK_START_MONTH"] # BY HOUR
y[ , .(trips_by_hour_day = .N), by = c("HOUR_START", "DAY_NAME", "MONTH_START","YEAR_START")]

y[ , `:=`(`FROM LOCATION` = st_as_sfc(`FROM LOCATION`), 
          `TO LOCATION` = st_as_sfc(`TO LOCATION`)),]
y <- st_as_sf(y)
st_crs(y) <- '+proj=longlat +datum=WGS84'

y$distance <- st_distance(y$`FROM LOCATION`, y$`TO LOCATION`)

div_trips_ct_o <- setDT(st_drop_geometry(st_join(y$`FROM LOCATION`, 
                                               race[,1], join = st_within)))

div_trips_ct[ , .(Age_med = median(`BIRTH YEAR`, na.rm = T),
                  duration_med =  median(`TRIP DURATION`, na.rm = T),
                  Age_mean = mean(`BIRTH YEAR`, na.rm = T),
                  duration_mean = mean(`TRIP DURATION`, na.rm = T)) , 
              by = c("GEOID" , "GENDER")]



# centroid <-  st_transform(st_as_sf(data.frame(ID = 1:length(d), geometry = d)),
#                            crs = '+proj=longlat +datum=WGS84')

y$`FROM LOCATION` <- st_as_sfc(y$`FROM LOCATION`)
leaflet() %>% 
  addTiles() %>%  
  addPolylines(data = k, color = "black") %>%
  addPolygons(data = st_transform(race, crs = '+proj=longlat +datum=WGS84'),
              fillOpacity = 0, weight = 2) 
  addCircleMarkers(data = y$`FROM LOCATION`,  weight = 0,
                   fillOpacity = 0.15) 

#joining data through spatial join








##### processing  divvy trips data ######






#avg duration










