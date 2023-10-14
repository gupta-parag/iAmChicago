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

setwd("/Users/parag.geminigmail.com/Downloads")

y_P <-fread("Divvy_Trips.csv") 

#Cleaning Data
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

# geaphs 

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
  e_labels(formatter = htmlwidgets::JS("
                         function(params){
                         return(params.value[0]+`%`) }") ) %>%
  e_legend(bottom = 0)


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
  e_bar(Male, stack = "grp",emphasis = list(focus = 'series')) %>%
  e_bar(Female, stack = "grp",emphasis = list(focus = 'series')) |> 
  e_bar(Unknown, stack = "grp",emphasis = list(focus = 'series')) |> 
  e_flip_coords() %>%
  e_tooltip(trigger = "axis") %>%
  e_title("Trips by Gender", "Divy Trips in Chicago") %>%
  e_theme("bee-insipired") %>%
  e_labels(formatter = htmlwidgets::JS("
                         function(params){
                         return(params.value[0]+`%`) }")) %>%
  e_legend(bottom = 0) 

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
  e_legend(show = T) 



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
  e_line(Total_Mean,stack = "grp",emphasis = list(focus = 'series')) %>%
  e_bar(EA, stack = "grp",emphasis = list(focus = 'series')) |> 
  e_bar(AM_PEAK, stack = "grp",emphasis = list(focus = 'series')) |> 
  e_bar(MD, stack = "grp",emphasis = list(focus = 'series')) |> 
  e_bar(PM_PEAK, stack = "grp",emphasis = list(focus = 'series')) |>
  e_bar(NT, stack = "grp",emphasis = list(focus = 'series')) |> 
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
#age stacked bar chart for every year, 
#or timeline for year and agedistirubition for every year.

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
  e_y_axis(show=F) %>%
  e_legend(bottom = 0)




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














