# marathon scraper


###############################################################################
###
###               Load Libraries 
###
###############################################################################

library(rvest)
library(dplyr)
library(lubridate)

###############################################################################
###############################################################################
##
##      Marathon
##
###############################################################################
###############################################################################


# first create some empty vectors
url_list <- c()
page_x   <- c()
pages_all <-c()


#Next create a new url for each page of the table and store in a vector
for(i in 1:14){
  url_list <- c(url_list, paste0("https://www.championchiptw.com/report/report_w.php?EventCode=20211219&Race=MA&sn=&Sex=N&CatId=0&Cat&pagenum=",i))
}


#page_2 <-  read_html(url_list[2]) %>% html_elements("td") %>% html_text2()

#Next we'll piece together the data by looking for text associated with the html element td
# this represents each cell in the data set
# then we combine it into a vector
#after removing some of the none data related elements stored at the top and bottom of each page
for(i in url_list[1:14]){
  page_x <- read_html(i) %>% html_elements("td") %>% html_text2()
  page_x <- page_x[15:(length(page_x)-13)]
  pages_all <- c(pages_all, page_x)
}

#the last page has a different format so we'll deal with it seperately
#page14 <-  read_html(url_list[14]) %>% html_elements("td") %>% html_text2()

#and then add the last page to the vector
#pages_all <- c(pages_all, page14[15:length(page14)])


# now we'll make a matrix out of the vector
marathon_all <- matrix(pages_all, byrow = TRUE, ncol = 8)

# and then convert to a dataframe
marathon_df <- as.data.frame(marathon_all)

#remove the last two stupid rows
marathon_df <- marathon_df[1:(nrow(marathon_df)-2),]

#fix column names
colnames(marathon_df) <- c("Bib_number","name","blank_column","category","race_time","race_rank","category_rank","net_time")


#remove the blank column
marathon_df <- marathon_df %>% select(-blank_column)


# convert to race times to time
marathon_df <- marathon_df %>%
  mutate(race_time = hms(race_time),
         net_time = hms(net_time)) %>%
  arrange(net_time)

#rank runners based of net time instead of race time
marathon_df[,"net_rank"] <- rownames(marathon_df)

#add field that shows how long runners had to wait 
marathon_df <- marathon_df %>% mutate(start_line_wait = (as.numeric(race_time) - as.numeric(net_time))/60) %>%
  arrange(desc(start_line_wait))

#add now per mile pace
#and per km pace


marathon_df <- marathon_df %>%
  mutate(min_per_mile_net = as.numeric(net_time)/60/26.2187575,
         min_per_mile_race = as.numeric(race_time)/60/26.2187575,
         min_per_km_net = as.numeric(net_time)/60/42.195,
         min_per_km_race = as.numeric(race_time)/60/42.195)%>%
  arrange(min_per_mile_net)

# this splits off minutes from the time by rounding down to the nearest minute
marathon_df <- marathon_df %>%
  mutate(min_per_mile_net_m = floor(min_per_mile_net),
         
         # the step above rounded down to the nearest minute
         # the step below subtracts out the number of minutes to leave the number of seconds
         #but the seconds need to be converted from a per 100 to per 60 scale
         
         min_per_mile_net_s = (min_per_mile_net - floor(min_per_mile_net))*60,
         min_per_mile_race_m = floor(min_per_mile_race),
         min_per_mile_race_s = ( min_per_mile_race - floor(min_per_mile_race) )*60,
         min_per_km_net_m = floor(min_per_km_net),
         min_per_km_net_s = (min_per_km_net - floor(min_per_km_net) )*60 ,
         min_per_km_race_m = floor(min_per_km_race),
         min_per_km_race_s = (min_per_km_race - floor(min_per_km_race))*60)

# remove intermediate data structures
rm(i,page_x,pages_all,url_list, marathon_all)

# write dataframe to csv file
write.csv(marathon_df, file = "./data/taipei_marathon_2021_results.csv")

###############################################################################
###############################################################################
##
##      Marathon
##
###############################################################################
###############################################################################



# first create some empty vectors
url_list <- c()
page_x   <- c()
pages_all <-c()


#Next create a new url for each page of the table and store in a vector
for(i in 1:31){
  url_list <- c(url_list, paste0("https://www.championchiptw.com/report/report_w.php?EventCode=20211219&Race=HM&sn=&Sex=N&CatId=0&Cat&pagenum=",i))
}


#Next we'll piece together the data by looking for text associated with the html element td
# this represents each cell in the data set
# then we combine it into a vector
#after removing some of the none data related elements stored at the top and bottom of each page
for(i in url_list[1:31]){
  page_x <- read_html(i) %>% html_elements("td") %>% html_text2()
  page_x <- page_x[15:(length(page_x)-13)]
  pages_all <- c(pages_all, page_x)
}

#the last page has a different format so we'll deal with it seperately
#page31 <-  read_html(url_list[31]) %>% html_elements("td") %>% html_text2()

#and then add the last page to the vector
#pages_all <- c(pages_all, page31[15:length(page31)])


# now we'll make a matrix out of the vector
half_marathon_all <- matrix(pages_all, byrow = TRUE, ncol = 8)

# and then convert to a dataframe
half_marathon_df <- as.data.frame(half_marathon_all)

#remove the last two stupid rows
half_marathon_df <- half_marathon_df[1:(nrow(half_marathon_df)-2),]

#fix column names
colnames(half_marathon_df) <- c("Bib_number","name","blank_column","category","race_time","race_rank","category_rank","net_time")


#remove the blank column
half_marathon_df <- half_marathon_df %>% select(-blank_column)


# convert to race times to time
half_marathon_df <- half_marathon_df %>%
  mutate(race_time = hms(race_time),
         net_time = hms(net_time)) %>%
  arrange(net_time)

#rank runners based of net time instead of race time
half_marathon_df[,"net_rank"] <- rownames(half_marathon_df)

#add field that shows how long runners had to wait 
half_marathon_df <- half_marathon_df %>% mutate(start_line_wait = (as.numeric(race_time) - as.numeric(net_time))/60) %>%
  arrange(desc(start_line_wait))

#add now per mile pace
#and per km pace

half_marathon_df <- half_marathon_df %>%
  mutate(min_per_mile_net = as.numeric(net_time)/60/13.10937873,
         min_per_mile_race = as.numeric(race_time)/60/13.10937873,
         min_per_km_net = as.numeric(net_time)/60/21.0975,
         min_per_km_race = as.numeric(race_time)/60/21.0975)%>%
  arrange(min_per_mile_net)

# this splits off minutes from the time by rounding down to the nearest minute
half_marathon_df <- half_marathon_df %>%
  mutate(min_per_mile_net_m = floor(min_per_mile_net),
         
         # the step above rounded down to the nearest minute
         # the step below subtracts out the number of minutes to leave the number of seconds
         #but the seconds need to be converted from a per 100 to per 60 scale
         #floor rounds down the nearest integer, bc no one cares about milis of miliseconds here
         
         min_per_mile_net_s = floor((min_per_mile_net - floor(min_per_mile_net))*60),
         min_per_mile_race_m = floor(min_per_mile_race),
         min_per_mile_race_s = floor(( min_per_mile_race - floor(min_per_mile_race) )*60),
         min_per_km_net_m = floor(min_per_km_net),
         min_per_km_net_s = floor((min_per_km_net - floor(min_per_km_net) )*60) ,
         min_per_km_race_m = floor(min_per_km_race),
         min_per_km_race_s = floor((min_per_km_race - floor(min_per_km_race))*60))


# remove intermediate data structures
rm(i,page_x,pages_all,url_list, half_marathon_all)

write.csv(half_marathon_df, file = "./data/taipei_halfmarathon_2021_results.csv")

###############################################################################
###############################################################################
##
##      Marathon and half marathon combined data set
##
###############################################################################
###############################################################################

#create a label column so that we can keep things straight after combination
half_marathon_df[,"course_length"] <- "Half Marathon"
     marathon_df[,"course_length"] <- "Marathon"

# combine the two data frames with marathon on top

taipei_combined_results <- rbind(marathon_df,half_marathon_df)


#write the combined dataframe to a csv file in the data folder
write.csv(taipei_combined_results, file = "./data/taipei_combined_results.csv")


