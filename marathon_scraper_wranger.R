# Marathon scrapings

library(rvest)


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
for(i in url_list[1:30]){
  page_x <- read_html(i) %>% html_elements("td") %>% html_text2()
  page_x <- page_x[15:(length(page_x)-13)]
  pages_all <- c(pages_all, page_x)
}

#the last page has a different format so we'll deal with it seperately
page31 <-  read_html(url_list[31]) %>% html_elements("td") %>% html_text2()

#and then add the last page to the vector
pages_all <- c(pages_all, page31[15:length(page31)])


# now we'll make a matrix out of the vector
records_all <- matrix(pages_all, byrow = TRUE, ncol = 8)

# and then convert to a dataframe
records_all_df <- as.data.frame(records_all)

#remove the last two stupid rows
records_all_df <- records_all_df[1:(nrow(records_all_df)-2),]

#fix column names
colnames(records_all_df) <- c("Bib_number","name","stupid_column","category","race_time","race_rank","category_rank","net_time")

library(dplyr)

#remove the blank column
records_all_df <- records_all_df %>% select(-stupid_column)


# convert to race times to time
records_all_df <- records_all_df %>%
  mutate(race_time = hms(race_time),
         net_time = hms(net_time)) %>%
  arrange(net_time)

#rank runners based of net time instead of race time
records_all_df[,"net_rank"] <- rownames(records_all_df)

#add field that shows how long runners had to wait 
records_all_df <- records_all_df %>% mutate(start_line_wait = (as.numeric(race_time) - as.numeric(net_time))/60) %>%
  arrange(desc(start_line_wait))

#add now per mile pace
#and per km pace

records_all_df <- records_all_df %>%
  mutate(min_per_mile_net = as.numeric(net_time)/60/13.10937873,
         min_per_mile_race = as.numeric(race_time)/60/13.10937873,
         min_per_km_net = as.numeric(net_time)/60/21.0975,
         min_per_km_race = as.numeric(race_time)/60/21.0975)%>%
  arrange(min_per_mile_net)

# this splits off minutes from the time by rounding down to the nearest minute
records_all_df <- records_all_df %>%
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


write.csv(records_all_df, file = "taipei_halfmarathon_2021_results.csv")


