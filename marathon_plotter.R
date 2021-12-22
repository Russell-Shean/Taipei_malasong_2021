library(ggplot2)


ggplot(records_all_df)+
  geom_point(aes(x=start_line_wait, y= as.numeric(net_time)))


  ggplot(records_all_df[records_all_df$Bib_number=="22579",])+
  geom_point(aes(x=start_line_wait, y= as.numeric(net_time)),color="red")

ggplot(records_all_df)+
  geom_point(aes(x=start_line_wait, y= as.numeric(net_time)))+
  geom_point(data= records_all_df[records_all_df$Bib_number=="22579",] ,aes(x=start_line_wait, y= as.numeric(net_time)),color="red")


ggplot(records_all_df)+
  geom_histogram(aes(x=as.numeric(net_time)/3600), color="red", fill="red", alpha=0.3)+
  geom_histogram(data=records_all_df, aes(x=as.numeric(race_time)/3600), color="blue", fill="blue", alpha=0.3)+
  geom_vline(xintercept = as.numeric(records_all_df[records_all_df$Bib_number=="22579","net_time"])/3600, color="red")+
  geom_vline(xintercept = as.numeric(records_all_df[records_all_df$Bib_number=="22579","race_time"])/3600, color="blue")


ggplot(records_all_df)+
  geom_histogram(aes(x=min_per_mile_net), color="red", fill="red", alpha=0.3)+
  geom_histogram(data=records_all_df, aes(x=min_per_mile_race), color="blue", fill="blue", alpha=0.3)+
  geom_vline(xintercept = as.numeric(records_all_df[records_all_df$Bib_number=="22579","min_per_mile_net"]), color="red")+
  geom_vline(xintercept = as.numeric(records_all_df[records_all_df$Bib_number=="22579","min_per_mile_race"]), color="blue")


ggplot(records_all_df)+
  geom_density(aes(x=min_per_mile_net), color="red", fill="red", alpha=0.3)+
  geom_density(data=records_all_df, aes(x=min_per_mile_race), color="blue", fill="blue", alpha=0.3)+
  geom_vline(xintercept = as.numeric(records_all_df[records_all_df$Bib_number=="22579","min_per_mile_net"]), color="red")+
  geom_vline(xintercept = as.numeric(records_all_df[records_all_df$Bib_number=="22579","min_per_mile_race"]), color="blue")


ggplot(records_all_df)+
  geom_density(data=records_all_df[records_all_df$category=="女子組",], aes(x=min_per_mile_net), color="pink", fill="pink", alpha=0.6)+
  geom_density(data=records_all_df[records_all_df$category=="男子組",], aes(x=min_per_mile_race), color="blue", fill="blue", alpha=0.3)+
  geom_vline(xintercept = as.numeric(records_all_df[records_all_df$Bib_number=="22579","min_per_mile_net"]), color="green")+
  geom_vline(xintercept = as.numeric(records_all_df[records_all_df$Bib_number=="22579","min_per_mile_race"]), color="orange")
