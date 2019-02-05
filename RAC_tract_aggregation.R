# reading in RAC csv and manipulating the df
RAC_df=read.csv(file="ok_rac_v1_OKcounty.csv")
RAC=as_tibble(RAC_df)

# change number to character to later use substring command
RAC$h_geocode<-as.character(RAC$h_geocode) 
RAC$h_geocode[which(length(RAC$h_geocode) == 14)]<-paste0("0",RAC$h_geocode[which(length(RAC$h_geocode) == 14)])

# first to eleventh character in string, making new column equal to the wac geocode without block info
RAC$tract_geocode<-substr(RAC$h_geocode,1,11) 

# creating new dataframe with a single summarized entry/row for each tract 
as.numeric(RAC$tract_geocode)

RAC <- RAC %>%
  group_by(tract_geocode) %>%
  select(-h_geocode)

RAC2 <- RAC %>%
  group_by(tract_geocode) %>%
  summarise_all(funs(sum))

# new csv of just aggregated tact level data with summed columns for each tract
write.csv(RAC2, 'RAC_tract_aggregation.csv', row.names=FALSE)