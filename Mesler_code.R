# reading in RAC csv and manipulating the df
RAC_df=read.csv(file="ok_rac_okcounty_only.csv")
RAC=as_tibble(RAC_df)

RAC$h_geocode<-as.character(RAC$h_geocode)

# create separate column with geocodes only to tract level (got rid of block #'s)
RAC$tract_geocode<-substr(RAC$h_geocode,1,11)

# creating new dataframe with a single summarized entry/row for each tract 
as.numeric(RAC$tract_geocode)

RAC <- RAC %>%
  group_by(tract_geocode) %>%
  select(-createdate & -h_geocode)

RAC2 <- RAC %>%
  group_by(tract_geocode) %>%
  summarise_all(funs(sum))

