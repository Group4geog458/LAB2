wac <- read.csv("OK_WAC.csv") # Reading in CSV file
wac$w_geocode<-as.character(wac$w_geocode) # change number to character to later use substring command
wac$w_geocode[which(length(wac$w_geocode) == 14)]<-paste0("0",wac$w_geocode[which(length(wac$w_geocode) == 14)])
wac$work_tract <-substr(wac$w_geocode,1,11) # first to eleventh character in string, making new column equal to the wac geocode without block info

as.numeric(wac$work_tract) # Turns column back to number from string

wac <- wac %>% 
  group_by(work_tract) %>% # Groups by tract (without blocks) column
  select(-w_geocode) # Gets rid of original geocode column

wac2 <- wac %>% # New table from first one
  group_by(work_tract) %>% 
  summarise_all(funs(sum)) # Creates sums from rows