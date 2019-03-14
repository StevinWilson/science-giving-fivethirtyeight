#Data from https://github.com/fivethirtyeight/data/tree/master/science-giving
#This datasets was originally obtained from the Federal Election Commission and contains information of donations during the period of 2007-2016 filtered for the occupations that can be braodly classified under the labels "Science" or "Engineering". For more details about the filtered occupations, visit the above Github repo page.
#Only considering donations to GOP or Democratic campaigns. Other parties are miniscule in the number of voters and donations, but not in big ideas.


library(tidyverse)

library(urbnmapr)

'%!in%' <- function(x,y)!('%in%'(x,y))
giving_file <- file.choose()

science_giving <- read_csv(giving_file)

typeof(science_giving)
head(science_giving)
class(science_giving)
str(science_giving)
science_giving$cleanedoccupation <- as.factor(science_giving$cleanedoccupation)
science_giving$classification <- as.factor(science_giving$classification)
science_giving$cmte_pty <- as.factor(science_giving$cmte_pty)
science_giving$cand_office <- as.factor(science_giving$cand_office)

levels(science_giving$cand_office)
levels(science_giving$cand_office) <- c('House','NULL', 'President', 'Senate')
levels(science_giving$cmte_pty)

#Selected data only for donations to GOP and Democratic campaigns
science_giving <- science_giving%>%
  filter(cmte_pty %in% c("dem",  "Dem",  "DEM", "D", "GOP","R","rep","Rep","REP"))


science_giving$cmte_pty <- factor(science_giving$cmte_pty)
levels(science_giving$cmte_pty ) <- c('DEM',  'DEM',  'DEM', 'DEM', 'REP','REP','REP','REP','REP')


#All donations from "Scientists" towards election campaigns 
scientist_giving <- science_giving %>%
  select(cleaned_name , cmte_pty, `2016_dollars`,state, cand_office, classification) %>%
  filter(cmte_pty %in% c("DEM","REP")) %>%
  filter(cand_office != "NULL") %>%
  filter(classification == "Scientist") %>%
  filter(`2016_dollars` > 0)

#All donations from "Engineers" towards election campaigns 
engineer_giving <- science_giving %>%
  select(cleaned_name , cmte_pty, `2016_dollars`,state, cand_office, classification) %>%
  filter(cmte_pty %in% c("DEM","REP")) %>%
  filter(cand_office != "NULL") %>%
  filter(classification == "Engineer") %>%
  filter(`2016_dollars` > 0)

head(scientist_giving)

head(engineer_giving)

#Bar charts indicating party-wise and office-wise donations from "Scientists"
ggplot(scientist_giving, aes(x = cmte_pty,y = `2016_dollars`/1000000 , fill = cmte_pty))+
  stat_summary(fun.y = 'sum', geom = 'bar')+
  scale_fill_manual(values=c("#002868", "#bf0a30"), name = "Candidate Party", labels = c("DEM", "GOP"))+
  facet_wrap(~cand_office)+
  ylim(0,10)+
  labs(title = "Donations from 'Scientists' [2007 - 2016]" , x = "Candidate Party" , y = "USD (in millions)" , caption = "Amounts adjusted to 2016 inflation" )


#Bar charts indicating party-wise and office-wise donations from "Engineers"
ggplot(engineer_giving, aes(x = cmte_pty,y = `2016_dollars`/1000000 , fill = cmte_pty))+
  stat_summary(fun.y = 'sum', geom = 'bar')+
  scale_fill_manual(values=c("#002868", "#bf0a30"),name = "Candidate Party", labels = c("DEM", "GOP"))+
  facet_wrap(~cand_office)+
  ylim(0,30)+
  labs(title = "Donations from 'Engineers' [2007 - 2016]" , x = "Candidate Party" , y = "USD (in millions)" , caption = "Amounts adjusted to 2016 inflation" )


#Box plots indicating the median party-wise and office-wise donations from "Scientists". Extreme Outliers omitted.

ggplot(scientist_giving, aes(x = cmte_pty,y = `2016_dollars` , fill = cmte_pty))+
  geom_boxplot(color = "dark green")+
  scale_fill_manual(values=c("#002868", "#bf0a30"),name = "Candidate Party", labels = c("DEM", "GOP"))+
  facet_wrap(~cand_office)+
  ylim(0,10000)+
  labs(title = "Distribution of donations from 'Scientists' [2007 - 2016]" , x = "Candidate Party" , y = "Amount in USD" , caption = "Amounts adjusted to 2016 inflation" )


#Box plots indicating the median party-wise and office-wise donations from "Engineers". Extreme Outliers omitted.

ggplot(engineer_giving, aes(x = cmte_pty,y = `2016_dollars` , fill = cmte_pty))+
  geom_boxplot(color = "dark green")+
  scale_fill_manual(values=c("#002868", "#bf0a30"),name = "Candidate Party", labels = c("DEM", "GOP"))+
  facet_wrap(~cand_office)+
  ylim(0,10000)+
  labs(title = "Distribution of donations from 'Engineers' [2007 - 2016]" , x = "Candidate Party" , y = "Amount in USD" , caption = "Amounts adjusted to 2016 inflation" )


#Donations from "Scientists" in Ohio towards election campaigns 

scientist_giving_OH <- science_giving %>%
  select(cleaned_name , cmte_pty, `2016_dollars`,state, cand_office, classification) %>%
  filter(cmte_pty == "DEM" | cmte_pty == "REP" ) %>%
  filter(cand_office != "NULL") %>%
  filter(classification == "Scientist") %>%
  filter(`2016_dollars` > 0) %>%
  filter(state == "OH")

#Donations from "Engineers" in Ohio towards election campaigns 

engineer_giving_OH <- science_giving %>%
  select(cleaned_name , cmte_pty, `2016_dollars`,state, cand_office, classification) %>%
  filter(cmte_pty == "DEM" | cmte_pty == "REP" ) %>%
  filter(cand_office != "NULL") %>%
  filter(classification == "Engineer") %>%
  filter(`2016_dollars` > 0) %>%
  filter(state == "OH")

str(scientist_giving_OH)
str(engineer_giving_OH)

#Bar charts indicating party-wise and office-wise donations from "Scientists" in Ohio

ggplot(scientist_giving_OH, aes(x = cmte_pty,y = `2016_dollars`/1000 , fill = cmte_pty))+
  stat_summary(fun.y = 'sum', geom = 'bar')+
  scale_fill_manual(values=c("#002868", "#bf0a30"), name = "Candidate Party", labels = c("DEM", "GOP"))+
  facet_wrap(~cand_office)+
  labs(title = "Donations from 'Scientists' in Ohio [2007 - 2016]" , x = "Candidate Party" , y = "USD (in thousands)" , caption = "Amounts adjusted to 2016 inflation" )


#Bar charts indicating party-wise and office-wise donations from "Engineers" in Ohio

ggplot(engineer_giving_OH, aes(x = cmte_pty,y = `2016_dollars`/1000 , fill = cmte_pty))+
  stat_summary(fun.y = 'sum', geom = 'bar')+
  scale_fill_manual(values=c("#002868", "#bf0a30"), name = "Candidate Party", labels = c("DEM", "GOP"))+
  facet_wrap(~cand_office)+
  labs(title = "Donations from 'Engineers' in Ohio [2007 - 2016]" , x = "Candidate Party" , y = "USD (in thousands)" , caption = "Amounts adjusted to 2016 inflation" )


#Box plots indicating the median party-wise and office-wise donations from "Scientists" in Ohio

ggplot(scientist_giving_OH, aes(x = cmte_pty,y = `2016_dollars` , fill = cmte_pty))+
  geom_boxplot()+
  scale_fill_manual(values=c("#002868", "#bf0a30"),name = "Candidate Party", labels = c("DEM", "GOP"))+
  facet_wrap(~cand_office)+
  labs(title = "Distribution of donations from 'Scientists' in Ohio [2007 - 2016]" , x = "Candidate Party" , y = "Amount in USD" , caption = "Amounts adjusted to 2016 inflation" )


#Box plots indicating the median party-wise and office-wise donations from "Engineers" in Ohio

ggplot(engineer_giving_OH, aes(x = cmte_pty,y = `2016_dollars` , fill = cmte_pty))+
  geom_boxplot()+
  scale_fill_manual(values=c("#002868", "#bf0a30"),name = "Candidate Party", labels = c("DEM", "GOP"))+
  facet_wrap(~cand_office)+
  labs(title = "Distribution of donations from 'Engineers' in Ohio [2007 - 2016]" , x = "Candidate Party" , y = "Amount in USD" , caption = "Amounts adjusted to 2016 inflation" )




#Campaigns with the highest amount in donations
science_giving$cand_name<- as.factor(science_giving$cand_name)
top_campaigns <- science_giving %>%
  filter(!is.na(cand_name)) %>%
  group_by(cand_name) %>%
  tally(`2016_dollars`)%>%
  arrange(desc(n))%>%
  top_n(10) 
head(top_campaigns,10)

ggplot(data = top_campaigns, aes(x = reorder(cand_name, -n), y = n/1000000))+
  geom_bar(stat = "identity" , fill = "black")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylim(0,20)+
  labs(title = "Most favored candidates among 'Scientists' & 'Engineers' [2007 - 2016]" , x = "Candidate" , y = "USD (in millions)" , caption = "Amounts adjusted to 2016 inflation" )


#Campaigns with the highest amount in donations from "Scientists"
top_campaigns_scientists <- science_giving %>%
  filter(!is.na(cand_name)) %>%
  filter(classification == "Scientist")
top_campaigns_scientists %>%
  group_by(cand_name) %>%
  tally(`2016_dollars`)%>%
  arrange(desc(n))%>%
  top_n(10) %>%
  ggplot(aes(x = reorder(cand_name, -n), y = n/1000000))+
  geom_bar(stat = "identity", fill = "black")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylim(0,6)+
  labs(title = "Most favored candidates among 'Scientists' [2007 - 2016]" , x = "Candidate" , y = "USD (in millions)" , caption = "Amounts adjusted to 2016 inflation" )


#Campaigns with the highest amount in donations from "Engineers"
top_campaigns_engineers <- science_giving %>%
  filter(!is.na(cand_name)) %>%
  filter(classification == "Engineer") 


top_campaigns_engineers %>%
  group_by(cand_name) %>%
  tally(`2016_dollars`)%>%
  arrange(desc(n))%>%
  top_n(10) %>%
  ggplot(aes(x = reorder(cand_name, -n), y = n/1000000))+
  geom_bar(stat = "identity", fill = "black")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylim(0,12.5)+
  labs(title = "Most favored candidates among 'Engineers' [2007 - 2016]" , x = "Candidate" , y = "USD (in millions)" , caption = "Amounts adjusted to 2016 inflation" )

  


#Campaigns with the highest amount in donations from "Scientists" in Ohio
top_campaigns_scientists_OH <- science_giving %>%
  filter(!is.na(cand_name)) %>%
  filter(classification == "Scientist")%>%
  filter(state == "OH")
top_campaigns_scientists_OH %>%
  group_by(cand_name) %>%
  tally(`2016_dollars`)%>%
  arrange(desc(n))%>%
  top_n(10) %>%
  ggplot(aes(x = reorder(cand_name, -n), y = n/1000))+
  geom_bar(stat = "identity", fill = "dark green")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylim(0,60)+
  labs(title = "Most favored candidates among 'Scientists' in Ohio [2007 - 2016]" , x = "Candidate" , y = "USD (in thousands)" , caption = "Amounts adjusted to 2016 inflation" )




#Campaigns with the highest amount in donations from "Engineers" in Ohio


top_campaigns_engineers_OH <- science_giving %>%
  filter(!is.na(cand_name)) %>%
  filter(classification == "Engineer") %>%
  filter(state == "OH")

top_campaigns_engineers_OH %>%
  group_by(cand_name) %>%
  tally(`2016_dollars`)%>%
  arrange(desc(n))%>%
  top_n(10) %>%
  ggplot(aes(x = reorder(cand_name, -n), y = n/1000))+
  geom_bar(stat = "identity", fill = "dark green")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylim(0,125)+
  labs(title = "Most favored candidates among 'Engineers' in Ohio [2007 - 2016]" , x = "Candidate" , y = "USD (in thousands)" , caption = "Amounts adjusted to 2016 inflation" )



#Distribution of median donation amount to GOP campaigns from "Scientists"

science_giving %>%
  select(`2016_dollars`,state, classification, cmte_pty) %>%
  filter(classification=="Scientist") %>%
  filter(cmte_pty == "REP") %>%
  filter(state %!in% c("1" ,"AA" ,"AE" ,"AP" ,"AS", "FM","GU","MH", "MP","PW","PR","VI" )) %>%
  group_by(state)%>%
  summarize(median_amt = median(`2016_dollars`, na.rm = TRUE)) %>%
  mutate("state_abbv" = state)%>%
  filter(!is.na(state_abbv)) %>%
  left_join(states, by = "state_abbv") %>% 
  ggplot(mapping = aes(long, lat, group = group, fill = median_amt)) +
  geom_polygon(color = "#ffffff", size = .25) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45)+
  scale_fill_gradient(low = "pink", high = "#800000",name = "Median amount (in USD)")+
  labs(title = "Distribution of median donation amount from 'Scientists' - GOP " , x = "Longitude" , y = "Latitude" , caption = "Amounts adjusted to 2016 inflation" )


#Distribution of median donation amount to Democratic campaigns from "Scientists"

science_giving %>%
  select(`2016_dollars`,state, classification, cmte_pty) %>%
  filter(classification=="Scientist") %>%
  filter(cmte_pty == "DEM") %>%
  filter(state %!in% c("1" ,"AA" ,"AE" ,"AP" ,"AS", "FM","GU","MH", "MP","PW","PR","VI" )) %>%
  group_by(state)%>%
  summarize(median_amt = median(`2016_dollars`, na.rm = TRUE)) %>%
  mutate("state_abbv" = state)%>%
  filter(!is.na(state_abbv)) %>%
  left_join(states, by = "state_abbv") %>% 
  ggplot(mapping = aes(long, lat, group = group, fill = median_amt)) +
  geom_polygon(color = "#ffffff", size = .25) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45)+
  scale_fill_gradient(low = "cyan", high = "navy",name = "Median amount (in USD)")+
  labs(title = "Distribution of median donation amount from 'Scientists' - Democrats" , x = "Longitude" , y = "Latitude" , caption = "Amounts adjusted to 2016 inflation" )


#Distribution of median donation amount to GOP campaigns from "Engineers"

science_giving %>%
  select(`2016_dollars`,state, classification, cmte_pty) %>%
  filter(classification=="Engineer") %>%
  filter(cmte_pty == "REP") %>%
  filter(state %!in% c("1" ,"AA" ,"AE" ,"AP" ,"AS", "FM","GU","MH", "MP","PW","PR","VI" )) %>%
  group_by(state)%>%
  summarize(median_amt = median(`2016_dollars`, na.rm = TRUE)) %>%
  mutate("state_abbv" = state)%>%
  filter(!is.na(state_abbv)) %>%
  left_join(states, by = "state_abbv") %>% 
  ggplot(mapping = aes(long, lat, group = group, fill = median_amt)) +
  geom_polygon(color = "#ffffff", size = .25) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45)+
  scale_fill_gradient(low = "pink", high = "#800000",name = "Median amount (in USD)")+
  labs(title = "Distribution of median donation amount from 'Engineers' - GOP " , x = "Longitude" , y = "Latitude" , caption = "Amounts adjusted to 2016 inflation" )


#Distribution of median donation amount to Democratic campaigns from "Engineers"

science_giving %>%
  select(`2016_dollars`,state, classification, cmte_pty) %>%
  filter(classification=="Engineer") %>%
  filter(cmte_pty == "DEM") %>%
  filter(state %!in% c("1" ,"AA" ,"AE" ,"AP" ,"AS", "FM","GU","MH", "MP","PW","PR","VI" )) %>%
  group_by(state)%>%
  summarize(median_amt = median(`2016_dollars`, na.rm = TRUE)) %>%
  mutate("state_abbv" = state)%>%
  filter(!is.na(state_abbv)) %>%
  left_join(states, by = "state_abbv") %>% 
  ggplot(mapping = aes(long, lat, group = group, fill = median_amt)) +
  geom_polygon(color = "#ffffff", size = .25) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45)+
  scale_fill_gradient(low = "cyan", high = "navy",name = "Median amount (in USD)")+
  labs(title = "Distribution of median donation amount from 'Engineers' - Democrats" , x = "Longitude" , y = "Latitude" , caption = "Amounts adjusted to 2016 inflation" )



#State-wise distribution of donations towards GOP campaigns from "Scientists"

science_giving %>%
  select(`2016_dollars`,state, classification, cmte_pty) %>%
  filter(classification=="Scientist") %>%
  filter(cmte_pty == "REP") %>%
  filter(state %!in% c("1" ,"AA" ,"AE" ,"AP" ,"AS", "FM","GU","MH", "MP","PW","PR","VI" )) %>%
  group_by(state)%>%
  summarize(sum_amt = sum(`2016_dollars`, na.rm = TRUE)/1000000) %>%
  mutate("state_abbv" = state)%>%
  filter(!is.na(state_abbv)) %>%
  left_join(states, by = "state_abbv") %>% 
  ggplot(mapping = aes(long, lat, group = group, fill = sum_amt)) +
  geom_polygon(color = "#ffffff", size = .25) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45)+
  scale_fill_gradient(low = "pink", high = "#800000",name = "USD (in millions)")+
  labs(title = "Total donations from 'Scientists' - GOP" , x = "Longitude" , y = "Latitude" , caption = "Amounts adjusted to 2016 inflation" )


#State-wise distribution of donations towards Democratic campaigns from "Scientists"

science_giving %>%
  select(`2016_dollars`,state, classification, cmte_pty) %>%
  filter(classification=="Scientist") %>%
  filter(cmte_pty == "DEM") %>%
  filter(state %!in% c("1" ,"AA" ,"AE" ,"AP" ,"AS", "FM","GU","MH", "MP","PW","PR","VI" )) %>%
  group_by(state)%>%
  summarize(sum_amt = sum(`2016_dollars`, na.rm = TRUE)/1000000) %>%
  mutate("state_abbv" = state)%>%
  filter(!is.na(state_abbv)) %>%
  left_join(states, by = "state_abbv") %>% 
  ggplot(mapping = aes(long, lat, group = group, fill = sum_amt)) +
  geom_polygon(color = "#ffffff", size = .25) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45)+
  scale_fill_gradient(low = "cyan", high = "navy",name = "USD (in millions)")+
  labs(title = "Total donations from 'Scientists' - Democrats" , x = "Longitude" , y = "Latitude" , caption = "Amounts adjusted to 2016 inflation" )


#State-wise distribution of donations towards GOP campaigns from "Engineers"

science_giving %>%
  select(`2016_dollars`,state, classification, cmte_pty) %>%
  filter(classification=="Engineer") %>%
  filter(cmte_pty == "REP") %>%
  filter(state %!in% c("1" ,"AA" ,"AE" ,"AP" ,"AS", "FM","GU","MH", "MP","PW","PR","VI" )) %>%
  group_by(state)%>%
  summarize(sum_amt = sum(`2016_dollars`, na.rm = TRUE)/1000000) %>%
  mutate("state_abbv" = state)%>%
  filter(!is.na(state_abbv)) %>%
  left_join(states, by = "state_abbv") %>% 
  ggplot(mapping = aes(long, lat, group = group, fill = sum_amt)) +
  geom_polygon(color = "#ffffff", size = .25) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45)+
  scale_fill_gradient(low = "pink", high = "#800000",name = "USD (in millions)")+
  labs(title = "Total donations from 'Engineers' - GOP" , x = "Longitude" , y = "Latitude" , caption = "Amounts adjusted to 2016 inflation" )


#State-wise distribution of donations towards Democratic campaigns from "Engineers"
science_giving %>%
  select(`2016_dollars`,state, classification, cmte_pty) %>%
  filter(classification=="Engineer") %>%
  filter(cmte_pty == "DEM") %>%
  filter(state %!in% c("1" ,"AA" ,"AE" ,"AP" ,"AS", "FM","GU","MH", "MP","PW","PR","VI" )) %>%
  group_by(state)%>%
  summarize(sum_amt = sum(`2016_dollars`, na.rm = TRUE)/1000000) %>%
  mutate("state_abbv" = state)%>%
  filter(!is.na(state_abbv)) %>%
  left_join(states, by = "state_abbv") %>% 
  ggplot(mapping = aes(long, lat, group = group, fill = sum_amt)) +
  geom_polygon(color = "#ffffff", size = .25) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45)+
  scale_fill_gradient(low = "cyan", high = "navy",name = "USD (in millions)")+
  labs(title = "Total donations from 'Engineers' - Democrats" , x = "Longitude" , y = "Latitude" , caption = "Amounts adjusted to 2016 inflation" )


#Median and total donations from "Scientists" with overseas military addresses.
science_giving %>%
  select(`2016_dollars`,state, classification, cmte_pty) %>%
  filter(classification=="Scientist") %>%
  filter(cmte_pty %in% c("DEM", "REP")) %>%
  filter(state %in% c("AA" ,"AE" ,"AP")) %>%
  group_by(cmte_pty)%>%
  summarize(median_amt = median(`2016_dollars`, na.rm = TRUE) , sum_amt = sum(`2016_dollars`, na.rm = TRUE))

#Median and total donations from "Engineers" with overseas military addresses.
science_giving %>%
  select(`2016_dollars`,state, classification, cmte_pty) %>%
  filter(classification=="Engineer") %>%
  filter(cmte_pty %in% c("DEM", "REP")) %>%
  filter(state %in% c("AA" ,"AE" ,"AP")) %>%
  group_by(cmte_pty)%>%
  summarize(median_amt = median(`2016_dollars`, na.rm = TRUE) , sum_amt = sum(`2016_dollars`, na.rm = TRUE))

