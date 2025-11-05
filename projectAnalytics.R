##project- Analytics Day
moneyball<- read.csv("~/Downloads/billionaires (1).csv")

library(tidyverse)
library(ggplot2)
##part2
data_specific <- moneyball %>%
  select(name,year,wealth.worth.in.billions,wealth.how.industry,location.region,location.citizenship,company.relationship,demographics.gender,wealth.type)
#use data from latest year:2014
data_latest<-data_specific %>%
  group_by(name)%>%
  filter(year==max(year)) %>%
  mutate(wealth.how.industry = recode(wealth.how.industry,
                                      "Constrution" = "Construction"))%>%
  
  mutate(company.relationship=recode(company.relationship,"relation"="Relation",
                                     "vice-chairman"="Vice Chairman","vice chairman"="Vice Chairman"
                                     ,"investor "="investor","ceo"="CEO","lawer"="lawyer"
                                     ,"chairman"="Chairman", "chariman"="Chairman"
                                     ,"chairman and ceo"="Chairman, CEO"))%>%
  mutate(across(everything(), ~
                  ifelse(is.na(.) | . == "", "Unknown", .)
  ))%>%
  mutate_at(vars(wealth.how.industry,location.region,location.citizenship,company.relationship,demographics.gender,wealth.type), ~recode(., `0` = "Unknown"))%>%
  mutate(company.relationship = ifelse(str_detect(company.relationship, regex("founder", ignore_case = TRUE)),
                                       "Founder", ifelse(str_detect(company.relationship, regex("owner", ignore_case = TRUE)),
                                                             "Owner", ifelse(str_detect(company.relationship, regex("investor", ignore_case = TRUE)),
                                                                             "Investor", 
                                                                             
                ifelse(str_detect(company.relationship, regex("ceo|coo|executive|chairman|president|director|leadership|head|chairwoman", ignore_case = TRUE)),
                                                                                                "CEO", "Others")))))%>%
  mutate(wealth.how.industry=ifelse(str_detect(wealth.how.industry,regex("Venture|banking|services",ignore_case=TRUE)),"Other",wealth.how.industry))
#Make a table of frequencies

freq_industry<-data_latest %>%
  group_by(wealth.how.industry) %>%
  count()%>%
  rename('Frequency' = n,'Industry Type'= wealth.how.industry )
print(freq_industry)

freq_sex<-data_latest%>%
  group_by(demographics.gender)%>%
  count()%>%
  rename('Number of Billionaires'=n,'Sex of Billionaire'=demographics.gender)
print(freq_sex)

freq_wealth<-data_latest%>%
  group_by(wealth.type)%>%
  count()%>%
  rename('Billionaire Wealth Type'=wealth.type,'Frequency'=n)

print(freq_wealth,n=Inf)

freq_location<-data_latest%>%
  group_by(location.region)%>%
  count()%>%
  rename('Billionaires Regional Location'=location.region,'Frequency'=n)
print(freq_location)
freq_citizen<-data_latest%>%
  group_by(location.citizenship)%>%
  count()%>%
  rename('Billionaires Citizenship'=location.citizenship,'Frequency'=n)
print(freq_citizen,n=Inf)

#mean tables
data_mean<-data_latest %>%
  group_by(wealth.how.industry) %>%
  summarise(Mean_Wealth_in_Billions = mean(wealth.worth.in.billions, na.rm = TRUE))
print(data_mean)
ggplot(data_mean, aes(x = wealth.how.industry, y = Mean_Wealth_in_Billions)) +
  geom_col(fill="red") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),plot.title = element_text(hjust = 0.5))+
  labs(title = "Mean Wealth by Industry",
       x = "Industry",
       y = "Mean Wealth") 
data_latest %>%
  group_by(company.relationship) %>%
  summarise(Mean_Wealth_in_Billions  = mean(wealth.worth.in.billions, na.rm = TRUE))
data_latest %>%
  group_by(demographics.gender) %>%
  summarise(Mean_Wealth_in_Billions  = mean(wealth.worth.in.billions, na.rm = TRUE))
data_latest %>%
  group_by(wealth.type) %>%
  summarise(Mean_Wealth_in_Billions  = mean(wealth.worth.in.billions, na.rm = TRUE))
data_latest %>%
  group_by(location.region) %>%
  summarise(Mean_Wealth_in_Billions  = mean(wealth.worth.in.billions, na.rm = TRUE))
data_latest %>%
  group_by(location.citizenship) %>%
  summarise(Mean_Wealth_in_Billions  = mean(wealth.worth.in.billions, na.rm = TRUE))

# industry 
data_latest$wealth.how.industry <- factor(data_latest$wealth.how.industry)
data_latest$wealth.how.industry<- relevel(data_latest$wealth.how.industry, ref = "Technology-Computer")

model <- lm(wealth.worth.in.billions~wealth.how.industry  , data = data_latest)
summary(model)

# company relation

data_latest$company.relationship <- factor(data_latest$company.relationship)
data_latest$company.relationship<- relevel(data_latest$company.relationship, ref = "Founder")

model <- lm(wealth.worth.in.billions~company.relationship  , data = data_latest)
summary(model)

#gender
data_latest$demographics.gender <- factor(data_latest$demographics.gender)
data_latest$demographics.gender<- relevel(data_latest$demographics.gender, ref = "female")

model <- lm(wealth.worth.in.billions~demographics.gender  , data = data_latest)
summary(model)

#wealth type
data_latest$wealth.type <- factor(data_latest$wealth.type)
data_latest$wealth.type<- relevel(data_latest$wealth.type, ref = "inherited")

model <- lm(wealth.worth.in.billions~wealth.type  , data = data_latest)
summary(model)

#region location
data_latest$location.region <- factor(data_latest$location.region)
data_latest$location.region<- relevel(data_latest$location.region, ref = "North America")

model <- lm(wealth.worth.in.billions~location.region  , data = data_latest)
summary(model)


# Final model
model <- lm(wealth.worth.in.billions~wealth.how.industry +demographics.gender+wealth.type+location.region+company.relationship , data = data_latest)
summary(model)
