##project- Analytics Day
moneyball<- read.csv("~/Downloads/billionaires (1).csv")

library(tidyverse)
library(ggplot2)
##part2
data_specific <- moneyball %>%
  select(name,year,wealth.worth.in.billions,wealth.how.industry,
         location.region,location.citizenship,company.relationship,demographics.gender,wealth.type)
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
  mutate(company.relationship = ifelse(str_detect(company.relationship,
                                                  regex("founder", ignore_case = TRUE)),
                                    
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

ggplot(data_latest, aes(x = wealth.how.industry,fill=location.region)) +   
  geom_bar(position = "dodge") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  geom_text(
    stat = "count",
    aes(label = ..count..),
    position = position_dodge(width = 0.9),
    vjust = -0.3,
    size = 3
  )+
  labs( title="Group Bar Plot of Industry Type vs Regional Location",x = "Industry Type",y = "Frequency",
        caption="We want to determine which industry do most billionaires come from.
        Consumer Industries have most frequency since the they have the largest bars in the plot.")

hist(log(data_latest$wealth.worth.in.billions),xlim=c(0,8),breaks=70,main="Histogram of Logarithmic Transform of Billionaires Wealth in Billions",xlab="Log Wealth in Billions",ylab="Frequency",
     sub="By Logarithmic Transformation, the histogram appears right-skewed")
ggplot(data_latest, aes(x = wealth.type)) +   
  geom_bar(fill = "red") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),plot.title = element_text(hjust = 0.5)) +
  labs( title="Group Bar Plot of Wealth Type",x = "Wealth Type",y = "Frequency",
        caption="Of the Billionaires, we see most of them inherited their wealth.")
ggplot(data_latest, aes(x = demographics.gender,fill=wealth.type)) +   
  geom_bar(position= "dodge") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),plot.title = element_text(hjust = 0.5)) +
  geom_text(
    stat = "count",
    aes(label = ..count..),
    position = position_dodge(width = 0.9),
    vjust = -0.3,
    size = 3
  )+
  labs( title="Group Bar Plot of Billionaire Gender vs Wealth Type",x = "Gender",y = "Frequency",
        caption="Of the Billionaires collected, most are male.")
ggplot(data_latest, aes(x = location.region,fill=company.relationship)) +   
  geom_bar(position="dodge") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),plot.title = element_text(hjust = 0.5)) +
  geom_text(
    stat = "count",
    aes(label = ..count..),
    position = position_dodge(width = 0.9),
    vjust = -0.3,
    size = 3
  )+
  labs( title="Group Bar Plot of Billionaires' Location",x = "Region Location",y = "Frequency",
        caption="Most billionaires come from North America, Europe, and East Asia.")
ggplot(data_latest, aes(x = company.relationship,fill=demographics.gender)) +   
  geom_bar(position = "dodge") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),plot.title = element_text(hjust = 0.5)) +
  geom_text(
    stat = "count",
    aes(label = ..count..),
    position = position_dodge(width = 0.9),
    vjust = -0.3,
    size = 3
  )+
  labs( title="Group Bar Plot of Billionaire Relationship with Company",x = "Relationship",y = "Frequency",
        caption="Most billionaires are founders and relations.")

ggplot(data_latest, aes(x = location.citizenship)) +   
  geom_bar(fill = "gold") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),plot.title = element_text(hjust = 0.5)) +
  labs( title="Frequency Bar Plot of Billionaire Citizenship",x = "Citizenship",y = "Frequency",
        caption="Most billionaires are United States citizens. There is slight majority of billionaires from China, Germany, and Russia.")


ggplot(data_latest, aes(x = wealth.how.industry, y = wealth.worth.in.billions),fill=wealth.how.industry) +
  geom_boxplot(fill="purple") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),plot.title = element_text(hjust = 0.5))+
  labs(caption="Multiple outliers for multiple industries.Largest outlier appears for industry Technology-Computer",title = "Side by Side Boxplots for Profit by Industry Type", x = "Industry Type", y = "Profit Wealth of Billionaires in Billions")


#mean tables and Barplot
mean_industry<-data_latest %>%
  group_by(wealth.how.industry) %>%
  summarise(Mean_Wealth_in_Billions = mean(wealth.worth.in.billions, na.rm = TRUE))%>%
  mutate(Mean_Wealth_in_Billions = round(Mean_Wealth_in_Billions, 2))
print(mean_industry)
ggplot(mean_industry, aes(x = wealth.how.industry, y = Mean_Wealth_in_Billions)) +
  geom_col(fill="skyblue") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),plot.title = element_text(hjust = 0.5))+
  geom_text(aes(label = Mean_Wealth_in_Billions), vjust = -0.3, size = 4)+
  labs(title = "Mean Wealth by Industry Type",
       x = "Industry Type",
       y = "Mean Wealth in Billions") 

mean_relationship<-data_latest %>%
  group_by(company.relationship) %>%
  summarise(Mean_Wealth_in_Billions  = mean(wealth.worth.in.billions, na.rm = TRUE))%>%
  mutate(Mean_Wealth_in_Billions = round(Mean_Wealth_in_Billions, 2))
print(mean_relationship)
ggplot(mean_relationship, aes(x = company.relationship, y = Mean_Wealth_in_Billions)) +
  geom_col(fill="red") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),plot.title = element_text(hjust = 0.5))+
  geom_text(aes(label = Mean_Wealth_in_Billions), vjust = -0.3, size = 4)+
  labs(title = "Mean Wealth by Company Relationship",
       x = "Company Relationship",
       y = "Mean Wealth in Billions") 
mean_sex<-data_latest %>%
  group_by(demographics.gender) %>%
  summarise(Mean_Wealth_in_Billions  = mean(wealth.worth.in.billions, na.rm = TRUE))%>%
  mutate(Mean_Wealth_in_Billions = round(Mean_Wealth_in_Billions, 2))
print(mean_sex)
ggplot(mean_sex, aes(x = demographics.gender, y = Mean_Wealth_in_Billions)) +
  geom_col(fill="orange") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),plot.title = element_text(hjust = 0.5))+
  geom_text(aes(label = Mean_Wealth_in_Billions), vjust = -0.3, size = 4)+
  labs(title = "Mean Wealth by Gender",
       x = "Gender",
       y = "Mean Wealth in Billions") 

mean_wealthtype<-data_latest %>%
  group_by(wealth.type) %>%
  summarise(Mean_Wealth_in_Billions  = mean(wealth.worth.in.billions, na.rm = TRUE))%>%
  mutate(Mean_Wealth_in_Billions = round(Mean_Wealth_in_Billions, 2))
print(mean_wealthtype)
ggplot(mean_wealthtype, aes(x = wealth.type, y = Mean_Wealth_in_Billions)) +
  geom_col(fill="gold") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),plot.title = element_text(hjust = 0.5))+
  geom_text(aes(label = Mean_Wealth_in_Billions), vjust = -0.3, size = 4)+
  labs(title = "Mean Wealth by Wealth Type",
       x = "Wealth Type",
       y = "Mean Wealth in Billions") 

mean_region<-data_latest %>%
  group_by(location.region) %>%
  summarise(Mean_Wealth_in_Billions  = mean(wealth.worth.in.billions, na.rm = TRUE))%>%
  mutate(Mean_Wealth_in_Billions = round(Mean_Wealth_in_Billions, 2))
print(mean_region)
ggplot(mean_region, aes(x = location.region, y = Mean_Wealth_in_Billions)) +
  geom_col(fill="darkgreen") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),plot.title = element_text(hjust = 0.5))+
  geom_text(aes(label = Mean_Wealth_in_Billions), vjust = -0.3, size = 4)+
  labs(title = "Mean Wealth by Geographic Region",
       x = "Region Location",
       y = "Mean Wealth in Billions") 


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




