##project- Analytics Day
moneyball<- read.csv("~/Downloads/billionaires (1).csv")

library(tidyverse)
library(ggplot2)
##part2
data_specific <- moneyball %>%
  select(name,year,wealth.worth.in.billions,wealth.how.industry,
         location.region,location.citizenship,company.relationship,demographics.gender,wealth.type)
#use data from latest year
data_latest<-data_specific %>%
  group_by(name)%>%
  filter(year==max(year)) %>%
  mutate(wealth.how.industry = recode(wealth.how.industry,
                                      "Constrution" = "Construction"))%>%
  mutate(demographics.gender = recode(demographics.gender,
                                      "married couple" = "Unknown"))%>%  
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

tab1<-table(data_latest$demographics.gender,data_latest$wealth.how.industry)
pct_tab1 <- round(prop.table(tab1) * 100,1)
print(pct_tab1)

bp1 <- barplot(pct_tab1,
        beside = TRUE,
        col = c("red", "blue","green"),
        legend.text = TRUE,
        main = "Figure 1. Billionaires by Industry Type and Gender (Percentages)",
        xaxt = "n",
        ylab = "Percentage",ylim=c(0,20))
mtext("Industry Type", side = 1, line = 4, cex = 1, font = 2,adj=0.5)
text(x = colMeans(bp1), y = -0.2, labels = colnames(pct_tab1),
     srt = 30, adj = 1, xpd = TRUE,cex=0.7)
grid(lty="dashed")
tab2<-table(data_latest$demographics.gender,data_latest$wealth.type)
pct_tab2 <- round(prop.table(tab2) * 100,1)
print(pct_tab2)

bp2 <- barplot(pct_tab2,
               beside = TRUE,
               col = c("red", "blue","green"),
               legend.text = TRUE,
               main = "Figure 2. Billionaires by Wealth Type and Gender (Percentages)",
               xaxt = "n",
               ylab = "Percentage",ylim=c(0,30))
mtext("Wealth Type", side = 1, line = 4, cex = 1, font = 2)
text(x = colMeans(bp2), y = -0.6, labels = colnames(pct_tab2),
      xpd = TRUE,cex=1.1)
grid(lty="dashed")



#mean tables and Barplot
mean_industry<-data_latest %>%
  group_by(wealth.how.industry) %>%
  summarise(Mean_Wealth_in_Billions = mean(wealth.worth.in.billions, na.rm = TRUE))%>%
  mutate(Mean_Wealth_in_Billions = round(Mean_Wealth_in_Billions, 2))
print(mean_industry)
ggplot(mean_industry, aes(x = wealth.how.industry, y = Mean_Wealth_in_Billions)) +
  geom_col(fill="skyblue") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust = 1,
                                   size=14),plot.title = element_text(hjust = 0.5)
        ,axis.title.x = element_text(hjust=0.45,size=13))+
  geom_text(aes(label = Mean_Wealth_in_Billions), vjust = -0.3, size = 4)+
  labs(title = "Figure 3. Mean Wealth by Industry Type",
       x = "Industry Type",
       y = "Mean Wealth in Billions") 

mean_relationship<-data_latest %>%
  group_by(company.relationship) %>%
  summarise(Mean_Wealth_in_Billions  = mean(wealth.worth.in.billions, na.rm = TRUE))%>%
  mutate(Mean_Wealth_in_Billions = round(Mean_Wealth_in_Billions, 2))
print(mean_relationship)
ggplot(mean_relationship, aes(x = company.relationship, y = Mean_Wealth_in_Billions)) +
  geom_col(fill="red") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust = 1),plot.title = element_text(hjust = 0.5))+
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
  theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust = 1,size=15),
        plot.title = element_text(hjust = 0.5)
        ,axis.title.x = element_text(size=13))+
  geom_text(aes(label = Mean_Wealth_in_Billions), vjust = -0.3, size = 4)+
  labs(title = "Figure 4. Mean Wealth by Gender",
       x = "Gender",
       y = "Mean Wealth in Billions") 

mean_wealthtype<-data_latest %>%
  group_by(wealth.type) %>%
  summarise(Mean_Wealth_in_Billions  = mean(wealth.worth.in.billions, na.rm = TRUE))%>%
  mutate(Mean_Wealth_in_Billions = round(Mean_Wealth_in_Billions, 2))
print(mean_wealthtype)
ggplot(mean_wealthtype, aes(x = wealth.type, y = Mean_Wealth_in_Billions)) +
  geom_col(fill="gold") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust = 1,
                                   size=15),plot.title = element_text(hjust = 0.5),
        axis.title = element_text(hjust = 0.4, size=12))+
  geom_text(aes(label = Mean_Wealth_in_Billions), vjust = -0.3, size = 4)+
  labs(title = "Figure 5. Mean Wealth by Wealth Type",
       x = "Wealth Type",
       y = "Mean Wealth in Billions") 

mean_region<-data_latest %>%
  group_by(location.region) %>%
  summarise(Mean_Wealth_in_Billions  = mean(wealth.worth.in.billions, na.rm = TRUE))%>%
  mutate(Mean_Wealth_in_Billions = round(Mean_Wealth_in_Billions, 2))
print(mean_region)
ggplot(mean_region, aes(x = location.region, y = Mean_Wealth_in_Billions)) +
  geom_col(fill="darkgreen") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust = 1
                                   ,size=14),plot.title = element_text(hjust = 0.5),
        axis.title = element_text(hjust = 0.5, size=13))+
  geom_text(aes(label = Mean_Wealth_in_Billions), vjust = -0.3, size = 4)+
  labs(title = "Figure 6. Mean Wealth by Geographic Region",
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

