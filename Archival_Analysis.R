##Archival Tracker 
## Library 

library(tidyverse)
library(ggplot2)
library(janitor)

data <- read.csv("CJ_Archival_Tracker.csv")%>%
 filter(!is.na(Year.of.publication))

data <- data%>%
  mutate(ScoreGroup_Procedural = case_when(Procedural == "D" |Procedural == "S" ~ "High", 
                                           Procedural == "N" |Procedural == "Z" |Procedural == "Z/N"|Procedural == "N/Z"  ~ "Low"))%>%
  mutate(ScoreGroup_Distributive = case_when(Distributive == "D" |Distributive == "S" | Distributive == "S/D"| Distributive == "N/S" ~ "High", 
                                             Distributive == "N" |Distributive == "Z" ~ "Low"))%>%
  mutate(ScoreGroup_Intergenerational = case_when(Intergenerational == "D" |Intergenerational == "S" | Intergenerational == "N/S" ~ "High", 
                                                  Intergenerational == "N" |Intergenerational == "Z" | Intergenerational == "z" ~ "Low"))%>%
  mutate(ScoreGroup_Diverse.perspectives = case_when(Diverse.perspectives == "D" |Diverse.perspectives == "S" | Diverse.perspectives == "N/S" ~ "High", 
                                                   Diverse.perspectives == "N" |Diverse.perspectives == "Z"  ~ "Low"))
data$Year.of.publication <- as.factor(data$Year.of.publication) 


##Import the features data 
Features <- read.csv("Features.csv")%>%
  rename(Diverse.perspectives = Diverse.persectives)

Features_norepeat <- Features%>%
  filter(!(Repeat. == "Yes"))

Features_norepeat  <- Features_norepeat %>%
  mutate(ScoreGroup_Procedural = case_when(Procedural == "D" |Procedural == "S" ~ "High", 
                                           Procedural == "N" |Procedural == "Z" |Procedural == "Z/N"|Procedural == "N/Z"  ~ "Low"))%>%
  mutate(ScoreGroup_Distributive = case_when(Distributive == "D" |Distributive == "S" | Distributive == "S/D"| Distributive == "N/S" ~ "High", 
                                             Distributive == "N" |Distributive == "Z" ~ "Low"))%>%
  mutate(ScoreGroup_Intergenerational = case_when(Intergenerational == "D" |Intergenerational == "S" | Intergenerational == "N/S" ~ "High", 
                                                  Intergenerational == "N" |Intergenerational == "Z" | Intergenerational == "z" ~ "Low"))%>%
  mutate(ScoreGroup_Diverse.perspectives = case_when(Diverse.perspectives == "D" |Diverse.perspectives == "S" | Diverse.perspectives == "N/S" ~ "High", 
                                                     Diverse.perspectives == "N" |Diverse.perspectives == "Z"  ~ "Low"))
Features_norepeat$Year.of.publication <- as.factor(Features_norepeat$Year.of.publication) 


##Procedural 
archival_summary_Procedural <- data%>%
  select(Year.of.publication, ScoreGroup_Procedural)

Features_summary_Procedural <- Features_norepeat%>%
  select(Year.of.publication, ScoreGroup_Procedural)
  
data_summary_Procedural <- rbind(archival_summary_Procedural,Features_summary_Procedural )

data_summary_Procedural_proportion <- data_summary_Procedural%>%
  filter(!is.na(ScoreGroup_Procedural))%>%
group_by(Year.of.publication, ScoreGroup_Procedural) %>%
  summarise (n = n()) %>%
  mutate(freq = (n / sum(n))*100)%>%
  filter(ScoreGroup_Procedural == "High")%>%
  ungroup()%>%
  add_row(Year.of.publication = "2014",ScoreGroup_Procedural = "High", n = 0, freq = 0)%>%
  add_row(Year.of.publication = "2015",ScoreGroup_Procedural = "High", n = 0, freq = 0)%>%
  add_row(Year.of.publication = "2018",ScoreGroup_Procedural = "High", n = 0, freq = 0)


##Distributive  

archival_summary_Distributive <- data%>%
  select(Year.of.publication, ScoreGroup_Distributive)

Features_summary_Distributive <- Features_norepeat%>%
  select(Year.of.publication, ScoreGroup_Distributive)

data_summary_Distributive <- rbind(archival_summary_Distributive,Features_summary_Distributive)

data_summary_Distributive_proportion <- data_summary_Distributive%>%
  filter(!is.na(ScoreGroup_Distributive))%>%
  group_by(Year.of.publication, ScoreGroup_Distributive) %>%
  summarise (n = n()) %>%
  mutate(freq = (n / sum(n))*100)%>%
  filter(ScoreGroup_Distributive == "High")%>%
  ungroup()%>%
  add_row(Year.of.publication = "2014",ScoreGroup_Distributive = "High", n = 0, freq = 0)%>%
  add_row(Year.of.publication = "2015",ScoreGroup_Distributive = "High", n = 0, freq = 0)%>%
  add_row(Year.of.publication = "2018",ScoreGroup_Distributive = "High", n = 0, freq = 0)


##Intergenerational  
archival_summary_Intergenerational  <- data%>%
  select(Year.of.publication, ScoreGroup_Intergenerational )

Features_summary_Intergenerational  <- Features_norepeat%>%
  select(Year.of.publication, ScoreGroup_Intergenerational )

data_summary_Intergenerational  <- rbind(archival_summary_Intergenerational ,Features_summary_Intergenerational )

data_summary_Intergenerational_proportion <- data_summary_Intergenerational %>%
  filter(!is.na(ScoreGroup_Intergenerational ))%>%
  group_by(Year.of.publication, ScoreGroup_Intergenerational ) %>%
  summarise (n = n()) %>%
  mutate(freq = (n / sum(n))*100)%>%
  filter(ScoreGroup_Intergenerational  == "High")%>%
  ungroup()%>%
  add_row(Year.of.publication = "2014",ScoreGroup_Intergenerational  = "High", n = 0, freq = 0)%>%
  add_row(Year.of.publication = "2015",ScoreGroup_Intergenerational = "High", n = 0, freq = 0)%>%
  add_row(Year.of.publication = "2019",ScoreGroup_Intergenerational  = "High", n = 0, freq = 0)



##Diverse.perspectives 
archival_summary_Diverse.perspectives   <- data%>%
  select(Year.of.publication, ScoreGroup_Diverse.perspectives )

Features_summary_Diverse.perspectives  <- Features_norepeat%>%
  select(Year.of.publication, ScoreGroup_Diverse.perspectives  )

data_summary_Diverse.perspectives  <- rbind(archival_summary_Diverse.perspectives ,Features_summary_Diverse.perspectives )

data_summary_Diverse.perspectives_proportion <- data_summary_Diverse.perspectives  %>%
  filter(!is.na(ScoreGroup_Diverse.perspectives ))%>%
  group_by(Year.of.publication, ScoreGroup_Diverse.perspectives  ) %>%
  summarise (n = n()) %>%
  mutate(freq = (n / sum(n))*100)%>%
  filter(ScoreGroup_Diverse.perspectives  == "High")%>%
  ungroup()%>%
  add_row(Year.of.publication = "2014",ScoreGroup_Diverse.perspectives   = "High", n = 0, freq = 0)%>%
  add_row(Year.of.publication = "2015",ScoreGroup_Diverse.perspectives = "High", n = 0, freq = 0)%>%
  add_row(Year.of.publication = "2018",ScoreGroup_Diverse.perspectives   = "High", n = 0, freq = 0)



###
data_summary_Distributive_proportion <- data_summary_Distributive_proportion%>%
  mutate(Dimension = "Distributive")%>%
  rename(ScoreGroup = ScoreGroup_Distributive)

data_summary_Diverse.perspectives_proportion <- data_summary_Diverse.perspectives_proportion%>%
  mutate(Dimension = "Diverse perspectives")%>%
  rename(ScoreGroup = ScoreGroup_Diverse.perspectives)

data_summary_Procedural_proportion <- data_summary_Procedural_proportion%>%
  mutate(Dimension = "Procedural")%>%
  rename(ScoreGroup = ScoreGroup_Procedural)

data_summary_Intergenerational_proportion <- data_summary_Intergenerational_proportion%>%
  mutate(Dimension = "Intergenerational")%>%
  rename(ScoreGroup = ScoreGroup_Intergenerational)

##Combine dataframes
data_summary_total <- rbind(data_summary_Distributive_proportion, data_summary_Diverse.perspectives_proportion,data_summary_Procedural_proportion, data_summary_Intergenerational_proportion  )
anno <- data.frame(x1 = c(1.75, 0.75), x2 = c(2.25, 1.25), 
                   y1 = c(36, 36), y2 = c(37, 37), 
                   xstar = c(2, 1), ystar = c(38, 38),
                   lab = c("***", "**"),
                   region = c("North", "South"))

ggplot(data_summary_total , aes(x = Year.of.publication, y = freq, fill = Dimension)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#C5D3BB", "#F3DDC4", "#C4B0CA","#C7D5DD"))+
  facet_wrap(~Dimension) +
  xlab("Year of Publication") +ylab ("High scoring publications (%)") +
  theme_classic()+
  theme(legend.position = "none") +
  geom_segment(aes(x = 10.6, xend = 11.5, y = 20, yend = 20), linetype = 3, size = 0.5) +
  annotate("text", x = 11, y = 22, label = "*", size = 5)

ggsave("Combined_facet.png", width = 8)


