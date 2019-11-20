library(dplyr)
library(ggplot2)
library(ggthemes)

# setwd("")#specify according to your needs

giardia_trend <- read.csv("assemblageb_trend.csv", head=T)


#Just filtering the data for the cases over the years
assemblageb_cases1 <- giardia_trend %>%
  filter(study_year ==1,
         case== "Cases",
         giardia_mix == "Negative",
         giardia_a=="Negative") %>%
  group_by(study_year, study, case, giardia_assemblage) %>%
  summarise(subjects= n())%>%
  mutate(subjects_by_year=sum(subjects), freq = round(((subjects/subjects_by_year)*100),1))

assemblageb_cases2 <- giardia_trend %>%
  filter(study_year ==2,
         case== "Cases",
         giardia_mix == "Negative",
         giardia_a=="Negative") %>%
  group_by(study_year, study, case, giardia_assemblage) %>%
  summarise(subjects= n())%>%
  mutate(subjects_by_year=sum(subjects), freq = round(((subjects/subjects_by_year)*100),1))  

assemblageb_cases3 <- giardia_trend %>%
  filter(study_year ==3,
         case== "Cases",
         giardia_mix == "Negative",
         giardia_a=="Negative") %>%
  group_by(study_year, study, case, giardia_assemblage) %>%
  summarise(subjects= n())%>%
  mutate(subjects_by_year=sum(subjects), freq = round(((subjects/subjects_by_year)*100),1))

assemblageb_cases4 <- giardia_trend %>%
  filter(study_year ==4,
         case== "Cases",
         giardia_mix == "Negative",
         giardia_a=="Negative") %>%
  group_by(study_year, study, case, giardia_assemblage) %>%
  summarise(subjects= n())%>%
  mutate(subjects_by_year=sum(subjects), freq = round(((subjects/subjects_by_year)*100),1))

assemblageb_cases5lsd <- giardia_trend %>%
  filter(study_year ==5,
         case== "Cases",
         giardia_mix == "Negative",
         giardia_a=="Negative",
         study=="LSD") %>%
  group_by(study_year, study, case, giardia_assemblage) %>%
  summarise(subjects= n())%>%
  mutate(subjects_by_year=sum(subjects), freq = round(((subjects/subjects_by_year)*100),1))

assemblageb_cases5msd <- giardia_trend %>%
  filter(study_year ==5,
         case== "Cases",
         giardia_mix == "Negative",
         giardia_a=="Negative",
         study=="MSD") %>%
  group_by(study_year, study, case, giardia_assemblage) %>%
  summarise(subjects= n())%>%
  mutate(subjects_by_year=sum(subjects), freq = round(((subjects/subjects_by_year)*100),1))

#Combining all the parsed datafram into one single dataframe
assemblageb_cases <- rbind.data.frame(assemblageb_cases1,
                                      assemblageb_cases2,
                                      assemblageb_cases3,
                                      assemblageb_cases4,
                                      assemblageb_cases5msd,
                                      assemblageb_cases5lsd,
                                      make.row.names = F) 


#Removing the parsed dataframes
rm(assemblageb_cases1,
   assemblageb_cases2,
   assemblageb_cases3,
   assemblageb_cases4,
   assemblageb_cases5lsd,
   assemblageb_cases5msd)

#Just filtering the data for the controls over the years
assemblageb_controls1 <- giardia_trend %>%
  filter(study_year ==1,
         case== "Controls",
         giardia_mix == "Negative",
         giardia_a=="Negative") %>%
  group_by(study_year, study, case, giardia_assemblage) %>%
  summarise(subjects= n())%>%
  mutate(subjects_by_year=sum(subjects), freq = round(((subjects/subjects_by_year)*100),1))

assemblageb_controls2 <- giardia_trend %>%
  filter(study_year ==2,
         case== "Controls",
         giardia_mix == "Negative",
         giardia_a=="Negative") %>%
  group_by(study_year, study, case, giardia_assemblage) %>%
  summarise(subjects= n())%>%
  mutate(subjects_by_year=sum(subjects), freq = round(((subjects/subjects_by_year)*100),1))  

assemblageb_controls3 <- giardia_trend %>%
  filter(study_year ==3,
         case== "Controls",
         giardia_mix == "Negative",
         giardia_a=="Negative") %>%
  group_by(study_year, study, case, giardia_assemblage) %>%
  summarise(subjects= n())%>%
  mutate(subjects_by_year=sum(subjects), freq = round(((subjects/subjects_by_year)*100),1))

assemblageb_controls4 <- giardia_trend %>%
  filter(study_year ==4,
         case== "Controls",
         giardia_mix == "Negative",
         giardia_a=="Negative") %>%
  group_by(study_year, study, case, giardia_assemblage) %>%
  summarise(subjects= n())%>%
  mutate(subjects_by_year=sum(subjects), freq = round(((subjects/subjects_by_year)*100),1))

assemblageb_controls5lsd <- giardia_trend %>%
  filter(study_year ==5,
         case== "Controls",
         giardia_mix == "Negative",
         giardia_a=="Negative",
         study=="LSD") %>%
  group_by(study_year, study, case, giardia_assemblage) %>%
  summarise(subjects= n())%>%
  mutate(subjects_by_year=sum(subjects), freq = round(((subjects/subjects_by_year)*100),1))

assemblageb_controls5msd <- giardia_trend %>%
  filter(study_year ==5,
         case== "Controls",
         giardia_mix == "Negative",
         giardia_a=="Negative",
         study=="MSD") %>%
  group_by(study_year, study, case, giardia_assemblage) %>%
  summarise(subjects= n())%>%
  mutate(subjects_by_year=sum(subjects), freq = round(((subjects/subjects_by_year)*100),1))

#Combining all the parsed datafram into one single dataframe
assemblageb_controls <- rbind.data.frame(assemblageb_controls1,
                                         assemblageb_controls2,
                                         assemblageb_controls3,
                                         assemblageb_controls4,
                                         assemblageb_controls5msd,
                                         assemblageb_controls5lsd,
                                         make.row.names = F) 


#Removing the parsed dataframes
rm(assemblageb_controls1,
   assemblageb_controls2,
   assemblageb_controls3,
   assemblageb_controls4,
   assemblageb_controls5lsd,
   assemblageb_controls5msd)

#Combining the small table
assemblageb <- rbind.data.frame(assemblageb_cases,
                                assemblageb_controls,
                                make.row.names = F)

#Preparing the final table for the plot
assemblage_evolution <- assemblageb[assemblageb$giardia_assemblage=="Assemblage B",] #for some reason filter using dplyr didn't work here.

# Make sure there is only one count per element
counts <- assemblage_evolution %>%
  group_by(study_year, case, study) %>% tally %>% arrange(desc(n))
counts
# message('WHY ARE THERE TWO OBSERVATIONS EACH FOR STUDY YEAR 5 - CASES, AND FOR STUDY YEAR 5 - CONTROLS???')
# message('For now, arbitrarily removing (but Augusto should fix this)')
# # Removing...
# pd <- assemblage_evolution %>%
#   group_by(study_year, case, study) %>%
#   mutate(n = n()) %>%
#   ungroup %>%
#   filter(n == 1)

# # Calculat p values
study_years <- sort(unique(pd$study_year))
studies <- sort(unique(pd$study))

pd$chi <- NA
for(a in 1:length(study_years)){
  for(b in 1:length(studies)){
    this_study_year <- study_years[a]
    this_study <- studies[b]
    these_data <- pd %>% filter(study_year == this_study_year,
                                study == this_study)
    if(nrow(these_data) == 2){
      chi <- chisq.test(t(as.matrix(these_data[,c('subjects', 'subjects_by_year')])))
      chi <- chi$p.value
      # Plug in
      pd$chi[pd$study == this_study & pd$study_year == this_study_year] <- chi
    }
  }
}

# # Put into format of augusto's table
# library(tidyr)
# augusto <- pd %>% dplyr::select(-giardia_assemblage) %>%
#   mutate(year_study = paste0('Year ', study_year, ' - ', study)) %>%
#   ungroup %>%
#   dplyr::select(-study_year, -study) %>%
#   mutate(val = paste0(subjects, ' / ',  subjects_by_year, ' (', freq, ')', ' p: ', round(chi, digits = 3))) %>%
#   dplyr::select(-subjects, -subjects_by_year, -freq, -chi) %>%
#   spread(key = case, value = val)

# Get p values from assemblageb
# # Calculat p values
study_years <- sort(unique(pd$study_year))
studies <- sort(unique(pd$study))

p <- assemblageb
p$chi <- NA
for(a in 1:length(study_years)){
  for(b in 1:length(studies)){
    this_study_year <- study_years[a]
    this_study <- studies[b]
    these_data <- pd %>% filter(study_year == this_study_year,
                                study == this_study)
    if(nrow(these_data) == 2){
      chi <- chisq.test((as.matrix(these_data[,c('subjects', 'subjects_by_year')])))
      chi <- chi$p.value
      # Plug in
      p$chi[p$study == this_study & p$study_year == this_study_year] <- chi
    }
  }
}
library(tidyr)
augusto <- p %>% ungroup %>% 
  mutate(year_study = paste0('Year ', study_year, ' - ', study)) %>%
  ungroup %>%
  dplyr::select(-study_year, -study) %>%
  mutate(val = paste0(subjects, ' / ',  subjects_by_year, ' (', freq, ')', ' p: ', round(chi, digits = 3))) %>%
  # mutate(case = ifelse(giardia_assemblage == 'Assemblage B', 'Positive', 'Negative')) %>%
  filter(giardia_assemblage != 'Negative') %>%
  dplyr::select(-giardia_assemblage) %>%
  dplyr::select(-subjects, -subjects_by_year, -freq, -chi) %>%
  spread(key = case, value = val) %>%
  mutate(Cases = unlist(lapply(strsplit(Cases, ' p:', fixed = TRUE), function(x){x[1]})))


library(cowplot)
ggplot(data = pd,
       aes(x=study_year, y = freq, fill = case)) +
  geom_bar(stat = 'identity', width = 0.7, position = position_dodge(width = 0.8), alpha= 0.6)+
  facet_wrap(~study, ncol = 1)+
  labs(x= "GEMS Study Year",
       y = "Frequency (%)",
       caption="*Statistically significant differences in Year 5")+
  theme_cowplot()+
  scale_fill_manual(name="Study group", 
                      values = c('darkred', 'black'),
                      labels = c("Symptomatic", "Asymptomatic"))+#Symptomatic == "Cases" & Asymptomatic == "Controls"
  theme(text=element_text(family = "Times", color = "black"),
        legend.position = "right",
        legend.title = element_text(family = "Times", face = "bold", size = 12, color = "black"),
        legend.text = element_text(family = "Times", size = 11),
        panel.grid.minor.y = element_line(colour="light grey", size=0.05, linetype = 2),
        panel.grid.major.y = element_line(colour="light grey", size=0.05),
        panel.grid.minor.x = element_line(colour = "light grey", size = 0.05, linetype = 2),
        axis.title = element_text(family = "Times", face = "bold", size = 12),
        axis.text = element_text(family = "Times", face = "bold", size = 10, color = "black"),
        strip.text = element_text(family = "Times", face = "bold", size = 12),
        plot.caption = element_text(hjust = 1, size =11, family = "Times"))+
  scale_y_continuous(limits = c(0,70)) +
  # Add labels
  geom_text(data = pd,
            aes(x = study_year,
                y = freq + 5,
                group = case,
                label = round(freq, digits = 1)),
            # nudge_y = 5,
            alpha = 0.6,
            position = position_dodge(width = 0.8)) +
  # Add p-values
  geom_text(data = pd %>% dplyr::distinct(study_year, study, .keep_all = TRUE),
            aes(x = study_year,
                y = 5,
                label = paste0('(p=', round(chi, digits = 2), ')',
                               ifelse(chi <= 0.05, '*', ''))),
            color = 'black',
            alpha = 0.7)

