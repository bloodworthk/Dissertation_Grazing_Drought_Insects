#### Grazing x Insect Data - 2020 ####
#### Code created by: Kathryn Bloodworth and Will Mann ####
#Date started: 06/14/2021 #

#### Set working directory and load libraries ####

# Set Working Directory)
setwd("/Users/kathrynbloodworth/Dropbox (Smithsonian)/Projects/Dissertation/Data/Insect_Data")

#Load Tidyverse#
library(tidyverse)

#### Load in data ####
Sweepnet_weight<-read.csv("2020_Sweep_Net_Weight_Data_FK.csv", header=T)
Sweepnet_ID<-read.csv("2020_Sweep_Net_Data_FK.csv", header=T)
D_Vac_Weight<-read.csv("2020_DVac_Weight_Data_FK.csv", header=T)
D_Vac_ID<-read.csv("2020_DVac_Data_FK.csv", header=T)

#### Formatting Data ####

#make new dataframe for sweepnet ID changing block to numerical counts, and remnaming incorrect columns and adding dataset type and plot number (1) to columns
S_ID<-Sweepnet_ID %>% 
mutate(Block=ifelse(Grazing_Treatment=="B3",3,ifelse(Grazing_Treatment=="B2",2,1))) %>%
  mutate(Grazing_Treatment=Plot) %>%
  mutate(Plot=1) %>%
  mutate(Dataset="S")

# make new dataframe for sweepnet weights, changing block to correct denotion, adding plot number and dataset type
S_Weight<- Sweepnet_weight %>%
  mutate(Block=ifelse(Plot=="B3",3,ifelse(Plot=="B2",2,1))) %>%
  mutate(Plot=1) %>%
  mutate(Dataset="S")

# make new dataframe for Dvac ID changing block to correct denotion, and adding in dataset type
D_ID<- D_Vac_ID %>%
  mutate(Block=ifelse(Block=="B3",3,ifelse(Block=="B2",2,1))) %>%
  mutate(Dataset="D")

#make new dataframe for Dvac weights, renaming sample number so it is consistant and adding in dataset type
D_Weight<-D_Vac_Weight %>%
  rename(Sample=Sample_num) %>%
  mutate(Dataset="D")

#Merge S_ID and D_ID and fix names#
ID_Data<- S_ID %>%
  rbind(D_ID) %>%
  mutate(Correct_Class=ifelse(Class=="insecta","Insecta",ifelse(Class=="arachnida","Arachnida",Class))) %>% 
  mutate(Correct_Order=ifelse(Order=="orthoptera","Orthoptera",ifelse(Order=="hemiptera","Hemiptera",ifelse(Order=="coleoptera","Coleoptera",ifelse(Order=="hymenoptera","Hymenoptera",ifelse(Order=="diptera","Diptera",ifelse(Order=="araneae","Araneae",Order))))))) %>% 
  mutate(Correct_Family=ifelse(Family=="acrididae", "Acrididae",ifelse(Family=="cicadellidae", "Cicadellidae", ifelse(Family=="geocoridae", "Geocordidae", ifelse(Family=="carabidae", "Carabidae", ifelse(Family=="chrysomelidae","Chrysomelidae", ifelse(Family=="formicidae", "Formicidae", ifelse(Family=="halictidae", "Halictidae", ifelse(Family=="agromyzidae", "Agromyzidae", ifelse(Family=="lycosidae", "Lycosidae", ifelse(Family=="platygastridae", "Platygastridae", ifelse(Family=="tettigoniidae", "Tettigoniidae", ifelse(Family=="salticidae", "Salticidae", ifelse(Family=="thomisidae", "Thomisidae", ifelse(Family=="pentatomidae", "Pentatomidae", ifelse(Family=="lygaeidae", "Lygaeidae", ifelse(Family=="scutelleridae", "Scutelleridae", ifelse(Family=="gryllidae", "Gryllidae", ifelse(Family=="asilidae", "Asilidae", ifelse(Family=="chrysididae", "Chrysididae", ifelse(Family=="curculionidae", "Curculionidae", ifelse(Family=="latridiidae","Latridiidae", ifelse(Family=="muscidae", "Muscidae", ifelse(Family=="tenebrionidae", "Tenebrionidae",Family)))))))))))))))))))))))) %>% 
  mutate(Correct_Genus=ifelse(Genus=="Melanoplus","Melanoplus",ifelse(Genus=="arphia","Arphia",ifelse(Genus=="melanoplus","Melanoplus",ifelse(Genus=="opeia","Opeia",ifelse(Genus=="nenconocephalus","Neoconocephalus",ifelse(Genus=="pachybrachis","Pachybrachis",ifelse(Genus=="ageneotettix ","Ageneotettix", ifelse(Genus=="phoetaliotes","Phoetaliotes",ifelse(Genus=="Ageneotettix ","Ageneotettix",ifelse(Genus=="amphiturnus","Amphiturnus",ifelse(Genus=="Ageneotettox","Ageneotettix",ifelse(Genus=="Agneotettix","Ageneotettix",ifelse(Genus=="ageneotettix","Ageneotettix",Genus)))))))))))))) %>% 
  mutate(Correct_Species=ifelse(Species=="differentalis","differentialis",ifelse(Species=="sanguinipes","sanguinipes",ifelse(Species=="packardi","packardii",ifelse(Species=="unknown","sp",Species))))) %>% 
  select(-Sub.order,-Class,-Family,-Genus,-Order,-Species)

#Merge S_Weight and D_Weight#
Weight_Data<- D_Weight %>%
  rbind(S_Weight)

#seperate out Orthoptera ID into seperate Datasheet
Orthoptera_ID_Data<-ID_Data %>% 
  filter(Correct_Order=="Orthoptera")

unique(Orthoptera_ID_Data$Correct_Genus)



