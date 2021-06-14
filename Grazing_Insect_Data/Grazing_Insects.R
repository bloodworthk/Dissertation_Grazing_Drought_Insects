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

