#### Personal Plot 1 Data ####

# Set Working Directory)
setwd("D:/Koerner Lab/Data/Excel Data/R_Ready Data")

#Load Tidyverse#
library(tidyverse)

# Load in data sets #
Sweepnet_weight<-read.csv("2020_Sweep_Net_Weight_Data_FK.csv", header=T)
Sweepnet_ID<-read.csv("2020_Sweep_Net_Data_FK.csv", header=T)
D_Vac_Weight<-read.csv("2020_DVac_Weight_Data_FK.csv", header=T)
D_Vac_ID<-read.csv("2020_DVac_Data_FK.csv", header=T)

####Proofreading Data####

colnames(Sweepnet_ID)
colnames(Sweepnet_weight)
S_ID<-Sweepnet_ID %>%
  
#rename(Grazing_Treatment=?..Grazing_Treatment)#
  
mutate(Block=ifelse(?..Grazing_Treatment=="B3",3,ifelse(?..Grazing_Treatment=="B2",2,1))) %>%
mutate(Grazing_Treatment=Plot) %>%
mutate(Plot=1) %>%
mutate(Dataset="S") %>%
select(-?..Grazing_Treatment)

D_Weight<-D_Vac_Weight %>%
mutate(Grazing_Treatment=?..Grazing_Treatment) %>%
rename(Sample=Sample_num) %>%
filter(Plot==1)%>%
mutate(Dataset="D") %>%
select(-?..Grazing_Treatment)

D_ID<- D_Vac_ID %>%
mutate(Grazing_Treatment=?..Grazing_Treatment) %>%
mutate(Block=ifelse(Block=="B3",3,ifelse(Block=="B2",2,1))) %>%
filter(Plot==1)%>%
mutate(Dataset="D")%>%
select(-?..Grazing_Treatment)

S_Weight<- Sweepnet_weight %>%
mutate(Grazing_Treatment=?..Grazing_Treatment) %>%
mutate(Block=ifelse(Plot=="B3",3,ifelse(Plot=="B2",2,1))) %>%
mutate(Plot=1) %>%
mutate(Dataset="S")%>%
select(-?..Grazing_Treatment)

S_ID<-Sweepnet_ID %>%
  #rename(Grazing_Treatment=?..Grazing_Treatment)%>%#
  mutate(Block=?..Grazing_Treatment) %>%
  mutate(Grazing_Treatment=Plot) %>%
  select(-?..Grazing_Treatment,-Plot)


colnames(D_ID)
colnames(S_ID)
colnames(S_Weight)
colnames(D_Weight)

#### Merge Data ####
#Merge S_ID and D_ID#

ID_Data<- S_ID %>%
rbind(D_ID) %>%
  mutate(Correct_Family=ifelse(Family=="acrididae", "Acrididae",ifelse(Family=="cicadellidae", "Cicadellidae", ifelse(Family=="geocoridae", "Geocordidae", ifelse(Family=="carabidae", "Carabidae", ifelse(Family=="chrysomelidae","Chrysomelidae", ifelse(Family=="formicidae", "Formicidae", ifelse(Family=="halictidae", "Halictidae", ifelse(Family=="agromyzidae", "Agromyzidae", ifelse(Family=="lycosidae", "Lycosidae", ifelse(Family=="platygastridae", "Platygastridae", ifelse(Family=="tettigoniidae", "Tettigoniidae", ifelse(Family=="salticidae", "Salticidae", ifelse(Family=="thomisidae", "Thomisidae", ifelse(Family=="pentatomidae", "Pentatomidae", ifelse(Family=="lygaeidae", "Lygaeidae", ifelse(Family=="scutelleridae", "Scutelleridae", ifelse(Family=="gryllidae", "Gryllidae", ifelse(Family=="asilidae", "Asilidae", ifelse(Family=="chrysididae", "Chrysididae", ifelse(Family=="curculionidae", "Curculionidae", ifelse(Family=="latridiidae","Latridiidae", ifelse(Family=="muscidae", "Muscidae", ifelse(Family=="tenebrionidae", "Tenebrionidae",Family))))))))))))))))))))))))

#Merge S_Weight and D_Weight#

Weight_Data<- D_Weight %>%
rbind(S_Weight)
View(D_ID)
View(D_Weight)
View(S_ID)
View(S_Weight)

#Preparing to make plots by diving data into three separate dataframes. In one, compare family number in Sweepnet vs D-Vac. In another, compare family numbers of the three grazing treatments. In the last, compare Sweepnet vs D-Vac, and how the treatments further separate them from one another#
Family_Num_Dataset<- ID_Data %>%
  group_by(Dataset) %>%
  distinct(Correct_Family) %>%
  filter(Correct_Family!="unknown_1") %>%
  filter(!is.na(Correct_Family)) %>%
  ungroup()

Count_Dataset<-Family_Num_Dataset %>%
  group_by(Dataset) %>%
  summarize(Family_number=length(Correct_Family))%>%
  ungroup()

Family_Num_Treatment<- ID_Data %>%
  group_by(Grazing_Treatment) %>%
  distinct(Correct_Family) %>%
  filter(Correct_Family!="unknown_1") %>%
  filter(!is.na(Correct_Family)) %>%
  ungroup()

Count_Treatment<-Family_Num_Treatment %>%
  group_by(Grazing_Treatment) %>%
  summarize(Family_number=length(Correct_Family))%>%
  ungroup()

Family_Num_Combined_Comparison<- ID_Data %>%
  group_by(Dataset, Grazing_Treatment) %>%
  distinct(Correct_Family) %>%
  filter(Correct_Family!="unknown_1") %>%
  filter(!is.na(Correct_Family)) %>%
  ungroup()

Count_Comparison<-Family_Num_Combined_Comparison %>%
  group_by(Dataset, Grazing_Treatment) %>%
  summarize(Family_number=length(Correct_Family))%>%
  ungroup()

#### Now do genus and species####

Genus_Num_Dataset<- ID_Data %>%
  filter(!is.na(Species)) %>%
  filter(Species!="unknown_1") %>%
  mutate(Correct_Genus=ifelse(Genus=="arphia", "Arphia",ifelse(Genus=="melanoplus", "Melanoplus", ifelse(Genus=="opeia", "Opeia", ifelse(Genus=="ageneotettix ", "Ageneotettix", ifelse(Genus=="phoetaliotes", "Phoetaliotes", ifelse(Genus=="ageneotettix", "Ageneotettix", ifelse(Genus=="Ageneotettix ", "Ageneotettix", ifelse(Genus=="amphiturnus", "Amphiturnus", Genus))))))))) %>%
  mutate(Nomenclature=paste(Correct_Genus, Species, sep = "_")) %>%
#Check incorrect Data in Genus and Species. Look over physical datasheets#
  group_by(Dataset) %>%
  distinct(Nomenclature) %>%
  ungroup()

Count_Genus_Dataset<-Genus_Num_Dataset %>%
  group_by(Dataset) %>%
  summarize(Genus_number=length(Nomenclature))%>%
  ungroup()

Genus_Num_Treatment<-ID_Data %>%
  filter(!is.na(Grazing_Treatment)) %>%
  filter(Species!="unknown_1") %>%
  mutate(Correct_Genus=ifelse(Genus=="arphia", "Arphia",ifelse(Genus=="melanoplus", "Melanoplus", ifelse(Genus=="opeia", "Opeia", ifelse(Genus=="ageneotettix ", "Ageneotettix", ifelse(Genus=="phoetaliotes", "Phoetaliotes", ifelse(Genus=="ageneotettix", "Ageneotettix", ifelse(Genus=="Ageneotettix ", "Ageneotettix", ifelse(Genus=="amphiturnus", "Amphiturnus", Genus))))))))) %>%
  group_by(Dataset) %>%
  distinct(Grazing_Treatment) %>%
  ungroup()

Count_Genus_Treatment<- Genus_Num_Treatment %>%
  group_by(Grazing_Treatment) %>%
  summarize(Number_of_Genus=length(Dataset))%>%
  ungroup()


Genus_Num_Comparison<-ID_Data %>%
  group_by(Dataset, Grazing_Treatment) %>%
  mutate(Correct_Genus=ifelse(Genus=="arphia", "Arphia",ifelse(Genus=="melanoplus", "Melanoplus", ifelse(Genus=="opeia", "Opeia", ifelse(Genus=="ageneotettix ", "Ageneotettix", ifelse(Genus=="phoetaliotes", "Phoetaliotes", ifelse(Genus=="ageneotettix", "Ageneotettix", ifelse(Genus=="Ageneotettix ", "Ageneotettix", ifelse(Genus=="amphiturnus", "Amphiturnus", Genus))))))))) %>%
  distinct(Correct_Genus) %>%
  filter(Correct_Genus!="unknown_1") %>%
  filter(!is.na(Correct_Genus)) %>%
  ungroup()

Count_Genus_Comparison<- Genus_Num_Comparison %>%
  group_by(Dataset, Grazing_Treatment) %>%
  summarize(Number_of_Genus=length(Dataset))%>%
  ungroup()


#### Species ####

Species_Num_Dataset<- ID_Data %>%
  filter(!is.na(Species)) %>%
  filter(Species!="unknown_1") %>%
  group_by(Dataset) %>%
  distinct(Species) %>%
  ungroup()

Count_Species_Dataset<- Species_Num_Dataset %>%
  group_by(Dataset) %>%
  summarize(Species_Number=length(Dataset)) %>%
  ungroup()


Species_Num_Treatment<- ID_Data %>%
  filter(!is.na(Grazing_Treatment)) %>%
  filter(Species!="unknown_1") %>%
  group_by(Grazing_Treatment) %>%
  distinct(Species) %>%
  ungroup()

Count_Species_Treatment<- Species_Num_Treatment %>%
  group_by(Grazing_Treatment) %>%
  summarize(Species_Per_Treatment=length(Grazing_Treatment)) %>%
  ungroup()

Species_Num_Comparison<-ID_Data %>%
  group_by(Dataset, Grazing_Treatment) %>%
  distinct(Species) %>%
  filter(Species!="unknown_1") %>%
  filter(!is.na(Species)) %>%
  ungroup()

Count_Species_Comparison<- Species_Num_Comparison %>%
  group_by(Dataset, Grazing_Treatment) %>%
  summarize(Number_of_Species=length(Dataset))%>%
  ungroup()

