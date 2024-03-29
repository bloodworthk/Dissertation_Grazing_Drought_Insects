#### Grazing x Insect Data - 2020 ####
#### Code created by: Kathryn Bloodworth and Will Mann ####
#Date started: 06/14/2021 # adapted and restarted 07/13/2022

#### Set working directory and load libraries ####

# Set Working Directory - Mac
setwd("~/Library/CloudStorage/Box-Box/Projects/Dissertation/Data/Insect_Data")

# Set Working Directory - PC
setwd("C:/Users/kjbloodw/Box/Projects/Dissertation/Data/Insect_Data")

#Load Tidyverse#
library(tidyverse)
#install.packages("scales")
library(scales)
library(vegan)
library(lmerTest)
library(nationalparkcolors)

#Set ggplot2 theme to black and white
theme_set(theme_bw())
#Update ggplot2 theme - make box around the x-axis title size 30, vertically justify x-axis title to 0.35, Place a margin of 15 around the x-axis title.  Make the x-axis title size 30. For y-axis title, make the box size 30, put the writing at a 90 degree angle, and vertically justify the title to 0.5.  Add a margin of 15 and make the y-axis text size 25. Make the plot title size 30 and vertically justify it to 2.  Do not add any grid lines.  Do not add a legend title, and make the legend size 20
theme_update(panel.grid.major=element_blank(),
             panel.grid.minor=element_blank())

#### Load in data and make sure columns names are consistant ####

#ID Data
ID_Data_20<-read.csv("2020_Sweep_Net_Dvac_Data_FK.csv",header=T)
ID_Data_21<-read.csv("2021_Sweep_Net_Dvac_Data_FK.csv",header=T)

#Weight Data
Weight_Data_20<-read.csv("2020_Sweep_Net_D-Vac_Weight_Data_FK.csv",header=T) %>% 
  rename(Sample_Number=Sample_num)
Weight_Data_21<-read.csv("2021_Sweep_Net_D-Vac_Weight_Data_FK.csv",header=T)

#### Formatting and Cleaning ID Data ####


ID_20<-ID_Data_20 %>% 
  #Change block and grazing treatment to be consistent
  mutate(Block=ifelse(Block=="B1",1,ifelse(Block=="B2",2,ifelse(Block=="B3",3,Block)))) %>% 
  #correct misspellings and inconsistencies in order data
  mutate(Correct_Order=ifelse(Order=="orthoptera","Orthoptera",ifelse(Order=="hemiptera","Hemiptera",ifelse(Order=="coleoptera","Coleoptera",ifelse(Order=="hymenoptera","Hymenoptera",ifelse(Order=="diptera","Diptera",ifelse(Order=="araneae","Araneae",Order))))))) %>% 
  #correct misspellings and inconsistencies in order data
  mutate(Correct_Family=ifelse(Family=="acrididae", "Acrididae",ifelse(Family=="cicadellidae", "Cicadellidae", ifelse(Family=="geocoridae", "Geocordidae", ifelse(Family=="carabidae", "Carabidae", ifelse(Family=="chrysomelidae","Chrysomelidae", ifelse(Family=="formicidae", "Formicidae", ifelse(Family=="halictidae", "Halictidae", ifelse(Family=="agromyzidae", "Agromyzidae", ifelse(Family=="lycosidae", "Lycosidae", ifelse(Family=="platygastridae", "Platygastridae", ifelse(Family=="tettigoniidae", "Tettigoniidae", ifelse(Family=="salticidae", "Salticidae", ifelse(Family=="thomisidae", "Thomisidae", ifelse(Family=="pentatomidae", "Pentatomidae", ifelse(Family=="lygaeidae", "Lygaeidae", ifelse(Family=="scutelleridae", "Scutelleridae", ifelse(Family=="gryllidae", "Gryllidae", ifelse(Family=="asilidae", "Asilidae", ifelse(Family=="chrysididae", "Chrysididae", ifelse(Family=="curculionidae", "Curculionidae", ifelse(Family=="latridiidae","Latridiidae", ifelse(Family=="muscidae", "Muscidae", ifelse(Family=="tenebrionidae", "Tenebrionidae",ifelse(Family=="Lygacidae","Lygaeidae",ifelse(Family=="Salticide","Salticidae", Family)))))))))))))))))))))))))) %>% 
  mutate(Correct_Genus=ifelse(Genus=="Melanoplus","Melanoplus",ifelse(Genus=="arphia","Arphia",ifelse(Genus=="melanoplus","Melanoplus",ifelse(Genus=="opeia","Opeia",ifelse(Genus=="nenconocephalus","Neoconocephalus",ifelse(Genus=="pachybrachis","Pachybrachis",ifelse(Genus=="ageneotettix ","Ageneotettix", ifelse(Genus=="phoetaliotes","Phoetaliotes",ifelse(Genus=="Ageneotettix ","Ageneotettix",ifelse(Genus=="amphiturnus","Amphiturnus",ifelse(Genus=="Ageneotettox","Ageneotettix",ifelse(Genus=="Agneotettix","Ageneotettix",ifelse(Genus=="ageneotettix","Ageneotettix",Genus)))))))))))))) %>% 
  mutate(Correct_Species=ifelse(Species=="differentalis","differentialis",ifelse(Species=="sanguinipes","sanguinipes",ifelse(Species=="packardi","packardii",ifelse(Species=="unknown","sp",ifelse(Species=="pachardii","packardii",ifelse(Species=="sanguinpes","sanguinipes",Species))))))) %>% 
  #remove unnecessary columns and reoder
  dplyr::select(Collection_Method,Year,Block,Grazing_Treatment,Plot,Sample,Correct_Order,Correct_Family,Correct_Genus,Correct_Species,Notes) %>% 
  #make sample # numeric instead of character 
  mutate(Sample=as.numeric(Sample))###why are there na's? check data

ID_21<-ID_Data_21 %>% 
  #Change block and grazing treatment to be consistent and match plot numbers
  mutate(Block=ifelse(Block=="B1",1,ifelse(Block=="B2",2,ifelse(Block=="B3",3,Block)))) %>% 
  #adding plot#1 for all sweepnet plots - making it plot 100 so that i can differentiate in if else statements below
  mutate(Plot=replace_na(Plot,100)) %>% 
  #Remove extra 100s placed by previous code
  filter(!is.na(Year)) %>% 
  #fix block numbers
  mutate(Block=ifelse(Plot<=15,1,ifelse(Plot==16,2,ifelse(Plot==17,2,ifelse(Plot==18,2,ifelse(Plot==19,2,ifelse(Plot==20,2,ifelse(Plot==21,2,ifelse(Plot==22,2,ifelse(Plot==23,2,ifelse(Plot==24,2,ifelse(Plot==25,2,ifelse(Plot==26,2,ifelse(Plot==27,2,ifelse(Plot==28,2,ifelse(Plot==29,2,ifelse(Plot==30,2,ifelse(Plot==31,3,ifelse(Plot==32,3,ifelse(Plot==33,3,ifelse(Plot==34,3,ifelse(Plot==35,3,ifelse(Plot==36,3,ifelse(Plot==37,3,ifelse(Plot==38,3,ifelse(Plot==39,3,ifelse(Plot==40,3,ifelse(Plot==41,3,ifelse(Plot==43,3,ifelse(Plot==43,3,ifelse(Plot==44,3,ifelse(Plot==45,3,Block))))))))))))))))))))))))))))))))%>% 
  #change grazing treatments to be consistent
  mutate(Grazing_Treatment=ifelse(Grazing_Treatment=="LG ","LG",Grazing_Treatment)) %>% 
  #correct misspellings and inconsistencies in order data
  mutate(Correct_Order=ifelse(Order=="Aranea ","Araneae",ifelse(Order=="Hemiptera ","Hemiptera",ifelse(Order=="Araneae ","Araneae",ifelse(Order=="Coleopetra","Coleoptera",ifelse(Order=="Coleoptera ","Coleoptera",ifelse(Order=="Hymenoptera ","Hymenoptera",ifelse(Order=="Hymeonptera","Hymenoptera",ifelse(Order=="Orthoptera ","Orthoptera",Order))))))))) %>% 
  #correct misspellings and inconsistencies in order data
  mutate(Correct_Family=ifelse(Family=="Acridiae", "Acrididae",ifelse(Family=="Agramyzidae", "Agromyzidae", ifelse(Family=="Coleoptera ", "Coleoptera", ifelse(Family=="Currulianidae", "Curculionidae", ifelse(Family=="Ligidae","Lygaeidae", ifelse(Family=="Scuttelleridae", "Scutelleridae", ifelse(Family=="Scutelleridae ", "Scutelleridae", ifelse(Family=="staphylinidae", "Staphylinidae", ifelse(Family=="Thamisidae", "Thomisidae", ifelse(Family=="Thomsidae", "Thomisidae", ifelse(Family=="Formicide", "Formicidae", Family))))))))))))%>% 
  mutate(Correct_Genus=ifelse(Genus=="longipennis","Longipennis",ifelse(Genus=="Opcia","Opeia",ifelse(Genus=="melanoplus","Melanoplus",ifelse(Genus=="opeia","Opeia",ifelse(Genus=="Phoetaliotes ","Phoetaliotes",Genus)))))) %>% 
  mutate(Correct_Species=ifelse(Species=="bru","bruneri",ifelse(Species=="Bruneri","bruneri",ifelse(Species=="Bruneri ","bruneri",ifelse(Species=="confuscus","confusus",ifelse(Species=="Confusus","confusus",ifelse(Species=="Curtipennis","curtipennis",ifelse(Species=="Deorum","deorum",ifelse(Species=="differntialis","differentialis",ifelse(Species=="Gladstoni","gladstoni",ifelse(Species=="Hebrascensis","nebrascensis",ifelse(Species=="Infantilis","infantilis",ifelse(Species=="Keeleri","keeleri",ifelse(Species=="Nebrascensis","nebrascensis",ifelse(Species=="Obscrua","obscura",ifelse(Species=="Obscura ","obscura",ifelse(Species=="Obscuria","obscura",ifelse(Species=="Pseudonietara","pseudonietana",ifelse(Species=="Pseudonietena","pseudonietana",ifelse(Species=="Sanguinipes","sanguinipes",ifelse(Species=="Simplex","simplex",ifelse(Species=="Angustipennis","angustipennis",Species)))))))))))))))))))))) %>% 
  #remove unnecessary columns and reoder
  dplyr::select(Collection_Method,Year,Block,Grazing_Treatment,Plot,Sample,Correct_Order,Correct_Family,Correct_Genus,Correct_Species,Notes) %>% 
  mutate(Sample=as.numeric(Sample))

#Merge together data frames

ID_Data_Official<-ID_20_Correct %>% 
  rbind(ID_21)

####Check data
#2020 Sweep Net check B3, NG,Plot 100 sample 17 - Ageneotettix deorum or Phoetaliotes nebranscensis?, sweep net check B3, NG,Plot 100 sample 18 - Phoetaliotes nebranscensis melanoplus gladstoni?
#2021 - Dvac check B2, HG,Plot 29 sample 2 - Ageneotettix deorum or arphia pseudonietana, Dvac check B1, HG,Plot 15 sample 5 - Ageneotettix deorum or melanoplus something?

#### Formatting and Cleaning Weight Data ####

Weight_20<-Weight_Data_20 %>%
  #change blocks to be numeric
  mutate(Block=ifelse(Block=="B1",1,ifelse(Block=="B2",2,ifelse(Block=="B3",3,Block)))) %>% 
  #Correct order spellings
  mutate(Correct_Order=ifelse(Order=="Aranaea","Araneae",ifelse(Order=="Aranea","Araneae",ifelse(Order=="Hempitera","Hemiptera",ifelse(Order=="Cicadellidae","Hemiptera",ifelse(Order=="Lyaceidae","Hemiptera",ifelse(Order=="","Orthoptera",Order))))))) %>%
  #fix NA issue related to body parts
  mutate(Correct_Order=ifelse(Notes=="Body Parts","Body_Parts",ifelse(Notes=="Body parts","Body_Parts",ifelse(Notes=="unknown","unknown",Correct_Order)))) %>%
  #remove unnecessary columns and reoder
  dplyr::select(Collection_Method,Year,Block,Grazing_Treatment,Plot,Sample_Number,Correct_Order,Dry_Weight_g,Notes)




Weight_21<-Weight_Data_21 %>%  
  #change grazing treatments to be correct
  mutate(Grazing_Treatment=ifelse(Grazing_Treatment=="LG ","LG",ifelse(Grazing_Treatment=="LH","LG",Grazing_Treatment))) %>% 
  #change blocks to be numeric
  mutate(Block=ifelse(Block=="B1",1,ifelse(Block=="B2",2,ifelse(Block=="B3",3,Block)))) %>% 
  #Remove extra rows
  filter(!is.na(Year)) %>% 
  #correct order spellings
  mutate(Correct_Order=ifelse(Order=="aranea","Araneae",ifelse(Order=="body_parts","Body_Parts",ifelse(Order=="Body Parts","Body_Parts",ifelse(Order=="Body_Parts ","Body_Parts",ifelse(Order=="coleoptera","Coleoptera",ifelse(Order=="Coleoptera ","Coleoptera",ifelse(Order=="diptera","Diptera",ifelse(Order=="hemiptera","Hemiptera",ifelse(Order=="hymenoptera","Hymenoptera",ifelse(Order=="Orthoptera ","Orthoptera",ifelse(Order=="body parts","Body_Parts",ifelse(Order=="Cicadellidae","Hemiptera",Order))))))))))))) %>% 
  #remove unnecessary columns and reoder
  dplyr::select(Collection_Method,Year,Block,Grazing_Treatment,Plot,Sample_Number,Correct_Order,Dry_Weight_g,Notes)

#Merge together data frames

Weight_Data_Official<-Weight_20 %>% 
  rbind(Weight_21) %>% 
  #replace plot # for sweepnet with 100 (so not confused with others)
  mutate(Plot=ifelse(Collection_Method=="sweep_net",100,Plot)) %>% 
  #replace any weight that is <0.0001 with 0.00001 %>% 
  mutate(Dry_Weight_g=as.numeric(ifelse(Dry_Weight_g=="<0.0001","0.00005",Dry_Weight_g))) %>% 
  #Create a column that merges together treatment data and year
  mutate(Coll_Year_Bl_Trt=paste(Collection_Method,Year,Block,Grazing_Treatment,sep = "_")) %>% 
  #Remove NAs from Dry weight
  filter(!is.na(Dry_Weight_g)) 
  


####Total Plot Weight Differences ####

#Summing all weights by order within dataset, grazing treatment, block, and plot so that we can look at differences in order across plots --- not working properly
Weight_Data_Summed<-aggregate(Dry_Weight_g~Coll_Year_Bl_Trt+Plot+Correct_Order, data=Weight_Data_Official, FUN=sum, na.rm=FALSE) 

#Seperating out Treatment_Plot into all distinctions again so that we can group based on different things
Weight_Data_Summed<-Weight_Data_Summed %>% 
  separate(Treatment_Plot, c("Dataset", "Grazing_Treatment","Block","Plot"), "_")

Weight_Data_Summed_S<-Weight_Data_Summed %>% 
  filter(Dataset=="S")

#### Anova comparing insect weights by grazing treatment  Sweep net #####
Weight_Data_S_AOV <- aov(Correct_Dry_Weight_g ~ Grazing_Treatment, data = Weight_Data_Summed_S) 
summary(Weight_Data_S_AOV)
model.tables(Weight_Data_S_AOV)


