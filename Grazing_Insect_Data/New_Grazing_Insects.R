#### Grazing x Insect Data - 2020 #
#### Code created by: Kathryn Bloodworth and Will Mann #
#Date started: 06/14/2021 # adapted and restarted 07/13/2022

#### Set working directory and load libraries ####

# Set Working Directory - Mac
setwd("~/Library/CloudStorage/Box-Box/Projects/Dissertation/Data/Insect_Data")

# Set Working Directory - PC
setwd("C:/Users/kjbloodw/Box/Projects/Dissertation/Data/Insect_Data")

#install.packages("scales")
library(scales)
library(vegan)
library(lmerTest)
#install.packages("devtools")
library(grid)
#install.packages("multcomp")
library(multcomp)
#Load Tidyverse#
library(tidyverse)
library(olsrr)
library(patchwork)
library(codyn)
library(pairwiseAdonis)
#install.packages("ggpattern")
library(ggpattern)

#Set ggplot2 theme to black and white
theme_set(theme_bw())
#Update ggplot2 theme - make box around the x-axis title size 30, vertically justify x-axis title to 0.35, Place a margin of 15 around the x-axis title.  Make the x-axis title size 30. For y-axis title, make the box size 30, put the writing at a 90 degree angle, and vertically justify the title to 0.5.  Add a margin of 15 and make the y-axis text size 25. Make the plot title size 30 and vertically justify it to 2.  Do not add any grid lines.  Do not add a legend title, and make the legend size 20
theme_update(panel.grid.major=element_blank(),
             panel.grid.minor=element_blank())

#### Load in data ####
#make sure column names are consistent 

#ID Data
ID_Data_20<-read.csv("2020_Sweep_Net_Dvac_Data_FK.csv",header=T) %>% 
  #make all collection methods the same across years
  mutate(Collection_Method=ifelse(Collection_Method=="d-vac","dvac",ifelse(Collection_Method=="sweep_net","sweep",Collection_Method))) %>% 
  #rename sample column so that it's the same across years
  rename(Sample_Number="Sample") %>% 
  dplyr::select(Collection_Method,Year,Block,Grazing_Treatment,Plot,Sample_Number,Order,Family,Genus,Species,Notes) %>% 
  filter(Collection_Method=="dvac")

ID_Data_21<-read.csv("2021_Sweep_Net_Dvac_Data_FK.csv",header=T) %>% 
  #make all collection methods the same across years
  mutate(Collection_Method=ifelse(Collection_Method=="d-vac","dvac",ifelse(Collection_Method=="sweep_net","sweep",Collection_Method))) %>% 
  #rename sample column so that it's the same across years
  rename(Sample_Number="Sample")%>% 
  dplyr::select(Collection_Method,Year,Block,Grazing_Treatment,Plot,Sample_Number,Order,Family,Genus,Species,Notes) %>% 
  #remove blanks from dataframe
  filter(Collection_Method!="") %>% 
  #fix "LG " to "LG"
  mutate(Grazing_Treatment=ifelse(Grazing_Treatment=="LG ","LG",Grazing_Treatment))%>% 
  filter(Collection_Method=="dvac")

ID_Data_22<-read.csv("2022_Sweep_Net_D-Vac_Data_FK.csv",header=T) %>% 
  #make all collection methods the same across years
  mutate(Collection_Method=ifelse(Collection_Method=="Dvac","dvac",ifelse(Collection_Method=="Sweep_Net","sweep",Collection_Method))) %>% 
  #rename sample column so that it's the same across years
  rename(Sample_Number="Sample")%>% 
  dplyr::select(Collection_Method,Year,Block,Grazing_Treatment,Plot,Sample_Number,Order,Family,Genus,Species,Notes)%>% 
  filter(Collection_Method=="dvac")
  

#Weight Data
Weight_Data_20<-read.csv("2020_Sweep_Net_D-Vac_Weight_Data_FK.csv",header=T) %>% 
  rename(Sample_Number=Sample_num) %>% 
  mutate(Collection_Method=ifelse(Collection_Method=="d-vac","dvac",ifelse(Collection_Method=="sweep_net","sweep",Collection_Method)))%>% 
  filter(Collection_Method=="dvac")

Weight_Data_21<-read.csv("2021_Sweep_Net_D-Vac_Weight_Data_FK.csv",header=T) %>% 
  mutate(Collection_Method=ifelse(Collection_Method=="d-vac","dvac",ifelse(Collection_Method=="sweep_net","sweep",Collection_Method)))%>% 
  filter(Collection_Method=="dvac")

Weight_Data_22<-read.csv("2022_Sweep_Net_D-Vac_Weight_Data_FK.csv",header=T) %>% 
  mutate(Collection_Method=ifelse(Collection_Method=="d-vac","dvac",ifelse(Collection_Method=="sweep_net","sweep",Collection_Method)))%>% 
  filter(Collection_Method=="dvac")

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
  dplyr::select(Collection_Method,Year,Block,Grazing_Treatment,Plot,Sample_Number,Correct_Order,Correct_Family,Correct_Genus,Correct_Species,Notes) %>% 
  #remove all body part entries
  filter(Notes!="Body Parts" & Notes!="Body Parts/Legs" & Notes!="Body parts" & Notes!="too smooshed to tell, put into body parts jar") %>% 
  #make sample # numeric instead of character 
  mutate(Sample_Number=as.numeric(Sample_Number)) 

ID_21<-ID_Data_21 %>% 
  #Change block and grazing treatment to be consistent and match plot numbers
  mutate(Block=ifelse(Block=="B1",1,ifelse(Block=="B2",2,ifelse(Block=="B3",3,Block)))) %>% 
  filter(!is.na(Year)) %>% 
  #fix block numbers
  mutate(Block=ifelse(Plot<=15,1,ifelse(Plot==16,2,ifelse(Plot==17,2,ifelse(Plot==18,2,ifelse(Plot==19,2,ifelse(Plot==20,2,ifelse(Plot==21,2,ifelse(Plot==22,2,ifelse(Plot==23,2,ifelse(Plot==24,2,ifelse(Plot==25,2,ifelse(Plot==26,2,ifelse(Plot==27,2,ifelse(Plot==28,2,ifelse(Plot==29,2,ifelse(Plot==30,2,ifelse(Plot==31,3,ifelse(Plot==32,3,ifelse(Plot==33,3,ifelse(Plot==34,3,ifelse(Plot==35,3,ifelse(Plot==36,3,ifelse(Plot==37,3,ifelse(Plot==38,3,ifelse(Plot==39,3,ifelse(Plot==40,3,ifelse(Plot==41,3,ifelse(Plot==43,3,ifelse(Plot==43,3,ifelse(Plot==44,3,ifelse(Plot==45,3,Block))))))))))))))))))))))))))))))))%>% 
  #change grazing treatments to be consistent
  mutate(Grazing_Treatment=ifelse(Grazing_Treatment=="LG ","LG",Grazing_Treatment)) %>% 
  #correct misspellings and inconsistencies in order data
  mutate(Correct_Order=ifelse(Order=="Aranea ","Araneae",ifelse(Order=="Hemiptera ","Hemiptera",ifelse(Order=="Araneae ","Araneae",ifelse(Order=="Coleopetra","Coleoptera",ifelse(Order=="Coleoptera ","Coleoptera",ifelse(Order=="Hymenoptera ","Hymenoptera",ifelse(Order=="Hymeonptera","Hymenoptera",ifelse(Order=="Orthoptera ","Orthoptera",Order))))))))) %>% 
  #correct misspellings and inconsistencies in order data
  mutate(Correct_Family=ifelse(Family=="Acridiae", "Acrididae",ifelse(Family=="Agramyzidae", "Agromyzidae", ifelse(Family=="Coleoptera ", "Coleoptera", ifelse(Family=="Currulianidae", "Curculionidae", ifelse(Family=="Ligidae","Lygaeidae", ifelse(Family=="Scuttelleridae", "Scutelleridae", ifelse(Family=="Scutelleridae ", "Scutelleridae", ifelse(Family=="staphylinidae", "Staphylinidae", ifelse(Family=="Thamisidae", "Thomisidae", ifelse(Family=="Thomsidae", "Thomisidae", ifelse(Family=="Formicide", "Formicidae", Family))))))))))))%>% 
  mutate(Correct_Genus=ifelse(Genus=="longipennis","Longipennis",ifelse(Genus=="Opcia","Opeia",ifelse(Genus=="melanoplus","Melanoplus",ifelse(Genus=="opeia","Opeia",ifelse(Genus=="Phoetaliotes ","Phoetaliotes",ifelse(Genus=="Erittix","Eritettix",Genus))))))) %>% 
  mutate(Correct_Species=ifelse(Species=="bru","bruneri",ifelse(Species=="Bruneri","bruneri",ifelse(Species=="Bruneri ","bruneri",ifelse(Species=="confuscus","confusus",ifelse(Species=="Confusus","confusus",ifelse(Species=="Curtipennis","curtipennis",ifelse(Species=="Deorum","deorum",ifelse(Species=="differntialis","differentialis",ifelse(Species=="Gladstoni","gladstoni",ifelse(Species=="Hebrascensis","nebrascensis",ifelse(Species=="Infantilis","infantilis",ifelse(Species=="Keeleri","keeleri",ifelse(Species=="Nebrascensis","nebrascensis",ifelse(Species=="Obscrua","obscura",ifelse(Species=="Obscura ","obscura",ifelse(Species=="Obscuria","obscura",ifelse(Species=="Pseudonietara","pseudonietana",ifelse(Species=="Pseudonietena","pseudonietana",ifelse(Species=="Sanguinipes","sanguinipes",ifelse(Species=="Simplex","simplex",ifelse(Species=="Angustipennis","angustipennis",Species)))))))))))))))))))))) %>% 
  #remove unnecessary columns and reoder
  dplyr::select(Collection_Method,Year,Block,Grazing_Treatment,Plot,Sample_Number,Correct_Order,Correct_Family,Correct_Genus,Correct_Species,Notes) %>% 
  mutate(Sample_Number=as.numeric(Sample_Number))


ID_22<-ID_Data_22 %>% 
  #Change block and grazing treatment to be consistent and match plot numbers
  mutate(Block=ifelse(Block=="B1",1,ifelse(Block=="B2",2,ifelse(Block=="B3",3,Block)))) %>%
  #correct misspellings and inconsistencies in order data
  mutate(Correct_Order=ifelse(Order=="araneae","Araneae",
                            ifelse(Order=="coleoptera","Coleoptera",
                            ifelse(Order=="diptera","Diptera",
                            ifelse(Order=="hemiptera","Hemiptera",
                            ifelse(Order=="hymenoptera","Hymenoptera",
                            ifelse(Order=="lepidoptera","Lepidoptera",
                            ifelse(Order=="neuroptera","Neuroptera",
                            ifelse(Order=="orthoptera","Orthoptera",
                            ifelse(Order=="thysanoptera","Thysanoptera",
                            ifelse(Order=="unknown","Unknown",Order))))))))))) %>% 
  #correct misspellings and inconsistencies in order data
  mutate(Correct_Family=ifelse(Family=="aphididae", "Aphididae",ifelse(Family=="asilidae", "Asilidae",ifelse(Family=="Ceraphionidae","Ceraphronidae",ifelse(Family=="chloropidae","Chloropidae",ifelse(Family=="Chrionomidae","Chironomidae",ifelse(Family=="chrysididae","Chrysididae",ifelse(Family=="Cicadellidea","Cicadellidae",ifelse(Family=="coccinellidae","Coccinellidae",ifelse(Family=="Coccinelliadae","Coccinellidae",ifelse(Family=="culicidae","Culicidae",ifelse(Family=="curculionidae","Curculionidae",ifelse(Family=="Diapriidea","Diapriidae",ifelse(Family=="Euiophidae","Eulophidae",ifelse(Family=="eupelmidae","Eupelmidae",ifelse(Family=="ichneumonidae","Ichneumonidae",ifelse(Family=="latridiidae","Latridiidae",ifelse(Family=="lycosidae","Lycosidae",ifelse(Family=="muscidae","Muscidae",ifelse(Family=="myrmeleontidae","Myrmeleontidae",ifelse(Family=="nabidae","Nabidae",ifelse(Family=="pentatomidae","Pentatomidae",ifelse(Family=="perilampidae","Perilampidae",ifelse(Family=="platygastridae","Platygastridae",ifelse(Family=="scarabaeidae","Scarabaeidae",ifelse(Family=="Scarabacidae","Scarabaeidae",ifelse(Family=="sepsidae","Sepsidae",ifelse(Family=="tomisidae","Thomisidae",ifelse(Family=="Thripinae","Thripidae",ifelse(Family=="Thrips","Thripidae",ifelse(Family=="Tiombiculidae","Trombiculidae",ifelse(Family=="tingidae","Tingidae",ifelse(Family=="trichoceridae","Trichoceridae",ifelse(Family=="Trichoceridea","Trichoceridae",ifelse(Family=="unknown","Unknown",ifelse(Family=="",NA,ifelse(Family=="N/A",NA,ifelse(Family=="n/a",NA,Family)))))))))))))))))))))))))))))))))))))) %>% 
  mutate(Correct_Genus=ifelse(Genus=="ageneotettix","Ageneotettix",ifelse(Genus=="arphia","Arphia",ifelse(Genus=="melanoplus","Melanoplus",ifelse(Genus=="opeia","Opeia",ifelse(Genus=="dissosteira","Dissosteira",ifelse(Genus=="Dissosteria","Dissosteira" ,ifelse(Genus=="Eritcttix","Eritettix",ifelse(Genus=="eritettix","Eritettix",ifelse(Genus=="Erotettix","Eritettix",ifelse(Genus=="phoetaliotes","Phoetaliotes",ifelse(Genus=="unknown","Unknown",ifelse(Genus=="",NA,ifelse(Genus=="N/A",NA,ifelse(Genus=="n/a",NA,Genus))))))))))))))) %>% 
  mutate(Correct_Species=ifelse(Species=="os","obscura",ifelse(Species=="pseudomietana","pseudonietana",ifelse(Species=="unknown","Unknown",ifelse(Species=="",NA,ifelse(Species=="N/A",NA,ifelse(Species=="n/a",NA,Species))))))) %>% 
  #remove unnecessary columns and reoder
  dplyr::select(Collection_Method,Year,Block,Grazing_Treatment,Plot,Sample_Number,Correct_Order,Correct_Family,Correct_Genus,Correct_Species,Notes) %>% 
  mutate(Sample_Number=as.numeric(Sample_Number))

#Merge together data frames

ID_Data_Official<-ID_20 %>% 
  rbind(ID_21) %>% 
  rbind(ID_22) %>% 
  mutate(Coll_Year_Bl_Trt=paste(Collection_Method,Year,Block,Grazing_Treatment,sep = "_")) %>% 
  mutate(Coll_Year_Bl_Trt_Pl=paste(Coll_Year_Bl_Trt,Plot,sep = "-"))


#### Abundance by Count ####
Abundance<-ID_Data_Official %>% 
  group_by(Collection_Method,Year,Block,Grazing_Treatment,Plot,Correct_Order) %>% 
  mutate(Abundance=length(Sample_Number)) %>% 
  ungroup() 

Abundance_Plot<-ID_Data_Official %>% 
  group_by(Collection_Method,Year,Block,Grazing_Treatment,Plot) %>% 
  mutate(Plot_Abundance=length(Sample_Number)) %>% 
  ungroup() %>% 
  select(Collection_Method,Year,Block,Grazing_Treatment,Plot,Plot_Abundance) %>% 
  unique() 

Abundance_Plot_Orthoptera<-Abundance %>% 
  filter(Correct_Order=="Orthoptera") %>%
  select(Collection_Method,Year,Block,Grazing_Treatment,Plot,Abundance) %>% 
  unique() 

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

Weight_22<-Weight_Data_22 %>%  
  #change blocks to be numeric
  mutate(Block=ifelse(Block=="B1",1,ifelse(Block=="B2",2,ifelse(Block=="B3",3,Block)))) %>%
  mutate(Correct_Order=ifelse(Order=="Trombicvlidae","Trombiculidae",Order)) %>% 
  #remove unnecessary columns and reoder
  dplyr::select(Collection_Method,Year,Block,Grazing_Treatment,Plot,Sample_Number,Correct_Order,Dry_Weight_g,Notes)

#Merge together data frames

Weight_Data_Official<-Weight_20 %>% 
  rbind(Weight_21) %>% 
  rbind(Weight_22) %>% 
  #wrong grazing treatment fixed
  mutate(Grazing_Treatment=ifelse(Plot==35,"NG",Grazing_Treatment)) %>% 
  #wrong block numbers fixed
  mutate(Block=ifelse(Plot=="40",3,Block)) %>% 
  #replace any weight that is <0.0001 with 0.00001 %>% 
  mutate(Dry_Weight_g=as.numeric(ifelse(Dry_Weight_g=="<0.0001","0.00001",Dry_Weight_g))) %>% 
  #Create a column that merges together treatment data and year
  mutate(Coll_Year_Bl_Trt=paste(Collection_Method,Year,Block,Grazing_Treatment,sep = "_")) %>% 
  mutate(Coll_Year_Bl_Trt_Pl=paste(Coll_Year_Bl_Trt,Plot,sep = "-")) %>% 
  mutate(Coll_Year_Bl_Trt_Pl=ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2021_1_NG_33","dvac_2021_3_NG_33",Coll_Year_Bl_Trt_Pl)) %>% 
  #fix plot numbers to be correct numbers
  mutate(Coll_Year_Bl_Trt_Pl=ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_1_NG-1","dvac_2020_1_NG-1",
                             ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_1_NG-2","dvac_2020_1_NG-2",
                             ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_1_NG-3","dvac_2020_1_NG-3",
                             ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_1_NG-4","dvac_2020_1_NG-4",
                             ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_1_NG-5","dvac_2020_1_NG-5",
                             ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_1_LG-1","dvac_2020_1_LG-6",
                             ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_1_LG-2","dvac_2020_1_LG-7",
                             ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_1_LG-3","dvac_2020_1_LG-8",
                             ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_1_LG-4","dvac_2020_1_LG-9",
                             ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_1_LG-5","dvac_2020_1_LG-10",
                             ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_1_HG-1","dvac_2020_1_HG-11",
                             ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_1_HG-2","dvac_2020_1_HG-12",
                             ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_1_HG-3","dvac_2020_1_HG-13",
                             ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_1_HG-4","dvac_2020_1_HG-14",
                             ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_1_HG-5","dvac_2020_1_HG-15",
                             ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_2_NG-1","dvac_2020_2_NG-16",
                             ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_2_NG-2","dvac_2020_2_NG-17",
                             ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_2_NG-3","dvac_2020_2_NG-18",
                             ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_2_NG-4","dvac_2020_2_NG-19",
                             ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_2_NG-5","dvac_2020_2_NG-20",
                             ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_2_LG-1","dvac_2020_2_LG-21",
                             ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_2_LG-2","dvac_2020_2_LG-22",
                             ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_2_LG-3","dvac_2020_2_LG-23",
                             ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_2_LG-4","dvac_2020_2_LG-24",
                             ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_2_LG-5","dvac_2020_2_LG-25",
                             ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_2_HG-1","dvac_2020_2_HG-26",
                             ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_2_HG-2","dvac_2020_2_HG-27",
                             ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_2_HG-3","dvac_2020_2_HG-28",
                             ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_2_HG-4","dvac_2020_2_HG-29",
                             ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_2_HG-5","dvac_2020_2_HG-30",    
                             ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_3_NG-1","dvac_2020_3_NG-31",
                             ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_3_NG-2","dvac_2020_3_NG-32",
                             ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_3_NG-3","dvac_2020_3_NG-33",
                             ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_3_NG-4","dvac_2020_3_NG-34",
                             ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_3_NG-5","dvac_2020_3_NG-35",
                             ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_3_LG-1","dvac_2020_3_LG-36",
                             ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_3_LG-2","dvac_2020_3_LG-37",
                             ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_3_LG-3","dvac_2020_3_LG-38",
                             ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_3_LG-4","dvac_2020_3_LG-39",
                             ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_3_LG-5","dvac_2020_3_LG-40",
                             ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_3_HG-1","dvac_2020_3_HG-41",
                             ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_3_HG-2","dvac_2020_3_HG-42",
                             ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_3_HG-3","dvac_2020_3_HG-43",
                             ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_3_HG-4","dvac_2020_3_HG-44",
                             ifelse(Coll_Year_Bl_Trt_Pl=="dvac_2020_3_HG-5","dvac_2020_3_HG-45", 
                             Coll_Year_Bl_Trt_Pl)))))))))))))))))))))))))))))))))))))))))))))) %>% 
  dplyr::select(Coll_Year_Bl_Trt_Pl,Sample_Number,Correct_Order,Dry_Weight_g,Notes) %>% 
  #RemovNAs from Dry weight
  filter(!is.na(Dry_Weight_g)) %>% 
  separate(Coll_Year_Bl_Trt_Pl, c("Coll_Year_Bl_Trt","Plot"), "-") 


#### Arthropod Abundance (Weight): Plot Level ####

#Summing all weights by order within dataset, grazing treatment, block, and plot so that we can look at differences in order across plots
Weight_Data_Summed<-aggregate(Dry_Weight_g~Coll_Year_Bl_Trt+Plot+Correct_Order, data=Weight_Data_Official, FUN=sum, na.rm=FALSE) 

#Separating out Treatment_Plot into all distinctions again so that we can group based on different things
Weight_Data_Summed<-Weight_Data_Summed %>% 
  separate(Coll_Year_Bl_Trt, c("Collection_Method","Year","Block","Grazing_Treatment"), "_")

#create dataframe that just has dvac samples in it
Weight_Data_Summed_dvac<-Weight_Data_Summed %>% 
  filter(Collection_Method=="dvac") %>% 
  filter(Plot!="NA") %>% 
  #sum by plot 
  group_by(Year,Block,Grazing_Treatment,Plot) %>% 
  summarise(Plot_Weight=sum(Dry_Weight_g)) %>% 
  ungroup() 

Weight_by_Grazing_dvac<-Weight_Data_Summed_dvac %>% 
  group_by(Year,Grazing_Treatment) %>% 
  summarise(Average_Weight=mean(Plot_Weight),Weight_SD=sd(Plot_Weight),Weight_n=length(Plot_Weight)) %>% 
  mutate(Weight_St_Error=Weight_SD/sqrt(Weight_n)) %>% 
  ungroup()%>% 
  mutate(Correct_Order="Plot")

### Order Abundance (Weight): Plot Level ###
Weight_by_Order_Dvac<-Weight_Data_Summed %>%  
  filter(Correct_Order!="Unknown_1") %>% 
  filter(Correct_Order!="Unknown") %>% 
  filter(Correct_Order!="unknown") %>% 
  filter(Correct_Order!="Snail") %>% 
  filter(Correct_Order!="Body_Parts") %>% 
  filter(Correct_Order!="Body Parts") %>% 
  filter(Plot!="NA") %>% 
  spread(key=Correct_Order,value=Dry_Weight_g, fill=0) %>% 
  gather(key="Correct_Order","Dry_Weight_g",6:15) %>% 
  group_by(Collection_Method,Year, Grazing_Treatment, Correct_Order) %>% 
  summarise(Average_Weight=mean(Dry_Weight_g),Weight_SD=sd(Dry_Weight_g),Weight_n=length(Dry_Weight_g)) %>%
  mutate(Weight_St_Error=Weight_SD/sqrt(Weight_n)) %>% 
  ungroup() %>% 
  #make a new column with new name for calculation below
  mutate(Orthoptera_Order_Weight=Average_Weight)

### Merge together Weight_by_Grazing_dvac and Weight_by_Order_Dvac to make dataframe for graph with total weight average across plots stacked with amount of that weight that is orthoptera
#Create a new dataframe for plot weight - grasshopper weight for graph below
Weight_Plot_Minus_Orthoptera <-Weight_by_Order_Dvac %>% 
  #only want grasshopper abundance
  filter(Correct_Order=="Orthoptera") %>% 
  select(Year,Grazing_Treatment,Orthoptera_Order_Weight) %>% 
  left_join(Weight_by_Grazing_dvac) %>% 
  mutate(NonOrtho_Plot_Weight=Average_Weight-Orthoptera_Order_Weight) %>% 
  select(Year,Grazing_Treatment,Correct_Order,NonOrtho_Plot_Weight,Weight_SD,Weight_n,Weight_St_Error) %>% 
  rename(Average_Weight=NonOrtho_Plot_Weight) 

Weight_Total_Order<-Weight_by_Order_Dvac %>% 
  select(Year,Grazing_Treatment,Correct_Order,Average_Weight,Weight_SD,Weight_n,Weight_St_Error) %>% 
  filter(Correct_Order=="Orthoptera") %>% 
  rbind(Weight_Plot_Minus_Orthoptera) 
 
### Average Abundance (Count) plot level####
Abundance_Plot_Orthoptera_Avg<-Abundance_Plot_Orthoptera %>% 
  rename(OrthopteraAbundance=Abundance) %>%
  #add a row for plot 43 which has no orthoptera
  add_row(Collection_Method = "dvac", Year=2022, Block = "3", Grazing_Treatment="HG",Plot=43, OrthopteraAbundance=0) %>% 
  group_by(Year,Grazing_Treatment) %>%
  summarise(Average_Plot_Abundance=mean(OrthopteraAbundance),Plot_Abundance_SD=sd(OrthopteraAbundance),Plot_Abundance_n=length(OrthopteraAbundance)) %>% 
  mutate(Plot_Abundance_St_Error=Plot_Abundance_SD/sqrt(Plot_Abundance_n)) %>% 
  ungroup() %>% 
  mutate(Correct_Order="Orthoptera")
  
Abundance_by_Grazing_Avg<-Abundance_Plot %>% 
  group_by(Year,Grazing_Treatment) %>%
  summarise(Average_Plot_Abundance=mean(Plot_Abundance),Plot_Abundance_SD=sd(Plot_Abundance),Plot_Abundance_n=length(Plot_Abundance)) %>% 
  mutate(Plot_Abundance_St_Error=Plot_Abundance_SD/sqrt(Plot_Abundance_n)) %>% 
  ungroup() %>% 
  mutate(Correct_Order="Plot")

Abundance_Plot_Minus_Orthoptera <-Abundance_Plot_Orthoptera_Avg %>% 
  rename(AvgOrthopteraAbundance=Average_Plot_Abundance) %>% 
  select(Year,Grazing_Treatment,AvgOrthopteraAbundance) %>% 
  left_join(Abundance_by_Grazing_Avg) %>% 
  mutate(NonOrtho_Plot_Abundance=Average_Plot_Abundance-AvgOrthopteraAbundance) %>% 
  select(Year,Grazing_Treatment,Correct_Order,NonOrtho_Plot_Abundance,Plot_Abundance_SD,Plot_Abundance_n,Plot_Abundance_St_Error) %>% 
  rename(Average_Plot_Abundance=NonOrtho_Plot_Abundance) 

Abundance_Total_Order<-Abundance_Plot_Orthoptera_Avg %>% 
  select(Year,Grazing_Treatment,Correct_Order,Average_Plot_Abundance,Plot_Abundance_SD,Plot_Abundance_n,Plot_Abundance_St_Error) %>% 
  rbind(Abundance_Plot_Minus_Orthoptera) 



##reorder bar graphs##
Weight_by_Grazing_dvac$Grazing_Treatment <- factor(Weight_by_Grazing_dvac$Grazing_Treatment, levels = c("NG", "LG", "HG"))
Abundance_by_Grazing_Avg$Grazing_Treatment <- factor(Abundance_by_Grazing_Avg$Grazing_Treatment, levels = c("NG", "LG", "HG"))

#### Total Plot Weight Differences - Figures ####

# 2020 - Dvac
#Graph of Weights from Sweep Net by Grazing treatment- 2020
Dvac_2020_Plot<-ggplot(subset(Weight_by_Grazing_dvac,Year==2020),aes(x=Grazing_Treatment,y=Average_Weight,fill=Grazing_Treatment))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge), and fill in the bars with the color grey.  
  geom_bar(stat="identity",position = "dodge")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(aes(ymin=Average_Weight-Weight_St_Error,ymax=Average_Weight+Weight_St_Error),position=position_dodge(),width=0.2)+
  #Label the x-axis "Treatment"
  xlab("Grazing Treatment")+
  #Label the y-axis "Species Richness"
  ylab("Average Plot Weight (g)")+
  theme(legend.background=element_blank())+
  scale_x_discrete(labels=c("HG"="High Impact Grazing","LG"="Destock","NG"="Cattle Removal"),limits=c("NG","LG","HG"))+
  scale_fill_manual(values=c("thistle2","thistle3","thistle4"), labels=c("High Impact Grazing","Cattle Removal","Destock"))+
  theme(legend.key = element_rect(size=3), legend.key.size = unit(1,"centimeters"),legend.position="NONE")+
  #Make the y-axis extend to 50
  expand_limits(y=0.5)+
  scale_y_continuous(labels = label_number(accuracy = 0.01))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45))+
  geom_text(x=0.85, y=0.5, label="2020 Weight",size=20)

# 2021 - Dvac
#Graph of Weights from dvac by Grazing treatment- 2021
Dvac_2021_Plot<-ggplot(subset(Weight_by_Grazing_dvac,Year==2021),aes(x=Grazing_Treatment,y=Average_Weight,fill=Grazing_Treatment))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge), and fill in the bars with the color grey.  
  geom_bar(stat="identity",position = "dodge")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(aes(ymin=Average_Weight-Weight_St_Error,ymax=Average_Weight+Weight_St_Error),position=position_dodge(),width=0.2)+
  #Label the x-axis "Treatment"
  xlab("Grazing Treatment")+
  #Label the y-axis "Species Richness"
  ylab("Average Plot Weight (g)")+
  theme(legend.background=element_blank())+ 
  scale_x_discrete(labels=c("HG"="High Impact Grazing","LG"="Destock","NG"="Cattle Removal"),limits=c("NG","LG","HG"))+
  scale_fill_manual(values=c("thistle2","thistle3","thistle4"), labels=c("High Impact Grazing","Cattle Removal","Destock"))+
  theme(axis.title.y=element_blank(),legend.position = "none")+
  #Make the y-axis extend to 50
  expand_limits(y=0.2)+
  scale_y_continuous(labels = label_number(accuracy = 0.01))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45))+
  geom_text(x=0.85, y=0.2, label="2021 Weight",size=20)

# 2022 - Dvac
#Graph of Weights from dvac by Grazing treatment- 2021
Dvac_2022_Plot<-ggplot(subset(Weight_by_Grazing_dvac,Year==2022),aes(x=Grazing_Treatment,y=Average_Weight,fill=Grazing_Treatment))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge), and fill in the bars with the color grey.  
  geom_bar(stat="identity",position = "dodge")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(aes(ymin=Average_Weight-Weight_St_Error,ymax=Average_Weight+Weight_St_Error),position=position_dodge(),width=0.2)+
  #Label the x-axis "Treatment"
  xlab("Grazing Treatment")+
  #Label the y-axis "Species Richness"
  ylab("Average Plot Weight (g)")+
  theme(legend.background=element_blank())+
  scale_x_discrete(labels=c("HG"="High Impact Grazing","LG"="Destock","NG"="Cattle Removal"),limits=c("NG","LG","HG"))+
  scale_fill_manual(values=c("thistle2","thistle3","thistle4"), labels=c("High Impact Grazing","Cattle Removal","Destock"))+
  theme(axis.title.y=element_blank(),legend.position = "none")+
  #Make the y-axis extend to 50
  expand_limits(y=0.1)+
  scale_y_continuous(labels = label_number(accuracy = 0.01))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45))+
  geom_text(x=0.85, y=0.1, label="2022 Weight",size=20)

#### Create Average Plot Weight Figure ####
  Dvac_2020_Plot+  
  Dvac_2021_Plot+
  Dvac_2022_Plot+
  plot_layout(ncol = 3,nrow = 1)
#Save at 4500x2000

#### Abundance by Count Figure ####
#Graph of Plot_Abundances from Dvac by Grazing treatment- 2020
Count_2020_Plot<-ggplot(subset(Abundance_by_Grazing_Avg,Year==2020),aes(x=Grazing_Treatment,y=Average_Plot_Abundance,fill=Grazing_Treatment))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge), and fill in the bars with the color grey.  
  geom_bar(stat="identity",position = "dodge")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(aes(ymin=Average_Plot_Abundance-Plot_Abundance_St_Error,ymax=Average_Plot_Abundance+Plot_Abundance_St_Error),position=position_dodge(),width=0.2)+
  #Label the x-axis "Treatment"
  xlab("Grazing Treatment")+
  #Label the y-axis "Species Richness"
  ylab("Average Plot Count")+
  theme(legend.background=element_blank())+
  scale_x_discrete(labels=c("HG"="High Impact Grazing","LG"="Destock","NG"="Cattle Removal"),limits=c("NG","LG","HG"))+
  scale_fill_manual(values=c("thistle2","thistle3","thistle4"), labels=c("High Impact Grazing","Cattle Removal","Destock"))+
  theme(legend.key = element_rect(size=3), legend.key.size = unit(1,"centimeters"),legend.position="NONE")+
  #Make the y-axis extend to 50
  expand_limits(y=25)+
  scale_y_continuous(labels = label_number(accuracy = 0.01))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45))+
  geom_text(x=0.85, y=25, label="2020 Count",size=20)

# 2021 - Dvac
#Graph of Plot_Abundances from dvac by Grazing treatment- 2021
Count_2021_Plot<-ggplot(subset(Abundance_by_Grazing_Avg,Year==2021),aes(x=Grazing_Treatment,y=Average_Plot_Abundance,fill=Grazing_Treatment))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge), and fill in the bars with the color grey.  
  geom_bar(stat="identity",position = "dodge")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(aes(ymin=Average_Plot_Abundance-Plot_Abundance_St_Error,ymax=Average_Plot_Abundance+Plot_Abundance_St_Error),position=position_dodge(),width=0.2)+
  #Label the x-axis "Treatment"
  xlab("Grazing Treatment")+
  #Label the y-axis "Species Richness"
  ylab("Average Plot Count")+
  theme(legend.background=element_blank())+ 
  scale_x_discrete(labels=c("HG"="High Impact Grazing","LG"="Destock","NG"="Cattle Removal"),limits=c("NG","LG","HG"))+
  scale_fill_manual(values=c("thistle2","thistle3","thistle4"), labels=c("High Impact Grazing","Cattle Removal","Destock"))+
  theme(axis.title.y=element_blank(),legend.position = "none")+
  #Make the y-axis extend to 50
  expand_limits(y=25)+
  scale_y_continuous(labels = label_number(accuracy = 0.01))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45))+
  geom_text(x=0.85, y=25, label="2021 Count",size=20)

# 2022 - Dvac
#Graph of Plot_Abundances from dvac by Grazing treatment- 2021
Count_2022_Plot<-ggplot(subset(Abundance_by_Grazing_Avg,Year==2022),aes(x=Grazing_Treatment,y=Average_Plot_Abundance,fill=Grazing_Treatment))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge), and fill in the bars with the color grey.  
  geom_bar(stat="identity",position = "dodge")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(aes(ymin=Average_Plot_Abundance-Plot_Abundance_St_Error,ymax=Average_Plot_Abundance+Plot_Abundance_St_Error),position=position_dodge(),width=0.2)+
  #Label the x-axis "Treatment"
  xlab("Grazing Treatment")+
  #Label the y-axis "Species Richness"
  ylab("Average Plot Count")+
  theme(legend.background=element_blank())+
  scale_x_discrete(labels=c("HG"="High Impact Grazing","LG"="Destock","NG"="Cattle Removal"),limits=c("NG","LG","HG"))+
  scale_fill_manual(values=c("thistle2","thistle3","thistle4"), labels=c("High Impact Grazing","Cattle Removal","Destock"))+
  theme(axis.title.y=element_blank(),legend.position = "none")+
  #Make the y-axis extend to 50
  expand_limits(y=300)+
  scale_y_continuous(labels = label_number(accuracy = 0.01))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45))+
  geom_text(x=0.85, y=300, label="2022 Count",size=20)

#### Create Average Plot Weight Figure ####
Count_2020_Plot+  
  Count_2021_Plot+
  Count_2022_Plot+
  plot_layout(ncol = 3,nrow = 1)
#Save at 4500x2000

#### Normality: Plot Weights####
# Dvac 2020
dvac_2020_Weight <- lm(data = subset(Weight_Data_Summed_dvac, Year == 2020), sqrt(Plot_Weight)  ~ Grazing_Treatment)
ols_plot_resid_hist(dvac_2020_Weight) 
ols_test_normality(dvac_2020_Weight) #normal

# dvac 2021
dvac_2021_Weight <- lm(data = subset(Weight_Data_Summed_dvac, Year == 2021), log(Plot_Weight)  ~ Grazing_Treatment)
ols_plot_resid_hist(dvac_2021_Weight) 
ols_test_normality(dvac_2021_Weight) #normal

# dvac 2022
dvac_2022_Weight <- lm(data = subset(Weight_Data_Summed_dvac, Year == 2022), log(Plot_Weight)  ~ Grazing_Treatment)
ols_plot_resid_hist(dvac_2022_Weight) 
ols_test_normality(dvac_2022_Weight) #normal

#### Glmm for Plot Weights by Grazing Treatment####
# 2020 Dvac
Plot_Weight_D_2020_Glmm <- lmer(sqrt(Plot_Weight) ~ Grazing_Treatment + (1 | Block) , data = subset(Weight_Data_Summed_dvac,Year==2020))
anova(Plot_Weight_D_2020_Glmm) #not significant

# 2021 Dvac
Plot_Weight_D_2021_Glmm <- lmer(log(Plot_Weight) ~ Grazing_Treatment + (1 | Block) , data = subset(Weight_Data_Summed_dvac,Year==2021))
anova(Plot_Weight_D_2021_Glmm) # p=0.003987
###post hoc test for lmer test ##
summary(glht(Plot_Weight_D_2021_Glmm, linfct = mcp(Grazing_Treatment = "Tukey")), test = adjusted(type = "BH")) #NG-LG (p=0.0.56774), #LG-HG (0.00857), NG-HG (0.00256)

# 2022 Dvac
Plot_Weight_D_2022_Glmm <- lmer(log(Plot_Weight) ~ Grazing_Treatment + (1 | Block) , data = subset(Weight_Data_Summed_dvac,Year==2022))
anova(Plot_Weight_D_2022_Glmm) #not significant

#### Normality: Plot Count####
# Dvac 2020
dvac_2020_count <- lm(data = subset(Abundance_Plot, Year == 2020), log(Plot_Abundance)  ~ Grazing_Treatment)
ols_plot_resid_hist(dvac_2020_count) 
ols_test_normality(dvac_2020_count) #normal

# dvac 2021
dvac_2021_count <- lm(data = subset(Abundance_Plot, Year == 2021), log(Plot_Abundance)  ~ Grazing_Treatment)
ols_plot_resid_hist(dvac_2021_count) 
ols_test_normality(dvac_2021_count) #normal

# dvac 2022
dvac_2022_count <- lm(data = subset(Abundance_Plot, Year == 2022), log(Plot_Abundance)  ~ Grazing_Treatment)
ols_plot_resid_hist(dvac_2022_count) 
ols_test_normality(dvac_2022_count) #normal

#### Glmm for Plot Count by Grazing Treatment####
# 2020 Dvac
Count_2020_Glmm <- lmer(log(Plot_Abundance) ~ Grazing_Treatment + (1 | Block) , data = subset(Abundance_Plot,Year==2020))
anova(Count_2020_Glmm) #not significant

# 2021 Dvac
Count_2021_Glmm <- lmer(log(Plot_Abundance) ~ Grazing_Treatment + (1 | Block) , data = subset(Abundance_Plot,Year==2021))
anova(Count_2021_Glmm) #0.02996
###post hoc test for lmer test ##
summary(glht(Count_2021_Glmm, linfct = mcp(Grazing_Treatment = "Tukey")), test = adjusted(type = "BH")) #NG-LG (p=0.0.2455, #LG-HG (0.1655), NG-HG (0.0175)

# 2022 Dvac
Count_2022_Glmm <- lmer(log(Plot_Abundance) ~ Grazing_Treatment + (1 | Block) , data = subset(Abundance_Plot,Year==2022))
anova(Count_2022_Glmm) #0.0119
###post hoc test for lmer test ##
summary(glht(Count_2022_Glmm, linfct = mcp(Grazing_Treatment = "Tukey")), test = adjusted(type = "BH")) #NG-LG (p=0.06767, #LG-HG (0.00568), NG-HG (0.27006)

#### Graphs: Plot Weight with Grasshoppers Hatched ####
Weight_Plot_WGrasshoppers_2020<-ggplot(subset(Weight_Total_Order,Year==2020),aes(x=Grazing_Treatment,y=Average_Weight, pattern=Correct_Order,fill=Correct_Order, position = "stack"))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge), and fill in the bars with the color grey.  
  geom_bar(stat="identity",position = position_stack(reverse = TRUE),color="black")+
  geom_col_pattern(aes(Grazing_Treatment,Average_Weight, pattern_fill = Correct_Order),pattern = c('stripe','stripe','stripe','none','none','none'),fill= 'grey20', colour  = 'grey20',position = position_stack(reverse = TRUE),pattern_alpha = 0.6) +
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(data=subset(Weight_by_Grazing_dvac, Year==2020),aes(ymin=Average_Weight-Weight_St_Error,ymax=Average_Weight+Weight_St_Error),position=position_dodge(),width=0.2)+
  #Label the x-axis "Treatment"
  xlab("Grazing Treatment")+
  #Label the y-axis "Species Richness"
  ylab("Average Plot Weight (g)")+
  theme(legend.background=element_blank())+
  scale_x_discrete(labels=c("HG"="High Impact Grazing","LG"="Destock","NG"="Cattle Removal"),limits=c("NG","LG","HG"))+
  #scale_fill_manual(values=c("grey30","grey10"), labels=c("Orthoptera Weight","Plot Weight"))+
  theme(legend.key = element_rect(size=3), legend.key.size = unit(1,"centimeters"),legend.position="NONE")+
  #Make the y-axis extend to 50
  expand_limits(y=0.5)+
  scale_y_continuous(labels = label_number(accuracy = 0.01))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45))+
  geom_text(x=0.85, y=0.5, label="2020 Weight",size=20)

Weight_Plot_WGrasshoppers_2021<-ggplot(subset(Weight_Total_Order,Year==2021),aes(x=Grazing_Treatment,y=Average_Weight, pattern=Correct_Order,fill=Correct_Order, position = "stack"))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge), and fill in the bars with the color grey.  
  geom_bar(stat="identity",position = position_stack(reverse = TRUE),color="black")+
  geom_col_pattern(aes(Grazing_Treatment,Average_Weight, pattern_fill = Correct_Order),pattern = c('stripe','stripe','stripe','none','none','none'),fill= 'grey20', colour  = 'grey20',position = position_stack(reverse = TRUE),pattern_alpha = 0.6) +
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(data=subset(Weight_by_Grazing_dvac, Year==2021),aes(ymin=Average_Weight-Weight_St_Error,ymax=Average_Weight+Weight_St_Error),position=position_dodge(),width=0.2)+
  #Label the x-axis "Treatment"
  xlab("Grazing Treatment")+
  #Label the y-axis "Species Richness"
  ylab("Average Plot Weight (g)")+
  theme(legend.background=element_blank())+
  scale_x_discrete(labels=c("HG"="High Impact Grazing","LG"="Destock","NG"="Cattle Removal"),limits=c("NG","LG","HG"))+
  #scale_fill_manual(values=c("grey30","grey10"), labels=c("Orthoptera Weight","Plot Weight"))+
  theme(legend.key = element_rect(size=3), legend.key.size = unit(1,"centimeters"),legend.position="NONE")+
  #Make the y-axis extend to 50
  expand_limits(y=0.2)+
  scale_y_continuous(labels = label_number(accuracy = 0.01))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45))+
  geom_text(x=0.85, y=0.2, label="2021 Weight",size=20)

Weight_Plot_WGrasshoppers_2022<-ggplot(subset(Weight_Total_Order,Year==2022),aes(x=Grazing_Treatment,y=Average_Weight, pattern=Correct_Order,fill=Correct_Order, position = "stack"))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge), and fill in the bars with the color grey.  
  geom_bar(stat="identity",position = position_stack(reverse = TRUE),color="black")+
  geom_col_pattern(aes(Grazing_Treatment,Average_Weight, pattern_fill = Correct_Order),pattern = c('stripe','stripe','stripe','none','none','none'),fill= 'grey20', colour  = 'grey20',position = position_stack(reverse = TRUE),pattern_alpha = 0.6) +
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(data=subset(Weight_by_Grazing_dvac, Year==2022),aes(ymin=Average_Weight-Weight_St_Error,ymax=Average_Weight+Weight_St_Error),position=position_dodge(),width=0.2)+
  #Label the x-axis "Treatment"
  xlab("Grazing Treatment")+
  #Label the y-axis "Species Richness"
  ylab("Average Plot Weight (g)")+
  theme(legend.background=element_blank())+
  scale_x_discrete(labels=c("HG"="High Impact Grazing","LG"="Destock","NG"="Cattle Removal"),limits=c("NG","LG","HG"))+
  #scale_fill_manual(values=c("grey30","grey10"), labels=c("Orthoptera Weight","Plot Weight"))+
  theme(legend.key = element_rect(size=3), legend.key.size = unit(1,"centimeters"),legend.position="NONE")+
  #Make the y-axis extend to 50
  expand_limits(y=0.1)+
  scale_y_continuous(labels = label_number(accuracy = 0.01))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45))+
  geom_text(x=0.85, y=0.1, label="2022 Weight",size=20)

#### Create Average Plot Weight Figure ####
Weight_Plot_WGrasshoppers_2020+  
  Weight_Plot_WGrasshoppers_2021+
  Weight_Plot_WGrasshoppers_2022+
  plot_layout(ncol = 3,nrow = 1)
#Save at 4500x2000

#### Graphs: Plot Count with Grasshoppers Hatched ####
Count_WGrasshoppers_2020<-ggplot(subset(Abundance_Total_Order,Year==2020),aes(x=Grazing_Treatment,y=Average_Plot_Abundance, pattern=Correct_Order,fill=Correct_Order, position = "stack"))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge), and fill in the bars with the color grey.  
  geom_bar(stat="identity",position = position_stack(reverse = TRUE),color="black")+
  geom_col_pattern(aes(Grazing_Treatment,Average_Plot_Abundance, pattern_fill = Correct_Order),pattern = c('stripe','stripe','stripe','none','none','none'),fill= 'grey20', colour  = 'grey20',position = position_stack(reverse = TRUE),pattern_alpha = 0.6) +
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(data=subset(Abundance_by_Grazing_Avg, Year==2020),aes(ymin=Average_Plot_Abundance-Plot_Abundance_St_Error,ymax=Average_Plot_Abundance+Plot_Abundance_St_Error),position=position_dodge(),width=0.2)+
  #Label the x-axis "Treatment"
  xlab("Grazing Treatment")+
  #Label the y-axis "Species Richness"
  ylab("Average Plot Weight (g)")+
  theme(legend.background=element_blank())+
  scale_x_discrete(labels=c("HG"="High Impact Grazing","LG"="Destock","NG"="Cattle Removal"),limits=c("NG","LG","HG"))+
  #scale_fill_manual(values=c("grey30","grey10"), labels=c("Orthoptera Weight","Plot Weight"))+
  theme(legend.key = element_rect(size=3), legend.key.size = unit(1,"centimeters"),legend.position="NONE")+
  #Make the y-axis extend to 50
  expand_limits(y=25)+
  scale_y_continuous(labels = label_number(accuracy = 0.01))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45))+
  geom_text(x=0.85, y=25, label="2020 Count",size=20)

Count_WGrasshoppers_2021<-ggplot(subset(Abundance_Total_Order,Year==2021),aes(x=Grazing_Treatment,y=Average_Plot_Abundance, pattern=Correct_Order,fill=Correct_Order, position = "stack"))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge), and fill in the bars with the color grey.  
  geom_bar(stat="identity",position = position_stack(reverse = TRUE),color="black")+
  geom_col_pattern(aes(Grazing_Treatment,Average_Plot_Abundance, pattern_fill = Correct_Order),pattern = c('stripe','stripe','stripe','none','none','none'),fill= 'grey20', colour  = 'grey20',position = position_stack(reverse = TRUE),pattern_alpha = 0.6) +
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(data=subset(Abundance_by_Grazing_Avg, Year==2021),aes(ymin=Average_Plot_Abundance-Plot_Abundance_St_Error,ymax=Average_Plot_Abundance+Plot_Abundance_St_Error),position=position_dodge(),width=0.2)+
  #Label the x-axis "Treatment"
  xlab("Grazing Treatment")+
  #Label the y-axis "Species Richness"
  ylab("Average Plot Weight (g)")+
  theme(legend.background=element_blank())+
  scale_x_discrete(labels=c("HG"="High Impact Grazing","LG"="Destock","NG"="Cattle Removal"),limits=c("NG","LG","HG"))+
  #scale_fill_manual(values=c("grey30","grey10"), labels=c("Orthoptera Weight","Plot Weight"))+
  theme(legend.key = element_rect(size=3), legend.key.size = unit(1,"centimeters"),legend.position="NONE")+
  #Make the y-axis extend to 50
  expand_limits(y=20)+
  scale_y_continuous(labels = label_number(accuracy = 0.01))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45))+
  geom_text(x=0.85, y=20, label="2021 Count",size=20)

Count_WGrasshoppers_2022<-ggplot(subset(Abundance_Total_Order,Year==2022),aes(x=Grazing_Treatment,y=Average_Plot_Abundance, pattern=Correct_Order,fill=Correct_Order, position = "stack"))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge), and fill in the bars with the color grey.  
  geom_bar(stat="identity",position = position_stack(reverse = TRUE),color="black")+
  geom_col_pattern(aes(Grazing_Treatment,Average_Plot_Abundance, pattern_fill = Correct_Order),pattern = c('stripe','stripe','stripe','none','none','none'),fill= 'grey20', colour  = 'grey20',position = position_stack(reverse = TRUE),pattern_alpha = 0.6) +
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(data=subset(Abundance_by_Grazing_Avg, Year==2022),aes(ymin=Average_Plot_Abundance-Plot_Abundance_St_Error,ymax=Average_Plot_Abundance+Plot_Abundance_St_Error),position=position_dodge(),width=0.2)+
  #Label the x-axis "Treatment"
  xlab("Grazing Treatment")+
  #Label the y-axis "Species Richness"
  ylab("Average Plot Weight (g)")+
  theme(legend.background=element_blank())+
  scale_x_discrete(labels=c("HG"="High Impact Grazing","LG"="Destock","NG"="Cattle Removal"),limits=c("NG","LG","HG"))+
  #scale_fill_manual(values=c("grey30","grey10"), labels=c("Orthoptera Weight","Plot Weight"))+
  theme(legend.key = element_rect(size=3), legend.key.size = unit(1,"centimeters"),legend.position="NONE")+
  #Make the y-axis extend to 50
  expand_limits(y=250)+
  scale_y_continuous(labels = label_number(accuracy = 0.01))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45))+
  geom_text(x=0.85, y=250, label="2022 Count",size=20)

#### Create Average Plot Weight Figure ####
Count_WGrasshoppers_2020 +  
  Count_WGrasshoppers_2021 +
  Count_WGrasshoppers_2022 +
  plot_layout(ncol = 3,nrow = 1)
#Save at 4500x2000

#### Order Relative Weight ####

Relative_Weight<-Weight_Data_Summed %>% 
  filter(Plot!="NA") %>% 
  #add together all data of each orders across grazing treatments 
  group_by(Year,Grazing_Treatment,Correct_Order) %>% 
  mutate(Order_Weight=sum(Dry_Weight_g)) %>%
  ungroup() %>% 
  #add together all data within each grazing treatment for total "plot" weight
  group_by(Year,Grazing_Treatment) %>% 
  mutate(Total_Weight=sum(Dry_Weight_g)) %>%
  ungroup() %>% 
  select(Year,Grazing_Treatment,Correct_Order,Order_Weight,Total_Weight) %>% 
  unique() %>% 
  filter(Correct_Order!="unknown"&Correct_Order!="Unknown"&Correct_Order!="Unknown_1"&Correct_Order!="Body_Parts"&Correct_Order!="Body Parts") %>% 
  mutate(RelativeWeight=Order_Weight/Total_Weight*100) %>% 
  group_by(Year,Grazing_Treatment,Correct_Order) %>% 
  summarise(Average_RelativeWeight=mean(RelativeWeight)) %>% 
  ungroup()

#### Order Relative Weight Plot ####
Order_2020<-ggplot(subset(Relative_Weight,Year==2020),aes(x=Grazing_Treatment,y=Average_RelativeWeight,fill=Correct_Order, position = "stack"))+
    geom_bar(stat="identity")+
    #Label the x-axis "Treatment"
    xlab("Grazing Treatment")+
    #Label the y-axis "Species Richness"
    ylab("Average Relative Weight (g)")+
    theme(legend.background=element_blank())+
    scale_x_discrete(labels=c("HG"="High Impact Grazing","LG"="Destock","NG"="Cattle Removal"),limits=c("NG","LG","HG"))+
  scale_x_discrete(labels=c("HG"="High Impact Grazing","LG"="Destock","NG"="Cattle Removal"),limits=c("NG","LG","HG"))+
  scale_fill_manual(values=c("#661100","#CC6677","#DDCC77","#2D7947", "#4B4084","#76948F"), labels=c("Araneae","Coleoptera","Diptera","Hemiptera","Hymenoptera","Orthoptera"), name = "Order")+
    #scale_fill_manual(values=c("grey30","grey10"), labels=c("Orthoptera Weight","Plot Weight"))+
    theme(legend.key = element_rect(size=3), legend.key.size = unit(1,"centimeters"),legend.position="NONE")+
    #Make the y-axis extend to 50
    expand_limits(y=100)+
    scale_y_continuous(labels = label_number(accuracy = 0.01))+
    theme(text = element_text(size = 55),legend.text=element_text(size=45))+
    geom_text(x=0.85, y=100, label="2020",size=20)

Order_2021<-ggplot(subset(Relative_Weight,Year==2021),aes(x=Grazing_Treatment,y=Average_RelativeWeight,fill=Correct_Order, position = "stack"))+
  geom_bar(stat="identity")+
  #Label the x-axis "Treatment"
  xlab("Grazing Treatment")+
  #Label the y-axis "Species Richness"
  ylab("Average Relative Weight (g)")+
  theme(legend.background=element_blank())+
  scale_x_discrete(labels=c("HG"="High Impact Grazing","LG"="Destock","NG"="Cattle Removal"),limits=c("NG","LG","HG"))+
  scale_fill_manual(values=c("#661100","#CC6677","#DDCC77","#2D7947", "#4B4084","#76948F"), labels=c("Araneae","Coleoptera","Diptera","Hemiptera","Hymenoptera","Orthoptera"), name = "Order")+
  #scale_fill_manual(values=c("grey30","grey10"), labels=c("Orthoptera Weight","Plot Weight"))+
  theme(legend.key = element_rect(size=3), legend.key.size = unit(1,"centimeters"),legend.position="NONE")+
  #Make the y-axis extend to 50
  expand_limits(y=100)+
  scale_y_continuous(labels = label_number(accuracy = 0.01))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45))+
  geom_text(x=0.85, y=100, label="2021",size=20)

Order_2022<-ggplot(subset(Relative_Weight,Year==2022),aes(x=Grazing_Treatment,y=Average_RelativeWeight,fill=Correct_Order, position = "stack"))+
  geom_bar(stat="identity")+
  #Label the x-axis "Treatment"
  xlab("Grazing Treatment")+
  #Label the y-axis "Species Richness"
  ylab("Average Relative Weight (g)")+
  theme(legend.background=element_blank())+
  scale_x_discrete(labels=c("HG"="High Impact Grazing","LG"="Destock","NG"="Cattle Removal"),limits=c("NG","LG","HG"))+
  scale_fill_manual(values=c("#661100","#CC6677","#DDCC77","#2D7947", "#4B4084","#7E69A0","#6B99C7","#76948F","#7B4B4E","#BCB9EC"), labels=c("Araneae","Coleoptera","Diptera","Hemiptera","Hymenoptera","Lepidoptera","Neuroptera","Orthoptera","Thysanoptera","Trombiculidae"), name = "Order")+
  #scale_fill_manual(values=c("grey30","grey10"), labels=c("Orthoptera Weight","Plot Weight"))+
  theme(legend.key = element_rect(size=3), legend.key.size = unit(1,"centimeters"),legend.position="NONE")+
  #Make the y-axis extend to 50
  expand_limits(y=100)+
  scale_y_continuous(labels = label_number(accuracy = 0.01))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45))+
  geom_text(x=0.85, y=100, label="2022",size=20)
  
#### Create Relative Weight Figure ####
Order_2020 +  
  Order_2021+
  Order_2022 +
  plot_layout(ncol = 3,nrow = 1)
#save at 4500 x 2000

#### Order Relative Count ####

Relative_Count<-Abundance %>% 
  filter(Plot!="NA") %>% 
  select(Year,Block,Grazing_Treatment,Plot,Correct_Order,Abundance) %>% 
  unique() %>% 
  #add together all data of each orders across grazing treatments 
  group_by(Year,Grazing_Treatment,Correct_Order) %>% 
  mutate(Order_Abundance=sum(Abundance)) %>%
  ungroup() %>% 
  #add together all data within each grazing treatment for total "plot"count
  group_by(Year,Grazing_Treatment) %>% 
  mutate(Total_Abundance=sum(Abundance)) %>%
  ungroup() %>% 
  select(Year,Grazing_Treatment,Correct_Order,Order_Abundance,Total_Abundance) %>% 
  unique() %>% 
  filter(Correct_Order!="unknown"&Correct_Order!="Unknown"&Correct_Order!="Unknown_1"&Correct_Order!="Body_Parts"&Correct_Order!="Body Parts") %>% 
  mutate(RelativeCount=Order_Abundance/Total_Abundance*100) %>% 
  group_by(Year,Grazing_Treatment,Correct_Order) %>% 
  summarise(Average_RelativeCount=mean(RelativeCount)) %>% 
  ungroup()

#### Order Relative Count Plot ####
Order_Count_2020<-ggplot(subset(Relative_Count,Year==2020),aes(x=Grazing_Treatment,y=Average_RelativeCount,fill=Correct_Order, position = "stack"))+
  geom_bar(stat="identity")+
  #Label the x-axis "Treatment"
  xlab("Grazing Treatment")+
  #Label the y-axis "Species Richness"
  ylab("Average Relative Count")+
  theme(legend.background=element_blank())+
  scale_x_discrete(labels=c("HG"="High Impact Grazing","LG"="Destock","NG"="Cattle Removal"),limits=c("NG","LG","HG"))+
  scale_x_discrete(labels=c("HG"="High Impact Grazing","LG"="Destock","NG"="Cattle Removal"),limits=c("NG","LG","HG"))+
  scale_fill_manual(values=c("#661100","#CC6677","#DDCC77","#2D7947", "#4B4084","#76948F"), labels=c("Araneae","Coleoptera","Diptera","Hemiptera","Hymenoptera","Orthoptera"), name = "Order")+
  #scale_fill_manual(values=c("grey30","grey10"), labels=c("Orthoptera Count","Plot Count"))+
  theme(legend.key = element_rect(size=3), legend.key.size = unit(1,"centimeters"),legend.position="NONE")+
  #Make the y-axis extend to 50
  expand_limits(y=100)+
  scale_y_continuous(labels = label_number(accuracy = 0.01))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45))+
  geom_text(x=0.85, y=100, label="2020 Count",size=20)

Order_Count_2021<-ggplot(subset(Relative_Count,Year==2021),aes(x=Grazing_Treatment,y=Average_RelativeCount,fill=Correct_Order, position = "stack"))+
  geom_bar(stat="identity")+
  #Label the x-axis "Treatment"
  xlab("Grazing Treatment")+
  #Label the y-axis "Species Richness"
  ylab("Average Relative Count (g)")+
  theme(legend.background=element_blank())+
  scale_x_discrete(labels=c("HG"="High Impact Grazing","LG"="Destock","NG"="Cattle Removal"),limits=c("NG","LG","HG"))+
  scale_fill_manual(values=c("#661100","#CC6677","#DDCC77","#2D7947", "#4B4084","#76948F"), labels=c("Araneae","Coleoptera","Diptera","Hemiptera","Hymenoptera","Orthoptera"), name = "Order")+
  #scale_fill_manual(values=c("grey30","grey10"), labels=c("Orthoptera Count","Plot Count"))+
  theme(legend.key = element_rect(size=3), legend.key.size = unit(1,"centimeters"),legend.position="NONE")+
  #Make the y-axis extend to 50
  expand_limits(y=100)+
  scale_y_continuous(labels = label_number(accuracy = 0.01))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45))+
  geom_text(x=0.85, y=100, label="2021 Count",size=20)

Order_Count_2022<-ggplot(subset(Relative_Count,Year==2022),aes(x=Grazing_Treatment,y=Average_RelativeCount,fill=Correct_Order, position = "stack"))+
  geom_bar(stat="identity")+
  #Label the x-axis "Treatment"
  xlab("Grazing Treatment")+
  #Label the y-axis "Species Richness"
  ylab("Average Relative Count (g)")+
  theme(legend.background=element_blank())+
  scale_x_discrete(labels=c("HG"="High Impact Grazing","LG"="Destock","NG"="Cattle Removal"),limits=c("NG","LG","HG"))+
  scale_fill_manual(values=c("#661100","#CC6677","#DDCC77","#2D7947", "#4B4084","#7E69A0","#6B99C7","#76948F","#7B4B4E","#BCB9EC"), labels=c("Araneae","Coleoptera","Diptera","Hemiptera","Hymenoptera","Lepidoptera","Neuroptera","Orthoptera","Thysanoptera","Trombiculidae"), name = "Order")+
  #scale_fill_manual(values=c("grey30","grey10"), labels=c("Orthoptera Count","Plot Count"))+
  theme(legend.key = element_rect(size=3), legend.key.size = unit(1,"centimeters"),legend.position="NONE")+
  #Make the y-axis extend to 50
  expand_limits(y=100)+
  scale_y_continuous(labels = label_number(accuracy = 0.01))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45))+
  geom_text(x=0.85, y=100, label="2022 Count",size=20)

#### Create Relative Count Figure ####
Order_Count_2020 +  
  Order_Count_2021+
  Order_Count_2022 +
  plot_layout(ncol = 3,nrow = 1)
#save at 4500 x 2000


#### Total Plot Weight Differences by Order - Figures ####
#Colors:
#Aranea: #661100
#Coleoptera: #CC6677
#Diptera: #DDCC77
#Hemiptera:#2D7947
#Hymenoptera: #4B4084
#Orthoptera:#76948F
#Lepidoptera:#7E69A0
#Neuroptera:#6B99C7
#Thysanoptera:#7B4B4E
#Trombiculidae:#BCB9EC

#2020 - dvac
Dvac_2020_Order<-ggplot(subset(Weight_by_Order_Dvac,Year==2020),aes(x=Grazing_Treatment,y=Average_Weight, fill=Correct_Order, position="stack"))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge), and fill in the bars with the color grey.  
  geom_bar(stat="identity")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  #Label the x-axis "Treatment"
  xlab("Grazing Treatment")+
  #Label the y-axis "Species Richness"
  ylab("Average Weight (g)")+
  theme(legend.background=element_blank())+
  scale_fill_manual(values=c("#661100","#CC6677","#DDCC77","#2D7947", "#4B4084","#7E69A0","#6B99C7","#76948F","#7B4B4E","#BCB9EC"), labels=c("Araneae","Coleoptera","Diptera","Hemiptera","Hymenoptera","Lepidoptera","Neuroptera","Orthoptera","Thysanoptera","Trombiculidae"), name = "Order")+
  scale_x_discrete(labels=c("HG"="High Impact Grazing","LG"="Destock","NG"="Cattle Removal"),limits=c("NG","LG","HG"))+
  theme(legend.key = element_rect(size=3), legend.key.size = unit(1,"centimeters"), legend.position="NONE")+
  #Make the y-axis extend to 50
  expand_limits(y=0.3)+
  scale_y_continuous(labels = label_number(accuracy = 0.01))+
  theme(text = element_text(size = 55),legend.position = "NONE")+
  geom_text(x=1.1, y=0.3, label="2020 Dvac",size=20)

#2021 - Dvac
Dvac_2021_Order<-ggplot(subset(Weight_by_Order_Dvac,Year==2021),aes(x=Grazing_Treatment,y=Average_Weight, fill=Correct_Order, position="stack"))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge), and fill in the bars with the color grey.  
  geom_bar(stat="identity")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  #Label the x-axis "Treatment"
  xlab("Grazing Treatment")+
  #Label the y-axis "Species Richness"
  ylab("Average Weight (g)")+
  theme(legend.background=element_blank())+
  scale_fill_manual(values=c("#661100","#CC6677","#DDCC77","#2D7947", "#4B4084","#7E69A0","#6B99C7","#76948F","#7B4B4E","#BCB9EC"), labels=c("Araneae","Coleoptera","Diptera","Hemiptera","Hymenoptera","Lepidoptera","Neuroptera","Orthoptera","Thysanoptera","Trombiculidae"), name = "Order")+
  scale_x_discrete(labels=c("HG"="High Impact Grazing","LG"="Destock","NG"="Cattle Removal"),limits=c("NG","LG","HG"))+
  theme(legend.key = element_rect(size=3), legend.key.size = unit(1,"centimeters"), legend.position=c(0.18,0.715))+
  #Make the y-axis extend to 50
  expand_limits(y=0.1)+
  scale_y_continuous(labels = label_number(accuracy = 0.01))+
  theme(text = element_text(size = 55),legend.position = "NONE",axis.title.y=element_blank())+
  geom_text(x=1.1, y=0.1, label="2021 Dvac",size=20)

#2022 - Dvac
Dvac_2022_Order<-ggplot(subset(Weight_by_Order_Dvac,Year==2022),aes(x=Grazing_Treatment,y=Average_Weight, fill=Correct_Order, position="stack"))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge), and fill in the bars with the color grey.  
  geom_bar(stat="identity")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  #Label the x-axis "Treatment"
  xlab("Grazing Treatment")+
  #Label the y-axis "Species Richness"
  ylab("Average Weight (g)")+
  theme(legend.background=element_blank())+
  scale_fill_manual(values=c("#661100","#CC6677","#DDCC77","#2D7947", "#4B4084","#7E69A0","#6B99C7","#76948F","#7B4B4E","#BCB9EC"), labels=c("Araneae","Coleoptera","Diptera","Hemiptera","Hymenoptera","Lepidoptera","Neuroptera","Orthoptera","Thysanoptera","Trombiculidae"), name = "Order")+
  scale_x_discrete(labels=c("HG"="High Impact Grazing","LG"="Destock","NG"="Cattle Removal"),limits=c("NG","LG","HG"))+
  theme(legend.key = element_rect(size=3), legend.key.size = unit(1,"centimeters"), legend.position=c(0.18,0.715))+
  #Make the y-axis extend to 50
  expand_limits(y=0.08)+
  scale_y_continuous(labels = label_number(accuracy = 0.01))+
  theme(text = element_text(size = 55),legend.position = "NONE",axis.title.y=element_blank())+
  geom_text(x=1.1, y=0.08, label="2022 Dvac",size=20)


#### Create Average Plot Weight Figure ####
  Dvac_2020_Order+  
  Dvac_2021_Order+
  Dvac_2022_Order+
  plot_layout(ncol = 3,nrow = 1)
#Save at 4500x3000


#### Changes in Orthoptera genra by grazing treatment ####

Weight_Orthoptera<-Weight_Data_Official %>% 
  filter(Correct_Order=="Orthoptera") %>% 
  separate(Coll_Year_Bl_Trt, c("Collection_Method","Year","Block","Grazing_Treatment"), "_") %>% 
  na.omit() %>% 
  dplyr::select(-Notes)

Weight_Orthoptera$Year=as.numeric(Weight_Orthoptera$Year)
Weight_Orthoptera$Block=as.character(Weight_Orthoptera$Block)
Weight_Orthoptera$Plot=as.numeric(Weight_Orthoptera$Plot)

ID_Orthoptera<-ID_Data_Official %>% 
  filter(Correct_Order=="Orthoptera") %>% 
  dplyr::select(-Notes)

Weight_Orthoptera_Official<-merge(Weight_Orthoptera, ID_Orthoptera, by=c("Collection_Method","Year","Block","Grazing_Treatment","Plot","Sample_Number","Correct_Order"),all=TRUE) %>%  
  filter(Sample_Number!="5a" & Sample_Number!="5b" & Sample_Number!="2a" & Sample_Number!="2b") %>% # need to be removed because of error
  na.omit() %>%  #all NAs need to be removed from 2020,2021,2022 (double checked all)
  filter(Correct_Family=="Acrididae")

#make dataframe with sum of each genus of orthoptera summed by plot
Weight_Orthoptera_Summed_D <- Weight_Orthoptera_Official %>% 
  group_by(Collection_Method, Year, Block, Grazing_Treatment,Plot,Correct_Genus) %>% 
  summarise(Genus_Weight=sum(Dry_Weight_g)) %>% 
  ungroup()

#Sweepnet
#make table a graph looking at differences in genus weight by grazing treatment
Weight_Orthoptera_Avg_D<-Weight_Orthoptera_Summed_D %>% 
  group_by(Year,Grazing_Treatment,Correct_Genus) %>% 
  summarise(Average_Weight=mean(Genus_Weight),Weight_SD=sd(Genus_Weight),Weight_n=length(Genus_Weight)) %>% 
  #Make a new column called "Richness_St_Error" and divide "Richness_Std" by the square root of "Richness_n"
  mutate(Weight_St_Error=Weight_SD/sqrt(Weight_n)) %>% 
  ungroup()

#### Orthoptera Genus Colors ####
#Ageneotettix "#661100"
#Amphiturnus "#CC6677"
#Arphia "#DDCC77"
#Chorthippus "#6699CC"
#Dissosteira #556F2E
#Eritettix #673F3F
#Melanoplus "#117733"
#Opeia "#332288"
#Phoetaliotes "#44AA99"
#Pseudopomala #7B66D9

#2020 - Dvac
Dvac_2020_Orthoptera<-ggplot(subset(Weight_Orthoptera_Avg_D,Year==2020),aes(x=Grazing_Treatment,y=Average_Weight, fill=Correct_Genus, position="stack"))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge), and fill in the bars with the color grey.  
  geom_bar(stat="identity")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  #Label the x-axis "Treatment"
  xlab("Grazing Treatment")+
  #Label the y-axis "Species Richness"
  ylab("Average Weight (g)")+
  theme(legend.background=element_blank())+
  scale_fill_manual(values=c("#661100","#CC6677","#DDCC77","#673F3F", "#117733","#332288", "#44AA99","#7B66D9"), labels=c("Ageneotettix","Amphiturnus","Arphia","Eritettix", "Melanoplus","Opeia","Phoetaliotes"), name = "Genus")+
  scale_x_discrete(labels=c("HG"="High Impact Grazing","LG"="Destock","NG"="Cattle Removal"),limits=c("NG","LG","HG"))+
  theme(legend.key = element_rect(size=3), legend.key.size = unit(1,"centimeters"), legend.position=c(0.18,0.715))+
  #Make the y-axis extend to 50
  expand_limits(y=0.5)+
  scale_y_continuous(labels = label_number(accuracy = 0.01))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45),legend.position="none")+
  geom_text(x=1.3, y=0.5, label="2020 Dvac",size=20)

#2021 - Dvac
Dvac_2021_Orthoptera<-ggplot(subset(Weight_Orthoptera_Avg_D,Year==2021),aes(x=Grazing_Treatment,y=Average_Weight, fill=Correct_Genus, position="stack"))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge), and fill in the bars with the color grey.  
  geom_bar(stat="identity")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  #Label the x-axis "Treatment"
  xlab("Grazing Treatment")+
  #Label the y-axis "Species Richness"
  ylab("Average Weight (g)")+
  theme(legend.background=element_blank())+
  scale_fill_manual(values=c("#661100","#DDCC77","#556F2E","#673F3F","#117733","#332288", "#44AA99"), labels=c("Ageneotettix","Arphia","Dissosteira","Erittix", "Melanoplus","Opeia","Phoetaliotes"), name = "Genus")+
  scale_x_discrete(labels=c("HG"="High Impact Grazing","LG"="Destock","NG"="Cattle Removal"),limits=c("NG","LG","HG"))+
  theme(legend.key = element_rect(size=3), legend.key.size = unit(1,"centimeters"), legend.position=c(0.18,0.715))+
  #Make the y-axis extend to 50
  expand_limits(y=0.1)+
  scale_y_continuous(labels = label_number(accuracy = 0.01))+
  theme(text = element_text(size = 55),legend.position = "NONE",axis.title.y=element_blank())+
  geom_text(x=1.3, y=0.1, label="2021 Dvac",size=20)

Dvac_2022_Orthoptera<-ggplot(subset(Weight_Orthoptera_Avg_D,Year==2022),aes(x=Grazing_Treatment,y=Average_Weight, fill=Correct_Genus, position="stack"))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge), and fill in the bars with the color grey.  
  geom_bar(stat="identity")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  #Label the x-axis "Treatment"
  xlab("Grazing Treatment")+
  #Label the y-axis "Species Richness"
  ylab("Average Weight (g)")+
  theme(legend.background=element_blank())+
  scale_fill_manual(values=c("#661100","#DDCC77","#556F2E","#673F3F","#117733","#332288", "#44AA99"), labels=c("Ageneotettix","Arphia","Dissosteira","Erittix", "Melanoplus","Opeia","Phoetaliotes"), name = "Genus")+
  scale_x_discrete(labels=c("HG"="High Impact Grazing","LG"="Destock","NG"="Cattle Removal"),limits=c("NG","LG","HG"))+
  theme(legend.key = element_rect(size=3), legend.key.size = unit(1,"centimeters"), legend.position=c(0.18,0.715))+
  #Make the y-axis extend to 50
  expand_limits(y=0.08)+
  scale_y_continuous(labels = label_number(accuracy = 0.01))+
  theme(text = element_text(size = 55),legend.position = "NONE",axis.title.y=element_blank())+
  geom_text(x=1.3, y=0.08, label="2022 Dvac",size=20)


#### Create Average Plot Weight Figure Orthoptera####
  Dvac_2020_Orthoptera+  
  Dvac_2021_Orthoptera+
  Dvac_2022_Orthoptera+
  plot_layout(ncol = 3,nrow = 1)
#Save at 4500x3000

#### Normality: Plot Weights by Orthoptera ####

# Dvac 2020
dvac_2020_Weight_Orthoptera <- lm(data = subset(Weight_Orthoptera_Summed_D, Year == 2020), log(Genus_Weight)  ~ Grazing_Treatment)
ols_plot_resid_hist(dvac_2020_Weight_Orthoptera) 
ols_test_normality(dvac_2020_Weight_Orthoptera) #normalish

# dvac 2021
dvac_2021_Weight_Orthoptera <- lm(data = subset(Weight_Orthoptera_Summed_D, Year == 2021), sqrt(Genus_Weight)  ~ Grazing_Treatment)
ols_plot_resid_hist(dvac_2021_Weight_Orthoptera) 
ols_test_normality(dvac_2021_Weight_Orthoptera) #normalish

# dvac 2022
dvac_2022_Weight_Orthoptera <- lm(data = subset(Weight_Orthoptera_Summed_D, Year == 2022), log(Genus_Weight)  ~ Grazing_Treatment)
ols_plot_resid_hist(dvac_2022_Weight_Orthoptera) 
ols_test_normality(dvac_2022_Weight_Orthoptera) #normal

#### Glmm for Plot Weights by Grazing Treatment Orthoptera####

# 2020 Dvac
Plot_Weight_D_2020_Glmm_Orthoptera <- lmer(log(Genus_Weight) ~ Grazing_Treatment + (1 | Block) , data = subset(Weight_Orthoptera_Summed_D,Year==2020))
anova(Plot_Weight_D_2020_Glmm_Orthoptera) #not significant

# 2021 Dvac
Plot_Weight_D_2021_Glmm_Orthoptera <- lmer(sqrt(Genus_Weight) ~ Grazing_Treatment + (1 | Block) , data = subset(Weight_Orthoptera_Summed_D,Year==2021))
anova(Plot_Weight_D_2021_Glmm_Orthoptera) # p=0.05
## post hoc test for lmer test ##
summary(glht(Plot_Weight_D_2021_Glmm_Orthoptera, linfct = mcp(Grazing_Treatment = "Tukey")), test = adjusted(type = "BH")) #NG-LG (p=0.2833), #LG-HG (0.2113, NG-HG (0.0391)

# 2022 Dvac
Plot_Weight_D_2022_Glmm_Orthoptera <- lmer(log(Genus_Weight) ~ Grazing_Treatment + (1 | Block) , data = subset(Weight_Orthoptera_Summed_D,Year==2022))
anova(Plot_Weight_D_2022_Glmm_Orthoptera) #not significant

#### Calculate Community Metrics: Weight Abundance ####
# uses codyn package and finds shannon's diversity 
Weight_Data_Summed_2<-Weight_Data_Summed %>% 
  filter(Plot!="NA")
Diversity_Weight <- community_diversity(df = Weight_Data_Summed_2,
                                 time.var = "Year",
                                 replicate.var = c("Collection_Method","Plot","Block","Grazing_Treatment"),
                                 abundance.var = "Dry_Weight_g")
#Sweep Net Community Structure
Structure_Weight <- community_structure(df = Weight_Data_Summed_2,
                                 time.var = "Year",
                                 replicate.var = c("Collection_Method","Plot","Block","Grazing_Treatment"),
                                 abundance.var = "Dry_Weight_g",
                                 metric = "Evar")

#Make a new data frame from "Extra_Species_Identity" to generate richness values for each research area
Order_Richness_Weight<-ID_Data_Official %>%  
  select(Collection_Method,Year,Block,Grazing_Treatment,Plot,Correct_Order) %>% 
  unique() %>% 
  #group data frame by Watershed and exclosure
  group_by(Collection_Method,Year,Block,Grazing_Treatment,Plot) %>%
  #Make a new column named "Richness" and add the unique number of rows in the column "taxa" according to the groupings
  summarise(richness=length(Correct_Order)) %>%
  #stop grouping by watershed and exclosure
  ungroup()

Order_Richness_Weight$Year=as.character(Order_Richness_Weight$Year)
Order_Richness_Weight$Plot=as.character(Order_Richness_Weight$Plot)

#join the datasets
CommunityMetrics_Weight <- Diversity_Weight %>%
  full_join(Structure_Weight) %>% 
  select(-richness) %>% 
  full_join(Order_Richness_Weight)

#make dataframe with averages
CommunityMetrics_Weight_Avg<-CommunityMetrics_Weight  %>% 
  group_by(Year,Grazing_Treatment) %>%
  summarize(Richness_Std=sd(richness),Richness_Mean=mean(richness),Richness_n=length(richness),
            Shannon_Std=sd(Shannon),Shannon_Mean=mean(Shannon),Shannon_n=length(Shannon),
            Evar_Std=sd(Evar,na.rm=T),Evar_Mean=mean(Evar,na.rm=T),Evar_n=length(Evar))%>%
  mutate(Richness_St_Error=Richness_Std/sqrt(Richness_n),
         Shannon_St_Error=Shannon_Std/sqrt(Shannon_n),
         Evar_St_Error=Evar_Std/sqrt(Evar_n)) %>% 
  ungroup()

#### Calculate Community Metrics: Count Abundance ####
# uses codyn package and finds shannon's diversity 
Diversity_Count <- community_diversity(df = Abundance,
                                           time.var = "Year",
                                           replicate.var = c("Collection_Method","Plot","Block","Grazing_Treatment"),
                                           abundance.var = "Abundance")
#Sweep Net Community Structure
Structure_Count <- community_structure(df = Abundance,
                                 time.var = "Year",
                                 replicate.var = c("Collection_Method","Plot","Block","Grazing_Treatment"),
                                 abundance.var = "Abundance",
                                    metric = "Evar")

#Make a new data frame from "Extra_Species_Identity" to generate richness values for each research area
Order_Richness_Count<-ID_Data_Official %>%  
  select(Collection_Method,Year,Block,Grazing_Treatment,Plot,Correct_Order) %>% 
  unique() %>% 
  #group data frame by Watershed and exclosure
  group_by(Collection_Method,Year,Block,Grazing_Treatment,Plot) %>%
  #Make a new column named "Richness" and add the unique number of rows in the column "taxa" according to the groupings
  summarise(richness=length(Correct_Order)) %>%
  #stop grouping by watershed and exclosure
  ungroup()

#join the datasets
CommunityMetrics_Count <- Diversity_Count %>%
  full_join(Structure_Count) %>% 
  select(-richness) %>% 
  full_join(Order_Richness_Count)
  
#make dataframe with averages
CommunityMetrics_Count_Avg<-CommunityMetrics_Count  %>% 
  group_by(Collection_Method,Year,Grazing_Treatment) %>%
  summarize(Richness_Std=sd(richness),Richness_Mean=mean(richness),Richness_n=length(richness),
            Shannon_Std=sd(Shannon),Shannon_Mean=mean(Shannon),Shannon_n=length(Shannon),
            Evar_Std=sd(Evar),Evar_Mean=mean(Evar),Evar_n=length(Evar))%>%
  mutate(Richness_St_Error=Richness_Std/sqrt(Richness_n),
         Shannon_St_Error=Shannon_Std/sqrt(Shannon_n),
         Evar_St_Error=Evar_Std/sqrt(Evar_n)) %>% 
  ungroup()

#### Plot Richness ####

# 2020 - Dvac
Richness_Count_2020<-ggplot(subset(CommunityMetrics_Count_Avg,Year==2020 & Collection_Method=="dvac"),aes(x=Grazing_Treatment,y=Richness_Mean,fill=Grazing_Treatment))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge)
  geom_bar(stat="identity",position = "dodge")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(aes(ymin=Richness_Mean-Richness_St_Error,ymax=Richness_Mean+Richness_St_Error),position=position_dodge(),width=0.2)+
  #Label the x-axis "Treatment"
  xlab("Grazing Treatment")+
  #Label the y-axis "Species Richness"
  ylab("Order Richness")+
  theme(legend.background=element_blank())+
  scale_x_discrete(labels=c("HG"="High Impact Grazing","LG"="Destock","NG"="Cattle Removal"),limits=c("NG","LG","HG"))+
  scale_fill_manual(values=c("thistle2","thistle3","thistle4"), labels=c("High Impact Grazing","Cattle Removal","Destock"))+
  theme(legend.key = element_rect(size=3), legend.key.size = unit(1,"centimeters"),legend.position="NONE")+
  #Make the y-axis extend to 50
  expand_limits(y=10)+
  scale_y_continuous(labels = label_number(accuracy = 1))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45))+
  geom_text(x=0.85, y=10, label="2020 Count",size=20)

# 2021 - Dvac
#Graph of Weights from dvac by Grazing treatment- 2021
Richness_Count_2021<-ggplot(subset(CommunityMetrics_Count_Avg,Year==2021 & Collection_Method=="dvac"),aes(x=Grazing_Treatment,y=Richness_Mean,fill=Grazing_Treatment))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge)
  geom_bar(stat="identity",position = "dodge")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(aes(ymin=Richness_Mean-Richness_St_Error,ymax=Richness_Mean+Richness_St_Error),position=position_dodge(),width=0.2)+
  #Label the x-axis "Treatment"
  xlab("Grazing Treatment")+
  #Label the y-axis "Species Richness"
  ylab("Richness")+
  theme(legend.background=element_blank())+
  scale_x_discrete(labels=c("HG"="High Impact Grazing","LG"="Destock","NG"="Cattle Removal"),limits=c("NG","LG","HG"))+
  scale_fill_manual(values=c("thistle2","thistle3","thistle4"), labels=c("High Impact Grazing","Cattle Removal","Destock"))+
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),legend.position = "none")+
  #Make the y-axis extend to 50
  expand_limits(y=10)+
  scale_y_continuous(labels = label_number(accuracy = 1))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45))+
  geom_text(x=0.85, y=10, label="2021 Count",size=20)

# 2022 - Dvac
Richness_Count_2022<-ggplot(subset(CommunityMetrics_Count_Avg,Year==2022 & Collection_Method=="dvac"),aes(x=Grazing_Treatment,y=Richness_Mean,fill=Grazing_Treatment))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge)
  geom_bar(stat="identity",position = "dodge")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(aes(ymin=Richness_Mean-Richness_St_Error,ymax=Richness_Mean+Richness_St_Error),position=position_dodge(),width=0.2)+
  #Label the x-axis "Treatment"
  xlab("Grazing Treatment")+
  #Label the y-axis "Species Richness"
  ylab("Richness")+
  theme(legend.background=element_blank())+
  scale_x_discrete(labels=c("HG"="High Impact Grazing","LG"="Destock","NG"="Cattle Removal"),limits=c("NG","LG","HG"))+
  scale_fill_manual(values=c("thistle2","thistle3","thistle4"), labels=c("High Impact Grazing","Cattle Removal","Destock"))+
  theme(axis.title.y=element_blank(),legend.position = "none",axis.text.y=element_blank())+
  #Make the y-axis extend to 50
  expand_limits(y=10)+
  scale_y_continuous(labels = label_number(accuracy = 1))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45))+
  geom_text(x=0.85, y=10, label="2022 Count",size=20)

#### Create Order Richness Figure ####
Richness_Count_2020+  
  Richness_Count_2021+
  Richness_Count_2022+
  plot_layout(ncol = 3,nrow = 1)
#Save at 4000x2000


#### Normality: Order Richness ####

# Dvac 2020
dvac_2020_OrderRichness <- lm(data = subset(CommunityMetrics, Year == 2020 & Collection_Method=="dvac"),log(richness)  ~ Grazing_Treatment)
ols_plot_resid_hist(dvac_2020_OrderRichness) 
ols_test_normality(dvac_2020_OrderRichness) #normalish

# dvac 2021
dvac_2021_OrderRichness <- lm(data = subset(CommunityMetrics, Year == 2021 & Collection_Method=="dvac"),(richness)  ~ Grazing_Treatment)
ols_plot_resid_hist(dvac_2021_OrderRichness) 
ols_test_normality(dvac_2021_OrderRichness) #normalish

# dvac 2022
dvac_2022_OrderRichness <- lm(data = subset(CommunityMetrics, Year == 2022 & Collection_Method=="dvac"), (richness)  ~ Grazing_Treatment)
ols_plot_resid_hist(dvac_2022_OrderRichness) 
ols_test_normality(dvac_2022_OrderRichness) #normal

#### Glmm for Richness by Grazing Treatment Orthoptera####

# 2020 Dvac
OrderRichness_D_2020_Glmm <- lmer(log(richness) ~ Grazing_Treatment + (1 | Block) , data = subset(CommunityMetrics,Year==2020 & Collection_Method=="dvac"))
anova(OrderRichness_D_2020_Glmm) #not significant

# 2021 Dvac
OrderRichness_D_2021_Glmm <- lmer((richness) ~ Grazing_Treatment + (1 | Block) , data = subset(CommunityMetrics,Year==2021 & Collection_Method=="dvac"))
anova(OrderRichness_D_2021_Glmm) #not significant 

# 2022 Dvac
OrderRichness_D_2022_Glmm <- lmer((richness) ~ Grazing_Treatment + (1 | Block) , data = subset(CommunityMetrics,Year==2022 & Collection_Method=="dvac"))
anova(OrderRichness_D_2022_Glmm) #not significant


#### Plot Shannon: Weight ####

# 2020 - Weight
Shannon_2020_Weight<-ggplot(subset(CommunityMetrics_Weight_Avg,Year==2020),aes(x=Grazing_Treatment,y=Shannon_Mean,fill=Grazing_Treatment))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge)
  geom_bar(stat="identity",position = "dodge")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(aes(ymin=Shannon_Mean-Shannon_St_Error,ymax=Shannon_Mean+Shannon_St_Error),position=position_dodge(),width=0.2)+
  #Label the x-axis "Treatment"
  xlab("Grazing Treatment")+
  #Label the y-axis "Species Shannon"
  ylab("Shannon's Diversity")+
  theme(legend.background=element_blank())+
  scale_x_discrete(labels=c("HG"="High Impact Grazing","LG"="Destock","NG"="Cattle Removal"),limits=c("NG","LG","HG"))+
  scale_fill_manual(values=c("thistle2","thistle3","thistle4"), labels=c("High Impact Grazing","Cattle Removal","Destock"))+
  theme(legend.key = element_rect(size=3), legend.key.size = unit(1,"centimeters"),legend.position="NONE")+
  #Make the y-axis extend to 50
  expand_limits(y=1)+
  scale_y_continuous(labels = label_number(accuracy = 0.1))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45))+
  geom_text(x=0.85, y=1, label="2020 Weight",size=20)

# 2021 - Dvac
#Graph of Weights from dvac by Grazing treatment- 2021
Shannon_2021_Weight<-ggplot(subset(CommunityMetrics_Weight_Avg,Year==2021),aes(x=Grazing_Treatment,y=Shannon_Mean,fill=Grazing_Treatment))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge)
  geom_bar(stat="identity",position = "dodge")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(aes(ymin=Shannon_Mean-Shannon_St_Error,ymax=Shannon_Mean+Shannon_St_Error),position=position_dodge(),width=0.2)+
  #Label the x-axis "Treatment"
  xlab("Grazing Treatment")+
  #Label the y-axis "Species Shannon"
  ylab("Shannon's Diversity")+
  theme(legend.background=element_blank())+
  scale_x_discrete(labels=c("HG"="High Impact Grazing","LG"="Destock","NG"="Cattle Removal"),limits=c("NG","LG","HG"))+
  scale_fill_manual(values=c("thistle2","thistle3","thistle4"), labels=c("High Impact Grazing","Cattle Removal","Destock"))+
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),legend.position = "none")+
  #Make the y-axis extend to 50
  expand_limits(y=1)+
  scale_y_continuous(labels = label_number(accuracy = 0.1))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45))+
  geom_text(x=0.85, y=1, label="2021 Weight",size=20)

# 2022 - Dvac
Shannon_2022_Weight<-ggplot(subset(CommunityMetrics_Weight_Avg,Year==2022),aes(x=Grazing_Treatment,y=Shannon_Mean,fill=Grazing_Treatment))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge)
  geom_bar(stat="identity",position = "dodge")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(aes(ymin=Shannon_Mean-Shannon_St_Error,ymax=Shannon_Mean+Shannon_St_Error),position=position_dodge(),width=0.2)+
  #Label the x-axis "Treatment"
  xlab("Grazing Treatment")+
  #Label the y-axis "Species Shannon"
  ylab("Shannon's Diversity")+
  theme(legend.background=element_blank())+
  scale_x_discrete(labels=c("HG"="High Impact Grazing","LG"="Destock","NG"="Cattle Removal"),limits=c("NG","LG","HG"))+
  scale_fill_manual(values=c("thistle2","thistle3","thistle4"), labels=c("High Impact Grazing","Cattle Removal","Destock"))+
  theme(axis.title.y=element_blank(),legend.position = "none",axis.text.y=element_blank())+
  #Make the y-axis extend to 50
  expand_limits(y=1)+
  scale_y_continuous(labels = label_number(accuracy = 0.1))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45))+
  geom_text(x=0.85, y=1, label="2022 Weight",size=20)

#### Create Order Shannon Figure: Weight####
Shannon_2020_Weight+  
  Shannon_2021_Weight+
  Shannon_2022_Weight+
  plot_layout(ncol = 3,nrow = 1)
#Save at 4000x2000

#### Normality: Order Shannon: Weight ####

# Weight 2020
Weight_2020_OrderShannon <- lm(data = subset(CommunityMetrics_Weight, Year == 2020),(Shannon)  ~ Grazing_Treatment)
ols_plot_resid_hist(Weight_2020_OrderShannon) 
ols_test_normality(Weight_2020_OrderShannon) #normal

# Weight 2021
Weight_2021_OrderShannon <- lm(data = subset(CommunityMetrics_Weight, Year == 2021),(Shannon)  ~ Grazing_Treatment)
ols_plot_resid_hist(Weight_2021_OrderShannon) 
ols_test_normality(Weight_2021_OrderShannon) #normal

# Weight 2020
Weight_2022_OrderShannon <- lm(data = subset(CommunityMetrics_Weight, Year == 2022),(Shannon)  ~ Grazing_Treatment)
ols_plot_resid_hist(Weight_2022_OrderShannon) 
ols_test_normality(Weight_2022_OrderShannon) #normal

#### Glmm for Shannon's Diversity by Grazing Treatment: Weight####

# 2020 Weight
OrderShannon_2020_Glmm_Weight <- lmer((Shannon) ~ Grazing_Treatment + (1 | Block) , data = subset(CommunityMetrics_Weight,Year==2020))
anova(OrderShannon_2020_Glmm_Weight) #not significant

# 2021 Weight
OrderShannon_2021_Glmm_Weight <- lmer((Shannon) ~ Grazing_Treatment + (1 | Block) , data = subset(CommunityMetrics_Weight,Year==2021))
anova(OrderShannon_2021_Glmm_Weight) #0.005528
### post hoc test for lmer test ##
summary(glht(OrderShannon_2021_Glmm_Weight, linfct = mcp(Grazing_Treatment = "Tukey")), test = adjusted(type = "BH")) #NG-LG (p=0.09455), #LG-HG (0.09455), NG-HG (0.00178)

# 2022 Weight
OrderShannon_2022_Glmm_Weight <- lmer((Shannon) ~ Grazing_Treatment + (1 | Block) , data = subset(CommunityMetrics_Weight,Year==2022))
anova(OrderShannon_2022_Glmm_Weight) #not significant



#### Plot Shannon: Count ####

# 2020 - Dvac
Shannon_2020_Dvac<-ggplot(subset(CommunityMetrics_Avg,Year==2020 & Collection_Method=="dvac"),aes(x=Grazing_Treatment,y=Shannon_Mean,fill=Grazing_Treatment))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge)
  geom_bar(stat="identity",position = "dodge")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(aes(ymin=Shannon_Mean-Shannon_St_Error,ymax=Shannon_Mean+Shannon_St_Error),position=position_dodge(),width=0.2)+
  #Label the x-axis "Treatment"
  xlab("Grazing Treatment")+
  #Label the y-axis "Species Shannon"
  ylab("Shannon's Diversity")+
  theme(legend.background=element_blank())+
  scale_x_discrete(labels=c("HG"="High Impact Grazing","LG"="Destock","NG"="Cattle Removal"),limits=c("NG","LG","HG"))+
  scale_fill_manual(values=c("thistle2","thistle3","thistle4"), labels=c("High Impact Grazing","Cattle Removal","Destock"))+
  theme(legend.key = element_rect(size=3), legend.key.size = unit(1,"centimeters"),legend.position="NONE")+
  #Make the y-axis extend to 50
  expand_limits(y=6)+
  scale_y_continuous(labels = label_number(accuracy = 1))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45))+
  geom_text(x=0.85, y=6, label="2020 Count",size=20)

# 2021 - Dvac
#Graph of Weights from dvac by Grazing treatment- 2021
Shannon_2021_Dvac<-ggplot(subset(CommunityMetrics_Avg,Year==2021 & Collection_Method=="dvac"),aes(x=Grazing_Treatment,y=Shannon_Mean,fill=Grazing_Treatment))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge)
  geom_bar(stat="identity",position = "dodge")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(aes(ymin=Shannon_Mean-Shannon_St_Error,ymax=Shannon_Mean+Shannon_St_Error),position=position_dodge(),width=0.2)+
  #Label the x-axis "Treatment"
  xlab("Grazing Treatment")+
  #Label the y-axis "Species Shannon"
  ylab("Shannon's Diversity")+
  theme(legend.background=element_blank())+
  scale_x_discrete(labels=c("HG"="High Impact Grazing","LG"="Destock","NG"="Cattle Removal"),limits=c("NG","LG","HG"))+
  scale_fill_manual(values=c("thistle2","thistle3","thistle4"), labels=c("High Impact Grazing","Cattle Removal","Destock"))+
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),legend.position = "none")+
  #Make the y-axis extend to 50
  expand_limits(y=6)+
  scale_y_continuous(labels = label_number(accuracy = 1))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45))+
  geom_text(x=0.85, y=6, label="2021 Count",size=20)

# 2022 - Dvac
Shannon_2022_Dvac<-ggplot(subset(CommunityMetrics_Avg,Year==2022 & Collection_Method=="dvac"),aes(x=Grazing_Treatment,y=Shannon_Mean,fill=Grazing_Treatment))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge)
  geom_bar(stat="identity",position = "dodge")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(aes(ymin=Shannon_Mean-Shannon_St_Error,ymax=Shannon_Mean+Shannon_St_Error),position=position_dodge(),width=0.2)+
  #Label the x-axis "Treatment"
  xlab("Grazing Treatment")+
  #Label the y-axis "Species Shannon"
  ylab("Shannon's Diversity")+
  theme(legend.background=element_blank())+
  scale_x_discrete(labels=c("HG"="High Impact Grazing","LG"="Destock","NG"="Cattle Removal"),limits=c("NG","LG","HG"))+
  scale_fill_manual(values=c("thistle2","thistle3","thistle4"), labels=c("High Impact Grazing","Cattle Removal","Destock"))+
  theme(axis.title.y=element_blank(),legend.position = "none",axis.text.y=element_blank())+
  #Make the y-axis extend to 50
  expand_limits(y=6)+
  scale_y_continuous(labels = label_number(accuracy = 1))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45))+
  geom_text(x=0.85, y=6, label="2022 Count",size=20)

#### Create Order Shannon Figure: Count ####
Shannon_2020_Dvac+  
  Shannon_2021_Dvac+
  Shannon_2022_Dvac+
  plot_layout(ncol = 3,nrow = 1)
#Save at 4000x2000

#### Normality: Order Shannon: Count ####

# Dvac 2020
dvac_2020_OrderShannon <- lm(data = subset(CommunityMetrics, Year == 2020 & Collection_Method=="dvac"),(Shannon)  ~ Grazing_Treatment)
ols_plot_resid_hist(dvac_2020_OrderShannon) 
ols_test_normality(dvac_2020_OrderShannon) #normalish

# dvac 2021
dvac_2021_OrderShannon <- lm(data = subset(CommunityMetrics, Year == 2021 & Collection_Method=="dvac"),(Shannon)  ~ Grazing_Treatment)
ols_plot_resid_hist(dvac_2021_OrderShannon) 
ols_test_normality(dvac_2021_OrderShannon) #normalish

# dvac 2022
dvac_2022_OrderShannon <- lm(data = subset(CommunityMetrics, Year == 2022 & Collection_Method=="dvac"), (Shannon)  ~ Grazing_Treatment)
ols_plot_resid_hist(dvac_2022_OrderShannon) 
ols_test_normality(dvac_2022_OrderShannon) #normal

#### Glmm for Shannon's Diversity by Grazing Treatment: Count####

# 2020 Dvac
OrderShannon_D_2020_Glmm <- lmer((Shannon) ~ Grazing_Treatment + (1 | Block) , data = subset(CommunityMetrics,Year==2020 & Collection_Method=="dvac"))
anova(OrderShannon_D_2020_Glmm) #not significant

# 2021 Dvac
OrderShannon_D_2021_Glmm <- lmer((Shannon) ~ Grazing_Treatment + (1 | Block) , data = subset(CommunityMetrics,Year==2021 & Collection_Method=="dvac"))
anova(OrderShannon_D_2021_Glmm) #0.03554 
### post hoc test for lmer test ##
summary(glht(OrderShannon_D_2021_Glmm, linfct = mcp(Grazing_Treatment = "Tukey")), test = adjusted(type = "BH")) #NG-LG (p=2733), #LG-HG (1695), NG-HG (0.0221)

# 2022 Dvac
OrderShannon_D_2022_Glmm <- lmer((Shannon) ~ Grazing_Treatment + (1 | Block) , data = subset(CommunityMetrics,Year==2022 & Collection_Method=="dvac"))
anova(OrderShannon_D_2022_Glmm) #0.01073
## post hoc test for lmer test ##
summary(glht(OrderShannon_D_2022_Glmm, linfct = mcp(Grazing_Treatment = "Tukey")), test = adjusted(type = "BH")) #NG-LG (p=0.05361), #LG-HG (0.00524), NG-HG (.30277)

#### Plot Evar: Weight ####

# 2020 - Weight
Evar_2020_Weight<-ggplot(subset(CommunityMetrics_Weight_Avg,Year==2020),aes(x=Grazing_Treatment,y=Evar_Mean,fill=Grazing_Treatment))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge)
  geom_bar(stat="identity",position = "dodge")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(aes(ymin=Evar_Mean-Evar_St_Error,ymax=Evar_Mean+Evar_St_Error),position=position_dodge(),width=0.2)+
  #Label the x-axis "Treatment"
  xlab("Grazing Treatment")+
  #Label the y-axis "Species Evar"
  ylab("Evenness")+
  theme(legend.background=element_blank())+
  scale_x_discrete(labels=c("HG"="High Impact Grazing","LG"="Destock","NG"="Cattle Removal"),limits=c("NG","LG","HG"))+
  scale_fill_manual(values=c("thistle2","thistle3","thistle4"), labels=c("High Impact Grazing","Cattle Removal","Destock"))+
  theme(legend.key = element_rect(size=3), legend.key.size = unit(1,"centimeters"),legend.position="NONE")+
  #Make the y-axis extend to 50
  expand_limits(y=1)+
  scale_y_continuous(labels = label_number(accuracy = .01))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45))+
  geom_text(x=0.85, y=1, label="2020 Weight",size=20)

# 2021 - Weight
#Graph of Weights from dvac by Grazing treatment- 2021
Evar_2021_Weight<-ggplot(subset(CommunityMetrics_Avg,Year==2021),aes(x=Grazing_Treatment,y=Evar_Mean,fill=Grazing_Treatment))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge)
  geom_bar(stat="identity",position = "dodge")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(aes(ymin=Evar_Mean-Evar_St_Error,ymax=Evar_Mean+Evar_St_Error),position=position_dodge(),width=0.2)+
  #Label the x-axis "Treatment"
  xlab("Grazing Treatment")+
  #Label the y-axis "Species Evar"
  ylab("Evenness")+
  theme(legend.background=element_blank())+
  scale_x_discrete(labels=c("HG"="High Impact Grazing","LG"="Destock","NG"="Cattle Removal"),limits=c("NG","LG","HG"))+
  scale_fill_manual(values=c("thistle2","thistle3","thistle4"), labels=c("High Impact Grazing","Cattle Removal","Destock"))+
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),legend.position = "none")+
  #Make the y-axis extend to 50
  expand_limits(y=1)+
  scale_y_continuous(labels = label_number(accuracy = .01))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45))+
  geom_text(x=0.85, y=1, label="2021 Weight",size=20)

# 2022 - Weight
Evar_2022_Weight<-ggplot(subset(CommunityMetrics_Avg,Year==2022),aes(x=Grazing_Treatment,y=Evar_Mean,fill=Grazing_Treatment))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge)
  geom_bar(stat="identity",position = "dodge")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(aes(ymin=Evar_Mean-Evar_St_Error,ymax=Evar_Mean+Evar_St_Error),position=position_dodge(),width=0.2)+
  #Label the x-axis "Treatment"
  xlab("Grazing Treatment")+
  #Label the y-axis "Species Evar"
  ylab("Evenness")+
  theme(legend.background=element_blank())+
  scale_x_discrete(labels=c("HG"="High Impact Grazing","LG"="Destock","NG"="Cattle Removal"),limits=c("NG","LG","HG"))+
  scale_fill_manual(values=c("thistle2","thistle3","thistle4"), labels=c("High Impact Grazing","Cattle Removal","Destock"))+
  theme(axis.title.y=element_blank(),legend.position = "none",axis.text.y=element_blank())+
  #Make the y-axis extend to 50
  expand_limits(y=1)+
  scale_y_continuous(labels = label_number(accuracy = .01))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45))+
  geom_text(x=0.85, y=1, label="2022 Weight",size=20)

#### Create Order Evar Figure: Weight ####
Evar_2020_Weight+  
  Evar_2021_Weight+
  Evar_2022_Weight+
  plot_layout(ncol = 3,nrow = 1)
#Save at 4000x2000


#### Normality: Order Evar: Weight ####

# Weight 2020
Weight_2020_OrderEvar <- lm(data = subset(CommunityMetrics_Weight, Year == 2020 & Collection_Method=="dvac"),1/(Evar)  ~ Grazing_Treatment)
ols_plot_resid_hist(Weight_2020_OrderEvar) 
ols_test_normality(Weight_2020_OrderEvar) #normalish

# Weight 2021
Weight_2021_OrderEvar <- lm(data = subset(CommunityMetrics_Weight, Year == 2021 & Collection_Method=="dvac"),log(Evar)  ~ Grazing_Treatment)
ols_plot_resid_hist(Weight_2021_OrderEvar) 
ols_test_normality(Weight_2021_OrderEvar) #normalish

# Weight 2022
Weight_2022_OrderEvar <- lm(data = subset(CommunityMetrics_Weight, Year == 2022 & Collection_Method=="dvac"),1/(Evar)  ~ Grazing_Treatment)
ols_plot_resid_hist(Weight_2022_OrderEvar) 
ols_test_normality(Weight_2022_OrderEvar) #normalish
#### Glmm for Evenness by Grazing Treatment: Weight ####

# 2020 Weight
OrderEvar_2020_Glmm_Weight <- lmer(1/(Evar) ~ Grazing_Treatment + (1 | Block) , data = subset(CommunityMetrics_Weight,Year==2020 ))
anova(OrderEvar_2020_Glmm_Weight) #not significant

# 2021 Weight
OrderEvar_2021_Glmm_Weight <- lmer((log(Evar)) ~ Grazing_Treatment + (1 | Block) , data = subset(CommunityMetrics_Weight,Year==2021 ))
anova(OrderEvar_2021_Glmm_Weight) #not significant

# 2020 Weight
OrderEvar_2021_Glmm_Weight <- lmer((1/Evar) ~ Grazing_Treatment + (1 | Block) , data = subset(CommunityMetrics_Weight,Year==2021 ))
anova(OrderEvar_2021_Glmm_Weight) #not significant

#### Plot Evar: Count ####

# 2020 - Count
Evar_2020_Count<-ggplot(subset(CommunityMetrics_Avg,Year==2020 & Collection_Method=="dvac"),aes(x=Grazing_Treatment,y=Evar_Mean,fill=Grazing_Treatment))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge)
  geom_bar(stat="identity",position = "dodge")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(aes(ymin=Evar_Mean-Evar_St_Error,ymax=Evar_Mean+Evar_St_Error),position=position_dodge(),width=0.2)+
  #Label the x-axis "Treatment"
  xlab("Grazing Treatment")+
  #Label the y-axis "Species Evar"
  ylab("Evenness")+
  theme(legend.background=element_blank())+
  scale_x_discrete(labels=c("HG"="High Impact Grazing","LG"="Destock","NG"="Cattle Removal"),limits=c("NG","LG","HG"))+
  scale_fill_manual(values=c("thistle2","thistle3","thistle4"), labels=c("High Impact Grazing","Cattle Removal","Destock"))+
  theme(legend.key = element_rect(size=3), legend.key.size = unit(1,"centimeters"),legend.position="NONE")+
  #Make the y-axis extend to 50
  expand_limits(y=1)+
  scale_y_continuous(labels = label_number(accuracy = .01))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45))+
  geom_text(x=0.85, y=1, label="2020 Count",size=20)

# 2021 - Count
#Graph of Weights from dvac by Grazing treatment- 2021
Evar_2021_Count<-ggplot(subset(CommunityMetrics_Avg,Year==2021 & Collection_Method=="dvac"),aes(x=Grazing_Treatment,y=Evar_Mean,fill=Grazing_Treatment))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge)
  geom_bar(stat="identity",position = "dodge")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(aes(ymin=Evar_Mean-Evar_St_Error,ymax=Evar_Mean+Evar_St_Error),position=position_dodge(),width=0.2)+
  #Label the x-axis "Treatment"
  xlab("Grazing Treatment")+
  #Label the y-axis "Species Evar"
  ylab("Evenness")+
  theme(legend.background=element_blank())+
  scale_x_discrete(labels=c("HG"="High Impact Grazing","LG"="Destock","NG"="Cattle Removal"),limits=c("NG","LG","HG"))+
  scale_fill_manual(values=c("thistle2","thistle3","thistle4"), labels=c("High Impact Grazing","Cattle Removal","Destock"))+
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),legend.position = "none")+
  #Make the y-axis extend to 50
  expand_limits(y=1)+
  scale_y_continuous(labels = label_number(accuracy = .01))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45))+
  geom_text(x=0.85, y=1, label="2021 Count",size=20)

# 2022 - Count
Evar_2022_Count<-ggplot(subset(CommunityMetrics_Avg,Year==2022 & Collection_Method=="dvac"),aes(x=Grazing_Treatment,y=Evar_Mean,fill=Grazing_Treatment))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge)
  geom_bar(stat="identity",position = "dodge")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(aes(ymin=Evar_Mean-Evar_St_Error,ymax=Evar_Mean+Evar_St_Error),position=position_dodge(),width=0.2)+
  #Label the x-axis "Treatment"
  xlab("Grazing Treatment")+
  #Label the y-axis "Species Evar"
  ylab("Evenness")+
  theme(legend.background=element_blank())+
  scale_x_discrete(labels=c("HG"="High Impact Grazing","LG"="Destock","NG"="Cattle Removal"),limits=c("NG","LG","HG"))+
  scale_fill_manual(values=c("thistle2","thistle3","thistle4"), labels=c("High Impact Grazing","Cattle Removal","Destock"))+
  theme(axis.title.y=element_blank(),legend.position = "none",axis.text.y=element_blank())+
  #Make the y-axis extend to 50
  expand_limits(y=1)+
  scale_y_continuous(labels = label_number(accuracy = .01))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45))+
  geom_text(x=0.85, y=1, label="2022 Count",size=20)

#### Create Order Evar Figure ####
Evar_2020_Count+  
  Evar_2021_Count+
  Evar_2022_Count+
  plot_layout(ncol = 3,nrow = 1)
#Save at 4000x2000


#### Normality: Order Evar: Count ####

# Dvac 2020
dvac_2020_OrderEvar <- lm(data = subset(CommunityMetrics, Year == 2020 & Collection_Method=="dvac"),(Evar)  ~ Grazing_Treatment)
ols_plot_resid_hist(dvac_2020_OrderEvar) 
ols_test_normality(dvac_2020_OrderEvar) #normalish

# dvac 2021
dvac_2021_OrderEvar <- lm(data = subset(CommunityMetrics, Year == 2021 & Collection_Method=="dvac"),(Evar)  ~ Grazing_Treatment)
ols_plot_resid_hist(dvac_2021_OrderEvar) 
ols_test_normality(dvac_2021_OrderEvar) #normalish

# dvac 2022
dvac_2022_OrderEvar <- lm(data = subset(CommunityMetrics, Year == 2022 & Collection_Method=="dvac"), 1/(Evar)  ~ Grazing_Treatment)
ols_plot_resid_hist(dvac_2022_OrderEvar) 
ols_test_normality(dvac_2022_OrderEvar) #normalish

#### Glmm for Evenness by Grazing Treatment ####

# 2020 Dvac
OrderEvar_D_2020_Glmm <- lmer((Evar) ~ Grazing_Treatment + (1 | Block) , data = subset(CommunityMetrics,Year==2020 & Collection_Method=="dvac"))
anova(OrderEvar_D_2020_Glmm) #not significant

# 2021 Dvac
OrderEvar_D_2021_Glmm <- lmer((Evar) ~ Grazing_Treatment + (1 | Block) , data = subset(CommunityMetrics,Year==2021 & Collection_Method=="dvac"))
anova(OrderEvar_D_2021_Glmm) #0.03418
# post hoc test for lmer test
summary(glht(OrderEvar_D_2021_Glmm, linfct = mcp(Grazing_Treatment = "Tukey")), test = adjusted(type = "BH")) #NG-LG (p=00.1897), #LG-HG (0.1897), NG-HG (0.0204)

# 2022 Dvac
OrderEvar_D_2022_Glmm <- lmer((1/Evar) ~ Grazing_Treatment + (1 | Block) , data = subset(CommunityMetrics,Year==2022 & Collection_Method=="dvac"))
anova(OrderEvar_D_2022_Glmm) #not significant

#### NMDS: By Order: Weight####

#### Bray Curtis: By Order: Weight ####
#Create wide relative cover dataframe
Abundance_Wide_Weight<-Weight_Data_Summed %>%
  filter(!Correct_Order %in% c("Unknown","unknown", "Unknown_1","Body_Parts","Body Parts")) %>% 
  filter(Plot!="NA") %>% 
  spread(key=Correct_Order,value=Dry_Weight_g, fill=0) %>% 
  filter(Collection_Method=="dvac")

#### Make new data frame called BC_Data and run an NMDS 

#dvac
BC_Data_Weight <- metaMDS(Abundance_Wide_Weight[,6:15])
#look at species signiciance driving NMDS 
intrinsics <- envfit(BC_Data_Weight, Abundance_Wide_Weight, permutations = 999)
head(intrinsics)
#Make a data frame called sites with 1 column and same number of rows that is in Wide Order weight
sites <- 1:nrow(Abundance_Wide_Weight)
#Make a new data table called BC_Meta_Data and use data from Wide_Relative_Cover columns 1-3
BC_Meta_Data_Weight <- Abundance_Wide_Weight[,1:5] #%>% 
  #mutate(Trt_Year=paste(Grazing_Treatment,Year,sep="."))
#make a plot using the dataframe BC_Data and the column "points".  Make Grazing Treatment a factor - make the different grazing treatments different colors
plot(BC_Data_Weight$points,col=as.factor(BC_Meta_Data_Weight$Year))

#Use the vegan ellipse function to make ellipses           
veganCovEllipse<-function (cov, center = c(0, 0), scale = 1, npoints = 100)
{
  theta <- (0:npoints) * 2 * pi/npoints
  Circle <- cbind(cos(theta), sin(theta))
  t(center + scale * t(Circle %*% chol(cov)))
}
#make elipses using the BC_Data.  Group by grazing treatment and use standard deviation to draw eclipses
ordiellipse(BC_Data_Weight,groups = as.factor(BC_Meta_Data_Weight$Year),kind = "sd",display = "sites", label = T)

#Make a data frame called BC_NMDS and at a column using the first set of "points" in BC_Data and a column using the second set of points.  Group them by watershed
BC_NMDS_Weight = data.frame(MDS1 = BC_Data_Weight$points[,1], MDS2 = BC_Data_Weight$points[,2],group=BC_Meta_Data_Weight$Year)
#Make data table called BC_NMDS_Graph and bind the BC_Meta_Data, and BC_NMDS data together
BC_NMDS_Graph_Weight <- cbind(BC_Meta_Data_Weight,BC_NMDS_Weight)
#Make a data table called BC_Ord_Ellipses using data from BC_Data and watershed information from BC_Meta_Data.  Display sites and find the standard error at a confidence iinterval of 0.95.  Place lables on the graph
BC_Ord_Ellipses_Weight<-ordiellipse(BC_Data_Weight, BC_Meta_Data_Weight$Year, display = "sites",
                                    kind = "sd", conf = 0.95, label = T)
#Make a new empty data frame called BC_Ellipses                
BC_Ellipses_Weight <- data.frame()
#Generate ellipses points - switched levels for unique - not sure if it's stil correct but it looks right
for(g in unique(BC_NMDS_Weight$group)){
  BC_Ellipses_Weight <- rbind(BC_Ellipses_Weight, cbind(as.data.frame(with(BC_NMDS_Weight[BC_NMDS_Weight$group==g,],                                                  veganCovEllipse(BC_Ord_Ellipses_Weight[[g]]$cov,BC_Ord_Ellipses_Weight[[g]]$center,BC_Ord_Ellipses_Weight[[g]]$scale)))
                                                        ,group=g))
}

#### NMDS Figures: By Order: Weight ####

#Plot the data from BC_NMDS_Graph, where x=MDS1 and y=MDS2, make an ellipse based on "group"
ggplot(data = BC_NMDS_Graph_Weight, aes(MDS1,MDS2, shape = group,color=group,linetype=group))+
  #make a point graph where the points are size 5.  Color them based on exlosure
  geom_point(size=8, stroke = 2) +
  #Use the data from BC_Ellipses to make ellipses that are size 1 with a solid line
  geom_path(data = BC_Ellipses_Weight, aes(x=NMDS1, y=NMDS2), size=4)+
  #make shape, color, and linetype in one combined legend instead of three legends
  labs(color  = "", linetype = "", shape = "")+
  scale_color_manual(values=c("skyblue3","springgreen3","brown"),labels = c("2020","2021", "2022"),name="")+
  scale_linetype_manual(values=c(1,2,3),labels = c("2020","2021", "2022"),name="")+
  # make legend 2 columns
  guides(shape=guide_legend(ncol=2),colour=guide_legend(ncol=2),linetype=guide_legend(ncol=2))+
  #make the text size of the legend titles 28
  theme(legend.key = element_rect(size=3), legend.key.size = unit(1,"centimeters"))+
  #Label the x-axis "NMDS1" and the y-axis "NMDS2"
  xlab("NMDS1")+
  ylab("NMDS2")+
  theme(text = element_text(size = 55),legend.text=element_text(size=40))+
  annotate(geom="text", x=-2, y=0.8, label="Weight",size=20)
#export at 2000 x 1800

####NMDS: Weight: 2021 by Grazing ####

BC_Meta_Data_Weight_Grazing <- Abundance_Wide_Weight[,1:5] %>% 
mutate(Trt_Year=paste(Grazing_Treatment,Year,sep="."))
#make a plot using the dataframe BC_Data and the column "points".  Make Grazing Treatment a factor - make the different grazing treatments different colors
plot(BC_Data_Weight$points,col=as.factor(BC_Meta_Data_Weight_Grazing$Trt_Year))

#make elipses using the BC_Data.  Group by grazing treatment and use standard deviation to draw eclipses
ordiellipse(BC_Data_Weight,groups = as.factor(BC_Meta_Data_Weight_Grazing$Trt_Year),kind = "sd",display = "sites", label = T)

#Make a data frame called BC_NMDS and at a column using the first set of "points" in BC_Data and a column using the second set of points.  Group them by watershed
BC_NMDS_Weight_Grazing = data.frame(MDS1 = BC_Data_Weight$points[,1], MDS2 = BC_Data_Weight$points[,2],group=BC_Meta_Data_Weight_Grazing$Trt_Year)
#Make data table called BC_NMDS_Graph and bind the BC_Meta_Data, and BC_NMDS data together
BC_NMDS_Graph_Weight_Grazing <- cbind(BC_Meta_Data_Weight_Grazing,BC_NMDS_Weight_Grazing)
#Make a data table called BC_Ord_Ellipses using data from BC_Data and watershed information from BC_Meta_Data.  Display sites and find the standard error at a confidence iinterval of 0.95.  Place lables on the graph
BC_Ord_Ellipses_Weight_Grazing<-ordiellipse(BC_Data_Weight, BC_Meta_Data_Weight_Grazing$Trt_Year, display = "sites",
                                    kind = "sd", conf = 0.95, label = T)
#Make a new empty data frame called BC_Ellipses                
BC_Ellipses_Weight_Grazing <- data.frame()
#Generate ellipses points - switched levels for unique - not sure if it's stil correct but it looks right
for(g in unique(BC_NMDS_Weight_Grazing$group)){
  BC_Ellipses_Weight_Grazing <- rbind(BC_Ellipses_Weight_Grazing, cbind(as.data.frame(with(BC_NMDS_Weight_Grazing[BC_NMDS_Weight_Grazing$group==g,],                                                  veganCovEllipse(BC_Ord_Ellipses_Weight_Grazing[[g]]$cov,BC_Ord_Ellipses_Weight_Grazing[[g]]$center,BC_Ord_Ellipses_Weight_Grazing[[g]]$scale)))
                                                        ,group=g))
}

#### NMDS Figures: By Order: Weight ####

#Plot the data from BC_NMDS_Graph, where x=MDS1 and y=MDS2, make an ellipse based on "group"
ggplot(data = subset(BC_NMDS_Graph_Weight_Grazing,group==c("HG.2021","LG.2021","NG.2021")), aes(MDS1,MDS2, shape = group,color=group,linetype=group))+
  #make a point graph where the points are size 5.  Color them based on exlosure
  geom_point(size=8, stroke = 2) +
  #Use the data from BC_Ellipses to make ellipses that are size 1 with a solid line
  geom_path(data = subset(BC_Ellipses_Weight_Grazing,group==c("HG.2021","LG.2021","NG.2021")), aes(x=NMDS1, y=NMDS2), size=4)+
  #make shape, color, and linetype in one combined legend instead of three legends
  labs(color  = "", linetype = "", shape = "")+
  scale_color_manual(values=c("thistle2","thistle3","thistle4"), labels=c("Cattle Removal","Destock","High Impact Grazing"), breaks=c("NG.2021","LG.2021","HG.2021"))+
  scale_shape_manual(values=c(15,16,17), labels=c("Cattle Removal","Destock","High Impact Grazing"), breaks=c("NG.2021","LG.2021","HG.2021"))+
  scale_linetype_manual(values=c(1,2,3),labels=c("Cattle Removal","Destock","High Impact Grazing"), breaks=c("NG.2021","LG.2021","HG.2021"))+
  # make legend 2 columns
  guides(shape=guide_legend(ncol=2),colour=guide_legend(ncol=2),linetype=guide_legend(ncol=2))+
  #make the text size of the legend titles 28
  theme(legend.key = element_rect(size=3), legend.key.size = unit(1,"centimeters"))+
  #Label the x-axis "NMDS1" and the y-axis "NMDS2"
  xlab("NMDS1")+
  ylab("NMDS2")+
  theme(text = element_text(size = 55),legend.text=element_text(size=40),legend.position="none")+
  annotate(geom="text", x=-2, y=0.8, label="2021 Weight",size=20)
#export at 2000 x 1800

#### PERMANOVA: By Order: Weight ####

##PerMANOVA

#Make a new dataframe with the data from Wide_Relative_Cover all columns after 5
Species_Matrix_Weight <- Abundance_Wide_Weight[,6:ncol(Abundance_Wide_Weight)]
#Make a new dataframe with data from Wide_Relative_Cover columns 1-3
Environment_Matrix_Weight <- Abundance_Wide_Weight[,1:5] %>% 
  mutate(Gr_Yr=paste(Grazing_Treatment,Year,sep="."))

Environment_Matrix_Weight$Grazing_Treatment_Fact=as.factor(Environment_Matrix_Weight$Grazing_Treatment)
Environment_Matrix_Weight$Block_Fact=as.numeric(Environment_Matrix_Weight$Block)
Environment_Matrix_Weight$Plot_Fact=as.factor(Environment_Matrix_Weight$Plot)
Environment_Matrix_Weight$Year_Fact=as.factor(Environment_Matrix_Weight$Year)

#run a perMANOVA comparing across watershed and exclosure, how does the species composition differ.  Permutation = 999 - run this 999 times and tell us what the preportion of times it was dissimilar
#Adding in the 'strata' function does not affect results - i can't figure out if I am doing in incorrectly or if they do not affect the results (seems unlikely though becuase everything is exactly the same)
PerMANOVA2_Weight <- adonis2(formula = Species_Matrix_Weight~Grazing_Treatment_Fact*Year_Fact + (1 | Block_Fact) , data=Environment_Matrix_Weight,permutations = 999, method = "bray")
#give a print out of the PermMANOVA
print(PerMANOVA2_Weight)  #Grazing (0.01), Year (0.001), GxYear (0.003)
#pairwise test
Posthoc_Weight_Year<-pairwise.adonis(Species_Matrix_Weight,factors=Environment_Matrix_Weight$Year, p.adjust.m = "BH")
Posthoc_Weight_Year   #2020-2021 (0.001), 2021-2022 (0.001), 2020-2022 (0.001)

Posthoc_Weight_Grazing_Year<-pairwise.adonis(Species_Matrix_Weight,factors=Environment_Matrix_Weight$Gr_Yr, p.adjust.m = "BH")
Posthoc_Weight_Grazing_Year #Significant: HG-NG (2021)


#### PERMDISP: By Order ####
Abundance_Wide_Weight_dispr<-Abundance_Wide_Weight %>% 
  mutate(Gr_Yr=paste(Grazing_Treatment,Year,sep="."))

#Dvac
#Make a new dataframe and calculate the dissimilarity of the Species_Matrix dataframe
BC_Distance_Matrix_Weight <- vegdist(Species_Matrix_Weight)
#Run a dissimilarity matrix (PermDisp) comparing grazing treatment
Dispersion_Results_Grazing_Weight <- betadisper(BC_Distance_Matrix_Weight,Abundance_Wide_Weight_dispr$Gr_Yr)
permutest(Dispersion_Results_Grazing_Weight,pairwise = T, permutations = 999) 


#### NMDS: By Order: Count####

#### Bray Curtis: By Order: Count ####
#Create wide relative cover dataframe
#Change row 54 and 55 where we don't cant equate sample number to weight to be unique sample number so it can be used here
#2020 block 1 NG, plot 3
Abundance[54, "Sample_Number"] <- 10
Abundance[55, "Sample_Number"] <- 11
#2021 LG, plot 8
Abundance[944, "Sample_Number"] <- 2
Abundance[945, "Sample_Number"] <- 3

Abundance_Wide_Count<-Abundance %>%
  dplyr::select(-c(Correct_Family,Correct_Genus, Correct_Species,Notes,Coll_Year_Bl_Trt,Coll_Year_Bl_Trt_Pl,Sample_Number)) %>% 
  unique() %>% 
  spread(key=Correct_Order,value=Abundance, fill=0) %>% 
  select(-c(Unknown,"<NA>")) %>% 
  filter(Collection_Method=="dvac")

Abundance_Wide_Count$Year=as.character(Abundance_Wide_Count$Year)
Abundance_Wide_Count$Plot=as.character(Abundance_Wide_Count$Plot)

#### Make new data frame called BC_Data and run an NMDS 

#dvac
BC_Data_Count <- metaMDS(Abundance_Wide_Count[,6:15])
#look at species signiciance driving NMDS 
intrinsics <- envfit(BC_Data_Count, Abundance_Wide_Count, permutations = 999)
head(intrinsics)
#Make a data frame called sites with 1 column and same number of rows that is in Wide Order Count
sites <- 1:nrow(Abundance_Wide_Count)
#Make a new data table called BC_Meta_Data and use data from Wide_Relative_Cover columns 1-3
BC_Meta_Data_Count <- Abundance_Wide_Count[,1:5] 
#make a plot using the dataframe BC_Data and the column "points".  Make Grazing Treatment a factor - make the different grazing treatments different colors
plot(BC_Data_Count$points,col=as.factor(BC_Meta_Data_Count$Year))

#make elipses using the BC_Data.  Group by grazing treatment and use standard deviation to draw eclipses
ordiellipse(BC_Data_Count,groups = as.factor(BC_Meta_Data_Count$Year),kind = "sd",display = "sites", label = T)

#Make a data frame called BC_NMDS and at a column using the first set of "points" in BC_Data and a column using the second set of points.  Group them by watershed
BC_NMDS_Count = data.frame(MDS1 = BC_Data_Count$points[,1], MDS2 = BC_Data_Count$points[,2],group=BC_Meta_Data_Count$Year)
#Make data table called BC_NMDS_Graph and bind the BC_Meta_Data, and BC_NMDS data together
BC_NMDS_Graph_Count <- cbind(BC_Meta_Data_Count,BC_NMDS_Count)
#Make a data table called BC_Ord_Ellipses using data from BC_Data and watershed information from BC_Meta_Data.  Display sites and find the standard error at a confidence iinterval of 0.95.  Place lables on the graph
BC_Ord_Ellipses_Count<-ordiellipse(BC_Data_Count, BC_Meta_Data_Count$Year, display = "sites",
                                   kind = "sd", conf = 0.95, label = T)
#Make a new empty data frame called BC_Ellipses                
BC_Ellipses_Count <- data.frame()
#Generate ellipses points - switched levels for unique - not sure if it's stil correct but it looks right
for(g in unique(BC_NMDS_Count$group)){
  BC_Ellipses_Count <- rbind(BC_Ellipses_Count, cbind(as.data.frame(with(BC_NMDS_Count[BC_NMDS_Count$group==g,],                                                  veganCovEllipse(BC_Ord_Ellipses_Count[[g]]$cov,BC_Ord_Ellipses_Count[[g]]$center,BC_Ord_Ellipses_Count[[g]]$scale)))
                                                                      ,group=g))
}

#### NMDS Figures: By Order: Count ####

#Plot the data from BC_NMDS_Graph, where x=MDS1 and y=MDS2, make an ellipse based on "group"
ggplot(data = BC_NMDS_Graph_Count, aes(MDS1,MDS2, shape = group,color=group,linetype=group))+
  #make a point graph where the points are size 5.  Color them based on exlosure
  geom_point(size=8, stroke = 2) +
  #Use the data from BC_Ellipses to make ellipses that are size 1 with a solid line
  geom_path(data = BC_Ellipses_Count, aes(x=NMDS1, y=NMDS2), size=4)+
  #make shape, color, and linetype in one combined legend instead of three legends
  labs(color  = "", linetype = "", shape = "")+
  scale_color_manual(values=c("skyblue3","springgreen3","brown"),labels = c("2020","2021", "2022"),name="")+
  scale_linetype_manual(values=c(1,2,3),labels = c("2020","2021", "2022"),name="")+
  # make legend 2 columns
  guides(shape=guide_legend(ncol=2),colour=guide_legend(ncol=2),linetype=guide_legend(ncol=2))+
  #make the text size of the legend titles 28
  theme(legend.key = element_rect(size=3), legend.key.size = unit(1,"centimeters"))+
  #Label the x-axis "NMDS1" and the y-axis "NMDS2"
  xlab("NMDS1")+
  ylab("NMDS2")+
  theme(text = element_text(size = 55),legend.text=element_text(size=40),legend.position="none")+
  annotate(geom="text", x=-2, y=0.8, label="Count",size=20)
#export at 2000 x 1800

####NMDS: Count: 2021 by Grazing ####

BC_Meta_Data_Count_Grazing <- Abundance_Wide_Count[,1:5] %>% 
  mutate(Trt_Year=paste(Grazing_Treatment,Year,sep="."))
#make a plot using the dataframe BC_Data and the column "points".  Make Grazing Treatment a factor - make the different grazing treatments different colors
plot(BC_Data_Count$points,col=as.factor(BC_Meta_Data_Count_Grazing$Trt_Year))

#make elipses using the BC_Data.  Group by grazing treatment and use standard deviation to draw eclipses
ordiellipse(BC_Data_Count,groups = as.factor(BC_Meta_Data_Count_Grazing$Trt_Year),kind = "sd",display = "sites", label = T)

#Make a data frame called BC_NMDS and at a column using the first set of "points" in BC_Data and a column using the second set of points.  Group them by watershed
BC_NMDS_Count_Grazing = data.frame(MDS1 = BC_Data_Count$points[,1], MDS2 = BC_Data_Count$points[,2],group=BC_Meta_Data_Count_Grazing$Trt_Year)
#Make data table called BC_NMDS_Graph and bind the BC_Meta_Data, and BC_NMDS data together
BC_NMDS_Graph_Count_Grazing <- cbind(BC_Meta_Data_Count_Grazing,BC_NMDS_Count_Grazing)
#Make a data table called BC_Ord_Ellipses using data from BC_Data and watershed information from BC_Meta_Data.  Display sites and find the standard error at a confidence iinterval of 0.95.  Place lables on the graph
BC_Ord_Ellipses_Count_Grazing<-ordiellipse(BC_Data_Count, BC_Meta_Data_Count_Grazing$Trt_Year, display = "sites",
                                           kind = "sd", conf = 0.95, label = T)
#Make a new empty data frame called BC_Ellipses                
BC_Ellipses_Count_Grazing <- data.frame()
#Generate ellipses points - switched levels for unique - not sure if it's stil correct but it looks right
for(g in unique(BC_NMDS_Count_Grazing$group)){
  BC_Ellipses_Count_Grazing <- rbind(BC_Ellipses_Count_Grazing, cbind(as.data.frame(with(BC_NMDS_Count_Grazing[BC_NMDS_Count_Grazing$group==g,],                                                  veganCovEllipse(BC_Ord_Ellipses_Count_Grazing[[g]]$cov,BC_Ord_Ellipses_Count_Grazing[[g]]$center,BC_Ord_Ellipses_Count_Grazing[[g]]$scale)))
                                                                      ,group=g))
}

#### NMDS Figures: By Order: Count ####

#2021
#Plot the data from BC_NMDS_Graph, where x=MDS1 and y=MDS2, make an ellipse based on "group"
ggplot(data = subset(BC_NMDS_Graph_Count_Grazing,group==c("HG.2021","LG.2021","NG.2021")), aes(MDS1,MDS2, shape = group,color=group,linetype=group))+
  #make a point graph where the points are size 5.  Color them based on exlosure
  geom_point(size=8, stroke = 2) +
  #Use the data from BC_Ellipses to make ellipses that are size 1 with a solid line
  geom_path(data = subset(BC_Ellipses_Count_Grazing,group==c("HG.2021","LG.2021","NG.2021")), aes(x=NMDS1, y=NMDS2), size=4)+
  #make shape, color, and linetype in one combined legend instead of three legends
  labs(color  = "", linetype = "", shape = "")+
  scale_color_manual(values=c("thistle2","thistle3","thistle4"), labels=c("Cattle Removal","Destock","High Impact Grazing"), breaks=c("NG.2021","LG.2021","HG.2021"))+
  scale_shape_manual(values=c(15,16,17), labels=c("Cattle Removal","Destock","High Impact Grazing"), breaks=c("NG.2021","LG.2021","HG.2021"))+
  scale_linetype_manual(values=c(1,2,3),labels=c("Cattle Removal","Destock","High Impact Grazing"), breaks=c("NG.2021","LG.2021","HG.2021"))+
  # make legend 2 columns
  guides(shape=guide_legend(ncol=2),colour=guide_legend(ncol=2),linetype=guide_legend(ncol=2))+
  #make the text size of the legend titles 28
  theme(legend.key = element_rect(size=3), legend.key.size = unit(1,"centimeters"))+
  #Label the x-axis "NMDS1" and the y-axis "NMDS2"
  xlab("NMDS1")+
  ylab("NMDS2")+
  theme(text = element_text(size = 55),legend.text=element_text(size=40),legend.position="none")+
  annotate(geom="text", x=-2, y=0.8, label="2021 Count",size=20)
#export at 2000 x 1800

#2022
#Plot the data from BC_NMDS_Graph, where x=MDS1 and y=MDS2, make an ellipse based on "group"
ggplot(data = subset(BC_NMDS_Graph_Count_Grazing,group==c("HG.2022","LG.2022","NG.2022")), aes(MDS1,MDS2, shape = group,color=group,linetype=group))+
  #make a point graph where the points are size 5.  Color them based on exlosure
  geom_point(size=8, stroke = 2) +
  #Use the data from BC_Ellipses to make ellipses that are size 1 with a solid line
  geom_path(data = subset(BC_Ellipses_Count_Grazing,group==c("HG.2022","LG.2022","NG.2022")), aes(x=NMDS1, y=NMDS2), size=4)+
  #make shape, color, and linetype in one combined legend instead of three legends
  labs(color  = "", linetype = "", shape = "")+
  scale_color_manual(values=c("thistle2","thistle3","thistle4"), labels=c("Cattle Removal","Destock","High Impact Grazing"), breaks=c("HG.2022","LG.2022","NG.2022"))+
  scale_shape_manual(values=c(15,16,17), labels=c("Cattle Removal","Destock","High Impact Grazing"), breaks=c("HG.2022","LG.2022","NG.2022"))+
  scale_linetype_manual(values=c(1,2,3),labels=c("Cattle Removal","Destock","High Impact Grazing"), breaks=c("HG.2022","LG.2022","NG.2022"))+
  # make legend 2 columns
  guides(shape=guide_legend(ncol=2),colour=guide_legend(ncol=2),linetype=guide_legend(ncol=2))+
  #make the text size of the legend titles 28
  theme(legend.key = element_rect(size=3), legend.key.size = unit(1,"centimeters"))+
  #Label the x-axis "NMDS1" and the y-axis "NMDS2"
  xlab("NMDS1")+
  ylab("NMDS2")+
  theme(text = element_text(size = 55),legend.text=element_text(size=40),legend.position="none")+
  annotate(geom="text", x=-1, y=0.8, label="2022 Count",size=20)
#export at 2000 x 1800

#### PERMANOVA: By Order: Count ####

##PerMANOVA

#Make a new dataframe with the data from Wide_Relative_Cover all columns after 5
Species_Matrix_Count <- Abundance_Wide_Count[,6:ncol(Abundance_Wide_Count)]
#Make a new dataframe with data from Wide_Relative_Cover columns 1-3
Environment_Matrix_Count <- Abundance_Wide_Count[,1:5] %>% 
  mutate(Gr_Yr=paste(Grazing_Treatment,Year,sep="."))

Environment_Matrix_Count$Grazing_Treatment_Fact=as.factor(Environment_Matrix_Count$Grazing_Treatment)
Environment_Matrix_Count$Block_Fact=as.numeric(Environment_Matrix_Count$Block)
Environment_Matrix_Count$Plot_Fact=as.factor(Environment_Matrix_Count$Plot)
Environment_Matrix_Count$Year_Fact=as.factor(Environment_Matrix_Count$Year)

#run a perMANOVA comparing across watershed and exclosure, how does the species composition differ.  Permutation = 999 - run this 999 times and tell us what the preportion of times it was dissimilar
#Adding in the 'strata' function does not affect results - i can't figure out if I am doing in incorrectly or if they do not affect the results (seems unlikely though becuase everything is exactly the same)
PerMANOVA2_Count <- adonis2(formula = Species_Matrix_Count~Grazing_Treatment_Fact*Year_Fact + (1 | Block_Fact) , data=Environment_Matrix_Count,permutations = 999, method = "bray")
#give a print out of the PermMANOVA
print(PerMANOVA2_Count)  #Grazing (0.01), Year (0.001), GxYear (0.003)
#pairwise test
Posthoc_Count_Year<-pairwise.adonis(Species_Matrix_Count,factors=Environment_Matrix_Count$Year, p.adjust.m = "BH")
Posthoc_Count_Year   #2020-2021 (0.001), 2021-2022 (0.001), 2020-2022 (0.001)
#pairwise test
Posthoc_Count_Grazing<-pairwise.adonis(Species_Matrix_Count,factors=Environment_Matrix_Count$Grazing_Treatment, p.adjust.m = "BH")
Posthoc_Count_Grazing  #ns

Posthoc_Count_Grazing_Year<-pairwise.adonis(Species_Matrix_Count,factors=Environment_Matrix_Count$Gr_Yr, p.adjust.m = "BH")
Posthoc_Count_Grazing_Year #Significant: HG-NG (2021)


#### PERMDISP: By Order ####
Abundance_Wide_Count_dispr<-Abundance_Wide_Count %>% 
  mutate(Gr_Yr=paste(Grazing_Treatment,Year,sep="."))

#Dvac
#Make a new dataframe and calculate the dissimilarity of the Species_Matrix dataframe
BC_Distance_Matrix_Count <- vegdist(Species_Matrix_Count)
#Run a dissimilarity matrix (PermDisp) comparing grazing treatment
Dispersion_Results_Grazing_Count <- betadisper(BC_Distance_Matrix_Count,Abundance_Wide_Count_dispr$Gr_Yr)
permutest(Dispersion_Results_Grazing_Count,pairwise = T, permutations = 999) 



#### NMDS: Orthoptera Genus ####

Abundance_OrthopteraGenus<-ID_Data_Official %>% 
  filter(Correct_Family=="Acrididae") %>% 
  group_by(Collection_Method,Year,Block,Grazing_Treatment,Plot,Correct_Genus) %>% 
  mutate(Abundance=length(Sample_Number)) %>% 
  ungroup() 

#### Bray Curtis: Orthoptera Genus  ####

#Create wide relative cover dataframe

Abundance_Wide_D__OrthopteraGenus<-Abundance_OrthopteraGenus %>%
  dplyr::select(-c(Correct_Order,Correct_Family, Correct_Species,Notes,Coll_Year_Bl_Trt,Coll_Year_Bl_Trt_Pl,Sample_Number)) %>% 
  unique() %>% 
  spread(key=Correct_Genus,value=Abundance, fill=0) %>% 
  select(-c("<NA>"))%>% 
  filter(Collection_Method=="dvac") %>% 
  mutate(Treatment=paste(Year,Plot,sep=".")) %>% 
  #remove plot 29 because it had no grasshoppers
  filter(Treatment!=2022.29) %>% 
  select(-Treatment)

#### Make new data frame called BC_Data and run an NMDS 

#dvac
BC_Data_D__OrthopteraGenus <- metaMDS(Abundance_Wide_D__OrthopteraGenus[,6:13])
#look at species signiciance driving NMDS 
intrinsics <- envfit(BC_Data_D__OrthopteraGenus, Abundance_Wide_D__OrthopteraGenus, permutations = 999)
head(intrinsics)
#Make a data frame called sites with 1 column and same number of rows that is in Wide Order weight
sites <- 1:nrow(Abundance_Wide_D__OrthopteraGenus)
#Make a new data table called BC_Meta_Data and use data from Wide_Relative_Cover columns 1-3
BC_Meta_Data_D__OrthopteraGenus <- Abundance_Wide_D__OrthopteraGenus[,1:5] %>% 
  mutate(Trt_Year=paste(Grazing_Treatment,Year,sep="."))
#make a plot using the dataframe BC_Data and the column "points".  Make Grazing Treatment a factor - make the different grazing treatments different colors
plot(BC_Data_D__OrthopteraGenus$points,col=as.factor(BC_Meta_Data_D__OrthopteraGenus$Trt_Year))
#make elipses using the BC_Data.  Group by grazing treatment and use standard deviation to draw eclipses
ordiellipse(BC_Data_D__OrthopteraGenus,groups = as.factor(BC_Meta_Data_D__OrthopteraGenus$Trt_Year),kind = "sd",display = "sites", label = T)

#Make a data frame called BC_NMDS and at a column using the first set of "points" in BC_Data and a column using the second set of points.  Group them by watershed
BC_NMDS_D__OrthopteraGenus = data.frame(MDS1 = BC_Data_D__OrthopteraGenus$points[,1], MDS2 = BC_Data_D__OrthopteraGenus$points[,2],group=BC_Meta_Data_D__OrthopteraGenus$Trt_Year)
#Make data table called BC_NMDS_Graph and bind the BC_Meta_Data, and BC_NMDS data together
BC_NMDS_Graph_D__OrthopteraGenus <- cbind(BC_Meta_Data_D__OrthopteraGenus,BC_NMDS_D__OrthopteraGenus)
#Make a data table called BC_Ord_Ellipses using data from BC_Data and watershed information from BC_Meta_Data.  Display sites and find the standard error at a confidence iinterval of 0.95.  Place lables on the graph
BC_Ord_Ellipses_D__OrthopteraGenus<-ordiellipse(BC_Data_D__OrthopteraGenus, BC_Meta_Data_D__OrthopteraGenus$Trt_Year, display = "sites",
                               kind = "se", conf = 0.95, label = T)
#Make a new empty data frame called BC_Ellipses                
BC_Ellipses_D__OrthopteraGenus <- data.frame()
#Generate ellipses points - switched levels for unique - not sure if it's stil correct but it looks right
for(g in unique(BC_NMDS_D__OrthopteraGenus$group)){
  BC_Ellipses_D__OrthopteraGenus <- rbind(BC_Ellipses_D__OrthopteraGenus, cbind(as.data.frame(with(BC_NMDS_D__OrthopteraGenus[BC_NMDS_D__OrthopteraGenus$group==g,],                                                  veganCovEllipse(BC_Ord_Ellipses_D__OrthopteraGenus[[g]]$cov,BC_Ord_Ellipses_D__OrthopteraGenus[[g]]$center,BC_Ord_Ellipses_D__OrthopteraGenus[[g]]$scale)))
                                              ,group=g))
}

#### NMDS Figures: Orthoptera Genus  ####
#Plot the data from BC_NMDS_Graph, where x=MDS1 and y=MDS2, make an ellipse based on "group"
NMDS_Dvac_OrthopteraGenus<-ggplot(data = BC_NMDS_Graph_D__OrthopteraGenus, aes(MDS1,MDS2, shape = group,color=group,linetype=group))+
  #make a point graph where the points are size 5.  Color them based on exlosure
  geom_point(size=8, stroke = 2) +
  #Use the data from BC_Ellipses to make ellipses that are size 1 with a solid line
  geom_path(data = BC_Ellipses_D__OrthopteraGenus, aes(x=NMDS1, y=NMDS2), size=4)+
  #make shape, color, and linetype in one combined legend instead of three legends
  labs(color  = "", linetype = "", shape = "")+
  # make legend 2 columns
  guides(shape=guide_legend(ncol=2),colour=guide_legend(ncol=2),linetype=guide_legend(ncol=2))+
  #change order of legend
  #Use different shapes 
  scale_shape_manual(values=c(15,16,17,22,21,24,21,15,17),labels = c("Heavy 2020","Destock 2020", "No Grazing 2020","Heavy 2021","Destock 2021", "No Grazing 2021","Heavy 2022","Destock 2022", "No Grazing 2022"), breaks = c("HG.2020","LG.2020","NG.2020","HG.2021","LG.2021","NG.2021","HG.2022","LG.2022","NG.2022"),name="")+
  scale_color_manual(values=c("skyblue3","springgreen3","plum3","royalblue4","springgreen4","plum4","red","yellow","blue"),labels = c("Heavy 2020","Destock 2020", "No Grazing 2020","Heavy 2021","Destock 2021", "No Grazing 2021","Heavy 2022","Destock 2022", "No Grazing 2022"), breaks = c("HG.2020","LG.2020","NG.2020","HG.2021","LG.2021","NG.2021","HG.2022","LG.2022","NG.2022"),name="")+
  scale_linetype_manual(values=c("solid","twodash","longdash","solid","twodash","longdash","solid","solid","solid"),labels = c("Heavy 2020","Destock 2020", "No Grazing 2020","Heavy 2021","Destock 2021", "No Grazing 2021","Heavy 2022","Destock 2022", "No Grazing 2022"), breaks = c("HG.2020","LG.2020","NG.2020","HG.2021","LG.2021","NG.2021","HG.2022","LG.2022","NG.2022"),name="")+
  #make the text size of the legend titles 28
  theme(legend.key = element_rect(size=3), legend.key.size = unit(1,"centimeters"),legend.position="NONE")+
  #Label the x-axis "NMDS1" and the y-axis "NMDS2"
  xlab("NMDS1")+
  ylab("NMDS2")+
  theme(text = element_text(size = 55),legend.text=element_text(size=40))+
  annotate(geom="text", x=-2, y=0.8, label="Dvac",size=20)

#### Create NMDS: Orthoptera Genus  ####
NMDS_Sweep_OrthopteraGenus+
  NMDS_Dvac_OrthopteraGenus+
  plot_layout(ncol = 1,nrow = 2)

#Save at 4000x3000
#### PERMANOVA: Orthoptera Genus  ####

##PerMANOVA
#Make a new dataframe with the data from Wide_Relative_Cover all columns after 5
Species_Matrix_D__OrthopteraGenus <- Abundance_Wide_D__OrthopteraGenus[,6:ncol(Abundance_Wide_D__OrthopteraGenus)]
#Make a new dataframe with data from Wide_Relative_Cover columns 1-3
Environment_Matrix_D__OrthopteraGenus <- Abundance_Wide_D__OrthopteraGenus[,1:5] %>% 
  mutate(Gr_Yr=paste(Grazing_Treatment,Year,sep="."))

Environment_Matrix_D__OrthopteraGenus$Grazing_Treatment_Fact=as.factor(Environment_Matrix_D__OrthopteraGenus$Grazing_Treatment)
Environment_Matrix_D__OrthopteraGenus$Block_Fact=as.numeric(Environment_Matrix_D__OrthopteraGenus$Block)
Environment_Matrix_D__OrthopteraGenus$Plot_Fact=as.factor(Environment_Matrix_D__OrthopteraGenus$Plot)
Environment_Matrix_D__OrthopteraGenus$Year_Fact=as.factor(Environment_Matrix_D__OrthopteraGenus$Year)

#run a perMANOVA comparing across watershed and exclosure, how does the species composition differ.  Permutation = 999 - run this 999 times and tell us what the preportion of times it was dissimilar
#Adding in the 'strata' function does not affect results - i can't figure out if I am doing in incorrectly or if they do not affect the results (seems unlikely though becuase everything is exactly the same)
PerMANOVA2_D__OrthopteraGenus <- adonis2(formula = Species_Matrix_D__OrthopteraGenus~Grazing_Treatment_Fact*Year_Fact + (1 | Block_Fact) , data=Environment_Matrix_D__OrthopteraGenus,permutations = 999, method = "bray")
#give a print out of the PermMANOVA
print(PerMANOVA2_D__OrthopteraGenus)  #Grazing (0.0187), Year (0.001), GxYear (NS)
#pairwise test
Posthoc_D__OrthopteraGenus_Year<-pairwise.adonis(Species_Matrix_D__OrthopteraGenus,factors=Environment_Matrix_D__OrthopteraGenus$Year, p.adjust.m = "BH")
Posthoc_D__OrthopteraGenus_Year   #2020-2021 (0.001), 2021-2022 (0.001), 2020-2022 (0.001)
#pairwise test
Posthoc_D__OrthopteraGenus_Grazing_Year<-pairwise.adonis(Species_Matrix_D__OrthopteraGenus,factors=Environment_Matrix_D__OrthopteraGenus$Gr_Yr, p.adjust.m = "BH")
Posthoc_D__OrthopteraGenus_Grazing_Year #Significant: HG-NG (2021)

#### PERMDISP: Orthoptera Genus  ####

Abundance_Wide_D__OrthopteraGenus_dispr<-Abundance_Wide_D__OrthopteraGenus %>% 
  mutate(Gr_Yr=paste(Grazing_Treatment,Year,sep="."))

#Dvac
#Make a new dataframe and calculate the dissimilarity of the Species_Matrix dataframe
BC_Distance_Matrix_D__OrthopteraGenus <- vegdist(Species_Matrix_D__OrthopteraGenus)
#Run a dissimilarity matrix (PermDisp) comparing grazing treatment
Dispersion_Results_Grazing_D__OrthopteraGenus <- betadisper(BC_Distance_Matrix_D__OrthopteraGenus,Abundance_Wide_D__OrthopteraGenus_dispr$Gr_Yr)
permutest(Dispersion_Results_Grazing_D__OrthopteraGenus,pairwise = T, permutations = 999) 
#

