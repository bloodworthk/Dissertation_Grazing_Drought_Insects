#### Grazing x Arthropod Data - 2020 - 2022#
#### Code created by: Kathryn Bloodworth #

#### Set working directory and load libraries ####

library(scales)
library(vegan)
library(lmerTest)
library(grid)
library(multcomp)
library(tidyverse)
library(olsrr)
library(patchwork)
library(codyn)
library(pairwiseAdonis)
library(ggpattern)


# Set Working Directory - Mac
setwd("~/Library/CloudStorage/Box-Box/Projects/Dissertation/Data/Insect_Data")

# Set Working Directory - PC
setwd("C:/Users/kjbloodw/Box/Projects/Dissertation/Data/Insect_Data")


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

#Plant Species Comp Data 
PlantComp<-read.csv("Plant_Species_Comp_2022.csv",header=T) 


Functional_Groups<-read.csv("FunctionalGroups.csv")

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

Abundance_Order<-Abundance %>% 
  select(Collection_Method,Year,Block,Grazing_Treatment,Correct_Order,Plot,Abundance) %>% 
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



#### Formatting and Cleaning Plant Species Data ####
#Create Long dataframe from wide dataframe and fix species issues
LongCov_SpComp<-gather(PlantComp,key="species","cover",12:70) %>% 
  dplyr::select(block,plot,grazing_treatment,added_total_excel,species,cover) %>% 
  filter(!species %in% c("dung","moss","rock","lichen","mushroom","litter","bare_ground","final_total","final_total_excel","Longpod.mustard..Erysimum.asperum..","Lygo.deomia","basal.rosette" )) %>%
  na.omit(cover) %>% 
  filter(cover!=0)

#Calculate Relative Cover
Relative_Cover_PlantSp<-LongCov_SpComp%>%
  #In the data sheet Relative_Cover, add a new column called "Relative_Cover", in which you divide "cover" by "Total_Cover"
  mutate(Relative_Cover=(cover/added_total_excel)*100) %>% 
  dplyr::select(block,plot,grazing_treatment,species,Relative_Cover)

#make plot a factor not an integer
Relative_Cover_PlantSp$plot<-as.factor(Relative_Cover_PlantSp$plot)

Relative_Cover_PlantSp_Clean<-Relative_Cover_PlantSp%>% 
  #change species codes to full species names
  mutate(Genus_Species=ifelse(species=="ALDE","Alyssum.desertorum",ifelse(species=="ANOC","Androsace.occidentalis",ifelse(species=="ANPA","Antennaria.parvifolia", ifelse(species=="ARDR","Artemisia.dracunculus",ifelse(species=="ARFR","Artemisia.frigida",ifelse(species=="ARPU","Aristida.purpurea",ifelse(species=="ASGR","Astragalus.gracilis",ifelse(species=="ASPU","Astragalus.purshii",ifelse(species=="BODA","Bouteloua.dactyloides",ifelse(species=="BOGR" ,"Bouteloua.gracilis",ifelse(species=="BRAR","Bromus.arvensis",ifelse(species=="BRTE","Bromus.tectorum",ifelse(species=="CADU","Carex.duriuscula",ifelse(species=="CAFI","Carex.filifolia",ifelse(species=="CHPR","Chenopodium.pratericola",ifelse(species=="COCA","Conyza.canadensis",ifelse(species=="DEPI","Descurainia.pinnata",ifelse(species=="HECO","Hesperostipa.comata",ifelse(species=="VUOC","Vulpia.octoflora",ifelse(species=="KOMA","Koeleria.macrantha",ifelse(species=="LOAR","Logfia.arvensis",ifelse(species=="LYJU","Lygodesmia.juncea",ifelse(species=="DRRE","Draba.reptans",ifelse(species=="HEHI","Hedeoma.hispida",ifelse(species=="LEDE","Lepidium.densiflorum",ifelse(species=="LIIN","Lithospermum.incisum",ifelse(species=="LIPU","Liatris.punctata",ifelse(species=="PEES","Pediomelum.esculentum", ifelse(species=="SPCR","Sporobolus.cryptandrus",ifelse(species=="POSE","Poa.secunda",ifelse(species=="SPCO","Sphaeralcea.coccinea",ifelse(species=="TRDU","Tragopogon.dubius",ifelse(species=="TAOF","Taraxacum.officinale",ifelse(species=="OESU","Oenotherea.suffrutescens", ifelse(species=="PASM","Pascopyrum.smithii",ifelse(species=="PLPA","Plantago.patagonica",ifelse(species== "OPPO","Opuntia.polyacantha",ifelse(species=="DECA","Dalea.candida",species))))))))))))))))))))))))))))))))))))))) %>% 
  dplyr::select(block,plot,grazing_treatment,Genus_Species,Relative_Cover) %>% 
  unique()

#Merge Relative Cover data and functional group data
RelCov_FunctionalGroups<-Relative_Cover_PlantSp_Clean %>% 
  full_join(Functional_Groups, relationship="many-to-many") %>% 
  filter(Relative_Cover!="NA")

#### Plot Level Arthropod Abundance by Grazing Treatment ####

## Weight
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

## Count

Abundance_Count<-Abundance_Order %>% 
  group_by(Year,Grazing_Treatment,Correct_Order) %>% 
  summarise(Average_Count=mean(Abundance),Count_SD=sd(Abundance),Count_n=length(Abundance)) %>% 
  mutate(Count_St_Error=Count_SD/sqrt(Count_n)) %>% 
  ungroup()%>% 
  mutate(Correct_Order="Plot")

### Plot Level Abundance by Order by Grazing Treatment ###
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
  mutate(Grazing_Treatment=ifelse(Grazing_Treatment=="HG","High Impact Grazing",ifelse(Grazing_Treatment=="LG","Destock Grazing",ifelse(Grazing_Treatment=="NG","Cattle Removal",Grazing_Treatment))))

#### Order Relative Weight ####

Relative_Weight<-Weight_Data_Summed %>% 
  filter(Plot!="NA") %>% 
  filter(Correct_Order!="unknown"&Correct_Order!="Unknown"&Correct_Order!="Unknown_1"&Correct_Order!="Body_Parts"&Correct_Order!="Body Parts") %>% 
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
  mutate(RelativeWeight=Order_Weight/Total_Weight) %>% 
  group_by(Year,Grazing_Treatment,Correct_Order) %>% 
  summarise(Average_RelativeWeight=mean(RelativeWeight)) %>% 
  ungroup() %>% 
  mutate(Grazing_Treatment=ifelse(Grazing_Treatment=="HG","High Impact Grazing",ifelse(Grazing_Treatment=="LG","Destock Grazing",ifelse(Grazing_Treatment=="NG","Cattle Removal",Grazing_Treatment))))

#### Order Relative Count ####

Relative_Count<-Abundance %>% 
  filter(Plot!="NA") %>% 
  filter(Correct_Order!="unknown"&Correct_Order!="Unknown"&Correct_Order!="Unknown_1"&Correct_Order!="Body_Parts"&Correct_Order!="Body Parts") %>% 
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
  mutate(RelativeCount=Order_Abundance/Total_Abundance) %>% 
  group_by(Year,Grazing_Treatment,Correct_Order) %>% 
  summarise(Average_RelativeCount=mean(RelativeCount)) %>% 
  ungroup() %>% 
  mutate(Grazing_Treatment=ifelse(Grazing_Treatment=="HG","High Impact Grazing",ifelse(Grazing_Treatment=="LG","Destock Grazing",ifelse(Grazing_Treatment=="NG","Cattle Removal",Grazing_Treatment))))



#### Figure 1: (A,B): Average Plot Weight, (C,D): Order Proportion by Weight, (E,F): Order Proportion by Cover ####

# 2020 Average Plot Weight
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
  scale_fill_manual(values=c("thistle2","thistle3","thistle4"), labels=c("High Impact Grazing","Cattle Removal","Destock"))+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),legend.position = "none")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  #Make the y-axis extend to 50
  expand_limits(y=0.5)+
  scale_y_continuous(labels = label_number(accuracy = 0.01))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45))+
  geom_text(x=1.6, y=0.5, label="A. 2020 Plot Weight",size=20)

# Average Plot Weight
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
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  scale_fill_manual(values=c("thistle2","thistle3","thistle4"), labels=c("High Impact Grazing","Cattle Removal","Destock"))+
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.title.x=element_blank(),axis.text.x=element_blank(),legend.position = "none")+
  #Make the y-axis extend to 50
  expand_limits(y=0.5)+
  scale_y_continuous(labels = label_number(accuracy = 0.01))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45))+
  geom_text(x=1.6, y=0.5, label="B. 2021 Plot Weight",size=20)

# Average Plot Weight
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
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  scale_fill_manual(values=c("thistle2","thistle3","thistle4"), labels=c("High Impact Grazing","Cattle Removal","Destock"))+
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.title.x=element_blank(),axis.text.x=element_blank(),legend.position = "none")+
  #Make the y-axis extend to 50
  expand_limits(y=0.5)+
  scale_y_continuous(labels = label_number(accuracy = 0.01))+
  theme(text = element_text(size = 55))+
  geom_text(x=1.6, y=0.5, label="C. 2022 Plot Weight",size=20)


# Proportion of Orders by Weight
Order_Weight_2020<-ggplot(subset(Relative_Weight,Year==2020),aes(x=Grazing_Treatment,y=Average_RelativeWeight,fill=Correct_Order, position = "stack"))+
  geom_bar(stat="identity")+
  #Label the x-axis "Treatment"
  xlab("Grazing Treatment")+
  #Label the y-axis "Species Richness"
  ylab("Proportion of Orders")+
  theme(legend.background=element_blank())+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  scale_fill_manual(values=c("#661100","#CC6677","#DDCC77","#2D7947", "#4B4084","#76948F"), labels=c("Araneae","Coleoptera","Diptera","Hemiptera","Hymenoptera","Orthoptera"), name = "Order")+
  #scale_fill_manual(values=c("grey30","grey10"), labels=c("Orthoptera Weight","Plot Weight"))+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),legend.position = "none")+
  expand_limits(y=1.2)+
  scale_y_continuous(labels = label_number(accuracy = 0.25))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45))+
  geom_text(x=1.9, y=1.2, label="D. Abundance by Weight",size=20)

Order_Weight_2021<-ggplot(subset(Relative_Weight,Year==2021),aes(x=Grazing_Treatment,y=Average_RelativeWeight,fill=Correct_Order, position = "stack"))+
  geom_bar(stat="identity")+
  #Label the x-axis "Treatment"
  xlab("Grazing Treatment")+
  #Label the y-axis "Species Richness"
  ylab("Proportion of Orders")+
  theme(legend.background=element_blank())+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  scale_fill_manual(values=c("#661100","#CC6677","#DDCC77","#2D7947", "#4B4084","#76948F"), labels=c("Araneae","Coleoptera","Diptera","Hemiptera","Hymenoptera","Orthoptera"), name = "Order")+
  #scale_fill_manual(values=c("grey30","grey10"), labels=c("Orthoptera Weight","Plot Weight"))+
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.title.x=element_blank(),axis.text.x=element_blank(),legend.position = "none")+
  expand_limits(y=1.2)+
  scale_y_continuous(labels = label_number(accuracy = 0.25))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45))+
  geom_text(x=1.9, y=1.2,label="E.Abundance by Weight",size=20)

Order_Weight_2022<-ggplot(subset(Relative_Weight,Year==2022),aes(x=Grazing_Treatment,y=Average_RelativeWeight,fill=Correct_Order, position = "stack"))+
  geom_bar(stat="identity")+
  #Label the x-axis "Treatment"
  xlab("Grazing Treatment")+
  #Label the y-axis "Species Richness"
  ylab("Proportion of Orders")+
  theme(legend.background=element_blank())+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  scale_fill_manual(values=c("#661100","#CC6677","#DDCC77","#2D7947", "#4B4084","#7E69A0","#6B99C7","#76948F","#7B4B4E","#BCB9EC"), labels=c("Araneae","Coleoptera","Diptera","Hemiptera","Hymenoptera","Lepidoptera","Neuroptera","Orthoptera","Thysanoptera","Trombiculidae"), name = "Order")+
  #scale_fill_manual(values=c("grey30","grey10"), labels=c("Orthoptera Weight","Plot Weight"))+
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.title.x=element_blank(),axis.text.x=element_blank(),legend.position = "none")+
  expand_limits(y=1.2)+
  scale_y_continuous(labels = label_number(accuracy = 0.25))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45))+
  geom_text(x=1.9, y=1.2, label="F.Abundance by Weight",size=20)

#### Order Relative Count Plot ####
Order_Count_2020<-ggplot(subset(Relative_Count,Year==2020),aes(x=Grazing_Treatment,y=Average_RelativeCount,fill=Correct_Order, position = "stack"))+
  geom_bar(stat="identity")+
  #Label the x-axis "Treatment"
  xlab("Grazing Treatment")+
  #Label the y-axis "Species Richness"
  ylab("Proportion of Orders")+
  theme(legend.background=element_blank())+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  scale_fill_manual(values=c("#661100","#CC6677","#DDCC77","#2D7947", "#4B4084","#76948F"), labels=c("Araneae","Coleoptera","Diptera","Hemiptera","Hymenoptera","Orthoptera"), name = "Order")+
  #scale_fill_manual(values=c("grey30","grey10"), labels=c("Orthoptera Count","Plot Count"))+
  theme(legend.key = element_rect(size=3), legend.key.size = unit(1,"centimeters"),legend.position="NONE")+
  #Make the y-axis extend to 50
  expand_limits(y=1.2)+
  scale_y_continuous(labels = label_number(accuracy = 0.25))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45))+
  geom_text(x=1.8, y=1.2,label="G.Abundance by Count",size=20)

Order_Count_2021<-ggplot(subset(Relative_Count,Year==2021),aes(x=Grazing_Treatment,y=Average_RelativeCount,fill=Correct_Order, position = "stack"))+
  geom_bar(stat="identity")+
  #Label the x-axis "Treatment"
  xlab("Grazing Treatment")+
  #Label the y-axis "Species Richness"
  ylab("Proportion of Orders")+
  theme(legend.background=element_blank())+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  scale_fill_manual(values=c("#661100","#CC6677","#DDCC77","#2D7947", "#4B4084","#76948F"), labels=c("Araneae","Coleoptera","Diptera","Hemiptera","Hymenoptera","Orthoptera"), name = "Order")+
  #scale_fill_manual(values=c("grey30","grey10"), labels=c("Orthoptera Count","Plot Count"))+
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),legend.position = "none")+
  #Make the y-axis extend to 50
  expand_limits(y=1.2)+
  scale_y_continuous(labels = label_number(accuracy = 0.25))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45))+
  geom_text(x=1.8, y=1.2, label="H.Abundance by Count",size=20)

Order_Count_2022<-ggplot(subset(Relative_Count,Year==2022),aes(x=Grazing_Treatment,y=Average_RelativeCount,fill=Correct_Order, position = "stack"))+
  geom_bar(stat="identity")+
  #Label the x-axis "Treatment"
  xlab("Grazing Treatment")+
  #Label the y-axis "Species Richness"
  ylab("Proportion of Orders")+
  theme(legend.background=element_blank())+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  scale_fill_manual(values=c("#661100","#CC6677","#DDCC77","#2D7947", "#4B4084","#7E69A0","#6B99C7","#76948F","#7B4B4E","#BCB9EC"), labels=c("Araneae","Coleoptera","Diptera","Hemiptera","Hymenoptera","Lepidoptera","Neuroptera","Orthoptera","Thysanoptera","Trombiculidae"), name = "Order")+
  #scale_fill_manual(values=c("grey30","grey10"), labels=c("Orthoptera Count","Plot Count"))+
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),legend.position = "none")+
  #Make the y-axis extend to 50
  expand_limits(y=1.2)+
  scale_y_continuous(labels = label_number(accuracy = 0.25))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45))+
  geom_text(x=1.8, y=1.2, label="I.Abundance by Count",size=20)

#### Create Figure 1 ####
Dvac_2020_Plot+
  Dvac_2021_Plot+
  Dvac_2022_Plot+
  Order_Weight_2020 +  
  Order_Weight_2021+
  Order_Weight_2022 +
  Order_Count_2020 +  
  Order_Count_2021+
  Order_Count_2022 +
  plot_layout(ncol = 3,nrow = 3)
#save at 3000 x 3000





#### Normality: Plot Weights####
#2020
dvac_2020_Weight <- lm(data = subset(Weight_Data_Summed_dvac, Year == 2020), sqrt(Plot_Weight)  ~ Grazing_Treatment)
ols_plot_resid_hist(dvac_2020_Weight) 
ols_test_normality(dvac_2020_Weight) #normal

#2021
dvac_2021_Weight <- lm(data = subset(Weight_Data_Summed_dvac, Year == 2021), log(Plot_Weight)  ~ Grazing_Treatment)
ols_plot_resid_hist(dvac_2021_Weight) 
ols_test_normality(dvac_2021_Weight) #normal

#2022
dvac_2022_Weight <- lm(data = subset(Weight_Data_Summed_dvac, Year == 2022), log(Plot_Weight)  ~ Grazing_Treatment)
ols_plot_resid_hist(dvac_2022_Weight) 
ols_test_normality(dvac_2022_Weight) #normal

#### Stats: Plot Weights by Grazing Treatment####
#2020
Plot_Weight_D_2020_Glmm <- lmer(sqrt(Plot_Weight) ~ Grazing_Treatment + (1 | Block) , data = subset(Weight_Data_Summed_dvac,Year==2020))
anova(Plot_Weight_D_2020_Glmm) #not significant

#2021
Plot_Weight_D_2021_Glmm <- lmer(log(Plot_Weight) ~ Grazing_Treatment + (1 | Block) , data = subset(Weight_Data_Summed_dvac,Year==2021))
anova(Plot_Weight_D_2021_Glmm) # p=0.003987
###post hoc test for lmer test ##
summary(glht(Plot_Weight_D_2021_Glmm, linfct = mcp(Grazing_Treatment = "Tukey")), test = adjusted(type = "BH")) #NG-LG (p=0.0.56774), #LG-HG (0.00857), NG-HG (0.00256)

#2022
Plot_Weight_D_2022_Glmm <- lmer(log(Plot_Weight) ~ Grazing_Treatment + (1 | Block) , data = subset(Weight_Data_Summed_dvac,Year==2022))
anova(Plot_Weight_D_2022_Glmm) #not significant

