#### Grazing x Arthropod Data - 2020 - 2022#
#### Code created by: Kathryn Bloodworth #

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
