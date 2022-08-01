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
#install.packages("devtools")
library(grid)

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
  rename(Sample_Number=Sample_num) %>% 
  mutate(Collection_Method=ifelse(Collection_Method=="d-vac","dvac",ifelse(Collection_Method=="sweep_net","sweep",Collection_Method)))
Weight_Data_21<-read.csv("2021_Sweep_Net_D-Vac_Weight_Data_FK.csv",header=T) %>% 
  mutate(Collection_Method=ifelse(Collection_Method=="d-vac","dvac",ifelse(Collection_Method=="sweep_net","sweep",Collection_Method)))

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

#fix data entry errors - 2020 - Sweep Net check B3, NG,Plot 100 sample 17 - should be Phoetaliotes nebranscensis
ID_20[1067, "Correct_Genus"] <- "Phoetaliotes"
#fix data entry errors - 2020 - sweep net check B3, NG,Plot 100 sample 18 - should be melanoplus gladstoni
ID_20[1115, "Correct_Genus"] <- "Melanoplus"

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

#fix data entry errors - 2021 - Dvac check B2, HG,Plot 29 sample 2 - should be Ageneotettix deorum
ID_21[261, "Correct_Species"] <- "deorum"
#fix data entry errors - 2021 - Dvac check B1, HG,Plot 15 sample 5 -  should be Melanoplus brunri
ID_21[283, "Correct_Species"] <- "bruneri"

#Merge together data frames

ID_Data_Official<-ID_20 %>% 
  rbind(ID_21)

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
  mutate(Plot=ifelse(Collection_Method=="sweep",100,Plot)) %>% 
  #replace any weight that is <0.0001 with 0.00001 %>% 
  mutate(Dry_Weight_g=as.numeric(ifelse(Dry_Weight_g=="<0.0001","0.00005",Dry_Weight_g))) %>% 
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
  select(Coll_Year_Bl_Trt_Pl,Sample_Number,Correct_Order,Dry_Weight_g,Notes) %>% 
  #RemovNAs from Dry weight
  filter(!is.na(Dry_Weight_g)) %>% 
  separate(Coll_Year_Bl_Trt_Pl, c("Coll_Year_Bl_Trt","Plot"), "-")

#fix data entry errors 
Weight_Data_Official[1703, "Dry_Weight_g"] <- 0.0140
Weight_Data_Official[1614, "Plot"] <- 40
Weight_Data_Official[45,"Plot"] <- 16
Weight_Data_Official[1521,"Coll_Year_Bl_Trt"] <- "dvac_2021_2_NG"
Weight_Data_Official[1528,"Coll_Year_Bl_Trt"] <- "dvac_2021_2_NG"
Weight_Data_Official[1606,"Coll_Year_Bl_Trt"] <- "dvac_2021_3_NG"
Weight_Data_Official[1607,"Coll_Year_Bl_Trt"] <- "dvac_2021_3_NG"
Weight_Data_Official[1608,"Coll_Year_Bl_Trt"] <- "dvac_2021_3_NG"
Weight_Data_Official[1609,"Coll_Year_Bl_Trt"] <- "dvac_2021_3_NG"
Weight_Data_Official[1610,"Coll_Year_Bl_Trt"] <- "dvac_2021_3_NG"
Weight_Data_Official[1611,"Coll_Year_Bl_Trt"] <- "dvac_2021_3_NG"
Weight_Data_Official[1612,"Coll_Year_Bl_Trt"] <- "dvac_2021_3_NG"
Weight_Data_Official[1613,"Coll_Year_Bl_Trt"] <- "dvac_2021_3_NG"
Weight_Data_Official[1703,"Coll_Year_Bl_Trt"] <- "dvac_2021_3_NG"
Weight_Data_Official[1699,"Coll_Year_Bl_Trt"] <- "dvac_2021_3_NG"
Weight_Data_Official[1628,"Coll_Year_Bl_Trt"] <- "dvac_2021_3_LG"

####Total Plot Weight Differences ####

#Summing all weights by order within dataset, grazing treatment, block, and plot so that we can look at differences in order across plots
Weight_Data_Summed<-aggregate(Dry_Weight_g~Coll_Year_Bl_Trt+Plot+Correct_Order, data=Weight_Data_Official, FUN=sum, na.rm=FALSE) 

#Separating out Treatment_Plot into all distinctions again so that we can group based on different things
Weight_Data_Summed<-Weight_Data_Summed %>% 
  separate(Coll_Year_Bl_Trt, c("Collection_Method","Year","Block","Grazing_Treatment"), "_")

#create dataframe that just has sweepnet samples in it
Weight_Data_Summed_sweep<-Weight_Data_Summed %>% 
  filter(Collection_Method=="sweep") %>% 
  #sum by plot #
  group_by(Year,Block,Grazing_Treatment,Plot) %>% 
  summarise(Plot_Weight=sum(Dry_Weight_g)) %>% 
  ungroup()

#create dataframe that just has dvac samples in it
Weight_Data_Summed_dvac<-Weight_Data_Summed %>% 
  filter(Collection_Method=="dvac") %>% 
  #sum by plot #
  group_by(Year,Block,Grazing_Treatment,Plot) %>% 
  summarise(Plot_Weight=sum(Dry_Weight_g)) %>% 
  ungroup()


### Average Plot Weight across Grazing treatment ####
Weight_by_Grazing_sweep<-Weight_Data_Summed_sweep %>% 
  group_by(Year,Grazing_Treatment) %>% 
  summarise(Average_Weight=mean(Plot_Weight),Weight_SD=sd(Plot_Weight),Weight_n=length(Plot_Weight)) %>% 
  mutate(Weight_St_Error=Weight_SD/sqrt(Weight_n)) 

Weight_by_Grazing_dvac<-Weight_Data_Summed_dvac %>% 
  group_by(Year,Grazing_Treatment) %>% 
  summarise(Average_Weight=mean(Plot_Weight),Weight_SD=sd(Plot_Weight),Weight_n=length(Plot_Weight)) %>% 
  mutate(Weight_St_Error=Weight_SD/sqrt(Weight_n))


#### Total Plot Weight Differences - Figures ####

# 2020 - Sweepnet
#Graph of Weights from Sweep Net by Grazing treatment- 2020
SN_2020_Plot<-ggplot(subset(Weight_by_Grazing_sweep,Year==2020),aes(x=Grazing_Treatment,y=Average_Weight,fill=Grazing_Treatment))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge), and fill in the bars with the color grey.  
  geom_bar(stat="identity",position = "dodge")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(aes(ymin=Average_Weight-Weight_St_Error,ymax=Average_Weight+Weight_St_Error),position=position_dodge(),width=0.2)+
  #Label the x-axis "Treatment"
  xlab("Grazing Treatment")+
  #Label the y-axis "Species Richness"
  ylab("Average Plot Weight (g)")+
  theme(legend.background=element_blank())+
  scale_x_discrete(labels=c("HG"="High Grazing","NG"="No Grazing","LG"="Low Grazing"))+
  scale_fill_manual(values=c("thistle2","thistle3","thistle4"), labels=c("High Grazing","No Grazing","Low Grazing"))+
  theme(legend.key = element_rect(size=3), legend.key.size = unit(1,"centimeters"),legend.position="NONE")+
  #Make the y-axis extend to 50
  expand_limits(y=8)+
  scale_y_continuous(labels = label_number(accuracy = 0.1))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45))+
  geom_text(x=1.1, y=8, label="2020 Sweep Net",size=20)
  #no grazing is different than low grazing, low grazing is different than high grazing, no and high grazing are the same
  #annotate("text",x=1,y=2.9,label="a",size=20)+ #no grazing
  #annotate("text",x=2,y=5.5,label="b",size=20)+ #low grazing
  #annotate("text",x=3,y=3.4,label="a",size=20) #high grazing
#Save at the graph at 1500x1500

# 2021 - Sweepnet
#Graph of Weights from Sweep Net by Grazing treatment- 2020
SN_2021_Plot<-ggplot(subset(Weight_by_Grazing_sweep,Year==2021),aes(x=Grazing_Treatment,y=Average_Weight,fill=Grazing_Treatment))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge), and fill in the bars with the color grey.  
  geom_bar(stat="identity",position = "dodge")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(aes(ymin=Average_Weight-Weight_St_Error,ymax=Average_Weight+Weight_St_Error),position=position_dodge(),width=0.2)+
  #Label the x-axis "Treatment"
  xlab("Grazing Treatment")+
  #Label the y-axis "Species Richness"
  ylab("Average Plot Weight (g)")+
  theme(legend.background=element_blank())+
  scale_x_discrete(labels=c("HG"="High Grazing","NG"="No Grazing","LG"="Low Grazing"))+
  scale_fill_manual(values=c("thistle2","thistle3","thistle4"), labels=c("High Grazing","No Grazing","Low Grazing"))+ 
  theme(legend.key = element_rect(size=3), legend.key.size = unit(1,"centimeters"),legend.position="NONE")+
  #Make the y-axis extend to 50
  expand_limits(y=1)+
  scale_y_continuous(labels = label_number(accuracy = 0.1))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45))+
  geom_text(x=1.1, y=1, label="2021 Sweep Net",size=20)
#no grazing is different than low grazing, low grazing is different than high grazing, no and high grazing are the same
#annotate("text",x=1,y=2.9,label="a",size=20)+ #no grazing
#annotate("text",x=2,y=5.5,label="b",size=20)+ #low grazing
#annotate("text",x=3,y=3.4,label="a",size=20) #high grazing
#Save at the graph at 1500x1500

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
  scale_x_discrete(labels=c("HG"="High Grazing","NG"="No Grazing","LG"="Low Grazing"))+
  scale_fill_manual(values=c("thistle2","thistle3","thistle4"), labels=c("High Grazing","No Grazing","Low Grazing"))+
  theme(legend.key = element_rect(size=3), legend.key.size = unit(1,"centimeters"),legend.position="NONE")+
  #Make the y-axis extend to 50
  expand_limits(y=0.4)+
  scale_y_continuous(labels = label_number(accuracy = 0.01))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45))+
  geom_text(x=1.1, y=0.4, label="2020 DVac",size=20)
#no grazing is different than low grazing, low grazing is different than high grazing, no and high grazing are the same
#annotate("text",x=1,y=2.9,label="a",size=20)+ #no grazing
#annotate("text",x=2,y=5.5,label="b",size=20)+ #low grazing
#annotate("text",x=3,y=3.4,label="a",size=20) #high grazing
#Save at the graph at 1500x1500

# 2021 - Dvac
#Graph of Weights from Sweep Net by Grazing treatment- 2020
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
  scale_x_discrete(labels=c("HG"="High Grazing","NG"="No Grazing","LG"="Low Grazing"))+
  scale_fill_manual(values=c("thistle2","thistle3","thistle4"), labels=c("High Grazing","No Grazing","Low Grazing"))+
  theme(legend.key = element_rect(size=3), legend.key.size = unit(1,"centimeters"),legend.position="NONE")+
  #Make the y-axis extend to 50
  expand_limits(y=0.15)+
  scale_y_continuous(labels = label_number(accuracy = 0.01))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45))+
  geom_text(x=1.1, y=0.15, label="2021 Dvac",size=20)
#low grazing is different from high grazing, no grazing is different from high grazing -- change text below
#annotate("text",x=1,y=2.9,label="a",size=20)+ #no grazing
#annotate("text",x=2,y=5.5,label="b",size=20)+ #low grazing
#annotate("text",x=3,y=3.4,label="a",size=20) #high grazing
#Save at the graph at 1500x1500

### Put all graphs together
pushViewport(viewport(layout=grid.layout(2,2)))
#print out the viewport plot where the Species_Richness_plot is at position 1,1
print(SN_2020_Plot,vp=viewport(layout.pos.row=1, layout.pos.col =1))
#print out the viewport plot where the Species_Richness_plot is at position 1,2
print(SN_2021_Plot,vp=viewport(layout.pos.row=1, layout.pos.col =2))
#print out the viewport plot where the Species_Richness_plot is at position 1,1
print(Dvac_2020_Plot,vp=viewport(layout.pos.row=2, layout.pos.col =1))
#print out the viewport plot where the Species_Richness_plot is at position 1,2
print(Dvac_2021_Plot,vp=viewport(layout.pos.row=2, layout.pos.col =2))
#Save at 1800 x 1000 

#### Glmm for Plot Weights by Grazing Treatment####

# 2020 Sweep net
Plot_Weight_S_2020_Glmm <- lmer(Plot_Weight ~ Grazing_Treatment + (1 | Block) , data = subset(Weight_Data_Summed_sweep,Year==2020))
summary(Plot_Weight_S_2020_Glmm)
anova(Plot_Weight_S_2020_Glmm)

# 2021 Sweep Net
Plot_Weight_S_2021_Glmm <- lmer(Plot_Weight ~ Grazing_Treatment + (1 | Block) , data = subset(Weight_Data_Summed_sweep,Year==2021))
summary(Plot_Weight_S_2021_Glmm)
anova(Plot_Weight_S_2021_Glmm)

# 2020 Dvac
Plot_Weight_D_2020_Glmm <- lmer(Plot_Weight ~ Grazing_Treatment + (1 | Block) , data = subset(Weight_Data_Summed_dvac,Year==2020))
summary(Plot_Weight_D_2020_Glmm)
anova(Plot_Weight_D_2020_Glmm)

# 2021 Dvac
Plot_Weight_D_2021_Glmm <- lmer(Plot_Weight ~ Grazing_Treatment + (1 | Block) , data = subset(Weight_Data_Summed_dvac,Year==2021))
summary(Plot_Weight_D_2021_Glmm)
anova(Plot_Weight_D_2021_Glmm) #grazing treatment is significantly different 



#### Average by order across Grazing treatment ####

#Averaging
Weight_by_Order<-Weight_Data_Summed %>% 
  group_by(Collection_Method,Year, Grazing_Treatment, Correct_Order) %>% 
  summarise(Average_Weight=mean(Dry_Weight_g),Weight_SD=sd(Dry_Weight_g),Weight_n=length(Dry_Weight_g)) %>% 
  filter(Correct_Order!="Unknown_1") %>% 
  filter(Correct_Order!="Unknown") %>% 
  filter(Correct_Order!="Snail") %>% 
  filter(Correct_Order!="Body_Parts") %>% 
  ungroup()

#Seperating out sweep net
Weight_by_Order_Sweepnet<-Weight_by_Order %>% 
  filter(Collection_Method=="sweep")

#Seperating out dvac
Weight_by_Order_Dvac<-Weight_by_Order %>% 
  filter(Collection_Method=="dvac")




