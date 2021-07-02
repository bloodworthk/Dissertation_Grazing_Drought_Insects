#### Grazing x Insect Data - 2020 ####
#### Code created by: Kathryn Bloodworth and Will Mann ####
#Date started: 06/14/2021 #

#### Set working directory and load libraries ####

# Set Working Directory - Mac
setwd("/Users/kathrynbloodworth/Dropbox (Smithsonian)/Projects/Dissertation/Data/Insect_Data")


#PC
setwd("/Users/kjbloodw/Dropbox (Smithsonian)/Projects/Dissertation/Data/Insect_Data")

#Load Tidyverse#
library(tidyverse)
library(lmerTest)

#### Load in data ####
Sweepnet_weight<-read.csv("2020_Sweep_Net_Weight_Data_FK.csv", header=T) %>% 
  rename(Grazing_Treatment=ï..Grazing_Treatment)
Sweepnet_ID<-read.csv("2020_Sweep_Net_Data_FK.csv", header=T) %>% 
  rename(Grazing_Treatment=ï..Grazing_Treatment)
D_Vac_Weight<-read.csv("2020_DVac_Weight_Data_FK.csv", header=T) %>% 
  rename(Grazing_Treatment=ï..Grazing_Treatment)
D_Vac_ID<-read.csv("2020_DVac_Data_FK.csv", header=T) %>% 
  rename(Grazing_Treatment=ï..Grazing_Treatment)

#Set ggplot2 theme to black and white
theme_set(theme_bw())
#Update ggplot2 theme - make box around the x-axis title size 30, vertically justify x-axis title to 0.35, Place a margin of 15 around the x-axis title.  Make the x-axis title size 30. For y-axis title, make the box size 30, put the writing at a 90 degree angle, and vertically justify the title to 0.5.  Add a margin of 15 and make the y-axis text size 25. Make the plot title size 30 and vertically justify it to 2.  Do not add any grid lines.  Do not add a legend title, and make the legend size 20
theme_update(axis.title.x=element_text(size=30, vjust=-0.35, margin=margin(t=15)),
             axis.text.x=element_text(size=30), axis.title.y=element_text(size=30, angle=90, vjust=0.5,
                                                                          margin=margin(r=15)), axis.text.y=element_text(size=30), plot.title =
               element_text(size=30, vjust=2), panel.grid.major=element_blank(),
             panel.grid.minor=element_blank(), legend.title=element_blank(),
             legend.text=element_text(size=30))


#### Formatting Data ####

#make new dataframe for sweepnet ID changing block to numerical counts, and remnaming incorrect columns and adding dataset type and plot number (1) to columns
S_ID<-Sweepnet_ID %>% 
  mutate(Block=ifelse(Grazing_Treatment=="B3",3,ifelse(Grazing_Treatment=="B2",2,1))) %>%
  mutate(Grazing_Treatment=ifelse(Plot=="NG",0,ifelse(Plot=="LG",1,ifelse(Plot=="HG",2,Plot)))) %>%
  mutate(Plot=1) %>%
  mutate(Dataset="S")

#make sample numeric not characters
S_ID<-transform(S_ID,Sample = as.numeric(Sample))

# make new dataframe for sweepnet weights, changing block to correct denotion, adding plot number and dataset type
S_Weight<- Sweepnet_weight %>%
  mutate(Block=ifelse(Plot=="B3",3,ifelse(Plot=="B2",2,1))) %>%
  mutate(Grazing_Treatment=ifelse(Grazing_Treatment=="NG",0,ifelse(Grazing_Treatment=="LG",1,ifelse(Grazing_Treatment=="HG",2,Grazing_Treatment)))) %>%
  mutate(Plot=1) %>%
  mutate(Dataset="S")

#make sample numeric not characters
S_Weight<-transform(S_Weight,Sample = as.numeric(Sample))

# make new dataframe for Dvac ID changing block to correct denotion, and adding in dataset type
D_ID<- D_Vac_ID %>%
  mutate(Block=ifelse(Block=="B3",3,ifelse(Block=="B2",2,1))) %>%
  mutate(Grazing_Treatment=ifelse(Grazing_Treatment=="NG",0,ifelse(Grazing_Treatment=="LG",1,ifelse(Grazing_Treatment=="HG",2,Grazing_Treatment)))) %>%
  mutate(Dataset="D")

D_ID<-transform(D_ID,Sample = as.numeric(Sample))

#make new dataframe for Dvac weights, renaming sample number so it is consistant and adding in dataset type
D_Weight<-D_Vac_Weight %>%
  rename(Sample=Sample_num) %>%
  mutate(Grazing_Treatment=ifelse(Grazing_Treatment=="NG",0,ifelse(Grazing_Treatment=="LG",1,ifelse(Grazing_Treatment=="HG",2,Grazing_Treatment)))) %>%
  mutate(Dataset="D")

D_Weight<-transform(D_Weight,Sample = as.numeric(Sample))

#Merge S_ID and D_ID and fix names#
ID_Data<- S_ID %>%
  rbind(D_ID) %>%
  mutate(Correct_Class=ifelse(Class=="insecta","Insecta",ifelse(Class=="arachnida","Arachnida",Class))) %>% 
  mutate(Correct_Order=ifelse(Order=="orthoptera","Orthoptera",ifelse(Order=="hemiptera","Hemiptera",ifelse(Order=="coleoptera","Coleoptera",ifelse(Order=="hymenoptera","Hymenoptera",ifelse(Order=="diptera","Diptera",ifelse(Order=="araneae","Araneae",Order))))))) %>% 
  mutate(Correct_Family=ifelse(Family=="acrididae", "Acrididae",ifelse(Family=="cicadellidae", "Cicadellidae", ifelse(Family=="geocoridae", "Geocordidae", ifelse(Family=="carabidae", "Carabidae", ifelse(Family=="chrysomelidae","Chrysomelidae", ifelse(Family=="formicidae", "Formicidae", ifelse(Family=="halictidae", "Halictidae", ifelse(Family=="agromyzidae", "Agromyzidae", ifelse(Family=="lycosidae", "Lycosidae", ifelse(Family=="platygastridae", "Platygastridae", ifelse(Family=="tettigoniidae", "Tettigoniidae", ifelse(Family=="salticidae", "Salticidae", ifelse(Family=="thomisidae", "Thomisidae", ifelse(Family=="pentatomidae", "Pentatomidae", ifelse(Family=="lygaeidae", "Lygaeidae", ifelse(Family=="scutelleridae", "Scutelleridae", ifelse(Family=="gryllidae", "Gryllidae", ifelse(Family=="asilidae", "Asilidae", ifelse(Family=="chrysididae", "Chrysididae", ifelse(Family=="curculionidae", "Curculionidae", ifelse(Family=="latridiidae","Latridiidae", ifelse(Family=="muscidae", "Muscidae", ifelse(Family=="tenebrionidae", "Tenebrionidae",Family)))))))))))))))))))))))) %>% 
  mutate(Correct_Genus=ifelse(Genus=="Melanoplus","Melanoplus",ifelse(Genus=="arphia","Arphia",ifelse(Genus=="melanoplus","Melanoplus",ifelse(Genus=="opeia","Opeia",ifelse(Genus=="nenconocephalus","Neoconocephalus",ifelse(Genus=="pachybrachis","Pachybrachis",ifelse(Genus=="ageneotettix ","Ageneotettix", ifelse(Genus=="phoetaliotes","Phoetaliotes",ifelse(Genus=="Ageneotettix ","Ageneotettix",ifelse(Genus=="amphiturnus","Amphiturnus",ifelse(Genus=="Ageneotettox","Ageneotettix",ifelse(Genus=="Agneotettix","Ageneotettix",ifelse(Genus=="ageneotettix","Ageneotettix",Genus)))))))))))))) %>% 
  mutate(Correct_Species=ifelse(Species=="differentalis","differentialis",ifelse(Species=="sanguinipes","sanguinipes",ifelse(Species=="packardi","packardii",ifelse(Species=="unknown","sp",Species))))) %>% 
  select(-Sub.order,-Class,-Family,-Genus,-Order,-Species) %>% 
  mutate(Genus_Species=paste(Correct_Genus,Correct_Species,sep="_"))
#remove blank rows in dataframe
ID_Data<-ID_Data[-which(ID_Data$Correct_Class==""),]

#### Check Data Sheets #### 
#Ageneotettix_nebrascensis is a species for Sweepnet data, B3,NG, sample 17 - should be phoetaliotes nebrascensis - fixed
#Melanoplus_deorum is a species for Sweepnet data, B3, NG, sample 16 - - should be ageneotettix deorum - fixed
#Melanoplus_nebrascensis is a species for Sweepnet data, B3, NG sample 7 - should be phoetaliotes nebrascensis - fixed
#Phoetaliotes_gladstoni is a species for Sweepnet data, B3, NG, sample 18 - should be melanoplus gladstoni-
#Phoetaliotes_sanguinipes is a species for Sweepnet data, B3, NG, sample 8 - should be melanoplus sanguinipes - fixed


#Change incorrect names in dataframe
ID_Data_Correct<-ID_Data[53, "Correct_Species"] <- "sp"
ID_Data_Correct<-ID_Data[54, "Correct_Species"] <- "sp"
ID_Data_Correct<-ID_Data[64, "Correct_Species"] <- "sp"
ID_Data_Correct<-ID_Data[774, "Correct_Species"] <- "sp"
ID_Data_Correct<-ID_Data[775, "Correct_Species"] <- "sp"
ID_Data_Correct<-ID_Data[780, "Correct_Species"] <- "sp"
ID_Data_Correct<-ID_Data[781, "Correct_Species"] <- "sp"
ID_Data_Correct<-ID_Data[790, "Correct_Species"] <- "sp"
ID_Data_Correct<-ID_Data[791, "Correct_Species"] <- "sp"
ID_Data_Correct<-ID_Data[792, "Correct_Species"] <- "sp"
ID_Data_Correct<-ID_Data[793, "Correct_Species"] <- "sp"
ID_Data_Correct<-ID_Data[794, "Correct_Species"] <- "sp"
ID_Data_Correct<-ID_Data[62, "Correct_Species"] <- "packardii"
ID_Data_Correct<-ID_Data[1372, "Correct_Species"] <- "sanguinipes"
ID_Data_Correct<-ID_Data[171, "Correct_Species"] <- "sp"
ID_Data_Correct<-ID_Data[253, "Correct_Species"] <- "sp"
ID_Data_Correct<-ID_Data[266, "Correct_Genus"] <- "Phoetaliotes"
ID_Data_Correct<-ID_Data[1072, "Correct_Genus"] <- "Ageneotettix"
ID_Data_Correct<-ID_Data[264, "Correct_Genus"] <- "Phoetaliotes"
ID_Data_Correct<-ID_Data[41, "Correct_Genus"] <- "Melanoplus"
ID_Data_Correct<-ID_Data[992, "Correct_Genus"] <- "Melanoplus"

#create a new column with genus and species together
ID_Data_Correct<- ID_Data %>% 
  mutate(Genus_Species=paste(Correct_Genus,Correct_Species,sep="_"))

ID_Data_Correct <- with(ID_Data_Correct,  ID_Data_Correct[order(Genus_Species) , ])

#seperate out Orthoptera ID into separate Datasheet
Orthoptera_ID_Data<-ID_Data %>% 
  filter(Correct_Order=="Orthoptera")

#Merge S_Weight and D_Weight#
Weight_Data<- D_Weight %>%
  rbind(S_Weight) %>% 
  mutate(Order2=ifelse(Notes=="Body Parts","Body_Parts",ifelse(Notes=="Body parts","Body_Parts",Order))) %>% 
  mutate(Correct_Order=ifelse(Order2=="Aranea","Araneae",ifelse(Order2=="Hempitera","Hemiptera",ifelse(Order2=="Lyaceidae","Lygaeidae",ifelse(Order2=="Aranaea","Araneae",ifelse(Order2=="","Orthoptera",Order2)))))) %>% 
  #change values that are <0.0001 to 0.00005 for analysis
  #### Check why there are NAs for some weights #### these two (B3-HG-3 sample 28 and 29) were just heads so they were put into body parts. remove from weight and ID data
  mutate(Correct_Dry_Weight_g=ifelse(Dry_Weight_g=="<0.0001","0.00005",ifelse(Dry_Weight_g=="<0.001","0.00005", Dry_Weight_g))) %>% 
  select(-Order,-Order2,-Dry_Weight_g) %>% 
  mutate(Treatment_Plot=paste(Dataset,Grazing_Treatment,Block,Plot,sep = "_"))

#remove blank rows in dataframe
Weight_Data<-Weight_Data[-which(Weight_Data$Correct_Dry_Weight_g==""),]

#make weights numeric not characters
Weight_Data<-transform(Weight_Data, Correct_Dry_Weight_g = as.numeric(Correct_Dry_Weight_g))
Weight_Data<-transform(Weight_Data, Sample = as.character(Sample))


#### Look at weight differences ####

#Summing all weights by order within dataset, grazing treatment, block, and plot so that we can look at differences in order across plots
Weight_Data_Summed<-aggregate(Correct_Dry_Weight_g~Treatment_Plot+Correct_Order, data=Weight_Data, FUN=sum) 

#Seperating out Treatment_Plot into all distinctions again so that we can group based on different things
Weight_Data_Summed<-Weight_Data_Summed %>% 
  separate(Treatment_Plot, c("Dataset", "Grazing_Treatment","Block","Plot"), "_")

Weight_Data_Summed_S<-Weight_Data_Summed %>% 
  filter(Dataset=="S")

#### Anova comparing insect weights by grazing treatment  Sweep net #####
Weight_Data_S_AOV <- aov(Correct_Dry_Weight_g ~ Grazing_Treatment, data = Weight_Data_Summed_S) 
summary(Weight_Data_S_AOV)
model.tables(Weight_Data_S_AOV)

#Create a 4-panel plot that contains the following in this order (clockwise from upper left)

plot2<-par(mfrow=c(2,2))


## b. scatterplot of the residuals vs. fitted values    for the model
plot(Weight_Data_S_AOV$residuals ~ Weight_Data_S_AOV$fitted.values, col = 'dark orange',main="",ylab="AOV Residuals",xlab="AOV Fitted Values",cex.lab=1.5,lwd = 2,cex.axis=1.5)
abline(h = 0, lty = 3)

## c. histogram of the residuals
hist(Weight_Data_S_AOV$residuals, col = 'white', border = 'dark orange',main="",ylab="Frequency",xlab="AOV Residuals",cex.lab=1.5,lwd = 2,cex.axis=1.5) 

## d. Q-Q plot
plot(Weight_Data_S_AOV, which = 2, col = 'dark orange',main="",cex.lab=1.5,lwd = 2,cex.axis=1.5) 

### Glmm for sweep net data ####
Weight_Data_S_GLMM <- lmer(Correct_Dry_Weight_g ~ Grazing_Treatment + (1 | Block) , data = Weight_Data_Summed_S)
summary(Weight_Data_S_GLMM)
anova(Weight_Data_S_GLMM)

####  Glmm for sweep net data by order too ####
Weight_Data_S_Order_GLMM <- lmer(Correct_Dry_Weight_g ~ Grazing_Treatment*Correct_Order + (1 | Block) , data = Weight_Data_Summed_S)
summary(Weight_Data_S_Order_GLMM)
anova(Weight_Data_S_Order_GLMM)


### D-vac data
Weight_Data_Summed_D<-Weight_Data_Summed %>% 
  filter(Dataset=="D")

#### Anova comparing insect weights by grazing treatment for d-vac #### 
Weight_Data_D_AOV <- aov(Correct_Dry_Weight_g ~ Grazing_Treatment, data = Weight_Data_Summed_D) 
summary(Weight_Data_D_AOV)
model.tables(Weight_Data_D_AOV)

#Create a 4-panel plot that contains the following in this order (clockwise from upper left)

plot2<-par(mfrow=c(2,2))


## b. scatterplot of the residuals vs. fitted values    for the model
plot(Weight_Data_D_AOV$residuals ~ Weight_Data_D_AOV$fitted.values, col = 'dark orange',main="",ylab="AOV Residuals",xlab="AOV Fitted Values",cex.lab=1.5,lwd = 2,cex.axis=1.5)
abline(h = 0, lty = 3)

## c. histogram of the residuals
hist(Weight_Data_D_AOV$residuals, col = 'white', border = 'dark orange',main="",ylab="Frequency",xlab="AOV Residuals",cex.lab=1.5,lwd = 2,cex.axis=1.5) 

## d. Q-Q plot
plot(Weight_Data_D_AOV, which = 2, col = 'dark orange',main="",cex.lab=1.5,lwd = 2,cex.axis=1.5) 

#### Glmm for D-vac ####
Weight_Data_D_GLMM <- lmer(Correct_Dry_Weight_g ~ Grazing_Treatment + (1 | Block) , data = Weight_Data_Summed_D)
summary(Weight_Data_D_GLMM)
anova(Weight_Data_D_GLMM)

#### Glmm for D-vac with order ####
Weight_Data_D_Order_GLMM <- lmer(Correct_Dry_Weight_g ~ Grazing_Treatment*Correct_Order + (1 | Block) , data = Weight_Data_Summed_D)
summary(Weight_Data_D_Order_GLMM)
anova(Weight_Data_D_Order_GLMM)


### Average by order across Grazing treatment ####
Weight_by_Grazing<-Weight_Data_Summed %>% 
  group_by(Dataset,Grazing_Treatment, Correct_Order) %>% 
  summarise(Average_Weight=mean(Correct_Dry_Weight_g),Weight_SD=sd(Correct_Dry_Weight_g),Weight_n=length(Correct_Dry_Weight_g)) %>% 
  filter(Correct_Order!="Unknown_1") %>% 
  filter(Correct_Order!="Unknown") %>% 
  filter(Correct_Order!="Snail") %>% 
  filter(Correct_Order!="Body_Parts")

Weight_by_Grazing_S<-Weight_by_Grazing %>% 
  filter(Dataset=="S")

Weight_by_Grazing_D<-Weight_by_Grazing %>% 
  filter(Dataset=="D")

#Color Palette
cbbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#999999")

custom.col <- c("#FFDB6D", "#C4961A", "#F4EDCA", 
                "#D16103", "#C3D7A4", "#52854C", "#4E84C4", "#293352")

#### Graph of Weights from Sweep Net by Grazing treatment ####
ggplot(Weight_by_Grazing_S,aes(x=Grazing_Treatment,y=Average_Weight, fill=Correct_Order, position="stack"))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge), and fill in the bars with the color grey.  
  geom_bar(stat="identity")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  #Label the x-axis "Treatment"
  xlab("Grazing Treatment")+
  #Label the y-axis "Species Richness"
  ylab("Average Weight (g)")+
  scale_fill_manual(values=custom.col, labels=c("Araneae","Coleoptera","Diptera","Hemiptera","Hymenoptera","Lygaeidae","Neuroptera","Orthoptera"))+
  scale_x_discrete(labels=c("2"="High Graznig","0"="No Grazing","1"="Low Grazing"))+
  theme(legend.key = element_rect(size=4), legend.key.size = unit(1,"centimeters"))+
  #Make the y-axis extend to 50
  expand_limits(y=6)
#Save at the graph at 1400x1500

#### Graph of Weights from Sweep Net by Grazing treatment - NO GRASSHOPPERS ####
ggplot(subset(Weight_by_Grazing_S,Correct_Order!="Orthoptera"),aes(x=Grazing_Treatment,y=Average_Weight, fill=Correct_Order, position="stack"))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge), and fill in the bars with the color grey.  
  geom_bar(stat="identity",color="black")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  #Label the x-axis "Treatment"
  xlab("Grazing Treatment")+
  #Label the y-axis "Species Richness"
  ylab("Average Weight (g)")+
  scale_fill_manual(values=custom.col, labels=c("Araneae","Coleoptera","Diptera","Hemiptera","Hymenoptera","Lygaeidae","Neuroptera"))+
  scale_x_discrete(labels=c("2"="High Graznig","1"="Low Grazing","0"="No Grazing"))+
  theme(legend.key = element_rect(size=4), legend.key.size = unit(1,"centimeters"))+
  #Make the y-axis extend to 50
  expand_limits(y=0.2)
#Save at the graph at 1400x1500


#### Graph of Weights from D-vac by Grazing treatment ####
ggplot(Weight_by_Grazing_D,aes(x=Grazing_Treatment,y=Average_Weight, fill=Correct_Order, position="stack"))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge), and fill in the bars with the color grey.  
  geom_bar(stat="identity",color="black")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  #Label the x-axis "Treatment"
  xlab("Grazing Treatment")+
  #Label the y-axis "Species Richness"
  ylab("Average Weight (g)")+
  scale_fill_manual(values=custom.col, labels=c("Araneae","Coleoptera","Diptera","Hemiptera","Hymenoptera","Orthoptera"))+
  scale_x_discrete(labels=c("2"="High Graznig","1"="Low Grazing","0"="No Grazing"))+
  theme(legend.key = element_rect(size=4), legend.key.size = unit(1,"centimeters"))+
  #Make the y-axis extend to 50
  expand_limits(y=0.3)
#Save at the graph at 1400x1500

#### Graph of Weights from D-vac by Grazing treatment - NO orthoptera ####
ggplot(subset(Weight_by_Grazing_D,Correct_Order!="Orthoptera"),aes(x=Grazing_Treatment,y=Average_Weight, fill=Correct_Order, position="stack"))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge), and fill in the bars with the color grey.  
  geom_bar(stat="identity",color="black")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  #Label the x-axis "Treatment"
  xlab("Grazing Treatment")+
  #Label the y-axis "Species Richness"
  ylab("Average Weight (g)")+
  scale_fill_manual(values=custom.col, labels=c("Araneae","Coleoptera","Diptera","Hemiptera","Hymenoptera"))+
  scale_x_discrete(labels=c("2"="High Graznig","1"="Low Grazing","0"="No Grazing"))+
  theme(legend.key = element_rect(size=4), legend.key.size = unit(1,"centimeters"))+
  #Make the y-axis extend to 50
  expand_limits(y=0.020)
#Save at the graph at 1400x1500

#### Changes in Orthoptera genra by grazing treatment - Sweep net####
## check datasheets for why there are NAs in following dataframe - don't see NAs now
Weight_Orthoptera_S<-Weight_Data %>% 
  filter(Correct_Order=="Orthoptera") %>% 
  filter(Dataset=="S") %>% 
  select(-Date,-Notes) %>% 
  left_join(ID_Data_Correct) %>% 
  filter(Correct_Family=="Acrididae") %>% 
  na.omit(Correct_Genus)

#make dataframe with sum of each genus of orthoptera summed by plot
Weight_Orthoptera_S_Summed <- Weight_Orthoptera_S %>% 
  group_by(Grazing_Treatment,Block, Plot,Correct_Genus) %>% 
  summarise(Genus_Weight=sum(Correct_Dry_Weight_g)) %>% 
  ungroup()

#make table a graph looking at differences in genus weight by grazing treatment
Weight_Orthoptera_Avg<-Weight_Orthoptera_S_Summed %>% 
  group_by(Grazing_Treatment,Correct_Genus) %>% 
  summarise(Average_Weight=mean(Genus_Weight),Weight_SD=sd(Genus_Weight),Weight_n=length(Genus_Weight)) %>% 
  #Make a new column called "Richness_St_Error" and divide "Richness_Std" by the square root of "Richness_n"
  mutate(Weight_St_Error=Weight_SD/sqrt(Weight_n)) %>% 
  ungroup()

####assess differences in orthoptera Genera by grazing treatment using ANOVA####
#### Anova comparing insect weights by grazing treatment for d-vac #### 
Orthoptera_Genera_AOV <- aov(Genus_Weight ~ Grazing_Treatment*Correct_Genus, data = Weight_Orthoptera_S_Summed) 
summary(Orthoptera_Genera_AOV)
model.tables(Orthoptera_Genera_AOV)

#Create a 4-panel plot that contains the following in this order (clockwise from upper left)

plot2<-par(mfrow=c(2,2))


## b. scatterplot of the residuals vs. fitted values    for the model
plot(Orthoptera_Genera_AOV$residuals ~ Orthoptera_Genera_AOV$fitted.values, col = 'dark orange',main="",ylab="AOV Residuals",xlab="AOV Fitted Values",cex.lab=1.5,lwd = 2,cex.axis=1.5)
abline(h = 0, lty = 3)

## c. histogram of the residuals
hist(Orthoptera_Genera_AOV$residuals, col = 'white', border = 'dark orange',main="",ylab="Frequency",xlab="AOV Residuals",cex.lab=1.5,lwd = 2,cex.axis=1.5) 

## d. Q-Q plot
plot(Orthoptera_Genera_AOV, which = 2, col = 'dark orange',main="",cex.lab=1.5,lwd = 2,cex.axis=1.5) 

#### Glmm for Orthoptera Weights by Genera####
Orthoptera_Genera_GLMM <- lmer(Genus_Weight ~ Grazing_Treatment*Correct_Genus + (1 | Block) , data = Weight_Orthoptera_S_Summed)
summary(Orthoptera_Genera_GLMM)
anova(Orthoptera_Genera_GLMM)
  
#graph diference in genus weight by grazing treatment
#### Graph of Weights from D-vac by Grazing treatment - NO orthoptera ####
ggplot(Weight_Orthoptera_Avg,aes(x=Grazing_Treatment,y=Average_Weight, fill=Correct_Genus, position = "stack"))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge), and fill in the bars with the color grey.  
  geom_bar(stat="identity",position = "stack")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  #Label the x-axis "Treatment"
  xlab("Grazing Treatment")+
  #Label the y-axis "Species Richness"
  ylab("Average Weight (g)")+
  geom_errorbar(aes(ymin=Average_Weight-Weight_St_Error,ymax=Average_Weight+Weight_St_Error),position=position_dodge(0.9),width=0.2)+
  scale_fill_manual(values=custom.col, labels=c("Ageneotettix","Amphiturnus","Arphia","Melanoplus","Opeia","Phoetaliotes"))+
  scale_x_discrete(labels=c("2"="High Graznig","1"="Low Grazing","0"="No Grazing"))+
  theme(legend.key = element_rect(size=4), legend.key.size = unit(1,"centimeters"))+
  #Make the y-axis extend to 50
  expand_limits(y=6)
#Save at the graph at 1400x1500


#### Changes in Orthoptera genra by grazing treatment - D-Vac####
## check datasheets for why there are NAs in following dataframe
Weight_Orthoptera_D<-Weight_Data %>% 
  filter(Correct_Order=="Orthoptera") %>% 
  filter(Dataset=="D") %>% 
  select(-Date,-Notes) %>% 
  left_join(ID_Data_Correct) %>% 
  filter(Correct_Family=="Acrididae") %>% 
  na.omit(Correct_Genus)

#make dataframe with sum of each genus of orthoptera summed by plot
Weight_Orthoptera_D_Summed <- Weight_Orthoptera_D %>% 
  group_by(Grazing_Treatment,Block, Plot,Correct_Genus) %>% 
  summarise(Genus_Weight=sum(Correct_Dry_Weight_g)) %>% 
  ungroup()

#make table a graph looking at differences in genus weight by grazing treatment
Weight_Orthoptera_D_Avg<-Weight_Orthoptera_D_Summed %>% 
  group_by(Grazing_Treatment,Correct_Genus) %>% 
  summarise(Average_Weight=mean(Genus_Weight),Weight_SD=sd(Genus_Weight),Weight_n=length(Genus_Weight)) %>% 
  #Make a new column called "Richness_St_Error" and divide "Richness_Std" by the square root of "Richness_n"
  mutate(Weight_St_Error=Weight_SD/sqrt(Weight_n)) %>% 
  ungroup()

####assess differences in orthoptera Genera by grazing treatment using ANOVA - D-vac####
#### Anova comparing insect weights by grazing treatment for d-vac #### 
Orthoptera_Genera_D_AOV <- aov(Genus_Weight ~ Grazing_Treatment*Correct_Genus, data = Weight_Orthoptera_D_Summed) 
summary(Orthoptera_Genera_D_AOV)
model.tables(Orthoptera_Genera_D_AOV)

#Create a 4-panel plot that contains the following in this order (clockwise from upper left)

plot2<-par(mfrow=c(2,2))


## b. scatterplot of the residuals vs. fitted values    for the model
plot(Orthoptera_Genera_D_AOV$residuals ~ Orthoptera_Genera_D_AOV$fitted.values, col = 'dark orange',main="",ylab="AOV Residuals",xlab="AOV Fitted Values",cex.lab=1.5,lwd = 2,cex.axis=1.5)
abline(h = 0, lty = 3)

## c. histogram of the residuals
hist(Orthoptera_Genera_D_AOV$residuals, col = 'white', border = 'dark orange',main="",ylab="Frequency",xlab="AOV Residuals",cex.lab=1.5,lwd = 2,cex.axis=1.5) 

## d. Q-Q plot
plot(Orthoptera_Genera_D_AOV, which = 2, col = 'dark orange',main="",cex.lab=1.5,lwd = 2,cex.axis=1.5) 

#### Glmm for Orthoptera Weights by Genera####
Orthoptera_Genera_D_GLMM <- lmer(Genus_Weight ~ Grazing_Treatment*Correct_Genus + (1 | Block) , data = Weight_Orthoptera_D_Summed)
summary(Orthoptera_Genera_D_GLMM)
anova(Orthoptera_Genera_D_GLMM)

#graph diference in genus weight by grazing treatment
#### Graph of Weights from D-vac by Grazing treatment - NO orthoptera ####
ggplot(Weight_Orthoptera_D_Avg,aes(x=Grazing_Treatment,y=Average_Weight, fill=Correct_Genus, position = "dodge"))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge), and fill in the bars with the color grey.  
  geom_bar(stat="identity",position = "dodge")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  #Label the x-axis "Treatment"
  xlab("Grazing Treatment")+
  #Label the y-axis "Species Richness"
  ylab("Average Weight (g)")+
  geom_errorbar(aes(ymin=Average_Weight-Weight_St_Error,ymax=Average_Weight+Weight_St_Error),position=position_dodge(0.9),width=0.2)+
  scale_fill_manual(values=custom.col, labels=c("Ageneotettix","Amphiturnus","Arphia", "Eritettix","Melanoplus","Opeia","Phoetaliotes"))+
  scale_x_discrete(labels=c("2"="High Graznig","1"="Low Grazing","0"="No Grazing"))+
  theme(legend.key = element_rect(size=4), legend.key.size = unit(1,"centimeters"))+
  #Make the y-axis extend to 50
  expand_limits(y=0.4)
#Save at the graph at 1400x1500

#### Differences in total plot arthropod weight by grazing treatment - SweepNet ####

#make new dataframe to sum total plot weight
Plot_Weight_S<-Weight_Data %>%
  filter(Dataset=="S") %>%
  group_by(Grazing_Treatment,Block,Plot) %>% 
  summarise(Plot_Weight=sum(Correct_Dry_Weight_g)) %>% 
  ungroup()

#make table a graph looking at differences in genus weight by grazing treatment
Plot_Weight_S_Avg<-Plot_Weight_S %>% 
  group_by(Grazing_Treatment) %>% 
  summarise(Average_Weight=mean(Plot_Weight),Weight_SD=sd(Plot_Weight),Weight_n=length(Plot_Weight)) %>% 
  #Make a new column called "Richness_St_Error" and divide "Richness_Std" by the square root of "Richness_n"
  mutate(Weight_St_Error=Weight_SD/sqrt(Weight_n)) %>% 
  ungroup()

####assess differences in plot weight by grazing treatment using ANOVA - SweepNet####
#### Anova comparing insect weights by grazing treatment for sweepnet #### 
Plot_Weight_S_AOV <- aov(Plot_Weight ~ Grazing_Treatment, data = Plot_Weight_S) 
summary(Plot_Weight_S_AOV)
model.tables(Plot_Weight_S_AOV)

#Create a 4-panel plot that contains the following in this order (clockwise from upper left)

plot2<-par(mfrow=c(2,2))


## b. scatterplot of the residuals vs. fitted values    for the model
plot(Plot_Weight_S_AOV$residuals ~ Plot_Weight_S_AOV$fitted.values, col = 'dark orange',main="",ylab="AOV Residuals",xlab="AOV Fitted Values",cex.lab=1.5,lwd = 2,cex.axis=1.5)
abline(h = 0, lty = 3)

## c. histogram of the residuals
hist(Plot_Weight_S_AOV$residuals, col = 'white', border = 'dark orange',main="",ylab="Frequency",xlab="AOV Residuals",cex.lab=1.5,lwd = 2,cex.axis=1.5) 

## d. Q-Q plot
plot(Plot_Weight_S_AOV, which = 2, col = 'dark orange',main="",cex.lab=1.5,lwd = 2,cex.axis=1.5) 

#### Glmm for Plot Weights by Grazing Treatment####
Plot_Weight_S_GLMM <- lmer(Plot_Weight ~ Grazing_Treatment + (1 | Block) , data = Plot_Weight_S)
summary(Plot_Weight_S_GLMM)
anova(Plot_Weight_S_GLMM)

#graph diference inplot weight by grazing treatment
#### Graph of Weights from sweepnet by Grazing treatment  ####
ggplot(Plot_Weight_S_Avg,aes(x=Grazing_Treatment,y=Average_Weight, position = "dodge",fill=Grazing_Treatment))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge), and fill in the bars with the color grey.  
  geom_bar(stat="identity",position = "dodge")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  #Label the x-axis "Treatment"
  xlab("Grazing Treatment")+
  #Label the y-axis "Species Richness"
  ylab("Average Weight (g)")+
  geom_errorbar(aes(ymin=Average_Weight-Weight_St_Error,ymax=Average_Weight+Weight_St_Error),position=position_dodge(0.9),width=0.2)+
  scale_x_discrete(labels=c("2"="High Graznig","1"="Low Grazing","0"="No Grazing"))+
  theme(legend.key = element_rect(size=4), legend.key.size = unit(1,"centimeters"))+
  #Make the y-axis extend to 50
  expand_limits(y=8)
#Save at the graph at 1400x1500

#### Differences in total plot arthropod weight by grazing treatment - D-Vac ####

#make new dataframe to sum total plot weight
Plot_Weight_D<-Weight_Data %>%
  filter(Dataset=="D") %>%
  #check why plot is NA for one entry
  drop_na(Correct_Dry_Weight_g,Plot) %>% 
  group_by(Grazing_Treatment,Block,Plot) %>% 
  summarise(Plot_Weight=sum(Correct_Dry_Weight_g)) %>% 
  ungroup()

#make table a graph looking at differences in genus weight by grazing treatment
Plot_Weight_D_Avg<-Plot_Weight_D %>% 
  group_by(Grazing_Treatment) %>% 
  summarise(Average_Weight=mean(Plot_Weight),Weight_SD=sd(Plot_Weight),Weight_n=length(Plot_Weight)) %>% 
  #Make a new column called "Richness_St_Error" and divide "Richness_Std" by the square root of "Richness_n"
  mutate(Weight_St_Error=Weight_SD/sqrt(Weight_n)) %>% 
  ungroup()

####assess differences in plot weight by grazing treatment using ANOVA - D-Vac####
#### Anova comparing insect weights by grazing treatment for d-vac #### 
Plot_Weight_D_AOV <- aov(Plot_Weight ~ Grazing_Treatment, data = Plot_Weight_D) 
summary(Plot_Weight_D_AOV)
model.tables(Plot_Weight_D_AOV)

#Create a 4-panel plot that contains the following in this order (clockwise from upper left)

plot2<-par(mfrow=c(2,2))


## b. scatterplot of the residuals vs. fitted values    for the model
plot(Plot_Weight_D_AOV$residuals ~ Plot_Weight_D_AOV$fitted.values, col = 'dark orange',main="",ylab="AOV Residuals",xlab="AOV Fitted Values",cex.lab=1.5,lwd = 2,cex.axis=1.5)
abline(h = 0, lty = 3)

## c. histogram of the residuals
hist(Plot_Weight_D_AOV$residuals, col = 'white', border = 'dark orange',main="",ylab="Frequency",xlab="AOV Residuals",cex.lab=1.5,lwd = 2,cex.axis=1.5) 

## d. Q-Q plot
plot(Plot_Weight_D_AOV, which = 2, col = 'dark orange',main="",cex.lab=1.5,lwd = 2,cex.axis=1.5) 

#### Glmm for Plot Weights by Grazing Treatment####
Plot_Weight_D_GLMM <- lmer(Plot_Weight ~ Grazing_Treatment + (1 | Block) , data = Plot_Weight_D)
summary(Plot_Weight_D_GLMM)
anova(Plot_Weight_D_GLMM)

#graph diference inplot weight by grazing treatment
#### Graph of Weights from sweepnet by Grazing treatment  ####
ggplot(Plot_Weight_D_Avg,aes(x=Grazing_Treatment,y=Average_Weight, position = "dodge",fill=Grazing_Treatment))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge), and fill in the bars with the color grey.  
  geom_bar(stat="identity",position = "dodge")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  #Label the x-axis "Treatment"
  xlab("Grazing Treatment")+
  #Label the y-axis "Species Richness"
  ylab("Average Plot Weight (g)")+
  geom_errorbar(aes(ymin=Average_Weight-Weight_St_Error,ymax=Average_Weight+Weight_St_Error),position=position_dodge(0.9),width=0.2)+
  scale_x_discrete(labels=c("2"="High Graznig","1"="Low Grazing","0"="No Grazing"))+
  theme(legend.key = element_rect(size=4), legend.key.size = unit(1,"centimeters"))+
  #Make the y-axis extend to 50
  expand_limits(y=0.5)
#Save at the graph at 1400x1500
  