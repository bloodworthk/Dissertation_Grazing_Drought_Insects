#### Kaysa URCA Project Code ####

#### Set Working Directory ####

#KB - Mac
setwd("~/Library/CloudStorage/Box-Box/Projects/Dissertation/Data/Insect_Data")

#### load libraries and set graph ####

#Load Tidyverse#
library(tidyverse)

#Set ggplot2 theme to black and white
theme_set(theme_bw())
#Update ggplot2 theme - make box around the x-axis title size 30, vertically justify x-axis title to 0.35, Place a margin of 15 around the x-axis title.  Make the x-axis title size 30. For y-axis title, make the box size 30, put the writing at a 90 degree angle, and vertically justify the title to 0.5.  Add a margin of 15 and make the y-axis text size 25. Make the plot title size 30 and vertically justify it to 2.  Do not add any grid lines.  Do not add a legend title, and make the legend size 20
theme_update(panel.grid.major=element_blank(),
             panel.grid.minor=element_blank())

#### read in data ####

#ID Data
ID_Data_22<-read.csv("2022_Sweep_Net_Data_FK.csv",header=T) 

#weight data
Weight_Data_22<-read.csv("2022_Sweep_Net_Weight_Data_FK.csv",header=T) 

#### check spellings and correct anything wrong ####

#compare unique vales in ID data and Weight Data

unique(sort(ID_Data_22$Block))
unique(sort(Weight_Data_22$Block))

unique(sort(ID_Data_22$Grazing_Treatment))
unique(sort(Weight_Data_22$Grazing_Treatment))

unique(sort(ID_Data_22$Order))
unique(sort(Weight_Data_22$Order))

unique(sort(ID_Data_22$Family))

unique(sort(ID_Data_22$Genus))

unique(sort(ID_Data_22$Species))

#fix data so that all IDs are the same (same capitalization too)

#### Data Analysis ####

#### Look at Total Plot Weight Differences ####

#Summing all weights by order within dataset, grazing treatment, block, and plot so that we can look at differences in order across plots
Weight_Data_Summed<-aggregate(Dry_Weight_g~Coll_Year_Bl_Trt+Plot+Order, data=Weight_Data_22, FUN=sum, na.rm=FALSE) 

#Separating out Treatment_Plot into all distinctions again so that we can group based on different things
Weight_Data_Summed<-Weight_Data_Summed %>% 
  separate(Coll_Year_Bl_Trt, c("Collection_Method","Year","Block","Grazing_Treatment"), "_")

#create dataframe that just has sweepnet samples in it
Weight_Data_Summed_sweep<-Weight_Data_Summed %>% 
  #sum by plot #
  group_by(Year,Block,Grazing_Treatment,Plot) %>% 
  summarise(Plot_Weight=sum(Dry_Weight_g)) %>% 
  ungroup()

### Average Plot Weight across Grazing treatment ####
Weight_by_Grazing_sweep<-Weight_Data_Summed_sweep %>% 
  group_by(Year,Grazing_Treatment) %>% 
  summarise(Average_Weight=mean(Plot_Weight),Weight_SD=sd(Plot_Weight),Weight_n=length(Plot_Weight)) %>% 
  mutate(Weight_St_Error=Weight_SD/sqrt(Weight_n)) 

##reorder bar graphs##
Weight_by_Grazing_sweep$Grazing_Treatment <- factor(Weight_by_Grazing_sweep$Grazing_Treatment, levels = c("NG", "LG", "HG"))

#### Total Plot Weight Differences - Figures ####

#Graph of Weights from Sweep Net by Grazing treatment- 2022
SN_2020_Plot<-ggplot(Weight_by_Grazing_sweep,aes(x=Grazing_Treatment,y=Average_Weight,fill=Grazing_Treatment))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge), and fill in the bars with the color grey.  
  geom_bar(stat="identity",position = "dodge")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(aes(ymin=Average_Weight-Weight_St_Error,ymax=Average_Weight+Weight_St_Error),position=position_dodge(),width=0.2)+
  #Label the x-axis "Treatment"
  xlab("Grazing Treatment")+
  #Label the y-axis "Species Richness"
  ylab("Average Plot Weight (g)")+
  theme(legend.background=element_blank())+
  scale_x_discrete(labels=c("HG"="High Impact Grazing","NG"="Cattle Removal","LG"="Destock"))+
  scale_fill_manual(values=c("thistle2","thistle3","thistle4"), labels=c("High Impact Grazing","Cattle Removal","Destock"))+
  theme(axis.text.x=element_blank(),axis.title.x=element_blank(),legend.position = "none")+
  expand_limits(y=8)+
  scale_y_continuous(labels = label_number(accuracy = 0.01))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45))+
  geom_text(x=1.0, y=8, label="2020 Sweepnet",size=20)
#save at 1600 x 1200

#### Glmm for Plot Weights by Grazing Treatment####

# 2022 Sweep net
Plot_Weight_S_2022_Glmm <- lmer(Plot_Weight ~ Grazing_Treatment + (1 | Block) , data = Weight_Data_Summed_sweep)
anova(Plot_Weight_S_2022_Glmm) #not significant

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

#### Total Plot Weight Differences by Order - Figures ####

#2020 - sweepnet
ggplot(Weight_by_Order_Sweepnet,aes(x=Grazing_Treatment,y=Average_Weight, fill=Correct_Order, position="stack"))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge), and fill in the bars with the color grey.  
  geom_bar(stat="identity")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  #Label the x-axis "Treatment"
  xlab("Grazing Treatment")+
  #Label the y-axis "Species Richness"
  ylab("Average Weight (g)")+
  theme(legend.background=element_blank())+
  scale_fill_manual(values=c("#661100","#CC6677","#DDCC77","#117733","#332288", "#44AA99","#AA4499","#6699CC"), labels=c("Araneae","Coleoptera","Diptera","Hemiptera","Hymenoptera","Neuroptera","Orthoptera"), name = "Order")+
  scale_x_discrete(labels=c("HG"="High Grazing","LG"="Low Grazing","NG"="No Grazing"))+
  theme(legend.key = element_rect(size=3), legend.key.size = unit(1,"centimeters"), legend.position=c(0.18,0.715))+
  #Make the y-axis extend to 50
  expand_limits(y=6)+
  scale_y_continuous(labels = label_number(accuracy = 0.1))+
  theme(text = element_text(size = 55),legend.text=element_text(size=45))+
  geom_text(x=1.3, y=6, label="2022 Sweep Net",size=20)
#no grazing is different than low grazing, low grazing is different than high grazing, no and high grazing are the same
#annotate("text",x=1,y=2.9,label="a",size=20)+ #no grazing
#annotate("text",x=2,y=5.5,label="b",size=20)+ #low grazing
#annotate("text",x=3,y=3.4,label="a",size=20) #high grazing
#Save at the graph at 1400x1400

#### NMDS ####

#Make a new data frame called Wide_Relative_Cover using data from Relative_Cover
Wide_Order_Weight<-Weight_Data_Summed%>%
  filter(Correct_Order!="Unknown_1") %>% 
  filter(Correct_Order!="Unknown") %>% 
  filter(Correct_Order!="unknown") %>% 
  filter(Correct_Order!="Snail") %>% 
  filter(Correct_Order!="Body_Parts") %>%
  filter(Plot!="NA") %>% 
  #Make a wide table using column correct order as overarching columns, fill with values from correct dry weight column, if there is no value for one cell, insert a zero
  spread(key=Correct_Order,value=Dry_Weight_g, fill=0)

#### Make new data frame called BC_Data and run an NMDS 

#sweepnet
BC_Data_S <- metaMDS(Wide_Order_Weight_S[,6:13])
#look at species signiciance driving NMDS 
intrinsics <- envfit(BC_Data_S, Wide_Order_Weight_S, permutations = 999)
head(intrinsics)
#Make a data frame called sites with 1 column and same number of rows that is in Wide Order weight
sites <- 1:nrow(Wide_Order_Weight_S)
#Make a new data table called BC_Meta_Data and use data from Wide_Relative_Cover columns 1-3
BC_Meta_Data_S <- Wide_Order_Weight_S[,1:5] %>% 
  mutate(Trt_Year=paste(Grazing_Treatment,Year,sep="."))
#make a plot using the dataframe BC_Data and the column "points".  Make Grazing Treatment a factor - make the different grazing treatments different colors
plot(BC_Data_S$points,col=as.factor(BC_Meta_Data_S$Trt_Year))
#make elipses using the BC_Data.  Group by grazing treatment and use standard deviation to draw eclipses
ordiellipse(BC_Data_S,groups = as.factor(BC_Meta_Data_S$Trt_Year),kind = "sd",display = "sites", label = T)

#Use the vegan ellipse function to make ellipses           
veganCovEllipse<-function (cov, center = c(0, 0), scale = 1, npoints = 100)
{
  theta <- (0:npoints) * 2 * pi/npoints
  Circle <- cbind(cos(theta), sin(theta))
  t(center + scale * t(Circle %*% chol(cov)))
}
#Make a data frame called BC_NMDS and at a column using the first set of "points" in BC_Data and a column using the second set of points.  Group them by watershed
BC_NMDS_S = data.frame(MDS1 = BC_Data_S$points[,1], MDS2 = BC_Data_S$points[,2],group=BC_Meta_Data_S$Trt_Year)
#Make data table called BC_NMDS_Graph and bind the BC_Meta_Data, and BC_NMDS data together
BC_NMDS_Graph_S <- cbind(BC_Meta_Data_S,BC_NMDS_S)
#Make a data table called BC_Ord_Ellipses using data from BC_Data and watershed information from BC_Meta_Data.  Display sites and find the standard error at a confidence iinterval of 0.95.  Place lables on the graph
BC_Ord_Ellipses_S<-ordiellipse(BC_Data_S, BC_Meta_Data_S$Trt_Year, display = "sites",
                               kind = "se", conf = 0.95, label = T)
#Make a new empty data frame called BC_Ellipses                
BC_Ellipses_S <- data.frame()
#Generate ellipses points - switched levels for unique - not sure if it's stil correct but it looks right
for(g in unique(BC_NMDS_S$group)){
  BC_Ellipses_S <- rbind(BC_Ellipses_S, cbind(as.data.frame(with(BC_NMDS_S[BC_NMDS_S$group==g,],                                                  veganCovEllipse(BC_Ord_Ellipses_S[[g]]$cov,BC_Ord_Ellipses_S[[g]]$center,BC_Ord_Ellipses_S[[g]]$scale)))
                                              ,group=g))
}

#### NMDS Figures ####

#Plot the data from BC_NMDS_Graph, where x=MDS1 and y=MDS2, make an ellipse based on "group"
NMDS_Sweep<-ggplot(data = BC_NMDS_Graph_S, aes(MDS1,MDS2, shape = group,color=group,linetype=group))+
  #make a point graph where the points are size 5.  Color them based on exlosure
  geom_point(size=8, stroke = 2) +
  #Use the data from BC_Ellipses to make ellipses that are size 1 with a solid line
  geom_path(data = BC_Ellipses_S, aes(x=NMDS1, y=NMDS2), size=4)+
  #make shape, color, and linetype in one combined legend instead of three legends
  labs(color  = "", linetype = "", shape = "")+
  # make legend 2 columns
  guides(shape=guide_legend(ncol=2),colour=guide_legend(ncol=2),linetype=guide_legend(ncol=2))+
  #change order of legend
  #Use different shapes 
  scale_shape_manual(values=c(15,16,17,22,21,24),labels = c("Heavy 2020","Destock 2020", "No Grazing 2020","Heavy 2021","Destock 2021", "No Grazing 2021"), breaks = c("HG.2020","LG.2020","NG.2020","HG.2021","LG.2021","NG.2021"),name="")+
  scale_color_manual(values=c("skyblue3","springgreen3","plum3","royalblue4","springgreen4","plum4"),labels = c("Heavy 2020","Destock 2020", "No Grazing 2020","Heavy 2021","Destock 2021", "No Grazing 2021"), breaks = c("HG.2020","LG.2020","NG.2020","HG.2021","LG.2021","NG.2021"),name="")+
  scale_linetype_manual(values=c("solid","twodash","longdash","solid","twodash","longdash"),labels = c("Heavy 2020","Destock 2020", "No Grazing 2020","Heavy 2021","Destock 2021", "No Grazing 2021"), breaks = c("HG.2020","LG.2020","NG.2020","HG.2021","LG.2021","NG.2021"),name="")+
  #make the text size of the legend titles 28
  theme(legend.key = element_rect(size=3), legend.key.size = unit(1,"centimeters"),legend.position="NONE")+
  #Label the x-axis "NMDS1" and the y-axis "NMDS2"
  xlab("NMDS1")+
  ylab("NMDS2")+
  theme(text = element_text(size = 55),legend.text=element_text(size=40))+
  annotate(geom="text", x=-1.5, y=0.8, label="Sweepnet",size=20)
#export at 2000 x 1800

#### PERMANOVA ####

##PerMANOVA

#Sweepnet
#Make a new dataframe with the data from Wide_Relative_Cover all columns after 5
Species_Matrix_S <- Wide_Order_Weight_S[,6:ncol(Wide_Order_Weight_S)]
#Make a new dataframe with data from Wide_Relative_Cover columns 1-3
Environment_Matrix_S <- Wide_Order_Weight_S[,1:5]

Environment_Matrix_S$Grazing_Treatment_Fact=as.factor(Environment_Matrix_S$Grazing_Treatment)
Environment_Matrix_S$Block_Fact=as.numeric(Environment_Matrix_S$Block)
Environment_Matrix_S$Plot_Fact=as.factor(Environment_Matrix_S$Plot)
Environment_Matrix_S$Year_Fact=as.factor(Environment_Matrix_S$Year)

#run a perMANOVA comparing across watershed and exclosure, how does the species composition differ.  Permutation = 999 - run this 999 times and tell us what the preportion of times it was dissimilar
#Adding in the 'strata' function does not affect results - i can't figure out if I am doing in incorrectly or if they do not affect the results (seems unlikely though becuase everything is exactly the same)
PerMANOVA2_S <- adonis2(formula = Species_Matrix_S~Grazing_Treatment_Fact*Year_Fact + (1 | Block_Fact) , data=Environment_Matrix_S,permutations = 999, method = "bray")
#give a print out of the PermMANOVA
print(PerMANOVA2_S) #grazing treatment (p=0.695), year (p=0.0.001), grazing:year (p=0.441)

#### PERMDISP ####

#Make a new dataframe with data from Relative_Cover2
Wide_Order_Weight2_S_2020 <- Weight_Data_Summed%>%
  filter(Plot!="NA") %>% 
  filter(Correct_Order!="Unknown_1") %>% 
  filter(Correct_Order!="Unknown") %>% 
  filter(Correct_Order!="unknown") %>% 
  filter(Correct_Order!="Snail") %>% 
  filter(Correct_Order!="Body_Parts") %>%
  filter(Collection_Method=="sweep") %>% 
  filter(Year=="2020") %>% 
  #Make a qide data frame using "Taxa" as the columns and fill with "Relative_Cover", if there is no data, fill cell with zero
  spread(key = Correct_Order, value = Dry_Weight_g, fill = 0)

#Make a new dataframe with data from Relative_Cover2
Wide_Order_Weight2_S_2021 <- Weight_Data_Summed%>%
  filter(Plot!="NA") %>% 
  filter(Correct_Order!="Unknown_1") %>% 
  filter(Correct_Order!="Unknown") %>% 
  filter(Correct_Order!="unknown") %>% 
  filter(Correct_Order!="Snail") %>% 
  filter(Correct_Order!="Body_Parts") %>%
  filter(Collection_Method=="sweep") %>% 
  filter(Year=="2021") %>% 
  #Make a qide data frame using "Taxa" as the columns and fill with "Relative_Cover", if there is no data, fill cell with zero
  spread(key = Correct_Order, value = Dry_Weight_g, fill = 0)


#Sweepnet
#Make a new dataframe and calculate the dissimilarity of the Species_Matrix dataframe
BC_Distance_Matrix_S_2020 <- vegdist(Species_Matrix_S)
#Run a dissimilarity matrix (PermDisp) comparing grazing treatment
Dispersion_Results_Grazing_S <- betadisper(BC_Distance_Matrix_S_2020,Wide_Order_Weight2_S_2020$Grazing_Treatment)
permutest(Dispersion_Results_Grazing_S_2020,pairwise = T, permutations = 999) #grazing treatments (p=0.95)

#Make a new dataframe and calculate the dissimilarity of the Species_Matrix dataframe
BC_Distance_Matrix_S_2021 <- vegdist(Species_Matrix_S_2021)
#Run a dissimilarity matrix (PermDisp) comparing grazing treatment
Dispersion_Results_Grazing_S_2020 <- betadisper(BC_Distance_Matrix_S_2020,Wide_Order_Weight2_S_2020$Grazing_Treatment)
permutest(Dispersion_Results_Grazing_S_2020,pairwise = T, permutations = 999) #grazing treatments (p=0.95)












