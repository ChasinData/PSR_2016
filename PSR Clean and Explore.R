
# remove all R objects in the working directory
rm(list=ls(all=TRUE))

#Increase memory size (java out of memory and must be before pkg load)
options(java.parameters = "-Xmx8000m")

# id packages
packages <- c("readxl", "zoo", "lubridate" ,"stringi", "dplyr","tidyr","stringr", "eeptools","bizdays")

#install and library
lapply(packages, require, character.only = TRUE)

#set folders
proj_name = "PSR Project"
base_path="U:/NRMC/Data Science"
SP_path="Y:/"


#set project path
proj_path = paste(base_path,"/",proj_name,sep = "")
out_path = file.path(proj_path, "Output")
cleaned_path = file.path(proj_path, "CleanData")
data_path = file.path(proj_path, "RawData")
share_path  = file.path(SP_path, proj_name)

dir.create(file.path(proj_path), showWarnings = FALSE, recursive = FALSE, mode = "0777")
dir.create(file.path(out_path), showWarnings = FALSE, recursive = FALSE, mode = "0777")
dir.create(file.path(cleaned_path), showWarnings = FALSE, recursive = FALSE, mode = "0777")
dir.create(file.path(data_path), showWarnings = FALSE, recursive = FALSE, mode = "0777")
dir.create(file.path(share_path), showWarnings = FALSE, recursive = FALSE, mode = "0777")



Site<-c("GAHC","IRACH","KACC","KACH","KAHC","MCAHC","RHC-A (P) HQ","WAMC","BACH","EAMC","FAHC","LAHC","MFACH","WINN","DAHC","BMACH","ARHC","FTDTL","KUSAHC","ALL")
HRP.name<-c("Ft. Drum","Ft. Knox","Ft. Meade","West Point","Ft. Lee","Ft. Eustis","RHC -A (P) HQ","Ft. Bragg","Ft. Campbell","Ft. Gordon","Redstone","Ft. Rucker","Ft. Jackson","Ft. Stewart","Carlisle Barracks","Ft. Benning","Ft. Myers","FTDTL","Kirk APG","Region (ALL HRPs)")
Facility<-c("CLINIC","HOSP","CLINIC","HOSP","CLINIC","CLINIC","HQ","MEDCEN","HOSP","MEDCEN","CLINIC","CLINIC","HOSP","HOSP","","HOSP","","","","ALL")
UIC<-c("W4U2AA","W2LAAA","W6F2AA","W2H8AA","W2LMAA","W2K1AA","W07TAA","W2L6AA","W2L8AA","W3QMAA","W2FLAA","W2MQAA","W2MJAA","W2MSAA","","W2L3AA","","","","ROLL UP")
Short<-c("DRUM","KNOX","MEADE","WEST POINT","LEE","EUSTIS","RHC-A","BRAGG","CAMPBELL","GORDON","REDSTONE","RUCKER" ,"JACKSON","STEWART","CARLISLE","BENNING","MYERS","FTDTL","KIRK","REGION")
SiteCode<-c("N03","N04","N05","N13","N12","N11","N18","N10","S05","S03","S07","S08","S06","S09","N14", "S04", "N05 / N18", "N17","N06","NN")
#PSR
Parent.MTF <- c("GUTHRIE AHC - FT DRUM", "IRELAND ACH - FT KNOX" ,"KIMBROUGH AHC - FT MEADE" ,"KELLER ACH - WEST POINT" ,
                "KENNER AHC - FT LEE","MCDONALD AHC - FT EUSTIS" ,"RHC-A","WOMACK AMC - FT BRAGG" ,"BLANCHFIELD ACH - FT CAMPBELL" ,
                "EISENHOWER AMC - FT GORDON" ,   "FOX ACH - REDSTONE ARSENAL" ,"LYSTER AHC - FT RUCKER" ,  "MONCRIEF ACH - FT JACKSON" ,         
                "WINN ACH - FT STEWART" ,"KIMBROUGH AHC - FT MEADE"  ,"MARTIN ACH - FT BENNING","KIMBROUGH AHC - FT MEADE" ,"KIMBROUGH AHC - FT MEADE" ,"KIMBROUGH AHC - FT MEADE" ,"REGION" )        
AHC <- c("GUTHRIE", "IRELAND" ,"KIMBROUGH" ,"KELLER" ,"KENNER","MCDONALD" ,"RHC-A","WOMACK" ,"BLANCHFIELD" ,
                "EISENHOWER" ,   "FOX" ,"LYSTER" ,  "MONCRIEF" ,         
                "WINN" ,"KIMBROUGH"  ,"MARTIN","KIMBROUGH" ,"KIMBROUGH" ,"KIMBROUGH" ,"REGION" )        
#Below for Encounters
DMISPARENT <- c("0330","0061","0069","0086","0122","0121","RHC-A","0089","0060","0047","0001","0003","0105","0049","0069","0048","0069","0069","0069","REGION")
Rollup <- as.data.frame(cbind(Site,HRP.name,Facility,UIC,Short,SiteCode,Parent.MTF,DMISPARENT,AHC))
Rollup <-Rollup[-c(3,7,17:20), ]

#Change Col Type
for (i in ncol(Rollup)) {
  Rollup[ ,i]<-as.factor(Rollup[ ,i])
}

match.df=Rollup[ ,c(5,7,8)]

#i1=file.path(data_path, "Copy of RHA-A Jan14-18Dec15.xlsx")
#i2=file.path(data_path, "2nd PSR Pull 2016.csv")
i3=file.path(data_path, "RHC-A.csv")
#Get Date
date <- Sys.Date()

#x1=read_excel(i1, sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 0)
#x2=read.csv(i2, header = TRUE, sep = ",")
x3=read.csv(i3, header = TRUE, sep = ",", na.strings = "",colClasses = "character")

#ID Date Cols
dates<-c(13,15:18,43)
#Change Col Type
for (i in dates) {
  x3[ ,i]<-as.POSIXct(x3[ ,i],format = '%m/%d/%Y', tz ="GMT")
}

complete.dates = x3[ ,c(1,2,dates)];complete.dates = complete.dates[complete.cases(complete.dates), ]

remov.x<-c("Date of manuf.","Date put in service","Last updated","SIRS initial export date")

#Calcs... first var is latter date
x3$Event2Opened<-as.numeric(difftime(x3$`Opened.date`,x3$`Event.date`, units = "days"))
x3$Event2Reported<-as.numeric(difftime(x3$`Reported.date`, x3$`Event.date`,units = "days"))
x3$Reported2Opened<-as.numeric(difftime(x3$`Opened.date`,x3$`Reported.date`, units = "days"))
x3$Event2Closed<-as.numeric(difftime(x3$`Event.Closed.date`, x3$`Event.date`,units = "days"))
x3$Reported2Closed<-as.numeric(difftime( x3$`Event.Closed.date`,x3$`Reported.date`,units = "days"))
x3$Opened2Closed<-as.numeric(difftime( x3$`Event.Closed.date`,x3$`Opened.date`,units = "days"))
x3$Unique.ID <- as.factor(paste(x3$PSR.,x3$ID, sep="_"))
#x3$Event.QTR = paste(year(x3$Event.date),"-","Q",quarter(x3$Event.date),sep = "")
x3$Event.MON = as.factor(paste(year(x3$Event.date),"-",month(x3$Event.date),sep = ""))

#Determine if needs to be opened: Events awaiting review
x3$Days.Since.Reported<-ifelse(x3$Approval.status=="Events awaiting review", as.numeric(difftime( date,x3$`Reported.date`,units = "days")),NA)

#Events that need to be wrapped up (Opened)
x3$Days.Since.Opened=ifelse(x3$Approval.status=="Events still being reviewed", as.numeric(difftime( date,x3$`Opened.date`,units = "days")),NA)

library(dplyr)
##########################################################

#Total Events
tmp=select(x3, Parent.MTF,Unique.ID, PSR.,Number.of.times.occurred) %>%
  group_by(Unique.ID) %>%
  summarise(Unique.ID.Count = length(PSR.))

x3=merge(x3, tmp, all=T, incomparables = NA)

tmp=select(x3, ID, PSR.) %>%
  group_by(PSR.) %>%
  summarise(Event.Occurrence = length(ID))

x3=merge(x3, tmp, all=T)
x3<- merge(x3,match.df, incomparables = NA)

x3$Degree.of.harm<-replace(x3$Degree.of.harm, is.na(x3$Degree.of.harm), "Not Labeled") 

x3$Degree.of.harm<-gsub("Unsafe Condition - potential event", "Unsafe",x3$Degree.of.harm)
x3$Degree.of.harm<-gsub("No Harm", "None",x3$Degree.of.harm)
x3$Degree.of.harm<-gsub("Near Miss - did not reach patient", "Near",x3$Degree.of.harm)
x3$Degree.of.harm<-gsub("Mild Harm", "Mild",x3$Degree.of.harm)
x3$Degree.of.harm<-gsub("Moderate Harm", "Moderate",x3$Degree.of.harm)
x3$Degree.of.harm<-gsub("Severe Harm", "Severe",x3$Degree.of.harm)
x3$Degree.of.harm<-gsub("Emotional Distress or Inconvenience (Inactive)", "Emotional",x3$Degree.of.harm)

x3$Approval.status<-gsub("Events with final approval status", "Final",x3$Approval.status)
x3$Approval.status<-gsub("Rejected events", "Rejected",x3$Approval.status)
x3$Approval.status<-gsub("Events still being reviewed", "In Review",x3$Approval.status)
x3$Approval.status<-gsub("Events awaiting review", "Awaiting Review",x3$Approval.status)
x3$Approval.status<-gsub("Events awaiting final approval status", "Pending Approval",x3$Approval.status)

#############################
#Total Encounters
enc=read_excel(file.path(data_path,"Encounters PSR Project.xlsx"), sheet = 2, col_names = TRUE, col_types = NULL, na = "", skip = 0)
#enc$Event.QTR = paste(year(enc$`Month/Year`),"-","Q",quarter(enc$`Month/Year`),sep = "")
enc$Event.MON = paste(year(enc$`Month/Year`),"-",month(enc$`Month/Year`),sep = "")
enc<- merge(enc,match.df, incomparables = NA)

enc.sum=select(enc, everything()) %>%
  group_by(Short,Parent.MTF,Event.MON) %>%
  summarise(TotalENCTRS = sum(TotalENCTRS))
#############################
Pt.bed.days<-read_excel(file.path(data_path,'Patient Days Jan 15 - Apr 16.xlsx'), sheet = 1, col_names = F, col_types = NULL, na = "", skip = 4)
colnames(Pt.bed.days)<-c("AHC",'2015-1','2015-2','2015-3','2015-4','2015-5','2015-6','2015-7','2015-8','2015-9','2015-10','2015-11','2015-12','2016-1','2016-2','2016-3','2016-4')
Pt.bed.days$AHC<-toupper(sapply(strsplit(Pt.bed.days$AHC, " "), `[`, 1)) #Remove everything after space

Pt.bed.days<-merge(Pt.bed.days, Rollup)
Pt.bed.days<-Pt.bed.days[-9:-10, -c(1,18:21,23:25) ]

Pt.bed.days<-gather(Pt.bed.days,key=Event.MON, value=Bed.Days, -Short)
#x3=merge(x3, Pt.bed.days)
x3$Short = as.factor(x3$Short); x3$Parent.MTF = as.factor(x3$Parent.MTF);x3$Number.of.times.occurred=as.numeric(x3$Number.of.times.occurred)

harm = c('None','Near','Mild','Unsafe','Not Labeled','Moderate','Severe')
#harm=as.list(unique(x3$Degree.of.harm))
x4=x3[complete.cases(x3$Drug.administered), ] 
###UPDATE FEEDBACK
#All Harm is a faceted chart and update the Event Ratio
x3.sum = select(x3, everything()) %>%
   # filter(Degree.of.harm== "Mild" )%>%
    group_by(Short,Degree.of.harm,Event.MON) %>%
    summarise(Total.Events.ID = sum(Number.of.times.occurred))
  nam=paste("Events per 1000 Encounters, by harm.png", sep = "")
  tit=paste("Events per 1000 Encounters", sep="")
  rat.sum=merge(x3.sum,enc.sum)
  rat.sum$ratio=round(rat.sum$Total.Events.ID/(rat.sum$TotalENCTRS/1000),2)
  rat.sum=rat.sum[complete.cases(rat.sum$TotalENCTRS), ] 
  meancalc=format(round(mean(rat.sum$ratio),2))
  rat.sum$Short<-reorder(rat.sum$Short, rat.sum$ratio)
  ggplot(rat.sum, aes(Short,ratio, fill=Degree.of.harm)) + geom_bar(stat='identity')+
    #geom_text(aes(label = Value, y = Value*1.05), size = 3) +
    facet_grid(Degree.of.harm ~ . , scales = 'free') + #scales = 'free'
    ggtitle(tit) +
    xlab("MTF ") +
    ylab("Harm Rate (Events per 1000 Encounters)") +
    geom_text(aes(label = ratio, y = ratio *1.051), size = 3) +
    geom_hline(aes(yintercept = as.numeric(format(round(mean(rat.sum$ratio),2))))) +
    annotate("text", min(as.numeric(rat.sum$ratio))+2, as.numeric(meancalc) *1.1, label = paste("Event Rate per 1000 Encounters is ",meancalc,sep = "")) +
    theme(legend.position = "left", axis.text.x=element_text(angle=20, vjust = 1,hjust=1)) #legend could be bottomt 
  ggsave(file.path(out_path,nam),width=12)

for (i in harm) {
  i=harm[2]
  x3.sum = select(x3, everything()) %>%
    filter(Degree.of.harm== i)%>%
    group_by(Short,Degree.of.harm,Event.MON) %>%
    summarise(Total.Events.ID = sum(Number.of.times.occurred))
  nam=paste(i, " Events Ratios.png", sep = "")
  tit=paste(i," based Ratio (Total Events/Encounters*.001)", sep="")
  rat.sum=merge(x3.sum,enc.sum)
  rat.sum$ratio=round(rat.sum$Total.Events.ID/(rat.sum$TotalENCTRS/1000),2)
  rat.sum=rat.sum[complete.cases(rat.sum$TotalENCTRS), ] 
  meancalc=format(round(mean(rat.sum$ratio),2))
  rat.sum$Short<-reorder(rat.sum$Short, rat.sum$ratio)
  ggplot(rat.sum, aes(Short,ratio, fill=Degree.of.harm)) + geom_bar(stat='identity')+
    #geom_text(aes(label = Value, y = Value*1.05), size = 3) +
    facet_grid( ~ Degree.of.harm , scales = 'free') + #scales = 'free'
    ggtitle(tit) +
    xlab("MTF ") +
    ylab("Ratio") +
    geom_text(aes(label = ratio, y = ratio +.051), size = 3) +
    geom_hline(aes(yintercept = as.numeric(format(round(mean(rat.sum$ratio),2))))) +
    annotate("text", min(as.numeric(rat.sum$ratio))+2, as.numeric(meancalc) *1.1, label = paste("Event Rate per 1000 Encounters is ",meancalc,sep = "")) +
    theme(legend.position = "bottom", axis.text.x=element_text(angle=20, vjust = 1,hjust=1)) #legend could be bottomt 
  ggsave(file.path(out_path,nam),width=12)
}
#### All Harm Type Chart
i="All Harm Types"
x3.sum = select(x3, everything()) %>%
  #filter(Degree.of.harm== i)%>%
  group_by(Short,Event.MON) %>%
  summarise(Total.Events.ID = length(Number.of.times.occurred))
#nam=paste(i, " Events.png", sep = "")
#tit=paste(i," ", sep="")
nam="All events, prior name All Harm Events.png"
tit="All Events"
rat.sum=merge(x3.sum,enc.sum)
rat.sum$ratio=round(rat.sum$Total.Events.ID/(rat.sum$TotalENCTRS/1000),2)
rat.sum=rat.sum[complete.cases(rat.sum$TotalENCTRS), ] 
meancalc=format(round(mean(rat.sum$ratio),2))
rat.sum$Short<-reorder(rat.sum$Short, rat.sum$ratio)
ggplot(rat.sum, aes(Short,ratio,fill = Short)) + geom_bar(stat='identity')+
  #geom_text(aes(label = Value, y = Value*1.05), size = 3) +
  #facet_grid(PSR.Date ~ . , scales = 'free') + #scales = 'free'
  ggtitle(tit) +
  xlab("MTF ") +
  ylab("Ratio") +
  geom_text(aes(label = ratio, y = ratio +.051), size = 3) +
  geom_hline(aes(yintercept = as.numeric(format(round(mean(rat.sum$ratio),2))))) +
  annotate("text", min(as.numeric(rat.sum$ratio))+2, as.numeric(meancalc) *1.1, label = paste("Avg. Ratio is ",meancalc,sep = "")) +
  theme(legend.position = "bottom", axis.text.x=element_text(angle=20, vjust = 1,hjust=1)) #legend could be bottomt 
ggsave(file.path(out_path,nam),width=12)

#### Near miss and no harm
i="Near Miss and No Harm"
x3.sum = select(x3, everything()) %>%
  filter(Degree.of.harm== "Near" |Degree.of.harm== "None")%>%
  group_by(Short,Event.MON,Degree.of.harm) %>%
  summarise(Total.Events.ID = sum(Number.of.times.occurred))
#x3.sum$Degree.of.harm=gsub("Near Miss - did not reach patient","Near Miss",x3.sum$Degree.of.harm )
nam=paste(i, " Events.png", sep = "")
tit=paste(i," ", sep="")
rat.sum=merge(x3.sum,enc.sum)
rat.sum$ratio=round(rat.sum$Total.Events.ID/(rat.sum$TotalENCTRS/1000),2)
rat.sum=rat.sum[complete.cases(rat.sum$TotalENCTRS), ] 
meancalc=format(round(mean(rat.sum$ratio),2))
rat.sum$Short<-reorder(rat.sum$Short, rat.sum$ratio)
ggplot(rat.sum, aes(Degree.of.harm,Total.Events.ID,fill = Degree.of.harm)) + geom_bar(stat='identity')+
  #geom_text(aes(label = Value, y = Value*1.05), size = 3) +
  facet_wrap( ~ Short , scales = 'fixed') + #scales = 'free'
  ggtitle(tit) +
  xlab("Harm ") +
  ylab("Number of Times Occured") +
  geom_text(aes(label = Total.Events.ID, y = Total.Events.ID +.051), size = 3) +
  #geom_hline(aes(yintercept = as.numeric(format(round(mean(rat.sum$ratio),2))))) +
  #annotate("text", min(as.numeric(rat.sum$ratio))+2, as.numeric(meancalc) *1.1, label = paste("Avg. Ratio is ",meancalc,sep = "")) +
  theme(legend.position = "bottom", axis.text.x=element_text(angle=20, vjust = 1,hjust=1)) #legend could be bottomt 
ggsave(file.path(out_path,nam),width=12)

#### Time to completion
##############  FIX BLANKS 
x4=x3
is.na(x4$Event2Opened)=0;is.na(x4$Opened2Closed)=0
x3.sum = select(x4, everything()) %>%
  #filter(Degree.of.harm== "Near Miss - did not reach patient" |Degree.of.harm== "No Harm")%>%
  group_by(Short,Event.MON) %>%
  summarise(Opened2Closed = mean(Opened2Closed), Event2Opened = mean(Event2Opened))
nam="Time Data"
tit="Time Data"
rat.sum=merge(x3.sum,enc.sum)
#rat.sum$ratio=round(rat.sum$Total.Events.ID/(rat.sum$TotalENCTRS/1000),2)
rat.sum=rat.sum[complete.cases(rat.sum$TotalENCTRS), ] 
#meancalc=format(round(mean(rat.sum$ratio),2))
#rat.sum$Short<-reorder(rat.sum$Short, rat.sum$ratio)
rat.sum=rat.sum[ ,c(1,3,4)]
rat.sum=gather(rat.sum,key=Time,value=Days,-Short)
ggplot(rat.sum, aes(Time,Days,fill=Time)) + geom_bar(stat='identity')+
  geom_text(aes(label = round(Days,2), y = Days*1.05), size = 3) +
  facet_wrap( ~ Short , scales = 'fixed') + #scales = 'free'
  ggtitle(tit) +
  coord_polar(theta = "y") +    
  xlab("") + ylab("") +
  scale_y_continuous(expand=c(0, 0))+
  theme_minimal() + 
  #theme(legend.position = "none",panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
  #     axis.line = element_blank(),axis.text.y = element_blank(), 
  #    axis.text.x = element_blank(), axis.ticks = element_blank())
  #geom_text(aes(label = Total.Events.ID, y = Total.Events.ID +.051), size = 3) +
  #geom_hline(aes(yintercept = as.numeric(format(round(mean(rat.sum$ratio),2))))) +
  #annotate("text", min(as.numeric(rat.sum$ratio))+2, as.numeric(meancalc) *1.1, label = paste("Avg. Ratio is ",meancalc,sep = "")) +
  theme(legend.position = "bottom", axis.text.x=element_text(angle=20, vjust = 1,hjust=1)) #legend could be bottomt 
ggsave(file.path(out_path,nam),width=12)
###
FIX SAVING OF PLOT ABOVE

####
x3.sum = select(x3, everything()) %>%
  #filter(Degree.of.harm== "Near Miss - did not reach patient" |Degree.of.harm== "No Harm")%>%
  #group_by(Short,Event.MON,Approval.status,Degree.of.harm) %>%
  group_by(Short,Event.MON,Approval.status) %>%
  summarise(Amount = length(Unique.ID))

nam="PSR Review Data"
tit="PSR Review Data (Ratio based)"
rat.sum=merge(x3.sum,enc.sum)
rat.sum$ratio=round(rat.sum$Amount/(rat.sum$TotalENCTRS/1000),2)
rat.sum=rat.sum[complete.cases(rat.sum$TotalENCTRS), ] 
meancalc=format(round(mean(rat.sum$ratio),2))
rat.sum$Short<-reorder(rat.sum$Short, rat.sum$ratio)
#rat.sum=rat.sum[ ,c(1,3,4)]
#rat.sum=gather(rat.sum,key=Time,value=Days,-Short)
ggplot(rat.sum, aes(Short,y=ratio,fill=Short)) + geom_bar(stat='identity')+
  geom_text(aes(label = round(ratio,2), y = ratio*1.05), size = 3) +
  facet_grid(  ~ Approval.status , scales = 'free') + #scales = 'free'
  ggtitle(tit) +
  #coord_polar(theta = "y") +    
  xlab("") + ylab("") +
  #scale_y_continuous(expand=c(0, 0))+
  #  theme_minimal() + 
  #  theme(legend.position = "none",panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
  ##        axis.line = element_blank(),axis.text.y = element_blank(), 
  #        axis.text.x = element_blank(), axis.ticks = element_blank())
  #geom_text(aes(label = ratio, y = ratio +.051), size = 3) +
  #geom_hline(aes(yintercept = as.numeric(format(round(mean(rat.sum$ratio),2))))) +
  #annotate("text", min(as.numeric(rat.sum$ratio))+2, as.numeric(meancalc) *1.1, label = paste("Avg. Ratio is ",meancalc,sep = "")) +
  theme(legend.position = "bottom", axis.text.x=element_text(angle=20, vjust = 1,hjust=1)) #legend could be bottomt 
ggsave(file.path(out_path,nam),width=12)










###################################################################################
#Remove inactives
x4=x3[x3$Degree.of.harm %in% harm, ]
x5=x4[x4$Approval.status %in% 'Events with final approval status', ]
x5$Unique.ID=as.character(x5$Unique.ID)
###
from=unique(x4$Approval.status)
to=c('Final','In.Review','Rejected','Req.Final', 'Await.Review')
map = setNames(to, from)
#x4[] = map[as.matrix(x4)]

x5$Approval.status=gsub("Events with final approval status","Final",x5$Approval.status)
x5.sum = select(x5, everything()) %>%
  #filter(Degree.of.harm== i)%>%
  group_by(Short,Degree.of.harm,Event.MON) %>%
  summarise(Total.Events.ID = sum(Number.of.times.occurred), Uniques=length(unique(Unique.ID)))
rat.sum=merge(x5.sum,enc.sum)
rat.sum$ratio=round(rat.sum$Total.Events.ID/(rat.sum$TotalENCTRS/1000),2)
rat.sum=rat.sum[complete.cases(rat.sum$TotalENCTRS), ] 
meancalc=format(round(mean(rat.sum$ratio),2))
rat.sum$Short<-reorder(rat.sum$Short, rat.sum$ratio)
rat.sum$Degree.of.harm<-factor(rat.sum$Degree.of.harm, 
                               levels=c('No Harm','Unsafe Condition - potential event',
                                        'Near Miss - did not reach patient','Mild Harm','Severe Harm'))

Count.sum=rat.sum
Count.sum=Count.sum[,c(1,3,4,5)]
Count.sum=gather(Count.sum,key=Metric,value=Value, -Short,-Degree.of.harm)
Count.sum$Degree.of.harm<-factor(Count.sum$Degree.of.harm, 
                                 levels=c('No Harm','Unsafe Condition - potential event',
                                          'Near Miss - did not reach patient','Mild Harm','Severe Harm'))


ggplot(Count.sum, aes(Short, Value, fill= Metric)) +  
  geom_bar(stat='identity')  + 
  facet_grid(Metric ~ Degree.of.harm )+
  #coord_polar()+#expand=TRUE) + 
  #scale_y_continuous(expand=c(0.5, 0)) +
  ggtitle("Count of Events Matrix") +
  xlab("") +
  ylab("Count of Events") 

ggplot(rat.sum, aes(Short, fill= Degree.of.harm)) +  
  geom_bar()  + 
  #facet_free(~ Approval.status ,scales = "free")+
  coord_polar()+#expand=TRUE) + 
  scale_y_continuous(expand=c(0.5, 0))


str(rat.sum)
###########PAIRING DF
str(x3)



library(lubridate)
#x.date$PSR.Date.Qtr = quarter(x.date$Event.date)
#x.date$PSR.Date.YR = year(x.date$Event.date)
x3$Event.QTR = paste(year(x3$Event.date),"-","Q",quarter(x3$Event.date),sep = "")
x3$Event.MON = paste(year(x3$Event.date),"-",month(x3$Event.date),sep = "")

num.dups=nrow(x.date)-length(unique(x.date$unique));num.dups
Zeros = filter(x.date, Opened2Closed == "0") 
Zeros =as.list(unique(Zeros$unique))
Events = as.list(unique(x.date$unique))
Ratio = nrows(Zeros)/Events

dupDF2<-select(x.date, everything()) %>%
  group_by(PSR.Date) %>%
  summarize(Events = sum(!is.na(Event.date)), Discovered=sum(!is.na(Discovery.date)), Reported = sum(!is.na(Reported.date)),
            Opened = sum(!is.na(Opened.date)), Closed = sum(!is.na(Event.Closed.date)), Completed = sum(!is.na(Date.completed))) %>%
  gather(key= Date, value = Value, -PSR.Date)



p = ggplot(dupDF2, aes(x=Date, y=Value, fill = Date)) + #fill = PSR.Date
  geom_bar(stat="identity") +
  #geom_text(aes(label = Value, y = Value*1.05), size = 3) +
  #facet_grid(PSR.Date ~ . , scales = 'free') + #scales = 'free'
  ggtitle("How many DATE fields have blanks?") +
  xlab("Date Type in Report ") +
  ylab("Amount of completed fields") +
  theme(legend.position = "bottom", axis.text.x=element_text(angle=20, vjust = 1,hjust=1)) #legend could be bottomt 
ggsave(file.path(proj_path,"PSRs.png"),width=12)

p

p = ggplot(dupDF2, aes(x=Date, y=Value, fill = Date)) + #fill = PSR.Date
  geom_bar(stat="identity") +
  geom_text(aes(label = Value, y = Value*1.05), size = 3) +
  facet_grid(PSR.Date ~ . , scales = 'free') + #scales = 'free'
  ggtitle("How many DATE fields have blanks?") +
  xlab("Date Type in Report ") +
  ylab("Amount of completed fields") +
  theme(legend.position = "bottom", axis.text.x=element_text(angle=20, vjust = 1,hjust=1)) #legend could be bottomt 
ggsave(file.path(proj_path,"PSRs by Qtr.png"),width=12)

p




dupDF<-select(x.date, everything()) %>%
  group_by(PSR.) %>%
  summarize(Num.IDs = length(ID)) %>%
  filter(Num.IDs > 3)

p = ggplot(dupDF, aes(Num.IDs), fill = Num.IDs) + #fill = 
  geom_histogram(binwidth = 1) +
  #geom_text(aes(label = Num.IDs, y = Num.IDs*1.05), size = 3) +
  ggtitle("How many Datix ID does each PSR # have, and at what frequency?") +
  xlab("Amount of DATIX IDs associated with a PSR (>3)") +
  ylab("Frequency")
p
ggsave(file.path(proj_path,"Frequency count of DATIX by PSR.png"),width=12)

x.date2=x.date[-18504, ]
dat <- x.date2[ ,9:13]
summary(dat, na.rm=T)

dat <-gather(dat, key = Lapse, value = Value)#, -dat$PSR.Date, -dat$unique)

means=aggregate(Value ~ Lapse, dat,mean)
median=aggregate(Value ~ Lapse, dat, median,na.action = na.omit)

#Midpoints for stacked bars
#df <- transform(df, mid_y = ave(df$y, df$x, FUN = function(val) cumsum(val) - (0.5 * val)))

#ggplot(data = df, aes(x, y, fill = grp, label = y)) +
#  geom_bar(stat = "identity") +
#  geom_text(aes(y = mid_y))



p = ggplot(dat, aes(x=Lapse, y=Value, fill = Lapse)) + geom_boxplot() + #, fill=cond
  guides(fill=FALSE) + #coord_flip() +
  facet_grid(~ Lapse , scales = 'free') + #
  geom_text(data=means, label = paste("MEAN: ",round(means$Value, 2), " days",sep = ""), y=580 ) + #,position = position_dodge(1))
  geom_text(data=median, label = paste("MEDIAN: ",round(median$Value, 2), " days",sep = ""), y=610 ) + #,position = position_dodge(1))
  xlab("Stages of Time Lapse") +
  ylab("Days to endpoint") +
  ggtitle("Time Calculations on Data Provided")
p
ggsave(file.path(proj_path,"Time Lapse BoxPlots.png"),width=12)


