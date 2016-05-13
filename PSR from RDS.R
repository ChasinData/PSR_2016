
# remove all R objects in the working directory
rm(list=ls(all=TRUE))

# id packages
packages <- c("readxl", "zoo", "RCurl","lubridate" ,"stringi", "dplyr","tidyr","stringr", "eeptools","bizdays")

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

load(file.path(data.path, "PSRPrelim.RData"))
save(file.path(data.path, "PSRPrelim.RData"))

x3$Event.MON=as.character(x3$Event.MON)
#x3=filter(x3, x31$Event.MON=="2015-12")  #This is crashing system... why?
x3<-x3[x3$Event.MON=="2015-12", ]


#### Further Calcs
#Total Events (GCount of Unique PSR + DATIX)
tmp=select(x3, Parent.MTF,Unique.ID, PSR.,Number.of.times.occurred) %>%
  group_by(Unique.ID) %>%
  summarise(Unique.ID.Count = length(PSR.))
x3=merge(x3, tmp, all=T, incomparables = NA)
#Sum of event occurrences (based on ID alone)
tmp=select(x3, ID, PSR.) %>%
  group_by(PSR.) %>%
  summarise(Event.Occurrence = length(ID))
x3=merge(x3, tmp, all=T)

############################# Graph Prep
x3$Short = as.factor(x3$Short); x3$Parent.MTF = as.factor(x3$Parent.MTF);x3$Number.of.times.occurred=as.numeric(x3$Number.of.times.occurred)

harm = c('None','Near','Mild','Unsafe','Not Labeled','Moderate','Severe')
#harm=as.list(unique(x3$Degree.of.harm))

###UPDATE FEEDBACK
#All Harm is a faceted chart and update the Event Ratio
x3.sum = select(x3, everything()) %>%
    filter(Degree.of.harm== "Mild" | 
             Degree.of.harm== "Moderate" |
             Degree.of.harm== "Severe" )%>%
    group_by(Short,Degree.of.harm,Event.MON) %>%
    summarise(Total.Events.ID = sum(Number.of.times.occurred), 
              TotalENCTRS=mean(TotalENCTRS),
              TotalBED.DAYS=mean(Bed.Days))
              
x3.sum$Ratio.EE=round(x3.sum$Total.Events.ID/(x3.sum$TotalENCTRS/1000),2)
x3.sum$Ratio.EBD=round(x3.sum$Total.Events.ID/x3.sum$TotalBED.DAYS,2)

rat.sum=x3.sum[complete.cases(x3.sum), ] 

#Events per 1000 Encounters, by harm
  nam=paste("Events per 1000 Encounters, by harm.png", sep = "")
  tit=paste("Harm Events (mild, moderate, severe) per 1000 Encounters", sep="")
  meancalc=format(round(mean(rat.sum$Ratio.EE),2))
  rat.sum$Short<-reorder(rat.sum$Short, rat.sum$Ratio.EE)
  ggplot(rat.sum, aes(Short,Ratio.EE, fill=Degree.of.harm)) + geom_bar(stat='identity')+
    #geom_text(aes(label = Value, y = Value*1.05), size = 3) +
    facet_grid(Degree.of.harm ~ . , scales = 'free') + #scales = 'free'
    ggtitle(tit) +
    xlab("MTF ") +
    ylab("Harm Rate (Events per 1000 Encounters)") +
    geom_text(aes(label = Ratio.EE, y = Ratio.EE *1.051), size = 3) +
    #geom_hline(aes(yintercept = as.numeric(format(round(mean(rat.sum$ratio),2))))) +
    #annotate("text", min(as.numeric(rat.sum$ratio))+2, as.numeric(meancalc) *1.1, label = paste("Event Rate per 1000 Encounters is ",meancalc,sep = "")) +
    theme(legend.position = "left", axis.text.x=element_text(angle=20, vjust = 1,hjust=1)) #legend could be bottomt 
  ggsave(file.path(out_path,nam),width=12)
  
#All Harm is a faceted chart and by Bed.Days
  nam=paste("Events per 1000 Bed Days, by harm.png", sep = "")
  tit=paste("Harm Events (mild, moderate, severe) per 1000 Bed Days", sep="")
  rat.sum$Short<-reorder(rat.sum$Short, rat.sum$Ratio.EBD)
  ggplot(rat.sum, aes(Short,Ratio.EBD, fill=Degree.of.harm)) + geom_bar(stat='identity')+
    #geom_text(aes(label = Value, y = Value*1.05), size = 3) +
    facet_grid(Degree.of.harm ~ . , scales = 'free') + #scales = 'free'
    ggtitle(tit) +
    xlab("MTF ") +
    ylab("Harm Rate (Events per 1000 Bed Days)") +
    geom_text(aes(label = Ratio.EBD, y = Ratio.EBD *1.051), size = 3) +
    #geom_hline(aes(yintercept = as.numeric(format(round(mean(rat.sum$ratio),2))))) +
    #annotate("text", min(as.numeric(rat.sum$ratio))+2, as.numeric(meancalc) *1.1, label = paste("Event Rate per 1000 Encounters is ",meancalc,sep = "")) +
    annotate("text", min(as.numeric(rat.sum$Ratio.EBD))+2, as.numeric(meancalc) *1.1, label = "Assumption, bed day data was in 1000's") +
    theme(legend.position = "left", axis.text.x=element_text(angle=20, vjust = 1,hjust=1)) #legend could be bottomt 
  ggsave(file.path(out_path,nam),width=12)
  
#All Harm  divided by Near Misses
  tmp.harm = select(x3, everything()) %>%
    filter(Degree.of.harm== "Mild" | 
             Degree.of.harm== "Moderate" |
             Degree.of.harm== "Severe" )%>%
    group_by(Short) %>%
    summarise(Total.Harm.Events = sum(Number.of.times.occurred), 
              TotalENCTRS=mean(TotalENCTRS),
              TotalBED.DAYS=mean(Bed.Days))
  
  tmp.nearMISS = select(x3, everything()) %>%
    filter(Degree.of.harm== "Near" )%>%
    group_by(Short) %>%
    summarise(Total.NearMISS.Events = sum(Number.of.times.occurred), 
              TotalENCTRS=mean(TotalENCTRS),
              TotalBED.DAYS=mean(Bed.Days))
  
  x3.sum=merge(tmp.harm, tmp.nearMISS)
  
  x3.sum$ratio=round(x3.sum$Total.Harm.Events/(x3.sum$Total.NearMISS.Events),2)
  
  rat.sum=x3.sum[complete.cases(x3.sum), ] 

  nam=paste("Harm Events by Near Miss.png", sep = "")
  tit=paste("Percent Harm events for Near Miss events", sep="")
  meancalc=format(round(mean(rat.sum$ratio),2))
  rat.sum$Short<-reorder(rat.sum$Short, rat.sum$ratio)
  ggplot(rat.sum, aes(Short,ratio, fill=Short)) + geom_bar(stat='identity')+
    #geom_text(aes(label = Value, y = Value*1.05), size = 3) +
    #facet_grid(Degree.of.harm ~ . , scales = 'free') + #scales = 'free'
    ggtitle(tit) +
    xlab("MTF ") +
    ylab("% Harm for each Near Miss") +
    #geom_text(aes(label = ratio, y = ratio *1.051), size = 3) +
    #geom_hline(aes(yintercept = as.numeric(format(round(mean(rat.sum$ratio),2))))) +
    #annotate("text", min(as.numeric(rat.sum$ratio))+2, as.numeric(meancalc) *1.1, label = paste("Event Rate per 1000 Encounters is ",meancalc,sep = "")) +
    theme(legend.position = "left", axis.text.x=element_text(angle=20, vjust = 1,hjust=1)) #legend could be bottomt 
  ggsave(file.path(out_path,nam),width=12)
  
#All Events  divided by Near Misses
  tmp = select(x3, everything()) %>%
    group_by(Short) %>%
    summarise(Total.Events = sum(Number.of.times.occurred), 
              TotalENCTRS=mean(TotalENCTRS),
              TotalBED.DAYS=mean(Bed.Days))
  
  tmp.nearMISS = select(x3, everything()) %>%
    filter(Degree.of.harm== "Near" )%>%
    group_by(Short) %>%
    summarise(Total.NearMISS.Events = sum(Number.of.times.occurred), 
              TotalENCTRS=mean(TotalENCTRS),
              TotalBED.DAYS=mean(Bed.Days))
  
  x3.sum=merge(tmp, tmp.nearMISS)
  
  x3.sum$ratio=round(x3.sum$Total.NearMISS.Events/x3.sum$Total.Events,2)
  
  rat.sum=x3.sum[complete.cases(x3.sum), ] 
  
  nam=paste("Total Events by Near Miss.png", sep = "")
  tit=paste("Percent events for Near Miss events", sep="")
  meancalc=format(round(mean(rat.sum$ratio),2))
  rat.sum$Short<-reorder(rat.sum$Short, rat.sum$ratio)
  ggplot(rat.sum, aes(Short,ratio, fill=Short)) + geom_bar(stat='identity')+
    #geom_text(aes(label = Value, y = Value*1.05), size = 3) +
    #facet_grid(Degree.of.harm ~ . , scales = 'free') + #scales = 'free'
    ggtitle(tit) +
    xlab("MTF ") +
    ylab("% Events for each Near Miss") +
    geom_text(aes(label = ratio, y = ratio *1.151), size = 3) +
    geom_hline(aes(yintercept = .5)) +
    annotate("text", min(as.numeric(rat.sum$ratio))+2, as.numeric(meancalc) *1.1, label = paste("Near Miss Events/All Events")) +
    theme(legend.position = "left", axis.text.x=element_text(angle=20, vjust = 1,hjust=1)) #legend could be bottomt 
  ggsave(file.path(out_path,nam),width=12)
  
  
#All Events  that are NOT LABELED
  tmp = select(x3, everything()) %>%
    tmp.harm = select(x3, everything()) %>%
    filter(Degree.of.harm== "Not Labeled") %>%
    group_by(Short) %>%
    summarise(Number.PSRDatix = count(PSR.),
              Total.Events = sum(Number.of.times.occurred))
  
  tmp.2 = select(x3, everything()) %>%
    group_by(Short) %>%
    summarise(Total.Events = sum(Number.of.times.occurred))
  
  x3.sum=merge(tmp, tmp.2)
  
  x3.sum$ratio=round(x3.sum$'Number.PSRDatix'/x3.sum$Total.Events,2)
  
  rat.sum=x3.sum[complete.cases(x3.sum), ] 
  
  nam=paste(" Percent Total Events Not Labeled in Degree of Harm.png", sep = "")
  tit=paste("Total Events Not Labeled in Degree of Harm", sep="")
  meancalc=format(round(mean(rat.sum$ratio),2))
  rat.sum$Short<-reorder(rat.sum$Short, rat.sum$ratio)
  ggplot(rat.sum, aes(Short,ratio, fill=Short)) + geom_bar(stat='identity')+
    #geom_text(aes(label = Value, y = Value*1.05), size = 3) +
    #facet_grid(Degree.of.harm ~ . , scales = 'free') + #scales = 'free'
    ggtitle(tit) +
    xlab("MTF ") +
    ylab("Percent of Events not labeled") +
    geom_text(aes(label = ratio, y = ratio *1.151), size = 3) +
   # geom_hline(aes(yintercept = .5)) +
    #annotate("text", min(as.numeric(rat.sum$ratio))+2, as.numeric(meancalc) *1.1, label = paste("Near Miss Events/All Events")) +
    theme(legend.position = "left", axis.text.x=element_text(angle=20, vjust = 1,hjust=1)) #legend could be bottomt 
  ggsave(file.path(out_path,nam),width=12)  
  
 
# Count Variant
  nam=paste(" Count of Total Events Not Labeled in Degree of Harm.png", sep = "")
  tit=paste("Total Events Not Labeled in Degree of Harm", sep="")
  meancalc=format(round(mean(rat.sum$'Number.PSR+Datix'),2))
  rat.sum$Short<-reorder(rat.sum$Short, rat.sum$ratio)
  ggplot(rat.sum, aes(Short,Number.PSR+Datix, fill=Short)) + geom_bar(stat='identity')+
    #geom_text(aes(label = Value, y = Value*1.05), size = 3) +
    #facet_grid(Degree.of.harm ~ . , scales = 'free') + #scales = 'free'
    ggtitle(tit) +
    xlab("MTF ") +
    ylab("Number of Events not labeled") +
    geom_text(aes(label = 'Number.PSR+Datix', y = ratio *1.151), size = 3) +
    # geom_hline(aes(yintercept = .5)) +
    #annotate("text", min(as.numeric(rat.sum$ratio))+2, as.numeric(meancalc) *1.1, label = paste("Near Miss Events/All Events")) +
    theme(legend.position = "left", axis.text.x=element_text(angle=20, vjust = 1,hjust=1)) #legend could be bottomt 
  ggsave(file.path(out_path,nam),width=12) 
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #All Events chart and by Bed.Days
  x3.sum = select(x3, everything()) %>%
   # filter(Degree.of.harm== "Mild" | 
    #         Degree.of.harm== "Moderate" |
    #         Degree.of.harm== "Severe" )%>%
    group_by(Short,Degree.of.harm,Event.MON) %>%
    summarise(Total.Events.ID = sum(Number.of.times.occurred), Total.Bed.Days=sum(Bed.Days))
  nam=paste("All Events per 1000 Bed Days.png", sep = "")
  tit=paste("All Events per 1000 Bed Days", sep="")
  rat.sum=x3.sum
  rat.sum$ratio=round(rat.sum$Total.Events.ID/(rat.sum$Total.Bed.Days),2)
  rat.sum=rat.sum[complete.cases(rat.sum$Total.Bed.Days), ] 
  meancalc=format(round(mean(rat.sum$ratio),2))
  rat.sum$Short<-reorder(rat.sum$Short, rat.sum$ratio)
  ggplot(rat.sum, aes(Short,ratio, fill=Degree.of.harm)) + geom_bar(stat='identity')+
    #geom_text(aes(label = Value, y = Value*1.05), size = 3) +
    #facet_grid(Degree.of.harm ~ . , scales = 'free') + #scales = 'free'
    ggtitle(tit) +
    xlab("MTF ") +
    ylab("Event Rate (Events per 1000 Bed Days)") +
    #geom_text(aes(label = ratio, y = ratio *1.051), size = 3) +
    #geom_hline(aes(yintercept = as.numeric(format(round(mean(rat.sum$ratio),2))))) +
    #annotate("text", min(as.numeric(rat.sum$ratio))+2, as.numeric(meancalc) *1.1, label = paste("Event Rate per 1000 Encounters is ",meancalc,sep = "")) +
    annotate("text", min(as.numeric(rat.sum$ratio))+2, as.numeric(meancalc) *2.1, label = "Assumption, bed day data was in 1000's") +
    theme(legend.position = "left", axis.text.x=element_text(angle=20, vjust = 1,hjust=1)) #legend could be bottomt 
  ggsave(file.path(out_path,nam),width=12)

  
#Total Harm by Near Misses Ratio  
  x3.sum.harm = select(x3, everything()) %>%
     filter(Degree.of.harm== "Mild" | 
             Degree.of.harm== "Moderate" |
             Degree.of.harm== "Severe" )%>%
    group_by(Short,Degree.of.harm,Event.MON) %>%
    summarise(Total.Harm.Events = sum(Number.of.times.occurred))
  x3.sum.miss = select(x3, everything()) %>%
    filter(Degree.of.harm== "Near" )%>%
    group_by(Short,Degree.of.harm,Event.MON) %>%
    summarise(Total.NearMiss.Events = sum(Number.of.times.occurred))
  x3.sum=merge(x3.sum.miss,x3.sum.harm, by = c("Short","Event.MON"))
  #Total for the same period
  x3.sum2= select(x3, everything()) %>%
    group_by(Short) %>%
    summarise(Harm.Miss.Ratio = sum(Total.Harm.Events)/sum(Total.NearMiss.Events))
  
  x3.sum$Harm.Miss.Ratio=x3$Total.Harm.Events/x3$Total.NearMiss.Events
  rat.sum=rat.sum[complete.cases(rat.sum$Total.Bed.Days), ] 
  
  nam=paste("All Events per 1000 Encounters.png", sep = "")
  tit=paste("All Events per 1000 Encounters", sep="")
  rat.sum=merge(x3.sum,enc.sum)
  rat.sum$ratio=round(rat.sum$Total.Events.ID/(rat.sum$TotalENCTRS/1000),2)
  rat.sum=rat.sum[complete.cases(rat.sum$TotalENCTRS), ] 
  meancalc=format(round(mean(rat.sum$ratio),2))
  rat.sum$Short<-reorder(rat.sum$Short, rat.sum$ratio)
  ggplot(rat.sum, aes(Short,ratio, fill=Degree.of.harm)) + geom_bar(stat='identity')+
    #geom_text(aes(label = Value, y = Value*1.05), size = 3) +
    #facet_grid(Degree.of.harm ~ . , scales = 'free') + #scales = 'free'
    ggtitle(tit) +
    xlab("MTF ") +
    ylab("Event Rate (Events per 1000 Encounters)") +
    #geom_text(aes(label = ratio, y = ratio *1.051), size = 3) +
    #geom_hline(aes(yintercept = as.numeric(format(round(mean(rat.sum$ratio),2))))) +
    #annotate("text", min(as.numeric(rat.sum$ratio))+2, as.numeric(meancalc) *1.1, label = paste("Event Rate per 1000 Encounters is ",meancalc,sep = "")) +
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


