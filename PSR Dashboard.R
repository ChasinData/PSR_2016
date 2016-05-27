
# remove all R objects in the working directory
rm(list=ls(all=TRUE))

#Increase memory size (java out of memory and must be before pkg load)
options(java.parameters = "-Xmx15000m")

# id packages
packages <- c("plyr", "xlsx","dplyr","readxl", "zoo",  "stringi", "ggplot2","tidyr","stringr", "RCurl")

#install and library
lapply(packages, require, character.only = TRUE)

#set folders
proj_name = "PSR Project"
base_path="U:/NRMC/Data Science"
pmb_data=""
sharepoint='Y:'

#set project path
proj_path = paste(base_path,"/",proj_name,sep = "")
share_path = paste(sharepoint,"/",proj_name,sep = "")

#Other folders needed
out_path = file.path(proj_path, "Output")
cleaned_path = file.path(proj_path, "CleanData")
data_path = file.path(share_path, "RawData")


#make folders
dir.create(file.path(proj_path), showWarnings = FALSE, recursive = FALSE, mode = "0777")
dir.create(file.path(out_path), showWarnings = FALSE, recursive = FALSE, mode = "0777")
dir.create(file.path(cleaned_path), showWarnings = FALSE, recursive = FALSE, mode = "0777")
dir.create(file.path(data_path), showWarnings = FALSE, recursive = FALSE, mode = "0777")
dir.create(file.path(share_path), showWarnings = FALSE, recursive = FALSE, mode = "0777")

#Get Date
date <- Sys.Date()

u <<- "https://raw.githubusercontent.com/ChasinData/Rollup/master/HRP_Builder.R"
script <- getURL(u, ssl.verifypeer = FALSE)
eval(parse(text = script))

#Only need Short Names
match.df=MTF.Only[ ,c(5,7,8,9)]

#Get File names
encounters<-list.files(data_path, pattern="*Encounters*", recursive = T, full.names=T, all.files = T, include.dirs = F)
patient.days<-list.files(data_path, pattern="*Patient Days*", recursive = T, full.names=T, all.files = T, include.dirs = F)
psr.files <- list.files(data_path, pattern="*.csv", recursive = F, full.names=T, all.files = T, include.dirs = T)
file.info<-file.info(psr.files)
psr.files=as.data.frame(cbind(psr.files,file.info), row.names = NULL); 
colnames(psr.files)=c("FullName","Size","Directory?","Permissions","Last Modified","Last Status Change","Last Accessed", "Executable")
psr.files<-psr.files[ ,c(1,5)]
psr.files$Days<-as.numeric(difftime(Sys.Date(),psr.files$'Last Modified',units="days"))

# clean-up
rm(Short.Names,MTF.Only, file.info)

#Find recent PSR File
psr.files <- psr.files[order(-psr.files$Days),] 
i3<-as.character(psr.files[1,1])
#Read PSR Data
x3=read.csv(i3, header = TRUE, sep = ",", na.strings = "",colClasses = "character")

Pt.bed.days<-read_excel(patient.days, sheet = 1, col_names = F, col_types = NULL, na = "", skip = 4)
#Total Encounters
enc=read_excel(encounters, sheet = 2, col_names = TRUE, col_types = NULL, na = "", skip = 0)


####CLEAN PRIMARY PSR DATA (X3)
#ID Date Cols
dates<-c(17,19:24,29)

#Change Col Type
for (i in dates) {
  x3[ ,i]<-as.POSIXct(x3[ ,i],format = '%m/%d/%Y', tz ="GMT")
}
#How many records have all dates completed

complete.dates = x3[ ,c(1,2,dates)];complete.dates = complete.dates[complete.cases(complete.dates), ]
#Calcs... first var is latter date
x3$Event2Opened<-as.numeric(difftime(x3$`Opened.date`,x3$`Event.date`, units = "days"))
x3$Opened2Closed<-as.numeric(difftime( x3$`Event.Closed.date`,x3$`Opened.date`,units = "days"))
x3$Unique.ID <- as.factor(paste(x3$PSR.,x3$ID, sep="_"))

#Harm Rplacements
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
#Determine if needs to be opened: Events awaiting review
x3$Days.Since.Reported<-ifelse(x3$Approval.status=="Awaiting Review", as.numeric(difftime( date,x3$`Reported.date`,units = "days")),NA)
#Events that need to be wrapped up (Opened)
x3$Days.Since.Opened=ifelse(x3$Approval.status=="In Review", as.numeric(difftime( date,x3$`Opened.date`,units = "days")),NA)
#Time Grouping
x3$Time=ifelse(as.numeric(gsub(":","",x3$Event.time)) %in% 0701:1500, "First", 
               ifelse(as.numeric(gsub(":","",x3$Event.time)) %in% 1501:2300, "Second",
                      ifelse(as.numeric(gsub(":","",x3$Event.time)) %in% 2301:2400, "Third", "Third")))

x3<-x3[ ,c(1:3,8:11,39,13,14,18:38)]

## CLEAN Encounter Data
#enc$Event.QTR = paste(year(enc$`Month/Year`),"-","Q",quarter(enc$`Month/Year`),sep = "")
enc$Event.MON = paste(year(enc$`Month/Year`),"-",month(enc$`Month/Year`),sep = "")

#Layer in short names
enc<- merge(enc,match.df, by='DMISPARENT', incomparables = NA)

save(x3, file='PSRPrelimX3.RDS')
save(Pt.bed.days, file='PSRPrelimPT.RDS')
save(enc, file='PSRPrelimENC.RDS')




#Summarize data
enc.sum=select(enc, everything()) %>%
  group_by(Short,Event.MON) %>%
  summarise(TotalENCTRS = sum(TotalENCTRS))
enc.sum=enc.sum[complete.cases(enc.sum$TotalENCTRS), ] 

##Clean Bed Day Data
##########################################################
colnames(Pt.bed.days)<-c("AHC",'2015-1','2015-2','2015-3','2015-4','2015-5','2015-6','2015-7','2015-8','2015-9','2015-10','2015-11','2015-12','2016-1','2016-2','2016-3','2016-4')
Pt.bed.days$AHC<-toupper(sapply(strsplit(Pt.bed.days$AHC, " "), `[`, 1)) #Remove everything after space
Pt.bed.days<-merge(Pt.bed.days, match.df, incomparables = NA)
Pt.bed.days<-Pt.bed.days[-9:-10, -c(1,19:20) ]
Pt.bed.days<-gather(Pt.bed.days,key=Event.MON, value=Bed.Days, -Short)



#Add Bed days back to full data set
x3=merge(x3, match.df, all=T)
x3=merge(x3, Pt.bed.days,  all=T)
x3=merge(x3, enc.sum, all=T)


x3=filter(x3, x3$Event.MON == "2015-12")