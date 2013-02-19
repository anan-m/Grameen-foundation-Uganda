#final project

#getting familiar. counting things
#Load data - ckw, search logs
#CKW dates
#8 October 2010- 22 August 2011
unique(ckw$Person.ID)
#894
unique(ckw$CKW.ID)
#892

unique(ckw$Gender)
#Female  Male            Unknown
#Levels:  Female Male Unknown

#Number of men
nrow(subset(ckw, ckw$Gender=="Male"))
#[1] 567

#Number of women
nrow(subset(ckw, ckw$Gender=="Female"))
#[1] 299

#Unknown gender
nrow(subset(ckw, ckw$Gender=="Unknown"))

#[1] 1

unique(ckw$District)
#[1] Gulu            Kapchorwa       Mbale           Bushenyi        Oyam           
#[6] Amuru           Nwoya           Kasese          Masindi         Kitgum         
#[11] Mukono          Luwero Nakaseke Pader           Agago   
#15 districts

#district subsets
dist_subs=split(ckw, ckw$District)
head(dist_subs$Gulu)
class(dist_subs)
#list
class(dist_subs$Gulu)
#data.frame

# for loop to get different variables from each district
for(i in 1:length(dist_subs)) {
  dist_males= sum(dist_subs[[i]]$Gender=="Male")
  #print(dist_males)
  
  dist_females= sum(dist_subs[[i]]$Gender=="Female")
  #print(dist_females)
  
  dist_unknown=sum(dist_subs[[i]]$Gender=="Unknown")
  #print(dist_unknown)
 
  
  }
 
#getting to know search_logs
head(unique(search_logs$Interviewee))
#33856 Levels
head(unique(search_logs$Category))
#10 Levels
#[1] Regional Weather Info Market Information    Animals              
#[4] Crops                 Farm Inputs           Content Not Found    
#[7]                       Service Providers     MobileMoney Directory
#[10] Cultures Vivrières   


table(search_logs$Category)

#Animals     Content Not Found 
#37297                 60673                 46304 
#Crops    Cultures Vivrières           Farm Inputs 
#82126                   228                  8770 
#Market Information MobileMoney Directory Regional Weather Info 
#71144                  5572                 18485 
#Service Providers 
#4119 

max(table(search_logs$Category))
#82126 - Crops
min(table(search_logs$Category))
#228- Cultures Vivieres

# search_logs by district
district_logs= split(search_logs, search_logs$Interviewer..District.Name)
head(district_logs$Kitgum)

#Max and min CKWs per district
which.max(table(search_logs$Interviewer..District.Name))
#Kasese 

nrow(subset(search_logs, search_logs$Interviewer..District.Name=="Kasese"))
#69121

which.min(table(search_logs$Interviewer..District.Name))
#Agago 

nrow(subset(search_logs, search_logs$Interviewer..District.Name=="Agago"))
#1493

#Kasese leads with 69121 and Agago comes in last with 1493

head(unique(district_logs$Kasese$Interviewee))
#33856 Levels

#which region's ckw reaches the most people
#note: Interviewee district info is NA

for(i in 1:length(district_logs)){
  people_reached = as.numeric(unique(district_logs[[i]]$Interviewee))
  district_name=(district_logs[[i]]$Interviewer..District.Name)
  print(district_name[i])
  print(people_reached[i])
  }

#Top 6 interviewees
head(rev(sort(table(search_logs$Interviewee))))
#Person-000972  Person-026033  Person-026056 Person-006638 Person-006661 
#49735          7652          2844          1064          1021          1007 

#Interviewees district are not part of the data. it does seem as though the interviewers travel to each of the 15 districts

#get lat lon of interviewer district and add to dataset
latlon <- read.table("~/Documents/DataWithoutBorders/FinalProject/latlon.txt", sep=";", quote="\"")
colnames(latlon)[1]="Interviewer..District.Name"
merged= merge(search_logs, latlon, by="Interviewer..District.Name", all.x= TRUE)
head(merged)
colnames(merged)[12]="Interviewer.Latitude"
colnames(merged)[13]="Interviewer.Longitude"

uganda= readShapePoly('Documents/DataWithoutBorders/FinalProject/UGA_adm/UGA_adm2.shp')
#putting districts on a map
colnames(latlon)[1]="District"
colnames(latlon)[2]= "Latitude"
colnames(latlon)[3]="Longitude"
head(latlon)
latlon= latlon[c("District","Longitude", "Latitude")]
head(latlon)

#make matrices of district lat/lon and submission lat/lon for map
m=matrix(NA,334718,2)
m= cbind(merged$Interviewer.Longitude, merged$Interviewer.Latitude)
#class(m)
#head(m)

x=matrix(NA,334718,2)
x=cbind(merged$Submission.Longitude, merged$Submission.Latitude)
#class(x)
#head(x)

library(maps)
library(geosphere)
#plot map
plot(uganda, col="#f2f2f2",bg="white",lwd=0.01)
title(paste("CKW Districts in Uganda"))
points(latlon$Longitude, latlon$Latitude, pch=16, cex=.5, col="red")
text(latlon$Longitude,latlon$Latitude,latlon$District,cex=.8,adj=0, pos=sample(0:4,1), col="red")

#plot survey zones
plot(uganda, col="#f2f2f2",bg="white",lwd=0.01)
title(paste("CKW Surveys in Uganda"))
points(merged$Submisison.Longitude, merged$Submission.Latitude, pch=16, cex=.5, col="blue")

plot(uganda, col="#f2f2f2",bg="white",lwd=0.01)
title(paste("CKW Interviewer locations in Uganda"))
points(merged$Interviewer.Longitude, merged$Interviewer.Latitude, pch=16, cex=.5, col="red")

