# create latlon dfs

#Submission Lon Lat
SubmitLonLat= cbind(merged$Submission.Longitude, merged$Submission.Latitude)
write.csv(SubmitLonLat,"Submits.csv",row.names=FALSE,row.names=FALSE, na="",options(stringsAsFactors)=FALSE)
Submits = read.table("~/Documents/DataWithoutBorders/FinalProject/Submits.csv", header=TRUE, sep=",")
#head(Submits)


#Interviewer/CKW Lon Lat
CkwLonLat= cbind(merged$Interviewer.Longitude, merged$Interviewer.Latitude)
head(CkwLonLat)
write.csv(CkwLonLat,"CkwLonLat.csv",row.names=FALSE, na="")
CKWLonLat= read.table("~/Documents/DataWithoutBorders/FinalProject/CkwLonLat.csv", header=TRUE, sep=",")
#head(CKWLonLat)

#fix unequal lengths
x= Submits[-66657,]
Submits=x
dim(Submits)

#Geo points
GeoPoints= data.frame(Submits,CKWLonLat)
colnames(GeoPoints)[1]= "SubmitsLon"
colnames(GeoPoints)[2]= "SubmitsLat"
colnames(GeoPoints)[3]= "CKWLon"
colnames(GeoPoints)[4]= "CKWLat"
df= GeoPoints[-1,]
head(df)
GeoPoints=df
head(GeoPoints)

for(i in 1:length(GeoPoints)){
  d=gsub(" ","", GeoPoints[,c(1,2)] , fixed=TRUE)
  
}

geodetic.distance <- function(point1, point2, point3, point4)
{
  R <- 6371
  p1rad <- point1 * pi/180
  p2rad <- point2 * pi/180
  p3rad <- point3 * pi/180
  p4rad <- point3 * pi/180
  d <- sin(p2rad)*sin(p4rad)+cos(p2rad)*cos(p4rad)*cos(abs(p1rad-p3rad))  
  d <- acos(d)
  R*d
}

for(i in 1:length(GeoPoints)){
  Distances=geodetic.distance(GeoPoints$SubmitsLon, GeoPoints$SubmitsLat, GeoPoints$CKWLon, GeoPoints$CKWLat)
}

head(Distances)
head(rev(sort(table(Distances))))
tail(rev(sort(table(Distances))))

opar=par(ps=12)
hist(Distances,breaks=10,main="Distances covered by CKWS", xlab="Distances", col="blue", labels=TRUE)

#add person ids and distances traveled to the df
GeoPoints$PersonId= merged[c(1:66655),2]
GeoPoints$DistanceTraveled= Distances


Top50Travelers= subset(GeoPoints, GeoPoints$DistanceTraveled>5220)
nrow(Top50Travelers)
TopTravelers= subset(GeoPoints, GeoPoints$DistanceTraveled>5245)
nrow(TopTravelers)

#below 5243 kms, 22050 CKW
#Above only 2 people. Plot them

plot(TopTravelers$DistanceTraveled,type="p", main="TOP 2 CKWs", ylab="Distance")
opar=par(ps=18)
text(x,y=NULL,TopTravelers$PersonId, col="red", cex=0.5, adj=2)

#check out details about top person. >8000 is a little unbelievable
check=subset(ckwRank,ckwRank$vid=="Person-000737")
check

#vid                ncontact nunique ngt5 ngt15 ngt25
#Person-000737      588      92   25     9     4

check2= subset(merged, merged$Interviewer..Person.ID=="Person-000737")
#Female from Kapchorwa
#has answered questions for all ten categories
check3= subset(ckw, ckw$Person.ID=="Person-000737")
#Education level -0. Poverty Status- Not Poor


#Plot distances over her activity period
check4=subset(GeoPoints, GeoPoints$PersonId=="Person-000737")
head(check4)

#most likely to have most nunique
ckw1= data.frame(ckw$Person.ID, ckw$Gender, ckw$Poverty.Status)
colnames(ckw1)[1]= "vid"
ckw2= merge(ckw1,ckwRank,by="vid", all.y=TRUE)
head(ckw2)
unique(ckw2$nunique)
nrow(subset(ckw2,ckw2$nunique>100))
#90 ckws with more than 100 unique contacts
hist(ckws100$nunique, main="CKW's with more than 100 unique contacts",xlab="Unique Contacts", col="blue", border="blue", label=T)
hist(ckw2$nunique, main="CKWs unique contacts", xlab="Unique Contacts", col="blue", border="blue", label=T)

#add column that has numbers for Gender
ckws100$fGender= as.numeric(ckws100$ckw.Gender)

head(ckws100$fGender)
nrow(subset(ckws100,ckws100$ckw.Gender=="Male")) #57
nrow(subset(ckws100,ckws100$ckw.Gender=="Female")) #33

