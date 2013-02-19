
#Unique CKW volunteers
ckwPerson=unique(search_logs$Interviewer..Person.ID)
ckwVolunteers=grep("Person", ckwPerson, value = TRUE)

library(plyr)
ckwRank <- NULL;
vid <- NULL; ncontact <- NULL;nunique<-NULL;ngt5<-NULL;ngt15<-NULL;ngt25<-NULL;

#for (i in 1:10) {
for (i in 1:length(ckwVolunteers)) {
tmp<-subset(search_logs,search_logs$Interviewer..Person.ID==ckwVolunteers[i])
  counts <- ddply(tmp, .(tmp$Interviewer..Person.ID, tmp$Interviewee), nrow)
  vid<-c(vid,ckwVolunteers[i])
  ncontact<-c(ncontact,nrow(tmp))
  nunique<-c(nunique,nrow(counts))
  ngt5<-c(ngt5,sum(counts$V1>5))
  ngt15<-c(ngt15,sum(counts$V1>15))
  ngt25<-c(ngt25,sum(counts$V1>25))
}

ckwRank<-data.frame(vid,ncontact,nunique,ngt5,ngt15,ngt25)
barplot(ckwRank$ncontact,main = "Number of Contacts",names.arg = c(vid))
head(rev(sort(table(ckwRank$ncontact))))

#plot ncontact
hist(ckwRank$ncontact, col="blue", border="blue", main= "CKW-Number of Contacts", xlab="Number of Contacts")
#plot ckw with top farmer reach
head(rev(sort(table(ckwRank$ncontact))))
g = subset(ckwRank, ckwRank$ncontact>400)
head(g)
opar=par(ps=12)
plot(g$vid, g$ncontact, type=p,type="P", main="CKWs with top famer reach", xlab="PersonID", ylab="No. of farmers reached")

#CKW with top farmer reach
opar=par(ps=18)
plot(g$ncontact, main="CKWs with top farmer reach", ylab="Number of farmers reached")
x=as.factor(1:6)
y=g$ncontact
text(x,y,g$vid,cex=0.5, col="black", adj=0, pos=4)

#CKW with top unique farmers reached
head(rev(sort(table(ckwRank$nunique))))
ckwUniqueReach= subset(ckwRank, ckwRank$nunique>13)
head(ckwUniqueReach)
plot(ckwUniqueReach$nunique,main="CKWs with top unique farmers reached",ylab="Number of unique farmers reached")
x=as.factor(1:7)
y= ckwUniqueReach$nunique
text(x,y,g$vid,cex=0.5, col="black", adj=0, pos=4)

#CKWs with >5 farmers reached
hist(ckwRank$ngt5,breaks=5, main="CKWs with more than 5 farmers reached", xlab="CKWs", col="blue", border="blue", labels=TRUE)

#CKWs with >15 farmers reached
hist(ckwRank$ngt15,breaks=5, main="CKWs with more than 15 farmers reached", xlab="CKWs", col="blue", border="blue", labels=TRUE)

#CKWs with >25 farmers reached
hist(ckwRank$ngt25,breaks=5, main="CKWs with more than 25 farmers reached", xlab="CKWs", col="blue", border="blue", labels=TRUE)

#Top farmers reached & top unique farmers reached is CKW Person-028134. Get Details.
check3= subset(ckw, ckw$Person.ID=="Person-028134")
head(check3)
#Female from Masindi. Poverty level - unknown 
check4=subset(ckwRank,ckwRank$vid=="Person-028134")
#1272 contacts. 130 unique contacts
