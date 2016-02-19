rm(list=ls())

ripdata <- read.csv("riparian_survey.csv")

#CREATE NEW PROJECT CODE COLUMN
oldvals <- levels(ripdata$ProjectID)
newvals <- factor(c("COSRP", "HEROW", "NAPSO", "SACTO"))
ripdata$ProjCode <- newvals[match(ripdata$ProjectID, oldvals)]

#REMOVE UNNECESSARY DATA
ripdata <- ripdata[-c(which(ripdata$SpeciesVarietalName == "Unknown")), ]
ripdata <- ripdata[-c(which(ripdata$SpeciesVarietalName == "Not Recorded")), ]
ripdata <- ripdata[-c(which(ripdata$SpeciesVarietalName == "dead wood")), ]
ripdata <- droplevels(ripdata)

#PROJECT LOCATION SUMMARY FOR EACH SITE
ProjLoc <- aggregate(cbind(Latitude,Longitude) ~ ProjCode, data=ripdata, mean)

#ADD PLANT GENUS COLUMN FACTOR AND PLOT FREQUENCY DISTRIBUTION
ripdata$Genus = as.factor(sapply(strsplit(as.character(ripdata$SpeciesVarietalName), " "), "[[", 1))
temp <- sort(summary(ripdata$Genus))
barplot(sort(summary(ripdata$Genus)), las=2)
top5 <- temp[(length(temp)-5+1):length(temp)]
rm(temp)
rm(top5)

top5 <- ripdata[c(which(ripdata$Genus == "Fraxinus")),]
top5 <- rbind(top5,ripdata[c(which(ripdata$Genus == "Acer")),])
top5 <- rbind(top5,ripdata[c(which(ripdata$Genus == "Quercus")),])
top5 <- rbind(top5,ripdata[c(which(ripdata$Genus == "Populus")),])
top5 <- rbind(top5,ripdata[c(which(ripdata$Genus == "Salix")),])
top5 <- droplevels(top5)

#TEST FOR INDEPENDENCE
ind_table <- table(top5$Genus,top5$ProjCode)
chisq.test(ind_table)

#PLOT MAP
plot(ProjLoc[1,3],ProjLoc[1,2],pch=15,main="Project Locations",xlab="Longitude",ylab="Latitude",xlim=c(-123,-121),ylim=c(37,41))
points(ProjLoc[2,3],ProjLoc[2,2],pch=0)
points(ProjLoc[3,3],ProjLoc[3,2],pch=8)
points(ProjLoc[4,3],ProjLoc[4,2],pch=24)
legend("topright",title="Project Sites",c("COSRP","HEROW","NAPSO","SACTO"),pch=c(15,0,8,24),inset=0.05)
grid(10,10)
write.csv(top5,file="top5_dataset.csv")