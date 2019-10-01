#1,Preliminary Analysis
#a)
Donors = read.csv("FinalProject.csv")
str(Donors)
Donors$Total.Donations = as.numeric(gsub("[\\$,]", "",Donors$Total.Donations))
#1.1
table(Donors$Total.Donations > 0)
table(Donors$Total.Donations >= 100)
table(Donors$Total.Donations >= 500)
table(Donors$Total.Donations >= 1000)
#1.2
#2004
table(Donors$X2004.Donation > 0)
table(Donors$X2004.Donation >= 100)
table(Donors$X2004.Donation >= 500)
table(Donors$X2004.Donation >= 1000)
#2005
table(Donors$X2005.Donation > 0)
table(Donors$X2005.Donation >= 100)
table(Donors$X2005.Donation >= 500)
table(Donors$X2005.Donation >= 1000)
#2006
table(Donors$X2006.Donation > 0)
table(Donors$X2006.Donation >= 100)
table(Donors$X2006.Donation >= 500)
table(Donors$X2006.Donation >= 1000)
#2007
table(Donors$X2007.Donation > 0)
table(Donors$X2007.Donation >= 100)
table(Donors$X2007.Donation >= 500)
table(Donors$X2007.Donation >= 1000)
#2008
table(Donors$X2008.Donation > 0)
table(Donors$X2008.Donation >= 100)
table(Donors$X2008.Donation >= 500)
table(Donors$X2008.Donation >= 1000)
#2009
table(Donors$X2009.Donation > 0)
table(Donors$X2009.Donation >= 100)
table(Donors$X2009.Donation >= 500)
table(Donors$X2009.Donation >= 1000)
#2010
table(Donors$X2010.Donation > 0)
table(Donors$X2010.Donation >= 100)
table(Donors$X2010.Donation >= 500)
table(Donors$X2010.Donation >= 1000)
#2011
table(Donors$X2011.Donation > 0)
table(Donors$X2011.Donation >= 100)
table(Donors$X2011.Donation >= 500)
table(Donors$X2011.Donation >= 1000)
#2012
table(Donors$X2012.Donation > 0)
table(Donors$X2012.Donation >= 100)
table(Donors$X2012.Donation >= 500)
table(Donors$X2012.Donation >= 1000)
#2013
table(Donors$X2013.Donation > 0)
table(Donors$X2013.Donation >= 100)
table(Donors$X2013.Donation >= 500)
table(Donors$X2013.Donation >= 1000)
#2014
table(Donors$X2014.Donation > 0)
table(Donors$X2014.Donation >= 100)
table(Donors$X2014.Donation >= 500)
table(Donors$X2014.Donation >= 1000)
#2015
table(Donors$X2015.Donation > 0)
table(Donors$X2015.Donation >= 100)
table(Donors$X2015.Donation >= 500)
table(Donors$X2015.Donation >= 1000)
#1.3
hist(Donors$Total.Donations,xlim=c(0,10000))
hist(Donors$X2014.Donation)
hist(Donors$X2015.Donation)
#1.4
plot_donorfile = subset(Donors,X2014.Donation>0 & X2015.Donation>0 )
plot(plot_donorfile$X2014.Donation,plot_donorfile$X2015.Donation)
#b
#2.1
table(Donors$Total.Tickets > 0)
#2.2
#2002
table(Donors$X2002.Qty > 0)
#2003
table(Donors$X2003.Qty > 0)
#2004
table(Donors$X2004.Qty > 0)
#2005
table(Donors$X2005.Qty > 0)
#2006
table(Donors$X2006.Qty > 0)
#2007
table(Donors$X2007.Qty > 0)
#2008
table(Donors$X2008.Qty > 0)
#2009
table(Donors$X2009.Qty > 0)
#2010
table(Donors$X2010.Qty > 0)
#2011
table(Donors$X2011.Qty > 0)
#2012
table(Donors$X2012.Qty > 0)
#2013
table(Donors$X2013.Qty > 0)
#2014
table(Donors$X2014.Qty > 0)
#2015
table(Donors$X2015.Qty > 0)
#2.3
Donors$Total.Ticket.Revenue = as.numeric(gsub("[\\$,]", "",Donors$Total.Ticket.Revenue))
hist(Donors$Total.Ticket.Revenue)
hist(Donors$X2014.Total.Cost)
hist(Donors$X2015.Total.Cost)
#2.4
plot_ticketfile = subset(Donors,X2014.Total.Cost>0 & X2015.Total.Cost>0 )
plot(plot_ticketfile$X2014.Total.Cost,plot_ticketfile$X2015.Total.Cost)
#c
#3.1
plot(Donors$Total.Donations , Donors$Total.Ticket.Revenue)
abline(lm(Donors$Total.Ticket.Revenue ~ Donors$Total.Donations))
#need to add something to remove outlier
NewDonors = subset(Donors,Total.Ticket.Revenue < 5000 & Total.Donations < 5000)
plot(NewDonors$Total.Donations , NewDonors$Total.Ticket.Revenue)
abline(lm(NewDonors$Total.Ticket.Revenue ~ NewDonors$Total.Donations))

#2,Donation Patterns Over Time
#a)donation cluster 
#delete most recent donation date
NewDonors = NewDonors[,-c(3)]
#delete $ and , 
NewDonors$Total.All.Revenue = as.numeric(gsub("[\\$,]", "",NewDonors$Total.All.Revenue))
NewDonors = NewDonors[,-c(1,2,4:33,45)]
NewDonors = subset(NewDonors, NewDonors$Total.Donations <= 5000
                   & NewDonors$Total.Donations > 0)
NewDonors = NewDonors[,-c(1)]

library(caret)
preproc = preProcess(NewDonors)

NewDonorsNorm = predict(preproc,NewDonors)


set.seed(420)

wss = (nrow(NewDonorsNorm)-1)*sum(apply(NewDonorsNorm,2,var))
for (i in 2:10) wss[i] = sum(kmeans(NewDonorsNorm,
                                    centers=i)$withinss)
plot(1:10, wss, type="b", xlab="Number of Clusters",
     ylab="groups sum of squares")
KmeansClustering = kmeans(NewDonorsNorm, centers = 3)
table(KmeansClustering$cluster)
KmeansClustering
OutNewDonors = data.frame(NewDonors,KmeansClustering$cluster)
write.csv(OutNewDonors, file = "NewDonor.csv")

#b)Donation linear regression
NewDonors = NewDonors[,-c(1:5)]
DonorReg = lm(X2014.Donation ~ .,data = NewDonors)
summary(DonorReg)
#Refine
NewDonors = NewDonors[,-c(4)]
DonorReg = lm(X2014.Donation ~ .,data = NewDonors)
summary(DonorReg)
Donor2015 = subset(Donors,Donors$X2015.Donation <= 2500 & Donors$Total.Donations > 0, select = c(
X2009.Donation,
X2010.Donation,
X2011.Donation,
X2012.Donation,
X2013.Donation,
X2015.Donation))
colnames(Donor2015) = c("X2009.Donation",
                        "X2010.Donation",
                        "X2011.Donation",
                        "X2012.Donation",
                        "X2013.Donation",
                        "X2014.Donation")
DonorPredictions = predict(DonorReg, newdata=Donor2015)
SSE = sum((Donor2015$X2014.Donation - DonorPredictions)^2)
SST = sum((Donor2015$X2014.Donation - mean(NewDonors$X2014.Donation))^2)
1 - SSE/SST

#3,Ticket Sales Patterns Over Time
#Ticket Clustering
NewTicket = Donors[,-c(1:4,33:46)]
NewTicket = subset(NewTicket, NewTicket$Total.Ticket.Revenue <= 5000 & NewTicket$Total.Ticket.Revenue >0)
NewTicket = NewTicket[,-c(1:2)]
NewTicket = NewTicket[,-c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33)]

#Normalization
library(caret)

preproc = preProcess(NewTicket)
NewTicketNorm = predict(preproc,NewTicket)
set.seed(420)
#Kmeans-Clustering
wss = (nrow(NewTicketNorm)-1)*sum(apply(NewTicketNorm,2,var))
for (i in 2:10) wss[i] = sum(kmeans(NewTicketNorm,
                                    centers=i)$withinss)
plot(1:10, wss, type="b", xlab="Number of Clusters",
     ylab="groups sum of squares")
KmeansClustering_2 = kmeans(NewTicketNorm, centers = 3)
table(KmeansClustering_2$cluster)
KmeansClustering_2
OutNewTicket = data.frame(NewTicket,KmeansClustering_2$cluster)
write.csv(OutNewTicket, file = "NewTicket.csv")
#Donation linear regression
NewTicket = NewTicket[,-c(1:7)]
TicketReg = lm(X2014.Total.Cost ~ .,data = NewTicket)
summary(TicketReg)
#Refine
NewTicket = NewTicket[,-c(1)]
TicketReg = lm(X2014.Total.Cost ~ .,data = NewTicket)
summary(TicketReg)
Ticket2015 = subset(Donors,Donors$X2015.Total.Cost <= 500 & Donors$Total.Ticket.Revenue > 0, select = c(
  X2009.Donation,
  X2010.Donation,
  X2011.Donation,
  X2012.Donation,
  X2013.Donation,
  X2015.Donation))
colnames(Ticket2015) = c("X2009.Total.Cost",
                         "X2010.Total.Cost",
                         "X2011.Total.Cost",
                         "X2012.Total.Cost",
                         "X2013.Total.Cost",
                         "X2014.Total.Cost")
TicketPredictions = predict(TicketReg, newdata=Ticket2015)
SSE = sum((Ticket2015$X2014.Total.Cost - TicketPredictions)^2)
SST = sum((Ticket2015$X2014.Total.Cost - mean(NewTicket$X2014.Total.Cost))^2)
1 - SSE/SST
#4,Joint Analysis
#i)
#Cluster Donations and Ticket Revenue
Both = read.csv("FinalProject_ForBoth.csv")
str(Both)
Both = Both[,-c(1)]
NewBoth = subset(Both, Both$Total.Donations <= 5000 
                 & Both$Total.Ticket.Revenue <= 5000 & Both$Total.All.Revenue > 0)


#Normalization
preproc = preProcess(NewBoth)
NewBothNorm = predict(preproc,NewBoth)
set.seed(420)
#Kmeans-Clustering
wss = (nrow(NewBothNorm)-1)*sum(apply(NewBothNorm,2,var))
for (i in 2:10) wss[i] = sum(kmeans(NewBothNorm,
                                    centers=i)$withinss)
plot(1:10, wss, type="b", xlab="Number of Clusters",
     ylab="groups sum of squares")
KmeansClustering_3 = kmeans(NewBothNorm, centers = 3)
table(KmeansClustering_3$cluster)
KmeansClustering_3
OutNewBoth = data.frame(NewBoth,KmeansClustering_3$cluster)
write.csv(OutNewBoth, file = "NewBoth.csv")
#ii)
#after adjusting
UpdateBoth = read.csv("FinalProject_NewForBoth.csv")
UpdateBoth = UpdateBoth[,-c(1,13,14,15,16,25:28)]
UpdateBoth = subset(UpdateBoth, UpdateBoth$Total.Donations <= 5000 & Total.Donations >0 
                 & UpdateBoth$Total.Ticket.Revenue <= 5000 &Total.Ticket.Revenue >0 & UpdateBoth$Total.All.Revenue > 0)
#Normalization
preproc = preProcess(UpdateBoth)
UpdateBothNorm = predict(preproc,UpdateBoth)
#adjust
X = as.matrix(UpdateBothNorm)
A = matrix(c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,4,5,1,1,4,5),27,1219)
UpdateBothNorm = X%*%A
set.seed(420)
#Kmeans-Clustering
wss = (nrow(UpdateBothNorm)-1)*sum(apply(UpdateBothNorm,2,var))
for (i in 2:10) wss[i] = sum(kmeans(UpdateBothNorm,
                                    centers=i)$withinss)
plot(1:10, wss, type="b", xlab="Number of Clusters",
     ylab="groups sum of squares")
KmeansClustering_4 = kmeans(UpdateBothNorm, centers = 3)
table(KmeansClustering_4$cluster)
KmeansClustering_4
OutUpdateBoth = data.frame(UpdateBoth,KmeansClustering_4$cluster)
write.csv(OutUpdateBoth, file = "UpdateBoth.csv")

