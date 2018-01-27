setwd("D:\\study\\rutgers\\DATA\\week14")
consumer <- read.csv("Consumer_Services_Mediated_Complaints.csv")
plot(consumer$Business.City,col=rainbow(100),ylim=c(0,1000),main="complain frequency")
abline(h=862,col=c("blue"),lty=2)
abline(h=827,col=c("red"),lty=2)
consumer_2city <- subset(consumer,consumer$Business.City=="NEW YORK"|consumer$Business.City=="BROOKLYN")
consumer[consumer$Restitution>0,"restitution"] <- "1"
consumer[consumer$Restitution==0,"restitution"] <-"0"
no_na_comsumer <- na.omit(consumer)
mosaicplot(no_na_comsumer$restitution~no_na_comsumer$Satisfaction,col=c("green","red"),xlab="satisfaction",ylab="restitution",main="satisfaction with restitution")
no_na_comsumer <- no_na_comsumer[-(no_na_comsumer$Complaint.Result==" Unable to Locate Consumer - ULC"|no_na_comsumer$Complaint.Result=="Cash Amount - Consumer Restitution from Escrow"|no_na_comsumer$Complaint.Result=="Referred to Manufacturer - RMF"|no_na_comsumer$Complaint.Result=="Agency Collected Judgement - ACJ"),]
no_na_comsumer <- no_na_comsumer[-4461,]
no_na_comsumer <- droplevels(no_na_comsumer)
satisfy.consumer <- subset(no_na_comsumer,restitution=="0")
satisfy.consumer[satisfy.consumer$Complaint.Type=="Damaged Goods - D01","related to goods"] <- 1
satisfy.consumer[satisfy.consumer$Complaint.Type=="Defective Goods - D02","related to goods"] <- 1
satisfy.consumer[satisfy.consumer$Complaint.Type=="Excessive Charges Exchange Goods/Contract Cancelled - E01","related to goods"] <- 1
satisfy.consumer[satisfy.consumer$Complaint.Type=="Non-Delivery Goods/Services","related to goods"] <- 1
satisfy.consumer[satisfy.consumer$Complaint.Type=="Non-Delivery of Goods - N01","related to goods"] <- 1
satisfy.consumer[satisfy.consumer$Complaint.Type=="Wrong Goods - W01","related to goods"] <- 1
satisfy.consumer[satisfy.consumer$Complaint.Type=="Surcharge/Overcharge - S02","related to goods"] <-1
satisfy.consumer[is.na(satisfy.consumer$`related to goods`),"related to goods"]<-0
satisfy.consumer[satisfy.consumer$Satisfaction=="Yes","satisfication"] <-1
satisfy.consumer[satisfy.consumer$Satisfaction=="No","satisfication"] <- 0
good_satisfy <- data.frame(satisfy.consumer$`related to goods`,satisfy.consumer$satisfication)
colnames(good_satisfy)<- c("good","satisfy")
satisfy <- good_satisfy$satisfy
0.375
mean(satisfy[satisfy.consumer[,"related to goods"]==1])
0.3519164
p_perm <- rep(NA,10000)
p_perm <- rep(NA,10000)
good <- good_satisfy$good
for (i in 1:10000){
  + good_i <- good[sample(2112,2112)]
  + p_perm[i] <- mean(satisfy[good_i==1])-mean(satisfy[good_i==0])}
p_diff <-mean(satisfy[good==1])-mean(satisfy[good==0])
p_value <- mean(p_diff>=p_perm)
plot(x=seq(from=-5,to=5,by=0.1),y=dnorm(seq(from=-5,to=5,by=0.1),mean=0),type="l",xlab="mean difference",ylab="possiblity")
abline(v=p_value,col="red")
hist(p_perm)
abline(v=p_diff,col="red")
p_value=0.0816
library(rpart)
Tree <- rpart(Satisfaction~Restitution+Complaint.Result,data=no_na_comsumer)
good_response <- subset(no_na_comsumer,no_na_comsumer$Restitution!=0)
restitution.means <- tapply(good_response$Restitution,good_response$Complaint.Type,mean)
barplot(restitution.means)
restitution.goods <- good_response[good_response$Complaint.Type=="Damaged Goods - D01","Restitution"]
restitution.service<- good_response[good_response$Complaint.Type=="Quality of Work - Q01","Restitution"]
sd_goods <-sd(restitution.goods)
sd_service <- sd(restitution.service)
l_good <- length(restitution.goods)
l_service <- length(restitution.service)
sd_good_service <- sqrt(sd_goods^2/l_good+sd_service^2/l_service)
zeta <- (mean(restitution.service)-mean(restitution.goods))/sd_good_service
plot(x=seq(from=-5,to=5,by=0.1),y=dnorm(seq(from=-5,to=5,by=0.1),mean=0),type="l",xlab="mean difference",ylab="probability")
abline(v=zeta,col="red")
p_value <-1-pnorm(zeta)
