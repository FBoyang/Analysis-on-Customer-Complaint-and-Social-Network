setwd("D:\\study\\rutgers\\DATA\\week14")
job <- read.csv("ALLYOUCANEAT.csv")
job_no_na<- na.omit(job)
plot(job_no_na$PROFESSION~job_no_na$NATIVE)
job_US <- subset(job_no_na,job_no_na$NATIVE=="United-States")
plot(job_US$YEARS~job_US$PROFESSION)
abline(h=10,col=c("red"))
job_no_us <- subset(job_no_na,job_no_na$NATIVE!="United-States")
plot(job_no_us$YEARS~job_no_us$PROFESSION)
abline(h=10,col=c("red"))
levels(job_no_na$NATIVE)<-c(levels(job_no_na$NATIVE),"Other Countries")
for(i in 1:28244)
  {
if(job_no_na[i,"NATIVE"]!="United-States") job_no_na[i,"NATIVE"] <- "Other Countries"}
job_no_na[job_no_na$NATIVE!="United-States","NATIVE"] <- "Other Countries"
job_no_na[job_no_na$NATIVE!="United-States","NATIVE1"] <- "United-States"
job_no_na <- droplevels(job_no_na)
plot(job_no_na$PROFESSION~job_no_na$NATIVE,col=c(5:7))
plot(job_no_na$EDUCATION~job_no_na$NATIVE,col=c(7:9))
