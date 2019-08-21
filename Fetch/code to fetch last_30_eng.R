
library(httr)
library(curl)
library(RCurl)
library(rjson)
library(jsonlite)
library(tidyr)
library(dplyr)
library(readr)
library(lubridate)
library(Hmisc)
library(chron)
library(rvest)

keys=c("AIzaSyAAbyrs8ofXHbyVDZV-u7DpH9c9IVAOok0",
       "AIzaSyB2NcRfFR2NwggTtobJpEOnY9YoEM9byZM",
       "AIzaSyDE_ihZTZB-3V29zzl9VqdEV945ZtaUMEQ",
       "AIzaSyBOoIbMV06l1-Vn10acksO54OCE2gmrjNE",
       "AIzaSyDw4U6z7GOqZLncFKUIaLj789ZxumPINkc",
       "AIzaSyAHegHlR1UfY6R-imJr1faqDxGM0YL-sBY",
       "AIzaSyBZvJhBF1MZn579tGdpta50HCl2CW374_U",
       "AIzaSyDUxDpxv1crwr4h3ZF9f7MCEV4kbPBMOCY",
       "AIzaSyDIuWfiFyyQ2TEihNVtnjqM39h7DoDPF3M",
       "AIzaSyDGIUHpM-KvnD1Xpa7m60KhxoJpEA1Iejk",
       "AIzaSyBedbS79J8txSToZqtelgaVyhY4mV9_7Bo",
       "AIzaSyBHiE5t86GKVX-YuKrV-09flVtZ1R1s6mA",
       "AIzaSyAV4W2ILFEl_4pkCZqAtjbK72I3Gp1m0Cc",
       "AIzaSyDMHv0urqfb2mKOMaUS5WNEdb4Oy5ZEYj8",
       "AIzaSyDQKbA4-SgwChoW7o-eWyPCNZ4YXG9RiI4",
       "AIzaSyAJiPQIjjRkB3Q5WEAg6FgHZANastW9KKA",
       "AIzaSyACrpudPvAJRNdj5ivQvG-J3RCikudfgpU",
       "AIzaSyCy97QAOAgYVfZ9Ow-jA5NK7Htx3d4ccF0",
       "AIzaSyD17YtJy2QvWPLYux58kGTvui88fmkqGdk",
       "AIzaSyCk2wFTWETQ1S7seIi2EpLiBx1ca441np0",
       "AIzaSyBhFtmV1-g9VjAYAiBqUjK345E9DDjS9GU",
       "AIzaSyAKowKJtGAwfXB2jyyDVwPPEfb6h7pf38w",
       "AIzaSyAsaTykyKSl5RJU-VblfzbWZV91xBsU3LY")

keys2<-vector(mode="numeric", length=0)

for(k in 1:length(keys))
{tryCatch({
  dum_url<-paste(c("https://www.googleapis.com/youtube/v3/channels?part=snippet,statistics&key=",keys[k],"&id=UCw9Ponb04uNH2AT5gPZ2z6A"),collapse="")
  dum_url2=GET(dum_url)
  stat=status_code(dum_url2)
},error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) 
  
  if(stat!=403)
  {
    keys2[k]<-keys[k]
  }else
  {
    next
  }
}
keys2 <- keys2[!is.na(keys2)]

a<-1:length(keys2)
keys_updated<-paste0(keys2[a], collapse=",")
####################
#Getting 30 engagement through fetching
#####################
url4="http://52.70.219.120/data/getvideostats.php?after=2018-12-01&before=2018-12-10&apikey=AIzaSyBedbS79J8txSToZqtelgaVyhY4mV9_7Bo&tags=0&desc=0&channelid=UCM2Fwf8C9iWGVrvUeAjyv2g"
smpl3<-read.csv(url4, header = T)

data7<-data.frame(matrix(nrow=0,ncol=ncol(smpl3)))
colnames(data7)<-names(smpl3)
data<-read.csv(file.choose(), header=T, stringsAsFactors = F)
after<-Sys.Date()-31
before<-Sys.Date()
date<-after
datef<-after
gap<-31
while(date<before)
{
  date1<-date+gap
  datef<-c(datef,date1)
  date<-date1
}
datef[length(datef)]<-before
datef


for(i in 1:nrow(data))
{j=1
threshold=3
for(j in c(1:(length(datef)-1))) 
{
  k<-1
  while (k<(threshold))
  {
  repeat{
    tryCatch({
      url5<-paste(c("http://52.70.219.120/data/getvideostats.php?after=",as.character(datef[j]),"&before=",as.character(datef[j+1]),"&apikey=",sample((keys2),1),"&tags=0&desc=0&channelid=",as.character(data[i,1])),collapse ="")
      print(url5)
      data_1<-read.csv(url5, header = T, stringsAsFactors = F)
      break
    },error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
    if(nrow(data_1)==0)
    {
      k<-k+1
      print(c("no of iteration is",k))
      
    }
    else
    {
      #data_1$keyword<-keyword[i,1] 
      break
    }
  }
  if(nrow(data_1) == 0) 
  {
    data_1 <- "#N/A"
  } 
  else 
  {
    data7<-rbind(data7,data_1)
  }
  print("count of channel")
  print(i)
}
}
write.table(data7, "30_days_eng_training.csv", sep="$")
data7[data7=="N/A"]<-0

newdate<-Sys.Date()-31
#created<-gsub("\\T.*","",data7$publishedAt)
data8<- data7 %>%
  group_by(channelid) %>%
  #subset(created> newdate) %>%
  summarise(last_30_eng = sum(as.numeric(likeCount)+
                                as.numeric(dislikeCount)+
                                as.numeric(commentCount)))
write.table(data8, "30_days_eng_training_pivot.csv", sep="\t")
