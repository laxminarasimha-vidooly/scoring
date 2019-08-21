
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
#Getting 30 Views through daily tracking
#####################
# paste4 <- function(x,sep=",") {
#   x <- gsub("^\\s+|\\s+$", "", x) 
#   ret <- paste(x[!is.na(x) & !(x %in% "")], collapse = sep)
#   is.na(ret) <- ret == ""
#   return(ret)
# }

url1="http://52.70.219.120/data/getchanneltracks.php?after=2018-06-01&before=2018-12-01&channelid=UCM2Fwf8C9iWGVrvUeAjyv2g"
smpl1<-GET(url1,timeout(1000))
smpl2<-httr::content(smpl1)
print(smpl2)

data4<-data.frame(matrix(nrow=0,ncol=ncol(smpl2)))
colnames(data4)<-names(smpl2)

data<-read.csv(file.choose(), header=T, stringsAsFactors = F)
# data1_11<-matrix(ncol=length(data))
# 
# data1_11[1,]<-data
# max=10
# data1_111<-split(data1_11, ceiling(seq_along(data1_11)/max))

slots<-ceiling(nrow(data)/25)
num<-c(0:(slots-1))

after<-Sys.Date()-31
before<-Sys.Date()

for (i in num)
{
  old<-Sys.time()
  start1<-((i*25)+1)
  end1<-((i+1)*25)
  if(end1>nrow(data))
  {
    end1<-nrow(data)
  }
  num1<-c(start1:end1)
  ch_ids<-paste0(data[num1,],collapse=",")
  url2<-paste(c("http://52.70.219.120/data/getchanneltracks.php?after=",as.character(after),"&before=",as.character(before),"&channelid=",as.character(ch_ids)),collapse="")
  print(url2)
  repeat{
  f<-tryCatch({
  url3<-GET(url2,timeout(1000));f<-1;},error=function(e) "0");if(f==1){print("ok");break};print("trying..")
  }
  data_main<-httr::content(url3)
  if(nrow(data_main) == 0) 
  {
    data_main <- "#N/A"
  } 
  else 
  {
    data4<-rbind(data4,data_main)
    write.table(data4, "final_part3.csv",sep="\t")
  }
  new<-Sys.time()-old
  print(i)
  print(new)
  Sys.sleep(2)
}
write.table(data4, "final_part3.csv",sep="\t")
data4<-read.csv(file.choose(), header=T, stringsAsFactors = F)
head(data4)
data6<- data4 %>%
  group_by(ï..channelid) %>%
  summarise(last_30_views = sum(as.numeric(positiveviews)))
#data4<- (merge(data4, data6, by = 'channelid'))
# data7<- data4 %>%
#   group_by(channelid) %>%
#   summarise(last_30_eng = sum(as.numeric(positiveviews)))
# data4<- (merge(data4, data6, by = 'channelid'))
write.table(data6, "30dayspred_training1.csv",sep="\t")

  