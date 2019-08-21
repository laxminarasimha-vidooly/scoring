# install.packages("httr")
# install.packages("curl")
# install.packages("RCurl")
# install.packages("rjson")
# install.packages("jsonlite")
# install.packages("tidyr")
# install.packages("dplyr")
# install.packages("readr")
# install.packages("lubridate")
# install.packages("Hmisc")
# install.packages("chron")
# install.packages("rvest")
#########################

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
##################
#Keychecker#######
##################
keys=c("keys")

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
#Getting Lifetime subscribers and views
#####################
# paste4 <- function(x,sep=",") {
#   x <- gsub("^\\s+|\\s+$", "", x) 
#   ret <- paste(x[!is.na(x) & !(x %in% "")], collapse = sep)
#   is.na(ret) <- ret == ""
#   return(ret)
# }

data<-read.csv(file.choose(), header=T, stringsAsFactors = F)
data<-as.data.frame(data[1:10,])

data2<-data.frame(matrix(nrow=0,ncol=4))
colnames(data2)<-c("channelid","subscriber_lifetime","views_lifetime","lifetime_uploads")

data_fin<-data.frame(matrix(nrow=0,ncol=4))
colnames(data_fin)<-c("channelid","subscriber_lifetime","views_lifetime","lifetime_uploads")

data1_11<-matrix(ncol=length(data))
data1_11[1,]<-data
max=50
data1_111<-split(data1_11, ceiling(seq_along(data1_11)/max))

slots<-ceiling(nrow(data)/50)
#if(slots==1){slots=2}
num<-c(0:(slots-1))

for(x in num)
{
  # a<-1:max
  # temp.data<-as.data.frame(data1_111[x])
  # ch_ids<-paste4(temp.data[a,])
  start1<-((x*50)+1)
  end1<-((x+1)*50)
  if(end1>nrow(data))
  {
    end1<-nrow(data)
  }
  num1<-c(start1:end1)
  
  ch_ids<-paste0(data[num1,],collapse=",")
  
  repeat{
    tryCatch({
      url_l=paste(c("https://www.googleapis.com/youtube/v3/channels?part=snippet,statistics&key=",sample((keys2),1),"&id=",as.character(ch_ids)),collapse ="")
      print(url_l)
      smplx=GET(url_l)
      temp_url<-fromJSON(txt = url_l)
      stat=status_code(smplx)
      break
    },error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) 
  }
  if(length(temp_url$items) > 0)
  {
    for (i in 1:length(temp_url$items$id)) 
    {
      data_tmp<-data.frame(matrix(nrow=1, ncol=4))
      colnames(data_tmp)<-c("channelid","subscriber_lifetime","views_lifetime","life_uploads")
      
      data_tmp[1,1]<-temp_url$items$id[i]
      data_tmp[1,2]<-temp_url$items$statistics$subscriberCount[i]
      data_tmp[1,3]<-temp_url$items$statistics$viewCount[i]
      data_tmp[1,4]<-temp_url$items$statistics$videoCount[i]
      data_fin<-rbind(data_fin,data_tmp)
    }
  }
}
data2<-rbind(data2, data_fin)
####################
#Getting 30 Views through daily tracking
#####################
url1="http://52.70.219.120/data/getchanneltracks.php?after=2018-06-01&before=2018-12-01&type=daily&channelid=UCM2Fwf8C9iWGVrvUeAjyv2g"
smpl1<-GET(url1,timeout(100))
smpl2<-httr::content(smpl1)
print(smpl2)

data4<-data.frame(matrix(nrow=0,ncol=ncol(smpl2)))
colnames(data4)<-names(smpl2)

after<-Sys.Date()-31
before<-Sys.Date()

for (i in 1:num)
{
  # a<-1:max
  # temp.data<-as.data.frame(data1_111[i])
  # ch_ids<-paste4(temp.data[a,])
  repeat{
    tryCatch({
      url2<-paste(c("http://52.70.219.120/data/getchanneltracks.php?after=",as.character(after),"&before=",as.character(before),"&type=daily&channelid=",as.character(ch_ids)),collapse="")
    },error=function(e){cat("ERROR :",conditionMessage(e), "\n")})  
    print(url2)
    url3<-GET(url2,timeout(500))
    break
  }
  data_main<-httr::content(url3)
  if(nrow(data_main) == 0) 
  {
    data_main <- "#N/A"
  } 
  else 
  {
    data4<-rbind(data4,data_main)
  }
}

data6<- data4 %>%
  group_by(channelid) %>%
  summarise(last_30_views = sum(as.numeric(positiveviews)))
data2<- (merge(data2, data6, by = 'channelid'))

####################
#Getting 30days engagement and 90 days uploads via fetching
#####################
url4="http://52.70.219.120/data/getvideostats.php?after=2018-12-01&before=2018-12-10&apikey=AIzaSyBedbS79J8txSToZqtelgaVyhY4mV9_7Bo&tags=0&desc=0&channelid=UCM2Fwf8C9iWGVrvUeAjyv2g"
smpl3<-read.csv(url4, header = T)

data7<-data.frame(matrix(nrow=0,ncol=ncol(smpl3)))
colnames(data7)<-names(smpl3)

after<-Sys.Date()-91
before<-Sys.Date()
date<-after
datef<-after
gap<-30
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
for(j in c(1:(length(datef)-1))) 
{
  repeat{
    tryCatch({
      url5<-paste(c("http://52.70.219.120/data/getvideostats.php?after=",as.character(datef[j]),"&before=",as.character(datef[j+1]),"&apikey=",sample((keys2),1),"&tags=0&desc=0&channelid=",as.character(data[i,1])),collapse ="")
      print(url5)
      data_1<-read.csv(url5, header = T, stringsAsFactors = F)
      break
    },error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
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
write.table(data7, "file.csv", sep="\t")
data7[data7=="N/A"]<-0

newdate<-Sys.Date()-31
created<-gsub("\\T.*","",data7$publishedAt)
data8<- data7 %>%
  group_by(channelid) %>%
  subset(created> newdate) %>%
  summarise(last_30_eng = sum(as.numeric(likeCount)+
                                as.numeric(dislikeCount)+
                                as.numeric(commentCount)))
str(data7)
head(data7)
data2<- (merge(data2, data8, by = 'channelid'))

data5<- data7 %>%
  group_by(channelid) %>%
  summarise(last_90_uploads = sum(as.numeric(length(videoid))))
data2 <- (merge(data2, data5, by = 'channelid'))

##########################################
#Uploads 90 if not available through fetching
##########################################
data9_1<-data2
data9_2<-data9_1$last_90_uploads[data9_1$last_90_uploads==0]

if(length(data9_2)>0){
  
  url1="http://52.70.219.120/data/getchanneltracks.php?after=2018-06-01&before=2018-12-01&type=daily&channelid=UCM2Fwf8C9iWGVrvUeAjyv2g"
  smpl1<-GET(url1,timeout(100))
  smpl2<-httr::content(smpl1)
  print(smpl2)
  
  data4_1<-data.frame(matrix(nrow=0,ncol=ncol(smpl2)))
  colnames(data4_1)<-names(smpl2)
  data1_1<-data9_1$channelid[data9_1$last_90_uploads==0]
  
  after<-Sys.Date()-91
  before<-Sys.Date()
  
  for (i in 1:length(data1_1))
  {  
    tryCatch({
      url2_1<-paste(c("http://52.70.219.120/data/getchanneltracks.php?after=",as.character(after),"&before=",as.character(before),"&type=daily&channelid=",as.character(data1_1[i])),collapse="")
    },error=function(e){cat("ERROR :",conditionMessage(e), "\n")})  
    print(url2_1)
    url3_11<-GET(url2_1,timeout(100))
    
    data_main<-httr::content(url3_11)
    
    if(nrow(data_main) == 0) 
    {
      data_main <- "#N/A"
    } 
    else 
    {
      data4_1<-rbind(data4_1,data_main)
    }
  }
  
  data_11<-data4_1 %>%
    group_by(channelid) %>%
    summarise(last_90_uploads.y = sum(as.numeric(positiveuploads)))
  
  data2<-left_join(data2, data_11, by='channelid') %>%
    mutate(last_90_uploads = ifelse(last_90_uploads == 0, last_90_uploads.y, last_90_uploads))
  #data2_1<-data2_1[,-7]
}else{break}
###########################################
#getting 30 days engagement through API
##########################################
data2$last_30_eng<-0
data9_3<-data2
data9_4<-data9_3$last_30_eng[data9_3$last_30_eng==0]

if(length(data9_4)>0){
  library(lubridate)
  data10<-data.frame(matrix(nrow=0,ncol=2))
  colnames(data10)<-c("channelid","last_30_eng")
  after1<-Sys.Date()-30
  before2<-Sys.Date()
  
  data1_2<-data9_3$channelid[data9_3$last_30_eng==0]

  if(month(after1)!=month(before2))
  {
    before1=paste(c(year(after1),"-",month(after1),"-","30"),collapse = "")
    after2=paste(c(year(before2),"-",month(before2),"-","01"),collapse = "")
    
    after3<-c(after1,as.Date(after2))
    before3<-c(as.Date(before1),before2)
    
    for (i in 1:length(data1_2))
    {
      for(j in 1:length(after3))
      {
        tryCatch({
          url6=paste(c("http://52.70.219.120/data/getchannelengagement.php?after=",as.character(after3[j]),"&before=",as.character(before3[j]),"&channelid=",as.character(data1_2[i])),collapse = "")
          print(url6)
          smpl4<-read.csv(url6, header = T, stringsAsFactors = F)
          if(nrow(smpl4) == 0) 
          {
            smpl4 <- "#N/A"
          } 
          else 
          {
            data10<-rbind(data10,smpl4)
          }
        },error=function(e){cat("ERROR :",conditionMessage(e), "\n")})  
        
        
        print (i)
      }
    }
  } 
  if(month(after1)==month(before2)){
    for (i in 1:length(data1_2))
    {
      tryCatch({
        url6=paste(c("http://52.70.219.120/data/getchannelengagement.php?after=",as.character(after1),"&before=",as.character(before2),"&channelid=",as.character(data1_2[i])),collapse = "")
        print(url6)
        smpl4<-read.csv(url6, header = T, stringsAsFactors = F)
        if(nrow(smpl4) == 0) 
        {
          smpl4 <- "#N/A"
        } 
        else 
        {
          data10<-rbind(data10,smpl4)
        }
      },error=function(e){cat("ERROR :",conditionMessage(e), "\n")})  
      
      print (i)
    } 
  }
  data11<- data10 %>%
    group_by(channelid) %>%
    summarise(last_30_eng1 = sum(as.numeric(total)))
  
  data2<-left_join(data2, data11, by='channelid') %>%
    mutate(last_30_eng = ifelse(last_30_eng == 0, last_30_eng1, last_30_eng))
  #data2_1<-data2_1[,-7]
  
}else{
  break
}
############################################
#Getting category dynamically
##############################################
# catid<-c(1,2,10,15,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44)
# YT_cat<-c("Film & Animation","Autos & Vehicles","Music","Pets & Animals","Sports","ShortMovies","Travel & Events","Gaming","Videoblogging","People & Blogs","Comedy","Entertainment","News & Politics","Howto & Style","Education","Science & Technology","Nonprofits & Activism","Entertainment","Anime/Animation","Action/Adventure","Classics","Comedy","Documentary","Drama","Family","Foreign","Horror","Sci-Fi/Fantasy","Thriller","Shorts","Entertainment","Entertainment")
# YT_Categories<-cbind(catid,YT_cat)
YT_categories<-read.csv("E:/Laxmi_Rnd/My Laptop/Work/Scoring_algo/Youtube categories.csv", header=T)
YT_categories<-as.data.frame(YT_categories)

# url7="http://52.70.219.120/trending/getchannlesdetails.php?channelsid=UCIHdr1oAtGvPeJTmxpQVDfw"
# smpl5<-read.csv(url7, header = T)
# 
# data11<-data.frame(matrix(nrow=0,ncol=ncol(smpl5)))
# colnames(data11)<-names(smpl5)
# 
# for (i in 1:length(data1_11))
# {
#   tryCatch({
#     url8=paste(c("http://52.70.219.120/trending/getchannlesdetails.php?channelsid=",as.character(data1_11[i])),collapse = "")
#   },error=function(e){cat("ERROR :",conditionMessage(e), "\n")})  
# print(url8)
#   smpl6<-read.csv(url8, header=T)
# if(nrow(smpl6)==0)
# {
#   smpl6<-"#N/A"
# }
# else
# {
#   data11<-rbind(data11, smpl6)
# }
# }
# data22<-c(data11[1],data11[7])
# data2<- (merge(data2, data22, by = 'channelid'))
# 
# data2<-(merge(data2, YT_Categories, by = 'catid'))

data2$category<-NULL
num<-c(1:nrow(data))
data1_1111<-NULL
data1_1111<-as.data.frame(as.vector(data),stringsAsFactors = F)
colnames(data1_1111)<-c("channelid")
#data2$catid<-as.numeric(data2$catid)
YT_categories$catid<-as.numeric(YT_categories$catid)
YT_categories$category<-as.character(YT_categories$category)

for(i in num)
{
  chid=data1_1111$channelid[i]
  document1=document2=videoId=url1=url2=NULL
  tryCatch({
    url1<-paste(c("https://www.googleapis.com/youtube/v3/search?key=",keys2[sample(1:length(keys2),1)],"&channelId=",as.character(chid),"&part=id&order=date&maxResults=50&pageToken="), collapse = "")
    print(url1)
    document1 <- fromJSON(txt=url1)},error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
  videoId<-paste0(document1$items$id$videoId, collapse=",")
  
  tryCatch({  
    url2<-paste(c("https://www.googleapis.com/youtube/v3/videos?part=snippet&maxResults=50&key=",keys2[sample(1:length(keys2),1)],"&id=",videoId), collapse = "")
    print(url2)
    document2 <- fromJSON(txt=url2)},error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  categid<-names(sort(table(document2$items$snippet$categoryId),decreasing=TRUE)[1])
  
  data1_1111$catid[i]=YT_categories[YT_categories[,2]==categid,][2]
  data1_1111$category[i]=YT_categories[YT_categories[,2]==categid,][1]
}

data2<- (merge(data2, data1_1111, by = 'channelid'))
data2$catid<-as.numeric(data2$catid)
data2$category<-as.character(data2$category)
#data2<-left_join(data2, YT_categories, by='catid') 

drops <- c("last_30_eng1","last_90_uploads.x","last_90_uploads.y","catid.y","catid.x", "last_30_eng1.y", "last_30_eng1.x")
data2<-data2[ , !(names(data2) %in% drops)]
write.table(data2, "E:/Laxmi_Rnd/My Laptop/Work/Scoring_algo/consolidated.csv", sep="\t")
