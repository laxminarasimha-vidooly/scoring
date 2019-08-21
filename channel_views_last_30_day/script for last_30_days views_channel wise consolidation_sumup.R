data1<-read.csv(file.choose(), header=T, stringsAsFactors = F, sep="$")
data2<-read.csv(file.choose(), header=T, stringsAsFactors = F, sep="$")
data3<-read.csv(file.choose(), header=T, stringsAsFactors = F, sep="$")
head(data1)
head(data2)
head(data3)

final_data<-data.frame()
final_data<-rbind(final_data,data3)
final_data1<-final_data[,c(1,3)]
final_data1<-as.data.frame(final_data1)
final_data1[final_data1$views=="NA",]
final_data2<-final_data1[!is.na(final_data1$views),]
require(dplyr)
fin_data<-NULL
fin_data<-data.frame(matrix(nrow=0,ncol=2))
colnames(fin_data)<-c("channelid", "views")
xyz=1
counter=1
for(i in unique(final_data2$channelid))
{
  aaa<-NULL
  aaa<-filter(final_data2, channelid %in% i)
  aaa<-as.data.frame(aaa)
  aaa$channelid<-as.character(aaa$channelid)
  #aaa$date<-as.Date(aaa$date)
  temp_data<-NULL
  temp_data<-data.frame(matrix(nrow=0,ncol=2))
  #colnames(temp_data)=c("channelid","views")
  for(y in 1:nrow(aaa))
  {
    temp_data<-aaa %>%
                    group_by(channelid)%>%
                    summarise(views=sum(as.numeric(views)))
  }
  fin_data<-rbind(fin_data,temp_data)
  write.table(fin_data,"data_last30views_pivot_channelwise.csv", sep="$")
  print(c(i,temp_data$views,xyz,"success"))
  
  if(xyz!=length(unique(final_data2$channelid)))
  {
    xyz=xyz+1
  }
  counter=counter+1
  if(counter>5)
  {
    Sys.sleep(2)
    print("no problem")
    counter=0
  }
} 
