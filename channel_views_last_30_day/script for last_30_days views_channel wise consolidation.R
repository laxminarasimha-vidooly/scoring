data1<-read.csv(file.choose(), header=T, sep="$")
colnames(data1)=c("channelid","date","views","lifetime_views")
head(data1)
ind=1:2183315
data2<-data1[-ind,]
head(data2)

require(dplyr)
fin_file<-data2
head(fin_file)
fin_file<-fin_file[,c(-4)]
head(fin_file)
colnames(fin_file)=c("channelid","date","views")
head(fin_file)
str(fin_file)
fin_file<-as.data.frame(fin_file)
fin_file$channelid<-as.character(fin_file$channelid)
fin_file$date<-as.Date(fin_file$date)
fin_data<-data.frame(matrix(nrow=0,ncol=3))
colnames(fin_data)<-colnames(fin_file)
final<-data.frame(matrix(nrow=0,ncol=3))
colnames(final)<-colnames(fin_file)
xyz=1
counter=1
for(i in unique(fin_file$channelid))
{
aaa<-NULL
aaa<-filter(fin_file, channelid %in% i)
aaa<-as.data.frame(aaa)
aaa$channelid<-as.character(aaa$channelid)
aaa$date<-as.Date(aaa$date)
temp_data<-NULL
temp_data<-data.frame(matrix(nrow=0,ncol=3))
colnames(temp_data)=c("channelid","date","views")
for(y in 1:nrow(aaa))
{
temp_data[y,1]<-as.character(aaa$channelid[y])
temp_data[y,2]<-as.Date(aaa$date[y])
temp_data[y,3]<-as.integer(aaa$views[y+1]-aaa$views[y])
}
fin_data<-rbind(fin_data,temp_data)
write.table(fin_data,"data_last30views_pivot.csv", sep="$")
print(c(i,xyz,"success"))

if(xyz!=length(unique(fin_file$channelid)))
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
