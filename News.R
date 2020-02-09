
library("dplyr") #data base
library("wordcloud") #wordcloud



#################
# load data
#################

data = read.csv("C:/Richard/Python/News-Dataset/abcnews-date-text.csv",sep=",")
data$year=substr(data$publish_date,1,4)
data$month=substr(data$publish_date,5,6)
data$day=substr(data$publish_date,7,8)
data$publish_date=NULL



##################
# get top words
##################
library("tm") #text mining, used for stopwords
library("plyr") #data base, count etc.

topx=function(year,month,top)
{
  test=data[as.numeric(data$year)==year & as.numeric(data$month)==month,]
  
  words=NULL
  
  for(i in 1:(length(test[,1])))
  {
    words=c(words,unlist(strsplit(as.character(test$headline_text[i])," ")))
  }
  
  words=data.frame(words)
  stopwords_en=data.frame(stopwords("english"))
  words=words[!words$words%in%stopwords_en$stopwords..english..,]
  words=gsub(";"," ",words)
  words=gsub(":"," ",words)
  words=gsub("'","",words)
  
  counter=as.data.frame(table(words))
  counter=counter[order(-counter$Freq),]
  counter=counter[1:top,]
  return(counter)
}

output=topx(2003,12,10)
colnames(output)=c("2003-12","freq2003-12")

for(year in 2004:2016)
{
  for(month in 1:12)
  {
    new=topx(year,month,10)
    colnames(new)=c(paste0(year,"-",month),paste0("freq",year,"-",month))
    output=cbind(output,new)
  }
}


#########################################
#plot development of word & applications
#########################################

library("ggplot2") #beautiful plots
library("reshape2") #beautiful plots (melt)

plotword=function(word)
{
  b=NULL
  for(year in 2004:2016)
  {
    for(month in 1:12)
    {
      test=data[as.numeric(data$year)==year & as.numeric(data$month)==month,]
      b=c(b,length(grep(word,test$headline_text)))
    }
  }
  for(month in 1:9)
  {
    test=data[as.numeric(data$year)==2017 & as.numeric(data$month)==month,]
    b=c(b,length(grep(word,test$headline_text)))
  }
  return(b)
}



###Prime Ministers of Australia

liste=c("howard","rudd","gillard","abbott","turnbull")

A=data.frame(seq(as.Date("2004/01/01"),by="month",length.out=165))


for(item in liste)
{
  out=plotword(item)
  A=cbind(A,out)
}

colnames(A)=c("date",liste)
charts=melt(A,id="date")
names(charts) <- c('x', 'func', 'value')
g=ggplot() +
  geom_line(data = charts, aes(x = x, y = value, color = func), size = 2)+
  xlab("year") +
  ylab("frequency")
g+scale_color_manual(values=c("#999999", "#FF0000", "#FF00D0","#0007FF","#09FF00"))



###interesting results

liste=c("donald","richard")

A=data.frame(seq(as.Date("2004/01/01"),by="month",length.out=165))


for(item in liste)
{
  out=plotword(item)
  A=cbind(A,out)
}

colnames(A)=c("date",liste)
charts=melt(A,id="date")
names(charts) <- c('x', 'func', 'value')
g=ggplot() +
  geom_line(data = charts, aes(x = x, y = value, color = func), size = 2)+
  xlab("year") +
  ylab("frequency")
g+scale_color_manual(values=c("#FF0000", "#09FF00"))


#################
# make wordcloud
#################

clean_corpus=function(data)
{
  words=NULL
  for(i in 1:(length(data[,1])))
  {
    words=c(words,unlist(strsplit(as.character(data$headline_text[i])," ")))
  }
  words <- Corpus(VectorSource(words))
  words = tm_map(words, removeWords, stopwords("english"))
  
  toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
  words = tm_map(words, toSpace,";")
  words = tm_map(words, toSpace,":")
  words = tm_map(words, toSpace, "'")
  return(words)
}


wordcloud_fun=function(corpus)
{
  dtm <- TermDocumentMatrix(corpus)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  head(d, 10)
  
  wordcloud(words = d$word, freq = d$freq, min.freq = 1,
            max.words=200, random.order=FALSE, rot.per=0.3, 
            colors=brewer.pal(12, "Dark2"))
}

my_corpus=clean_corpus(data)
wordcloud_fun(my_corpus)


#######################
#sentiment analysis
#######################

library(tidytext)
library(ggplot2)
library(lubridate) #month and year function
library(data.table) #for the inner join

pos_neg = get_sentiments("afinn")
pos_neg$X1=factor(pos_neg$word)
pos_neg$word=NULL
pos_neg=data.table(pos_neg,key="X1")


result=data.frame(matrix(ncol = 2, nrow = 0))

for(year in 2004:2016)
{
  for(month in 1:12)
  {
    test=data.frame(data[as.numeric(data$year)==year & as.numeric(data$month)==month,1])
    h=apply(test,MARGIN=1,function(x) strsplit(x," "))
    h1=unlist(h,recursive = F)
    names(h1)=rep(1,length(h1))
    score=NULL
    for(i in 1:length(h1))
    {
      h2=data.table(data.frame(h1[i]),key="X1")[pos_neg,nomatch=0]
      score=c(score,sum(h2$score))
    }
    out=data.frame(test,score)
    out$year=year
    out$month=month
    
    result=rbind(result,out[,2:4])
  }
  print(year)
}

h=NULL
h=aggregate(score ~ year + month,data=result,mean)
colnames(h)=c("year","month","mean")
h$date=as.Date(paste0(h$year,"-",h$month,"-01"))
h$pos=aggregate(score ~ year + month, data=result, function(x) length(x[x>0]))
h$neg=aggregate(score ~ year + month, data=result, function(x) length(x[x<0]))
h$pos=h$pos$score
h$neg=h$neg$score


g1=ggplot(h, aes(x=date,y=mean))+geom_line(size=2)

g2=ggplot(h, aes(x=date))+geom_line(aes(y=pos$score),size=2,colour="#00ff00")+
  geom_line(aes(y=neg$score),size=2,colour="#ff0000")




##################
#country analysis
##################

library(plyr)
library(ggplot2)
library(stringr) 


world=map_data("world")
world=world[world$region!="Antarctica",]

countries=world$region
countries=countries[!duplicated(countries)]
countries=data.frame(countries[order(countries)])
colnames(countries)="region"
countries$x=tolower(countries$region)
countries$x=gsub("usa","us",countries$x)

for(i in 1:length(countries$x))
{
  countries$count[i]=length(grep(paste0(" ",countries$x[i]," "),data$headline_text))
}

countries$count[countries$count==0]=1

countries=countries[order(countries$count,decreasing=T),]
countries[1:10,c(1,3)]

g=ggplot()
g=g+geom_map(data=world,map=world, aes(x=long,y=lat,map_id=region))
g=g + scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c())
g=g+labs(fill="# of apperances", title="The world in ABC News", x="", y="")
g=g + scale_fill_continuous(breaks=c(0,10,100,1000,10000),trans="log",low="#ffff00",high="#ff0000")
g=g + geom_map(data=countries, map=world,
             aes(fill=count, map_id=region),colour="grey",size=0.05)
g