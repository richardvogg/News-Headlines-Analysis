
library(dplyr)
library(wordcloud) #wordcloud
library(data.table)
library(ggplot2)

#################
# load data
#################

data <- fread("C:/Richard/R and Python/Kaggle/News-Dataset/abcnews-date-text.csv",sep=",") %>%
  as.data.frame()

data <- data %>%
  mutate(year=substr(publish_date,1,4),
         month=substr(publish_date,5,6),
         day=substr(publish_date,7,8),
         publish_date=NULL)




##################
# get top words
##################

library(tm) #text mining, used for stopwords
library(plyr) #data base, count etc.

topx=function(year,month,top)
{
  test=data[as.numeric(data$year)==year & as.numeric(data$month)==month,]
  
  words <- strsplit(test$headline_text," ") %>% unlist()
  
  words=data.frame(words)
  stopwords_en=data.frame(stopwords("english"))
  words=words[!words$words%in%stopwords_en$stopwords..english..,]
  words=gsub(";"," ",words)
  words=gsub(":"," ",words)
  words=gsub("'","",words)
  
  counter=as.data.frame(table(words))
  counter=counter[order(-counter$Freq),]
  counter=counter[1:top,]
  
  
  g <- ggplot(counter,aes(x=reorder(words,Freq),y=Freq))+
    geom_bar(stat="identity")+
    coord_flip()
  colnames(counter)=c(paste0(year,"-",month),paste0("freq",year,"-",month))
  return(list(table=counter,plot=g))
}

output <- topx(2005,12,10)
output$plot
output$table


#########################################
#plot development of word & applications
#########################################


plotword2 <- function(word) {
  output <- data %>% dplyr::group_by(year,month) %>%
    dplyr::summarise(freq=sum(grepl(word,headline_text)))
  names(output) <- c("year","month",word)
  return(output)
}

plotword2("hills")

###Prime Ministers of Australia

liste=list("chile")

#A=data.frame(seq(as.Date("2004/01/01"),by="month",length.out=165))

l <- lapply(liste,plotword2)
do.call(cbind,l) %>%
  select(-matches("\\d$")) %>%
  mutate(date=as.Date(paste0(year,"-",month,"-01")),
         month=NULL,
         year=NULL) %>%
  tidyr::pivot_longer(-date,values_to="Freq",names_to="words") %>%
  ggplot(aes(x = date, y = Freq, color = words)) +
    geom_line(size = 1)+
    xlab("year") +
    ylab("frequency")+
    scale_color_manual(values=c("#999999", "#FF0000", "#FF00D0","#0007FF","#09FF00"))


#################
# make wordcloud
#################

clean_corpus <- function(data) {
  toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
  words <- strsplit(data$headline_text," ") %>% unlist() %>%
    VectorSource() %>% Corpus() %>%
    tm_map(tm::removeWords, stopwords("english")) %>%
    tm_map(toSpace,";") %>%
    tm_map(toSpace,":") %>%
    tm_map(toSpace, "'")
  return(words)
}


wordcloud_fun=function(corpus)
{
  v <- TermDocumentMatrix(corpus) %>%
    as.matrix() %>%
    {sort(rowSums(.),decreasing=TRUE)}
  d <- data.frame(word = names(v),freq=v)
  rm(v)
  wordcloud::wordcloud(words = d$word, freq = d$freq, min.freq = 1,
            max.words=200, random.order=FALSE, rot.per=0.3, 
            colors=brewer.pal(12, "Dark2"))
  
}

my_corpus <- data %>% filter(year==2015,month=="01") %>% clean_corpus()

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