
library(dplyr)
library(wordcloud) #wordcloud
library(data.table)
library(ggplot2)
library(tm) #text mining, used for stopwords

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


#################
#Theme for plots
#################

theme <- theme(axis.title.x = element_blank(),
               axis.text.x  = element_text(angle = 0, face = "bold",
                                           colour = "black", size = 12),
               axis.title.y = element_blank(),
               axis.text.y = element_text(angle = 0, 
                                          face = "bold", 
                                          colour = "black", 
                                          hjust = 0.5,
                                          size = 12),
               panel.grid.major = element_line(colour = 'grey82'),
               panel.grid.minor = element_line(colour = NA),
               panel.background = element_rect(fill = 'white'),
               strip.text = element_text(size = 8, face = "bold"),
               legend.title = element_text(colour = "black", size = 9, face = "bold"),
               legend.text = element_text(size = 8, face = "bold"))

##################
# get top words
##################



topx=function(year,month,top) {
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
    coord_flip()+ggtitle(paste0("Most frequent words in ",year,"-",month))+theme
  colnames(counter)=c(paste0(year,"-",month),paste0("freq",year,"-",month))
  return(list(table=counter,plot=g))
}

output <- topx(2006,12,10)
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

liste=list("chile","germany")

#A=data.frame(seq(as.Date("2004/01/01"),by="month",length.out=165))

l <- lapply(liste,plotword2)
do.call(cbind,l) %>%
  select(-matches("\\d$")) %>%
  ungroup() %>%
  mutate(date=as.Date(paste0(year,"-",month,"-01")),
         month=NULL,
         year=NULL) %>%
  tidyr::pivot_longer(-date,values_to="Freq",names_to="words") %>%
  ggplot(aes(x = date, y = Freq, color = words)) +
    geom_line(size = 1)+
    xlab("year") +
    ylab("frequency")+
    scale_color_manual(values=c("#999999", "#FF0000", "#FF00D0","#0007FF","#09FF00"))+
  theme


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

my_corpus <- data %>% filter(year==2014,month=="07") %>% clean_corpus()

wordcloud_fun(my_corpus)


#######################
#sentiment analysis
#######################

library(tidytext)
library(ggplot2)
library(lubridate) #month and year function
library(stringr)

pos_neg = get_sentiments("afinn")

words <- data %>% 
  mutate(title_id=row_number()) %>%
  unnest_tokens(word,headline_text)

sentiments <- words %>% 
  left_join(pos_neg,by="word") %>%
  group_by(year,month,day) %>%
  summarise(count=n_distinct(title_id),
            total_sentiment=sum(value,na.rm=T),
            neg=sum(value[value<0],na.rm=T),
            pos=sum(value[value>0],na.rm=T)) %>%
  mutate(date=as.Date(paste0(year,"-",month,"-",day)))

#Analyze

ggplot(data=sentiments,aes(x=date,y=count))+geom_line()+theme

# sentiments %>% group_by(year,month) %>%
#   summarise(date=max(date),
#             neg=sum(neg),
#             pos=sum(pos)) %>%
sentiments %>% filter(year==2014,month=='07') %>%
ggplot(aes(x=date))+
  geom_line(aes(y=pos),size=1,colour="#00ff00")+
  geom_line(aes(y=-neg),size=1,colour="#ff0000")+
  theme




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