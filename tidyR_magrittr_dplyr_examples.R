## Data Processing with dplyr, magrittr, tidyr, ggplot2
## notes from tutorial at http://zevross.com/

library(dplyr)
library(bigrquery)

##dplyr way to find the most common and least common word in shapespeare henry
#extract data
sql<-"select * from [publicdata:samples.shakespeare]"
shakespeare <-query_exec(sql, project = "dark-ola-44",max_pages=Inf)

shakespeare<-mutate(shakespeare, word=tolower(word))
grp<-group_by(shakespeare, word, corpus, corpus_date)
shakespeare<-summarize(grp, word_count=sum(word_count))
head(filter(shakespeare, tolower(word)=="henry"))

#what are the most and least popular words are by computing 
#the total times each word occurs across all of Shakespeare's works

grp <- group_by(shakespeare, word)
cnts <- summarize(grp, count=n(), total = sum(word_count))
word.ount <- arrange(cnts, desc(total))

##magrittr way

library(magrittr)
word.count <- group_by(shakespeare, word) %>%
  summarize(count=n(), total = sum(word_count)) %>%
  arrange(desc(total))

head(word.count)

word.count <- filter(word.count, nchar(word)>4, count<42)

#OR (using magrittr)
word.count %<>% filter(nchar(word)>4, count<42)
head(word.count)


top8<-word.count$word[1:8]
top8 <- filter(shakespeare, word%in%top8)%>%
  select(-corpus_date)

## Note that Hadley Wickham suggests simpler code as
## top8<-shakespeare %>% semi_join(head(word.count,8))

head(top8)

## tidyr

library(tidyr)
top8.wide<- spread(top8, word, word_count)
head(top8.wide, n=10)


# 0 instead of NAs
top8.wide<- spread(top8, word, word_count, fill=0)


# the older traditional way to do the same:

top8<-data.frame(top8) # needs to be a data frame
tmp<-reshape(top8, v.names="word_count", idvar="corpus", timevar="word", direction="wide")
head(tmp)
head(top8.wide, n=10)


## gpplot workflow
library(ggplot2)
ggplot(word.count, aes(count, total))+geom_point(color="firebrick")+
  labs(x="Number of works a word appears in", 
       y="Total number of times word appears")

ggplot(word.count, aes(count, total))+geom_point(color="firebrick")+
  labs(x="Number of works a word appears in", 
       y="Total number of times word appears")+
  scale_y_log10()

ggplot(word.count, aes(count, total))+geom_point(color="firebrick")+
  labs(x="Number of works a word appears in", 
       y="Total number of times word appears")+
  scale_y_log10()+stat_smooth()


word.stats1<-group_by(word.count, count, total)%>%
  summarize(
    cnttot=n())

head(word.stats1)


word.count<-inner_join(word.count, word.stats1, by=c("count", "total"))
head(word.count)


# sizing words based on a new variable

ggplot(word.count, aes(count, total, size=cnttot))+
  geom_point(color="firebrick")+
  labs(x="Number of works a word appears in", 
       y="Total number of times word appears")+
  scale_y_log10()+
  scale_size_area(max_size=20)+ # scale circles
  theme(legend.position="none")# turn off legend


ggplot(word.count, aes(count, total))+
  geom_jitter(alpha=0.1,position = position_jitter(width = .2), color="firebrick")+
  labs(x="Number of works a word appears in", 
       y="Total number of time word appears")+
  scale_y_log10()

ggplot(word.count, aes(count, total))+
  geom_jitter(alpha=0.1,position = position_jitter(width = .2), color="firebrick")+
  labs(x="Number of works a word appears in", 
       y="Total number of time word appears")+
  scale_y_log10()+
  stat_smooth()


word.stats2<-group_by(word.count, count)%>%
  summarize(max=max(total), min=min(total))

head(word.stats1)
word.count<-inner_join(word.count, word.stats2, by=c("count"))

ggplot(word.count, aes(count, total))+
  geom_jitter(alpha=0.1,position = position_jitter(width = .2), color="firebrick")+
  labs(x="Number of works a word appears in", 
       y="Total number of time word appears")+
  scale_y_log10()+
  geom_text(data=filter(word.count, total==max), 
            aes(count, total, label=word), size=3)+
  geom_text(data=filter(word.count, total==37 & min==37), 
            aes(count, total, label=word), position=position_jitter(height=0.2), size=3)

