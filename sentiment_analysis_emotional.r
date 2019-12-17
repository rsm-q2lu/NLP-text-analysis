

## load libraries 

library(tidyverse)
library(scales)
library(forcats)
library(tidytext)
#install.packages("textdata")
library(textdata)

## example 
sentences <- data_frame(sentence = 1:3,
                        text = c("I just love this burger. It is amazing. 
                                 America has the best burgers. I just love them. 
                                 They are great. Someone says otherwise,  
                                 they are a loser",
                                 "This burger was terrible - bad taste all around.
                                 But I did like the music in the bar.",
                                 "I had a burger yesterday - it was ok. 
                                 I ate all of it"))

t=tidy.sentences <- sentences %>%
  unnest_tokens(word,text)


## join sentiment lexicon
t=tidy.sentences %>%
  inner_join(get_sentiments("bing"),by="word")  
#bing doesn't contain OK. Nothing left in sentence 3.

t=tidy.sentences %>%
  count(sentence,word) %>%
  inner_join(get_sentiments("bing"),by="word") 


t=tidy.sentences %>%
  count(sentence,word) %>%
  inner_join(get_sentiments("bing"),by="word") %>%
  group_by(sentence,sentiment) %>%
  summarize(total=sum(n))


## net sentiment 
tidy.sentences %>%
  count(sentence,word) %>%
  inner_join(get_sentiments("bing"),by="word") %>%
  group_by(sentence,sentiment) %>%
  summarize(total=sum(n)) %>%
  spread(sentiment,total) %>%
  mutate(net.positve=positive-negative)




#### Convention speeches Cliton vs Trump:



speech <- read_rds('data/convention_speeches.rds')

tidy.speech <- speech %>%
  unnest_tokens(word,text)

total.terms.speaker <- tidy.speech %>%
  count(speaker)

## plot sentiments
sentiment.orientation <- data.frame(orientation = c(rep("Positive",5),rep("Negative",5)),
                                    sentiment = c("anticipation","joy","positive","trust","surprise","anger","disgust","fear","negative","sadness"))

tidy.speech %>%
  count(speaker,word) %>%
  inner_join(get_sentiments("nrc"),by=c("word")) %>%
  group_by(speaker,sentiment) %>%
  summarize(total=sum(n)) %>%
  inner_join(total.terms.speaker) %>%
  mutate(relative.sentiment=total/n) %>%
  inner_join(sentiment.orientation,by='sentiment') %>%
  ggplot(aes(x=sentiment,y=relative.sentiment,fill=speaker)) + geom_bar(stat='identity',position='dodge') + 
  facet_wrap(~orientation,scales='free_x')+
  ylab('Relative Sentiment')+
  labs(title='Sentiment of Convention Acceptance Speeches',
       subtitle='Hillary Clinton vs. Donald Trump')+
  scale_y_continuous(labels=percent)

#### Cliton's speech is more positive than trump's.


### sentiments on Las Vegas Hotels Reviews from Tripadvisor


reviews <- read_rds('data/reviewsTripAll.rds')

meta.data <- reviews %>%
  select(hotel,reviewID,reviewRating)

reviewsTidy <- reviews %>%
  unnest_tokens(word,reviewText) %>%
  count(reviewID,word)

term.hotel <- reviewsTidy %>%
  inner_join(meta.data,by='reviewID') %>%
  group_by(hotel) %>%
  summarize(n.hotel=sum(n)) 

## sentiments by hotel 

bing <- get_sentiments("bing") 

hotel.sentiment <- reviewsTidy %>%
  inner_join(bing,by=c("word")) %>%
  inner_join(meta.data,by='reviewID') %>%
  group_by(hotel,sentiment) %>%
  summarize(total=sum(n)) %>%
  inner_join(term.hotel,by='hotel') %>%
  mutate(relative.sentiment = total/n.hotel)


hotel.sentiment %>%
  ggplot(aes(x=sentiment,y=relative.sentiment,fill=sentiment)) + geom_bar(stat='identity') + 
  facet_wrap(~hotel)+ theme(axis.text.x = element_text(angle = 45, hjust = 1))


## plot of net sentiment
hotel.sentiment %>%
  select(sentiment,relative.sentiment,hotel) %>%
  spread(sentiment,relative.sentiment) %>%
  mutate(net.pos = positive-negative) %>%
  ggplot(aes(x=fct_reorder(hotel,net.pos),y=net.pos)) + geom_point(size=4) + coord_flip()+
  scale_y_continuous(labels=percent)+ylab('Net Positive Sentiment')+xlab('Resort')



#### Palazzo showed the highest positive sentiment while Flamingo showed the lowest. People preferred Palazzo than Flamingo.




## wider range of sentiments
nrc <- get_sentiments("nrc")

hotel.sentiment <- reviewsTidy %>%
  inner_join(nrc,by=c("word")) %>%
  left_join(meta.data,by='reviewID') %>%
  group_by(hotel,sentiment) %>%
  summarize(total=sum(n)) %>%
  inner_join(term.hotel,by='hotel') %>%
  mutate(relative.sentiment = total/n.hotel)


hotel.sentiment %>%
  ggplot(aes(x=sentiment,y=relative.sentiment,fill=sentiment)) + geom_bar(stat='identity') + 
  facet_wrap(~hotel,nrow=3)+ theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  theme(legend.position="none")


#### Different hotels have their own problems. Hotels can analyze the keywords of the sentiment and adjust the brand to express a different feeling.




