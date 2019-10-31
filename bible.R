#install inportant packages
install.packages('topicmodels')
install.packages('tidytext')

#import important libraries
library(tm)
library(tidytext)
library(topicmodels)
library(tidyverse)
library(dplyr)
require(XML)

#import the bible datasets 
bible_xml<-"http://simoncozens.github.io/open-source-bible-data/cooked/simple-xml/kjv.xml"
data <- xmlParse(bible_xml)
#convert to list
xml_data <- xmlToList(data)


#vector
bible_vect_source = tm::VectorSource(xml_data)

#This creates the actually corpus
bible_corp = tm::VCorpus(bible_vect_source)



#create a function that clean the datasets and remove the stopword and useless words
clean_corpus <- function(corpus) {  
  # Remove punctuation
  corpus <- tm_map(corpus, removePunctuation)
  # convert to lower case
  corpus <- tm_map(corpus, content_transformer(tolower))
  #remove stopwords from the bible
  corpus <- tm_map(corpus, removeWords, c(stopwords("en"),"upon","will", "hath","listpan","cnum","verse","came","went","thou", "attrs","listtext","unto","said","text","thy","thee","shall")) 
  # stripWhitespace
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
}

#clean the bible datasets using the function
clean_bible_corp = clean_corpus(bible_corp)


clean_content <- vector("character", length(clean_bible_corp))


#DocumentTermMatrix
bible_dtm <- tm::DocumentTermMatrix(clean_bible_corp)

#Latent Dirichlet Allocation with the topicmodels package
bible_lda = topicmodels::LDA(bible_dtm, k =6, control = list(seed = 1))

#from tidytext to separate each bible  title into words
bible_topics = tidytext::tidy(bible_lda, matrix = 'beta')

bible_top_terms <- bible_topics %>% 
  group_by(topic) %>% 
  top_n(10, beta) %>% 
  ungroup() %>% 
  arrange(topic, -beta)

#visualize the result
bible_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()

