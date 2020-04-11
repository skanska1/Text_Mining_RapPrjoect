library(textreadr)
library(dplyr)
library(tidyverse)
library(reshape2)
# text mining library
library(tidytext)
# plotting packages
library(igraph)
library(ggraph)
library(ggplot2)
#Corpus
library(tm)
#Word cloud
library(wordcloud)
#lyrics
library(genius)

#eminem Album
eminem <- genius_album(artist = "Eminem", album = "Kamikaze")
tidy_emi <- eminem %>%
  unnest_tokens(word, lyric) %>%
  anti_join(stop_words)

#Kendrick Album
Kendrick <- genius_album(artist = "Kendrick Lamar", album = "DAMN")
tidy_ken <- Kendrick %>%
  unnest_tokens(word, lyric) %>%
  anti_join(stop_words)

#migos Album
migos <- genius_album(artist = "Migos", album = "Culture")
tidy_mig <- migos %>%
  unnest_tokens(word, lyric) %>%
  anti_join(stop_words)

### Combining all 3 tidy data frames 

df <- bind_rows(mutate(tidy_emi, author="Eminem"),
                mutate(tidy_ken, author= "Kendrick"),
                mutate(tidy_mig, author="Migos")) #closing bind_rows

#Frequency of the words
frequency <- df %>%
  unnest_tokens(word, word)%>%
  count(word, sort=T)

#World cloud
frequency %>%
  with(wordcloud(word, n, max.words = 100))

#sentiment NRC
df_nrc <- df %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=150,
                   scale=c(0.8,0.8),
                   fixed.asp=TRUE,
                   title.size = 1)                   
#sentiment bing
df_bing <- df %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup() #%>%

#positive and negative graph
df_bing %>%
  group_by(sentiment) %>%
  top_n(30) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()

#number of negative words
df_bing_neg <- df %>%
  unnest_tokens(word, word) %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup() %>%
  filter(sentiment =='negative')

#number of positive words
df_bing_pos <- df %>%
  unnest_tokens(word, word) %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup() %>%
  filter(sentiment =='positive')

df %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "black"),
                   max.words=200,
                   scale=c(1,1),
                   fixed.asp=TRUE,
                   title.size =1
  )


################# TF_IDF ##########################

rap_tfidf<- inner_join(df, frequency,by= 'word')

rap_tfidf2 <- rap_tfidf %>%
  bind_tf_idf(word, author, n)%>%
  arrange(desc(tf_idf))%>% 
  filter (tf_idf > 0.0000001)

################# Bigram ##########################

my_bigrams <- df %>%
  unnest_tokens(bigram, word, token = "ngrams", n=2)
bigrams_separated <- my_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")
bigram_counts <- bigrams_separated%>%
  count(word1, word2, sort = TRUE)

bigram_graph <- bigram_counts %>%
  filter(n>5) %>%
  graph_from_data_frame()

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)





