#####################################
#####################################
###########Tien-Wei Hsu##############
#Create for A3: Business Insight Report
##12/02/2021

##### require library####
library("twitteR")
library("tm")
library("NLP")

####twitter#####
#necessary file for Windows
# setwd("/Users/thomaskurnicki/Desktop/Text analytics class/Day 1/Twitter API :: to be posted")
setwd("")
# download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")

#to get your consumerKey and consumerSecret see the twitteR documentation for instructions
consumer_key <- your key
consumer_secret <- your secret
access_token <- your token
access_secret <- your secret

# setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
# tesla <- twitteR::searchTwitter('#tesla', n = 2000, since = '2017-01-01',
#                                retryOnRateLimit = 1e3, lang = "en") #include_rt = FALSE
# d = twitteR::twListToDF(tesla)
# 
# honda <- twitteR::searchTwitter('#honda', n = 2000, since = '2017-01-01',
#                                  retryOnRateLimit = 1e3, lang = "en") ### honda
# e = twitteR::twListToDF(honda)
# 
# ford <- twitteR::searchTwitter('#ford', n = 2000, since = '2017-01-01',
#                                retryOnRateLimit = 1e3, lang = "en") ### ford
# f = twitteR::twListToDF(ford)
# # 
# write.csv(d, "C:/Users/gostj/Desktop/Text Analytics and Natural Language Processing (NLP)/individual/d.csv", row.names = FALSE)
# write.csv(e, "C:/Users/gostj/Desktop/Text Analytics and Natural Language Processing (NLP)/individual/e.csv", row.names = FALSE)
# write.csv(f,  "C:/Users/gostj/Desktop/Text Analytics and Natural Language Processing (NLP)/individual/f.csv", row.names = FALSE)

##### d, e ,f files
d <- read.csv("C:/Users/gostj/Desktop/Text Analytics and Natural Language Processing (NLP)/individual/d.csv")
e <- read.csv("C:/Users/gostj/Desktop/Text Analytics and Natural Language Processing (NLP)/individual/e.csv")
f <- read.csv("C:/Users/gostj/Desktop/Text Analytics and Natural Language Processing (NLP)/individual/f.csv")

##### require library####
library(dplyr)
library(janeaustenr)
library(tidytext)
library(stringr)
library(tidyverse)
library(magrittr)
library(tidyr)
data(stop_words)

###### add my words in stop_words 

stop_words2 <- data.frame(word=c("https", "t.co", "HONDA",  "rt", "it's", "125",
                                 "FORD","Tesla","TESLA" , "tesla", "ford", "honda"
                                 ,"12","5","14","00","car","1","3","2705","0001f680","0001f525"
                                 ,"0001f415","fe0f","20e3","2","day"), 
                          lexicon=c("CUSTOM","CUSTOM", "CUSTOM", "CUSTOM", "CUSTOM", "CUSTOM"
                                    ,"CUSTOM","CUSTOM","CUSTOM","CUSTOM", "CUSTOM","CUSTOM", 
                                    "CUSTOM", "CUSTOM", "CUSTOM", "CUSTOM","CUSTOM","CUSTOM","CUSTOM"
                                    ,"CUSTOM","CUSTOM","CUSTOM","CUSTOM","CUSTOM","CUSTOM","CUSTOM","CUSTOM"))
stop_words <- rbind(stop_words, stop_words2)

##### Tidy Framework for TSLA ##### 

tesla_words <- d %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words,by = 'word')
   

library(ggplot2)

### top 15 words
tesla_words %>% 
  count(word, sort = TRUE) %>% 
  top_n(15) %>% 
  mutate(word=reorder (word,n)) %>% 
  ggplot(aes(x = n,y = word)) +
  geom_col() +
  labs(X = "count", y = "words") +
  ggtitle("Frequency Table for Tesla")
 

##### Framework for honda ##### 
honda_words <- e %>%
  unnest_tokens(word, text) %>% 
  anti_join(stop_words, by = 'word')
  

#top 15 for honda 
honda_words %>% 
  count(word, sort = TRUE) %>% 
  top_n(15) %>% 
  mutate(word=reorder (word,n)) %>% 
  ggplot(aes(n,word)) +
  geom_col() +
  labs(X = "count", y = "words") +
  ggtitle("Frequency Table for honda")

##### Framework for ford##### 
ford_words <- f %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = 'word')
  
#top 15 for ford   
ford_words %>% 
  count(word, sort = TRUE) %>% 
  top_n(15) %>% 
  mutate(word=reorder (word,n)) %>% 
  ggplot(aes(n,word)) +
  geom_col() +
  labs(X = "count", y = "words") +
  ggtitle("Frequency Table for Ford")


###### creat a frequency

frequency <- bind_rows(mutate(tesla_words, tag = "tesla"),
                       mutate(honda_words, tag = "honda"), 
                       mutate(ford_words, tag = "ford")) %>% 
  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(tag, word) %>%
  group_by(tag) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  pivot_wider(names_from = tag, values_from = proportion) %>%
  pivot_longer(`honda`:`ford`,
               names_to = "tag", values_to = "proportion")

library(scales)  
  
# expect a warning about rows with missing values being removed
ggplot(frequency, aes(x = proportion, y = `tesla`, 
                      color = abs(`tesla` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), 
                       low = "darkslategray4", high = "gray75") +
  facet_wrap(~tag, ncol = 2) +
  theme(legend.position="none") +
  labs(y = "tesla", x = NULL)

###cor test

cor.test(data = frequency[frequency$tag == "honda",],
         ~ proportion + `tesla`)

cor.test(data = frequency[frequency$tag == "ford",], 
         ~ proportion + `tesla`)


#### sentiments
library(tidytext)
get_sentiments("afinn")
get_sentiments("bing")
get_sentiments("nrc")

library(janeaustenr)
library(dplyr)
library(stringr)

nrc_joy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")

######
tesla_words %>%
  inner_join(nrc_joy) %>%
  count(word, sort = TRUE)
######
honda_words %>%
  inner_join(nrc_joy) %>%
  count(word, sort = TRUE)
######
ford_words %>%
  inner_join(nrc_joy) %>%
  count(word, sort = TRUE)


library(tidyr)
#######
tesla_tags <- d %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words,by = 'word') %>% 
  count(word, sort = TRUE)

tesla_tags$tag <- c("tesla")

#
honda_tags <- e %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words,by = 'word') %>% 
  count(word, sort = TRUE)

honda_tags$tag <- c("honda")
#
ford_tags <- f %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words,by = 'word') %>% 
  count(word, sort = TRUE)

ford_tags$tag <- c("ford")

######## need to ask index
car_tags <- rbind(tesla_tags,honda_tags, ford_tags)

total_sentiment <- car_tags %>%
  inner_join(get_sentiments("bing")) %>%
  count(tag, index = n %% 40 , sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  mutate(sentiment = positive - negative)


ggplot(total_sentiment, aes(index, sentiment, fill = tag)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~tag, ncol = 2, scales = "free_x")


####The statistic tf-idf is intended to measure how important a word is to a document in 
####    collection (or corpus) of documents, for example, to one novel in a collection of novels 
####    or to one website in a collection of websites.

total_tags <- car_tags %>%   
  group_by(tag) %>% 
  summarize(total = sum(n))

car_tags <- left_join(car_tags, total_tags) ##book_words

# library(ggplot2)

ggplot(car_tags, aes(n/total, fill = tag)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~tag, ncol = 2, scales = "free_y")

####Zipf's law
####Zipf's law states that the frequency that a word appears is inversely proportional to its rank.

freq_by_rank <- car_tags %>% 
  group_by(tag) %>% 
  mutate(rank = row_number(), 
         `term frequency` = n/total) %>%
  ungroup()

###The rank column here tells us the rank of each word within the frequency table
## it is in log-log coordinates. 
## term Frequency-Rank line
freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = tag)) + 
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()

rank_subset <- freq_by_rank %>% 
  filter(rank < 500,
         rank > 10)


lm(log10(`term frequency`) ~ log10(rank), data = rank_subset)

##plot this fitted power law 
freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = tag)) + 
  geom_abline(intercept = -0.62, slope = -1.1, 
              color = "gray50", linetype = 2) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()


###3.3 The bind_tf_idf() function

tags_tf_idf <- car_tags %>%
  bind_tf_idf(word, tag, n)

tags_tf_idf

###Let's look at terms with high tf-idf in twitters works.

tags_tf_idf %>%
  select(-total) %>%
  arrange(desc(tf_idf))

###Some of the values for idf are the same for different terms because there are 3 documents in this corpus

library(forcats)

tags_tf_idf %>%
  group_by(tag) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = tag)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~tag, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)

###^^^ the most important to each tag and most twitters would likely agree. 


####3.4 A corpus of physics texts

car_tags2 <- car_tags[,-4]

# create level, but some meaningless words
plot_car <- car_tags2 %>%
  bind_tf_idf(word, tag, n) %>%
  mutate(tag = factor(tag, levels = c("tesla","honda", "ford")))

## havent do anti join
plot_car %>% 
  group_by(tag) %>% 
  slice_max(tf_idf, n = 15) %>% 
  ungroup() %>%
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(tf_idf, word, fill = tag)) +
  geom_col(show.legend = FALSE) +
  labs(x = "tf-idf", y = NULL) +
  facet_wrap(~tag, ncol = 2, scales = "free")

####move meaningless words
mystopwords <- tibble(word = c("125","run","1??????","did", "2??????", "atj","nsx", warning=FALSE))

car_tags2 <- anti_join(car_tags2, mystopwords,  
                           by = "word")

plot_car <- car_tags2 %>%
  bind_tf_idf(word, tag, n) %>%
  mutate(word = str_remove_all(word, "_")) %>%
  group_by(tag) %>% 
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  mutate(word = reorder_within(word, tf_idf, tag)) %>%
  mutate(tag = factor(tag, levels = c("tesla","honda", "ford")))

### delete the signal 

ggplot(plot_car, aes(word, tf_idf, fill = tag)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~tag, ncol = 2, scales = "free") +
  coord_flip() +
  scale_x_reordered()


###ti-idf part finished

###4.1 Tokenizing by n-gram !!!!!!!!!!!!!!!!!!!!!bug

car_bigrams %>%
  count(bigram, sort = TRUE)


# library(dplyr)
# library(tidytext)
# library(janeaustenr)

### deal the data 
d$tag <- c("tesla")
tesla_bigrams <-  d %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) 

tesla_bigrams_count <- tesla_bigrams %>%
                       count(bigram, sort = TRUE)

tesla_bigrams_count$tag <- c("tesla")
tesla_bigrams_count <- tesla_bigrams_count[,-2]
#
e$tag <- c("honda")  
honda_bigrams <-  e %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) 

honda_bigrams_count <- honda_bigrams %>%
                       count(bigram, sort = TRUE)

honda_bigrams_count$tag <- c("honda")
honda_bigrams_count <- honda_bigrams_count[,-2]
#

f$tag <- c("ford")  
ford_bigrams <-  f %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) 


ford_bigrams_count <- ford_bigrams %>%
                      count(bigram, sort = TRUE)

ford_bigrams_count$tag <- c("ford")
ford_bigrams_count <- ford_bigrams_count[,-2]

##### 
  
bigrams_united <- rbind(tesla_bigrams_count,honda_bigrams_count,ford_bigrams_count)


###time to use tidyr's separate() to deal the data ## 

library(tidyr)

bigrams_separated <- bigrams_united %>%  ##bug
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# new bigram counts:
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

bigram_counts

## use united to comibned the word, to inverse the separate()

bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

#####  consecutive sequences of 3 words
## tri_word, do not have enough data to support
# combined_def <- rbind(d,e,f)
# 
# 
# car_bigrams_3 <- combined_def %>%
#                   unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
#                   separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
#                   filter(!word1 %in% stop_words$word,
#                        !word2 %in% stop_words$word,
#                        !word3 %in% stop_words$word) %>%
#                   count(word1, word2, word3, sort = TRUE)


##4.1.2 Analyzing bigrams 

# bigrams_filtered %>%
#   filter(word2 == "electric") %>%
#   count(tag, word1, sort = TRUE)
# 
# #A bigram can also be treated as a term in a document in the same way that we treated individual words. 
# 
# bigram_tf_idf <- bigrams_united %>%
#   count(tag, bigram) %>%
#   bind_tf_idf(bigram, tag, n) %>%
#   arrange(desc(tf_idf))
# 
# bigram_tf_idf
# 
# #bug
# 
# bigram_tf_idf %>% 
#   group_by(tag) %>% 
#   slice_max(tf_idf, n = 5) %>% 
#   ungroup() %>%
#   mutate(word = reorder(bigram, tf_idf)) %>%
#   ggplot(aes(tf_idf, bigram, fill = tag)) +
#   geom_col(show.legend = FALSE) +
#   labs(x = "tf-idf", y = NULL) +
#   facet_wrap(~tag, ncol = 2, scales = "free")
# 
# ## havent do anti join
# 
# 
# #bug

#4.1.3 Using bigrams to provide context in sentiment analysis

#use the AFINN lexicon for sentiment analysis, which you may recall gives a numeric sentiment value 
#for each word, with positive or negative numbers indicating the direction of the sentiment.

bigrams_separated

bigrams_separated %>%
  filter(word1 == "car") %>%
  count(word1, word2, sort = TRUE)

AFINN <- get_sentiments("afinn")

AFINN

not_words <- bigrams_separated %>%
  filter(word1 == "not") %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word2, value, sort = TRUE)

not_words

##Words preceded by 'not' that had the greatest contribution to sentiment values,
#in either a positive or negative direction

not_words %>%
  mutate(contribution = n * value) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(n * value, word2, fill = n * value > 0)) +
  geom_col(show.legend = FALSE) +
  labs(x = "Sentiment value * number of occurrences",
       y = "Words preceded by \"not\"")

#if we put 4 negative words 
negation_words <- c("not", "no", "never", "without")

negated_words <- bigrams_separated %>%
  filter(word1 %in% negation_words) %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word1, word2, value, sort = TRUE)


negated_words%>%
  mutate(contribution = n * value) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(n * value, word2, fill = n * value > 0)) +
  geom_col(show.legend = FALSE) +
  labs(x = "Sentiment value * number of occurrences",
       y = "Words preceded by \"not,no,never,without\"")

###do not use for this assignment

#4.1.4 Visualizing a network of bigrams with ggraph

# filter for only relatively common combinations

library(igraph)

bigram_counts

# filter for only relatively common combinations
bigram_graph <- bigram_counts %>%
  filter(n > 1) %>%
  graph_from_data_frame()

bigram_graph

####### Common bigrams in our data, showing word was a stop word
library(ggraph)
set.seed(2020)

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

######Common bigrams , with some polishing

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

####finished
