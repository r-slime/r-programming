library(tidyverse) # for easy data wrangling
library(janitor) # for cleaning data frame names 
library(tidytext) # for easy text data wrangling
library(widyr) # for pairwise correlation 
library(igraph) # for data visualization
library(ggraph) # for data visualization

tinder <- read_csv("tinderswindlerversion2.csv") # import data 


tinder_words <- tinder %>% # make new df "tinder_words" to store the individual words of the tweets
  unnest_tokens(output = word, input = text) %>% # seperate tweets into individual words
  anti_join(stop_words, by = "word") %>% # remove stop words
  filter(str_detect(word, "[:alpha:]")) %>% # filter out non characters 
  distinct()


tweets_that_mention_word <- tinder_words %>% # make new df from "tinder_words" to see how frequency (count) of word usage
  count(word, name = "number_of_tweets") %>% # count 
  filter(number_of_tweets >= 250)


tweet_correlations <- tinder_words %>% # make new df called "tweet_correlations" to store correlations
  semi_join(tweets_that_mention_word, by = "word") %>% # semi join "tinder_words" and tweets_that_mention_word df by word 
  pairwise_cor(item = word, feature = user_name) %>% # pairwise correlation for words and artists
  filter(correlation >= 0.11) # filter out low correlations


graph_from_data_frame(d = tweet_correlations,
                      vertices = tweets_that_mention_word %>%
                        semi_join(tweet_correlations, by = c("word" = "item1"))) %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(alpha = correlation)) + 
  geom_node_point() +
  geom_node_text(aes(color = number_of_tweets, label = name), repel = TRUE) + 
  labs(title = "The Tinder Swindler: Words from 100,000 Tweets") +
  scale_colour_gradientn(colours=c("#3e3b92", "#f44369")) +
  theme(panel.background = element_rect(fill = "#eaeaea"),
        plot.background = element_rect(fill = "#ffffff"))
