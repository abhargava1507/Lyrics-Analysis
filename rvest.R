install.packages("tidyverse")
library(tidyverse)
install.packages("rvest")
library(rvest)
library(stringr)
install.packages("rebus")
library(rebus)
install.packages("lubridate")
library(lubridate)

url <- "http://ie.trustpilot.com/review/www.amazon.com"

get_last_page <- function(html) {
  pages_data <- read_html(html) %>% 
    html_nodes(".pagination-page") %>%
    html_text()
  
  pages_data[length(pages_data)] %>%
    as.numeric()
}

get_last_page(url)

status <- read_html(url) %>% 
  html_nodes(".review-date--tooltip-target") %>% 
  # The status information is this time a tag attribute
  html_attrs() %>%
  map(2)

movie_url <- "http://www.imdb.com/search/title?title_type=feature&release_date=2016-01-01,2016-12-31"

name_movie <- read_html(movie_url) %>%
  html_nodes(".lister-item-header a") %>%
  html_text()
movie_dataset <- data.frame(name = name_movie)

music_data <- read.csv("./Downloads/prince_raw_data.csv")

expand_contractions <- function(file) {
  
  file = gsub("won't","will not",file)
  file <- gsub("can't", "can not", file)
  file <- gsub("n't", " not", file)
  file <- gsub("'ll", " will", file)
  file <- gsub("'re", " are", file)
  file <- gsub("'ve", " have", file)
  file <- gsub("'m", " am", file)
  file <- gsub("'d", " would", file)
  file <- gsub("'s", "", file)
  
  return(file)
}

music_data$text <- sapply(music_data$text, expand_contractions)

remove_special_char <- function(file) {
  
  gsub("[^a-zA-Z0-9 ]", " ",file)
  
}

music_data$text <- sapply(music_data$text, remove_special_char)
music_data$text <- sapply(music_data$text, tolower)

music_data$decade <- ifelse(music_data$year %in% 1970:1979, "1970s",
                            ifelse(music_data$year %in% 1980:1989, "1980s",
                                   ifelse(music_data$year %in% 1980:1999, "1990s",
                                          ifelse(music_data$year %in% 2000:2009, "2000s",
                                                 ifelse(music_data$year %in% 2010:2019, "2010s","NA")))))


music_data$chart_level <- ifelse(music_data$peak %in% 1:10, "Top 10",
                                 ifelse(music_data$peak %in% 11:100, "Top 100","Uncharted"))
music_data$Charted <- ifelse(music_data$chart_level == "Uncharted", "Uncharted", "Charted")

prince_new_file <- write_xlsx(music_data, "./Downloads/prince_new_data.xlsx")

song_stats <- music_data %>%
  select(decade, Charted) %>%
  filter(decade != "NA") %>%
  group_by(decade, Charted) %>%
  summarise(Song_Count = n())

ggplot(song_stats) + geom_bar(aes(x = decade, y = Song_Count, fill = Charted), stat = "identity")

song_stats_2 <- music_data %>%
  select(decade, chart_level) %>%
  filter(chart_level != "Uncharted") %>%
  group_by(decade, chart_level) %>%
  summarise(Song_Count= n())

ggplot(song_stats_2) + geom_bar(aes(x = decade, y = Song_Count, fill = chart_level), stat = "identity")

no1_songs <- music_data %>%
  select(song, year) %>%
filter(music_data$peak == 1)

stop_words$word
install.packages("tidytext")
require(tidytext)

undesirable_words <- c("prince", "chorus", "repeat", "lyrics", 
                       "theres", "bridge", "fe0f", "yeah", "baby", 
                       "alright", "wanna", "gonna", "chorus", "verse", 
                       "whoa", "gotta", "make", "miscellaneous", "2", 
                       "4", "ooh", "uurh", "pheromone", "poompoom", "3121", 
                       "matic", " ai ", " ca ", " la ", "hey", " na ", 
                       " da ", " uh ", " tin ", "  ll", "transcription",
                       "repeats")
music_data_updated <- music_data %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  filter(nchar(word) >3) %>%
  filter(!word %in% undesirable_words) %>%
  distinct()

word_count_songs <- music_data %>%
  unnest_tokens(word, text) %>%
  select(song,chart_level) %>%
  group_by(song,chart_level) %>%
  summarise(number_of_words = n()) %>%
  arrange(desc(number_of_words))

ggplot(word_count_songs) + geom_histogram(aes(x = number_of_words, fill = chart_level))

top_words <- music_data_updated %>%
  count(word) %>%
  top_n(20) %>%
  arrange(desc(n))

ggplot(top_words) + geom_bar(aes(x = word, y = n), stat = "identity") + coord_flip()

random_word_cloud <- music_data_updated %>%
  count(word) %>%
  arrange(desc(n))

install.packages("wordcloud2")
require(wordcloud2)
wordcloud2(top_words, size = 1)
wordcloud2(random_word_cloud[1:200,], size = 0.5, color = "random-dark", figPath  = "Bane-Batman-Scarecrow-Joker-Morph-Wallpaper.jpeg")
getwd()

plot_facet <- music_data_updated %>%
  select(chart_level, word)%>%
  filter(chart_level != "NA") %>%
  group_by(chart_level) %>%
  count(word) %>%
  arrange(desc(n)) %>%
  top_n(10)

ggplot(plot_facet) + geom_bar(aes(x = word, y = n, fill = chart_level), stat = "identity") + facet_wrap(~chart_level) + coord_flip()

number_of_characters <- nchar(music_data_updated[,"word"])
music_data_updated$characters <- number_of_characters  
ggplot(music_data_updated %>%
         filter(!word %in% undesirable_words) %>%
         filter(decade != "NA")) + geom_histogram(aes (x = characters))

word_cloud_2 <- music_data_updated %>%
  select(word, characters) %>%
  group_by(word, characters) %>%
  arrange(desc(characters))

wordcloud2(word_cloud_2[1:100,], size = 0.1)

rm(lexical_diversity)

lex_diversity_per_year <- music_data_updated %>%
  filter(decade != "NA") %>%
  group_by(year) %>%
  summarise(lex_diversity = n_distinct(word)) 

ggplot(lex_diversity_per_year) +geom_point(aes(x = year, y = lex_diversity)) + geom_smooth(aes(x = year, y = lex_diversity), method = "lm")

chart_diversity <- music_data_updated %>%
  filter(decade != "NA" & chart_level != "Uncharted") %>%
  group_by(year, chart_level) %>%
  summarise(count = n())

ggplot(chart_diversity) + geom_point(aes(x = year, y = count)) + geom_smooth(aes(x = year, y = count), method = "lm")  

popular_tfidf_words <- music_data_updated %>%
  group_by(decade, word) %>%
  count(word)  %>%
  bind_tf_idf(word, decade, n)

?bind_
str(popular_tfidf_words)
 
top_10_tf_idf <- popular_tfidf_words %>%
  select(decade,word, tf_idf) %>%
  group_by(decade) %>%
  arrange(desc(tf_idf)) %>%
  slice(seq_len(10))

ggplot(top_10_tf_idf) +
  geom_bar(aes(x = word, y = tf_idf, fill = decade), stat = "identity") + facet_wrap(~decade) + coord_flip()

install.packages("widyr")
require(widyr)


library(ggrepel) #`geom_label_repel`
library(gridExtra) #`grid.arrange()` for multi-graphs
library(knitr) #Create nicely formatted output tables
library(kableExtra) #Create nicely formatted output tables
library(formattable) #For the color_tile function
library(circlize) #Visualizations - chord diagram
library(memery) #Memes - images with plots
library(magick) #Memes - images with plots (image_read)
library(yarrr)  #Pirate plot
library(radarchart) #Visualizations
library(igraph) #ngram network diagrams
library(ggraph) #ngram network diagrams

word_count_per_song <- music_data %>%
  unnest_tokens(word,text) %>%
  filter(!word %in% undesirable_words) %>%
  anti_join(stop_words) %>%
  group_by(song, Charted, decade) %>%
  summarise(word_count_in_a_song = n_distinct(word))

pirateplot(formula = word_count_in_a_song ~ decade + Charted,
           data = word_count_per_song,
           xlab = NULL, ylab = "Song Distinct Word Count", #Axis labels
           main = "Lexical Diversity Per Decade", #Plot title
           pal = "southpark", #Color scheme
           point.o = .2, #Points
           avg.line.o = 1, #Turn on the Average/Mean line
           theme = 0, #Theme
           point.pch = 16, #Point `pch` type
           point.cex = 1.5, #Point size
           jitter.val = .1, #Turn on jitter to see the songs better
           cex.lab = .9, cex.names = .7) #Axis label size)

sentiments_table <- write_xlsx(sentiments, "./Downloads/sentiments.xlsx")

rm(new_sentiments)
sentiments_updated <- sentiments %>% #From the tidytext package
  filter(lexicon != "loughran") %>% #Remove the finance lexicon
  mutate( sentiment = ifelse(lexicon == "AFINN" & score >= 0, "positive",
                             ifelse(lexicon == "AFINN" & score < 0,
                                    "negative", sentiment))) %>%
  group_by(lexicon) %>%
  mutate(words_in_lexicon = n_distinct(word))

match_rate <- music_data_updated %>%
  mutate(words_in_lyrics = n_distinct(word)) %>%
  inner_join(sentiments_updated) %>%
  select(word, words_in_lyrics, lexicon, words_in_lexicon) %>%
  distinct() %>%
  group_by(lexicon) %>%
  summarise(matching_words = n())

bing_sentiment <- music_data_updated %>%
  inner_join(get_sentiments("bing"))

nrc_sentiment <- music_data_updated %>%
  inner_join(get_sentiments("nrc"))

nrc_sentiment_wo_pos_neg <- music_data_updated %>%
  inner_join(get_sentiments("nrc")) %>%
  filter(!sentiment %in% c("positive", "negative"))

afinn_sentiment <- music_data_updated %>%
  inner_join(get_sentiments("afinn"))

polar_chart_unchart <- bing_sentiment %>%
  select(chart_level, sentiment) %>%
  group_by(chart_level, sentiment) %>%
  summarise(count = n()) %>%
  spread(sentiment, count) %>%
  mutate(polarity = positive - negative) %>%
  ggplot() + geom_histogram(aes(x = chart_level, y = polarity,fill = chart_level), stat = "identity")

polar_year <- bing_sentiment %>%
  select(year, sentiment) %>%
  group_by(year, sentiment) %>%
  summarise(count = n()) %>%
  spread(sentiment, count) %>%
  mutate(polarity = positive - negative) %>%
  ggplot() +
  geom_col(aes(x = year, y = polarity), fill = rainbow(28), stat = "identity") +
  geom_smooth(aes(x = year, y = polarity), method = "loess", se = FALSE)


plot_94_to_96 <- nrc_sentiment_wo_pos_neg %>%
  select(sentiment, word) %>%
  group_by(sentiment, word) %>%
  summarise(word_count = n()) %>%
  arrange(desc(word_count)) %>%
  slice(seq_len(10))

ggplot(plot_94_to_96, aes(word, 1, label = word, fill = sentiment ))+
  geom_label_repel(force = 1,nudge_y = .5,  
                   direction = "y",
                   box.padding = 0.1,
                   segment.color = "transparent",
                   size = 3) +
  facet_grid(~sentiment)
?geom_label_repel

bigram_split <- music_data %>%
  unnest_tokens(bigrams, text, token = "ngrams", n = 2)

bigram_words <- bigram_split %>%
  select(bigrams, decade)

bigram_words_separate <- bigram_words %>%
  separate(bigrams, c("word1", "word2"), sep = " ")

bigram_words_filter <- bigram_words_separate %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word1 %in% undesirable_words) %>%
  filter(!word2 %in% undesirable_words) %>%
  filter(word1 != word2) %>%
  filter(decade != "NA")

bigram_words_re_unite <- bigram_words_filter %>%
  unite(bigrams, word1, word2,sep = " ") %>%
  inner_join(bigram_split) %>%
  select(decade, chart_level, bigrams) %>%
  group_by(decade, chart_level, bigrams) %>%
  summarise(count = n())
  
pwc <- music_data_updated %>%
  pairwise_count(word, song) %>%
  filter(item1 %in% c("love", "peace")) %>%
  filter(n>5)

install.packages("pdftools")
require(pdftools)


three_sources_tidy_balanced <- read.csv("./Downloads/three_sources_tidy_balanced.csv",
                                        stringsAsFactors = FALSE)

all_sources_tidy_balanced <- read.csv("./Downloads/all_sources_tidy_balanced.csv",
                                      stringsAsFactors = FALSE)

prince_tidy <- read.csv("./Downloads/prince_tidy.csv",
                        stringsAsFactors = FALSE)

three_sources_tidy_balanced %>%
  group_by(source,genre) %>%
  summarise(count = n())

install.packages("tm")
install.packages("topicmodels")
require(tm)
require(topicmodels)

model_matrix_1 <- three_sources_tidy_balanced %>%
  count(document, word, sort = TRUE) %>%
  cast_dtm(document, word, n)

inspect(model_matrix_1)
str(model_matrix_1)

lda_model_1 <- LDA(model_matrix_1, k = 3, method = "GIBBS", control = list(seed = 1234))

class(lda_model_1)

tidy(lda_model_1, matrix = "beta") %>%
  filter(term == "iceberg")

top_terms <- tidy(lda_model_1, matrix = "beta") %>%
  group_by(topic) %>%
  arrange(desc(beta)) %>%
  slice(seq_len(10))

ggplot(top_terms) + geom_label_repel(aes(term, beta, label = term,fill = topic),
                                     nudge_x = .2,  
                                     direction = "y",
                                     box.padding = 0.1,
                                     segment.color = "transparent",
                                     size = 3) +
  facet_grid(~topic) + coord_flip()

documents <- tidy(lda_model_1, matrix = "gamma") %>%
  inner_join(three_sources_tidy_balanced, by = "document") %>%
  select(topic, gamma, source) %>%
  group_by(topic) %>%
  mutate(mean_gamma = mean(gamma)) %>%
  select(-gamma) %>%
  distinct()

chordDiagramFromDataFrame(documents)

k_means_model_2 <- kmeans(model_matrix_1, 3)

k_means_list <- lapply(1:3, function(i) {
  xyz <- sort(k_means_model_2$centers[i,], decreasing = T)
  names(xyz)
})

k_means_dataframe <- as.data.frame(k_means_list)

k_means_dataframe<- k_means_model_2$centers
attributes(k_means_dataframe)