# quick and dirty text mining of the ISEC2020 abstracts
# using the awesome https://www.tidytextmining.com/

# load packages
library(tidyverse)
library(tm)
library(wordcloud)
library(tidytext)
library(igraph)
library(ggraph)
library(topicmodels)

# read in and clean up abstracts
text_df <- read.delim('vISEC2020 Abstracts.txt') %>%
  as_tibble() %>%
  mutate(Abstracts = tolower(Abstracts)) %>% # lower case
  # get rid of rows with location, session, and dates/hours
  filter(!str_detect(Abstracts, 'location:')) %>%
  filter(!str_detect(Abstracts, 'session:')) %>%
  filter(!str_detect(Abstracts, '06/26/2020')) %>%
  filter(!str_detect(Abstracts, '06/25/2020')) %>%
  filter(!str_detect(Abstracts, '06/24/2020')) %>%
  filter(!str_detect(Abstracts, '06/23/2020')) %>%
  filter(!str_detect(Abstracts, '06/22/2020')) %>%
  filter(!str_detect(Abstracts, '06/20-21/2020')) %>%
  # get rid of rows with authors
  filter(!str_detect(Abstracts, 'university')) %>%
  filter(!str_detect(Abstracts, 'department of')) %>%
  filter(!str_detect(Abstracts, 'universidad')) %>%
  filter(!str_detect(Abstracts, 'institute')) %>%
  filter(!str_detect(Abstracts, 'us geological survey')) %>%
  filter(!str_detect(Abstracts, 'fisheries and oceans canada')) %>%
  filter(!str_detect(Abstracts, 'u.s. geological survey')) %>%
  filter(!str_detect(Abstracts, 'unsw')) %>%
  filter(!str_detect(Abstracts, 'cnrs')) %>%
  filter(!str_detect(Abstracts, 'instituto')) %>%
  filter(!str_detect(Abstracts, 'academia sinica')) %>%
  filter(!str_detect(Abstracts, 'oncfs')) %>%
  filter(!str_detect(Abstracts, 'csiro')) %>%
  filter(!str_detect(Abstracts, 'british trust for ornithology')) %>%
  filter(!str_detect(Abstracts, 'national audubon society')) %>%
  filter(!str_detect(Abstracts, '__')) %>%
  filter(!str_detect(Abstracts, 'morton arboretum')) %>%
  filter(!str_detect(Abstracts, 'noaa')) %>%
  filter(!str_detect(Abstracts, 'usda')) %>%
  filter(!str_detect(Abstracts, 'saeon')) %>%
  filter(!str_detect(Abstracts, 'philippe marchand')) %>%
  filter(!str_detect(Abstracts, 'canadian wildlife service')) %>%
  filter(!str_detect(Abstracts, 'south african environmental observation network')) %>%
  filter(!str_detect(Abstracts, 'snow leopard trust')) %>%
  filter(!str_detect(Abstracts, 'inrae')) %>%
  filter(!str_detect(Abstracts, 'universitetet i bergen')) %>%
  filter(!str_detect(Abstracts, 'red panda network')) %>%
  filter(!str_detect(Abstracts, 'swiss federal insitute')) %>%
  filter(!str_detect(Abstracts, 'south african medical research council')) %>%
  mutate(Abstracts = str_remove(Abstracts, 'keywords: ')) %>% # remove char string 'keywords: '
  filter(str_length(Abstracts) > 1) %>% # remove empty rows
  rowid_to_column() %>% # get row id
  rename(id = rowid, text = Abstracts) # rename columns
  

# have a look
text_df 

# tokenization and remove stop words
data(stop_words)
tidy_text <- text_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  mutate(word = as_factor(word))
  
# count words
tidy_text %>%
  count(word, sort = TRUE) 

# merge words together, mainly plural becomes singular
tidy_text <- tidy_text %>% 
  mutate(word = fct_recode(word, model = 'models', model = 'modelling')) %>%
  mutate(word = fct_recode(word, individual = 'individuals')) %>%
  mutate(word = fct_recode(word, estimation = 'estimates', estimation = 'estimate')) %>%
  mutate(word = fct_recode(word, method = 'methods')) %>%
  mutate(word = fct_recode(word, effect = 'effects')) %>%
  mutate(word = fct_recode(word, parameter = 'parameters')) 

# count words
tidy_text %>%
  count(word, sort = TRUE) 

# visually
tidy_text %>%
  count(word, sort = TRUE) %>%
  filter(n > 100) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

# 2-grams
isec_bigrams <- text_df %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

bigrams_separated <- isec_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

bigram_counts

set.seed(2020)
bigram_graph <- bigram_counts %>%
  filter(n > 20) %>%
  graph_from_data_frame()

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

# same graph, with some polishing
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

ggsave('bigrams.png',dpi=300)

# 3-grams
text_df %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word) %>%
  count(word1, word2, word3, sort = TRUE)

# topic modelling
isec_dtm <- tidy_text %>% 
  count(id, word, sort = TRUE) %>%
  cast_dfm(id, word, n)

ap_lda <- LDA(isec_dtm, k = 4, control = list(seed = 1234))
ap_topics <- tidy(ap_lda, matrix = "beta")

ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()

ggsave('topic_isec.png', dpi=300)