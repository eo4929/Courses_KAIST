
library(gutenbergr)
PBR <- gutenberg_download(730)
PBR

# Tokenizing by n-grams
library(dplyr); library(tidytext)
n_PBR <- PBR %>%
  unnest_tokens(word, text, token="ngrams", n=2) %>%
  anti_join(stop_words)

# Filtering n-grams
library(tidyr)
sep_PBR <- n_PBR %>%
  separate(word, c("word1","word2"), sep=" ")
# Removing cases where either is a stop word
sepf_PBR <- sep_PBR %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)
sepf_PBR

#(a)
# Counting n-grams
count_PBR <- sepf_PBR %>%
  count(word1,word2, sort=TRUE)
count_PBR # This is answer to report



#(b)
# Creating graph object
library(igraph)
graph_bg <- count_PBR %>% filter(n > 4) %>% graph_from_data_frame()
graph_bg
# Visualizations of bigrams
library(ggraph)
ar <- grid::arrow(type="closed", length=unit(.1,"inches"))
ggraph(graph_bg,layout='fr') +
  geom_edge_link(aes(edge_alpha=n), show.legend=FALSE,
                 arrow=ar, end_cap=circle(.07,"inches")) +
  geom_node_point(color="navyblue", size=2) +
  geom_node_text(aes(label=name), vjust=1, hjust=1) +
  theme_void()
#Adding thickness to edges 
count_PBR %>%
  filter(n > 4) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha=n,edge_width=n),edge_colour="cyan4") +
  geom_node_point(color="navyblue", size = 2) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.2, "lines")) +
  theme_void()


#(c)
library(tidytext)
n_words <- c("not", "no", "never", "neither")
nega_words <- sep_PBR %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(word1 %in% n_words) %>% 
  count(word1, word2,sort=TRUE)
nega_words

## ranking for plotting only top ones

ranked_nega_words <- nega_words %>% 
  group_by(word1) %>%
  mutate(rank = order(n, decreasing=TRUE))

library(forcats)
top_nega_words <- ranked_nega_words[ranked_nega_words$rank <= 5,] %>% 
  arrange(rank) %>%
  mutate(word2=fct_reorder(word2, rank))
# fct_reorder -> 아 char 형 단어를 factor로 바꾸는 구나 레이블로 만들기 위해
  
top_nega_words %>%
  ggplot(aes(word2, n, fill = n)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ word1, scales = "free") +
  xlab("Words preceded by negation") +
  ylab("Number of occurrences") +
  theme_bw() +
  coord_flip()

