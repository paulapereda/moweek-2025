pacman::p_load(topicmodels, stopwords, tidytext, tidyverse, ggraph, 
               igraph, widyr, here, tm)

# (1) Preprocess the Text

df <- readRDS("~/Library/CloudStorage/Dropbox/R/moweek-2025/data/previa/product_04_clothing.rds") %>% 
  distinct(name, price, brand, type, category, characteristics, sizes, colors, description)

# Clean and tokenize
clean_text <- df %>%
  mutate(description = str_to_lower(description),
         description = str_replace_all(description, "[[:punct:]]", "")) %>%
  unnest_tokens(word, description)

# (2) Remove Stopwords in Spanish

clean_text <- clean_text %>%
  filter(!word %in% stopwords("es"))

# (3) Find Most Common Words

popular_words <- clean_text %>%
  count(word, sort = TRUE)

print(popular_words)

# (4) Find Word Associations / Bigrams

# Get bigrams (word pairs)
bigrams <- df %>%
  mutate(description = str_to_lower(description),
         description = str_replace_all(description, "[[:punct:]]", "")) %>%
  unnest_tokens(bigram, description, token = "ngrams", n = 2)

# Remove stopwords from bigrams
bigrams_separated <- bigrams %>%
  separate(bigram, into = c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stopwords("es"),
         !word2 %in% stopwords("es"))

# Count most common bigrams
popular_bigrams <- bigrams_separated %>%
  count(word1, word2, sort = TRUE)

print(popular_bigrams)

# (5) Visualize Results

popular_words %>%
  filter(n > 1) %>%
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_col(fill = "#b56576") +
  coord_flip() +
  labs(title = "Palabras más frecuentes", x = "Palabra", y = "Frecuencia")

# (6) Word Co-occurrence Network (pairwise correlation)
### This helps find words that tend to appear together in the same document (or description).

# Pairwise word co-occurrence (within the same description)
word_pairs <- clean_text %>%
  pairwise_count(item = word, feature = name, sort = TRUE)

# Filter for stronger associations (e.g., n >= 2)
filtered_pairs <- word_pairs %>%
  filter(n >= 2)

# Plot with ggraph
graph <- graph_from_data_frame(filtered_pairs)

ggraph(graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void() +
  labs(title = "Red de co-ocurrencia de palabras")

# (7) Topic Modeling with LDA (Latent Dirichlet Allocation)

# Re-create a clean corpus
corpus <- VCorpus(VectorSource(df$description)) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeWords, stopwords("spanish")) %>%
  tm_map(stripWhitespace)

# Document-term matrix
dtm <- DocumentTermMatrix(corpus)

# Run LDA with k topics (you can change 2 to 3, 5, etc.)
lda_model <- LDA(dtm, k = 2, control = list(seed = 1234))

# Extract topics
topics <- tidy(lda_model, matrix = "beta")

# Top words per topic

top_terms <- topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  ungroup() %>%
  mutate(term = reorder_within(term, beta, topic))

# Plot
ggplot(top_terms, aes(x = term, y = beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered() +
  labs(title = "Tópicos encontrados por LDA", x = NULL, y = "Probabilidad")

doc_topics <- tidy(lda_model, matrix = "gamma")

print(doc_topics)  # each description’s topic probabilities
