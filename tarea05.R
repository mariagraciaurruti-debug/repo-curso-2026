
# tarea 05

install.packages("tidytext")
install.packages("tidyverse")
install.packages("topicmodels")
install.packages("textdata")
install.packages("igraph")
install.packages("wordcloud")
install.packages("reshape2")

library(tidytext)
library(tidyverse)
library(topicmodels)
library(textdata)
library(igraph)
library(ggraph)
library(wordcloud)
library(reshape2)

df <- read_csv("DATA-T9-mckinsey-mind-the-gap-articles-20251020.csv")

tidy_mckinsey <- df %>%
  unnest_tokens(output = word, input = article_text)

cleaned_mckinsey <- tidy_mckinsey %>%
  anti_join(get_stopwords())

df %>%
  mutate(n_words = str_count(article_text, "\\w+")) %>%
  select(title, n_words) %>%
  summary()
# Si son comparables, las longitudes van desde 161 a 1072 palabras
# con una media de 640

nrow(df)
nchar(df$article_text) %>% summary()
df %>% count(date)
# esto nos dice que tenemos 146 articulos, y la longitud de textos 
# ademas de las fechas, que se situan en el 2022 desde 15/03 a 17/05

cleaned_mckinsey <- tidy_mckinsey %>%
  anti_join(get_stopwords())

cleaned_mckinsey %>%
  count(word, sort = TRUE) %>%
  head(20)
# las palabras mas utilizadas son gen, z, zers, que hablan de lo mismo
# tratn sobre el tema central GEN Z

bing <- get_sentiments("bing")
mckinsey_sentiment_bing <- tidy_mckinsey %>%
  inner_join(bing, relationship = "many-to-many") %>%
  count(title, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(sentiment_score = positive - negative)

ggplot(mckinsey_sentiment_bing, aes(title, sentiment_score, fill = sentiment_score > 0)) +
  geom_col(show.legend = FALSE) +
  coord_flip()
# de esta forma no es clara, y no es entendible

mckinsey_sentiment_bing %>%
  slice_max(abs(sentiment_score), n = 15) %>%  # top 15 más extremos
  ggplot(aes(reorder(title, sentiment_score), sentiment_score, fill = sentiment_score > 0)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(title = "Top 15 Artículos por Sentimiento", x = "", y = "Sentiment Score")
# de esta manera logro ver los top15 por sentimiento
# nos habla sobre los Gen Z, y su interaccion con la vida adulta
# y con bing nos los ordena con una conotacion positiva o negativa

AFINN <- get_sentiments("afinn")
mckinsey_sentiment_afinn <- tidy_mckinsey %>%
  inner_join(AFINN, relationship = "many-to-many") %>%
  group_by(title) %>%
  summarise(sentiment_score = mean(value))

mckinsey_sentiment_afinn %>% head()
View(mckinsey_sentiment_afinn)
mckinsey_sentiment_afinn %>% summary()
mckinsey_sentiment_afinn %>% arrange(desc(sentiment_score))
mckinsey_sentiment_afinn %>%
  mutate(tipo = if_else(sentiment_score > 0, "positivo", "negativo")) %>%
  count(tipo)
# ahora con AFINN me califica del 5 al -5, es mas gradual
# con el ultimo comando puedo ver que hay 140 positivos y solo 6 negativos

tidy_mckinsey %>%
  count(title, word, sort = TRUE)
# la palabra "the" aparece 45 veces en el primer articulo- Should Gen Z care about Davos?
# la paabra que mas se repite por articulo es THE 

book_words <- tidy_mckinsey %>%
  count(title, word, sort = TRUE)
dtm <- book_words %>%
  cast_dtm(title, word, n)
dtm
dim(dtm)
# como contiene muchos valores sin significado, con esto me quedo con los que contienen un valor

lda_10 <- LDA(dtm, k = 10, control = list(seed = 1234))
ap_topics_10 <- tidy(lda_10, matrix = "beta")
ap_top_terms_10 <- ap_topics_10 %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms_10 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

# este grafico me muestra los 10 topicos 
# y su probabilidad de que la palabra sea importante en ese topico 
# la mayoria de topicos comparten las mismas palabras

lda_15 <- LDA(dtm, k = 15, control = list(seed = 1234))
ap_topics_15 <- tidy(lda_15, matrix = "beta")
ap_top_terms_15 <- ap_topics_15 %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  ungroup() %>%
  arrange(topic, -beta)
ap_top_terms_15 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

# ahora con 15 topicos 

head(ap_top_terms_10, 30)
head(ap_top_terms_15, 30)

# el data set, nos habla sobre el gen z, y como interactuan con distintos aspectos de la vida adulta