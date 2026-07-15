install.packages(c("tm", "SnowballC", "syuzhet", "tidyverse", "wordcloud", "ggplot2"))

library(tm)
library(SnowballC)
library(syuzhet)
library(tidyverse)
library(wordcloud)
library(ggplot2)

data <- read.csv("C:\\Users\\kshar\\Downloads\\tripadvisor.csv", header = TRUE)
str(data)

corpus <- iconv(data$Review, to = "UTF-8", sub = "byte")
corpus <- Corpus(VectorSource(corpus))

inspect(corpus[1:5])

cleaned_corpus <- tm_map(corpus, content_transformer(tolower))
cleaned_corpus <- tm_map(cleaned_corpus, removePunctuation)
cleaned_corpus <- tm_map(cleaned_corpus, removeNumbers)
cleaned_corpus <- tm_map(cleaned_corpus, removeWords, stopwords('english'))
cleaned_corpus <- tm_map(cleaned_corpus, stripWhitespace)
cleaned_corpus <- tm_map(cleaned_corpus, stemDocument)

inspect(cleaned_corpus[1:5])

set.seed(123)

sampled_reviews <- sample(data$Review, 200)
sampled_corpus <- Corpus(VectorSource(iconv(sampled_reviews, to = "UTF-8", sub = "byte")))

cleaned_sampled_corpus <- tm_map(sampled_corpus, content_transformer(tolower))
cleaned_sampled_corpus <- tm_map(cleaned_sampled_corpus, removePunctuation)
cleaned_sampled_corpus <- tm_map(cleaned_sampled_corpus, removeNumbers)
cleaned_sampled_corpus <- tm_map(cleaned_sampled_corpus, removeWords, stopwords('english'))
cleaned_sampled_corpus <- tm_map(cleaned_sampled_corpus, stripWhitespace)
cleaned_sampled_corpus <- tm_map(cleaned_sampled_corpus, stemDocument)

tdm_sparse <- TermDocumentMatrix(cleaned_sampled_corpus, control = list(weighting = weightTfIdf))
tdm_m_sparse <- as.matrix(tdm_sparse)

term_freq <- rowSums(tdm_m_sparse)
term_freq_sorted <- sort(term_freq, decreasing = TRUE)
tdm_d_sparse <- data.frame(word = names(term_freq_sorted), freq = term_freq_sorted)

head(tdm_d_sparse, 5)

text <- iconv(data$Review)

syuzhet_vector <- get_sentiment(text, method = "syuzhet")
cat("Syuzhet method",head(syuzhet_vector),"\n")

bing_vector <- get_sentiment(text, method = "bing")
cat("Bing method:",head(bing_vector),"\n")

afinn_vector <- get_sentiment(text, method = "afinn")
cat("Afinn method:",head(afinn_vector),"\n")

rbind(
  sign(head(syuzhet_vector)),
  sign(head(bing_vector)),
  sign(head(afinn_vector))
)

wordcloud(words = tdm_d_sparse$word, freq = tdm_d_sparse$freq, 
          min.freq = 5, max.words = 100, colors = brewer.pal(8, "Dark2"))

text_sampled <- iconv(sampled_reviews)
syuzhet_vector_sampled <- get_sentiment(text_sampled, method = "syuzhet")

ggplot(data.frame(syuzhet_vector_sampled), aes(x = syuzhet_vector_sampled)) + 
  geom_histogram(binwidth = 0.1, fill = "blue", color = "black") + 
  labs(title = "Sentiment Distribution using Syuzhet Method (Sampled Data)", 
       x = "Sentiment Score", y = "Frequency") + 
  theme_minimal()

nrc_sampled <- get_nrc_sentiment(text_sampled)
nrct_sampled <- data.frame(t(nrc_sampled))
nrcs_sampled <- data.frame(rowSums(nrct_sampled))
nrcs_sampled <- cbind("sentiment" = rownames(nrcs_sampled), nrcs_sampled)

rownames(nrcs_sampled) <- NULL
names(nrcs_sampled)[1] <- "sentiment"
names(nrcs_sampled)[2] <- "frequency"

nrcs_sampled <- nrcs_sampled %>% mutate(percent = frequency/sum(frequency))
nrcs2_sampled <- nrcs_sampled[1:8, ]
colnames(nrcs2_sampled)[1] <- "emotion"

ggplot(nrcs2_sampled, aes(x = reorder(emotion, -frequency), y = frequency, 
                          fill = emotion)) + 
  geom_bar(stat = "identity") + 
  labs(title = "Emotion Distribution (Sampled Data)", x = "Emotion", y = "Frequency") + 
  theme_minimal() + 
  scale_fill_brewer(palette = "Set3")

tdm_d_sparse <- tdm_d_sparse[1:10, ]
tdm_d_sparse$word <- reorder(tdm_d_sparse$word, tdm_d_sparse$freq)
ggplot(tdm_d_sparse, aes(x = word, y = freq, fill = word)) + 
  geom_bar(stat = "identity") + 
  coord_flip() + 
  labs(title = "Most Popular Words", x = "Word", y = "Frequency") + 
  theme_minimal()

library(ggplot2)
library(RColorBrewer)

sentiment_df <- data.frame(
  sentiment = c("Positive", "Negative", "Neutral"),
  count = c(sum(syuzhet_vector_sampled > 0), sum(syuzhet_vector_sampled < 0), 
            sum(syuzhet_vector_sampled == 0))
)

ggplot(sentiment_df, aes(x = "", y = count, fill = sentiment)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Sentiment Distribution", x = "", y = "") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3")

