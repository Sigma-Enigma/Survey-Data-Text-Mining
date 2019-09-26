require(foreign)
require(ggplot2)
require(MASS)
require(Hmisc)
require(reshape2)
require(dplyr)
require(tidyr)
require(wordcloud)
require(tibble)
require(tidytext)



# Techniques to use: Ordinal logistic regression, t-tests, ANOVAs, NLP 
# Things to do: check model assumptions of OrdLogReg

# sense of belonging, UC GPA, 

mv.data <- read.csv(file="D:/Downloads/Survey.csv")

summary(mv.data)


View(mv.data)


# TidyText

mv.data$What.is.your.major.challenge.at.UCLA. <- as.character(mv.data$What.is.your.major.challenge.at.UCLA.)
mv.data$What.is.your.best.part.about.your.UCLA.experience. <- as.character(mv.data$What.is.your.best.part.about.your.UCLA.experience.)
mv.data$What.could.improve.your.experience.at.UCLA. <- as.character(mv.data$What.could.improve.your.experience.at.UCLA.)

text <- mv.data$What.is.your.major.challenge.at.UCLA.[1]



# converting each question to a tidytext dataframe format
# also tokenizing words using "word" tokenization format

text1 <- mv.data$What.is.your.major.challenge.at.UCLA.
text_df1 <- tibble(subject = 1:33, text = text1)

text_df1 <- text_df1 %>%
  unnest_tokens(word, text)

text2 <- mv.data$What.is.your.best.part.about.your.UCLA.experience.
text_df2 <- tibble(subject = 1:33, text = text2)

text_df2 <- text_df2 %>%
  unnest_tokens(word, text) 

text3 <- mv.data$What.could.improve.your.experience.at.UCLA.
text_df3 <- tibble(subject = 1:33, text = text3)

text_df3 <- text_df3 %>%
  unnest_tokens(word, text)


# checking tidydata dimensions
dim(text_df1); dim(text_df2); dim(text_df3)

# merging all text questions into a single tidytext format
text_all <- tibble(subject = c(1:33, 1:33, 1:33), question = c(rep(1, 33), rep(2,33), rep(3,33) ), text = c(text1,text2,text3) )

text_all <- text_all %>%
  unnest_tokens(word, text)

data("stop_words") # list of nuisance words to be filtered out before data analysis

text_all_clean <- text_all %>%
  anti_join(stop_words) #defaults to joining by stop_words with words in "word"

text_counts <- text_all_clean %>%
  count(word, sort=TRUE)


# Wordcloud

# expanding plot window
dev.new(width = 5000, height = 5000, unit = "px")

# word cloud
text_all_clean %>% 
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words=50, min.freq = 3, ordered.colors = TRUE, random.order = TRUE, random.color = FALSE))

# sentiment analysis for word cloud
text_all_clean %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("orange3", "olivedrab4"),
                   max.words = 100, scale = c(6, 0.75),
                   title.colors = c("orange3", "olivedrab4") )

# Topic analysis with network links between topics

