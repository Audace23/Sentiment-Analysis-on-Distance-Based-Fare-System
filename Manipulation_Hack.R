
# Data and Preparations
#Articles CSV
articles<- read.csv("www/Articles.csv")
glimpse(articles)

#Cleaning the data
articles_clean <- articles %>%
  mutate(
    Title = str_squish(Title),
    Content = str_squish(Content),
    Content = tolower(Content)
  ) %>%
  filter(!is.na(Content), nchar(Content) > 100)
# Tokenization and Removing the stop words
tokens <- articles_clean %>%
  unnest_tokens(word, Content)
data("stop_words") #Loading the stop words
tokens <- tokens %>%
  anti_join(stop_words, by = "word")
#Frequent words
word_freq <- tokens %>%
  count(word, sort = TRUE)
colnames(word_freq)[colnames(word_freq)=="n"]<- "freq"
#===============================
# Twiter (X) Comments CSV
comments<- read.csv("www/Comments.csv")
# Cleanning the data
comments <- comments %>%
  mutate(
    Translation = iconv(Translation, from = "", to = "UTF-8")
  ) # Converting the Translation var to appropriate format
comments_clean<- comments %>%
  mutate(
    User = str_squish(User),
    Comment = str_squish(Comment),
    Translation = tolower(Translation)
  )
# Tokenization of Comments 
tokens_comments<- comments_clean %>%
  unnest_tokens(comments_clean, Translation)
colnames(tokens_comments)[colnames(tokens_comments)=="comments_clean"]<- "word"
#Removing stopwords
tokens_comments <- tokens_comments %>%
  anti_join(stop_words, by = "word")
#Frequent word-2
word_freq_comments<- tokens_comments %>%
  count(word, sort = TRUE)
colnames(word_freq_comments)[colnames(word_freq_comments)=="n"]<- "freq"
# Specifi=ying the source
word_freq$source = "Articles"
word_freq_comments$source = "Twitter (X)"
#Binding the two datasets
frequent_words<- rbind(word_freq, word_freq_comments)
write.csv(frequent_words, "frequent_words.csv")
write.csv(all_with_sentiment, "all_with_sentiment.csv")
write.csv(all_with_sentiment_pie, "all_with_sentiment_pie.csv")
write.csv(contingency_data,"contigency_data.csv")
#===============================================================================
# Building the word_cloud
set.seed(123)
Word_Cloud<-wordcloud2(data = frequent_words, size = 1.5, color = 'random-light', backgroundColor = "navy")
Word_Cloud
#===============================================================================
#Add sentiment column
bing_sentiments <- get_sentiments("bing") #Load Lexicon sentiment (Positive/Negative) data set 
#Join sentiment to the data set
word_sentiments <- frequent_words %>%
  inner_join(bing_sentiments, by = c("word" = "word")) # 'positive' or 'negative'
# Words not in the sentiment lexicon are considered 'neutral'
all_with_sentiment <- frequent_words %>%
  left_join(bing_sentiments, by = "word") %>%
  mutate(sentiment = ifelse(is.na(sentiment), "neutral", sentiment))
table(all_with_sentiment$sentiment)
#===============================================================================
all_with_sentiment_pie<- all_with_sentiment %>%
  dplyr::filter(sentiment!= "neutral")
colnames(all_with_sentiment_pie)[colnames(all_with_sentiment_pie)=="freq"]<- "n"
# Function to create pie chart
create_pie <- function(df) {
  all_with_sentiment_pie %>%
    count(sentiment) %>%
    mutate(
      sentiment = factor(sentiment, levels = c("positive", "negative")),
      color = case_when(
        sentiment == "positive" ~ "#28a745",
        sentiment == "negative" ~ "#dc3545",
        TRUE ~ "#999999"
      )
    ) %>%
    plot_ly(
      labels = ~sentiment,
      values = ~n,
      type = "pie",
      marker = list(colors = ~color)
    )
}

# Build a list of pie charts by source
pie_charts <- list()
unique_sources <- unique(all_with_sentiment$source)

for (src in unique_sources) {
  pie_charts[[src]] <- create_pie(all_with_sentiment_pie %>% filter(source == src))
}

# Add one for "All"
pie_charts[["All"]] <- create_pie(all_with_sentiment_pie)
#===============================================================================
#Creating the contingency table and running Chi-Square test
contingency_data <- all_with_sentiment %>%
  count(source, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  # Reorder columns to ensure consistent order
  select(source, positive, neutral, negative) %>%
  # Make it a data frame explicitly
  as.data.frame()

# Add row for total counts
contingency_data <- rbind(
  contingency_data,
  data.frame(
    source = "Total",
    positive = sum(contingency_data$positive),
    neutral = sum(contingency_data$neutral),
    negative = sum(contingency_data$negative)
  )
)

# Rename the source column for better display
contingency_data <- contingency_data %>%
  mutate(source = case_when(
    source == "Articles" ~ "News Articles",
    source == "Twitter (X)" ~ "Social Media",
    TRUE ~ source
  )) %>% dplyr::filter(positive!=70)

# Store the matrix form for chi-square test
chi_matrix <- contingency_data %>%
  select(-source) %>%
  as.matrix()

# Perform Chi-Square test 
chi_test_result <- chisq.test(chi_matrix)

# Prepare interpretation text
if(chi_test_result$p.value < 0.05) {
  chi_interpretation <- paste("Using Chi-Square test at 95% level of significance (χ² =", 
                              round(chi_test_result$statistic, 2), 
                              ", p-value =", round(chi_test_result$p.value, 4), 
                              "), there is a statistically significant dependence between sentiment and the platform through which the message is sent. This suggests that the distribution of positive, negative, and neutral sentiments differs significantly between articles and social media posts regarding distance-based transport fares in Rwanda.")
} else {
  chi_interpretation <- paste("Using Chi-Square test at 95% level of significance (χ² =", 
                              round(chi_test_result$statistic, 2), 
                              ", p-value =", round(chi_test_result$p.value, 4), 
                              "), there is no statistically significant dependence between sentiment and the platform. This suggests that the distribution of positive, negative, and neutral sentiments is similar across articles and social media posts regarding distance-based transport fares in Rwanda.")
}


rsconnect::setAccountInfo(name='c4hpat-audace-tuyizere',
                          token='67D40AC18EB25113A446D23BEB970AE6',
                          secret='lb2N2II6EgAXEFa/4hzsNZL9OdtO4odO4Yf4P94F')

rsconnect::deployApp(appDir = "C:/Users/user/Desktop/R_Stuff")
