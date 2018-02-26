################################################################################
# Amanda McGowin
# Thesis: An Analysis of Major Acquisition Reforms Through Text Mining and 
#         Grounded Theory Design 
# 
# 6. Sentiment Analysis  
#
# Contains:
# - basic sentimentn analysis using the NRC lexicon (line 20)
# - analysis of how sentiment changes throughout document by paragraph (line 73)
# - analysis of how sentiment changes throughout document by sentence (line 117) 
#   -- Looking closer at the VERY positivr/negative bars in the progression (line 295)
# -Examining effects of sentiment negation with n-grams (line 382)
#  -- Did the effects of negation affect the sentiment categorizations (line 518)
################################################################################



##############################
#  Basic Sentiment Analysis  #
##############################

# Basic Sentiment Analysis: Using 'nrc' lexicon dataset
# data_tb %>%
#   prep_analysis_word("TXT") %>%
#   inner_join(get_sentiments("nrc")) %>%
#   filter(!is.na(sentiment)) %>%
#   ungroup() %>%
#   count(sentiment, sort = TRUE) %>%
#   plot_sentiment_basic()

# Basic Sentiment Analysis: Using 'nrc' lexicon dataset
# 1. Reform only
data_tb %>%
  prep_analysis_word("TXT") %>%
  analyze_sentiment_basic("nrc", "Reform") %>%
  plot_sentiment_basic(t = "Basic Sentiment Analysis: Reforms",
                       sub_t = "Categorized as positive/negative & by Sentiment Category",
                       c = "*Common acquisition words removed")

# 1.1-1.5: Reform only: Single Reform (input into analyze_sentiment_basic function)
# DAWIA, FASA, Nunn-McCurdy, Packard, WSARA
data_tb %>%
  prep_analysis_word("TXT") %>%
  analyze_sentiment_basic("nrc", "Reform", "^WSARA") %>%
  plot_sentiment_basic(t = "Basic Sentiment Analysis: Reform (WSARA)",
                       sub_t = "Categorized as positive/negative & by Sentiment Category",
                       c = "*Common acquisition words removed")

# 2. Compendium only
data_tb %>%
  prep_analysis_word("TXT") %>%
  analyze_sentiment_basic("nrc", "Compendium") %>%
  plot_sentiment_basic(t = "Basic Sentiment Analysis: Expert Compendium",
                       sub_t = "Categorized as positive/negative & by Sentiment Category",
                       c = "*Common acquisition words removed")

# 2.1.-2.32: Compendium only: Single Author (input into analyze_sentiment_basic function)
# Anderson, Augustine, Berteau, Blickstein, Cartwright, Christie, Etherton, Finley,
# Fox C., Fox D., Francis, Gansler, Gilmore, Gordon, Greenwalt, Harrison, Jonas, 
# Kaminski, Kendall, Lehman, McGrath, McNichol, Morin, Oliver, Roughead, Schinasi, 
# Schwartz, Stackley, Sullivan, Venlet, Ward, Zakheim
data_tb %>%
  prep_analysis_word("TXT") %>%
  analyze_sentiment_basic("nrc", "Compendium", "^And") %>%
  plot_sentiment_basic(t = "Basic Sentiment Analysis: Expert Compendium (Anderson)",
                       sub_t = "Categorized as positive/negative & by Sentiment Category",
                       c = "*Common acquisition words removed")



#################################################
# Sentiment: Approximate look across paragraphs #
#################################################

# 3. Reform Only
data_tb %>%
  prep_analysis_word("TXT") %>%
  analyze_sentiment_NetScorePara("Reform") %>%
  plot_sentiment_NetScorePara(t = "Net Sentiment Score Across Paragraphs: Major Reforms",
                              c = "*Common acquisition words removed")
# # Compendium Only
# data_tb %>%
#   prep_analysis_word("TXT") %>%
#   analyze_sentiment_NetScorePara("Compendium") %>%
#   plot_sentiment_NetScorePara(t = "Net Sentiment Score Across Paragraphs: Expert Compendium",
#                               c = "*Common acquisition words removed")

# 4. Compendium Only: Experts A-F
data_tb %>%
  prep_analysis_word("TXT") %>%
  analyze_sentiment_NetScorePara("Compendium", "^[A-F]") %>%
  plot_sentiment_NetScorePara(t = "Net Sentiment Score Across Paragraphs: Expert Compendium (A-F)",
                              sub_t = "Acquisition Experts A-F",
                              c = "*Common acquisition words removed")

# 5. Compendium Only: Experts G-L
data_tb %>%
  prep_analysis_word("TXT") %>%
  analyze_sentiment_NetScorePara("Compendium", "^[G-L]") %>%
  plot_sentiment_NetScorePara(t = "Net Sentiment Score Across Paragraphs: Expert Compendium (G-L)",
                              sub_t = "Acquisition Experts G-L",
                              c = "*Common acquisition words removed")

# 6. Compendium Only: Experts M-Z
data_tb %>%
  prep_analysis_word("TXT") %>%
  analyze_sentiment_NetScorePara("Compendium", "^[M-Z]") %>%
  plot_sentiment_NetScorePara(t = "Net Sentiment Score Across Paragraphs: Expert Compendium (M-Z)",
                              sub_t = "Acquisition Experts M-Z",
                              c = "*Common acquisition words removed")




####################################
# Sentiment: Look across sentences #
####################################

# Can change mutate line to the following, which would round to the number of 
# decimals necessary to make each one unique:
#
# mutate(sentence_count = 1:n(),
#        index = round(sentence_count / n(), floor(log(n(),10))))
#
# You might need to change `floor` to `ceiling`


# 7. Reform only
data_tb %>%
  filter(CLASS1 == "Reform") %>%
  prep_analysis_sentence("TXT", SW = FALSE, AW = FALSE) %>%
  group_by(NAME) %>%
  mutate(sentence_count = 1:n(),
         index = round(sentence_count / n(), 2)) %>%
  unnest_tokens(word, sentence) %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(NAME, index) %>%
  summarise(sentiment = sum(score, na.rm = TRUE)) %>%
  arrange(NAME, desc(sentiment)) %>%
  plot_sentiment_sentence(t = "Sentiment within Acquisition Reforms",
                          sub_t = "Summary of the net sentiment score as the reform text progresses",
                          xlab = "Text Progression",
                          ylab = "Reform")

data_tb %>%
  filter(CLASS1 == "Reform") %>%
  filter(NAME != "FASA") %>%
  prep_analysis_sentence("TXT", SW = FALSE, AW = FALSE) %>%
  group_by(NAME) %>%
  mutate(sentence_count = 1:n(),
         index = round(sentence_count / n(), 2)) %>%
  unnest_tokens(word, sentence) %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(NAME, index) %>%
  summarise(sentiment = sum(score, na.rm = TRUE)) %>%
  arrange(NAME, desc(sentiment)) %>%
  plot_sentiment_sentence(t = "Sentiment within Acquisition Reforms",
                          sub_t = "Summary of the net sentiment score as the reform text progresses",
                          xlab = "Text Progression",
                          ylab = "Reform")

# 8. Compendium Only

data_tb %>%
  filter(CLASS1 == "Compendium") %>%
  prep_analysis_sentence("TXT", SW = FALSE, AW = FALSE) %>%
  group_by(NAME) %>%
  mutate(sentence_count = 1:n(),
         index = round(sentence_count / n(), 2)) %>%
  unnest_tokens(word, sentence) %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(NAME, index) %>%
  summarise(sentiment = sum(score, na.rm = TRUE)) %>%
  arrange(NAME, desc(sentiment)) %>%
  plot_sentiment_sentence(t = "Sentiment within Acquisition Expert Opinion",
                          sub_t = "Summary of the net sentiment score as the expert's opinion progresses",
                          xlab = "Opinion Progression",
                          ylab = "Acq Expert")

# 8.1: Compendium Only: Kendall removed to have a more clear view of the other experts 
data_tb %>%
  filter(CLASS1 == "Compendium") %>%
  filter(!str_detect(NAME, "^Kendall")) %>%
  prep_analysis_sentence("TXT", SW = FALSE, AW = FALSE) %>%
  group_by(NAME) %>%
  mutate(sentence_count = 1:n(),
         index = round(sentence_count / n(), 2)) %>%
  unnest_tokens(word, sentence) %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(NAME, index) %>%
  summarise(sentiment = sum(score, na.rm = TRUE)) %>%
  arrange(NAME, desc(sentiment)) %>%
  plot_sentiment_sentence(t = "Sentiment within Acquisition Expert Opinion",
                          sub_t = "Summary of the net sentiment score as the expert's opinion progresses (Excludes Kendall)",
                          xlab = "Opinion Progression",
                          ylab = "Acq Expert")


# 8.1: Compendium Only: Kendall only
data_tb %>%
  filter(CLASS1 == "Compendium") %>%
  filter(str_detect(NAME, "^Kendall")) %>%
  prep_analysis_sentence("TXT", SW = FALSE, AW = FALSE) %>%
  group_by(NAME) %>%
  mutate(sentence_count = 1:n(),
         index = round(sentence_count / n(), 2)) %>%
  unnest_tokens(word, sentence) %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(NAME, index) %>%
  summarise(sentiment = sum(score, na.rm = TRUE)) %>%
  arrange(NAME, desc(sentiment)) %>%
  plot_sentiment_sentence(t = "Sentiment within Acquisition Expert Opinion",
                          sub_t = "Summary of the net sentiment score as the expert's opinion progresses (Excludes Kendall)",
                          xlab = "Opinion Progression",
                          ylab = "Acq Expert")

# 8.2: Compendium Only: Experts A-F
data_tb %>%
  filter(CLASS1 == "Compendium") %>%
  filter(str_detect(NAME, "^[A-F]")) %>%
  prep_analysis_sentence("TXT", SW = FALSE, AW = FALSE) %>%
  group_by(NAME) %>%
  mutate(sentence_count = 1:n(),
         index = round(sentence_count / n(), 2)) %>%
  unnest_tokens(word, sentence) %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(NAME, index) %>%
  summarise(sentiment = sum(score, na.rm = TRUE)) %>%
  arrange(NAME, desc(sentiment)) %>%
  plot_sentiment_sentence(t = "Sentiment within Acquisition Expert Opinion (A-F)",
                          sub_t = "Summary of the net sentiment score as the expert's opinion progresses",
                          xlab = "Opinion Progression",
                          ylab = "Acq Expert")

# 8.3: Compendium Only: Experts G-L
data_tb %>%
  filter(CLASS1 == "Compendium") %>%
  filter(str_detect(NAME, "^[G-L]")) %>%
  prep_analysis_sentence("TXT", SW = FALSE, AW = FALSE) %>%
  group_by(NAME) %>%
  mutate(sentence_count = 1:n(),
         index = round(sentence_count / n(), 2)) %>%
  unnest_tokens(word, sentence) %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(NAME, index) %>%
  summarise(sentiment = sum(score, na.rm = TRUE)) %>%
  arrange(NAME, desc(sentiment)) %>%
  plot_sentiment_sentence(t = "Sentiment within Acquisition Expert Opinion (G-L)",
                          sub_t = "Summary of the net sentiment score as the expert's opinion progresses",
                          xlab = "Opinion Progression",
                          ylab = "Acq Expert")

# 8.3.1: Compendium Only: Experts G-L (Excluding Kendall)
data_tb %>%
  filter(CLASS1 == "Compendium") %>%
  filter(!str_detect(NAME, "^Kendall")) %>%
  filter(str_detect(NAME, "^[G-L]")) %>%
  prep_analysis_sentence("TXT", SW = FALSE, AW = FALSE) %>%
  group_by(NAME) %>%
  mutate(sentence_count = 1:n(),
         index = round(sentence_count / n(), 2)) %>%
  unnest_tokens(word, sentence) %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(NAME, index) %>%
  summarise(sentiment = sum(score, na.rm = TRUE)) %>%
  arrange(NAME, desc(sentiment)) %>%
  plot_sentiment_sentence(t = "Sentiment within Acquisition Expert Opinion (G-L Excluding Kendall)",
                          sub_t = "Summary of the net sentiment score as the expert's opinion progresses",
                          xlab = "Opinion Progression",
                          ylab = "Acq Expert")

# 8.4: Compendium Only: Experts M-Z
data_tb %>%
  filter(CLASS1 == "Compendium") %>%
  filter(str_detect(NAME, "^[M-Z]")) %>%
  prep_analysis_sentence("TXT", SW = FALSE, AW = FALSE) %>%
  group_by(NAME) %>%
  mutate(sentence_count = 1:n(),
         index = round(sentence_count / n(), 2)) %>%
  unnest_tokens(word, sentence) %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(NAME, index) %>%
  summarise(sentiment = sum(score, na.rm = TRUE)) %>%
  arrange(NAME, desc(sentiment)) %>%
  plot_sentiment_sentence(t = "Sentiment within Acquisition Expert Opinion (M-Z)",
                          sub_t = "Summary of the net sentiment score as the expert's opinion progresses",
                          xlab = "Opinion Progression",
                          ylab = "Acq Expert")




# -----------------------------------------------------------------------------
# Looking closer at the VERY positive/negative bars in the progression
# -----------------------------------------------------------------------------

# Writes file with each expert, index, and sentiment score 
data_tb %>%
  filter(CLASS1 == "Compendium") %>%
  filter(!str_detect(NAME, "^Kendall")) %>%
  prep_analysis_sentence("TXT", SW = FALSE, AW = FALSE) %>%
  group_by(NAME) %>%
  mutate(sentence_count = 1:n(),
         index = round(sentence_count / n(), 2)) %>%
  unnest_tokens(word, sentence) %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(NAME, index) %>%
  summarise(sentiment = sum(score, na.rm = TRUE)) %>%
  #arrange(NAME, desc(index)) 
  arrange(NAME, index) %>%
  write_csv("./Plots/Sentiment/sentiment.csv")



# identify expert indicies with sentiment scores very positive (>= 10)
data_tb %>%
  filter(CLASS1 == "Compendium") %>%
  filter(!str_detect(NAME, "^Kendall")) %>%
  prep_analysis_sentence("TXT", SW = FALSE, AW = FALSE) %>%
  group_by(NAME) %>%
  mutate(sentence_count = 1:n(),
         index = round(sentence_count / n(), 2)) %>%
  unnest_tokens(word, sentence) %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(NAME, index) %>%
  summarise(sentiment = sum(score, na.rm = TRUE)) %>%
  filter(sentiment >= 10)

data_tb %>%
  prep_analysis_sentence("TXT", SW = FALSE, AW = FALSE) %>%
  group_by(NAME) %>%
  mutate(sentence_count = 1:n(),
         index = round(sentence_count / n(), 2)) %>%
  filter((str_detect(NAME, "^Berteau") & index == 0.950) |
         (str_detect(NAME, "^Etherton") & index == 0.500) |
         (str_detect(NAME, "^Francis") & index == 0.240) |
         (str_detect(NAME, "^Gansler") & index == 0.740) |
         (str_detect(NAME, "^Gansler") & index == 0.830) |
         (str_detect(NAME, "^Harrison") & index == 0.510) |
         (str_detect(NAME, "^Harrison") & index == 0.660) |
         (str_detect(NAME, "^McGrath") & index == 0.370) |
         (str_detect(NAME, "^Schinasi") & index == 0.990) |
         (str_detect(NAME, "^Sullivan") & index == 0.570)) %>%
  select(NAME, index, sentence_count, sentence) %>%
  write_csv("./Plots/Sentiment/UltraPos.csv")



# identify expert indicies with sentiment scores very negative (<= -9)
data_tb %>%
  filter(CLASS1 == "Compendium") %>%
  filter(!str_detect(NAME, "^Kendall")) %>%
  prep_analysis_sentence("TXT", SW = FALSE, AW = FALSE) %>%
  group_by(NAME) %>%
  mutate(sentence_count = 1:n(),
         index = round(sentence_count / n(), 2)) %>%
  unnest_tokens(word, sentence) %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(NAME, index) %>%
  summarise(sentiment = sum(score, na.rm = TRUE)) %>%
  filter(sentiment <= -9)

data_tb %>%
  prep_analysis_sentence("TXT", SW = FALSE, AW = FALSE) %>%
  group_by(NAME) %>%
  mutate(sentence_count = 1:n(),
         index = round(sentence_count / n(), 2)) %>%
  filter((str_detect(NAME, "^Christie") & index == 0.390) |
         (str_detect(NAME, "^Gilmore") & index == 0.380) |
         (str_detect(NAME, "^Greenwalt") & index == 0.930) |
         (str_detect(NAME, "^Stackley") & index == 0.900)) %>%
  select(NAME, index, sentence_count, sentence) %>%
  write_csv("./Plots/Sentiment/UltraNeg.csv")






# -----------------------------------------------------------------------------
# Examining effects of sentiment negation with n-grams 
# -----------------------------------------------------------------------------
# Examine how often positively-associated words are preceded by negating words
# Negation words = no, not, never, without


# tokenize data into bi-grams
data_bigrams <- data_tb %>%
  unnest_tokens(bigram, TXT, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") 



# assess the most frequent words that have a sentiment score and were preceded 
# by negation words

# 9.
data_bigrams %>%
  filter(CLASS1 == "Reform",
         word1 %in% negation_words) %>%
  inner_join(get_sentiments("afinn"), by = c(word2 = "word")) %>%
  count(word1, word2, score, sort = TRUE) %>%
  ungroup() %>%
  mutate(contribution = n * score) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  ggplot(aes(reorder(word2, contribution), n * score, fill = n * score > 0)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(title = "Most frequent words preceded by negation words",
       subtitle = "Within Reforms",
       x = "Words preceded by negation words",
       y = "Sentiment score * # of occurrances") + 
  facet_wrap(~ word1, scales = "free", ncol = 1) + 
  coord_flip()

# 9.1 Reforms: faceted by reform & negation word
data_bigrams %>%
  filter(CLASS1 == "Reform",
         word1 %in% negation_words) %>%
  inner_join(get_sentiments("afinn"), by = c(word2 = "word")) %>%
  count(word1, word2, score, NAME, sort = TRUE) %>%
  ungroup() %>%
  mutate(contribution = n * score) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  ggplot(aes(reorder(word2, contribution), n * score, fill = n * score > 0)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(title = "Most frequent words preceded by negation words",
       subtitle = "Within Reforms",
       x = "Words preceded by negation words",
       y = "Sentiment score * # of occurrances") + 
  facet_grid(NAME ~ word1, scales = "free_y") +
  coord_flip()

# 10.
data_bigrams %>%
  filter(CLASS1 == "Compendium",
         word1 %in% negation_words) %>%
  inner_join(get_sentiments("afinn"), by = c(word2 = "word")) %>%
  count(word1, word2, score, sort = TRUE) %>%
  ungroup() %>%
  mutate(contribution = n * score) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  ggplot(aes(reorder(word2, contribution), n * score, fill = n * score > 0)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(title = "Most frequent words preceded by negation words",
       subtitle = "Within the Compendium",
       x = "Words preceded by negation words",
       y = "Sentiment score * # of occurrances") + 
  facet_wrap(~ word1, scales = "free", ncol = 1) + 
  coord_flip()

# 10.1 Compendium: faceted by expert (A-F) & negation word
data_bigrams %>%
  filter(CLASS1 == "Compendium",
         str_detect(NAME, "^[A-F]"),
         word1 %in% negation_words) %>%
  inner_join(get_sentiments("afinn"), by = c(word2 = "word")) %>%
  count(word1, word2, score, NAME, sort = TRUE) %>%
  ungroup() %>%
  mutate(contribution = n * score) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  ggplot(aes(reorder(word2, contribution), n * score, fill = n * score > 0)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(title = "Most frequent words preceded by negation words",
       subtitle = "Within Compendium: Experts A-F",
       x = "Words preceded by negation words",
       y = "Sentiment score * # of occurrances") + 
  facet_grid(NAME ~ word1, scales = "free_y") + 
  coord_flip()

# 10.2 Compendium: faceted by expert (G-L) & negation word
data_bigrams %>%
  filter(CLASS1 == "Compendium",
         str_detect(NAME, "^[G-L]"),
         word1 %in% negation_words) %>%
  inner_join(get_sentiments("afinn"), by = c(word2 = "word")) %>%
  count(word1, word2, score, NAME, sort = TRUE) %>%
  ungroup() %>%
  mutate(contribution = n * score) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  ggplot(aes(reorder(word2, contribution), n * score, fill = n * score > 0)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(title = "Most frequent words preceded by negation words",
       subtitle = "Within Compendium: Experts G-L",
       x = "Words preceded by negation words",
       y = "Sentiment score * # of occurrances") + 
  facet_grid(NAME ~ word1, scales = "free_y") + 
  coord_flip()

# 10.3 Compendium: faceted by expert (M-Z) & negation word
data_bigrams %>%
  filter(CLASS1 == "Compendium",
         str_detect(NAME, "^[M-Z]"),
         word1 %in% negation_words) %>%
  inner_join(get_sentiments("afinn"), by = c(word2 = "word")) %>%
  count(word1, word2, score, NAME, sort = TRUE) %>%
  ungroup() %>%
  mutate(contribution = n * score) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  ggplot(aes(reorder(word2, contribution), n * score, fill = n * score > 0)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(title = "Most frequent words preceded by negation words",
       subtitle = "Within Compendium: Experts M-Z",
       x = "Words preceded by negation words",
       y = "Sentiment score * # of occurrances") + 
  facet_grid(NAME ~ word1, scales = "free_y") + 
  coord_flip()



# -----------------------------------------------------------------------------
# Did the effects of sentiment negation affect the sentiment categorizations 
# -----------------------------------------------------------------------------
# FASA was the only document with substantial effects of negation ("not greater").  
# Want to re-examine the basic sentiment of FASA to see if the sentiment 
# categories change or shift order.  


# "not" does not have a negativeness or sentiment category associated with it    
get_sentiments("nrc") %>%
  filter(word == "not")      

# "greater" is only positively associated; it does not have a sentiment category 
# associated with it
get_sentiments("nrc") %>%
  filter(word == "greater")

