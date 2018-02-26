################################################################################
# Amanda McGowin
# Thesis: An Analysis of Major Acquisition Reforms Through Text Mining and 
#         Grounded Theory Design 
# 
# 5. tf-idf & Word Relationships
#
# Contains:
#
# TF-IDF (line 25)
#   - TF: Term Frequency (line 47)
#   - Zipf's Law (line 100)
#   - TF-IDF: Term Frequency - Inverse Document Frequency (line 168)
#   - Rexamining term-frequency in comparison to tf-idf results (line 204)
#
# Word Relationships (line 232)
#   - n-gram (bi-gram) tokenization (line 240)
#   - n-gram (TF-IDF)bi-gram) Network Plots (line 291)
#   - n-gram TF-IDF (line 465)
################################################################################



#########################################################
#  TF-IDF: Term Frequency - Inverse Document Frequency  #
#########################################################

# identify high frequency words that provide particularly important context to 
# a single document within a group of documents.

# -----------------------------------------------------------------------------
# IDF decreases weight for commonly used words and increases weight for words
# used less frequently in a collection of documents.
# idf(t,D) = log(N/nt)
# Where idf of a given term (t) in a set of documents (D) is a function of the 
# total number of documents being assessed (N) and the number of documents where 
# the term (t) appears (nt) 

# TF-IDF computes the frequency of a term adjusted for how rarely it is used. 
# tf-idf(t,d,D) = tf(t,d) * idf(t,D)
# Where TF-IDF for a term (t) in document (d) for a set of documents (D) is 
# the product of that terms TF and IDF
# -----------------------------------------------------------------------------


# -----------------------------------------------------------------------------
# tf: Term Frequencies for each word
# -----------------------------------------------------------------------------

# NOTE: common Acquisition words removed

# Compendium Data
expert_freq <- data_tb %>%
  prep_analysis_word("TXT") %>%
  filter(CLASS1 == "Compendium") %>%
  count(NAME, word, sort=TRUE) %>%
  ungroup()

total_freq <- expert_freq %>%
  group_by(NAME) %>%
  summarise(total = sum(n))

expert_freq <- left_join(expert_freq, total_freq)


# Reform Data
reform_freq <- data_tb %>%
  prep_analysis_word("TXT") %>%
  filter(CLASS1 == "Reform") %>%
  count(NAME, word, sort=TRUE) %>%
  ungroup()

total_freq <- reform_freq %>%
  group_by(NAME) %>%
  summarise(total = sum(n))

reform_freq <- left_join(reform_freq, total_freq)



# Plot term frequencies
reform_freq %>% plot_tf("Reforms")


expert_freq %>%
  filter(str_detect(NAME, "^[A-F]")) %>%
  plot_tf("Compendium: Experts A-F")

expert_freq %>%
  filter(str_detect(NAME, "^[G-L]")) %>%
  plot_tf("Compendium: Experts G-L")

expert_freq %>%
  filter(str_detect(NAME, "^[M-Z]")) %>%
  plot_tf("Compendium: Experts M-Z")



# -----------------------------------------------------------------------------
# Zipf's Law
# -----------------------------------------------------------------------------

# Within a group of documents, the frequency of any word is inversely 
# proportional to its rank in a frequency table

# an inversely proportional relationship will have a constant, negative slope

# The deviations we see here at high rank are not uncommon for many kinds of 
# language; a corpus of language often contains fewer rare words than predicted 
# by a single power law. The deviations at low rank are more unusual. 


# for regression line in plots

# # compedium regression line
# lower_rank <- expert_freq %>%
#   group_by(NAME) %>%
#   mutate(rank = row_number(), `term freq` = n / total) %>%
#   filter(rank < 500)
# 
# lm(log10(`term freq`) ~ log10(rank), data = lower_rank)
# # result: y = =1.4287   m = -0.6299
# 
# 
# # reform regression line
# lower_rank <- reform_freq %>%
#   group_by(NAME) %>%
#   mutate(rank = row_number(), `term freq` = n / total) %>%
#   filter(rank < 500)
# 
# lm(log10(`term freq`) ~ log10(rank), data = lower_rank)
# # result: y = =1.0400  m = -0.8453
# # slope close to -1 follows Zipf's power law
# 
# remove(lower_rank)



# if lines are close and follow similar distribution, then distribution is 
# similar across the data set.  If tails of the distribution do not deviate far 
# from the regression line, the distribution approximately follows Zipf's Law

# NOTE: common Acquisition words removed
reform_freq %>% plot_zipf(y = -1.0409, m = -0.8453, append_t = "Reforms")


expert_freq %>% 
  filter(str_detect(NAME, c("^[A-E]", "^Fin", "^Fox, C"))) %>%
  plot_zipf(y = -1.4288, m = -0.6298, append_t = "Compendium: Experts A-F")

expert_freq %>% 
  filter(str_detect(NAME, c("^Fox, D", "^Fran","^[G-J]", "^Kam"))) %>%
  plot_zipf(y = -1.4288, m = -0.6298, append_t = "Compendium: Experts F-K")

expert_freq %>% 
  filter(str_detect(NAME, c("^Ken", "^[L-R]", "^Sc"))) %>%
  plot_zipf(y = -1.4288, m = -0.6298, append_t = "Compendium: Experts K-S")

expert_freq %>% 
  filter(str_detect(NAME, c("^St", "^Su", "^[V-Z]"))) %>%
  plot_zipf(y = -1.4288, m = -0.6298, append_t = "Compendium: Experts S-Z")

# expert_freq %>% plot_zipf(y = -1.4287, m = -0.6299, append_t = "Compendium")



# -----------------------------------------------------------------------------
# tf-idf: Word's Inverse Document Frequency 
# -----------------------------------------------------------------------------

# tf-idf: measures how important a word is to a document in a corpus.

# Calculating tf-idf attemps to find the words that are importat in a text, but 
# not too common.  tf-idf can help find important words that can provide 
# specific document context.  

# Decreases weight for commonly used words and increases weight for words that 
# are not used very much in a collection or corpus of documents.


# NOTE: common Acquisition words removed
reform_freq %>% plot_tf_idf(append_t = "Reforms")


expert_freq %>% 
  filter(str_detect(NAME, c("^[A-E]", "^Fin", "^Fox, C"))) %>%
  plot_tf_idf(append_t = "Compendium: Experts A-F")

expert_freq %>% 
  filter(str_detect(NAME, c("^Fox, D", "^Fran","^[G-J]", "^Kam"))) %>%
  plot_tf_idf(append_t = "Compendium: Experts F-K")

expert_freq %>% 
  filter(str_detect(NAME, c("^Ken", "^[L-R]", "^Sc"))) %>%
  plot_tf_idf(append_t = "Compendium: Experts K-S")

expert_freq %>% 
  filter(str_detect(NAME, c("^St", "^Su", "^[V-Z]"))) %>%
  plot_tf_idf(append_t = "Compendium: Experts S-Z")



# -----------------------------------------------------------------------------
# Rexamining term-frequency in comparison to tf-idf results 
# -----------------------------------------------------------------------------

# top15_reform_freq <- reform_freq %>%
#   bind_tf_idf(word, NAME, n) %>%
#   arrange(desc(tf_idf)) %>%
#   mutate(word = factor(word, levels = rev(unique(word))),
#          NAME = factor(NAME)) %>%
#   group_by(NAME) %>%
#   top_n(15, wt = tf_idf) %>%
#   ungroup 
# 
# 
# top15_expert_freq <- expert_freq %>%
#   bind_tf_idf(word, NAME, n) %>%
#   arrange(desc(tf_idf)) %>%
#   mutate(word = factor(word, levels = rev(unique(word))),
#          NAME = factor(NAME)) %>%
#   group_by(NAME) %>%
#   top_n(15, wt = tf_idf) %>%
#   ungroup 






########################
#  Word Relationships  #
########################

# Using n-grams to calculate, analyze, and visualize the relationships between 
# words


# -----------------------------------------------------------------------------
# n-gram (bi-gram) tokenization
# -----------------------------------------------------------------------------

# tokenize data into bi-grams
# removes bigrams containing double stop words/double Acq words
data_bigrams <- data_tb %>%
  unnest_tokens(bigram, TXT, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word) %>%
  filter(!word1 %in% common_ACQ$word,
         !word2 %in% common_ACQ$word) 

# data_bigrams %>% select(NAME, word1, word2)



# plot top 10 bi-grams per reform/expert

data_bigrams %>% 
  plot_bigram1()


data_bigrams %>% 
  filter(CLASS1 == "Reform") %>%
  plot_bigram2(topn = 10, append_t = "Reforms")


data_bigrams %>% 
  filter(CLASS1 == "Compendium") %>%
  filter(str_detect(NAME, c("^[A-E]", "^Fin", "^Fox, C"))) %>%
  plot_bigram2(topn = 5, append_t = "Experts A-F")

data_bigrams %>% 
  filter(CLASS1 == "Compendium") %>%
  filter(str_detect(NAME, c("^Fox, D", "^Fran","^[G-J]", "^Kam"))) %>%
  plot_bigram2(topn = 5, append_t = "Experts F-K")

data_bigrams %>% 
  filter(CLASS1 == "Compendium") %>%
  filter(str_detect(NAME, c("^Ken", "^[L-R]", "^Sc"))) %>%
  plot_bigram2(topn = 5, append_t = "Experts K-S")

data_bigrams %>% 
  filter(CLASS1 == "Compendium") %>%
  filter(str_detect(NAME, c("^St", "^Su", "^[V-Z]"))) %>%
  plot_bigram2(topn = 5, append_t = "Experts S-Z")



# -----------------------------------------------------------------------------
# Plot bigram networks
# -----------------------------------------------------------------------------

# data_bigrams %>% 
#   count(NAME, word1, word2, sort = TRUE) %>%
#   select(NAME, word1, word2, n)

# NOTE: Common Acquisition word bigrams removed

data_bigrams %>%
  filter(CLASS1 == "Reform") %>%
  plot_bigram_network(topn = 10, append_t = "Reforms")

data_bigrams %>%
  filter(str_detect(NAME, "^WSARA")) %>%
  plot_bigram_network(topn = 10, append_t = "Reforms: WSARA")

data_bigrams %>%
  filter(str_detect(NAME, "^FASA")) %>%
  plot_bigram_network(topn = 10, append_t = "Reforms: FASA")

data_bigrams %>%
  filter(str_detect(NAME, "^DAWIA")) %>%
  plot_bigram_network(topn = 10, append_t = "Reforms: DAWIA")

data_bigrams %>%
  filter(str_detect(NAME, "^Packard")) %>%
  plot_bigram_network(topn = 1, append_t = "Reforms: Packard Commission")

data_bigrams %>%
  filter(str_detect(NAME, "^Nunn")) %>%
  plot_bigram_network(topn = 10, append_t = "Reforms: Nunn McCurdy")






data_bigrams %>% 
  filter(CLASS1 == "Compendium") %>%
  plot_bigram_network(topn = 10, append_t = "Experts")


data_bigrams %>% 
  filter(str_detect(NAME, "^And")) %>%
  plot_bigram_network(topn = 1, append_t = "Expert: Anderson")

data_bigrams %>% 
  filter(str_detect(NAME, "^Aug")) %>%
  plot_bigram_network(topn = 1, append_t = "Expert: Augustine")

data_bigrams %>% 
  filter(str_detect(NAME, "^Ber")) %>%
  plot_bigram_network(topn = 1, append_t = "Expert: Berteau")

data_bigrams %>% 
  filter(str_detect(NAME, "^Bli")) %>%
  plot_bigram_network(topn = 1, append_t = "Expert: Blickstein")

data_bigrams %>% 
  filter(str_detect(NAME, "^Car")) %>%
  plot_bigram_network(topn = 1, append_t = "Expert: Cartwright")

data_bigrams %>% 
  filter(str_detect(NAME, "^Chr")) %>%
  plot_bigram_network(topn = 1, append_t = "Expert: Christie")

data_bigrams %>% 
  filter(str_detect(NAME, "^Eth")) %>%
  plot_bigram_network(topn = 1, append_t = "Expert: Etherton")

data_bigrams %>% 
  filter(str_detect(NAME, "^Fin")) %>%
  plot_bigram_network(topn = 1, append_t = "Expert: Finley")

data_bigrams %>% 
  filter(str_detect(NAME, "^Fox, C")) %>%
  plot_bigram_network(topn = 1, append_t = "Expert: Fox, C.")

data_bigrams %>% 
  filter(str_detect(NAME, "^Fox, D")) %>%
  plot_bigram_network(topn = 1, append_t = "Expert: Fox, D.")

data_bigrams %>% 
  filter(str_detect(NAME, "^Fra")) %>%
  plot_bigram_network(topn = 1, append_t = "Expert: Francis")

data_bigrams %>% 
  filter(str_detect(NAME, "^Gan")) %>%
  plot_bigram_network(topn = 1, append_t = "Expert: Gansler")

data_bigrams %>% 
  filter(str_detect(NAME, "^Gil")) %>%
  plot_bigram_network(topn = 1, append_t = "Expert: Gilmore")

data_bigrams %>% 
  filter(str_detect(NAME, "^Gor")) %>%
  plot_bigram_network(topn = 1, append_t = "Expert: Gordon")

data_bigrams %>% 
  filter(str_detect(NAME, "^Gre")) %>%
  plot_bigram_network(topn = 1, append_t = "Expert: Greenwalt")

data_bigrams %>% 
  filter(str_detect(NAME, "^Har")) %>%
  plot_bigram_network(topn = 1, append_t = "Expert: Harrison")

data_bigrams %>% 
  filter(str_detect(NAME, "^Jon")) %>%
  plot_bigram_network(topn = 1, append_t = "Expert: Jonas")

data_bigrams %>% 
  filter(str_detect(NAME, "^Kam")) %>%
  plot_bigram_network(topn = 1, append_t = "Expert: Kaminski")

data_bigrams %>% 
  filter(str_detect(NAME, "^Ken")) %>%
  plot_bigram_network(topn = 10, append_t = "Expert: Kendall")

data_bigrams %>% 
  filter(str_detect(NAME, "^Leh")) %>%
  plot_bigram_network(topn = 1, append_t = "Expert: Lehman")

data_bigrams %>% 
  filter(str_detect(NAME, "^McG")) %>%
  plot_bigram_network(topn = 1, append_t = "Expert: McGrath")

data_bigrams %>% 
  filter(str_detect(NAME, "^McN")) %>%
  plot_bigram_network(topn = 1, append_t = "Expert: McNichol")

data_bigrams %>% 
  filter(str_detect(NAME, "^Mor")) %>%
  plot_bigram_network(topn = 1, append_t = "Expert: Morin")

data_bigrams %>% 
  filter(str_detect(NAME, "^Oli")) %>%
  plot_bigram_network(topn = 1, append_t = "Expert: Oliver")

data_bigrams %>% 
  filter(str_detect(NAME, "^Rou")) %>%
  plot_bigram_network(topn = 1, append_t = "Expert: Roughead")

data_bigrams %>% 
  filter(str_detect(NAME, "^Schi")) %>%
  plot_bigram_network(topn = 1, append_t = "Expert: Schinasi")

data_bigrams %>% 
  filter(str_detect(NAME, "^Schw")) %>%
  plot_bigram_network(topn = 1, append_t = "Expert: Schwartz")

data_bigrams %>% 
  filter(str_detect(NAME, "^Sta")) %>%
  plot_bigram_network(topn = 1, append_t = "Expert: Stackley")

data_bigrams %>% 
  filter(str_detect(NAME, "^Sul")) %>%
  plot_bigram_network(topn = 1, append_t = "Expert: Sullivan")

data_bigrams %>% 
  filter(str_detect(NAME, "^Ven")) %>%
  plot_bigram_network(topn = 1, append_t = "Expert: Venlet")

data_bigrams %>% 
  filter(str_detect(NAME, "^War")) %>%
  plot_bigram_network(topn = 1, append_t = "Expert: Ward")

data_bigrams %>% 
  filter(str_detect(NAME, "^Zak")) %>%
  plot_bigram_network(topn = 1, append_t = "Expert: Zakheim")



# -----------------------------------------------------------------------------
# n-gram TF-IDF
# -----------------------------------------------------------------------------

# There are advantages and disadvantages to examining the tf-idf of bigrams 
# rather than individual words. Pairs of consecutive words might capture 
# structure that isn’t present when one is just counting single words, and may 
# provide context that makes tokens more understandable. However, the per-bigram 
# counts are also sparser: a typical two-word pair is rarer than either of its 
# component words. Thus, bigrams can be especially useful when you have a very 
# large text dataset. 


# reforms
data_bigrams %>% 
  filter(CLASS1 == "Reform") %>%
  plot_bigram_tf_idf(topn = 10, append_t = "Reforms", sub_t = "Top 10 bi-grams")

# compendium
data_bigrams %>% 
  filter(CLASS1 == "Compendium") %>%
  filter(str_detect(NAME, c("^[A-E]", "^Fin", "^Fox, C"))) %>%
  plot_bigram_tf_idf(topn = 5, append_t = "Experts A-F", sub_t = "Top 5 bi-grams")

data_bigrams %>% 
  filter(CLASS1 == "Compendium") %>%
  filter(str_detect(NAME, c("^Fox, D", "^Fran","^[G-J]", "^Kam"))) %>%
  plot_bigram_tf_idf(topn = 5, append_t = "Experts F-K", sub_t = "Top 5 bi-grams")

data_bigrams %>% 
  filter(CLASS1 == "Compendium") %>%
  filter(str_detect(NAME, c("^Ken", "^[L-R]", "^Sc"))) %>%
  plot_bigram_tf_idf(topn = 5, append_t = "Experts K-S", sub_t = "Top 5 bi-grams")

data_bigrams %>% 
  filter(CLASS1 == "Compendium") %>%
  filter(str_detect(NAME, c("^St", "^Su", "^[V-Z]"))) %>%
  plot_bigram_tf_idf(topn = 5, append_t = "Experts S-Z", sub_t = "Top 5 bi-grams")






