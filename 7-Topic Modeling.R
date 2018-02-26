################################################################################
# Amanda McGowin
# Thesis: An Analysis of Major Acquisition Reforms Through Text Mining and 
#         Grounded Theory Design 
# 
# 8. Topic Modeling   
#
# Contains:
# Convert tidy-data frame to matrix (line 18), 
# Estimate number of topics for LDA model parameter (k) (line 85), 
# and 
# Fit data to a topic model (line 174)  
################################################################################



############################################
#  convert tidy-text data frame to matrix  #
############################################

# Document Term Matrix

# data_tb$NAME <- as.character(data_tb$NAME)

data_mx <- data_tb %>%                     # All Data
  prep_analysis_word("TXT") %>%
  count(NAME, word) %>%
  cast_dtm(NAME, word, n)



data_mx_expert <- data_tb %>%              # Compendium Data
  prep_analysis_word("TXT") %>%
  filter(CLASS1 == "Compendium") %>%    
  count(NAME, word) %>%
  cast_dtm(NAME, word, n)

data_mx_expert <- data_mx_expert[apply(data_mx_expert, 1, sum)> 0, ] 



data_mx_reform <- data_tb %>%              # Reform Data 
  prep_analysis_word("TXT") %>%
  filter(CLASS1 == "Reform") 

data_mx_reform$NAME <- factor(data_mx_reform$NAME)

data_mx_reform <- data_mx_reform %>%
  count(NAME, word) %>%
  cast_dtm(NAME, word, n)

data_mx_reform <- data_mx_reform[apply(data_mx_reform, 1, sum)> 0, ] 



data_mx_Gansler <- subset_tb %>%              # Gansler Data
  prep_analysis_word("TXT") %>%
  filter(str_detect(NAME, "^Gansler")) 

data_mx_Gansler$NAME <- factor(data_mx_Gansler$NAME)

data_mx_Gansler <- data_mx_Gansler %>%    
  count(NAME, word) %>%
  cast_dtm(NAME, word, n)

data_mx_Gansler <- data_mx_Gansler[apply(data_mx_Gansler, 1, sum)> 0, ] 



data_mx_WSARA <- subset_tb %>%              # WSARA Subset Data
  prep_analysis_word("TXT") %>%
  filter(str_detect(NAME, "^WSARA")) 

data_mx_WSARA$NAME <- factor(data_mx_WSARA$NAME)

data_mx_WSARA <- data_mx_WSARA %>%    
  count(NAME, word) %>%
  cast_dtm(NAME, word, n)

data_mx_WSARA <- data_mx_WSARA[apply(data_mx_WSARA, 1, sum)> 0, ] 



##################################
#  Determining number of topics  #
##################################

# -----------------------------------------------------------------------------
# estimate best fitting number of topics for LDA model parameter (k) 
# -----------------------------------------------------------------------------

# ALL DATA 
# (2, 100, by=1) = about 10-14 topics    Runtime = 2 hr 10 mins (2-core)
# (2, 50, by=1) = about 8-14 topics    Runtime = 30.74 mins
# (2, 30, by=1) = about 6-10 topics    Runtime = 9.78 mins

result_all <- data_mx %>% 
  FindTopicsNumber(topics = seq(2, 30, by=1),
                   metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", 
                               "Deveaud2014"),
                   method = "Gibbs",
                   control = list(seed = 1234),
                   mc.cores = 3L,
                   verbose = TRUE) 

FindTopicsNumber_plot(result_all)


# Compendium DATA 
# (2, 20, by=1) = about 5-8 topics    Runtime = 2.735 mins

result_expert <- data_mx_expert %>% 
  FindTopicsNumber(topics = seq(2, 20, by=1),
                   metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", 
                               "Deveaud2014"),
                   method = "Gibbs",
                   control = list(seed = 1234),
                   mc.cores = 3L,
                   verbose = TRUE) 

FindTopicsNumber_plot(result_expert)


# Reform DATA 
# (2, 20, by=1) = about 7-9 topics    Runtime = 1.729 mins

result_reform <- data_mx_reform %>% 
  FindTopicsNumber(topics = seq(2, 20, by=1),
                   metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", 
                               "Deveaud2014"),
                   method = "Gibbs",
                   control = list(seed = 1234),
                   mc.cores = 3L,
                   verbose = TRUE) 

FindTopicsNumber_plot(result_reform)




# Gansler (For GT Comparison) 
# (2, 20, by=1) = about 4-8 topics    Runtime = 8.623 secs

result_Gansler <- data_mx_Gansler %>% 
  FindTopicsNumber(topics = seq(2, 20, by=1),
                   metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", 
                               "Deveaud2014"),
                   method = "Gibbs",
                   control = list(seed = 1234),
                   mc.cores = 3L,
                   verbose = TRUE) 

FindTopicsNumber_plot(result_Gansler)



# WSARA Subset (For GT Comparison) 
# (2, 20, by=1) = about 4-7 topics    Runtime = 6.784 secs

result_WSARA <- data_mx_WSARA %>% 
  FindTopicsNumber(topics = seq(2, 20, by=1),
                   metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", 
                               "Deveaud2014"),
                   method = "Gibbs",
                   control = list(seed = 1234),
                   mc.cores = 3L,
                   verbose = TRUE) 

FindTopicsNumber_plot(result_WSARA)



#####################
#  Topic Modeling  #
####################

# fit data to a topic model (number of topics, k=6-10)
lda_ALL <- LDA(data_mx, k = 10, control = list(seed = 1234))

# (number of topics, k=5-8)
lda_expert <- LDA(data_mx_expert, k = 8, control = list(seed = 1234))

# (number of topics, k=7-9)
lda_reform <- LDA(data_mx_reform, k = 9, control = list(seed = 1234))

# (number of topics, k=4-8)
lda_Gansler <- LDA(data_mx_Gansler, k = 8, control = list(seed = 1234))

# (number of topics, k=4-6)
lda_WSARA <- LDA(data_mx_WSARA, k = 6, control = list(seed = 1234))


# -----------------------------------------------------------------------------
# extract the "per-topic-per-word" probabilities (beta) form the LDA model
# computes probability of that term being generated from that topic
# top 10 terms that are most common within each topic
# This visualization lets us understand the two topics that were extracted from 
# the articles

#data_topics <- tidy(data_lda, matrix = "beta")
# -----------------------------------------------------------------------------

# All Data Topics
lda_ALL %>% tidy(matrix = "beta") %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  ggplot(aes(drlib::reorder_within(term, beta, topic), beta, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    drlib::scale_x_reordered() +
    facet_wrap(~ topic, scales = "free") +
    coord_flip() +
    theme(axis.text.y = element_text(size = 15),
          strip.text = element_text(size = 12))

# Compendium Data
lda_expert %>% tidy(matrix = "beta") %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  ggplot(aes(drlib::reorder_within(term, beta, topic), beta, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    drlib::scale_x_reordered() +
    facet_wrap(~ topic, scales = "free") +
    coord_flip() +
    theme(axis.text.y = element_text(size = 15),
          strip.text = element_text(size = 12))

# Reform Data
lda_reform %>% tidy(matrix = "beta") %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  ggplot(aes(drlib::reorder_within(term, beta, topic), beta, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    drlib::scale_x_reordered() +
    facet_wrap(~ topic, scales = "free") +
    coord_flip() +
    theme(axis.text.y = element_text(size = 15),
          strip.text = element_text(size = 12))


# Grounded Theory Comparison (Gansler)
lda_Gansler %>% tidy(matrix = "beta") %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  ggplot(aes(drlib::reorder_within(term, beta, topic), beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  drlib::scale_x_reordered() +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  theme(axis.text.y = element_text(size = 15),
        strip.text = element_text(size = 12))

# Grounded Theory Comparison (WSARA)
lda_WSARA %>% tidy(matrix = "beta") %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  ggplot(aes(drlib::reorder_within(term, beta, topic), beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  drlib::scale_x_reordered() +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  theme(axis.text.y = element_text(size = 15),
        strip.text = element_text(size = 12))



# # -----------------------------------------------------------------------------
# # extract the "per-document-per-topic" probabilities (gamma) form the LDA model
# # estimated proportion of words from that document generated from that topic 

# # probability of a topic belonging to a document??
# 
# data_documents <- tidy(data_lda, matrix = "gamma")
# # -----------------------------------------------------------------------------
# 
# data_documents <- tidy(lda_ALL, matrix = "gamma")












