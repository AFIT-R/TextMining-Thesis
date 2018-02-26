################################################################################
# Amanda McGowin
# Thesis: An Analysis of Major Acquisition Reforms Through Text Mining and 
#         Grounded Theory Design 
# 
# 2. Functions 
#
################################################################################


###############
#  FUNCTIONS  #
###############

clean <- function(df, j) {
  # parse document by variable using <NEXT> as deliminator % tidy data and
  
  x <- unlist(str_split(df[[j]], pattern = "<NEXT>")) %>%
    str_replace_all(., pattern = "\r", replacement = " ") %>%
    str_replace_all(., pattern = "\n", replacement = " ") %>%
    str_replace_all(., pattern = "\\(.?\\)", replacement = "")
  
  return(x)
}


strip_var_names <- function(x) {
  # remove variable names from variable data
  
  for(j in seq_along(x)){
    x[[j]] <- unlist(strsplit(x[[j]], split=':: ', fixed=TRUE))[2]
  }
  
  return(x)
}


insert_row <- function(df, x) {
  df <- add_row(df,
                DOCUMENT = x[1],
                DATE = x[2],
                CLASS1 = x[3],
                CLASS2 = x[4],
                NAME = x[5],
                TYPE = x[6],
                SOURCE = x[7],
                URL = x[8],
                EXCERPT = x[9],
                NOTES = x[10],
                BIO = x[11],
                TXT = x[12]
  )
  return(df)
}


prep_analysis_word <- function(df, variable, SW = TRUE, AW = TRUE) {
  # unnests text data into individual words, then, by default, removes stop  
  # words (SW) and commonly used acquisition and DoD words (AW)
  
  # To keep common acquisition words in analysis, set AW to FALSE
  
  if(AW == TRUE & SW == TRUE) {
    df %>%
      group_by(NAME) %>%
      unnest_tokens_("word", variable) %>%
      anti_join(stop_words) %>%
      anti_join(common_ACQ)
  } else {
    
    if(AW == TRUE & SW == FALSE) {
      df %>%
        group_by(NAME) %>%
        unnest_tokens_("word", variable) %>%
        anti_join(common_ACQ)
    } else {
      
      if(AW == FALSE & SW == TRUE) {
        df %>%
          group_by(NAME) %>%
          unnest_tokens_("word", variable) %>%
          anti_join(stop_words)
      } else {
        
        df %>%
          group_by(NAME) %>%
          unnest_tokens_("word", variable)
      }
    }
  }
}


prep_analysis_sentence <- function(df, variable, SW = TRUE, AW = TRUE) {
  # unnests text data into individual words, then, by default, removes stop  
  # words and commonly used acquisition and DoD words
  
  # To keep common acquisition words in analysis, set AW to FALSE
  
  if(AW == TRUE & SW == TRUE) {
    df %>%
      group_by(NAME) %>%
      unnest_tokens_("sentence", variable, token = "Sentences") %>%
      anti_join(stop_words) %>%
      anti_join(common_ACQ)
  } else {
    
    if(AW == TRUE & SW == FALSE) {
      df %>%
        group_by(NAME) %>%
        unnest_tokens_("sentence", variable, token = "Sentences") %>%
        anti_join(common_ACQ)
    } else {
      
      if(AW == FALSE & SW == TRUE) {
        df %>%
          group_by(NAME) %>%
          unnest_tokens_("sentence", variable, token = "sentences") %>%
          anti_join(stop_words)
      } else {
        
        df %>%
          group_by(NAME) %>%
          unnest_tokens_("sentence", variable, token = "sentences")
      }
    }
  }
}


analyze_frequency <- function(df, variable, condition2 = NA) {
  # filters and mutates dataframe analyzing word usage
  
  if(!is.na(condition2)) {
    df %>%
      group_by_(variable) %>%
      count(word, sort = TRUE) %>%
      top_n(10) %>%
      filter(str_detect(NAME, condition2)) %>%
      ungroup() %>%
      mutate_(variable, text_order = nrow(.):1)
  } else {
    df %>%
      group_by_(variable) %>%
      count(word, sort = TRUE) %>%
      top_n(10) %>%
      ungroup() %>%
      mutate_(variable, text_order = nrow(.):1)
  }
}


analyze_frequency_pct <- function(df, condition, condition2 = NA) {
  
  # filters and mutates dataframe analyzing percentage of word usage
  
  if(!is.na(condition2)) {
    df %>%
      filter(CLASS1 == condition) %>%
      count(NAME, word) %>%
      mutate(NAME_words = n / sum(n)) %>%
      left_join(data_pct) %>%
      arrange(desc(NAME_words)) %>%
      filter(str_detect(NAME, condition2)) %>%
      ungroup()
  } else {
    df %>%
      filter(CLASS1 == condition) %>%
      count(NAME, word) %>%
      mutate(NAME_words = n / sum(n)) %>%
      left_join(data_pct) %>%
      arrange(desc(NAME_words)) %>%
      ungroup()
  }
}


analyze_sentiment_basic <- function(df, lex, condition, condition2 = NA) {
  
  # Analyzes sentiment within documnet by categorizing words as either positive
  # or negative, and if the NRC lexicon is used, further categorizes by 
  # sentimnet category (anger, anticipation, disgust, fear, joy, trust, sadness,
  # and surprise)
  
  # lex: either BING or NRC lexicon can be used
  # condition: either "Reform" or "Compendium"
  
  if(!is.na(condition2)) {
    df %>%
      inner_join(get_sentiments(lex)) %>%
      filter(!is.na(sentiment)) %>%
      filter(CLASS1 == condition) %>%
      ungroup() %>%
      filter(str_detect(NAME, condition2)) %>%
      count(sentiment, sort = TRUE) 
    
  } else {
    df %>%
      inner_join(get_sentiments(lex)) %>%
      filter(!is.na(sentiment)) %>%
      filter(CLASS1 == condition) %>%
      ungroup() %>%
      count(sentiment, sort = TRUE) 
  }
}


analyze_sentiment_NetScorePara <- function(df, condition, condition2 = NA) {
  
  # Analyzes positive/negative net score (count) across paragraphs within text
  # Paragraphs approximated to consist of 50 words each 
  
  # condition: either "Reform" or "Compendium"
  # condition2: used to analyze subset of experts within the Compendium (for 
  #             example, experts with last names begining A-F)
  
  if(!is.na(condition2)) {
    df %>%
      filter(CLASS1 == condition) %>%
      mutate(word_count = 1:n(),
             index = word_count %/% 50 + 1) %>%
      inner_join(get_sentiments("bing")) %>%
      filter(str_detect(NAME, condition2)) %>%
      count(NAME, index = index , sentiment) %>%
      ungroup()
    
  } else {
    df %>%
      filter(CLASS1 == condition) %>%
      mutate(word_count = 1:n(),
             index = word_count %/% 50 + 1) %>%
      inner_join(get_sentiments("bing")) %>%
      count(NAME, index = index , sentiment) %>%
      ungroup()
  }
}



plot_frequency_CLASS1 <- function(df, t, sub_t = NA, c = NA) {
  # plot percentage of word usage between REFORM and COMPENDIUM
  # using variable CLASS1
  
  df %>%
    ggplot(aes(drlib::reorder_within(word, n, CLASS1), n, fill = CLASS1)) +
    geom_bar(stat = "identity") +
      drlib::scale_x_reordered() +
      facet_wrap(~ CLASS1, scales = "free_y") +
      labs(x = "Top 10 Words",
           y = "Frequency",
           title = t,
           if(!is.na(sub_t)) {subtitle = sub_t},
           if(!is.na(c)) {caption = c}) +
      coord_flip() +
      theme(legend.position = "none",
            text = element_text(size = 20))
}


plot_frequency_NAME <- function(df, t, sub_t = NA, c = NA) {
  # plot percentage of word usage between reforms/authors in respective text
  # using variable CLASS1
  
  df %>%
    ggplot(aes(drlib::reorder_within(word, n, NAME), n, fill = NAME)) +
    geom_bar(stat = "identity") +
    drlib::scale_x_reordered() +
    facet_wrap(~ NAME, ncol = 3, scales = "free_y") +
    labs(x = "Top 10 Words",
         y = "Frequency",
         title = t,
         if(!is.na(sub_t)) {subtitle = sub_t},
         if(!is.na(c)) {caption = c}) +
    coord_flip() +
    theme(legend.position = "none",
          axis.text.y = element_text(size = 12),
          strip.text = element_text(size = 12))
}


plot_frequency_pct <- function(df, lab_y, append_y = NA, t, sub_t = NA, c = NA) {
  # plot percentage of word usage 
  
  df %>%
    ggplot(aes(x = NAME_words, y = all_words, 
               color = abs(all_words - NAME_words))) + 
      geom_abline(color = "gray40", lty = 2) + 
      geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
      geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) + 
      scale_x_log10(labels = scales::percent_format()) + 
      scale_y_log10(labels = scales::percent_format()) + 
      scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4",
                           high = "gray75") + 
      facet_wrap(~ NAME, ncol = 2) + 
      theme(legend.position = "none",
            text = element_text(size = 20)) + 
      labs(y = if(is.na(append_y)) {lab_y} else {paste(lab_y, append_y)}, 
           x = NULL,
           title = t,
           if(!is.na(sub_t)) {subtitle = sub_t},
           if(!is.na(c)) {caption = c})
}


plot_wordcloud <- function(df) {
  # apply filter and word count, then plot using wordcloud
  
  # layout(matrix(c(1, 2), nrow=2), heights=c(1, 6))
  # par(mar=rep(0, 4))
  # plot.new()
  # text(x=0.5, y=0.5, "Figure 1: Word Cloud for all Compendium text")
  
  df %>%
    count(word) %>%
    with(wordcloud(word, n, max.words = 200, random.order = FALSE, color = pal))
}


plot_sentiment_basic <- function(df, t, sub_t = NA, c = NA) {
  
  # Plots a basic sentiment count using the NRC lexicon.  
  # Brings positive & negative categories to top and colors blue,
  # Sorts remaing categories in order of greatest value 
  
  df %>% 
    mutate(ID = ifelse(sentiment %in% c("positive", "negative"), TRUE, FALSE)) %>%
    ggplot(aes(x = order_by(factor(sentiment, levels = c("positive", "negative")), 
                            reorder(sentiment, n)), 
               y = n)) + 
      geom_bar(aes(fill = ID), stat = "identity", show.legend = FALSE) +
      coord_flip() +
      scale_fill_manual(values = c("grey50", "dodgerblue")) +
      labs(x = "Sentiment & Category",
           y = "Count (n)",
           title = t,
           subtitle = sub_t,
           caption = c) +
      theme(text = element_text(size = 15))
}


plot_sentiment_NetScorePara <- function(df, t = NA, sub_t = NA, c = NA) {
  
  df %>% 
    spread(sentiment, n, fill = 0) %>%
    mutate(sentiment = positive - negative) %>%
    ggplot(aes(index, sentiment, fill = sentiment > 0)) +
      geom_bar(alpha = 0.5, stat = "identity", show.legend = FALSE) +
      facet_wrap(~ NAME, ncol = 2, scales = "free_x") + 
      labs(title = t,
           if(!is.na(sub_t)) {subtitle = sub_t},
           if(!is.na(c)) {caption = c},
           x = "Paragraph",
           y = "Net Sentiment Score")
}


plot_sentiment_sentence <- function(df, t = NA, sub_t = NA, c = NA, 
                                    xlab = NA, ylab = NA) {
  
  # Plots heatmap displaying sentiment across sentences within the document
  # Blue indicates positive sentimnet, red indicates negative, and White 
  # indicates netural  
  
  df %>% 
    ggplot(aes(index, 
               factor(NAME, levels = sort(unique(NAME), decreasing = TRUE)), 
               fill = sentiment)) + 
      geom_tile(color = "white") + 
      scale_fill_gradient2() + 
      scale_x_continuous(labels = scales::percent, expand = c(0, 0)) +
      scale_y_discrete(expand = c(0, 0)) +
      labs(x = xlab, y = ylab) + 
      ggtitle(t, subtitle = if(!is.na(sub_t)) {subtitle = sub_t}) + 
      theme_minimal() + 
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.position = "top")
}


plot_tf <- function(df, append_t = NA) {
  # tf: Term Frequencies for each word
  
  t <- "Term Frequencies"
  
  df %>%
    mutate(ratio = n / total) %>%
    ggplot(aes(ratio, fill = NAME)) +
      geom_histogram(show.legend = FALSE) +
      scale_x_log10() +
      labs(title = if(is.na(append_t)) {t} else {paste(t, "-", append_t)}) +
      facet_wrap(~ NAME, ncol = 3, scales = "free")
}


plot_zipf <- function(df, y, m, append_t = NA) {
  # Zipf's Law: Within a group of documents, the frequency of any word is 
  # inversely proportional to its rank in a frequency table
  
  # if lines are close and follow similar distribution, then distribution is 
  # similar across the data set.  If tails of the distribution do not deviate far 
  # from the regression line, the distribution approximately follows Zipf's Law
  
  t <- "Zipf's Law Distribution"
  
  df %>%
    group_by(NAME) %>%
    mutate(rank = row_number(), `term freq` = n / total) %>%
    ggplot(aes(rank, `term freq`, color = NAME)) +
      geom_abline(intercept = y, slope = m, color = "gray50", linetype = 2) +
      geom_line() +
      scale_x_log10() +
      scale_y_log10() + 
      labs(title = if(is.na(append_t)) {t} else {paste(t, "-", append_t)})
}


plot_tf_idf <- function(df, append_t = NA) {
  # tf-idf: measures how important a word is to a document in a corpus.
  
  # calculates and plots the top 15 tf-idf words (importat words in a text, but 
  # not too common) 
  
  t <- "Highest tf-idf words"
  
  df %>%
    bind_tf_idf(word, NAME, n) %>%
    arrange(desc(tf_idf)) %>%
    mutate(word = factor(word, levels = rev(unique(word))),
           NAME = factor(NAME)) %>%
    group_by(NAME) %>%
    top_n(10, wt = tf_idf) %>%
    ungroup %>%
    ggplot(aes(drlib::reorder_within(word, tf_idf, NAME), tf_idf, fill = NAME)) + 
      geom_bar(stat = "identity", alpha = .8, show.legend = FALSE) + 
      drlib::scale_x_reordered() +
      labs(title = if(is.na(append_t)) {t} else {paste(t, "in", append_t)},
           x = NULL, y = "tf-idf") +
      facet_wrap(~NAME, ncol = 3, scales = "free") + 
      coord_flip() +
      theme(axis.text.y = element_text(size = 15),
            strip.text = element_text(size = 12))
}


plot_bigram1 <- function(df) {
  
  # Plots top 10 bi-grams for reforms vs. experts
  # Differs from function plot_bigram2: groups by CLASS1 instead of by NAME
  
  df %>% 
    count(CLASS1, word1, word2, sort = TRUE) %>%
    unite("bigram", c(word1, word2), sep = " ") %>%
    group_by(CLASS1) %>%
    top_n(10) %>%
    ungroup() %>%
    ggplot(aes(reorder(bigram, n), n, fill = CLASS1)) + 
      geom_bar(stat = "identity", alpha = .8, show.legend = FALSE) + 
      labs(title = "Top 10 Bi-grams: Reforms vs. Compendium") +
      facet_wrap(~ CLASS1, ncol = 2, scales = "free") + 
      coord_flip() +
      theme(legend.position = "none",
            text = element_text(size = 20))
}


plot_bigram2 <- function(df, topn, append_t = NA, sub_t = NA) {
  
  # Plots top n bi-grams per reform/expert
  # Differs from function plot_bigram1: groups by NAME instead of by CLASS1 
  
  t <- paste("Top", topn, "Bi-grams")
  
  df %>% 
    count(NAME, word1, word2, sort = TRUE) %>%
    unite("bigram", c(word1, word2), sep = " ") %>%
    group_by(NAME) %>%
    top_n(topn) %>%
    ungroup() %>%
    ggplot(aes(drlib::reorder_within(bigram, n, NAME), n, fill = NAME)) + 
      geom_bar(stat = "identity", alpha = .8, show.legend = FALSE) + 
      drlib::scale_x_reordered() +
      labs(title = if(is.na(append_t)) {t} else {paste(t, "-", append_t)}) +
      facet_wrap(~ NAME, ncol = 3, scales = "free") + 
      coord_flip() +
      theme(legend.position = "none",
            text = element_text(size = 15))
}


plot_bigram_tf_idf <- function(df, topn, append_t = NA, sub_t = NA) {
  # tf-idf: measures how important a word is to a document in a corpus.
  
  # calculates and plots the top 15 tf-idf words (importat words in a text, but 
  # not too common) 
  
  t <- "Highest bi-gram tf-idf"
  
  df %>%
    count(NAME, word1, word2, sort = TRUE) %>%
    unite("bigram", c(word1, word2), sep = " ") %>%
    bind_tf_idf(bigram, NAME, n) %>%
    arrange(desc(tf_idf)) %>%
    group_by(NAME) %>%
    top_n(topn, wt = tf_idf) %>%
    ungroup %>%
    ggplot(aes(drlib::reorder_within(bigram, tf_idf, NAME), tf_idf, fill = NAME)) + 
      geom_bar(stat = "identity", alpha = .8, show.legend = FALSE) + 
      drlib::scale_x_reordered() +
      labs(title = if(is.na(append_t)) {t} else {paste(t, "in", append_t)},
           subtitle = if(is.na(sub_t)) {NULL} else {sub_t},
           x = NULL, y = "tf-idf") +
      facet_wrap(~NAME, ncol = 3, scales = "free") + 
      coord_flip() +
      theme(legend.position = "none",
            text = element_text(size = 15))
}


plot_bigram_network <- function(df, topn, append_t = NA) {
  
  t <- "Bi-gram Network Map"
  set.seed(1234)
  a <- grid::arrow(type = "closed", length = unit(.1, "inches"))
  
  df %>%
    count(word1, word2, sort = TRUE) %>%
    filter(n > topn) %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
      geom_edge_link(aes(edge_alpha = n), width = 1.1, arrow = a, end_cap = circle(1, 'mm')) + 
      geom_node_point(color = "lightblue", size = 2) + 
      geom_node_text(aes(label = name), vjust = 1.1, hjust = 1.1) + 
      labs(title = if(is.na(append_t)) {t} else {paste(t, "-", append_t)}) +
      theme_void()
}

