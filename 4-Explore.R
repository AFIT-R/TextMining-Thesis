################################################################################
# Amanda McGowin
# Thesis: An Analysis of Major Acquisition Reforms Through Text Mining and 
#         Grounded Theory Design 
# 
# 4. Exploratory Analysis  
#
# Contains:
# Word Cloud Plots (line 18), 
# Word Frequency Analysis (line 45)
# and 
# Word Frequency Analysis by Percentage (line 118)
################################################################################



################
#  WORD CLOUD  #
################

# (1) wordcloud for all COMPENDIUM document text
data_tb %>%
  prep_analysis_word("TXT") %>%
  filter(CLASS1 == "Compendium") %>%
  plot_wordcloud() 


# (2) wordcloud for all REFORM document text
data_tb %>%
  prep_analysis_word("TXT") %>%
  filter(CLASS1 == "Reform") %>%
  plot_wordcloud()


# wordcloud for biographys within the COMPENDIUM data

#png("wc.png")
data_tb %>%
  prep_analysis_word("BIO", AW = FALSE) %>%
  plot_wordcloud()



#############################
#  Word Frequency Analysis  #
#############################

# wordcount comparison between REFORM and COMPENDIUM document text
data_tb %>%
  prep_analysis_word("TXT", AW=FALSE) %>%
  analyze_frequency("CLASS1") %>%
  plot_frequency_CLASS1("Wordcount comparison between Reform and Compendium text")


# (3) wordcount comparison between REFORM and COMPENDIUM document text
# removing obvious words (acuisition, defense, ...)
data_tb %>%
  prep_analysis_word("TXT") %>%
  analyze_frequency("CLASS1") %>%
  plot_frequency_CLASS1(t ="Wordcount comparison between Reform and Compendium text",
                        sub_t = "With common acquisition words removed")



# (4) wordcount comparison between each reform within REFORM document text
# removing obvious words (acuisition, defense, ...) that may mask the trends 
# within the text
data_tb %>%
  prep_analysis_word("TXT") %>%
  filter(CLASS1 == "Reform") %>%
  analyze_frequency("NAME") %>%
  plot_frequency_NAME(t = "Wordcount comparison between reforms within Reform text", 
                      sub_t = "With common acquisition words removed")



# wordcount comparison between experts within COMPENDIUM document text
# removing obvious words (acuisition, defense, ...) that may mask the trends 
# within the text
data_tb %>%
  prep_analysis_word("TXT") %>%
  filter(CLASS1 == "Compendium") %>%
  analyze_frequency("NAME") %>%
  plot_frequency_NAME(t = "Wordcount comparison between experts within Compendium text", 
                      sub_t = "With common acquisition words removed")

# (5) Experts A-F
data_tb %>%
  prep_analysis_word("TXT") %>%
  filter(CLASS1 == "Compendium") %>%
  analyze_frequency("NAME", "^[A-F]") %>%
  plot_frequency_NAME(t = "Wordcount comparison between experts within Compendium text", 
                      sub_t = "Expert A-F",
                      c = "With common acquisition words removed")

# (6) Experts G-L
data_tb %>%
  prep_analysis_word("TXT") %>%
  filter(CLASS1 == "Compendium") %>%
  analyze_frequency("NAME", "^[G-L]") %>%
  plot_frequency_NAME(t = "Wordcount comparison between experts within Compendium text", 
                      sub_t = "Expert G-L",
                      c = "With common acquisition words removed")

# (7) Experts M-Z
data_tb %>%
  prep_analysis_word("TXT") %>%
  filter(CLASS1 == "Compendium") %>%
  analyze_frequency("NAME", "^[M-Z]") %>%
  plot_frequency_NAME(t = "Wordcount comparison between experts within Compendium text", 
                      sub_t = "Expert M-Z",
                      c = "With common acquisition words removed")




###############################
#  Percentage word frequency  #
###############################

# Compendium word frequency per expert (ALL)
# data_tb %>%
#   prep_analysis_word("TXT") %>%
#   prep_analysis_frequency_pct("Compendium") %>%
#   plot_frequency_pct("Acquisition Expert")


# (8) Compendium word frequency per expert (last names begining A-C)
data_tb %>%
  prep_analysis_word("TXT") %>%
  analyze_frequency_pct("Compendium", "^[A-C]") %>%
  plot_frequency_pct(lab_y = "Acquisition Expert", append_y = "(A-C)",
                     t = "Percentage of word usage across expert opinions: Expert A-C",
                     c = "*Common acquisition words removed")

# (9) Compendium word frequency per expert (last names begining E-F)
data_tb %>%
  prep_analysis_word("TXT") %>%
  analyze_frequency_pct("Compendium", "^[E-F]") %>%
  plot_frequency_pct(lab_y = "Acquisition Expert", append_y = "(E-F)",
                     t = "Percentage of word usage across expert opinions: Expert E-F",
                     c = "*Common acquisition words removed")


# (10) Compendium word frequency per expert (last names begining G-J)
data_tb %>%
  prep_analysis_word("TXT") %>%
  analyze_frequency_pct("Compendium", "^[G-J]") %>%
  plot_frequency_pct(lab_y = "Acquisition Expert", append_y = "(G-J)",
                     t = "Percentage of word usage across expert opinions: Expert G-L",
                     c = "*Common acquisition words removed")

# (11) Compendium word frequency per expert (last names begining K-M)
data_tb %>%
  prep_analysis_word("TXT") %>%
  analyze_frequency_pct("Compendium", "^[K-M]") %>%
  plot_frequency_pct(lab_y = "Acquisition Expert", append_y = "(K-M)",
                     t = "Percentage of word usage across expert opinions: Expert K-M",
                     c = "*Common acquisition words removed")

# (12) Compendium word frequency per expert (last names begining N-S)
data_tb %>%
  prep_analysis_word("TXT") %>%
  analyze_frequency_pct("Compendium", "^[N-S]") %>%
  plot_frequency_pct(lab_y = "Acquisition Expert", append_y = "(N-S)",
                     t = "Percentage of word usage across expert opinions: Expert N-S",
                     c = "*Common acquisition words removed")

# (13) Compendium word frequency per expert (last names begining V-Z)
data_tb %>%
  prep_analysis_word("TXT") %>%
  analyze_frequency_pct("Compendium", "^[V-Z]") %>%
  plot_frequency_pct(lab_y = "Acquisition Expert", append_y = "(V-Z)",
                     t = "Percentage of word usage across expert opinions: Expert V-Z",
                     c = "*Common acquisition words removed")

# (14) Reform word frequency per reform
data_tb %>%
  prep_analysis_word("TXT") %>%
  analyze_frequency_pct("Reform") %>%
  plot_frequency_pct(lab_y = "Acquisition Reform",
                     t = "Percentage of word usage across Major Reforms",
                     c = "*Common acquisition words removed")





# (14.1-14.5) Reform word frequency per reform
data_tb %>%
  prep_analysis_word("TXT") %>%
  analyze_frequency_pct("Reform", "^Pac") %>%
  plot_frequency_pct(lab_y = "Acquisition Reform", append_y = "(Packard Commission)",
                     t = "Percentage of word usage across Major Reforms",
                     c = "*Common acquisition words removed")

# (15.1-15.32) Compendium word frequency per expert
data_tb %>%
  prep_analysis_word("TXT") %>%
  analyze_frequency_pct("Compendium", "^Z") %>%
  plot_frequency_pct(lab_y = "Acquisition Expert", append_y = "(Zakheim)",
                     t = "Percentage of word usage across expert opinions",
                     c = "*Common acquisition words removed")





