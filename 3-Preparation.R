################################################################################
# Amanda McGowin
# Thesis: An Analysis of Major Acquisition Reforms Through Text Mining and 
#         Grounded Theory Design 
# 
# 3. Data Preparation & Formatting 
# 
################################################################################



######################
#  DATA PREPARATION  #
######################

# Read in Expert Compendium Data Files
datafiles <- lapply(Sys.glob("../COMPENDIUM/*.txt"), read_file)


# create an empty tibble to hold data 
data_tb <- tribble(~DOCUMENT, ~DATE, ~CLASS1, ~CLASS2, ~NAME, ~TYPE, ~SOURCE, 
                   ~URL, ~EXCERPT, ~NOTES, ~BIO, ~TXT)

# clean & Sort Expert Compendium Data
for(i in seq_along(datafiles)){
  
  # parse document by variable using <NEXT> as deliminator % tidy data and
  # remove variable names from variable data
  x <- clean(datafiles, i) %>%
    strip_var_names()
  
  data_tb <- insert_row(data_tb, x)  # insert data line into tibble
}



# Read in Defense Acquisition Reform/Legislation Data Files
datafiles <- lapply(Sys.glob("../REFORM/*.txt"), read_file)


# clean & Sort Defense Acquisition Reform/Legislation Data
for(i in seq_along(datafiles)){
  
  # parse document by variable using <NEXT> as deliminator % tidy data and
  # remove variable names from variable data
  x <- clean(datafiles, i) %>%
    str_replace_all(., pattern = "\\[.+?\\]", replacement = " ") %>%
    str_replace_all(., pattern = "\\<.+?\\>", replacement = " ") %>%
    str_replace_all(., pattern = "-", replacement = " ") %>%
    strip_var_names()
  
  data_tb <- insert_row(data_tb, x)  # insert data line into tibble
  
}


# convert following variables to required format (date, logical, & factor)
# and remove numbers from text 
data_tb %<>% 
  mutate(DATE = as.Date(DATE, format = "%d %b %Y"),
         DOCUMENT = as.factor(DOCUMENT),
         CLASS1 = as.factor(CLASS1),
         CLASS2 = as.logical(CLASS2),
         NAME = as.factor(NAME),
         TYPE = as.factor(TYPE), 
         SOURCE = as.factor(SOURCE),
         TXT = str_replace_all(TXT, pattern = "[0-9]", replacement = "") 
  )

remove(datafiles, i, x)

# remove non-ASCII characters from the text
data_tb$TXT <- iconv(data_tb$TXT, "", "ASCII", "")


# set seed for random number generation for replication purposes 
set.seed(1234)




######################
#  Formatting & Misc #
######################

# color brewer color palats 
pal <- brewer.pal(8, "Dark2")
#pal2 <- brewer.pal(9, "Set1")


# tibble containing frequent Acquisition words that do not add to the true 
# theam(s) of the document
common_ACQ <- tibble(word = c("acquisition", "defense", "dod", "title", 
                              "section", "ii", "iii", "shall", "amended", "pub", 
                              "subsec", "div", "xhtml", "subtitle", "u.s.c",
                              "uscode.house.gov", "subsection", "oct", "sec", 
                              "ckhrpdgx", "htp", "lojewih", "e.g", "ve", "vii",
                              "viii", "xii", "jan", "nov", "dec", "ix", "req",
                              "granu", "pre", "limtitle", "leid", "ausc", 
                              "chapter"))


# Negation words: for bi-gram sentiment analysis
negation_words <- c("not", "no", "never", "without")


# percentage of word usage across all expert's opinions 
data_pct <- data_tb %>%
  prep_analysis_word("TXT") %>%
  filter(CLASS1 == "Compendium") %>%
  ungroup() %>%
  count(word) %>%
  transmute(word, all_words = n / sum(n))


# word_tb <- data_tb %>%
#   group_by(NAME) %>%
#   unnest_tokens(word, TXT) %>%
#   anti_join(stop_words) %>%
#   anti_join(common_ACQ)
# write_csv(word_tb, "C:/Users/Amanda/Documents/AFIT/- THESIS/DATA & ANALYSIS/Analysis/Plots/words2.csv")





#######################################################
#  Import & Clean Data for Grounded Theory Validation #
#######################################################




# Read in Expert Compendium Data Files
datafiles <- lapply(Sys.glob("../GT Validation/*.txt"), read_file)


# create an empty tibble to hold data 
subset_tb <- tribble(~DOCUMENT, ~DATE, ~CLASS1, ~CLASS2, ~NAME, ~TYPE, ~SOURCE, 
                   ~URL, ~EXCERPT, ~NOTES, ~BIO, ~TXT)

# clean & Sort Defense Acquisition Reform/Legislation Data
for(i in seq_along(datafiles)){
  
  # parse document by variable using <NEXT> as deliminator % tidy data and
  # remove variable names from variable data
  x <- clean(datafiles, i) %>%
    str_replace_all(., pattern = "\\[.+?\\]", replacement = " ") %>%
    str_replace_all(., pattern = "\\<.+?\\>", replacement = " ") %>%
    str_replace_all(., pattern = "-", replacement = " ") %>%
    strip_var_names()
  
  subset_tb <- insert_row(subset_tb, x)  # insert data line into tibble
  
}


# convert following variables to required format (date, logical, & factor)
# and remove numbers from text 
subset_tb %<>% 
  mutate(DATE = as.Date(DATE, format = "%d %b %Y"),
         DOCUMENT = as.factor(DOCUMENT),
         CLASS1 = as.factor(CLASS1),
         CLASS2 = as.logical(CLASS2),
         NAME = as.factor(NAME),
         TYPE = as.factor(TYPE), 
         SOURCE = as.factor(SOURCE),
         TXT = str_replace_all(TXT, pattern = "[0-9]", replacement = "") 
  )

remove(datafiles, i, x)

# remove non-ASCII characters from the text
subset_tb$TXT <- iconv(subset_tb$TXT, "", "ASCII", "")
