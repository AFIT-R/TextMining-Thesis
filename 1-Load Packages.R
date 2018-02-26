################################################################################
# Amanda McGowin
# Thesis: An Analysis of Major Acquisition Reforms Through Text Mining and 
#         Grounded Theory Design 
# 
# 1. Install & Load Packages 
#
# NOTE: Run scripts 1 through 3 prior to running analysis scripts
#
################################################################################


##########################################
#  CHECK IF REQUIRED PACKAGES INSTALLED  #
##########################################

package_required <- c("tidyverse", "stringr", "magrittr", "tidytext", "ldatuning", 
                      "topicmodels", "tm", "wordcloud", "RColorBrewer", "igraph",
                      "widyr", "ggraph", "devtools")
packages_new <- 
  package_required[!(package_required %in% installed.packages()[,"Package"])]

if(length(packages_new)) install.packages(packages_new)
remove(package_required, packages_new)



#######################
#  PACKAGES REQUIRED  #
#######################

library(ggraph)        # additional graphs for use with igraph
library(igraph)        # network graphs for mapping word relationships 
library(ldatuning)     # estimation/tuning of LDA model parameter (k)
library(magrittr)      # %>% for efficient code
library(stringr)       # text cleaning and regular expressions
library(tidyverse)     # data manipulation & plotting # INCLUDES: ggplot2, 
                         # tibble, tidyr, readr, purrr, dplyr
library(tidytext)      # provides additional text mining functions
library(topicmodels)   # fitting data to LDA model (Gibbs) 
library(RColorBrewer)  # additional color palettes for graphs and charts 
# library(widyr)         # pairwise counting & correlation of related words 
library(wordcloud)     # plots word clouds using text data
library(devtools)

install_github("dgrtwo/drlib")
library(drlib)

