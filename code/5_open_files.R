
#######################################################################
#This is an extra script for reading files                            #
#if you have them in one folder and want to process them              #
#as we did with pop songs or gutenberg texts.                         #
#Here it reads the files from the corpus included in the dataset      #
#######################################################################

library(tidyverse)
filelist <- list.files("data/corpus/",full.names=T)
texts <- map_df(filelist, ~ data_frame(txt = read_lines(.x)) %>%
                  mutate(filename = .x)) %>%
  mutate(filename= gsub("data/corpus/","",filename))


#And then just use the same tricks as before
#For example, make texts into wordlists
texts %>%
  unnest_tokens(word,txt)

#Get the wordcount of each file. This becomes much more interesting with metadata - e.g. year of publication or something else.
texts %>%
  unnest_tokens(word,txt) %>%
  group_by(filename) %>%
  summarise(n=n())

  
#etc