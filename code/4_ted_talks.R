
# Intro to tidyverse and tidytext with a corpus of TED talks 


# Compiled for Text mining in R and reproducible research in Digital Methods in Humanities and Social Sciences Summer School, Tartu 2019 
# by Peeter Tinits

# Corpus from: https://www.kaggle.com/goweiting/ted-talks-transcript
# Based on: https://en.wikipedia.org/wiki/TED_(conference)

####################################
# New dataset - TED talks corpus
#####################################


# Run this command to install the necessary packages
lapply(c("tidytext","tidyverse","here","ggrepel"), 
       function(x) if(!is.element(x, installed.packages())) install.packages(x, dependencies = T))


library(tidyverse)
library(tidytext)
library(ggrepel)

## Read in the metadata
ted_meta <- read_tsv(here::here("data/TED_talks/ted_talks_meta.tsv"))

# Ratings are not used in this script, but feel free to include if you want.
#ted_ratings <- read_tsv(here::here("data/ted_talks_en/ted_talks_ratings.tsv"))

# Format the metadata a bit
ted_meta <- ted_meta %>% 
  mutate(eventtype=str_extract(event,"TED2|TEDGlobal|TEDx")) %>% #Other options: |TEDMED|TEDYouth|TEDSalon
  mutate(eventtype=str_replace(eventtype,"TED2","TEDMain"))
ted_meta <- ted_meta %>% 
  mutate(film_date=as.POSIXct(film_date, origin = "1970-01-01")) %>% 
  mutate(year=format(film_date, "%Y"))

# Exclude the talks that don't have a clear event type
ted_meta <- ted_meta %>% 
  filter(!is.na(eventtype)) 

# We make the filelist to call
filelist <- tibble(file=list.files(here::here("data/TED_talks/sub_tsvs"),full.names=T)) %>% 
  mutate(talkid=basename(file)) %>% 
  mutate(talkid=str_remove(talkid,".tsv$")) %>% 
  inner_join(ted_meta) %>% 
  pull(file)

# We read the files
ted_texts <- map_df(filelist, ~ read_tsv(.x,col_types = cols_only("c","c")) %>% #using characters as column types because it's faster.
                  mutate(talkid = .x)) %>% 
  mutate(talkid=basename(talkid)) %>% 
  mutate(talkid=str_remove(talkid,".tsv$"))

# We do some additional formatting to the data
ted_texts <- ted_texts %>% 
  mutate(startsecond=as.numeric(startsecond)) %>% 
  filter(startsecond!=0)
ted_texts <- ted_texts %>% 
  group_by(talkid) %>% 
  mutate(talk_in_seconds=max(startsecond)+2) %>% 
  mutate(startpercentage=startsecond/talk_in_seconds)


# Here is some basic statistics about the data
ted_meta %>% 
  group_by(main_speaker) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n))

ted_meta %>% 
  group_by(categories) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n))

ted_meta %>% 
  group_by(eventtype) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n))

ted_meta %>% 
  group_by(year,eventtype) %>% 
  summarise(n=n()) %>% 
  arrange(desc(year)) %>% 
  ggplot(aes(x=year,y=n,fill=eventtype))+
  geom_col()

ted_meta %>% 
  group_by(speaker_occupation) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n))


# We can start with text processing here
ted_tokens <- ted_texts %>%
  unnest_tokens("word","text")

# We can add the data on word locations in text as with song lyrics
ted_tokens <- ted_tokens %>% 
  group_by(talkid) %>% 
  mutate(wordnr=row_number(),talklength=n()) %>% 
  mutate(loc_perc=wordnr/(talklength+1)) %>% 
  mutate(loc_dec=floor(loc_perc*5)/5) %>% 
  ungroup()

# And we can add absolute counts of how many observations and talks there are per word
ted_tokens <- ted_tokens %>% 
  group_by(word) %>% 
  mutate(count=n(),in_talks=n_distinct(talkid)) %>% 
  ungroup()

#####################################################################################################
# Here, I would like You to pick up on what you've learned and try to explore the dataset a bit.
# You can feel free to choose what you are interested in, but here are some questions to start with:
#
# 1. What are the 10 most common words across the corpus?
# 2. What are the 10 most common words across the corpus, if you exclude the stopwords?
# 3. What are the 10 most common words for each ted event type, excluding stopwords?
# 4. Find one speaker and explore the talks given:
# 4.1 What are the distinguishing keywords for that speaker, vs all other speakers?
# 4.2 What is the average sentiment in their talks?
# 4.2 How do the sentiments vary over time in their talks? (hint: see end of counting_lyrics.R file)
# 4.3 Are there words that tend to be at the beginning or end of their talks? (hint: try to find median positions)
# 5. Pick a word and look at how it has been used (a word that is in more than 50 talks recommended)
# 5.1 Find the speaker who used it most times.
# 5.2 Make a graph that shows the locations of these use events for that speaker.
# 5.3 Has the usage of this word changed over the years in frequency across all speakers?
# 6. Pick a category and explore the topics given.
# 7. The dataset also has information on the professions of the speakers
# 7.1 Find the words that distinguish that profession from other professions.
# 7.2 Pick a few of those words and plot their locations within the talks given by that profession
# 7.3 Look at the positive and negative sentiment words for that profession. Plot the top 10 for each.
# 8. Explore the metadata with plots

# Whenever you make an interesting graph, save it - either use Export function on the plot window, or 
# ggsave(here::here("plots/my_plot_name.png")) # as in counting_lyrics.R
# If you are willing, share it with others here: http://tiny.cc/digmet_TM_plots




#######################################################################################
# Some inspiration is below. Feel free to reuse that code in your own explorations!
#


########################################################
## Locations in text
########################################################

# Medians just like with songs. 
# Since there is not many repetitions, we can use the raw data
word_medians <- ted_tokens %>% 
  filter(in_talks>50) %>% #for all words that occur at least 50 talks
  group_by(word,in_talks) %>% 
  summarise(median=median(loc_perc)) %>%  #another option is to use time median=median(startpercentage)
  ungroup() %>% 
  arrange(desc(median))

# "Average" TED talk
word_medians %>% 
  mutate(slice=floor(median*10)/10) %>% 
  group_by(slice) %>% 
  top_n(20,in_talks) %>% 
  ggplot(aes(x=median,y=1,label=word))+
  geom_text_repel(direction="y",segment.size=0,hjust = 0,force=0.5, box.padding = 0)

####################################################
## Sentiments
####################################################

# Sentiments per talk
ted_sentiments <- ted_tokens %>% 
  inner_join(get_sentiments("bing"),by="word") %>% 
  group_by(talkid,sentiment) %>% 
  summarise(n=n()) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative) %>% 
  ungroup()

ted_sentiments <- ted_sentiments %>%
  left_join(ted_meta)

# Store a variable
mean_sentiment <- ted_sentiments %>% 
  summarise(mean(sentiment)) %>% 
  pull()

# Hans Roslins is positive above average even among TED talks
ted_sentiments %>% 
  filter(main_speaker=="Hans Rosling") %>% 
  ggplot(aes(x=year,y=sentiment, color= sentiment>1))+
    geom_point()+
    geom_hline(aes(yintercept=mean_sentiment)) #average sentiment line.


# Sentiments by locations
ted_sentiments_locations <- ted_tokens %>% 
  inner_join(get_sentiments("bing"),by="word") %>% 
  group_by(talkid,sentiment,loc_dec) %>% 
  summarise(n=n()) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative)

ted_sentiments_locations %>% 
  group_by(loc_dec) %>% 
  summarise(sentiment=mean(sentiment)) %>% 
  ggplot(aes(x=loc_dec,y=sentiment))+
  geom_col(aes(fill = sentiment>0))+
  theme_minimal()

###############################################
## Top words
##############################################

# Top words by speaker
speakers_topwords_nostop <- ted_tokens %>%
  left_join(ted_meta) %>% 
  anti_join(stop_words, by = "word") %>%
  group_by(word,main_speaker) %>%
  summarise(n=n()) %>% 
  arrange(desc(n)) %>%
  group_by(main_speaker) %>% 
  mutate(n_rank=row_number()) %>% 
  filter(n_rank<26) %>% 
  ungroup()


speakers_topwords_nostop %>% 
  filter(main_speaker=="Bill Gates"|main_speaker=="Hans Rosling"|main_speaker=="Juan Enriquez"|main_speaker=="Lawrence Lessig"|main_speaker=="Al Gore") %>% 
  #filter(word=="love"|word=="you"|word=="baby"|word=="time") %>% 
  ggplot(aes(x=n_rank,y=main_speaker,fill=word,label=word))+
  geom_tile(alpha=0.4)+
  geom_text()+
  guides(fill=FALSE)+
  coord_flip()


# Top words by topic
topics_topwords_nostop <- ted_tokens %>%
  left_join(ted_meta) %>% 
  anti_join(stop_words, by = "word") %>%
  group_by(word,categories) %>%
  summarise(n=n(),in_talks=n_distinct(talkid)) %>% 
  arrange(desc(in_talks)) %>%
  group_by(categories) %>% 
  mutate(in_talks_rank=row_number()) %>% 
  arrange(desc(n)) %>%
  mutate(n_rank=row_number()) %>% 
  ungroup()


topics_topwords_nostop %>% 
  filter(in_talks_rank<26) %>% 
  filter(categories=="['Science & Technology']"|categories=="['People & Blogs']"|categories=="['Nonprofits & Activism']"|categories=="['News & Politics']"|categories=="['Entertainment']") %>% 
  ggplot(aes(x=in_talks_rank,y=categories,fill=word,label=word))+
  geom_tile(alpha=0.4)+
  geom_text()+
  guides(fill=FALSE)+
  coord_flip()


topics_topwords_nostop %>% 
  filter(n_rank<26) %>% 
  filter(categories=="['Science & Technology']"|categories=="['People & Blogs']"|categories=="['Nonprofits & Activism']"|categories=="['News & Politics']"|categories=="['Entertainment']") %>% 
  filter(word=="laughter"|word=="love"|word=="life"|word=="time") %>% 
  ggplot(aes(x=n_rank,y=categories,fill=word,label=word))+
  geom_tile(alpha=0.4)+
  geom_text()+
  guides(fill=FALSE)+
  coord_flip()


#########################
## Keywords
#########################

# Distinguishing keywords by topic
ted_categories_tf_idf <- ted_tokens %>%
  left_join(ted_meta) %>% 
  group_by(word,categories) %>%
  summarise(n=n(),in_talks=n_distinct(talkid)) %>% 
  bind_tf_idf(word,categories,in_talks) %>% 
  arrange(desc(tf_idf)) %>% 
  group_by(categories) %>% 
  mutate(tf_idf_rank=row_number()) %>% 
  ungroup() 

# Distinguishing keywords for 4 largest topics
ted_categories_tf_idf%>% 
  filter(categories=="['Science & Technology']"|categories=="['People & Blogs']"|categories=="['Nonprofits & Activism']"|categories=="['News & Politics']"|categories=="['Entertainment']") %>% 
  filter(tf_idf_rank<26) %>% 
  ggplot(aes(x=tf_idf_rank,y=categories,fill=word,label=word))+
  geom_tile(alpha=0.4)+
  geom_text()+
  guides(fill=FALSE)+
  coord_flip()

  
