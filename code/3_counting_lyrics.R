
# Intro to tidyverse and tidytext with US Billboard Top 100 songs 1965-2015

# Compiled for Text mining in R and reproducible research in Digital Methods in Humanities and Social Sciences Summer School, Tartu 2019 
# by Peeter Tinits

# Dataset adapted from: https://github.com/walkerkq/musiclyrics
# Based on: https://en.wikipedia.org/wiki/Billboard_Hot_100



# Run this command to install the necessary packages
lapply(c("tidytext","tidyverse","here","ggrepel"), 
       function(x) if(!is.element(x, installed.packages())) install.packages(x, dependencies = T))


# tidytext is a library for R that follows the tidyverse logic, but is not included in the main distribution.
# It needs to be loaded separately as here.
library(tidyverse)
library(tidytext)
library(ggrepel)

# Part 1
# - Counting words
# - Locating words
#
# Part 2
# - Getting the relevant words
#    - Stopwords
#    - Keywords
#    - Sentiment words

# The important added function is unnest_tokens()
# This tokenizes the text - i.e. makes it into word tokens and distributes them one per line.
# It's a fairly simple function, for more extensive studies, you should experiment with other tokenizers as well.

billboard_data <- read_tsv(here::here("data/US_top100/billboard_lyrics_1965-2015.tsv"))


# Let's store this as a separate data frame
billboard_tokens <- billboard_data %>% 
  unnest_tokens("word",lyrics)


# Now this has a total of 1.6 million tokens (one per row)
billboard_tokens

# As before, we can count occurrences by any grouping item.
# We can count by word to get the frequencies of each word.
wordcounts <- billboard_tokens %>% 
  group_by(word) %>%
  summarise(n=n()) %>% 
  arrange(desc(n))

#Let's have a look
wordcounts


# We can group also by particular song, get the words that repeat a lot in songs
# Note that we know from earlier, that some songs by different artists have same titles

# This gives us repetitions of some words within songs
billboard_tokens %>% 
  group_by(artist,song,word) %>%
  summarise(n=n()) %>% 
  arrange(desc(n))


# Sanity check on the first one
song_test <- billboard_tokens %>% 
  filter(artist=="katy perry") %>% 
  filter(song=="roar")


# We can now get the wordcounts per each song.
# Need to include artist and year also to get unique ones.
billboard_tokens %>% 
  group_by(artist,song,year,word) %>%
  summarise(n=n()) %>% 
  arrange(desc(n))


# Sanity check on the first one
song_test <- billboard_tokens %>% 
  filter(artist=="britney spears") %>% 
  filter(song=="till the world ends")


# A better solution, build a new tokens frame filtering out duplicates
# Keeps the first occurrences only
billboard_tokens <- billboard_data %>% 
  group_by(artist,song) %>%
  filter(year==min(year)) %>% 
  unnest_tokens("word",lyrics)



###################################################
# Group by decades, artists
#################################################


# If we also group them by decade, we get top words by decade
decades_topwords <- billboard_tokens %>% 
  group_by(word,decade) %>%
  summarise(n=n()) %>% 
  arrange(desc(n)) %>%
  group_by(decade) %>% 
  mutate(n_rank=row_number()) %>% # we add row numbers
  ungroup() %>% 
  arrange(decade,desc(n))

decades_topwords

# We can plot these results
decades_topwords %>% 
  filter(n_rank<26) %>% # and use only rows that are within the first 50 per decade
  ggplot(aes(x=n_rank,y=decade,fill=word,label=word))+
  geom_tile(alpha=0.4)+
  geom_text()+
  guides(fill=FALSE)+
  coord_flip()


# To look at it better, we can also track specific options
decades_topwords %>% 
  filter(word=="love") %>% 
  ggplot(aes(x=n_rank,y=decade,fill=word,label=word))+
  geom_tile(alpha=0.4)+
  geom_text()+
  guides(fill=FALSE)+
  coord_flip()

# We can pick a couple of options
# | is the operator for OR. Any of these conditions should be true.
decades_topwords %>% 
  filter(word=="love"|word=="you"|word=="baby") %>% 
  ggplot(aes(x=n_rank,y=decade,fill=word,label=word))+
  geom_tile(alpha=0.4)+
  geom_text()+
  guides(fill=FALSE)+
  coord_flip()


# Top words by artist
artists_topwords <- billboard_tokens %>% 
  group_by(word,artist) %>%
  summarise(n=n()) %>% 
  arrange(desc(n)) %>%
  group_by(artist) %>% 
  mutate(n_rank=row_number()) %>% 
  ungroup()


# Top words for 4 selected artists
artists_topwords %>% 
  filter(n_rank<26) %>% 
  filter(artist=="madonna"|artist=="rihanna"|artist=="aretha franklin"|artist=="britney spears") %>% 
  #filter(word=="love"|word=="you"|word=="baby"|word=="time") %>% 
  ggplot(aes(x=n_rank,y=artist,fill=word,label=word))+
  geom_tile(alpha=0.4)+
  geom_text()+
  guides(fill=FALSE)+
  coord_flip()



###################################################
# Locations within song
#################################################


# As we added the rank data, we can also add the location data.
# row_number() indicates the position of the word
# n() indicates length of the song
# loc_dec divides each song into 10 quantiles
# We will use mutate() to add it to our current variable

billboard_tokens <- billboard_tokens %>% 
  group_by(artist,song) %>% 
  mutate(wordnr=row_number(),songlength=n()) %>% 
  mutate(loc_perc=wordnr/(songlength+1)) %>% 
  mutate(loc_dec=floor(loc_perc*5)/5) %>% 
  ungroup()

# Let's also add wordcounts for particular words in the dataframe
billboard_tokens <- billboard_tokens %>% 
  group_by(word) %>% 
  mutate(count=n(),in_songs=n_distinct(song,artist)) %>% 
  ungroup()


# All occurrences of hello.
billboard_tokens %>% 
  filter(word=="hello")


# As before, we can plot
billboard_tokens %>% 
  filter(word=="hello") %>% 
  ggplot(aes(x=loc_perc,y=song))+
  geom_point()


billboard_tokens %>% 
  filter(word=="party") %>% 
  ggplot(aes(x=loc_perc,y=song))+
  geom_point()


billboard_tokens %>% 
  filter(word=="love") %>% 
  ggplot(aes(x=loc_perc,y=song))+
  geom_point()

billboard_tokens %>% 
  filter(word=="end") %>% 
  ggplot(aes(x=loc_perc,y=song))+
  geom_point()

# Let's look at hello again
billboard_tokens %>% 
  filter(word=="hello") %>% 
  ggplot(aes(x=loc_perc,y=song))+
  geom_point()

# We can attach it a median point
billboard_tokens %>% 
  filter(word=="hello") %>% 
  ggplot(aes(x=loc_perc,y=song))+
  geom_point()+
  geom_vline(aes(xintercept=median(loc_perc)))
# However this was overly biased by many hellos in a few songs

# This can be balanced by taking a mean from each song.
# In this case, each song is one datapoint
billboard_tokens %>% 
  filter(word=="hello") %>% 
  group_by(artist,song) %>% 
  summarise(loc_perc=mean(loc_perc)) %>% 
  ggplot(aes(x=loc_perc,y=song))+
  geom_point()

# Let's mark the median now. It has moved slightly to the left.
billboard_tokens %>% 
  filter(word=="hello") %>% 
  group_by(artist,song) %>% 
  summarise(loc_perc=mean(loc_perc)) %>% 
  ggplot(aes(x=loc_perc,y=song))+
  geom_point()+
  geom_vline(aes(xintercept=median(loc_perc)))


# We can also use the location information to explore individual songs
# Here we set the location as x axis, and the number of repetitions on y axis. 
# geom_text_repel finds locations for the text so that they don't interrupt too much
# The plot takes top 30 words in each decile of the song (or less if there's less there)
billboard_tokens %>% 
  filter(song=="happy") %>% 
  group_by(word) %>% 
  arrange(loc_perc) %>% 
  mutate(n=n(),in_songs=n_distinct(song),wordseq=row_number()) %>% 
  group_by(word,n,in_songs) %>% 
  filter(n>1) %>% 
  mutate(slice=floor(loc_perc*10)/10) %>% 
  group_by(slice) %>% 
  top_n(30,n) %>% 
  ggplot(aes(x=loc_perc,y=n,label=word))+
  geom_text_repel(direction="y",segment.size=0,hjust = 0,force=0.5, box.padding = 0)

# Same plot, different song
billboard_tokens %>% 
  filter(artist=="the doors") %>% 
  filter(song=="light my fire") %>% 
  group_by(word) %>% 
  arrange(loc_perc) %>% 
  mutate(n=n(),in_songs=n_distinct(song),wordseq=row_number()) %>% 
  group_by(word,n,in_songs) %>% 
  filter(n>1) %>% 
  mutate(slice=floor(loc_perc*10)/10) %>% 
  group_by(slice) %>% 
  top_n(20,n) %>% 
  ggplot(aes(x=loc_perc,y=n,label=word))+
  geom_text_repel(direction="y",segment.size=0)

# Here we use the sequence of repetitions on y axis instead. The more the word is repeated, the higher it goes
billboard_tokens %>% 
  filter(artist=="the doors") %>% 
  filter(song=="hello i love you") %>% 
  group_by(word) %>% 
  arrange(loc_perc) %>% 
  mutate(n=n(),in_songs=n_distinct(song),wordseq=row_number()) %>% 
  group_by(word,n,in_songs) %>% 
  filter(n>1) %>% 
  mutate(slice=floor(loc_perc*10)/10) %>% 
  group_by(slice) %>% 
  top_n(20,n) %>% 
  ggplot(aes(x=loc_perc,y=wordseq,label=word))+
  geom_text_repel(direction="y",segment.size=0,hjust = 0,force=0.5, box.padding = 0)



# A constructed "average" doors song on the basis of this
billboard_tokens %>% 
  filter(artist=="the doors") %>% 
  group_by(word) %>% 
  mutate(n=n(),in_songs=n_distinct(song)) %>% 
  group_by(word,n,in_songs) %>% 
  summarise(median=median(loc_perc)) %>% 
  arrange(desc(median)) %>% 
  filter(n>3) %>% 
  mutate(slice=floor(median*10)/10) %>% 
  group_by(slice) %>% 
  top_n(5,n) %>% 
  ggplot(aes(x=median,y=1,label=word))+
  geom_text_repel(direction="y",segment.size=0)


# Let's calculate the medians per decade across all songs
word_medians <- billboard_tokens %>% 
  group_by(word,decade) %>% 
  mutate(in_songs=n_distinct(song)) %>%  # we recalculate in_songs for that decade
  ungroup() %>% 
  filter(in_songs>50) %>% #for all words that occur at least 50 songs in that decade
  group_by(decade,artist,song,word,in_songs) %>% 
  mutate(mean=mean(loc_perc))%>%  #take the median location within each song
  group_by(word,in_songs,decade) %>% 
  summarise(median=median(loc_perc)) %>% 
  ungroup() %>% 
  arrange(desc(median))

# A constructed "average" song for each decade
word_medians %>% 
  mutate(slice=floor(median*10)/10) %>% 
  group_by(slice,decade) %>% 
  top_n(20,in_songs) %>% 
  ggplot(aes(x=median,y=1,label=word))+
  geom_text_repel(direction="y",segment.size=0,hjust = 0,force=0.5, box.padding = 0)+
  facet_wrap(~decade,ncol=1)


###################################################
# Counting words within locations
#################################################


# Here we count the words within each location
locations_topwords <- billboard_tokens %>% 
  group_by(word,loc_dec) %>%
  summarise(n=n(),in_songs=n_distinct(song)) %>% 
  arrange(desc(n)) %>%
  group_by(loc_dec) %>% 
  mutate(n_rank=row_number()) %>% 
  arrange(desc(in_songs)) %>%
  mutate(in_songs_rank=row_number()) %>% 
  ungroup()

# And plot the top 25 in each 20-percentile slice
locations_topwords %>% 
  filter(n_rank<26) %>% 
  ggplot(aes(x=n_rank,y=loc_dec,fill=word,label=word))+
  geom_tile(alpha=0.4)+
  geom_text()+
  guides(fill=FALSE)+
  coord_flip()

# We can pick a couple of them to better undesrtand the trends
locations_topwords %>% 
  #filter(n_rank<26) %>% 
  filter(word=="love"|word=="like"|word=="baby"|word=="want") %>% 
  ggplot(aes(x=n_rank,y=loc_dec,fill=word,label=word))+
  geom_tile(alpha=0.4)+
  geom_text()+
  guides(fill=FALSE)+
  coord_flip()

# We get slightly different results if we use the number of songs they were in instead of raw number (i.e. in_songs_rank instead of n_rank)
locations_topwords %>% 
  #filter(n_rank<26) %>% 
  filter(word=="love"|word=="like"|word=="baby"|word=="want") %>% 
  ggplot(aes(x=in_songs_rank,y=loc_dec,fill=word,label=word))+
  geom_tile(alpha=0.4)+
  geom_text()+
  guides(fill=FALSE)+
  coord_flip()

# Here are some words that do show a difference
locations_topwords %>% 
  filter(word=="hello"|word=="meaning"|word=="bought"|word=="looked") %>% 
  ggplot(aes(x=n,y=loc_dec,fill=word,label=word))+
  geom_tile(alpha=0.4)+
  geom_label()+
  guides(fill=FALSE)+
  coord_flip()

# The locations are slightly different when counting just the number of songs (because there are many repetitions within the song)
locations_topwords %>% 
  filter(word=="hello"|word=="meaning"|word=="bought"|word=="looked") %>% 
  ggplot(aes(x=in_songs,y=loc_dec,fill=word,label=word))+
  geom_tile(alpha=0.4)+
  geom_label()+
  guides(fill=FALSE)+
  coord_flip()




#################################################
# Getting the relevant words
##################################################
#
# 1) Removing stopwords
# 2) Finding keywords
# 3) Finding emotion words
#
###################################################

# Stopwords are a common set to be removed during text analysis.
# Typically they meant to be the ones that are too common to inform about the current text.
# There is no universal set though.


###################################################
# Removing stopwords
#################################################

# This adds data from tidytext package.
data("stop_words")

# Let's look at this.
stop_words

# We can look at the top words by location with the stopwords removed
locations_topwords %>% 
  filter(n_rank<51) %>% 
  anti_join(stop_words, by = "word") %>%
  ggplot(aes(x=n_rank,y=loc_dec,fill=word,label=word))+
  geom_tile(alpha=0.4)+
  geom_text()+
  guides(fill=FALSE)+
  coord_flip()


# Let's store this as a variable
locations_topwords_nostop <- billboard_tokens %>% 
  anti_join(stop_words, by = "word") %>%
  group_by(word,loc_dec) %>%
  summarise(n=n()) %>% 
  arrange(desc(n)) %>%
  group_by(loc_dec) %>% 
  mutate(n_rank=row_number()) %>% 
  ungroup() %>% 
  arrange(loc_dec,desc(n))

# Now we can plot more easily here
locations_topwords_nostop %>% 
  filter(n_rank<51) %>% 
  ggplot(aes(x=n_rank,y=loc_dec,fill=word,label=word))+
  geom_tile(alpha=0.4)+
  geom_text()+
  guides(fill=FALSE)+
  coord_flip()



# Top words by artist without the stopwords
words_artist_nostop <- billboard_tokens %>% 
  anti_join(stop_words, by = "word") %>%
  group_by(word,artist) %>%
  summarise(n=n()) %>% 
  arrange(desc(n)) %>%
  group_by(artist) %>% 
  mutate(n_rank=row_number()) %>% 
  ungroup()

# Let's plot four selected artists again, now without stopwords
words_artist_nostop %>% 
  filter(n_rank<26) %>% 
  filter(artist=="madonna"|artist=="rihanna"|artist=="aretha franklin"|artist=="britney spears") %>% 
  ggplot(aes(x=n_rank,y=artist,fill=word,label=word))+
  geom_tile(alpha=0.4)+
  geom_text()+
  guides(fill=FALSE)+
  coord_flip()



###################################################
# Keyword analysis
#################################################


## Keyword analysis
## tf_idf https://en.wikipedia.org/wiki/Tf%E2%80%93idf
## "term frequency–inverse document frequency"
## reflects how important a word is to a document in a collection or corpus
## Meaning: it finds the words that are special to that text, compared to all other texts in the comparison set
## Read more in https://www.tidytextmining.com/tfidf.html


# Number of artists that use a word
# == number of documents for tf_idf
artists_word <- words_artist %>% 
  group_by(word) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n)) %>% 
  filter(n>9)



# Keywords of this artist against all other artists
words_artist %>% 
  bind_tf_idf(word,artist,n) %>%
  arrange(desc(tf_idf)) %>%
  group_by(artist) %>% 
  mutate(tf_idf_rank=row_number()) %>% 
  ungroup() %>% 
  filter(artist=="madonna"|artist=="rihanna"|artist=="aretha franklin"|artist=="britney spears") %>% 
  filter(tf_idf_rank<26) %>% 
  ggplot(aes(x=tf_idf_rank,y=artist,fill=word,label=word))+
  geom_tile(alpha=0.4)+
  geom_text()+
  guides(fill=FALSE)+
  coord_flip()

# Keywords of this artist against all other artists with stopwords removed
words_artist_nostop %>% 
  bind_tf_idf(word,artist,n) %>%
  arrange(desc(tf_idf)) %>%
  group_by(artist) %>% 
  mutate(tf_idf_rank=row_number()) %>% 
  ungroup() %>% 
  filter(artist=="madonna"|artist=="rihanna"|artist=="aretha franklin"|artist=="britney spears") %>% 
  filter(tf_idf_rank<26) %>% 
  ggplot(aes(x=tf_idf_rank,y=artist,fill=word,label=word))+
  geom_tile(alpha=0.4)+
  geom_text()+
  guides(fill=FALSE)+
  coord_flip()


#Find the keywords of each lady gaga song compared to other lady gaga songs
ladygaga_songs <- billboard_tokens %>%
  filter(artist=="lady gaga") %>% #Here only lady gaga songs are considered
  anti_join(stop_words, by = "word") %>%
  group_by(artist,song,word) %>%
  summarise(n=n()) %>% 
  bind_tf_idf(word, song, n) %>%
  arrange(desc(tf_idf)) %>%
  ungroup()

#Now plot the dataframe
ladygaga_songs %>% 
  group_by(song) %>%
  mutate(n_rank=row_number()) %>% 
  filter(tf_idf>0) %>% #Ifnore keywords that are not at all special to the text
  filter(n_rank<11) %>% #And take the top 10 from them - might be less available
  ggplot(aes(reorder(word, tf_idf), tf_idf, fill = song)) +
  geom_col() +
  labs(x = NULL, y = "tf-idf") +
  coord_flip()+
  guides(fill=FALSE) +
  facet_wrap(~song,scale="free_y")




##Distinguishing keywords for each artist compared to other artists (all songs are lumped together per artist)
tf_idf_artist <- billboard_tokens %>%
  anti_join(stop_words, by = "word") %>%
  group_by(artist,word) %>%
  summarise(n=n()) %>% 
  bind_tf_idf(word, artist, n) %>%
  arrange(desc(tf_idf)) %>%
  ungroup()


#Top 10 all time stored as variable
top10 <- billboard_data %>%
  group_by(artist) %>% 
  summarise(n=n()) %>%
  arrange(desc(n)) %>%
  mutate(n_rank=row_number()) %>% 
  filter(n_rank<11)


#reorder doesn't really work because there are groups there...
tf_idf_artist %>%
  rename(count=n) %>%
  inner_join(top10,by="artist") %>%
  group_by(artist) %>%
  filter(count>5) %>% #If we filter for words that occurred at least 5 times per that artist. Must be done AFTER tf_idf is calculated.
  top_n(10,tf_idf) %>%
  filter(tf_idf>0) %>%
  ggplot(aes(reorder(word, tf_idf), tf_idf, fill = artist)) +
  geom_col() +
  labs(x = NULL, y = "tf-idf") +
  coord_flip()+
  guides(fill=FALSE) +
  facet_wrap(~artist,scale="free_y")



##Distinguishing keywords for each decade compared to other decades (all songs and artists are lumped together per decade)
tf_idf_decade <- billboard_tokens %>%
  anti_join(stop_words, by = "word") %>%
  group_by(decade,word) %>%
  summarise(n=n()) %>% 
  bind_tf_idf(word, decade, n) %>%
  arrange(desc(tf_idf)) %>%
  ungroup()

tf_idf_decade %>%
  rename(count=n) %>%
  group_by(decade) %>%
  filter(count>10) %>% #If we filter for words that occurred at least 10 times for that decade. This must be done after tf_idf
  arrange(desc(tf_idf)) %>% 
  filter(tf_idf>0) %>%
  mutate(tf_idf_rank=row_number()) %>% 
  filter(tf_idf_rank<11) %>% 
  ggplot(aes(reorder(word, tf_idf), tf_idf, fill = factor(decade))) +
  geom_col() +
  labs(x = NULL, y = "tf-idf") +
  coord_flip()+
  guides(fill=FALSE) +
  facet_wrap(~decade,scale="free_y")




###################################################
# Sentiments of the songs
#################################################

##############################################################3
# Combining with sentiments
# Sentiments are given as a simple vocabulary table in tidytext
# Each word has just a sentiment positive or negative.
# tidytext has also other lexicons and many different lexicons are used.
# Here we can use this one.

# Here is the lexicon
get_sentiments("bing")

# As with stopwords we can use this to join with our table.
# Instead of anti_join, here we use inner_join, to keep only the vocabulary within the sentiment table

#Plotting option 1
tf_idf_decade %>%
  rename(count=n) %>%
  group_by(decade) %>%
  anti_join(stop_words, by = "word") %>%
  inner_join(get_sentiments("bing")) %>% 
  group_by(decade,sentiment) %>%
  filter(count>2) %>% #If we filter for words that occurred at least 5 times for that decade. This must be done after tf_idf
  arrange(desc(tf_idf)) %>%
  top_n(10,tf_idf) %>% #top 10 rows, top_n() method has a problem if there are too many ties
  filter(tf_idf>0) %>%
  arrange(desc(tf_idf)) %>%
  ggplot(aes(x=reorder(word,count), y=tf_idf, fill = factor(sentiment))) +
  geom_col() +
  labs(x = NULL, y = "tf-idf") +
  coord_flip()+
  guides(fill=FALSE) +
  facet_wrap(~decade+sentiment,scale="free_y") 

#Plotting option 2 with same data
tf_idf_decade %>%
  rename(count=n) %>%
  group_by(decade) %>%
  anti_join(stop_words, by = "word") %>%
  inner_join(get_sentiments("bing")) %>% 
  group_by(decade,sentiment) %>%
  filter(count>2) %>% #If we filter for words that occurred at least 5 times for that decade. This must be done after tf_idf
  top_n(10,tf_idf) %>%
  filter(tf_idf>0) %>%
  mutate(linenumber=row_number()) %>%
  arrange(desc(tf_idf)) %>%
  ggplot(aes(decade, (-1*linenumber))) + geom_point(color="white") + 
  geom_text(aes(label=word, color=factor(decade)),  fontface='bold', size=4) + 
  theme(legend.position="none", plot.title = element_text(size=18), 
        axis.title.y=element_text(margin=margin(0,10,0,0)),
        axis.text.x=element_text(size=16)) + 
  labs(title="Most Characteristic lyrics by decade \n Billboard year-End Top 100, 1965-2015") + 
  xlab("") + ylab("Ranking")+
  facet_wrap(~sentiment) +
  scale_y_continuous(limits=c(-15,-1), breaks=c(-10, -10, -.5), labels=c("#10", "#10", "#1"))+
  scale_x_continuous(limits=c(1955,2015))+
  theme_classic()



###################################################
# Locations within song
#################################################


#Looking at sentiments within songs
#The Doors is fairly positive
billboard_tokens %>%
  filter(artist=="the doors") %>%
  anti_join(stop_words, by = "word") %>%
  inner_join(get_sentiments("bing"), by = "word") %>% 
  mutate(line = wordnr %/% 40) %>% #Change the number here to group the words into smaller or larger chunks
  group_by(song,line,sentiment) %>% 
  summarise(n=n()) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative) %>% 
  ggplot(aes(x=line,y=sentiment))+
  geom_bar(aes(fill = sentiment>0),stat = 'identity')+
  theme_minimal()+
  facet_wrap(~song)


#Eminem has fairly negative songs throughout
billboard_tokens %>%
  filter(artist=="eminem") %>%
  anti_join(stop_words, by = "word") %>%
  inner_join(get_sentiments("bing"), by = "word") %>% 
  mutate(line = wordnr %/% 40) %>% #Change the number here to group the words into smaller or larger chunks
  group_by(song,line,sentiment) %>% 
  summarise(n=n()) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative) %>% 
  ggplot(aes(x=line,y=sentiment))+
  geom_bar(aes(fill = sentiment>0),stat = 'identity')+
  theme_minimal()+
  facet_wrap(~song)

#Aretha Franklin is fairly positive
billboard_tokens %>%
  filter(artist=="aretha franklin") %>%
  anti_join(stop_words, by = "word") %>%
  inner_join(get_sentiments("bing"), by = "word") %>% 
  mutate(line = wordnr %/% 40) %>% #Change the number here to group the words into smaller or larger chunks
  group_by(song,line,sentiment) %>% 
  summarise(n=n()) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative) %>% 
  ggplot(aes(x=line,y=sentiment))+
  geom_bar(aes(fill = sentiment>0),stat = 'identity')+
  theme_minimal()+
  facet_wrap(~song)

#The Beatles is fairly positive
billboard_tokens %>%
  filter(artist=="the beatles") %>%
  anti_join(stop_words, by = "word") %>%
  inner_join(get_sentiments("bing"), by = "word") %>% 
  mutate(line = wordnr %/% 40) %>% #Change the number here to group the words into smaller or larger chunks
  group_by(song,line,sentiment) %>% 
  summarise(n=n()) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative) %>% 
  ggplot(aes(x=line,y=sentiment))+
  geom_bar(aes(fill = sentiment>0),stat = 'identity')+
  theme_minimal()+
  facet_wrap(~song)

#Taylor Swift has some songs classified as quite negative
billboard_tokens %>%
  filter(artist=="taylor swift") %>%
  anti_join(stop_words, by = "word") %>%
  inner_join(get_sentiments("bing"), by = "word") %>% 
  mutate(line = wordnr %/% 40) %>% #Change the number here to group the words into smaller or larger chunks
  group_by(song,line,sentiment) %>% 
  summarise(n=n()) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative) %>% 
  ggplot(aes(x=line,y=sentiment))+
  geom_bar(aes(fill = sentiment>0),stat = 'identity')+
  theme_minimal()+
  facet_wrap(~song)

# We can average across all songs, sentiments separately
billboard_tokens %>% 
  inner_join(get_sentiments("bing"), by = "word") %>%
  group_by(artist,song,loc_dec,sentiment) %>% 
  summarise(n=n()) %>% 
  #count(loc_dec,sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative) %>% 
  group_by(loc_dec) %>% 
  summarise(sentiment=mean(sentiment),positive=mean(positive),negative=mean(negative)) %>% 
  ggplot()+
  geom_col(aes(x=loc_dec+0.045,y=positive,fill="positive"),width = 0.09)+
  geom_col(aes(x=loc_dec-0.045,y=negative,fill="negative"),width = 0.09)+
  theme_minimal()

# We can average across all songs, overall sentiment
billboard_tokens %>% 
  inner_join(get_sentiments("bing"), by = "word") %>%
  group_by(artist,song,loc_dec,sentiment) %>% 
  summarise(n=n()) %>% 
  #count(loc_dec,sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative) %>% 
  group_by(loc_dec) %>% 
  summarise(sentiment=mean(sentiment)) %>% 
  ggplot(aes(x=loc_dec,y=sentiment))+
  geom_bar(aes(fill = sentiment>0),stat = 'identity')+
  theme_minimal()



###################################################
# Sentiment trends over the years
#################################################


# General trends in positive emotions
plot1 <-
  billboard_tokens %>%
  inner_join(get_sentiments("bing")) %>%
  group_by(year,sentiment) %>% 
  summarise(n=n()) %>% 
  spread(sentiment, n, fill = 0, convert=T) %>%
  left_join(count(unnest_tokens(billboard_data,word, lyrics), year)) %>%
  ggplot(aes(year, (positive)/n)) + # change here
  geom_point(shape=19, alpha=.5, colour="black" ) +
  geom_smooth(method=lm, colour="black", fill="grey") +
  labs(y="Proportion", title="Positive emotions")  +
  theme_minimal() +
  scale_y_continuous(limits=c(0.02,0.07))+
  theme(plot.title = element_text(face="bold", hjust=.5))# +
#  ggsave(filename="plots/trend_example1.pdf", width = 5, height = 5)

#General trend for negative words
plot2 <-
  billboard_tokens %>%
  inner_join(get_sentiments("bing")) %>%
  group_by(year,sentiment) %>% 
  summarise(n=n()) %>% 
  spread(sentiment, n, fill = 0, convert=T) %>%
  left_join(count(unnest_tokens(billboard_data,word, lyrics), year)) %>%
  ggplot(aes(year, (negative)/n)) + # change here
  geom_point(shape=19, alpha=.5, colour="black" ) +
  geom_smooth(method=lm, colour="black", fill="grey") +
  labs(y="Proportion", title="Negative emotions")  +
  theme_minimal() +
  scale_y_continuous(limits=c(0.02,0.07))+
  theme(plot.title = element_text(face="bold", hjust=.5))# +
#  ggsave(filename="plots/trend_example2.pdf", width = 5, height = 5)


#Positive emotions go down, negative emotions go up. Slightly.
gridExtra::grid.arrange(plot1,plot2,ncol=2)
# See https://osf.io/3j6wx/ for more on this trend



#######################################################################################################
# Here you may have some extra time to explore the dataset.
# Try to recombine the code and ideas given to answers questions that you would be interested in.
#
# Some suggestions:
# Pick an artist or song and explore that.
#
# - What are the common words that they use?
# - What are the common words that they use if stopwords are excluded
# - What are the keywords that distinguish this artist from others?
# - What is the average sentiment in their songs?
# - How is that sentiment distributed in particular songs?

# Other, more general topics to explore?
# - Are the songs that are happier, better ranked?
# - Are artists that make happier songs, also more successful?
# - What are the average sentiments for the top artists in each decade?
# - What are the positive words that are getting less common over the years?
# - Etc

# Feel free to pick your own activity.

# A perfect result is, if you are able to make one plot that depicts what you want.
# Start by simply changing artist or song names within some functions.


# Whenever you make an interesting graph, save it - either use Export function on the plot window, or 
# ggsave(here::here("plots/my_plot_name.png")) # as in counting_lyrics.R
# If you are willing, share it with others here: http://tiny.cc/digmet_TM_plots

########################################################################################################
