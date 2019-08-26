
# Intro to tidyverse with US Billboard Top 100 songs 1965-2015

# Compiled for Text mining in R and reproducible research in Digital Methods in Humanities and Social Sciences Summer School, Tartu 2019 
# by Peeter Tinits

# Dataset adapted from: https://github.com/walkerkq/musiclyrics
# Based on: https://en.wikipedia.org/wiki/Billboard_Hot_100



# Run this command to install the necessary packages
lapply(c("tidytext","tidyverse","here"), 
       function(x) if(!is.element(x, installed.packages())) install.packages(x, dependencies = T))

library(tidyverse)



# Read in the dataframe
# read_tsv is a tidyverse command for reading files, and reads it as a tibble.
billboard_data <- read_tsv(here::here("data/US_top100/billboard_lyrics_1965-2015.tsv"))


# See the tibble here
billboard_data
# 5,100 rows/observations, 7 columns/variables
# Columns are type "double" == numbers; and type "character" == text.

# Information in file:
# rank, song(name), artist, year, lyrics, source, decade


# The main operator in tidyverse data processing is the pipe %>% 
# The pipe takes its input (this is before the pipe), and transmits it onward.

#The basic model is the following:
#data %>%
#  process()

# Overview of basic elements
# %>% - carry the data into function (try also ctrl+shift+m)
# select() selecting variables
# filter() provides basic filtering capabilities
# arrange() ordering data
# group_by() groups data by categorical levels
# summarise() summarise data by functions of choice
# mutate() create new variables


# Let's look at our data again
billboard_data

# Select takes one variable
billboard_data %>% 
  select(artist)

# Filter takes the rows that have the required condition
billboard_data %>% 
  filter(artist=="madonna")


# We can for example select all the songs
billboard_data %>% 
  filter(artist=="madonna") %>% 
  select(song)

# pull() makes the result into a list
billboard_data %>% 
  filter(artist=="madonna") %>% 
  select(song) %>% 
  pull()


# selection can be made inside pull()
billboard_data %>% 
  filter(artist=="madonna") %>% 
  pull(rank)


# Arrange orders them by some variable
# Here we order the songs alphabetically
billboard_data %>% 
  filter(artist=="madonna") %>% 
  arrange(song)

# desc() reverses the order: "descending order"
billboard_data %>% 
  filter(artist=="madonna") %>% 
  arrange(desc(song))

# Summarise summarises the values by some function
# min() - minimal value
# max() - maximal value
# n() - number of rows

billboard_data %>% 
  filter(artist=="madonna") %>% 
  summarise(firstyear=min(year))

billboard_data %>% 
  filter(artist=="madonna") %>% 
  summarise(lastyear=max(year))

billboard_data %>% 
  filter(artist=="madonna") %>% 
  summarise(n=n())


# We can do this by group.
# group_by groups the data by some variable.
# Notice that the tibble now has extra information
# A tibble: 5,100 x 7
# Groups:   artist [2,473]
# This means that it is grouped by artist name and there are 2,473 groups (== 2,473 artist names)
billboard_data %>% 
  group_by(artist)

# We can also group by song
# A tibble: 5,100 x 7
# Groups:   song [4,583]
billboard_data %>% 
  group_by(song)

# And we can group by more than one thing
# A tibble: 5,100 x 7
# Groups:   artist, song [4,899]
billboard_data %>% 
  group_by(artist,song)
# Question: Why are there now more groups then the previous one?





# Repeating song names
billboard_data %>% 
  group_by(song) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n))

billboard_data %>% 
  filter(song=="angel")

billboard_data %>% 
  filter(song=="dance with me")




# Now with groups, we can also summarise items
# We summarise by group, counting n() - number of rows.
billboard_data %>% 
  group_by(artist) %>% 
  summarise(n=n())
# Question: What does this number mean?

# As with the names, we can sort by this data:
billboard_data %>% 
  group_by(artist) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n))
# Question: What does this show?


# We can also add several functions to the summarise command
billboard_data %>% 
  group_by(artist) %>% 
  summarise(n=n(),firstyear=min(year)) %>% 
  arrange(desc(n))

# For example first and last year for each artist
billboard_data %>% 
  group_by(artist) %>% 
  summarise(n=n(),firstyear=min(year),lastyear=max(year)) %>% 
  arrange(desc(n))


# We can group by various columns
# For example by artist and year
billboard_data %>% 
  group_by(artist,year) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n))


# By artist and song
billboard_data %>% 
  group_by(artist,song) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n))

# And we can summarise multiple times.
# Here we group by n after the first grouping
# A new grouping replaces the old
billboard_data %>% 
  group_by(artist,song) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n)) %>% 
  group_by(n) %>% 
  summarise(nn=n())

# It is also possible to ungroup
billboard_data %>% 
  group_by(artist,song) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n)) %>% 
  ungroup() %>% 
  summarise(nn=n())

# What happens if we group by artist
billboard_data %>% 
  group_by(artist) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n)) %>% 
  group_by(n) %>% 
  summarise(nn=n())




# There is another way to use summary data.
# summarise uses the grouping variable, and removes all the extra values.
# mutate keeps all the values, but simply adds a new column or replaces an old one

# For example we can add a column for the number of times the artist had a song in the billboards
billboard_data %>% 
  group_by(artist) %>% 
  mutate(n=n()) %>% 
  arrange(desc(n))



# In this way we can for example filter the group for all artists that had more than 10 observations
billboard_data %>% 
  group_by(artist) %>% 
  mutate(n=n()) %>% 
  filter(n>10)


# To get their list, we can use pull. While "select" selects one or few columns, "pull" can take one column as a list.
# unique() outputs all its unique elements
billboard_data %>% 
  group_by(artist) %>% 
  mutate(n=n()) %>% 
  filter(n>10) %>% 
  pull(artist) %>% 
  unique()


# Question: How do we get all artist names that had more than 15 instances?






### Quick introduction to ggplot
# The most popular package to make nice graphs in R is called ggplot2.
# It is part of the tidyverse framework too, but has a slightly different syntax.
# Primarily, operations are continued with the plus sign (+) instead of the pipe

# A common plotting function would look like this.
# 
# 1) You take some data and you pass it through a pipe to the plotting function.
# 2) And you build up the plot by adding more layers and commands
#
# data %>%
# ggplot(mapping = aes(x=var1, y=var2)) +
#   geom_point()
#
# Look up ggplot2 for tips.



# Let's take data that we know.
billboard_data %>% 
  group_by(artist) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n)) %>% 
  filter(n>16)
# We get 12 most popular bands.

# Let's store it separately for convenience
plotdata <- billboard_data %>% 
  group_by(artist) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n)) %>% 
  filter(n>16)

# The variable now has the same table stored
plotdata


# We start by feeding the data in.
plotdata %>% 
  ggplot()
# The result is an empty plot.


# We can put n on the x axis.
# Currently we only have the basic frame
plotdata %>% 
  ggplot(aes(x=n))

# And the name on the y axis
plotdata %>% 
  ggplot(aes(x=n, y=artist))


# So here we have the points
plotdata %>% 
  ggplot(aes(x=n, y=artist))+
  geom_point()


# We can change some parameters, for example transparency
plotdata %>% 
  ggplot(aes(x=n, y=artist))+
  geom_point(alpha=0.4) #alpha=0.4 makes them only 40% visible



# We can add colors
plotdata %>% 
  ggplot(aes(x=n, y=artist, color=artist))+
  geom_point(alpha=0.4) #alpha=0.4 makes them only 40% visible


# We can add visual parameters
plotdata %>% 
  ggplot(aes(x=n, y=artist, color=artist))+
  geom_point(alpha=0.4)+
  theme_classic() #sets a different theme


# And we can change point types
plotdata %>% 
  ggplot(aes(x=n, y=artist, color=artist))+
  geom_point(alpha=0.4,pch=4)+ #pch - is the type of predefined point, e.g. x instead of o
  theme_classic()

# There are other data types
plotdata %>% 
  ggplot(aes(y=n, x=artist))+
  geom_col() # geom_bar makes a barplot. 


# A barplot looks sometimes nicer with coordinates reversed
plotdata %>% 
  ggplot(aes(y=n, x=artist))+
  geom_col()+
  coord_flip()

# For color, barplot takes fill, as it wants to fill the shape.
# color - for coloring edges
# fill - for coloring interiors
plotdata %>% 
  ggplot(aes(y=n, x=artist, fill=artist))+
  geom_col()+
  coord_flip()


# The sequence of the names comes from making the characters into a factor variable.
# R, by default, does this alphabetically, or in another given sequence.
# We need to set the factor levels ourselves to reorder them

# The tibble is ordered currently by n, so we can use unique to get the sequence we want.
plotdata %>% 
  pull(artist) %>% 
  unique()

# Now we need to mutate the dataframe before entering it into the plot
plotdata %>% 
  mutate(artist=factor(artist,levels=unique(artist))) %>% #by setting the levels in the factor, we control the sequence
  ggplot(aes(y=n, x=artist, fill=artist))+
  geom_col()+
  coord_flip()


# Use rev() to reverse the sequence
plotdata %>% 
  mutate(artist=factor(artist,levels=rev(unique(artist)))) %>%
  ggplot(aes(y=n, x=artist, fill=artist))+
  geom_bar(stat="identity")+
  coord_flip()



# A third option for plotting uses the characters
# Let's say we want top 10 artists per decade
# we can add row_numbers to get the ranks of each artist per decade
billboard_data %>% 
  group_by(artist,decade) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n)) %>% 
  group_by(decade) %>% 
  mutate(rownr=row_number())
  

# So here we can get ten band names per decade
# Let's store this for convenience  
plotdata2 <- billboard_data %>% 
  group_by(artist,decade) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n)) %>% 
  group_by(decade) %>% 
  mutate(rownr=row_number()) %>% 
  filter(rownr<11)


# We can build a tile plot
plotdata2 %>% 
  ggplot(aes(x=rownr,y=decade,fill=artist))+
  geom_tile()


# We can add text to the plot, x and y determine texts locations as with other layers
plotdata2 %>% 
  ggplot(aes(x=rownr,y=decade,fill=artist,label=artist))+
  geom_tile()+
  geom_text()


# And we can remove the legend
plotdata2 %>% 
  ggplot(aes(x=rownr,y=decade,fill=artist,label=artist))+
  geom_tile()+
  geom_text()+
  guides(fill=FALSE)
  

# And we can look at different aspects
plotdata2 %>% 
  ggplot(aes(x=n,y=rownr))+
  geom_point()


# And we can look at different aspects
plotdata2 %>% 
  ggplot(aes(x=n,y=rownr,color=factor(decade)))+
  geom_point()


# Another very useful function of ggplot is facet_wrap, you can make one plot into multiple ones
plotdata2 %>% 
  ggplot(aes(x=n,y=rownr,color=factor(decade)))+
  geom_point()+
  facet_wrap(~decade)


# And it's possible to rescale them if needed
plotdata2 %>% 
  ggplot(aes(x=n,y=rownr,color=factor(decade)))+
  geom_point()+
  facet_wrap(~decade, scales="free_x")


plotdata2 %>% 
  ggplot(aes(y=n,x=rownr,color=factor(decade)))+
  geom_line()+
  facet_wrap(~decade)


# We can also use it to explore some particular group
billboard_data %>% 
  filter(artist=="madonna") %>% 
  ggplot(aes(x=year,y=rank))+
  geom_point()



# We can also use it to explore some particular group
# | is the operator for OR. Any of these conditions should be true.
billboard_data %>% 
  filter(artist=="madonna"|artist=="elton john"|artist=="mariah carey"|artist=="stevie wonder") %>% 
  ggplot(aes(x=year,y=rank))+
  geom_point()+
  facet_wrap(~artist)


# These operations can be more complicated sequences,
# Here we count the observations per decade first, to be able to color them in the plot.
billboard_data %>% 
  group_by(artist,decade) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n)) %>% 
  head(20) %>% 
  arrange(desc(decade)) %>% 
  ungroup() %>% 
  mutate(artist=factor(artist,levels=unique(artist))) %>% 
  ggplot(aes(x=artist,y=n,fill=factor(decade)))+
  geom_col()+
  coord_flip()




# We can use this x and y to create a variety of plots
# Active years for each of the artists

billboard_data %>% 
  group_by(artist) %>% 
  summarise(n=n(),firstyear=min(year),lastyear=max(year)) %>% 
  arrange(desc(n)) %>% 
  head(20) %>% 
  mutate(artist=factor(artist,levels=unique(artist))) %>% 
  ggplot(aes(y=artist))+
  geom_segment(aes(yend=artist,x=firstyear,xend=lastyear),size=3)+
  theme_minimal()


# We can add a number for the number of songs they had as an extra layer
billboard_data %>% 
  group_by(artist) %>% 
  summarise(n=n(),firstyear=min(year),lastyear=max(year)) %>% 
  arrange(desc(n)) %>% 
  head(20) %>% 
  mutate(artist=factor(artist,levels=unique(artist))) %>% 
  ggplot(aes(y=artist))+
  geom_segment(aes(yend=artist,x=firstyear,xend=lastyear),size=3)+
  geom_text(aes(label=n,x=1963))+ # the extra layer is added here. If an aes() is given as a number, then the same value goes for all data points
  theme_minimal()


#Sorted by year of first begin
billboard_data %>% 
  group_by(artist) %>% 
  summarise(n=n(),firstyear=min(year),lastyear=max(year)) %>% 
  arrange(desc(n)) %>% 
  head(20) %>% 
  arrange(desc(firstyear)) %>%  # here we sort them by the first year before we set the factor levels
  mutate(artist=factor(artist,levels=unique(artist))) %>% 
  ggplot(aes(y=artist))+
  geom_segment(aes(yend=artist,x=firstyear,xend=lastyear),size=3)+
  geom_text(aes(label=n,x=1963))+
  theme_minimal()




  
#We can use labs to customize axis labels and title.
billboard_data %>% 
  group_by(artist) %>% 
  summarise(n=n(),firstyear=min(year),lastyear=max(year)) %>% 
  arrange(desc(n)) %>% 
  head(20) %>% 
  arrange(desc(firstyear)) %>% 
  mutate(artist=factor(artist,levels=unique(artist))) %>% 
  ggplot(aes(y=artist))+
  geom_segment(aes(yend=artist,x=firstyear,xend=lastyear),size=3)+
  geom_text(aes(label=n,x=1963))+
  theme_minimal()+
  labs(title="Artist's productive lifespan",x="Year",y="Artist")


# And we can also save this final plot.
ggsave(here::here("plots/artist_productive_years.png"))



# ggplot2 gives a variety of functions that you can discover as you use it further.
# See also the cheatsheet in the data folders.

