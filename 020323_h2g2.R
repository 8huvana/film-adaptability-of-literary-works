#install and load packages
# install.packages("syuzhet")
# install.packages("spacyr")
# install.packages("gutenbergr")
# install.packages("dplyr")
# install.packages("stringr")
# install.packages("ggplot2")
# install.packages("stopwords")
# install.packages("quanteda")
# install.packages('quanteda.textplots')
# install.packages("readtext")
# install.packages("stm")              # For structural topic models
# install.packages("stminsights")      # For visual exploration of STM
# install.packages("wordcloud")        # To generate wordclouds
# install.packages("gsl")              # Required for the topicmodels package
# install.packages("topicmodels")      # For topicmodels
# install.packages("caret") 
#install.packages("ggpubr")

library("ggpubr")
library(syuzhet)
library(spacyr)
spacy_initialize(model = "en_core_web_sm")
library("wesanderson")
library("gutenbergr")
library("dplyr")
library(stringr)
library(ggplot2)
library(stopwords)
library(quanteda)
library(quanteda.textplots)
library(tidyverse)        # Also loads dplyr, ggplot2, and haven
library(readtext)         # To read .txt files
library(stm)              # For structural topic models
library(stminsights)      # For visual exploration of STM
library(wordcloud)        # To generate wordclouds
library(gsl)              # Required for the topicmodels package
library(topicmodels)      # For topicmodels
library(caret)            # For machine learning



require(readtext)
require(quanteda)

#create dataframe with metadata from filenames

df_h2g2 <- readtext("h2g2/*.txt",
                   docvarsfrom = "filenames", 
                   docvarnames = c("genre", "year"),
                   dvsep = "_", 
                   encoding = "utf-8")
#generate corpus

h2g2_corpus <- corpus(df_h2g2)

h2g2.stats <- summary(h2g2_corpus)
h2g2_token <-
  tokens(
    h2g2_corpus,
    split_hyphens = TRUE,
    remove_numbers = TRUE,
    remove_punct = TRUE,
    remove_symbols = TRUE,
    remove_url = TRUE,
    include_docvars = TRUE
  )

h2g2_dfm <- dfm(h2g2_token,tolower = TRUE) #convert to dfm
h2g2_dfm <- dfm_remove(h2g2_dfm, stopwords("english")) #remove stopwords
h2g2_dfm <- dfm_wordstem(h2g2_dfm) #use stemmer
#trim words appearing less than 1% and more than 99% - this has been effective in removing character names 
#but I'm leaning toward not using it because there isnt a foolproof way to remove secondary references to a character from another
#It's also importnant to retain this information in the scripts because it's a standin for line freq
#h2g2_dfm <- dfm_trim(h2g2_dfm, min_docfreq = 0.01, max_docfreq = 0.99, docfreq_type = "prop") 

dfm_sort(h2g2_dfm, decreasing = TRUE, margin = "both")

sentdict <- dictionary(data_dictionary_LSD2015) #load sentiment dictionary - Lexicoder
mftdict <- dictionary(file = "moralfoundationsdictionary.dic", format = "LIWC")

h2g2_mft <- dfm_lookup(h2g2_dfm, mftdict) #apply moral foundations dictionary to H2G2 corpus

#convert to df
h2g2_mft_df <- convert(h2g2_mft, "data.frame") %>% 
  gather(HarmVirtue:MoralityGeneral, key = "MF_Typ", value = "Share") %>% 
  mutate(doc_id = factor(doc_id))

#convert df to represent the percentage distribution of MFT types
h2g2_mft_df_perc <- h2g2_mft_df %>%                               
  group_by(doc_id) %>%
  mutate(perc = Share / sum(Share)) %>% 
  as.data.frame()


#visualize MFT type by text
ggplot(h2g2_mft_df_perc, aes(doc_id, perc, fill = MF_Typ)) +
  geom_bar(stat="identity") + coord_flip() +
  ggtitle("Topics for H2G2 Corpus applying the Moral Foundations Theory dictionary")+
  scale_fill_brewer(palette = "Spectral")+
  xlab("Genre") +
  ylab("Share of topics") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# create a dfm
# extract row with count for "life"/"universe" in each chapter
# and convert to data frame for plotting
life_universe_df <- h2g2_dfm %>% 
  dfm_keep(pattern = c("life", "universe")) %>% 
  convert(to = "data.frame")

#plotting occurrence of life
ggplot(data = life_universe_df, aes(x = doc_id, y = life, fill = doc_id)) + 
  geom_bar(stat = "identity") +
  scale_fill_manual(values = wes_palette("Darjeeling1", n = 4))+
  labs(x = "Genre", 
       y = "Frequency",
       title = 'Occurrence of "life"')+
  theme_light()


#plotting occurrence of universe
ggplot(data = life_universe_df, aes(x = doc_id, y = universe, fill = doc_id)) + 
  geom_bar(stat = "identity") +
  scale_fill_manual(values = wes_palette("Royal1", n = 4))+
  labs(x = "Genre", 
       y = "Frequency",
       title = 'Occurrence of "universe"')+
  theme_light()

newsmap.lexicon <- dictionary(file = "newsmap.yml", format = "YAML")
newsmap.lexicon <- dictionary(list(africa = unname(unlist(newsmap.lexicon$AFRICA)), america = unname(unlist(newsmap.lexicon$AMERICA)), asia = unname(unlist(newsmap.lexicon$ASIA)), europe = unname(unlist(newsmap.lexicon$EUROPE)), oceania = unname(unlist(newsmap.lexicon$OCEANIA))))

h2g2_newsmap <- dfm_lookup(h2g2_dfm, newsmap.lexicon) #apply newsmap dictionary to H2G2 corpus

#convert to df
h2g2_newsmap_df <- convert(h2g2_newsmap, "data.frame") %>% 
  gather(africa:oceania, key = "NewsMap_Region", value = "Share") %>% 
  mutate(doc_id = factor(doc_id))

#plot newsmap df
ggplot(h2g2_newsmap_df, aes(doc_id, Share, colour = NewsMap_Region, fill = NewsMap_Region)) +
  geom_bar(stat="identity") + scale_colour_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Pastel1") +
  ggtitle("Mentioned world regions within the H2G2 corpus applying the NewsMap dictionary") +
  xlab("Genre") + ylab("Terms") + theme(axis.text.x = element_text(angle = 45, hjust = 1))

readability <- textstat_readability(corpus(h2g2_corpus), "Flesch.Kincaid")

ggplot(readability, aes(x=document, y=Flesch.Kincaid)) +
  geom_segment( aes(x=document, xend=document, y=0, yend=Flesch.Kincaid) , size=1, color="indianred1", linetype="dotdash" ) +
  geom_point( size=5, color="red", fill=alpha("orange", 0.3), alpha=0.7, shape=21, stroke=2)+
  coord_flip()+
  xlab("Flesch-Kincaid Readability Score")+
  ylab("Genre")+
  theme_minimal()

film <- get_text_as_string("h2g2/film_2005.txt")
book <- get_text_as_string("h2g2/book_1979.txt")
tv <- get_text_as_string("h2g2/tv_1981.txt")
radio <- get_text_as_string("h2g2/radio_1978.txt")

h2g2 <- c(film = film, book = book, tv = tv, radio = radio)

#comparison word cloud with quanteda
textplot_wordcloud(h2g2_dfm, comparison = TRUE,
                   color = c("blue", "red", "orange", "green"))

parsedtxt <- spacy_parse(h2g2, tag = TRUE, entity = TRUE, lemma = FALSE)

tokens_h2 <- spacy_tokenize(
  h2g2,
  "word",
  remove_punct = TRUE,
  remove_url = TRUE,
  remove_numbers = TRUE,
  remove_separators = TRUE,
  remove_symbols = TRUE,
  padding = FALSE,
  multithread = TRUE) %>% 
  as.tokens()



sw <- c(stopwords("english"), "'s", "n't", "continued", "cont'd", "draft", "int", "cont", "'ve", "'re", "background", "'m", "'ll", "Series", "Episodes", "8/8/03")

tokens_remove(tokens_h2, sw)

wordsdf <- convert(dfm(tokens_h2), to = "data.frame")


posdf <- parsedtxt %>% group_by(doc_id,pos) %>% 
  summarise(total_count=n(),.groups = 'drop') %>% 
  as.data.frame()

#filter by pos
verbs <- posdf %>% group_by(doc_id) %>% filter(pos=="VERB")
noun <- posdf %>% group_by(doc_id) %>% filter(pos=="NOUN")
adj <- posdf %>% group_by(doc_id) %>% filter(pos=="ADJ")
pronoun <- posdf %>% group_by(doc_id) %>% filter(pos=="PRON")
propn <- posdf %>% group_by(doc_id) %>% filter(pos=="PROPN")

#filter by genre
filmpos <- posdf %>% group_by(pos) %>% filter(doc_id=="film")
tvpos <- posdf %>% group_by(pos) %>% filter(doc_id=="tv")
bookpos <- posdf %>% group_by(pos) %>% filter(doc_id=="book")
radiopos <- posdf %>% group_by(pos) %>% filter(doc_id=="radio")

#filter for the radial plot, exclude less significant pos
posdf4plot <- posdf %>%  filter(pos %in% c("PRON", "PROPN", "NOUN", "VERB", "ADJ"))

posplt <- ggplot(posdf4plot) +
  # Make custom panel grid
  geom_hline(
    aes(yintercept = y), 
    data.frame(y = c(0:3) * 1000)
  ) + 
  # Add bars to represent the cumulative track lengths
  # str_wrap(region, 5) wraps the text so each line has at most 5 characters
  # (but it doesn't break long words!)
  geom_col(
    aes(
      x = reorder(str_wrap(pos, 5), total_count),
      y = total_count,
      fill = doc_id
    ),
    position = "dodge2",
    show.legend = TRUE,
    alpha = .7
  ) +
  scale_fill_manual(values = wes_palette("BottleRocket2", n = 4))+
  
  # Add dots to represent the mean gain
  # Lollipop shaft for mean gain per region
  geom_segment(
    aes(
      x = reorder(str_wrap(pos, 5), total_count),
      y = 0,
      xend = reorder(str_wrap(pos, 5), total_count),
      yend = 3000
    ),
    linetype = "dashed",
    color = "gray12"
  ) + 
  
  # Make it circular!
  coord_polar()

posplt

#Zipf's Law eval

#create word df
wordsdf <- tokens_h2 %>% group_by(token, doc_id) %>% 
  summarise(total_count=n(),.groups = 'drop') %>% 
  as.data.frame()
#add total column for each genre
wordsdf <- wordsdf %>% 
  group_by(doc_id) %>% 
  mutate(total_genre = sum(total_count))

wordsdf <- wordsdf[order(wordsdf$total_count, decreasing = TRUE),]

freq_by_rank <- wordsdf %>% 
  group_by(doc_id) %>% 
  mutate(rank = row_number(), 
         `term frequency` = total_count / total_genre)
#plot zipf
freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = doc_id)) + 
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()

#facetbar function 
facet_bar <- function(df, y, x, by, nrow = 2, ncol = 2, scales = "free") {
  mapping <- aes(y = reorder_within({{ y }}, {{ x }}, {{ by }}), 
                 x = {{ x }}, 
                 fill = {{ by }})
  
  facet <- facet_wrap(vars({{ by }}), 
                      nrow = nrow, 
                      ncol = ncol,
                      scales = scales) 
  
  ggplot(df, mapping = mapping) + 
    geom_col(show.legend = FALSE) + 
    scale_y_reordered() + 
    facet + 
    ylab("")
} 


#TTR (Type-Token Ratio) Calculation
lexdiv <- tokens(h2g2_corpus) %>% 
  textstat_lexdiv(measure = "TTR")

# hapaxes per document
#hapax richness = number of words that occur only once / by the total number of words

rowSums(h2g2_dfm == 1) %>% head()
hapax_proportion <- rowSums(h2g2_dfm == 1) / ntoken(h2g2_dfm)
hapax_proportion
hapax_df <- data.frame(document=names(hapax_proportion), hapax=hapax_proportion, row.names=NULL)

ratio_list <- list(readability, hapax_df, lexdiv)      
ratio_list <- ratio_list %>% reduce(full_join, by='document')

normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}


#verify normalization

rat_norm <- as.data.frame(lapply(ratio_list[2:4], normalize))
#add genre column
rat_norm$genre <- ratio_list$document

rat_norm <- rat_norm %>%
  select(genre, everything())

lcols <- c("#FFB500", "#007B88", "#FF595F", "forestgreen")

ggradar(rat_norm,
        background.circle.colour = "white",
        axis.line.colour = "gray60",
        gridline.min.colour = "gray60",
        gridline.mid.colour = "gray60",
        gridline.max.colour = "gray60",
        group.colours = lcols)


library(quanteda.corpora)

h2g2_dfm <- tokens(h2g2_corpus, remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE, remove_url = TRUE) %>%
  tokens_remove(c(stopwords("en"), "continued", "cont'd", "3rd", "ext", "draft", "continuous", "cont")) %>%
  dfm()

# hierarchical clustering - get distances on normalized dfm

h2g2_dist_mat <- dfm_weight(h2g2_dfm, scheme = "prop") %>%
  textstat_dist(method = "euclidean") %>% 
  as.dist()
# hiarchical clustering the distance object
h2g2_cluster <- hclust(h2g2_dist_mat)

h2g2_cluster$labels <- docnames(h2g2_dfm)

plot(h2g2_cluster, xlab = "", sub = "", 
     main = "Euclidean Distance on Normalized Token Frequency")


#quanteda tfidf

h2g2_tfidf <- convert(dfm_tfidf(h2g2_dfm), to = "data.frame")
head(h2g2_tfidf)
h2g2_tfidf <- h2g2_tfidf %>% 
  pivot_longer(
    cols = !doc_id, 
    names_to = "word", 
    values_to = "score"
  )
#sort tibble
h2g2_tfidf <- h2g2_tfidf %>% arrange(desc(score))

#plotting tfidf
h2g2_tfidf %>% 
  group_by(doc_id) %>% 
  top_n(15) %>%
  ungroup() %>%
  facet_bar(y = word, 
            x = score, 
            by = doc_id, 
            nrow = 3)

#Latent Dirichlet Allocation (LDA) 

library(topicmodels)
LDA_fit_20 <- convert(h2g2_dfm, to = "topicmodels") %>% 
  LDA(k = 20)
# get top five terms per topic
get_terms(LDA_fit_20, 5)

LDA_fit_5 <- convert(h2g2_dfm, to = "topicmodels") %>% 
  LDA(k = 5)


#beta represents topic-word density
genre_topics <- tidy(LDA_fit_20, matrix = "beta")

top_terms <- genre_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 5) %>% 
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

#gamma is the proportion of the document that consists of words from the assigned topic

genre_gamma <- tidy(LDA_fit_5, matrix = "gamma")
genre_gamma

genre_gamma %>%
  mutate(genre = reorder(document, gamma * topic)) %>%
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot() +
  facet_wrap(~ genre) +
  labs(x = "topic", y = expression(gamma))

genre_dtm <- convert(h2g2_dfm, to = "tm")

genre_classifications <- genre_gamma %>%
  group_by(document) %>%
  slice_max(gamma) %>%
  ungroup()

genre_topics <- genre_classifications %>%
  count(document, topic) %>%
  group_by(document) %>%
  slice_max(n, n = 1) %>% 
  ungroup() %>%
  transmute(consensus = document, topic)


assignments <- augment(LDA_fit_20, data = genre_dtm)

assignments <- assignments %>%
  inner_join(genre_topics, by = c(".topic" = "topic"))


library(scales)

#visualizing word origin vs assignment

assignments %>%
  count(document, consensus, wt = count) %>%
  mutate(across(c(document, consensus), ~str_wrap(., 20))) %>%
  group_by(document) %>%
  mutate(percent = n / sum(n)) %>%
  ggplot(aes(consensus, document, fill = percent)) +
  geom_tile() +
  scale_fill_gradient2(high = "forestgreen", low = "indianred1", label = percent_format()) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid = element_blank()) +
  labs(x = "Document words were assigned to",
       y = "Document words came from",
       fill = "% of assignments")

#structured topic modelling

library("stm")


dfm_stm <- convert(h2g2_dfm, to = "stm")



model <- stm(documents = dfm_stm$documents,
             vocab = dfm_stm$vocab, 
             K = 15,
             verbose = TRUE)
plot(model)

#cloud viz

stm::cloud(model,
           topic = 4,
           scale = c(2.25, .5))

#perspective viz

plot(model,
     type = "perspectives",
     topics = c(4, 12),
     main = "Topics 4 and 12 comparitively visualized")



