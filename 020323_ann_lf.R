
library("tidyverse")
library("fuzzyjoin")


lit = read.csv("C:/Users/rb122/DA2/books.csv") #goodreads dataset
films = read.csv("C:/Users/rb122/DA2/tmdb_5000_movies.csv") #movies dataset

lf <- stringdist_join(
  lit, films, 
  by           = "title",
  mode         = "left",
  ignore_case  = TRUE, 
  method       = "jw", 
  max_dist     = 0.2, 
  distance_col = "dist") %>%
  group_by(title.x) %>%
  top_n(1, -dist)
#classify budget into buckets 
lf <- lf %>% 
  mutate(budget_bucket = if_else(budget > 85*(10^6),"High", if_else(budget > 35*(10^6), "Mid", "Low")))
#classify runtime into buckets
lf <- lf %>% 
  mutate(runtime_bucket = if_else(runtime > 150,"Long", if_else(budget > 60, "Average", "Short")))

#optimize data type
lf$average_rating <- as.numeric(lf$average_rating)
lf$budget_bucket <-  as.factor(lf$budget_bucket)
lf$runtime_bucket <- as.factor(lf$runtime_bucket)
lf$release_date <- as.Date(lf$release_date)
lf$text_reviews_count <- as.numeric(lf$text_reviews_count)
lf$num_pages <-  as.numeric(lf$X..num_pages)


normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

#relocate columns for convenience while normalizing
lf <- lf %>% relocate(vote_average, .after=average_rating)
lf <-lf %>% relocate(vote_count, .after=average_rating)
lf <-lf %>% relocate(revenue, .after=average_rating)
lf <-lf %>% relocate(runtime, .after=average_rating)
lf <-lf %>% relocate(popularity, .after=average_rating)
lf <-lf %>% relocate(text_reviews_count, .after=average_rating)
lf <-lf %>% relocate(num_pages, .after=average_rating)
lf <-lf %>% relocate(budget, .after=average_rating)
lf <-lf %>% relocate(ratings_count, .after=average_rating)

#verify normalization
summary(lf_norm$vote_average)


lf_norm <- as.data.frame(lapply(lf[4:13], normalize))
lf_train <- lf_norm[1:(round(nrow(lf)*0.75)),]
lf_test <- lf_norm[((round(nrow(lf)*0.75))+1):nrow(lf),]

#install.packages("neuralnet")
library("neuralnet")

softplus <- function(x) { log(1 + exp(x)) }
set.seed(4242)

m <- neuralnet(vote_average ~ average_rating + ratings_count + budget + num_pages + text_reviews_count + popularity + runtime + vote_count, data = lf_train, hidden = c(4,4), threshold = 0.04, act.fct = softplus, stepmax = 1e+06)
#revised stepmax and threshold functions due to non-convergence when using defaults
plot(m)
model_results <- compute(m, lf_test)
predicted_voteavg <- model_results$net.result
cor(predicted_voteavg, lf_test$vote_average) #0.7804659
