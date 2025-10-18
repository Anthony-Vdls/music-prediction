# import neccecery libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(tm)
library(stringr)
library(rpart)
library(rpart.plot)

# read the data in from CSV
dir <- "./data/full_music_data.csv"
full_music_data  <- read.csv(dir, header = TRUE, sep = ",")

dir <- "./data/influence_data.csv"
influence_data <- read.csv(dir, header = TRUE, sep = ",")

dir <- "./data/data_by_artist.csv"
by_artist <- read.csv(dir, header = TRUE, sep = ",")

dir <- "./data/data_by_year.csv"
by_year <- read.csv(dir, header = TRUE, sep = ",")

# Join data tables
full_music_data <- full_music_data |>
	mutate(artists_id = as.numeric(gsub(artists_id,
	pattern = "\\[|\\]",
	replacement = "")))
influence_small <- influence_data |>
	select(follower_id, follower_main_genre) |>
	distinct()
music <- full_music_data %>%
	inner_join(influence_small, by = c("artists_id" = "follower_id"))

# Further tydy the data
# Remove brackets from artists column
music <- music %>%
  mutate(artist_names = str_remove_all(artist_names, '\\[|\\"|\\]'))
# Remove ';' from all the R&B mentions
music <- music %>%
    mutate(follower_main_genre = str_remove(follower_main_genre, ";$"))
# Song title column fix
music <- rename(.data = music, song_title = song_title..censored.)

### T1
# regession analysis predicting song durration
my_reg <- lm(data = music, duration_ms ~ speechiness + instrumentalness + loudness)
summary(my_reg)

### T2
# lofistical regresssion analysis predicting mode
my_logreg <- glm(data = music, mode ~ valence + energy, family = "binomial")
2.71828^(.324)
# 1.382647 valence Coefficient
2.71828^(-0.43763)
# 0.6455648 energy Coefficient

### T3
music <- mutate(music, decade = as.character(year - year %% 10))
# Make a decition tree with the tuning perameter = 3
my_tree <- rpart(decade ~ instrumentalness + acousticness + duration_ms,
                 data = music,
                 method = "class",
                 control = rpart.control(minsplit = 35),
                 cp = 0.02)
# Plot the tree and make it readable
par(xpd = TRUE)
rpart.plot(my_tree,
           tweak = 1.25,
           cex = 0.7)
par(xpd = FALSE)
# get the real song classifications
preds <- predict(my_tree, type = "class")
music <- music %>%
  mutate(predicted_decade = preds)
# find the accuracy of this models in sample predictions
mean(music$predicted_decade == music$decade)
# predict one row of in sample data
song_evaluate <- music[666,]
predict(my_tree, newdata = song_evaluate, type = "class")
