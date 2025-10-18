######
# MTH 3270
# Project 3
######

# import neccecery libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(tm)
library(stringr)
library(kknn)
library(rpart)
library(rpart.plot)
library(rsample)
library(nnet)
library(yardstick)
library(tibble)
library(mclust)

# read the data in from CSV
dir <- "~/Documents/MSU/MTH_3270/projects/project_03/data/full_music_data.csv"
full_music_data  <- read.csv(dir, header = TRUE, sep = ",")

dir <- "~/Documents/MSU/MTH_3270/projects/project_03/data/influence_data.csv"
influence_data <- read.csv(dir, header = TRUE, sep = ",")

dir <- "~/Documents/MSU/MTH_3270/projects/project_03/data/data_by_artist.csv"
by_artist <- read.csv(dir, header = TRUE, sep = ",")

dir <- "~/Documents/MSU/MTH_3270/projects/project_03/data/data_by_year.csv"
by_year <- read.csv(dir, header = TRUE, sep = ",")

# Join data tables and clean up columns
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
# rename genere col
music <- rename(music, genre = follower_main_genre)
# Uncomment to write out CSV
# write.csv(music, "output.csv", row.names = FALSE)

### T1: Supervised Learning
# Split data into 3/4 for training the model and 1/4 for OOB testing
music_split <- initial_split(music, prop = 3/4)
music_training <- training(music_split)
music_test <- testing(music_split)
# needed to use nnet
music_training$genre <- as.factor(music_training$genre)
music_test$genre <- as.factor(music_test$genre)
# k = 2,5,7 NNs
nn_k2 <- nnet(genre ~ danceability + energy + valence + tempo,
	data = music_training,
	size = 2,
	maxit = 1000, 
	trace = FALSE)
nn_k5 <- nnet(genre ~ danceability + energy + valence + tempo,
	data = music_training,
	size = 5,
	maxit = 1000, 
	trace = FALSE)
nn_k7 <- nnet(genre ~ danceability + energy + valence + tempo,
	data = music_training,
	size = 7,
	maxit = 1000, 
	trace = FALSE)
# make predictors of these models using the test set
pred_k2 = predict(nn_k2, music_test, type = 'class')
pred_k5 = predict(nn_k5, music_test, type = 'class')
pred_k7 = predict(nn_k7, music_test, type = 'class')
# store them in tibble for yard stick use
# have to convert the prediction as a factor as well
results_k2 <- tibble(
  actual = music_test$genre,
  prediction = factor(pred_k2, levels = levels(music_test$genre)))
results_k5 <- tibble(
  actual = music_test$genre,
  prediction = factor(pred_k5, levels = levels(music_test$genre)))
results_k7 <- tibble(
  actual = music_test$genre,
  prediction = factor(pred_k7, levels = levels(music_test$genre)))
# check the accuracy of eatch model
acc_k2 <- accuracy(results_k2, truth = actual, estimate = prediction)
acc_k5 <- accuracy(results_k5, truth = actual, estimate = prediction)
acc_k7 <- accuracy(results_k7, truth = actual, estimate = prediction)
# data frame holding the accuracy of the models
accuracy <- data.frame(
    model = c("NN k=2", "NN k=5", "NN k=7"),
    accuracy = c(acc_k2$.estimate, acc_k5$.estimate, acc_k7$.estimate))
print(accuracy)

# KNN models
knn_k3 <- kknn(genre ~ danceability + energy + valence + tempo,
	train = music_training,
	test = music_test,
	k = 3)
knn_k5 <- kknn(genre ~ danceability + energy + valence + tempo,
	train = music_training,
	test = music_test,
	k = 5)
knn_k7 <- kknn(genre ~ danceability + energy + valence + tempo,
	train = music_training,
	test = music_test,
	k = 7)
# get the predictions 
pred_knn3 <- fitted(knn_k3)
pred_knn5 <- fitted(knn_k5)
pred_knn7 <- fitted(knn_k7)
# make the tibbles to use in the accuracy functon
results_knn3 <- tibble(
  actual = music_test$genre,
  prediction = factor(pred_knn3, levels = levels(music_test$genre)))
results_knn5 <- tibble(
  actual = music_test$genre,
  prediction = factor(pred_knn5, levels = levels(music_test$genre)))
results_knn7 <- tibble(
  actual = music_test$genre,
  prediction = factor(pred_knn7, levels = levels(music_test$genre)))
# compute the accuracy of the models
acc_knn3 <- accuracy(results_knn3, truth = actual, estimate = prediction)
acc_knn5 <- accuracy(results_knn5, truth = actual, estimate = prediction)
acc_knn7 <- accuracy(results_knn7, truth = actual, estimate = prediction)
# and the data frame again
knn_accuracy <- data.frame(
  model = c("KNN k=3", "KNN k=5", "KNN k=7"),
  accuracy = c(acc_knn3$.estimate, acc_knn5$.estimate, acc_knn7$.estimate))
print(knn_accuracy)

### T2 Unsupervised Learning:
# take advice and use only a subset of the rows, half of them
music_half <- music[1:(nrow(music) / 2), ]
# scale music to that (X)s are normalized
music_scaled <- scale(select(music_half, danceability, energy, valence,tempo))
# clustering, nstart to avoid converging error
kmc <- kmeans(music_scaled, centers = 3, nstart = 25)
# add the cluster numbers to back to music 
music_half$cluster <- factor(kmc$cluster)
# see how many songs got assigned to what cluster
summary(music_half$cluster)
#    1     2     3 
#13051 17677 10934 
# try to see what they are clustering too if any
genre_plot <- ggplot(music_half, aes(x = cluster, fill = genre)) +
  geom_bar(position = "fill") +
  labs(title = "Genre Distribution per Cluster",
       y = "Proportion", x = "Cluster") +
  theme_minimal()
mode_plot <-ggplot(music_half, aes(x = cluster, fill = as.factor(mode))) +
  geom_bar(position = "fill") +
  labs(title = "Mode Distribution per Cluster", y = "Proportion")
music_half <- mutate(music_half, decade = as.character(year - year %% 10))
decade_plot <- ggplot(music_half, aes(x = cluster, fill = as.factor(decade))) +
  geom_bar(position = "fill") +
  labs(title = "Decade Distribution per Cluster", y = "Proportion")
 

