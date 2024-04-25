## Project:  STA 215, Spring 2024, Final Project
# Located:   Posit Cloud
# File Name: lorde songs
# Date:      2024_2_29
# Who:       Kevin Janas



## Load packages
# NOTE: Run base.R if these commands return an error!
library(readr)
library(dplyr)
library(tidytext)
library(tidyverse)
library(ggplot2)
library(haven)
library(forcats)
library(psych)

# Load data 
data <- read_delim("raw_data - Sheet 1.csv")



##################################################################################
############### Table 1: descriptive statistics    ####################   
##################################################################################
# mean of Lorde's age at song release
mean(data$release_age)
sd(data$release_age)
hist(data$release_age)
summary(data$release_age)
min(data$release_age)
max(data$release_age)

# Length of the song
mean(data$song_length)
sd(data$song_length)
hist(data$song_length)
summary(data$song_length)
min(data$song_length)
max(data$song_length)


# Streams per song
mean(data$streams)
sd(data$streams)
hist(data$streams)
summary(data$streams)
min(data$streams)
max(data$streams)


# Statistic on release name
table(data$release_name)

# Statistic on emotion felt during the song
table(data$emotion_me)

# Is the song a single?
table(data$single)

# Does the song have a music video?
table(data$music_video)


##################################################################################
#################### Figure 1: boxplot             ####################   
##################################################################################
# BOX PLOT
ggplot(data, aes(x = emotion_me, y = song_length)) +
  geom_boxplot() +
  labs(title = "Box Plot of Song Length and Emotion",
       x = "Emotion",
       y = "Song Length") +
  theme_minimal()

ggplot(data, aes(x = emotion_me, y = release_age)) +
  geom_boxplot() +
  labs(title = "Box Plot of Release Age and Emotion",
       x = "Emotion",
       y = "Release Age") +
  theme_minimal()


##################################################################################
####################   Figure 2: scatter plot             ####################   
##################################################################################
linear_plot <- plot(data$streams, data$release_age)
print(linear_plot)

# add x line and y line for means
meany <- mean(data$streams)
meanx <- mean(data$release_age)

abline(h = meanx, col = "black")
abline(v = meany, col = "black")

linear_relationship <- lm(data$release_age ~ data$streams, data = data)
summary(linear_relationship)

# Add the linear regression line to the scatter plot
# NOTE: double check the scatter plot is currently in your utilities window!
abline(linear_relationship, col = "red")
##################################################################################
####################  Figure 3: residual plot                ####################   
##################################################################################
# Plot the residuals
plot(data$streams, residuals(linear_relationship))

# Add a horizontal line at zero to indicate the baseline
abline(h = 0, col = "red")

##################################################################################
####################  Table 2: contingency table                ####################   
##################################################################################
table(data$release_name, data$emotion_me)
chisq.test(data$release_name, data$emotion_me)
