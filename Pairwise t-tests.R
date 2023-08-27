install.packages("nnet")  # Install the nnet package
library(nnet)             # Load the nnet package
install.packages("readxl")  # Install the readxl package
library(readxl)             # Load the readxl package
install.packages("tidyverse")  # Install the tidyverse package
library(tidyverse)             # Load the tidyverse package
install.packages("magrittr")  # Install the magrittr package
library(magrittr)             # Load the magrittr package
install.packages("dplyr")  # Install the magrittr package
library(dplyr)  
install.packages("tidyr")  # Install the magrittr package
library(tidyr)  
install.packages("nnet")  # Install the nnet package
library(nnet)             # Load the nnet package
library(stargazer)

data <- read_excel("/Users/Masha/Desktop/survey_15_june.xlsx")

#Converting countries to binary
data$country_live_Ukraine <- ifelse(data$country_live == "Ukraine", 1, 0)
data$country_origin_Ukraine <- ifelse(data$country_origin == "Ukraine", 1, 0)

###**CORRELATION ANALYSIS**###

# a) Kherson
# Calculate the average donations to rebuilding, medical, and humanitarian aid causes per subject
kherson_data <- data[data$treatment == "Kherson Liberation", ]
kherson_data$avg_donations_3 <- rowMeans(kherson_data[, c("rebuilding","humanitarian")])
print(kherson_data$avg_donations_3)

kherson_data$avg_donations<- kherson_data$rebuilding + kherson_data$humanitarian
View(kherson_data)
correlation_kherson <- cor(kherson_data$joy, kherson_data$avg_donations)
print(correlation_kherson)

kherson_data$avg_donations_1<- (kherson_data$rebuilding + kherson_data$humanitarian)/2
correlation_kherson <- cor(kherson_data$joy, kherson_data$avg_donations_1)
print(correlation_kherson)
correlation_kherson <- cor(kherson_data$joy, kherson_data$avg_donations_3)

# Calculate the correlation coefficient between "joy" and average donations
correlation_kherson <- cor(kherson_data$joy, kherson_data$avg_donations)

print(correlation_kherson)
cor.test(kherson_data$joy, kherson_data$avg_donations)$p.value


# b) Shelling: fear and anger
#Fear
# Calculate the average donations to defense and demeaning cause per subject
shelling_data <- data[data$treatment == "Ukraine Shelling", ]
shelling_data$avg_donations <- rowMeans(shelling_data[, c("defence")])

# Calculate the correlation coefficient between "fear" and average donations
correlation_shelling <- cor(shelling_data$fear, shelling_data$avg_donations)
print(correlation_shelling)
cor.test(shelling_data$fear, shelling_data$avg_donations)$p.value

#Anger
correlation_shelling_ang <- cor(shelling_data$anger, shelling_data$avg_donations)
print(correlation_shelling_ang)
cor.test(shelling_data$anger, shelling_data$avg_donations)$p.value


# c) Release POWs: relief
# Calculate the average donations to rebuilding, medical, and humanitarian aid causes per subject
POWs_data <- data[data$treatment == "Release of POWs", ]
POWs_data$avg_donations <- rowMeans(POWs_data[, c("rebuilding", "humanitarian")])

# Calculate the correlation coefficient between "relief" and average donations
correlation_POWs <- cor(POWs_data$relief, POWs_data$avg_donations)
print (correlation_POWs)
cor.test(POWs_data$relief, POWs_data$avg_donations)$p.value

# d) Mariupol sadness, disgust
# Sadness
mariupol_data <- data[data$treatment == "Drama Theater", ]

# Calculate the correlation coefficient between the average of sadness & disgust and average donations
correlation_mariupol <- cor(mariupol_data$sadness, mariupol_data$defence)
print(correlation_mariupol)
cor.test(mariupol_data$sadness, mariupol_data$defence)$p.value

#Disgust
correlation_mariupol_dis <- cor(mariupol_data$disgust, mariupol_data$defence)
print(correlation_mariupol_dis)
cor.test(mariupol_data$disgust, mariupol_data$defence)$p.value


###**PAIRWISE T-TEST***###

# Function to apply Bonferroni correction
apply_bonferroni_correction <- function(p_value, num_tests) {
  adjusted_p_value <- p_value * num_tests
  return(pmin(adjusted_p_value, 1)) # Cap the adjusted p-value at 1
}

num_tests_per_treatment <- 10

#KHERSON LIBERATION


#Checking if target emotion 'joy' is higher in Kherson liberation than other emotions
kherson_data <- data[data$treatment == "Kherson Liberation", ]

#WITH BONFERRONI
# Perform t-tests for each emotion comparing 'joy' with the other emotions
emotions <- c("anger", "fear", "sadness", "disgust", "relief")
t_test_results <- lapply(emotions, function(emotion) {
  t_test <- t.test(kherson_data$joy, kherson_data[[emotion]])
  t_test$p.value <- apply_bonferroni_correction(t_test$p.value, num_tests_per_treatment)
  return(t_test)
})

for (i in seq_along(emotions)) {
  emotion <- emotions[i]
  t_test <- t_test_results[[i]]
  cat("Comparison for", emotion, "vs. joy in the Kherson liberation treatment:\n")
  cat("t-value:", t_test$statistic, "\n")
  cat("p-value (Bonferroni adjusted):", t_test$p.value, "\n")
  cat("------------------------------\n")
}

#WITHOUT BONFERRONI
emotions <- c("anger", "fear", "sadness", "disgust", "relief")
t_test_results <- lapply(emotions, function(emotion) {
  t_test <- t.test(kherson_data$joy, kherson_data[[emotion]])
  return(t_test)
})

# Extract and print the results
for (i in seq_along(emotions)) {
  emotion <- emotions[i]
  t_test <- t_test_results[[i]]
  cat("Comparison for", emotion, "vs. joy in the Kherson liberation treatment:\n")
  cat("t-value:", t_test$statistic, "\n")
  cat("p-value:", t_test$p.value, "\n")
  cat("------------------------------\n")
}


#RELEASE OF POWS
#Checking if target emotion 'relief' is higher in Release of POWs than other emotions
POWs_data <- data[data$treatment == "Release of POWs", ]

# Perform t-tests for each emotion comparing 'joy' with the other emotions
emotions <- c("anger", "fear", "sadness", "disgust", "joy")
t_test_results <- lapply(emotions, function(emotion) {
  t_test <- t.test(POWs_data$relief, POWs_data[[emotion]])
  t_test$p.value <- apply_bonferroni_correction(t_test$p.value, num_tests_per_treatment)
  return(t_test)
})

# Extract and print the results
for (i in seq_along(emotions)) {
  emotion <- emotions[i]
  t_test <- t_test_results[[i]]
  cat("Comparison for", emotion, "vs. relief in the Release of POWs treatment:\n")
  cat("t-value:", t_test$statistic, "\n")
  cat("p-value (Bonferroni adjusted):", t_test$p.value, "\n")
  cat("------------------------------\n")
}


#Checking if non-target emotion 'joy' is higher in Release of POWs than other emotions
POWs_data <- data[data$treatment == "Release of POWs", ]

# Perform t-tests for each emotion comparing 'joy' with the other emotions
emotions <- c("anger", "fear", "sadness", "disgust", "relief")
t_test_results <- lapply(emotions, function(emotion) {
  t_test <- t.test(POWs_data$joy, POWs_data[[emotion]])
  return(t_test)
})

# Extract and print the results
for (i in seq_along(emotions)) {
  emotion <- emotions[i]
  t_test <- t_test_results[[i]]
  cat("Comparison for", emotion, "vs. joy in the Release of POWs treatment:\n")
  cat("t-value:", t_test$statistic, "\n")
  cat("p-value:", t_test$p.value, "\n")
  cat("------------------------------\n")
}

###DRAMA THEATER
#Checking if target emotion 'sadness' is higher in Drama Theater than other emotions
Theater_data <- data[data$treatment == "Drama Theater", ]

# Perform t-tests for each emotion comparing 'joy' with the other emotions
emotions <- c("anger", "fear", "relief", "disgust", "joy")
t_test_results <- lapply(emotions, function(emotion) {
  t_test <- t.test(Theater_data$sadness, Theater_data[[emotion]])
  t_test$p.value <- apply_bonferroni_correction(t_test$p.value, num_tests_per_treatment)
  return(t_test)
})

# Extract and print the results
for (i in seq_along(emotions)) {
  emotion <- emotions[i]
  t_test <- t_test_results[[i]]
  cat("Comparison for", emotion, "vs. sadness in the Drama Theater treatment:\n")
  cat("t-value:", t_test$statistic, "\n")
  cat("p-value (Bonferroni adjusted):", t_test$p.value, "\n")
  cat("------------------------------\n")
}

#Checking if target emotion 'disgust' is higher in Drama Theater than other emotions

# Perform t-tests for each emotion comparing 'joy' with the other emotions
emotions <- c("anger", "fear", "relief", "sadness", "joy")
t_test_results <- lapply(emotions, function(emotion) {
  t_test <- t.test(Theater_data$disgust, Theater_data[[emotion]])
  t_test$p.value <- apply_bonferroni_correction(t_test$p.value, num_tests_per_treatment)
  return(t_test)
})

# Extract and print the results
for (i in seq_along(emotions)) {
  emotion <- emotions[i]
  t_test <- t_test_results[[i]]
  cat("Comparison for", emotion, "vs. disgust in the Drama Theater treatment:\n")
  cat("t-value:", t_test$statistic, "\n")
  cat("p-value (Bonferroni adjusted):", t_test$p.value, "\n")
  cat("------------------------------\n")
}

#Checking if target emotion 'anger' is higher in Ukraine Shelling than other emotions
Shelling_data <- data[data$treatment == "Ukraine Shelling", ]

# Perform t-tests for each emotion comparing 'joy' with the other emotions
emotions <- c("sadness", "fear", "relief", "disgust", "joy")
t_test_results <- lapply(emotions, function(emotion) {
  t_test <- t.test(Shelling_data$anger, Shelling_data[[emotion]])
  t_test$p.value <- apply_bonferroni_correction(t_test$p.value, num_tests_per_treatment)
  return(t_test)
})

# Extract and print the results
for (i in seq_along(emotions)) {
  emotion <- emotions[i]
  t_test <- t_test_results[[i]]
  cat("Comparison for", emotion, "vs. anger in the Ukraine Shelling treatment:\n")
  cat("t-value:", t_test$statistic, "\n")
  cat("p-value (Bonferroni adjusted):", t_test$p.value, "\n")
  cat("------------------------------\n")
}


#Checking if target emotion 'fear' is higher in Ukraine Shelling than other emotions

# Perform t-tests for each emotion comparing 'joy' with the other emotions
emotions <- c("sadness", "anger", "relief", "disgust", "joy")
t_test_results <- lapply(emotions, function(emotion) {
  t_test <- t.test(Shelling_data$fear, Shelling_data[[emotion]])
  t_test$p.value <- apply_bonferroni_correction(t_test$p.value, num_tests_per_treatment)
  return(t_test)
})

# Extract and print the results
for (i in seq_along(emotions)) {
  emotion <- emotions[i]
  t_test <- t_test_results[[i]]
  cat("Comparison for", emotion, "vs. fear in the Ukraine Shelling treatment:\n")
  cat("t-value:", t_test$statistic, "\n")
  cat("p-value (Bonferroni adjusted):", t_test$p.value, "\n")
  cat("------------------------------\n")
}


#Checking if non-target emotion 'sadness' is higher in Ukraine Shelling than other emotions
Shelling_data <- data[data$treatment == "Ukraine Shelling", ]

# Perform t-tests for each emotion comparing 'joy' with the other emotions
emotions <- c("fear", "anger", "relief", "disgust", "joy")
t_test_results <- lapply(emotions, function(emotion) {
  t_test <- t.test(Shelling_data$sadness, Shelling_data[[emotion]])
  return(t_test)
})

# Extract and print the results
for (i in seq_along(emotions)) {
  emotion <- emotions[i]
  t_test <- t_test_results[[i]]
  cat("Comparison for", emotion, "vs. sadness in the Ukraine Shelling treatment:\n")
  cat("t-value:", t_test$statistic, "\n")
  cat("p-value:", t_test$p.value, "\n")
  cat("------------------------------\n")
}

#Checking if non-target emotion 'disgust' is higher in Ukraine Shelling than other emotions
Shelling_data <- data[data$treatment == "Ukraine Shelling", ]

# Perform t-tests for each emotion comparing 'joy' with the other emotions
emotions <- c("fear", "anger", "relief", "sadness", "joy")
t_test_results <- lapply(emotions, function(emotion) {
  t_test <- t.test(Shelling_data$disgust, Shelling_data[[emotion]])
  return(t_test)
})

# Extract and print the results
for (i in seq_along(emotions)) {
  emotion <- emotions[i]
  t_test <- t_test_results[[i]]
  cat("Comparison for", emotion, "vs. disgust in the Ukraine Shelling treatment:\n")
  cat("t-value:", t_test$statistic, "\n")
  cat("p-value:", t_test$p.value, "\n")
  cat("------------------------------\n")
}