#Comparing positive and negative emotions
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

pos_treatments <- c("Kherson Liberation", "Release of POWs")
neg_treatments <- c("Ukraine Shelling", "Drama Theater")

high_treatments <- c("Kherson Liberation", "Ukraine Shelling")
low_treatments <- c("Release of POWs", "Drama Theater")

#create subsets for pos/neg
data_pos_treatments <- data[data$treatment %in% c("Kherson Liberation", "Release of POWs"), ]
#81 observ.
data_neg_treatments <- data[data$treatment %in% c("Ukraine Shelling", "Drama Theater"), ]


#WORKING WAY - By pos/neg 
pos_reb_eur <- data[data$treatment %in% pos_treatments, "rebuilding"]
pos_hum_eur <- data[data$treatment %in% pos_treatments, "humanitarian"]

neg_reb_eur <- data[data$treatment %in% neg_treatments, "rebuilding"]
neg_hum_eur <- data[data$treatment %in% neg_treatments, "humanitarian"]

t_test_result <- t.test((pos_reb_eur + pos_hum_eur), (neg_reb_eur + neg_hum_eur), na.action = na.omit)
print(t_test_result)

pos_def_eur <- data[data$treatment %in% pos_treatments, "defence"]
neg_def_eur <- data[data$treatment %in% neg_treatments, "defence"]

t_test_result <- t.test((pos_def_eur), (neg_def_eur), na.action = na.omit)
print(t_test_result)

#FOR OPTION KEEP
high_keep <- data[data$treatment %in% high_treatments, "keep"]
low_keep <- data[data$treatment %in% low_treatments, "keep"]
t_test_result <- t.test((high_keep), (low_keep), na.action = na.omit)
print(t_test_result)

#doing for positive emotions

pos_joy_scores <- data[data$treatment %in% pos_treatments, "joy"]
pos_relief_scores <- data[data$treatment %in% pos_treatments, "relief"]

neg_joy_scores <- data[data$treatment %in% neg_treatments, "joy"]
neg_relief_scores <- data[data$treatment %in% neg_treatments, "relief"]

t_test_result <- t.test((pos_joy_scores + pos_relief_scores) / 2, (neg_joy_scores + neg_relief_scores) / 2, na.action = na.omit)

print(t_test_result)

###!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#doing for low-arousal emotions:

#high_treatments
high_relief_scores <- data[data$treatment %in% high_treatments, "relief"]
high_sadness_scores <- data[data$treatment %in% high_treatments, "sadness"]
high_disgust_scores <- data[data$treatment %in% high_treatments, "disgust"]

#low_treatments
low_relief_scores <- data[data$treatment %in% low_treatments, "relief"]
low_sadness_scores <- data[data$treatment %in% low_treatments, "sadness"]
low_disgust_scores <- data[data$treatment %in% low_treatments, "disgust"]

t_test_result <- t.test((high_relief_scores + high_sadness_scores + high_disgust_scores)/ 3, (low_relief_scores + low_sadness_scores + low_disgust_scores) / 3, na.action = na.omit)
print(t_test_result)


#doing for high-arousal emotions:

#high_treatments
high_anger_scores <- data[data$treatment %in% high_treatments, "anger"]
high_fear_scores <- data[data$treatment %in% high_treatments, "fear"]
high_joy_scores <- data[data$treatment %in% high_treatments, "joy"]

#low_treatments
low_anger_scores <- data[data$treatment %in% low_treatments, "relief"]
low_fear_scores <- data[data$treatment %in% low_treatments, "sadness"]
low_joy_scores <- data[data$treatment %in% low_treatments, "disgust"]

t_test_result <- t.test((high_anger_scores + high_fear_scores + high_joy_scores)/ 3, (low_anger_scores + low_fear_scores + low_joy_scores) / 3, na.action = na.omit)
print(t_test_result)

#creating new dataset for pos treatment
new_data_pos_treatment <- data %>%
filter(treatment %in% c("Release of POWs", "Kherson Liberation")) %>%
mutate(neg_em = (anger + fear + sadness + disgust) / 4) %>% 
mutate(pos_em = (joy + relief) / 2)

mean_pos_em <- mean(new_data_pos_treatment$pos_em, na.rm = TRUE)
se_pos_em <- sd(new_data_pos_treatment$pos_em, na.rm = TRUE) / sqrt(length(new_data_pos_treatment$pos_em))

mean_neg_em <- mean(new_data_pos_treatment$neg_em, na.rm = TRUE)
se_neg_em <- sd(new_data_pos_treatment$neg_em, na.rm = TRUE) / sqrt(length(new_data_pos_treatment$neg_em))

# Print the mean and standard error
print(paste("Mean of pos_em:", mean_pos_em))
print(paste("Standard Error of pos_em:", se_pos_em))
print(paste("Mean of neg_em:", mean_neg_em))
print(paste("Standard Error of neg_em:", se_neg_em))


#creating new dataset for neg treatment

new_data_neg_treatment <- data %>%
  filter(treatment %in% c("Ukraine Shelling", "Drama Theater")) %>%
  mutate(neg_em = (anger + fear + sadness + disgust) / 4) %>% 
  mutate(pos_em = (joy + relief) / 2)

mean_pos_em_neg_tr <- mean(new_data_neg_treatment$pos_em, na.rm = TRUE)
se_pos_em_neg_tr <- sd(new_data_neg_treatment$pos_em, na.rm = TRUE) / sqrt(length(new_data_neg_treatment$pos_em))

mean_neg_em_neg_tr <- mean(new_data_neg_treatment$neg_em, na.rm = TRUE)
se_neg_em_neg_tr <- sd(new_data_neg_treatment$neg_em, na.rm = TRUE) / sqrt(length(new_data_neg_treatment$neg_em))

# Print the mean and standard error
print(paste("Mean of pos_em:", mean_pos_em_neg_tr))
print(paste("Standard Error of pos_em:", se_pos_em_neg_tr))
print(paste("Mean of neg_em:", mean_neg_em_neg_tr))
print(paste("Standard Error of neg_em:", se_neg_em_neg_tr))

#doing for negative emotions

pos_anger_scores <- data[data$treatment %in% pos_treatments, "anger"]
pos_fear_scores <- data[data$treatment %in% pos_treatments, "fear"]
pos_sadness_scores <- data[data$treatment %in% pos_treatments, "sadness"]
pos_disgust_scores <- data[data$treatment %in% pos_treatments, "disgust"]

neg_anger_scores <- data[data$treatment %in% neg_treatments, "anger"]
neg_fear_scores <- data[data$treatment %in% neg_treatments, "fear"]
neg_sadness_scores <- data[data$treatment %in% neg_treatments, "sadness"]
neg_disgust_scores <- data[data$treatment %in% neg_treatments, "disgust"]

t_test_result <- t.test((pos_anger_scores + pos_fear_scores + 
pos_sadness_scores + pos_disgust_scores) / 4, (neg_anger_scores + neg_fear_scores + 
neg_sadness_scores + neg_disgust_scores) / 4, na.action = na.omit)

print(t_test_result)


# CREATING HISTOGRAM
# Create a vector of average scores for negative emotions under negative treatments
neg_emotion_neg_event <- 4.479412

# Create a vector of average scores for positive emotions under negative treatments
pos_emotion_neg_event <- 0.5823529

# Create a vector of average scores for negative emotions under positive treatments
neg_emotion_pos_event <- 2.314815

# Create a vector of average scores for positive emotions under positive treatments
pos_emotion_pos_event <- 3.4938272

# Combine the four vectors into a single data frame
data_histogram <- data.frame(emotions = c(neg_emotion_neg_event, pos_emotion_neg_event, neg_emotion_pos_event, pos_emotion_pos_event),
                   treatment = c("Negative Treatments", "Negative Treatments", "Positive Treatments", "Positive Treatments"))

# Plot the histogram
hist(data_histogram$emotions, breaks = 4, col = "steelblue", border = "white",
     main = "Average Scores of Negative and Positive Emotions",
     xlab = "Average Emotion Score",
     ylab = "Frequency",
     xlim = c(0, 5))

# Add labels to the histogram bars
text(x = data_histogram$emotions, y = par("usr")[4] * 0.03,
     labels = round(data_histogram$emotions, 2), col = "black")

# Add legend
legend("topright", legend = data$treatment, fill = "steelblue", border = "white")


