#Installing packages
#####
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
#####
data <- read_excel("/Users/Masha/Desktop/survey_15_june.xlsx")

#Summary of the dataset
#####
summary_data <- summary(data)
print(summary_data)

gender_table <- table(data$gender)
print(gender_table)
gender_percentages <- prop.table(gender_table)
print(gender_percentages)

hear_table <- table (data$hear_before)
print(hear_table)
hear_per <- prop.table(hear_table)
print(hear_per)

donate_table <- table(data$donate_before)
print(donate_table)
don_per <- prop.table(donate_table)
print(don_per)

#Kherson
kherson_data <- data[data$treatment == 'Kherson Liberation', ]
#39 obs
summary_kherson <- summary(kherson_data)
print(summary_kherson)


gender_table <- table(kherson_data$gender)
print(gender_table)
gender_percentages <- prop.table(gender_table)
print(gender_percentages)

hear_table <- table (kherson_data$hear_before)
print(hear_table)
hear_per <- prop.table(hear_table)
print(hear_per)

donate_table <- table(kherson_data$donate_before)
print(donate_table)
don_per <- prop.table(donate_table)
print(don_per)

#Drama Theater
drama_data <- data[data$treatment == 'Drama Theater', ]
#39 obs
summary_drama <- summary(drama_data)
print(summary_drama)

gender_table <- table(drama_data$gender)
print(gender_table)
gender_percentages <- prop.table(gender_table)
print(gender_percentages)

hear_table <- table (drama_data$hear_before)
print(hear_table)
hear_per <- prop.table(hear_table)
print(hear_per)

donate_table <- table(drama_data$donate_before)
print(donate_table)
don_per <- prop.table(donate_table)
print(don_per)

#Ukraine Shelling
shelling_data <- data[data$treatment == 'Ukraine Shelling', ]
#46 obs
summary_shelling <- summary(shelling_data)
print(summary_shelling)

gender_table <- table(shelling_data$gender)
print(gender_table)
gender_percentages <- prop.table(gender_table)
print(gender_percentages)

hear_table <- table (shelling_data$hear_before)
print(hear_table)
hear_per <- prop.table(hear_table)
print(hear_per)

donate_table <- table(shelling_data$donate_before)
print(donate_table)
don_per <- prop.table(donate_table)
print(don_per)

#Release of POWs

pows_data <- data[data$treatment == 'Release of POWs', ]

summary_pows <-  summary(pows_data)
print(summary_pows)

gender_table <- table(pows_data$gender)
print(gender_table)
gender_percentages <- prop.table(gender_table)
print(gender_percentages)

hear_table <- table (pows_data$hear_before)
print(hear_table)
hear_per <- prop.table(hear_table)
print(hear_per)

donate_table <- table(pows_data$donate_before)
print(donate_table)
don_per <- prop.table(donate_table)
print(don_per)
#####

#Converting countries to binary
data$country_live_Ukraine <- ifelse(data$country_live == "Ukraine", 1, 0)
data$country_origin_Ukraine <- ifelse(data$country_origin == "Ukraine", 1, 0)

# Doing ANOVA to see if there is effect of treatment on induced emotions
anova_result <- aov(anger + fear + joy + sadness + disgust + relief ~ treatment, data = data)
summary(anova_result)

#OLD CODE
#####
data_long <- data %>%
  pivot_longer(cols = c(keep, defence, humanitarian, rebuilding),
               names_to = "category",
               values_to = "dependent_variable") %>%
  select(response_Id, anger, fear, joy, sadness, disgust, relief, category, dependent_variable)

data_3 <- data_long %>% 
  group_by(response_Id)

str(data2)      # Display the structure of the data
summary(data2)  # Provide summary statistics of the data

model2 <- multinom(cbind(keep, defence, humanitarian, rebuilding) ~ anger + fear + joy + sadness + disgust + relief + country_live_Ukraine + country_origin_Ukraine + age + gender, data = data2)
summary(model2)
library(stargazer)
stargazer(model2, type="text")

coef <- coef(model2)
p_values <- summary(model2)$coefficients
odds_ratios <- exp(coef)
#####

#OLS Regressions:
causes <- c("defence", "humanitarian", "rebuilding", "keep")

# Iterate over each cause and fit the OLS regression model
for (cause in causes) {
  # Subset the data for the current cause
  cause_data <- data[data$cause_donated_to == cause, ]
  
  # Perform OLS regression
  model <- lm(cause_donated ~ anger + fear + joy + sadness + disgust + relief, data = cause_data)
  
  # Print the results
  print(paste("Regression results for", cause))
  summary(model)
}

# OLS regression for emotions influence - HAVE DONE THIS ON PICTURE
#####
model_keep <- lm(keep ~ joy + relief + anger + fear + sadness + disgust + age + gender + country_origin_Ukraine, data = data)
summary(model_keep)

model_hum <- lm(humanitarian ~ joy + relief + anger + fear + sadness + disgust + age + gender + country_origin_Ukraine, data = data)
summary(model_hum)

model_def <- lm(defence ~ joy + relief + anger + fear + sadness + disgust + age + gender + country_origin_Ukraine, data = data)
summary(model_def)

model_reb <- lm(rebuilding ~ joy + relief + anger + fear + sadness + disgust + age + gender + country_origin_Ukraine, data = data)
summary(model_reb)

#without including country_origin, Because it could correlate with emotions.. SHOULD ALSO DO ON PICTURE...
model_keep <- lm(keep ~ joy + relief + anger + fear + sadness + disgust + age + gender, data = data)
summary(model_keep)

model_hum <- lm(humanitarian ~ joy + relief + anger + fear + sadness + disgust + age + gender, data = data)
summary(model_hum)

model_def <- lm(defence ~ joy + relief + anger + fear + sadness + disgust + age + gender, data = data)
summary(model_def)

model_reb <- lm(rebuilding ~ joy + relief + anger + fear + sadness + disgust + age + gender, data = data)
summary(model_reb)

#with don before
model_keep_don <- lm(keep ~ joy + relief + anger + fear + sadness + disgust + age + gender + country_origin_Ukraine + donate_before, data = data)
summary(model_keep_don)

model_hum_don <- lm(humanitarian ~ joy + relief + anger + fear + sadness + disgust + age + gender + country_origin_Ukraine + donate_before, data = data)
summary(model_hum_don)

model_def_don <- lm(defence ~ joy + relief + anger + fear + sadness + disgust + age + gender + country_origin_Ukraine + donate_before, data = data)
summary(model_def_don)

model_reb_don <- lm(rebuilding ~ joy + relief + anger + fear + sadness + disgust + age + gender + country_origin_Ukraine + donate_before, data = data)
summary(model_reb_don)

#with treatment
model_keep_tr <- lm(keep ~ treatment + joy + relief + anger + fear + sadness + disgust + age + gender + country_origin_Ukraine, data = data)
summary(model_keep_tr)

model_hum_tr <- lm(humanitarian ~ treatment + joy + relief + anger + fear + sadness + disgust + age + gender + country_origin_Ukraine, data = data)
summary(model_hum_tr)

model_def_tr <- lm(defence ~ treatment + joy + relief + anger + fear + sadness + disgust + age + gender + country_origin_Ukraine, data = data)
summary(model_def_tr)

model_reb_tr <- lm(rebuilding ~ treatment + joy + relief + anger + fear + sadness + disgust + age + gender + country_origin_Ukraine, data = data)
summary(model_reb_tr)
#####

#DOING SEPARATE OLS REGRESSIONS FOR UKRAINIAN AND NON-UKRAINIAN SUBSAMPLES
#####
#ONLY UKR
data_ukr <- data[data$country_origin_Ukraine == 1, ]
#104 observations of 27 variables

model_keep_ukr <- lm(keep ~ treatment + age + gender, data = data_ukr)
summary(model_keep_ukr)

model_hum_ukr <- lm(humanitarian ~ treatment + age + gender, data = data_ukr)
summary(model_hum_ukr)

model_def_ukr <- lm(defence ~ treatment + age + gender, data = data_ukr)
summary(model_def_ukr)

model_reb_ukr <- lm(rebuilding ~ treatment + age + gender, data = data_ukr)
summary(model_reb_ukr)

#ONLY NON-UKR
data_non_ukr <- data[data$country_origin_Ukraine == 0, ]
#62 observations of 27 var
model_keep_non_ukr <- lm(keep ~ treatment + age + gender, data = data_non_ukr)
summary(model_keep_non_ukr)

#Kherson lib decrease hum
model_hum_non_ukr <- lm(humanitarian ~ treatment + age + gender, data = data_non_ukr)
summary(model_hum_non_ukr)

model_def_non_ukr <- lm(defence ~ treatment + age + gender, data = data_non_ukr)
summary(model_def_non_ukr)

#Kherson lib decrease reb, Ukraine shelling increase reb, male decrease reb
model_reb_non_ukr <- lm(rebuilding ~ treatment + age + gender, data = data_non_ukr)
summary(model_reb_non_ukr)
#####


#FOR FEMALE MALE DIF
#####

#fem only
data_female <- data[data$gender == "female", ]
#83 obs 27 var

model_keep_female <- lm(keep ~ treatment + age + country_origin_Ukraine, data = data_female)
summary(model_keep_female)

model_hum_female <- lm(humanitarian ~ treatment + age + country_origin_Ukraine, data = data_female)
summary(model_hum_female)

model_def_female <- lm(defence ~ treatment + age + country_origin_Ukraine, data = data_female)
summary(model_def_female)

model_reb_female <- lm(rebuilding ~ treatment + age + country_origin_Ukraine, data = data_female)
summary(model_reb_female)

#male only 
data_male <- data[data$gender == "male", ]
#80 obs
model_keep_male <- lm(keep ~ treatment + age + country_origin_Ukraine, data = data_male)
summary(model_keep_male)

model_hum_male <- lm(humanitarian ~ treatment + age + country_origin_Ukraine, data = data_male)
summary(model_hum_male)

model_def_male <- lm(defence ~ treatment + age + country_origin_Ukraine, data = data_male)
summary(model_def_male)

model_reb_male <- lm(rebuilding ~ treatment + age + country_origin_Ukraine, data = data_male)
summary(model_reb_male)

#####

# OLS regression for treatment influence
#####
#Ref category here was Drama Theater - THE ONE THAT IS ON THE PICTURE

data$treatment <- factor(data$treatment)
data$treatment <- relevel(data$treatment, ref = "Drama Theater")

model_keep_2 <- lm(keep ~ treatment + age + gender + country_origin_Ukraine, data = data)
summary(model_keep_2)

model_hum_2 <- lm(humanitarian ~ treatment + age + gender + country_origin_Ukraine, data = data)
summary(model_hum_2)

model_def_2 <- lm(defence ~ treatment + age + gender + country_origin_Ukraine, data = data)
summary(model_def_2)

model_reb_2 <- lm(rebuilding ~ treatment + age + gender + country_origin_Ukraine, data = data)
summary(model_reb_2)

#NOW THE SAME BUT DONT INCLUDE COUNTRY_ORIGIN. THIS IS BAD IDEA CAUSE MOST 
#UKRAINIANS WERE OLDER SO IT SHOWS THAT AGE DECREASES DONATIONS TO HUMANITARIAN

model_keep_2 <- lm(keep ~ treatment + age + gender, data = data)
summary(model_keep_2)

model_hum_2 <- lm(humanitarian ~ treatment + age + gender, data = data)
summary(model_hum_2)

model_def_2 <- lm(defence ~ treatment + age + gender, data = data)
summary(model_def_2)

model_reb_2 <- lm(rebuilding ~ treatment + age + gender, data = data)
summary(model_reb_2)

# other options to experiment: +hear_before
model_keep_7 <- lm(keep ~ treatment + age + gender + country_origin_Ukraine + hear_before, data = data)
summary(model_keep_7)

model_hum_7 <- lm(humanitarian ~ treatment + age + gender + country_origin_Ukraine + hear_before, data = data)
summary(model_hum_7)

model_def_7 <- lm(defence ~ treatment + age + gender + country_origin_Ukraine + hear_before, data = data)
summary(model_def_7)

model_reb_7 <- lm(rebuilding ~ treatment + age + gender + country_origin_Ukraine + hear_before, data = data)
summary(model_reb_7)

#+ donate_before instead of hear_before
data$donate_before[is.na(data$donate_before)] <- "No"

model_keep_8 <- lm(keep ~ treatment + age + gender + country_origin_Ukraine + donate_before, data = data)
summary(model_keep_8)

model_hum_8 <- lm(humanitarian ~ treatment + age + gender + country_origin_Ukraine + donate_before, data = data)
summary(model_hum_8)

model_def_8 <- lm(defence ~ treatment + age + gender + country_origin_Ukraine + donate_before, data = data)
summary(model_def_8)

model_reb_8 <- lm(rebuilding ~ treatment + age + gender + country_origin_Ukraine + donate_before, data = data)
summary(model_reb_8)
#####

#emotion as dependent variable on treatment  DO THIS ON PICTURE!!!!
#####
model_joy <- lm(joy ~ treatment + age + gender + country_origin_Ukraine, data = data)
summary(model_joy)

model_relief <- lm(relief ~ treatment + age + gender + country_origin_Ukraine, data = data)
summary(model_relief)

model_anger <- lm(anger ~ treatment + age + gender + country_origin_Ukraine, data = data)
summary(model_anger)

model_fear <- lm(fear ~ treatment + age + gender + country_origin_Ukraine, data = data)
summary(model_fear)

model_digust <- lm(disgust ~ treatment + age + gender + country_origin_Ukraine, data = data)
summary(model_digust)

model_sadness <- lm(sadness ~ treatment + age + gender + country_origin_Ukraine, data = data)
summary(model_sadness) 
#####
#with donations cause, treatment as influence and emotion as dependent
model_joy_2 <- lm(joy ~ treatment + age + gender + country_origin_Ukraine + keep + defence + humanitarian + rebuilding, data = data)
summary(model_joy_2)

#And treatment as dependent variable on emotion to see if induction happened as planned

#Changing ref categories
######
#Changing ref category to Ukraine Shelling
data$treatment <- factor(data$treatment)
data$treatment <- relevel(data$treatment, ref = "Ukraine Shelling")

model_keep_3 <- lm(keep ~ treatment + age + gender + country_origin_Ukraine, data = data)
summary(model_keep_3)

model_hum_3 <- lm(humanitarian ~ treatment + age + gender + country_origin_Ukraine, data = data)
summary(model_hum_3)

model_def_3 <- lm(defence ~ treatment + age + gender + country_origin_Ukraine, data = data)
summary(model_def_3)

model_reb_3 <- lm(rebuilding ~ treatment + age + gender + country_origin_Ukraine, data = data)
summary(model_reb_3)

#Changing ref category to Release of POWs
data$treatment <- factor(data$treatment)
data$treatment <- relevel(data$treatment, ref = "Release of POWs")

model_keep_4 <- lm(keep ~ treatment + age + gender + country_origin_Ukraine, data = data)
summary(model_keep_4)

model_hum_4 <- lm(humanitarian ~ treatment + age + gender + country_origin_Ukraine, data = data)
summary(model_hum_4)

model_def_4 <- lm(defence ~ treatment + age + gender + country_origin_Ukraine, data = data)
summary(model_def_4)

model_reb_4 <- lm(rebuilding ~ treatment + age + gender + country_origin_Ukraine, data = data)
summary(model_reb_4)

#Changing ref category to Kherson Liberation
data$treatment <- factor(data$treatment)
data$treatment <- relevel(data$treatment, ref = "Kherson Liberation")

model_keep_5 <- lm(keep ~ treatment + age + gender + country_origin_Ukraine, data = data)
summary(model_keep_5)

model_hum_5 <- lm(humanitarian ~ treatment + age + gender + country_origin_Ukraine, data = data)
summary(model_hum_5)

model_def_5 <- lm(defence ~ treatment + age + gender + country_origin_Ukraine, data = data)
summary(model_def_5)

model_reb_5 <- lm(rebuilding ~ treatment + age + gender + country_origin_Ukraine, data = data)
summary(model_reb_5)
#####

  