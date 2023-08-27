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


# COMPARING EMOTIONS INDUCTION WITHIN TREATMENTS
#compare if 'joy' Kherson Liberation treatment has statistically significant 
#different score than joy in other treatments

#KHERSON
kherson_joy <- data[data$treatment == "Kherson Liberation", "joy"]
other_joy <- data[data$treatment != "Kherson Liberation", "joy"]
t_test_result <- t.test(kherson_joy, other_joy)
print(t_test_result)

kherson_relief <- data[data$treatment == "Kherson Liberation", "relief"]
other_relief <- data[data$treatment != "Kherson Liberation", "relief"]
t_test_result <- t.test(kherson_relief, other_relief)
print(t_test_result)

#POWs 
pows_joy <- data[data$treatment == "Release of POWs", "joy"]
other_joy <- data[data$treatment != "Release of POWs", "joy"]
t_test_result <- t.test(pows_joy, other_joy)
print(t_test_result)

pows_relief <- data[data$treatment == "Release of POWs", "relief"]
other_relief <- data[data$treatment != "Release of POWs", "relief"]
t_test_result <- t.test(pows_relief, other_relief)
print(t_test_result)

pows_sadness <- data[data$treatment == "Release of POWs", "sadness"]
other_sadness <- data[data$treatment != "Release of POWs", "sadness"]
t_test_result <- t.test(pows_sadness, other_sadness)
print(t_test_result)

pows_fear <- data[data$treatment == "Release of POWs", "fear"]
other_fear <- data[data$treatment != "Release of POWs", "fear"]
t_test_result <- t.test(pows_fear, other_fear)
print(t_test_result)

pows_disgust <- data[data$treatment == "Release of POWs", "disgust"]
other_disgust <- data[data$treatment != "Release of POWs", "disgust"]
t_test_result <- t.test(pows_disgust, other_disgust)
print(t_test_result)

pows_anger <- data[data$treatment == "Release of POWs", "anger"]
other_anger <- data[data$treatment != "Release of POWs", "anger"]
t_test_result <- t.test(pows_anger, other_anger)
print(t_test_result)

# WELCH TWO-SAMPLE T-TEST FOR DONATIONS
#####
#POWs
pows_defence <- data[data$treatment == "Release of POWs", "defence"]
other_defence <- data[data$treatment != "Release of POWs", "defence"]
t_test_result <- t.test(pows_defence, other_defence)
print(t_test_result)

pows_keep <- data[data$treatment == "Release of POWs", "keep"]
other_keep <- data[data$treatment != "Release of POWs", "keep"]
t_test_result <- t.test(pows_keep, other_keep)
print(t_test_result)

pows_humanitarian <- data[data$treatment == "Release of POWs", "humanitarian"]
other_humanitarian <- data[data$treatment != "Release of POWs", "humanitarian"]
t_test_result <- t.test(pows_humanitarian, other_humanitarian)
print(t_test_result)

pows_rebuilding <- data[data$treatment == "Release of POWs", "rebuilding"]
other_rebuilding <- data[data$treatment != "Release of POWs", "rebuilding"]
t_test_result <- t.test(pows_rebuilding, other_rebuilding)
print(t_test_result)

#Drama theater
theater_rebuilding <- data[data$treatment == "Drama Theater", "rebuilding"]
other_rebuilding <- data[data$treatment != "Drama Theater", "rebuilding"]
t_test_result <- t.test(theater_rebuilding, other_rebuilding)
print(t_test_result)

theater_humanitarian <- data[data$treatment == "Drama Theater", "humanitarian"]
other_humanitarian <- data[data$treatment != "Drama Theater", "humanitarian"]
t_test_result <- t.test(theater_humanitarian, other_humanitarian)
print(t_test_result)

theater_keep <- data[data$treatment == "Drama Theater", "keep"]
other_keep <- data[data$treatment != "Drama Theater", "keep"]
t_test_result <- t.test(theater_keep, other_keep)
print(t_test_result)

theater_defence <- data[data$treatment == "Drama Theater", "defence"]
other_defence <- data[data$treatment != "Drama Theater", "defence"]
t_test_result <- t.test(theater_defence, other_defence)
print(t_test_result)

#Ukraine shelling
shelling_defence <- data[data$treatment == "Ukraine Shelling", "defence"]
other_defence <- data[data$treatment != "Ukraine Shelling", "defence"]
t_test_result <- t.test(shelling_defence, other_defence)
print(t_test_result)

shelling_keep <- data[data$treatment == "Ukraine Shelling", "keep"]
other_keep <- data[data$treatment != "Ukraine Shelling", "keep"]
t_test_result <- t.test(shelling_keep, other_keep)
print(t_test_result)

shelling_humanitarian <- data[data$treatment == "Ukraine Shelling", "humanitarian"]
other_humanitarian <- data[data$treatment != "Ukraine Shelling", "humanitarian"]
t_test_result <- t.test(shelling_humanitarian, other_humanitarian)
print(t_test_result)

shelling_rebuilding <- data[data$treatment == "Ukraine Shelling", "rebuilding"]
other_rebuilding <- data[data$treatment != "Ukraine Shelling", "rebuilding"]
t_test_result <- t.test(shelling_rebuilding, other_rebuilding)
print(t_test_result)

#Kherson Liberation

kherson_defence <- data[data$treatment == "Kherson Liberation", "defence"]
other_defence <- data[data$treatment != "Kherson Liberation", "defence"]
t_test_result <- t.test(kherson_defence, other_defence)
print(t_test_result)

kherson_keep <- data[data$treatment == "Kherson Liberation", "keep"]
other_keep <- data[data$treatment != "Kherson Liberation", "keep"]
t_test_result <- t.test(kherson_keep, other_keep)
print(t_test_result)

kherson_humanitarian <- data[data$treatment == "Kherson Liberation", "humanitarian"]
other_humanitarian <- data[data$treatment != "Kherson Liberation", "humanitarian"]
t_test_result <- t.test(kherson_humanitarian, other_humanitarian)
print(t_test_result)

kherson_rebuilding <- data[data$treatment == "Kherson Liberation", "rebuilding"]
other_rebuilding <- data[data$treatment != "Kherson Liberation", "rebuilding"]
t_test_result <- t.test(kherson_rebuilding, other_rebuilding)
print(t_test_result)
#####

##BY TREATMNET FOR DONATIONS WITH GENDER SUBSET
#####
#Kherson Liberation & Female

kherson_defence <- data[data$treatment == "Kherson Liberation" & data$gender == "female", "defence"]
other_defence <- data[data$treatment != "Kherson Liberation" & data$gender == "female", "defence"]
t_test_result <- t.test(kherson_defence, other_defence)
print(t_test_result)

kherson_keep <- data[data$treatment == "Kherson Liberation" & data$gender == "female", "keep"]
other_keep <- data[data$treatment != "Kherson Liberation" & data$gender == "female", "keep"]
t_test_result <- t.test(kherson_keep, other_keep)
print(t_test_result)

kherson_humanitarian <- data[data$treatment == "Kherson Liberation" & data$gender == "female", "humanitarian"]
other_humanitarian <- data[data$treatment != "Kherson Liberation" & data$gender == "female", "humanitarian"]
t_test_result <- t.test(kherson_humanitarian, other_humanitarian)
print(t_test_result)

kherson_rebuilding <- data[data$treatment == "Kherson Liberation" & data$gender == "female", "rebuilding"]
other_rebuilding <- data[data$treatment != "Kherson Liberation" & data$gender == "female", "rebuilding"]
t_test_result <- t.test(kherson_rebuilding, other_rebuilding)
print(t_test_result)

#Kherson Liberation & Male
kherson_defence <- data[data$treatment == "Kherson Liberation" & data$gender == "male", "defence"]
other_defence <- data[data$treatment != "Kherson Liberation" & data$gender == "male", "defence"]
t_test_result <- t.test(kherson_defence, other_defence)
print(t_test_result)

kherson_keep <- data[data$treatment == "Kherson Liberation" & data$gender == "male", "keep"]
other_keep <- data[data$treatment != "Kherson Liberation" & data$gender == "male", "keep"]
t_test_result <- t.test(kherson_keep, other_keep)
print(t_test_result)

kherson_humanitarian <- data[data$treatment == "Kherson Liberation" & data$gender == "male", "humanitarian"]
other_humanitarian <- data[data$treatment != "Kherson Liberation" & data$gender == "male", "humanitarian"]
t_test_result <- t.test(kherson_humanitarian, other_humanitarian)
print(t_test_result)

kherson_rebuilding <- data[data$treatment == "Kherson Liberation" & data$gender == "male", "rebuilding"]
other_rebuilding <- data[data$treatment != "Kherson Liberation" & data$gender == "male", "rebuilding"]
t_test_result <- t.test(kherson_rebuilding, other_rebuilding)
print(t_test_result)

#####

#GENDER COMPARISON BY TREATMENT

#####
#Kherson liberation
female_defence <- data[data$gender == "female" & data$treatment == "Kherson Liberation", "defence"]
male_defence <- data[data$gender == "male" & data$treatment == "Kherson Liberation", "defence"]
t_test_result <- t.test(female_defence, male_defence)
print(t_test_result)

female_humanitarian <- data[data$gender == "female" & data$treatment == "Kherson Liberation", "humanitarian"]
male_humanitarian <- data[data$gender == "male" & data$treatment == "Kherson Liberation", "humanitarian"]
t_test_result <- t.test(female_humanitarian, male_humanitarian)
print(t_test_result)

female_rebuilding <- data[data$gender == "female" & data$treatment == "Kherson Liberation", "rebuilding"]
male_rebuilding <- data[data$gender == "male" & data$treatment == "Kherson Liberation", "rebuilding"]
t_test_result <- t.test(female_rebuilding, male_rebuilding)
print(t_test_result)

female_keep <- data[data$gender == "female" & data$treatment == "Kherson Liberation", "keep"]
male_keep <- data[data$gender == "male" & data$treatment == "Kherson Liberation", "keep"]
t_test_result <- t.test(female_keep, male_keep)
print(t_test_result)

#Ukraine Shelling
female_defence <- data[data$gender == "female" & data$treatment == "Ukraine Shelling", "defence"]
male_defence <- data[data$gender == "male" & data$treatment == "Ukraine Shelling ", "defence"]
t_test_result <- t.test(female_defence, male_defence)
print(t_test_result)

female_humanitarian <- data[data$gender == "female" & data$treatment == "Ukraine Shelling", "humanitarian"]
male_humanitarian <- data[data$gender == "male" & data$treatment == "Ukraine Shelling", "humanitarian"]
t_test_result <- t.test(female_humanitarian, male_humanitarian)
print(t_test_result)

female_rebuilding <- data[data$gender == "female" & data$treatment == "Ukraine Shelling", "rebuilding"]
male_rebuilding <- data[data$gender == "male" & data$treatment == "Ukraine Shelling", "rebuilding"]
t_test_result <- t.test(female_rebuilding, male_rebuilding)
print(t_test_result)

female_keep <- data[data$gender == "female" & data$treatment == "Ukraine Shelling", "keep"]
male_keep <- data[data$gender == "male" & data$treatment == "Ukraine Shelling", "keep"]
t_test_result <- t.test(female_keep, male_keep)
print(t_test_result)

#Drama Theater
female_defence <- data[data$gender == "female" & data$treatment == "Drama Theater", "defence"]
male_defence <- data[data$gender == "male" & data$treatment == "Drama Theater", "defence"]
t_test_result <- t.test(female_defence, male_defence)
print(t_test_result)

female_humanitarian <- data[data$gender == "female" & data$treatment == "Drama Theater", "humanitarian"]
male_humanitarian <- data[data$gender == "male" & data$treatment == "Drama Theater", "humanitarian"]
t_test_result <- t.test(female_humanitarian, male_humanitarian)
print(t_test_result)

female_rebuilding <- data[data$gender == "female" & data$treatment == "Drama Theater", "rebuilding"]
male_rebuilding <- data[data$gender == "male" & data$treatment == "Drama Theater", "rebuilding"]
t_test_result <- t.test(female_rebuilding, male_rebuilding)
print(t_test_result)

female_keep <- data[data$gender == "female" & data$treatment == "Drama Theater", "keep"]
male_keep <- data[data$gender == "male" & data$treatment == "Drama Theater", "keep"]
t_test_result <- t.test(female_keep, male_keep)
print(t_test_result)

#####


# pos vs neg
pos_rebuilding <- data[data$treatment != "Drama Theater" & data$treatment != "Ukraine Shelling", "rebuilding"]
neg_rebuilding <- data[data$treatment != "Kherson Liberation" & data$treatment != "Release of POWs", "rebuilding"]
t_test_result <- t.test(pos_rebuilding, neg_rebuilding)
print(t_test_result)

pos_keep <- data[data$treatment != "Drama Theater" & data$treatment != "Ukraine Shelling", "keep"]
neg_keep <- data[data$treatment != "Kherson Liberation" & data$treatment != "Release of POWs", "keep"]
t_test_result <- t.test(pos_keep, neg_keep)
print(t_test_result)

pos_defence <- data[data$treatment != "Drama Theater" & data$treatment != "Ukraine Shelling", "defence"]
neg_defence <- data[data$treatment != "Kherson Liberation" & data$treatment != "Release of POWs", "defence"]
t_test_result <- t.test(pos_defence, neg_defence)
print(t_test_result)

pos_humanitarian <- data[data$treatment != "Drama Theater" & data$treatment != "Ukraine Shelling", "humanitarian"]
neg_humanitarian <- data[data$treatment != "Kherson Liberation" & data$treatment != "Release of POWs", "humanitarian"]
t_test_result <- t.test(pos_humanitarian, neg_humanitarian)
print(t_test_result)


#pers vs non_pers
pers_rebuilding <- data[data$treatment != "Kherson Liberation" & data$treatment != "Ukraine Shelling", "rebuilding"]
non_pers_rebuilding <- data[data$treatment != "Drama Theater" & data$treatment != "Release of POWs", "rebuilding"]
t_test_result <- t.test(pers_rebuilding, non_pers_rebuilding)
print(t_test_result)

pers_humanitarian <- data[data$treatment != "Kherson Liberation" & data$treatment != "Ukraine Shelling", "humanitarian"]
non_pers_humanitarian <- data[data$treatment != "Drama Theater" & data$treatment != "Release of POWs", "humanitarian"]
t_test_result <- t.test(pers_humanitarian, non_pers_humanitarian)
print(t_test_result)

pers_keep <- data[data$treatment != "Kherson Liberation" & data$treatment != "Ukraine Shelling", "keep"]
non_pers_keep <- data[data$treatment != "Drama Theater" & data$treatment != "Release of POWs", "keep"]
t_test_result <- t.test(pers_keep, non_pers_keep)
print(t_test_result)

pers_defence <- data[data$treatment != "Kherson Liberation" & data$treatment != "Ukraine Shelling", "defence"]
non_pers_defence <- data[data$treatment != "Drama Theater" & data$treatment != "Release of POWs", "defence"]
t_test_result <- t.test(pers_defence, non_pers_defence)
print(t_test_result)

#Ukrainian vs non-ukrainians ORIGIN
ukr_defence <- data[data$country_origin_Ukraine == "1", "defence"]
non_ukr_defence <- data[data$country_origin_Ukraine == "0", "defence"]
t_test_result <- t.test(ukr_defence, non_ukr_defence)
print(t_test_result)

ukr_humanitarian <- data[data$country_origin_Ukraine == "1", "humanitarian"]
non_ukr_humanitarian <- data[data$country_origin_Ukraine == "0", "humanitarian"]
t_test_result <- t.test(ukr_humanitarian, non_ukr_humanitarian)
print(t_test_result)

ukr_rebuilding <- data[data$country_origin_Ukraine == "1", "rebuilding"]
non_ukr_rebuilding <- data[data$country_origin_Ukraine == "0", "rebuilding"]
t_test_result <- t.test(ukr_rebuilding, non_ukr_rebuilding)
print(t_test_result)

ukr_keep <- data[data$country_origin_Ukraine == "1", "keep"]
non_ukr_keep <- data[data$country_origin_Ukraine == "0", "keep"]
t_test_result <- t.test(ukr_keep, non_ukr_keep)
print(t_test_result)

#Ukrainian vs non-ukrainians LIVE
ukr_defence <- data[data$country_live_Ukraine == "1", "defence"]
non_ukr_defence <- data[data$country_live_Ukraine == "0", "defence"]
t_test_result <- t.test(ukr_defence, non_ukr_defence)
print(t_test_result)

ukr_humanitarian <- data[data$country_live_Ukraine == "1", "humanitarian"]
non_ukr_humanitarian <- data[data$country_live_Ukraine == "0", "humanitarian"]
t_test_result <- t.test(ukr_humanitarian, non_ukr_humanitarian)
print(t_test_result)

ukr_rebuilding <- data[data$country_live_Ukraine == "1", "rebuilding"]
non_ukr_rebuilding <- data[data$country_live_Ukraine == "0", "rebuilding"]
t_test_result <- t.test(ukr_rebuilding, non_ukr_rebuilding)
print(t_test_result)

ukr_keep <- data[data$country_live_Ukraine == "1", "keep"]
non_ukr_keep <- data[data$country_live_Ukraine == "0", "keep"]
t_test_result <- t.test(ukr_keep, non_ukr_keep)
print(t_test_result)

#Female vs male 
female_defence <- data[data$gender == "female", "defence"]
male_defence <- data[data$gender == "male", "defence"]
t_test_result <- t.test(female_defence, male_defence)
print(t_test_result)

female_humanitarian <- data[data$gender == "female", "humanitarian"]
male_humanitarian <- data[data$gender == "male", "humanitarian"]
t_test_result <- t.test(female_humanitarian, male_humanitarian)
print(t_test_result)

female_rebuilding <- data[data$gender == "female", "rebuilding"]
male_rebuilding <- data[data$gender == "male", "rebuilding"]
t_test_result <- t.test(female_rebuilding, male_rebuilding)
print(t_test_result)

female_keep <- data[data$gender == "female", "keep"]
male_keep <- data[data$gender == "male", "keep"]
t_test_result <- t.test(female_keep, male_keep)
print(t_test_result)