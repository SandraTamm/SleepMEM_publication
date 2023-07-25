# Table 1
library(table1)


load("~/Desktop/Data/demographics_sleep_mood.RData")

All_data$Typical_TIB <- All_data$Typical_TIB*60
All_data$TIB <- All_data$TIB*60  

table1(~ Sex + Age + Education + Overall_health + Dementia_MCI + Any_depression_diagnosis + Current_depression + Current_antidepressant +
       Current_psychological_therapy_depression, 
       data=All_data)





# Make table with those who have full evening assessmnet

full_eveining <- subset(All_data, !is.na(All_data$Evening_PANAS_neg))

table1(~ Sex + Age + Education + Overall_health + Dementia_MCI + Any_depression_diagnosis + Current_depression + Current_antidepressant +
         Current_psychological_therapy_depression, 
       data=full_eveining)


load("~/Desktop/Data/image_ratings.RData")
image_ratings <- data_out

complete_mornings <- subset(image_ratings, !is.na(image_ratings$Valence_morning_negative))
complete_mornings <- subset(complete_mornings, select = "Participant_ID")


full_morning <- merge(complete_mornings, All_data, all.x = TRUE)

table1(~ Sex + Age + Education + Overall_health + Dementia_MCI + Any_depression_diagnosis + Current_depression + Current_antidepressant +
         Current_psychological_therapy_depression, 
       data=full_morning)





# Table of sleep and sleepiness
table1(~ SCI + Probable_insomnia + Typical_sleep_duration + Typical_TIB + Typical_sleep_efficiency + Sleep_medication + Evening_KSS + Sleep_quality_last_night +
         Sleep_duration_last_night +TIB+ Sleep_efficiency + Morning_KSS, 
       data=All_data)


table1(~ SCI + Probable_insomnia + Typical_sleep_duration + Typical_TIB + Typical_sleep_efficiency + Sleep_medication + Evening_KSS + Sleep_quality_last_night +
         Sleep_duration_last_night +TIB+ Sleep_efficiency + Morning_KSS, 
       data=full_eveining)

table1(~ SCI + Probable_insomnia + Typical_sleep_duration + Typical_TIB + Typical_sleep_efficiency + Sleep_medication + Evening_KSS + Sleep_quality_last_night +
         Sleep_duration_last_night +TIB+ Sleep_efficiency + Morning_KSS, 
       data=full_morning)


