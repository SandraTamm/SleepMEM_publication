# Analyses
library(gridExtra)
library(ggplot2)
library(ggeffects)
library(sjPlot)
library(sjmisc)
library(sjlabelled)

load("~/Desktop/Data/wordpairs.RData")
wordpairs <- data_out

load("~/Desktop/Data/demographics_sleep_mood.RData")


wordpairs <- merge(wordpairs, All_data)

load("~/Desktop/Data/image_ratings.RData")
image_ratings <- data_out

image_ratings <- merge(image_ratings, All_data)

load("~/Desktop/Data/emotional_memory.RData")

recall <- data_out

recall <- merge(recall, All_data)

#### Habitual poor sleep quality, habitual short sleep duration and habitual sleep efficiency will be 
#negatively associated with declarative memory performance in a word-pairs task (immediate recall) 

#SCI

h1_a <- ggplot(data = wordpairs) +
  geom_point(mapping = aes(x = SCI, y = Correct_answers_evening), size = 1)+
  geom_smooth(method = lm, formula = y~x, aes(x = SCI, y = Correct_answers_evening))+
  labs(x = "Sleep Condition Indicator", y = "Correct answers (immediate recall)") +
  ggtitle("A")+
  theme_ggeffects()


# Crude model
h1a_crude <- lm(Correct_answers_evening ~ SCI, data = wordpairs)

tab_model(h1a_crude)

# Adjusted model
h1a_adj <- lm(Correct_answers_evening ~ SCI + Age_numeric + Sex + Overall_health_numeric + Education_numeric + Dementia_MCI, data = wordpairs)

tab_model(h1a_adj)

# Sleep duration

h1_b <- ggplot(data = wordpairs) +
  geom_point(mapping = aes(x = Typical_sleep_duration, y = Correct_answers_evening), size = 1)+
  geom_smooth(method = lm, formula = y~x, aes(x = Typical_sleep_duration, y = Correct_answers_evening))+
  labs(x = "Typical sleep duration (min)", y = "Correct answers (immediate recall)") +
  ggtitle("B")+
  theme_ggeffects()

# Crude model
h1b_crude <- lm(Correct_answers_evening ~ Typical_sleep_duration, data = wordpairs)
tab_model(h1b_crude)

# Adjusted model
h1b_adj <- lm(Correct_answers_evening ~ Typical_sleep_duration + Age_numeric + Sex + Overall_health_numeric + Education_numeric + Dementia_MCI, data = wordpairs)
tab_model(h1b_adj)


# Sleep efficiency

h1_c <- ggplot(data = wordpairs) +
  geom_point(mapping = aes(x = Typical_sleep_efficiency, y = Correct_answers_evening), size = 1)+
  geom_smooth(method = lm, formula = y~x, aes(x = Typical_sleep_efficiency, y = Correct_answers_evening))+
  labs(x = "Typical sleep efficiency", y = "Correct answers (immediate recall)") +
  ggtitle("C")+
  theme_ggeffects()

# Crude model

h1c_crude <- lm(Correct_answers_evening ~ Typical_sleep_efficiency, data = wordpairs)
tab_model(h1c_crude)


# Adjusted model
h1c_adj <- lm(Correct_answers_evening ~ Typical_sleep_efficiency + Age_numeric + Sex + Overall_health_numeric + Education_numeric + Dementia_MCI, data = wordpairs)
tab_model(h1c_adj)

grid.arrange(h1_a, h1_b, h1_c, ncol=1)



#Poor sleep quality, short sleep duration and low sleep efficiency for the preceding night will be associated with less 
#overnight memory consolidation, as measured with a word-pairs task (delayed recall, in the morning), 
#co-varying for pre-sleep performance

# Sleep quality
plot(wordpairs$Correct_answers_morning ~ wordpairs$Sleep_quality_last_night)
#plot(wordpairs$Correct_answers_wordpairs_change ~ wordpairs$Sleep_quality_last_night)

h2_a <- ggplot(data = wordpairs) +
  geom_boxplot(mapping = aes(x = Sleep_quality_last_night, y = Correct_answers_wordpairs_change), size = 1)+
  geom_smooth(method = lm, formula = y~x, aes(x = Sleep_quality_last_night, y = Correct_answers_wordpairs_change))+
  labs(x = "Sleep quality last night", y = "Correct answers (overnight change)") +
  ggtitle("A")+
  theme_ggeffects()


# Crude model

h2a_crude <- lm(Correct_answers_wordpairs_change ~ Sleep_quality_last_night_numeric + Correct_answers_evening, data = wordpairs)
tab_model(h2a_crude)

# Adjusted model
h2a_adj <-lm(Correct_answers_wordpairs_change ~ Sleep_quality_last_night_numeric + Correct_answers_evening + Age_numeric + Sex + Overall_health_numeric + 
             Education_numeric + Dementia_MCI, data = wordpairs)
tab_model(h2a_adj)



# Sleep duration
plot(wordpairs$Correct_answers_wordpairs_change ~ wordpairs$Sleep_duration_last_night)
abline(lm(wordpairs$Correct_answers_wordpairs_change ~ wordpairs$Sleep_duration_last_night))

h2_b <- ggplot(data = wordpairs) +
  geom_point(mapping = aes(x = Sleep_duration_last_night, y = Correct_answers_wordpairs_change), size = 1)+
  geom_smooth(method = lm, formula = y~x, aes(x = Sleep_duration_last_night, y = Correct_answers_wordpairs_change))+
  labs(x = "Sleep duration last night last night", y = "Correct answers (overnight change)") +
  ggtitle("B")+
  theme_ggeffects()


# Crude model

h2b_crude <- lm(Correct_answers_wordpairs_change ~ Sleep_duration_last_night + Correct_answers_evening, data = wordpairs)
tab_model(h2b_crude)


# Adjusted model
h2b_adj <- lm(Correct_answers_wordpairs_change ~ Sleep_duration_last_night + Correct_answers_evening + Age_numeric + Sex + Overall_health_numeric +
             Education_numeric + Dementia_MCI, data = wordpairs)
tab_model(h2b_adj)



# Sleep efficiency
plot(wordpairs$Correct_answers_wordpairs_change ~ wordpairs$Sleep_efficiency)
abline(lm(wordpairs$Correct_answers_wordpairs_change ~ wordpairs$Sleep_efficiency))


h2_c <- ggplot(data = wordpairs) +
  geom_point(mapping = aes(x = Sleep_efficiency, y = Correct_answers_wordpairs_change), size = 1)+
  geom_smooth(method = lm, formula = y~x, aes(x = Sleep_efficiency, y = Correct_answers_wordpairs_change))+
  labs(x = "Sleep efficiency last night", y = "Correct answers (overnight change)") +
  ggtitle("C")+
  theme_ggeffects()


# Crude model

h2c_crude <- lm(Correct_answers_wordpairs_change ~ Sleep_efficiency + Correct_answers_evening, data = wordpairs)
tab_model(h2c_crude)

# Adjusted model
h2c_adj <- lm(Correct_answers_wordpairs_change ~ Sleep_efficiency + Correct_answers_evening + Age_numeric + Sex + Overall_health_numeric +
             Education_numeric + Dementia_MCI, data = wordpairs)

tab_model(h2c_adj)


grid.arrange(h2_a, h2_b, h2_c, ncol=1)

### Image ratings
### Habitual poor sleep quality, habitual short sleep duration and habitual low sleep efficiency will be associated with higher ratings 
# of unpleasantness in response to negative emotional images


#SCI

h3_a <- ggplot(data = image_ratings) +
  geom_point(mapping = aes(x = SCI, y = Valence_evening_negative), size = 1)+
  geom_smooth(method = lm, formula = y~x, aes(x = SCI, y = Valence_evening_negative))+
  labs(x = "Sleep Condition Indicator", y = "Valence evening (negative stimuli)") +
  ggtitle("A")+
  theme_ggeffects()


# Crude model
h3a_crude <- lm(Valence_evening_negative ~ SCI, data = image_ratings)

tab_model(h3a_crude)

# Adjusted model
h3a_adj <- lm(Valence_evening_negative ~ SCI + Age_numeric + Sex + Overall_health_numeric + Education_numeric + Dementia_MCI, data = image_ratings)

tab_model(h3a_adj)

# Sleep duration

h3_b <- ggplot(data = image_ratings) +
  geom_point(mapping = aes(x = Typical_sleep_duration, y = Valence_evening_negative), size = 1)+
  geom_smooth(method = lm, formula = y~x, aes(x = Typical_sleep_duration, y = Valence_evening_negative))+
  labs(x = "Typical sleep duration (min)", y = "Valence evening (negative stimuli)") +
  ggtitle("B")+
  theme_ggeffects()

# Crude model
h3b_crude <- lm(Valence_evening_negative ~ Typical_sleep_duration, data = image_ratings)
tab_model(h3b_crude)

# Adjusted model
h3b_adj <- lm(Valence_evening_negative ~ Typical_sleep_duration + Age_numeric + Sex + Overall_health_numeric + Education_numeric + Dementia_MCI, data = image_ratings)
tab_model(h3b_adj)


# Sleep efficiency
h3_c <- ggplot(data = image_ratings) +
  geom_point(mapping = aes(x = Typical_sleep_efficiency, y = Valence_evening_negative), size = 1)+
  geom_smooth(method = lm, formula = y~x, aes(x = Typical_sleep_efficiency, y = Valence_evening_negative))+
  labs(x = "Typical sleep efficiency", y = "Valence evening (negative stimuli)") +
  ggtitle("C")+
  theme_ggeffects()

# Crude model

h3c_crude <- lm(Valence_evening_negative ~ Typical_sleep_efficiency, data = image_ratings)
tab_model(h3c_crude)


# Adjusted model
h3c_adj <- lm(Valence_evening_negative ~ Typical_sleep_efficiency + Age_numeric + Sex + Overall_health_numeric + Education_numeric + Dementia_MCI, data = image_ratings)
tab_model(h3c_adj)

grid.arrange(h3_a, h3_b, h3_c, ncol=1)




##	Poor self-rated sleep quality, short sleep duration and low sleep efficiency for the preceding night will be associated with 
#higher ratings of unpleasantness in response to negative emotional images... 

# Sleep quality

h4_aa <- ggplot(data = image_ratings) +
  geom_boxplot(mapping = aes(x = Sleep_quality_last_night, y = Valence_morning_negative), size = 1)+
  geom_smooth(method = lm, formula = y~x, aes(x = Sleep_quality_last_night, y = Valence_morning_negative))+
  labs(x = "Sleep quality last night", y = "Valence morning (negative stimuli)") +
  ggtitle("A")+
  theme_ggeffects()


# Crude model

h4aa_crude <- lm(Valence_morning_negative ~ Sleep_quality_last_night_numeric, data = image_ratings)
tab_model(h4aa_crude)

# Adjusted model
h4aa_adj <-lm(Valence_morning_negative ~ Sleep_quality_last_night_numeric + Age_numeric + Sex + Overall_health_numeric + 
               Education_numeric + Dementia_MCI, data = image_ratings)
tab_model(h4aa_adj)



# Sleep duration
h4_ab <- ggplot(data = image_ratings) +
  geom_point(mapping = aes(x = Sleep_duration_last_night, y = Valence_morning_negative), size = 1)+
  geom_smooth(method = lm, formula = y~x, aes(x = Sleep_duration_last_night, y = Valence_morning_negative))+
  labs(x = "Sleep duration last night last night", y = "Valence morning (negative stimuli)") +
  ggtitle("B")+
  theme_ggeffects()


# Crude model

h4ab_crude <- lm(Valence_morning_negative ~ Sleep_duration_last_night, data = image_ratings)
tab_model(h4ab_crude)


# Adjusted model
h4ab_adj <- lm(Valence_morning_negative ~ Sleep_duration_last_night + Age_numeric + Sex + Overall_health_numeric +
                Education_numeric + Dementia_MCI, data = image_ratings)
tab_model(h4ab_adj)



# Sleep efficiency

h4_ac <- ggplot(data = image_ratings) +
  geom_point(mapping = aes(x = Sleep_efficiency, y = Valence_morning_negative), size = 1)+
  geom_smooth(method = lm, formula = y~x, aes(x = Sleep_efficiency, y = Valence_morning_negative))+
  labs(x = "Sleep efficiency last night", y = "Valence morning (negative stimuli)") +
  ggtitle("C")+
  theme_ggeffects()


# Crude model

h4ac_crude <- lm(Valence_morning_negative ~ Sleep_efficiency, data = image_ratings)
tab_model(h4ac_crude)

# Adjusted model
h4ac_adj <- lm(Valence_morning_negative ~ Sleep_efficiency + Age_numeric + Sex + Overall_health_numeric +
                Education_numeric + Dementia_MCI, data = image_ratings)

tab_model(h4ac_adj)


##and less overnight habituation for ratings of unpleasantness

# Sleep quality

h4_ba <- ggplot(data = image_ratings) +
  geom_boxplot(mapping = aes(x = Sleep_quality_last_night, y = Valence_change_negative), size = 1)+
  geom_smooth(method = lm, formula = y~x, aes(x = Sleep_quality_last_night, y = Valence_change_negative))+
  labs(x = "Sleep quality last night", y = "Valence change (negative stimuli)") +
  ggtitle("D")+
  theme_ggeffects()


# Crude model

h4ba_crude <- lm(Valence_change_negative ~ Sleep_quality_last_night_numeric, data = image_ratings)
tab_model(h4ba_crude)

# Adjusted model
h4ba_adj <-lm(Valence_change_negative ~ Sleep_quality_last_night_numeric + Age_numeric + Sex + Overall_health_numeric + 
                Education_numeric + Dementia_MCI, data = image_ratings)
tab_model(h4ba_adj)



# Sleep duration
h4_bb <- ggplot(data = image_ratings) +
  geom_point(mapping = aes(x = Sleep_duration_last_night, y = Valence_change_negative), size = 1)+
  geom_smooth(method = lm, formula = y~x, aes(x = Sleep_duration_last_night, y = Valence_change_negative))+
  labs(x = "Sleep duration last night last night", y = "Valence change (negative stimuli)") +
  ggtitle("E")+
  theme_ggeffects()


# Crude model

h4bb_crude <- lm(Valence_change_negative ~ Sleep_duration_last_night, data = image_ratings)
tab_model(h4bb_crude)


# Adjusted model
h4bb_adj <- lm(Valence_change_negative ~ Sleep_duration_last_night + Age_numeric + Sex + Overall_health_numeric +
                 Education_numeric + Dementia_MCI, data = image_ratings)
tab_model(h4bb_adj)



# Sleep efficiency

h4_bc <- ggplot(data = image_ratings) +
  geom_point(mapping = aes(x = Sleep_efficiency, y = Valence_change_negative), size = 1)+
  geom_smooth(method = lm, formula = y~x, aes(x = Sleep_efficiency, y = Valence_change_negative))+
  labs(x = "Sleep efficiency last night", y = "Valence change (negative stimuli)") +
  ggtitle("F")+
  theme_ggeffects()


# Crude model

h4bc_crude <- lm(Valence_change_negative ~ Sleep_efficiency, data = image_ratings)
tab_model(h4bc_crude)

# Adjusted model
h4bc_adj <- lm(Valence_change_negative ~ Sleep_efficiency + Age_numeric + Sex + Overall_health_numeric +
                 Education_numeric + Dementia_MCI, data = image_ratings)

tab_model(h4bc_adj)




grid.arrange(h4_aa, h4_ab, h4_ac, h4_ba, h4_bb, h4_bc, ncol=2)





#Poor self-rated sleep quality, short sleep duration and low sleep efficiency for the preceding night 
# will be associated with less accuracy in recalling both neutral and negative images. 

# Negative
# Sleep quality

h5_aa <- ggplot(data = recall) +
  geom_boxplot(mapping = aes(x = Sleep_quality_last_night, y = d_prime_neg), size = 1)+
  geom_smooth(method = lm, formula = y~x, aes(x = Sleep_quality_last_night, y = d_prime_neg))+
  labs(x = "Sleep quality last night", y = "Recognition accuracy (d', negative) ") +
  ggtitle("A")+
  theme_ggeffects()


# Crude model

h5aa_crude <- lm(d_prime_neg ~ Sleep_quality_last_night_numeric, data = recall)
tab_model(h5aa_crude)

# Adjusted model
h5aa_adj <-lm(d_prime_neg ~ Sleep_quality_last_night_numeric + Age_numeric + Sex + Overall_health_numeric + 
                Education_numeric + Dementia_MCI, data = recall)
tab_model(h5aa_adj)



# Neutral

h5_ab <- ggplot(data = recall) +
  geom_boxplot(mapping = aes(x = Sleep_quality_last_night, y = d_prime_neu), size = 1)+
  geom_smooth(method = lm, formula = y~x, aes(x = Sleep_quality_last_night, y = d_prime_neu))+
  labs(x = "Sleep quality last night", y = "Recognition accuracy (d', neutral) ") +
  ggtitle("B")+
  theme_ggeffects()


# Crude model

h5ab_crude <- lm(d_prime_neu ~ Sleep_quality_last_night_numeric, data = recall)
tab_model(h5ab_crude)

# Adjusted model
h5ab_adj <-lm(d_prime_neu ~ Sleep_quality_last_night_numeric + Age_numeric + Sex + Overall_health_numeric + 
               Education_numeric + Dementia_MCI, data = recall)
tab_model(h5ab_adj)


# Sleep duration
# Negative
h5_ba <- ggplot(data = recall) +
  geom_point(mapping = aes(x = Sleep_duration_last_night, y = d_prime_neg), size = 1)+
  geom_smooth(method = lm, formula = y~x, aes(x = Sleep_duration_last_night, y = d_prime_neg))+
  labs(x = "Sleep duration last night last night", y = "Recognition accuracy (d', negative)") +
  ggtitle("C")+
  theme_ggeffects()


# Crude model

h5ba_crude <- lm(d_prime_neg ~ Sleep_duration_last_night, data = recall)
tab_model(h5ba_crude)


# Adjusted model
h5ba_adj <- lm(d_prime_neg ~ Sleep_duration_last_night + Age_numeric + Sex + Overall_health_numeric +
                 Education_numeric + Dementia_MCI, data = recall)
tab_model(h5ba_adj)



# Neutral
h5_bb <- ggplot(data = recall) +
  geom_point(mapping = aes(x = Sleep_duration_last_night, y = d_prime_neu), size = 1)+
  geom_smooth(method = lm, formula = y~x, aes(x = Sleep_duration_last_night, y = d_prime_neu))+
  labs(x = "Sleep duration last night last night", y = "Recognition accuracy (d', neutral)") +
  ggtitle("D")+
  theme_ggeffects()


# Crude model

h5bb_crude <- lm(d_prime_neu ~ Sleep_duration_last_night, data = recall)
tab_model(h5bb_crude)


# Adjusted model
h5bb_adj <- lm(d_prime_neu ~ Sleep_duration_last_night + Age_numeric + Sex + Overall_health_numeric +
                 Education_numeric + Dementia_MCI, data = recall)
tab_model(h5bb_adj)


# Sleep efficiency
# NEgative

h5_ca <- ggplot(data = recall) +
  geom_point(mapping = aes(x = Sleep_efficiency, y = d_prime_neg), size = 1)+
  geom_smooth(method = lm, formula = y~x, aes(x = Sleep_efficiency, y = d_prime_neg))+
  labs(x = "Sleep efficiency last night", y = "Recognition accuracy (d', negative)") +
  ggtitle("E")+
  theme_ggeffects()


# Crude model

h5ca_crude <- lm(d_prime_neg ~ Sleep_efficiency, data = recall)
tab_model(h5ca_crude)

# Adjusted model
h5ca_adj <- lm(d_prime_neg ~ Sleep_efficiency + Age_numeric + Sex + Overall_health_numeric +
                 Education_numeric + Dementia_MCI, data = recall)

tab_model(h5ca_adj)


# Neutral

h5_cb <- ggplot(data = recall) +
  geom_point(mapping = aes(x = Sleep_efficiency, y = d_prime_neu), size = 1)+
  geom_smooth(method = lm, formula = y~x, aes(x = Sleep_efficiency, y = d_prime_neu))+
  labs(x = "Sleep efficiency last night", y = "Recognition accuracy (d', neutral)") +
  ggtitle("F")+
  theme_ggeffects()


# Crude model

h5cb_crude <- lm(d_prime_neu ~ Sleep_efficiency, data = recall)
tab_model(h5cb_crude)

# Adjusted model
h5cb_adj <- lm(d_prime_neu ~ Sleep_efficiency + Age_numeric + Sex + Overall_health_numeric +
                 Education_numeric + Dementia_MCI, data = recall)

tab_model(h5cb_adj)


grid.arrange(h5_aa, h5_ab, h5_ba, h5_bb, h5_ca, h5_cb)



plot(recall$Correct_answers_picture ~ recall$Sleep_quality_last_night)
plot(recall$Correct_answers_picture_negative ~ recall$Sleep_quality_last_night)
plot(recall$Correct_answers_picture_neutral ~ recall$Sleep_quality_last_night)

# All pictures, sleep quality
# Crude model

summary(lm(recall$Correct_answers_picture ~ recall$Sleep_quality_last_night_numeric))

# Adjusted model
summary(lm(Correct_answers_picture ~ Sleep_quality_last_night_numeric + Age_numeric + Sex + Overall_health_numeric + Education_numeric + Dementia_MCI, data = recall))

# Neg pictures, sleep quality
# Crude model

summary(lm(recall$Correct_answers_picture_negative ~ recall$Sleep_quality_last_night_numeric))

# Adjusted model
summary(lm(Correct_answers_picture_negative ~ Sleep_quality_last_night_numeric + Age_numeric + Sex + Overall_health_numeric + Education_numeric + Dementia_MCI, data = recall))

# Neutral pictures, sleep quality
# Crude model

summary(lm(recall$Correct_answers_picture_neutral ~ recall$Sleep_quality_last_night_numeric))

# Adjusted model
summary(lm(Correct_answers_picture_neutral ~ Sleep_quality_last_night_numeric + Age_numeric + Sex + Overall_health_numeric + Education_numeric + Dementia_MCI, data = recall))


# Sleep duration
plot(recall$Correct_answers_picture ~ recall$Sleep_duration_last_night)
abline(lm(recall$Correct_answers_picture ~ recall$Sleep_duration_last_night))
plot(recall$Correct_answers_picture_negative ~ recall$Sleep_duration_last_night)
abline(lm(recall$Correct_answers_picture_negative ~ recall$Sleep_duration_last_night))
plot(recall$Correct_answers_picture_neutral ~ recall$Sleep_duration_last_night)
abline(lm(recall$Correct_answers_picture_neutral ~ recall$Sleep_duration_last_night))

# All pictures, sleep duration
# Crude model

summary(lm(recall$Correct_answers_picture ~ recall$Sleep_duration_last_night))

# Adjusted model
summary(lm(Correct_answers_picture ~ Sleep_duration_last_night + Age_numeric + Sex + Overall_health_numeric + Education_numeric + Dementia_MCI, data = recall))

# Neg pictures, sleep duration
# Crude model

summary(lm(recall$Correct_answers_picture_negative ~ recall$Sleep_duration_last_night))

# Adjusted model
summary(lm(Correct_answers_picture_negative ~ Sleep_duration_last_night + Age_numeric + Sex + Overall_health_numeric + Education_numeric + Dementia_MCI, data = recall))

# Neutral pictures, sleep duration
# Crude model

summary(lm(recall$Correct_answers_picture_neutral ~ recall$Sleep_duration_last_night))

# Adjusted model
summary(lm(Correct_answers_picture_neutral ~ Sleep_duration_last_night + Age_numeric + Sex + Overall_health_numeric + Education_numeric + Dementia_MCI, data = recall))


# Sleep efficiency
plot(recall$Correct_answers_picture ~ recall$Sleep_efficiency)
abline(lm(recall$Correct_answers_picture ~ recall$Sleep_efficiency))
plot(recall$Correct_answers_picture_negative ~ recall$Sleep_efficiency)
abline(lm(recall$Correct_answers_picture_negative ~ recall$Sleep_efficiency))
plot(recall$Correct_answers_picture_neutral ~ recall$Sleep_efficiency)
abline(lm(recall$Correct_answers_picture_neutral ~ recall$Sleep_efficiency))

# All pictures, sleep efficiency
# Crude model

summary(lm(recall$Correct_answers_picture ~ recall$Sleep_efficiency))

# Adjusted model
summary(lm(Correct_answers_picture ~ Sleep_efficiency + Age_numeric + Sex + Overall_health_numeric + Education_numeric + Dementia_MCI, data = recall))

# Neg pictures, sleep efficiency
# Crude model

summary(lm(recall$Correct_answers_picture_negative ~ recall$Sleep_efficiency))

# Adjusted model
summary(lm(Correct_answers_picture_negative ~ Sleep_efficiency + Age_numeric + Sex + Overall_health_numeric + Education_numeric + Dementia_MCI, data = recall))

# Neutral pictures, sleep efficiency
# Crude model

summary(lm(recall$Correct_answers_picture_neutral ~ recall$Sleep_efficiency))

# Adjusted model
summary(lm(Correct_answers_picture_neutral ~ Sleep_efficiency + Age_numeric + Sex + Overall_health_numeric + Education_numeric + Dementia_MCI, data = recall))


#Habitual poor sleep quality, habitual sleep duration and habitual sleep efficiency will be associated with more negative affect and less positive affect 
# Sleep quality

# Negative affect

#SCI

h6_a <- ggplot(data = All_data) +
  geom_point(mapping = aes(x = SCI, y = Evening_PANAS_neg), size = 1)+
  geom_smooth(method = lm, formula = y~x, aes(x = SCI, y = Evening_PANAS_neg))+
  labs(x = "Sleep Condition Indicator", y = "Negative affect (PANAS)") +
  ggtitle("A")+
  theme_ggeffects()


# Crude model
h6a_crude <- lm(Evening_PANAS_neg ~ SCI, data = All_data)

tab_model(h6a_crude)

# Adjusted model
h6a_adj <- lm(Evening_PANAS_neg ~ SCI + Age_numeric + Sex + Overall_health_numeric + Education_numeric + Dementia_MCI, data = All_data)

tab_model(h6a_adj)

# Sleep duration

h6_b <- ggplot(data = All_data) +
  geom_point(mapping = aes(x = Typical_sleep_duration, y = Evening_PANAS_neg), size = 1)+
  geom_smooth(method = lm, formula = y~x, aes(x = Typical_sleep_duration, y = Evening_PANAS_neg))+
  labs(x = "Habitual sleep duration (min)", y = "Negative affect (PANAS)") +
  ggtitle("B")+
  theme_ggeffects()

# Crude model
h6b_crude <- lm(Evening_PANAS_neg ~ Typical_sleep_duration, data = All_data)
tab_model(h6b_crude)

# Adjusted model
h6b_adj <- lm(Evening_PANAS_neg ~ Typical_sleep_duration + Age_numeric + Sex + Overall_health_numeric + Education_numeric + Dementia_MCI, data = All_data)
tab_model(h6b_adj)


# Sleep efficiency

h6_c <- ggplot(data = All_data) +
  geom_point(mapping = aes(x = Typical_sleep_efficiency, y = Evening_PANAS_neg), size = 1)+
  geom_smooth(method = lm, formula = y~x, aes(x = Typical_sleep_efficiency, y = Evening_PANAS_neg))+
  labs(x = "Habitual sleep efficiency", y = "Negative affect (PANAS)") +
  ggtitle("C")+
  theme_ggeffects()

# Crude model

h6c_crude <- lm(Evening_PANAS_neg ~ Typical_sleep_efficiency, data = All_data)
tab_model(h6c_crude)


# Adjusted model
h6c_adj <- lm(Evening_PANAS_neg ~ Typical_sleep_efficiency + Age_numeric + Sex + Overall_health_numeric + Education_numeric + Dementia_MCI, data = All_data)
tab_model(h6c_adj)

# Positive affect
h6_d <- ggplot(data = All_data) +
  geom_point(mapping = aes(x = SCI, y = Evening_PANAS_pos), size = 1)+
  geom_smooth(method = lm, formula = y~x, aes(x = SCI, y = Evening_PANAS_pos))+
  labs(x = "Sleep Condition Indicator", y = "Positive affect (PANAS)") +
  ggtitle("D")+
  theme_ggeffects()


# Crude model
h6d_crude <- lm(Evening_PANAS_pos ~ SCI, data = All_data)

tab_model(h6d_crude)

# Adjusted model
h6d_adj <- lm(Evening_PANAS_pos ~ SCI + Age_numeric + Sex + Overall_health_numeric + Education_numeric + Dementia_MCI, data = All_data)

tab_model(h6d_adj)

# Sleep duration

h6_e <- ggplot(data = All_data) +
  geom_point(mapping = aes(x = Typical_sleep_duration, y = Evening_PANAS_pos), size = 1)+
  geom_smooth(method = lm, formula = y~x, aes(x = Typical_sleep_duration, y = Evening_PANAS_pos))+
  labs(x = "Habitual sleep duration (min)", y = "Positive affect (PANAS)") +
  ggtitle("E")+
  theme_ggeffects()

# Crude model
h6e_crude <- lm(Evening_PANAS_pos ~ Typical_sleep_duration, data = All_data)
tab_model(h6e_crude)

# Adjusted model
h6e_adj <- lm(Evening_PANAS_pos ~ Typical_sleep_duration + Age_numeric + Sex + Overall_health_numeric + Education_numeric + Dementia_MCI, data = All_data)
tab_model(h6e_adj)


# Sleep efficiency

h6_f <- ggplot(data = All_data) +
  geom_point(mapping = aes(x = Typical_sleep_efficiency, y = Evening_PANAS_pos), size = 1)+
  geom_smooth(method = lm, formula = y~x, aes(x = Typical_sleep_efficiency, y = Evening_PANAS_pos))+
  labs(x = "Habitual sleep efficiency", y = "Positive affect (PANAS)") +
  ggtitle("F")+
  theme_ggeffects()

# Crude model

h6f_crude <- lm(Evening_PANAS_pos ~ Typical_sleep_efficiency, data = All_data)
tab_model(h6f_crude)


# Adjusted model
h6f_adj <- lm(Evening_PANAS_pos ~ Typical_sleep_efficiency + Age_numeric + Sex + Overall_health_numeric + Education_numeric + Dementia_MCI, data = All_data)
tab_model(h6f_adj)



grid.arrange(h6_a, h6_b, h6_c, h6_d, h6_e, h6_f, ncol=2)



# Poor self-rated sleep quality and short sleep duration for the preceding night will be associated with lower positive and higher negative current
# affect in the following morning, co-varying for pre-sleep mood. 




Morning_PANAS_data <- subset(All_data, !is.na(All_data$Morning_PANAS_pos))

# Negative affect

#Sleep Quality

h7_a <- ggplot(data = Morning_PANAS_data) +
  geom_boxplot(mapping = aes(x = Sleep_quality_last_night, y = Morning_PANAS_neg), size = 1)+
  labs(x = "Sleep quality last night", y = "Negative affect (PANAS)") +
  ggtitle("A")+
  theme_ggeffects()


# Crude model
h7a_crude <- lm(Morning_PANAS_neg ~ Sleep_quality_last_night_numeric + Evening_PANAS_neg, data = Morning_PANAS_data)

tab_model(h7a_crude)

# Adjusted model
h7a_adj <- lm(Morning_PANAS_neg ~ Sleep_quality_last_night_numeric + Evening_PANAS_neg + Age_numeric + Sex + Overall_health_numeric + Education_numeric + Dementia_MCI, data = All_data)

tab_model(h7a_adj)

# Sleep duration

h7_b <- ggplot(data = Morning_PANAS_data) +
  geom_point(mapping = aes(x = Sleep_duration_last_night, y = Morning_PANAS_neg), size = 1)+
  geom_smooth(method = lm, formula = y~x, aes(x = Sleep_duration_last_night, y = Morning_PANAS_neg))+
  labs(x = "Sleep duration last night", y = "Negative affect (PANAS)") +
  ggtitle("B")+
  theme_ggeffects()

# Crude model
h7b_crude <- lm(Morning_PANAS_neg ~ Sleep_duration_last_night + Evening_PANAS_neg, data = All_data)
tab_model(h7b_crude)

# Adjusted model
h7b_adj <- lm(Morning_PANAS_neg ~ Sleep_duration_last_night + Evening_PANAS_neg + Age_numeric + Sex + Overall_health_numeric + Education_numeric + Dementia_MCI, data = All_data)
tab_model(h7b_adj)


# Sleep efficiency

h7_c <- ggplot(data = Morning_PANAS_data) +
  geom_point(mapping = aes(x = Sleep_efficiency, y = Morning_PANAS_neg), size = 1)+
  geom_smooth(method = lm, formula = y~x, aes(x = Sleep_efficiency, y = Morning_PANAS_neg))+
  labs(x = "Sleep efficiency last night", y = "Negative affect (PANAS)") +
  ggtitle("C")+
  theme_ggeffects()

# Crude model

h7c_crude <- lm(Morning_PANAS_neg ~ Sleep_efficiency + Evening_PANAS_neg, data = All_data)
tab_model(h7c_crude)


# Adjusted model
h7c_adj <- lm(Morning_PANAS_neg ~ Sleep_efficiency + Evening_PANAS_neg + Age_numeric + Sex + Overall_health_numeric + Education_numeric + Dementia_MCI, data = All_data)
tab_model(h7c_adj)


#### START FROM HERE


# Positive affect
h7_d <- ggplot(data = Morning_PANAS_data) +
  geom_boxplot(mapping = aes(x = Sleep_quality_last_night, y = Morning_PANAS_pos), size = 1)+
  geom_smooth(method = lm, formula = y~x, aes(x = Sleep_quality_last_night, y = Morning_PANAS_pos))+
  labs(x = "Sleep quality last night", y = "Positive affect (PANAS)") +
  ggtitle("D")+
  theme_ggeffects()


# Crude model
h7d_crude <- lm(Morning_PANAS_pos ~ Sleep_quality_last_night_numeric + Evening_PANAS_pos, data = All_data)

tab_model(h7d_crude)

# Adjusted model
h7d_adj <- lm(Morning_PANAS_pos ~ Sleep_quality_last_night_numeric + Evening_PANAS_pos + Age_numeric + Sex + Overall_health_numeric + Education_numeric + Dementia_MCI, data = All_data)

tab_model(h7d_adj)

# Sleep duration

h7_e <- ggplot(data = Morning_PANAS_data) +
  geom_point(mapping = aes(x = Sleep_duration_last_night, y = Morning_PANAS_pos), size = 1)+
  geom_smooth(method = lm, formula = y~x, aes(x = Sleep_duration_last_night, y = Morning_PANAS_pos))+
  labs(x = "Sleep duration last night", y = "Positive affect (PANAS)") +
  ggtitle("E")+
  theme_ggeffects()

# Crude model
h7e_crude <- lm(Morning_PANAS_pos ~ Sleep_duration_last_night + Evening_PANAS_pos, data = All_data)
tab_model(h7e_crude)

# Adjusted model
h7e_adj <- lm(Morning_PANAS_pos ~ Sleep_duration_last_night + Evening_PANAS_pos + Age_numeric + Sex + Overall_health_numeric + Education_numeric + Dementia_MCI, data = All_data)
tab_model(h7e_adj)


# Sleep efficiency

h7_f <- ggplot(data = Morning_PANAS_data) +
  geom_point(mapping = aes(x = Sleep_efficiency, y = Morning_PANAS_pos), size = 1)+
  geom_smooth(method = lm, formula = y~x, aes(x = Sleep_efficiency, y = Morning_PANAS_pos))+
  labs(x = "Sleep efficiency last night", y = "Positive affect (PANAS)") +
  ggtitle("F")+
  theme_ggeffects()

# Crude model

h7f_crude <- lm(Morning_PANAS_pos ~ Sleep_efficiency + Evening_PANAS_pos, data = All_data)
tab_model(h7f_crude)


# Adjusted model
h7f_adj <- lm(Morning_PANAS_pos ~ Sleep_efficiency + Evening_PANAS_pos + Age_numeric + Sex + Overall_health_numeric + Education_numeric + Dementia_MCI, data = All_data)
tab_model(h7f_adj)



grid.arrange(h7_a, h7_b, h7_c, h7_d, h7_e, h7_f, ncol=2)


# Secondary analyses comparing insomnia vs non-insomnia

#Insomnia ~ wordpairs (h 1)

h1i_a <- ggplot(data = wordpairs) +
  geom_boxplot(mapping = aes(x = `SCI <17`, y = Correct_answers_evening))+
  labs(x = "Probable insomnia", y = "Correct answers (immediate recall)") +
  ggtitle("A")+
  theme_ggeffects()


# Crude model
h1ia_crude <- lm(Correct_answers_evening ~ `SCI <17`, data = wordpairs)

tab_model(h1ia_crude)

# Adjusted model
h1ia_adj <- lm(Correct_answers_evening ~ `SCI <17` + Age_numeric + Sex + Overall_health_numeric + Education_numeric + Dementia_MCI, data = wordpairs)

tab_model(h1ia_adj)

# Insomnia - morning recall

h2i_a <- ggplot(data = wordpairs) +
  geom_boxplot(mapping = aes(x = `SCI <17`, y = Correct_answers_wordpairs_change))+
  labs(x = "Probable insomnia", y = "Correct answers (overnight change)") +
  ggtitle("B")+
  theme_ggeffects()


# Crude model

h2ia_crude <- lm(Correct_answers_wordpairs_change ~ `SCI <17` + Correct_answers_evening, data = wordpairs)
tab_model(h2ia_crude)

# Adjusted model
h2ia_adj <-lm(Correct_answers_wordpairs_change ~ `SCI <17` + Correct_answers_evening + Age_numeric + Sex + Overall_health_numeric + 
               Education_numeric + Dementia_MCI, data = wordpairs)
tab_model(h2ia_adj)

# Insomnia vs not - evening valence

h3i_a <- ggplot(data = image_ratings) +
  geom_boxplot(mapping = aes(x = `SCI <17`, y = Valence_evening_negative))+
  labs(x = "Probable insomnia", y = "Valence evening (negative stimuli)") +
  ggtitle("C")+
  theme_ggeffects()


# Crude model
h3ia_crude <- lm(Valence_evening_negative ~ `SCI <17`, data = image_ratings)

tab_model(h3ia_crude)

# Adjusted model
h3ia_adj <- lm(Valence_evening_negative ~ `SCI <17` + Age_numeric + Sex + Overall_health_numeric + Education_numeric + Dementia_MCI, data = image_ratings)

tab_model(h3ia_adj)

# Sleep quality

h4i_aa <- ggplot(data = image_ratings) +
  geom_boxplot(mapping = aes(x = `SCI <17`, y = Valence_morning_negative))+
  labs(x = "Probable insomnia", y = "Valence morning (negative stimuli)") +
  ggtitle("D")+
  theme_ggeffects()


# Crude model

h4iaa_crude <- lm(Valence_morning_negative ~ `SCI <17`, data = image_ratings)
tab_model(h4iaa_crude)

# Adjusted model
h4iaa_adj <-lm(Valence_morning_negative ~ `SCI <17` + Age_numeric + Sex + Overall_health_numeric + 
                Education_numeric + Dementia_MCI, data = image_ratings)
tab_model(h4iaa_adj)

h4i_ba <- ggplot(data = image_ratings) +
  geom_boxplot(mapping = aes(x = `SCI <17`, y = Valence_change_negative))+
  labs(x = "Probable insomnia", y = "Valence change (negative stimuli)") +
  ggtitle("E")+
  theme_ggeffects()


# Crude model

h4iba_crude <- lm(Valence_change_negative ~ `SCI <17`, data = image_ratings)
tab_model(h4iba_crude)

# Adjusted model
h4iba_adj <-lm(Valence_change_negative ~ `SCI <17` + Age_numeric + Sex + Overall_health_numeric + 
                Education_numeric + Dementia_MCI, data = image_ratings)
tab_model(h4iba_adj)


h5i_aa <- ggplot(data = recall) +
  geom_boxplot(mapping = aes(x = `SCI <17`, y = d_prime_neg))+
  labs(x = "Probable insomnia", y = "Recognition accuracy (d', negative) ") +
  ggtitle("F")+
  theme_ggeffects()


# Crude model

h5iaa_crude <- lm(d_prime_neg ~ `SCI <17`, data = recall)
tab_model(h5iaa_crude)

# Adjusted model
h5iaa_adj <-lm(d_prime_neg ~ `SCI <17` + Age_numeric + Sex + Overall_health_numeric + 
                Education_numeric + Dementia_MCI, data = recall)
tab_model(h5iaa_adj)

grid.arrange(h1i_a, h2i_a, h3i_a, h4i_aa, h4i_ba, h5i_aa, ncol=2)

