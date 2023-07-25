# this is aimed at extracting all the other variables useful for analysis, mapped to the private ID:

library(plyr)
library(lubridate)


# insomnia, quality of last night, sex, age, overall health, education, cognitive status, depression
setwd("~/Desktop/Data") # put as applicable

# read all the spreadsheets in (demographics)

# Read participants that were downloaded and recruited before 2021-12-28
demographics_1 <- read.csv("Included_participants_211228/data_exp_43283-v26 (long format)/data_exp_43283-v26_questionnaire-kho5.csv")

# Read data from 1 additional participant that was not included in the file
demographics_2 <- read.csv("Included_participants_211228/data_exp_43283-v26_4991353/data_exp_43283-v26_questionnaire-kho5-4991353.csv")

#Read data from participants collected between 2021-12-28 and XXX
demographics_3 <- read.csv("Data_220520/Long_format/data_exp_43283-v26 (4)/data_exp_43283-v26_questionnaire-kho5.csv")

#Remove 1 participant which was already included in the first dataset
demographics_3 <- subset(demographics_3, demographics_3$Participant.Private.ID != 4964917)

#Read data from participants collected between XXX and 2022-05-20
demographics_4 <- read.csv("Data_220520/Long_format/data_exp_43283-v27/data_exp_43283-v27_questionnaire-kho5.csv")

# Combine files
demographics <- rbind(demographics_1, demographics_2, demographics_3, demographics_4)

# Check number of participants (should be 1648?)
length((unique(demographics$Participant.Private.ID)))

# Remove participants with no data (from first batch). Also seems to remove rows where ID is NA
demographics <- subset(demographics, demographics$Participant.Private.ID != 4964839)


# loop over the subjects to extract and code the variables

for(i in 1:length(unique(demographics$Participant.Private.ID))){
  
  # create new data frame
  data <- data.frame(matrix(ncol = 10, nrow = 1))
  x <- c("Participant_ID", "Sex", "Age", "Education", "Overall_health", "Dementia_MCI", "Any_depression_diagnosis", "Current_depression", 
         "Current_antidepressant", "Current_psychological_therapy_depression")
  colnames(data) <- x
  data$Participant_ID[1] <- unique(demographics$Participant.Private.ID)[[i]]
  
  # create temporary file for one subject
  data_temp_demographics <- subset(demographics, demographics$Participant.Private.ID == unique(demographics$Participant.Private.ID)[[i]])
  
  # extract and calculate all the relevant information from all of the variables
  data$Sex <- subset(data_temp_demographics, data_temp_demographics$Question.Key == "response-2")$Response
  data$Age <- subset(data_temp_demographics, data_temp_demographics$Question.Key == "response-3")$Response
  data$Overall_health <- subset(data_temp_demographics, data_temp_demographics$Question.Key == "response-5")$Response
  data$Education <- subset(data_temp_demographics, data_temp_demographics$Question.Key == "response-4")$Response
  data$Dementia_MCI <- subset(data_temp_demographics, data_temp_demographics$Question.Key == "response-6")$Response
  if(length(subset(data_temp_demographics, data_temp_demographics$Question.Key == "response-7")$Response) < 1){
    data$Any_depression_diagnosis <- NA
    data$Current_depression <- NA
    data$Current_antidepressant <- NA
    data$Current_psychological_therapy_depression <- NA
  }else{
    data$Any_depression_diagnosis <- subset(data_temp_demographics, data_temp_demographics$Question.Key == "response-7")$Response
    data$Current_depression <- subset(data_temp_demographics, data_temp_demographics$Question.Key == "response-8")$Response
    data$Current_antidepressant <- subset(data_temp_demographics, data_temp_demographics$Question.Key == "response-9")$Response
    data$Current_psychological_therapy_depression <- subset(data_temp_demographics, data_temp_demographics$Question.Key == "response-10")$Response
  }
    
  # combine data for all participants
  if(i == 1){
    data_out <- data
  }else{
    data_out <- rbind(data_out, data)
  }
  #print(unique(demographics$Participant.Private.ID)[[i]])
  
}



# read all the spreadsheets in (sleep questionnaire)

# Read participants that were downloaded and recruited before 2021-12-28
sleep_questionnaire_1 <- read.csv("Included_participants_211228/data_exp_43283-v26 (long format)/data_exp_43283-v26_questionnaire-6rrc.csv")

# Read data from 1 additional participant that was not included in the file
sleep_questionnaire_2 <- read.csv("Included_participants_211228/data_exp_43283-v26_4991353/data_exp_43283-v26_questionnaire-6rrc-4991353.csv")

#Read data from participants collected between 2021-12-28 and XXX
sleep_questionnaire_3 <- read.csv("Data_220520/Long_format/data_exp_43283-v26 (4)/data_exp_43283-v26_questionnaire-6rrc.csv")

#Remove 1 participant which was already included in the first dataset
sleep_questionnaire_3 <- subset(sleep_questionnaire_3, sleep_questionnaire_3$Participant.Private.ID != 4964917)

#Read data from participants collected between XXX and 2022-05-20
sleep_questionnaire_4 <- read.csv("Data_220520/Long_format/data_exp_43283-v27/data_exp_43283-v27_questionnaire-6rrc.csv")

sleep_questionnaire <- rbind(sleep_questionnaire_1, sleep_questionnaire_2, sleep_questionnaire_3, sleep_questionnaire_4)

# Check how many participants completed
length((unique(sleep_questionnaire$Participant.Private.ID)))


# Create a dataset with just the SCI questions
SCI <- subset(sleep_questionnaire, sleep_questionnaire$Question.Key == "response-1-quantised" | 
                sleep_questionnaire$Question.Key == "response-3-quantised" | 
                sleep_questionnaire$Question.Key == "response-4-quantised" | 
                sleep_questionnaire$Question.Key ==  "response-5-quantised" | 
                sleep_questionnaire$Question.Key == "response-6-quantised" | 
                sleep_questionnaire$Question.Key == "response-8-quantised" | 
                sleep_questionnaire$Question.Key == "response-9-quantised" | 
                sleep_questionnaire$Question.Key == "response-10-quantised")

# Recode the items in line with the original scale
SCI$Recoded_responses <- NA
for(i in 1:length(SCI$Response)){
  if(SCI$Response[i] == 1){
    SCI$Recoded_responses[i] <- 4
  }else if(SCI$Response[i] == 2){
    SCI$Recoded_responses[i] <- 3
  }else if(SCI$Response[i] == 3){
    SCI$Recoded_responses[i] <- 2
  }else if(SCI$Response[i] == 4){
    SCI$Recoded_responses[i] <- 1
  }else if(SCI$Response[i] == 5){
    SCI$Recoded_responses[i] <- 0
  }
}



# loop over the subjects
for(i in 1:length(unique(SCI$Participant.Private.ID))){
  
  # create new data frame
  data <- data.frame(matrix(ncol = 8, nrow = 1))
  x <- c("Participant_ID", "SCI", "Probable_insomnia", "SCI <17", "Typical_bedtime", "Typical_risetime", "Typical_sleep_duration", "Sleep_medication")
  colnames(data) <- x
  data$Participant_ID[1] <- unique(SCI$Participant.Private.ID)[[i]]
  
  # create temporary file for one subject
  data_temp_SCI <- subset(SCI, SCI$Participant.Private.ID == unique(SCI$Participant.Private.ID)[[i]])
  data_temp_sleep_questionnaire <- subset(sleep_questionnaire, sleep_questionnaire$Participant.Private.ID == unique(SCI$Participant.Private.ID)[[i]])
 
  # extract and calculate all the relevant information from all of the variables
  data$SCI <- sum(data_temp_SCI$Recoded_responses)
  
  # Characterise as probable insomnia or not. DOUBLE-CHECK!!!
  if(
    subset(data_temp_SCI, data_temp_SCI$Question.Key == "response-1-quantised")$Recoded_responses < 3 |
    subset(data_temp_SCI, data_temp_SCI$Question.Key == "response-3-quantised")$Recoded_responses < 3 &
    subset(data_temp_SCI, data_temp_SCI$Question.Key == "response-4-quantised")$Recoded_responses < 3 &
    subset(data_temp_SCI, data_temp_SCI$Question.Key == "response-5-quantised")$Recoded_responses < 3 &
    subset(data_temp_SCI, data_temp_SCI$Question.Key == "response-6-quantised")$Recoded_responses < 3 &
    subset(data_temp_SCI, data_temp_SCI$Question.Key == "response-8-quantised")$Recoded_responses < 3 &
    subset(data_temp_SCI, data_temp_SCI$Question.Key == "response-9-quantised")$Recoded_responses < 3 &
    subset(data_temp_SCI, data_temp_SCI$Question.Key == "response-10-quantised")$Recoded_responses < 3 
  ){
    probable_insomnia <- "yes"
  }else{
    probable_insomnia <- "no"
  }
  
  if(
    data$SCI <= 16
  ){
    data$`SCI <17` <- "yes"
  }else{
    data$`SCI <17` <- "no"
  }
  
  data$Probable_insomnia <- probable_insomnia
  
  # Save as time format and add minutes
  data$Typical_bedtime <- paste(as.character(subset(data_temp_sleep_questionnaire, data_temp_sleep_questionnaire$Question.Key == "response-12-hour")$Response), ":", 
                                as.character(subset(data_temp_sleep_questionnaire, data_temp_sleep_questionnaire$Question.Key == "response-12-minute")$Response), sep = "")
  data$Typical_risetime <- paste(as.character(subset(data_temp_sleep_questionnaire, data_temp_sleep_questionnaire$Question.Key == "response-13-hour")$Response), ":", 
                                 as.character(subset(data_temp_sleep_questionnaire, data_temp_sleep_questionnaire$Question.Key == "response-13-minute")$Response), sep = "")
  data$Typical_sleep_duration <- as.numeric(subset(data_temp_sleep_questionnaire, data_temp_sleep_questionnaire$Question.Key == "response-14-hour")$Response)*60 +
                                       as.numeric(subset(data_temp_sleep_questionnaire, data_temp_sleep_questionnaire$Question.Key == "response-14-minute")$Response)
  
  data$Sleep_medication <- subset(data_temp_sleep_questionnaire, data_temp_sleep_questionnaire$Question.Key == "response-15")$Response
  
  
  # combine data for all participants
  if(i == 1){
    data_sleep_out <- data
  }else{
    data_sleep_out <- rbind(data_sleep_out, data)
  }
  #print(unique(SCI$Participant.Private.ID)[[i]])
  
}


# Read evening KSS data
# Read participants that were downloaded and recruited before 2021-12-28
KSS_1 <- read.csv("Included_participants_211228/data_exp_43283-v26 (long format)/data_exp_43283-v26_questionnaire-te1y.csv")

# Read data from 1 additional participant that was not included in the file
KSS_2 <- read.csv("Included_participants_211228/data_exp_43283-v26_4991353/data_exp_43283-v26_questionnaire-te1y-4991353.csv")

#Read data from participants collected between 2021-12-28 and XXX
KSS_3 <- read.csv("Data_220520/Long_format/data_exp_43283-v26 (4)/data_exp_43283-v26_questionnaire-te1y.csv")

#Participant 4964917 did not complete

#Read data from participants collected between XXX and 2022-05-20
KSS_4 <- read.csv("Data_220520/Long_format/data_exp_43283-v27/data_exp_43283-v27_questionnaire-te1y.csv")

# Combine files
KSS <- rbind(KSS_1, KSS_2, KSS_3, KSS_4)


# Check how many participants completed
length((unique(KSS$Participant.Private.ID)))


# Read morning KSS data
# Read participants that were downloaded and recruited before 2021-12-28
KSS_morning_1a <- read.csv("Included_participants_211228/data_exp_43283-v26 (long format)/data_exp_43283-v26_questionnaire-3fd3.csv")
KSS_morning_1b <- read.csv("Included_participants_211228/data_exp_43283-v26 (long format)/data_exp_43283-v26_questionnaire-f75v.csv")

# Read data from 1 additional participant that was not included in the file
KSS_morning_2a <- read.csv("Included_participants_211228/data_exp_43283-v26_4991353/data_exp_43283-v26_questionnaire-3fd3-4991353.csv")
KSS_morning_2b <- read.csv("Included_participants_211228/data_exp_43283-v26_4991353/data_exp_43283-v26_questionnaire-f75v-4991353.csv")

#Read data from participants collected between 2021-12-28 and XXX
KSS_morning_3a <- read.csv("Data_220520/Long_format/data_exp_43283-v26 (4)/data_exp_43283-v26_questionnaire-3fd3.csv")
KSS_morning_3b <- read.csv("Data_220520/Long_format/data_exp_43283-v26 (4)/data_exp_43283-v26_questionnaire-f75v.csv")

#Participant 4964917 did not complete 

#Read data from participants collected between XXX and 2022-05-20
KSS_morning_4a <- read.csv("Data_220520/Long_format/data_exp_43283-v27/data_exp_43283-v27_questionnaire-3fd3.csv")
KSS_morning_4b <- read.csv("Data_220520/Long_format/data_exp_43283-v27/data_exp_43283-v27_questionnaire-f75v.csv")

# Combine files
KSS_morning <- rbind(KSS_morning_1a, KSS_morning_2a, KSS_morning_3a, KSS_morning_4a, KSS_morning_1b, KSS_morning_2b, KSS_morning_3b, KSS_morning_4b)


# Check how many participants completed
length((unique(KSS_morning$Participant.Private.ID)))


KSS <- subset(KSS, !is.na(KSS$Participant.Private.ID))

# loop over the subjects to extract and code the variables

for(i in 1:length(unique(KSS$Participant.Private.ID))){
  
  # create new data frame
  data <- data.frame(matrix(ncol = 3, nrow = 1))
  x <- c("Participant_ID", "Evening_KSS", "Morning_KSS")
  colnames(data) <- x
  data$Participant_ID[1] <- unique(KSS$Participant.Private.ID)[[i]]
  
  # create temporary files for one subject
  data_temp_KSS <- subset(KSS, KSS$Participant.Private.ID == unique(KSS$Participant.Private.ID)[[i]])
  data_temp_KSS_morning <- subset(KSS_morning, KSS_morning$Participant.Private.ID == unique(KSS$Participant.Private.ID)[[i]])
  
  # extract and calculate all the relevant information from all of the variables
  data$Evening_KSS <- subset(data_temp_KSS, data_temp_KSS$Question.Key == "response-1-quantised")$Response
  
  
  if(length(subset(data_temp_KSS_morning, data_temp_KSS_morning$Question.Key == "response-1-quantised")$Response) < 1){
    data$Morning_KSS <- NA
  }else{
    data$Morning_KSS <- subset(data_temp_KSS_morning, data_temp_KSS_morning$Question.Key == "response-1-quantised")$Response
  }
  
  # combine data for all participants
  if(i == 1){
    data_out_KSS <- data
  }else{
    data_out_KSS <- rbind(data_out_KSS, data)
  }
  #print(unique(KSS$Participant.Private.ID)[[i]])
  
}


# Read evening Mood data
# Read participants that were downloaded and recruited before 2021-12-28
PANAS_1 <- read.csv("Included_participants_211228/data_exp_43283-v26 (long format)/data_exp_43283-v26_questionnaire-o8kr.csv")

# Read data from 1 additional participant that was not included in the file
PANAS_2 <- read.csv("Included_participants_211228/data_exp_43283-v26_4991353/data_exp_43283-v26_questionnaire-o8kr-4991353.csv")

#Read data from participants collected between 2021-12-28 and XXX
PANAS_3 <- read.csv("Data_220520/Long_format/data_exp_43283-v26 (4)/data_exp_43283-v26_questionnaire-o8kr.csv")

#Participant 4964917 did not complete

#Read data from participants collected between XXX and 2022-05-20
PANAS_4 <- read.csv("Data_220520/Long_format/data_exp_43283-v27/data_exp_43283-v27_questionnaire-o8kr.csv")

# Combine files
PANAS <- rbind(PANAS_1, PANAS_2, PANAS_3, PANAS_4)

# Remove rowns with NA (ID)
PANAS <- subset(PANAS, !is.na(PANAS$Participant.Private.ID))

# Remove 
# 6224566
# 6218377
# 6207510
# These ps started the questionnaire but did not complete

PANAS <- subset(PANAS, PANAS$Participant.Private.ID != 6224566)
PANAS <- subset(PANAS, PANAS$Participant.Private.ID != 6218377)
PANAS <- subset(PANAS, PANAS$Participant.Private.ID != 6207510)



# Check how many participants completed
length((unique(PANAS$Participant.Private.ID)))


# Read morning mood data
# Read participants that were downloaded and recruited before 2021-12-28
PANAS_morning_1a <- read.csv("Included_participants_211228/data_exp_43283-v26 (long format)/data_exp_43283-v26_questionnaire-4vt5.csv")
PANAS_morning_1b <- read.csv("Included_participants_211228/data_exp_43283-v26 (long format)/data_exp_43283-v26_questionnaire-xvxg.csv")

# Read data from 1 additional participant that was not included in the file
PANAS_morning_2a <- read.csv("Included_participants_211228/data_exp_43283-v26_4991353/data_exp_43283-v26_questionnaire-4vt5-4991353.csv")
PANAS_morning_2b <- read.csv("Included_participants_211228/data_exp_43283-v26_4991353/data_exp_43283-v26_questionnaire-xvxg-4991353.csv")

#Read data from participants collected between 2021-12-28 and XXX
PANAS_morning_3a <- read.csv("Data_220520/Long_format/data_exp_43283-v26 (4)/data_exp_43283-v26_questionnaire-4vt5.csv")
PANAS_morning_3b <- read.csv("Data_220520/Long_format/data_exp_43283-v26 (4)/data_exp_43283-v26_questionnaire-xvxg.csv")

#Participant 4964917 did not complete 

#Read data from participants collected between XXX and 2022-05-20
PANAS_morning_4a <- read.csv("Data_220520/Long_format/data_exp_43283-v27/data_exp_43283-v27_questionnaire-4vt5.csv")
PANAS_morning_4b <- read.csv("Data_220520/Long_format/data_exp_43283-v27/data_exp_43283-v27_questionnaire-xvxg.csv")

# Combine files
PANAS_morning <- rbind(PANAS_morning_1a, PANAS_morning_2a, PANAS_morning_3a, PANAS_morning_4a, PANAS_morning_1b, PANAS_morning_2b, PANAS_morning_3b, PANAS_morning_4b)



# Check how many participants completed
length((unique(PANAS_morning$Participant.Private.ID)))


# loop over the subjects to extract and code the variables

for(i in 1:length(unique(PANAS$Participant.Private.ID))){
  
  # create new data frame
  data <- data.frame(matrix(ncol = 5, nrow = 1))
  x <- c("Participant_ID", "Evening_PANAS_pos", "Evening_PANAS_neg", "Morning_PANAS_pos", "Morning_PANAS_neg")
  colnames(data) <- x
  data$Participant_ID[1] <- unique(PANAS$Participant.Private.ID)[[i]]
  
  # create temporary files for one subject
  data_temp_PANAS <- subset(PANAS, PANAS$Participant.Private.ID == unique(PANAS$Participant.Private.ID)[[i]])
  data_temp_PANAS_morning <- subset(PANAS_morning, PANAS_morning$Participant.Private.ID == unique(PANAS$Participant.Private.ID)[[i]])
  
  # extract and calculate all the relevant information from all of the variables
  # Positive affect evening
  data_evening_pos <- subset(data_temp_PANAS, data_temp_PANAS$Question.Key == "response-2-quantised" |
                               data_temp_PANAS$Question.Key == "response-4-quantised" |
                               data_temp_PANAS$Question.Key == "response-7-quantised" |
                               data_temp_PANAS$Question.Key == "response-16-quantised" |
                               data_temp_PANAS$Question.Key == "response-15-quantised" |
                               data_temp_PANAS$Question.Key == "response-13-quantised" |
                               data_temp_PANAS$Question.Key == "response-11-quantised" |
                               data_temp_PANAS$Question.Key == "response-9-quantised" |
                               data_temp_PANAS$Question.Key == "response-22-quantised" |
                               data_temp_PANAS$Question.Key == "response-20-quantised")
  
  
  data$Evening_PANAS_pos <- sum(as.numeric(data_evening_pos$Response))
  
  # Negative affect evening
  data_evening_neg <- subset(data_temp_PANAS, data_temp_PANAS$Question.Key == "response-3-quantised" |
                               data_temp_PANAS$Question.Key == "response-8-quantised" |
                               data_temp_PANAS$Question.Key == "response-6-quantised" |
                               data_temp_PANAS$Question.Key == "response-5-quantised" |
                               data_temp_PANAS$Question.Key == "response-17-quantised" |
                               data_temp_PANAS$Question.Key == "response-14-quantised" |
                               data_temp_PANAS$Question.Key == "response-12-quantised" |
                               data_temp_PANAS$Question.Key == "response-10-quantised" |
                               data_temp_PANAS$Question.Key == "response-21-quantised" |
                               data_temp_PANAS$Question.Key == "response-19-quantised")
  
  
  data$Evening_PANAS_neg <- sum(as.numeric(data_evening_neg$Response))
  
  
  if(length(subset(data_temp_PANAS_morning, data_temp_PANAS_morning$Question.Key == "response-2-quantised")$Response) < 1){
    data$Morning_PANAS_pos <- NA
    data$Morning_PANAS_neg <- NA
  }else{
    # Positive affect evening
    data_morning_pos <- subset(data_temp_PANAS_morning, data_temp_PANAS_morning$Question.Key == "response-2-quantised" |
                                 data_temp_PANAS_morning$Question.Key == "response-4-quantised" |
                                 data_temp_PANAS_morning$Question.Key == "response-7-quantised" |
                                 data_temp_PANAS_morning$Question.Key == "response-16-quantised" |
                                 data_temp_PANAS_morning$Question.Key == "response-15-quantised" |
                                 data_temp_PANAS_morning$Question.Key == "response-13-quantised" |
                                 data_temp_PANAS_morning$Question.Key == "response-11-quantised" |
                                 data_temp_PANAS_morning$Question.Key == "response-9-quantised" |
                                 data_temp_PANAS_morning$Question.Key == "response-22-quantised" |
                                 data_temp_PANAS_morning$Question.Key == "response-20-quantised")
    
    
    data$Morning_PANAS_pos <- sum(as.numeric(data_morning_pos$Response))
    
    # Negative affect evening
    data_morning_neg <- subset(data_temp_PANAS_morning, data_temp_PANAS_morning$Question.Key == "response-3-quantised" |
                                 data_temp_PANAS_morning$Question.Key == "response-8-quantised" |
                                 data_temp_PANAS_morning$Question.Key == "response-6-quantised" |
                                 data_temp_PANAS_morning$Question.Key == "response-5-quantised" |
                                 data_temp_PANAS_morning$Question.Key == "response-17-quantised" |
                                 data_temp_PANAS_morning$Question.Key == "response-14-quantised" |
                                 data_temp_PANAS_morning$Question.Key == "response-12-quantised" |
                                 data_temp_PANAS_morning$Question.Key == "response-10-quantised" |
                                 data_temp_PANAS_morning$Question.Key == "response-21-quantised" |
                                 data_temp_PANAS_morning$Question.Key == "response-19-quantised")
    
    
    data$Morning_PANAS_neg <- sum(as.numeric(data_morning_neg$Response))
    
  }
  
  # combine data for all participants
  if(i == 1){
    data_out_PANAS <- data
  }else{
    data_out_PANAS <- rbind(data_out_PANAS, data)
  }
  #print(unique(PANAS$Participant.Private.ID)[[i]])
  
}


# Read morning sleep data
# Read participants that were downloaded and recruited before 2021-12-28
last_night_sleep_1a <- read.csv("Included_participants_211228/data_exp_43283-v26 (long format)/data_exp_43283-v26_questionnaire-4zzr.csv")
last_night_sleep_1b <- read.csv("Included_participants_211228/data_exp_43283-v26 (long format)/data_exp_43283-v26_questionnaire-6zuu.csv")

# Read data from 1 additional participant that was not included in the file
last_night_sleep_2a <- read.csv("Included_participants_211228/data_exp_43283-v26_4991353/data_exp_43283-v26_questionnaire-4zzr-4991353.csv")
last_night_sleep_2b <- read.csv("Included_participants_211228/data_exp_43283-v26_4991353/data_exp_43283-v26_questionnaire-6zuu-4991353.csv")

#Read data from participants collected between 2021-12-28 and XXX
last_night_sleep_3a <- read.csv("Data_220520/Long_format/data_exp_43283-v26 (4)/data_exp_43283-v26_questionnaire-4zzr.csv")
last_night_sleep_3b <- read.csv("Data_220520/Long_format/data_exp_43283-v26 (4)/data_exp_43283-v26_questionnaire-6zuu.csv")

#Participant 4964917 did not complete 

#Read data from participants collected between XXX and 2022-05-20
last_night_sleep_4a <- read.csv("Data_220520/Long_format/data_exp_43283-v27/data_exp_43283-v27_questionnaire-4zzr.csv")
last_night_sleep_4b <- read.csv("Data_220520/Long_format/data_exp_43283-v27/data_exp_43283-v27_questionnaire-6zuu.csv")

# Combine files
last_night_sleep <- rbind(last_night_sleep_1a, last_night_sleep_2a, last_night_sleep_3a, last_night_sleep_4a, last_night_sleep_1b, last_night_sleep_2b, last_night_sleep_3b, last_night_sleep_4b)


# Check how many participants completed
length((unique(last_night_sleep$Participant.Private.ID)))



# loop over the subjects
for(i in 1:length(unique(last_night_sleep$Participant.Private.ID))){
  
  # create new data frame
  data <- data.frame(matrix(ncol = 5, nrow = 1))
  x <- c("Participant_ID", "Bedtime", "Risetime", "Sleep_quality_last_night", "Sleep_duration_last_night")
  colnames(data) <- x
  data$Participant_ID[1] <- unique(last_night_sleep$Participant.Private.ID)[[i]]
  
  # create temporary file for one subject
  data_temp_last_night_sleep <- subset(last_night_sleep, last_night_sleep$Participant.Private.ID == unique(last_night_sleep$Participant.Private.ID)[[i]])
  
  # Fix participants with no responses
  if(length(data_temp_last_night_sleep$Response) < 2){
    data$Bedtime <- NA
    data$Risetime <- NA
    data$Sleep_quality_last_night <- NA
    data$Sleep_duration_last_night <- NA   
  }else{
    # Save as time format and add minutes. Need to check particiants who esponded using am/pm
    data$Bedtime <- paste(as.character(subset(data_temp_last_night_sleep, data_temp_last_night_sleep$Question.Key == "response-3-hour")$Response), ":", 
                          as.character(subset(data_temp_last_night_sleep, data_temp_last_night_sleep$Question.Key == "response-3-minute")$Response), sep = "")
    data$Risetime <- paste(as.character(subset(data_temp_last_night_sleep, data_temp_last_night_sleep$Question.Key == "response-4-hour")$Response), ":", 
                           as.character(subset(data_temp_last_night_sleep, data_temp_last_night_sleep$Question.Key == "response-4-minute")$Response), sep = "")
    
    data$Sleep_quality_last_night <- subset(data_temp_last_night_sleep, data_temp_last_night_sleep$Question.Key == "response-2")$Response
    data$Sleep_duration_last_night <- as.numeric(subset(data_temp_last_night_sleep, data_temp_last_night_sleep$Question.Key == "response-5-hour")$Response)*60 +
      as.numeric(subset(data_temp_last_night_sleep, data_temp_last_night_sleep$Question.Key == "response-5-minute")$Response)
  }
  
  # extract and calculate all the relevant information from all of the variables
  
  
  # combine data for all participants
  if(i == 1){
    data_sleep_last_night_out <- data
  }else{
    data_sleep_last_night_out <- rbind(data_sleep_last_night_out, data)
  }
  print(unique(last_night_sleep$Participant.Private.ID)[[i]])
  
}


# Combine different types of data
All_data <- merge(data_out, data_sleep_out, all.x = T)
All_data <- merge(All_data, data_out_KSS, all.x = T)
All_data <- merge(All_data, data_out_PANAS, all.x = T)
All_data <- merge(All_data, data_sleep_last_night_out, all.x = T)


# Convert to reasonable factors and plot data
All_data$Sex <- as.factor(All_data$Sex)
plot(All_data$Sex)
All_data$Age <- as.factor(All_data$Age)
All_data$Age <- ordered(All_data$Age, levels = c("18-24", "25-30", "31-40", "41-50", "51-60", "61-70", "71-80", ">80"))
plot(All_data$Age)
All_data$Education <- as.factor(All_data$Education)
All_data$Education <- ordered(All_data$Education, levels = c("Prefer not to answer", "None", "GCSE or equivalent", "A-levels or equivalent", "University Undergraduate", "University Postgraduate"))
plot(All_data$Education)
All_data$Overall_health <- as.factor(All_data$Overall_health)
All_data$Overall_health <- ordered(All_data$Overall_health, levels = c("Prefer not to answer", "Do not know", "Poor", "Fair", "Good", "Excellent"))
plot(All_data$Overall_health)
All_data$Dementia_MCI <- as.factor(All_data$Dementia_MCI)
plot(All_data$Dementia_MCI)
All_data$Any_depression_diagnosis <- as.factor(All_data$Any_depression_diagnosis)
plot(All_data$Any_depression_diagnosis)
All_data$Current_depression <- as.factor(All_data$Current_depression)
plot(All_data$Current_depression)
All_data$Current_antidepressant <- as.factor(All_data$Current_antidepressant)
plot(All_data$Current_antidepressant)
All_data$Current_psychological_therapy_depression <- as.factor(All_data$Current_psychological_therapy_depression)
plot(All_data$Current_psychological_therapy_depression)

hist(All_data$SCI)
All_data$Probable_insomnia <- as.factor(All_data$Probable_insomnia)
plot(All_data$Probable_insomnia)

All_data$`SCI <17` <- as.factor(All_data$`SCI <17`)
plot(All_data$`SCI <17`)

All_data$Typical_bedtime <- strptime(All_data$Typical_bedtime, format = "%H:%M")
All_data$Typical_bedtime_numeric = hour(All_data$Typical_bedtime) + minute(All_data$Typical_bedtime)/60 + second(All_data$Typical_bedtime)/3600
hist((All_data$Typical_bedtime_numeric), breaks = 24)




# Assuming that participants answered using 12 h clock, changing everyone answering after 05:00 and before 15:00 to 12 h later
All_data$Typical_bedtime_numeric_corrected <- All_data$Typical_bedtime_numeric

for(i in 1:length(All_data$Typical_bedtime_numeric_corrected)){
  if(is.na(All_data$Typical_bedtime_numeric[[i]])){
  }else{
    if(All_data$Typical_bedtime_numeric[[i]] > 5 & All_data$Typical_bedtime_numeric[[i]] < 15){
      print(i)
      All_data$Typical_bedtime_numeric_corrected[i] <- All_data$Typical_bedtime_numeric[i] + 12
    }
  }
  if(is.na(All_data$Typical_bedtime_numeric_corrected[[i]])){
  }else if(All_data$Typical_bedtime_numeric_corrected[i] >= 24){
    All_data$Typical_bedtime_numeric_corrected[i] <- All_data$Typical_bedtime_numeric_corrected[i] - 24
  }
}


hist((All_data$Typical_bedtime_numeric_corrected), breaks = 24)



All_data$Typical_risetime <- strptime(All_data$Typical_risetime, format = "%H:%M")
All_data$Typical_risetime_numeric = hour(All_data$Typical_risetime) + minute(All_data$Typical_risetime)/60 + second(All_data$Typical_risetime)/3600
hist((All_data$Typical_risetime_numeric), breaks = 24)


# Inspect participants with risetime > 14 
test <- subset(All_data, All_data$Typical_risetime_numeric > 14)

# Correct participants responding with incorrect 24 h clock

All_data$Typical_risetime_numeric_corrected <- All_data$Typical_risetime_numeric

for(i in 1:length(All_data$Typical_risetime_numeric_corrected)){
  if(is.na(All_data$Typical_risetime_numeric[[i]])){
  }else{
    if(All_data$Typical_risetime_numeric[[i]] > 13){
      print(i)
      All_data$Typical_risetime_numeric_corrected[i] <- All_data$Typical_risetime_numeric[i] - 12
    }
  }
}
hist((All_data$Typical_risetime_numeric_corrected), breaks = 24)


# Typical Sleep efficiency and TIB

All_data$Typical_sleep_before_midnight <- 24-All_data$Typical_bedtime_numeric_corrected
for(i in 1:length(All_data$Typical_sleep_before_midnight)){
  if(is.na(All_data$Typical_sleep_before_midnight[[i]])){
  }else if(All_data$Typical_sleep_before_midnight[i] > 15){
    All_data$Typical_sleep_before_midnight[i] <- All_data$Typical_sleep_before_midnight[i] - 24
  }
}

All_data$Typical_TIB <- All_data$Typical_sleep_before_midnight + All_data$Typical_risetime_numeric_corrected
hist(All_data$Typical_TIB)

# Inspect data with TIB < 0 h
test <- subset(All_data, All_data$Typical_TIB < 4)

# All participants with negative TIB: TIB, risetime and bedtime is set as NA

for(i in 1:length(All_data$Typical_TIB)){
  if(is.na(All_data$Typical_TIB[[i]])){
    
  }else if(All_data$Typical_TIB[[i]] < 4){
    print(i)
    All_data$Typical_TIB[[i]] <- NA
    All_data$Typical_bedtime_numeric_corrected[[i]] <- NA
    All_data$Typical_risetime_numeric_corrected[[i]] <- NA
    print(All_data$Participant_ID[i])
  }else{
    
  }
}

plot(All_data$Typical_TIB ~ All_data$Typical_sleep_duration)

# Remove participants with very high sleep duration (and shorter TIB)
for(i in 1:length(All_data$Typical_sleep_duration)){
  if(is.na(All_data$Typical_sleep_duration[[i]])){
    
  }else if(All_data$Typical_sleep_duration[[i]] > 800){
    print(i)
    All_data$Typical_sleep_duration[[i]] <- NA
    All_data$Typical_TIB[[i]] <- NA
    All_data$Typical_bedtime_numeric_corrected[[i]] <- NA
    All_data$Typical_risetime_numeric_corrected[[i]] <- NA
    print(All_data$Participant_ID[i])
  }else{
    
  }
}

plot(All_data$Typical_TIB ~ All_data$Typical_sleep_duration)

# Remove participants with very short typical sleep duration (shorter than 10 min)
All_data$Typical_sleep_duration[All_data$Typical_sleep_duration < 10] <- NA


All_data$Typical_sleep_efficiency <- All_data$Typical_sleep_duration/(All_data$Typical_TIB*60)*100
hist(All_data$Typical_sleep_efficiency)

# Inspect participants with SE > 100
test <- subset(All_data, All_data$Typical_sleep_efficiency > 100)
# These 23 ps. are probably not "wrong" values


# Change values to NA for SE > 100
All_data$Typical_sleep_efficiency[All_data$Typical_sleep_efficiency > 100] <- NA
hist(All_data$Typical_sleep_efficiency)


hist(All_data$Typical_sleep_duration)

All_data$Sleep_medication <- as.factor(All_data$Sleep_medication)
All_data$Sleep_medication <- ordered(All_data$Sleep_medication, levels = c("Prefer not to answer", "Not during the past month", "Less than once a week", 
                                                                           "Once or twice a week", "Three or more times a week"))
plot(All_data$Sleep_medication)

All_data$Evening_KSS <- as.numeric(All_data$Evening_KSS)
hist(All_data$Evening_KSS)
All_data$Morning_KSS <- as.numeric(All_data$Morning_KSS)
hist(All_data$Morning_KSS)

#TODO: make the same x
hist(All_data$Evening_PANAS_neg, xlim = range(0,10,20,30,40,50))
hist(All_data$Morning_PANAS_neg, xlim = range(0,10,20,30,40,50))
hist(All_data$Evening_PANAS_pos, xlim = range(0,10,20,30,40,50))
hist(All_data$Morning_PANAS_pos, xlim = range(0,10,20,30,40,50))



# Bedtime study night
All_data$Bedtime <- strptime(All_data$Bedtime, format = "%H:%M")
All_data$Bedtime_numeric = hour(All_data$Bedtime) + minute(All_data$Bedtime)/60 + second(All_data$Bedtime)/3600
hist((All_data$Bedtime_numeric), breaks = 24)

# Assuming that participants answered using 12 h clock, changing everyone answering after 05:00 and before 13:00 to 12 h later
All_data$Bedtime_numeric_corrected <- All_data$Bedtime_numeric

for(i in 1:length(All_data$Bedtime_numeric_corrected)){
  if(is.na(All_data$Bedtime_numeric[[i]])){
  }else{
    if(All_data$Bedtime_numeric[[i]] > 5 & All_data$Bedtime_numeric[[i]] < 15){
      print(i)
      All_data$Bedtime_numeric_corrected[i] <- All_data$Bedtime_numeric[i] + 12
    }
    if(is.na(All_data$Bedtime_numeric_corrected[[i]])){
    }else if(All_data$Bedtime_numeric_corrected[i] >= 24){
      All_data$Bedtime_numeric_corrected[i] <- All_data$Bedtime_numeric_corrected[i] - 24
    }
  }
}

hist(All_data$Bedtime_numeric_corrected)
summary(All_data$Bedtime_numeric_corrected)

All_data$Risetime <- strptime(All_data$Risetime, format = "%H:%M")
All_data$Risetime_numeric = hour(All_data$Risetime) + minute(All_data$Risetime)/60 + second(All_data$Risetime)/3600
hist((All_data$Risetime_numeric), breaks = 24)

# Correct 12 h clock
All_data$Risetime_numeric_corrected <- All_data$Risetime_numeric

for(i in 1:length(All_data$Risetime_numeric_corrected)){
  if(is.na(All_data$Risetime_numeric[[i]])){
  }else{
    if(All_data$Risetime_numeric[[i]] > 15){
      print(i)
      All_data$Risetime_numeric_corrected[i] <- All_data$Risetime_numeric[i] - 12
    }
  }
}
hist((All_data$Risetime_numeric_corrected), breaks = 24)

# Change very low values to NA
All_data$Risetime_numeric_corrected[All_data$Risetime_numeric_corrected <3] <- NA


All_data$Sleep_quality_last_night <- as.factor(All_data$Sleep_quality_last_night)
All_data$Sleep_quality_last_night <- ordered(All_data$Sleep_quality_last_night, levels = c("Very poor", "Poor", 
                                                                                           "Fair", "Good", "Very good"))
plot(All_data$Sleep_quality_last_night)

hist(All_data$Sleep_duration_last_night)


# Sleep efficiency for experimental night
All_data$Sleep_before_midnight <- 24-All_data$Bedtime_numeric_corrected
for(i in 1:length(All_data$Sleep_before_midnight)){
  if(is.na(All_data$Sleep_before_midnight[[i]])){
  }else if(All_data$Sleep_before_midnight[i] > 15){
    All_data$Sleep_before_midnight[i] <- All_data$Sleep_before_midnight[i] - 24
  }
}
hist(All_data$Sleep_before_midnight)

All_data$TIB <- All_data$Sleep_before_midnight + All_data$Risetime_numeric_corrected
hist(All_data$TIB)

test <- subset(All_data, All_data$TIB > 14)

All_data$Sleep_efficiency <- All_data$Sleep_duration/(All_data$TIB*60)*100
hist(All_data$Sleep_efficiency)

# Change participants with SE > 100 to NA
test <- subset(All_data, All_data$Sleep_efficiency > 100)
All_data$Sleep_efficiency[All_data$Sleep_efficiency > 100] <- NA
hist(All_data$Sleep_efficiency)

# Add columns with ordinal values as numeric
All_data$Sleep_quality_last_night_numeric <- as.numeric(All_data$Sleep_quality_last_night)
All_data$Age_numeric <- as.numeric(All_data$Age)
All_data$Overall_health_numeric <- as.numeric(All_data$Overall_health)
All_data$Education_numeric <- as.numeric(All_data$Education)


# save the information in a file
setwd("/Users/sandratamm/Desktop/Data")
write.csv(All_data, file = "demographics_sleep_mood.csv")
save(All_data, file = "demographics_sleep_mood.RData")
