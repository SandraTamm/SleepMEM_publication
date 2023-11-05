library(plyr)

#valence rating
setwd("~/Desktop/Data") # set as applicable
getwd()

## All morning and evening assessments have 2 separate lists because of the randomiser

# read all the spreadsheets in (evening task)

# Read participants that were downloaded and recruited before 2021-12-28
valence_evening_1a <- read.csv("Included_participants_211228/data_exp_43283-v26 (long format)/data_exp_43283-v26_task-vsrz.csv")
valence_evening_1b <- read.csv("Included_participants_211228/data_exp_43283-v26 (long format)/data_exp_43283-v26_task-eq6n.csv")

# Read data from 1 additional participant that was not included in the file
valence_evening_2b <- read.csv("Included_participants_211228/data_exp_43283-v26_4991353/data_exp_43283-v26_task-eq6n-4991353.csv")

#Read data from participants collected between 2021-12-28 and XXX
valence_evening_3a <- read.csv("Data_220520/Long_format/data_exp_43283-v26 (4)/data_exp_43283-v26_task-vsrz.csv")
valence_evening_3b <- read.csv("Data_220520/Long_format/data_exp_43283-v26 (4)/data_exp_43283-v26_task-eq6n.csv")

#Read data from participants collected between XXX and 2022-05-20
valence_evening_4a <- read.csv("Data_220520/Long_format/data_exp_43283-v27/data_exp_43283-v27_task-vsrz.csv")
valence_evening_4b <- read.csv("Data_220520/Long_format/data_exp_43283-v27/data_exp_43283-v27_task-eq6n.csv")

# Combine files
valence_evening <- rbind.fill(valence_evening_1a, valence_evening_1b, valence_evening_2b, valence_evening_3a, valence_evening_3b, 
                         valence_evening_4a, valence_evening_4b)


# read all the spreadsheets in (morning task)

# Read participants that were downloaded and recruited before 2021-12-28
valence_morning_1a <- read.csv("Included_participants_211228/data_exp_43283-v26 (long format)/data_exp_43283-v26_task-6pxm.csv")
valence_morning_1b <- read.csv("Included_participants_211228/data_exp_43283-v26 (long format)/data_exp_43283-v26_task-l857.csv")


#Read data from participants collected between 2021-12-28 and XXX
valence_morning_3a <- read.csv("Data_220520/Long_format/data_exp_43283-v26 (4)/data_exp_43283-v26_task-6pxm.csv")
valence_morning_3b <- read.csv("Data_220520/Long_format/data_exp_43283-v26 (4)/data_exp_43283-v26_task-l857.csv")

#Read data from participants collected between XXX and 2022-05-20
valence_morning_4a <- read.csv("Data_220520/Long_format/data_exp_43283-v27/data_exp_43283-v27_task-6pxm.csv")
valence_morning_4b <- read.csv("Data_220520/Long_format/data_exp_43283-v27/data_exp_43283-v27_task-l857.csv")

# Combine files
valence_morning <- rbind.fill(valence_morning_1a, valence_morning_1b, valence_morning_3a, valence_morning_3b, 
                              valence_morning_4a, valence_morning_4b)


type_1 <- read.csv("spreadsheet_recall_1_210519.csv")
type_2 <- read.csv("spreadsheet_recall_2_210521.csv")

type_stimuli <- rbind(type_1, type_2)
type_stimuli <- subset(type_stimuli, type_stimuli$randomise_trials == 1)
type_stimuli <- subset(type_stimuli, select = c("Stimuli", "Type"))

valence_morning <- merge(valence_morning, type_stimuli, by = "Stimuli")
valence_morning$Type <- valence_morning$Type.y

# check that correct number of individuals
length(unique(valence_evening$Participant.Private.ID))

# save file for morning assessment
write.csv(valence_morning, file = "morning_image_data.csv")
save(valence_morning, file = "morning_image.RData")


#loop over unique subject numbers
for(i in 1:length(unique(valence_evening$Participant.Private.ID))){
  # Create a new data frame where you put the new data
  data <- data.frame(matrix(ncol = 7, nrow = 1))
  x <- c("Participant_ID", "Valence_evening_negative", "Valence_evening_neutral", 
         "Valence_morning_negative", "Valence_morning_neutral", 
         "Valence_change_negative", "Valence_change_neutral")
  colnames(data) <- x
  data$Participant_ID[1] <- unique(valence_evening$Participant.Private.ID)[[i]]
  
  # create a temporary file with data from just one subject 
  data_temp_valence_evening <- subset(valence_evening, valence_evening$Participant.Private.ID == unique(valence_evening$Participant.Private.ID)[[i]])
  data_temp_valence_morning <- subset(valence_morning, valence_morning$Participant.Private.ID == unique(valence_evening$Participant.Private.ID)[[i]])
  
  data$Valence_evening_negative <- mean(as.numeric(data_temp_valence_evening[(data_temp_valence_evening$Zone.Type == "response_slider_endValue") & 
                                                                               (data_temp_valence_evening$Type == "negative"), "Response"]))
  data$Valence_evening_neutral <- mean(as.numeric(data_temp_valence_evening[(data_temp_valence_evening$Zone.Type == "response_slider_endValue") & 
                                                                              (data_temp_valence_evening$Type == "neutral"), "Response"]))
  #change so only valences of previously presented Stimuli
  data$Valence_morning_negative <- mean(as.numeric(data_temp_valence_morning[(data_temp_valence_morning$Zone.Type == "response_slider_endValue") & 
                                                                               (data_temp_valence_morning$Type == "negative") & 
                                                                               (data_temp_valence_morning$ANSWER == "Yes"), "Response"]))
  data$Valence_morning_neutral <- mean(as.numeric(data_temp_valence_morning[(data_temp_valence_morning$Zone.Type == "response_slider_endValue") & 
                                                                              (data_temp_valence_morning$Type == "neutral") & 
                                                                              (data_temp_valence_morning$ANSWER == "Yes"), "Response"]))
  
  data$Valence_change_negative <- data$Valence_morning_negative - data$Valence_evening_negative
  data$Valence_change_neutral <- data$Valence_morning_neutral - data$Valence_evening_neutral
  
  
  
  # Comine data for all participants
  if(i == 1){
    data_out <- data
  }else{
    data_out <- rbind(data_out, data)
  }
  
}

data_out <- subset(data_out, !is.na(data_out$Participant_ID))

#Check how many participants exited the task early
count(is.na(data_out$Valence_evening_neutral))
count(is.na(data_out$Valence_evening_negative))

# 12 participants exited the task and are removed
data_out <- subset(data_out, !is.na(data_out$Valence_evening_neutral))


hist(data_out$Valence_evening_negative)
hist(data_out$Valence_evening_neutral)

count(is.na(data_out$Valence_morning_neutral))

hist(data_out$Valence_morning_negative)
hist(data_out$Valence_morning_neutral)

hist(data_out$Valence_change_negative)
hist(data_out$Valence_change_neutral)


# Set working directory to wherever you want to save the file
setwd("/Users/sandratamm/Desktop/Data")

# save file
write.csv(data_out, file = "image_ratings_data.csv")
save(data_out, file = "image_ratings.RData")
