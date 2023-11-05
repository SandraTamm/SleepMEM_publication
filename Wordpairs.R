# Cleaning word-pairs

#Set this to the folder where you have the data
setwd("Desktop/Data") # set as applicable


# read all the spreadsheets in (immediate recall)

# Read participants that were downloaded and recruited before 2021-12-28
immediate_recall_1 <- read.csv("Included_participants_211228/data_exp_43283-v26 (long format)/data_exp_43283-v26_task-jed4.csv")

# Read data from 1 additional participant that was not included in the file
immediate_recall_2 <- read.csv("Included_participants_211228/data_exp_43283-v26_4991353/data_exp_43283-v26_task-jed4-4991353.csv")

#Read data from participants collected between 2021-12-28 and XXX
immediate_recall_3 <- read.csv("Data_220520/Long_format/data_exp_43283-v26 (4)/data_exp_43283-v26_task-jed4.csv")

#Read data from participants collected between XXX and 2022-05-20
immediate_recall_4 <- read.csv("Data_220520/Long_format/data_exp_43283-v27/data_exp_43283-v27_task-jed4.csv")

# Combine files
immediate_recall <- rbind(immediate_recall_1, immediate_recall_2, immediate_recall_3, immediate_recall_4)


# All morning assessments have 2 separate lists because of the randomiser

# Read morning data
# Read participants that were downloaded and recruited before 2021-12-28
delayed_recall_1a <- read.csv("Included_participants_211228/data_exp_43283-v26 (long format)/data_exp_43283-v26_task-lckm.csv")
delayed_recall_1b <- read.csv("Included_participants_211228/data_exp_43283-v26 (long format)/data_exp_43283-v26_task-9rx3.csv")

# Read data from 1 additional participant that was not included in the file
#delayed_recall_2a <- read.csv("Included_participants_211228/data_exp_43283-v26_4991353/data_exp_43283-v26_task-lckm-4991353.csv")
#delayed_recall_2b <- read.csv("Included_participants_211228/data_exp_43283-v26_4991353/data_exp_43283-v26_task-9rx3-4991353.csv")

#Read data from participants collected between 2021-12-28 and XXX
delayed_recall_3a <- read.csv("Data_220520/Long_format/data_exp_43283-v26 (4)/data_exp_43283-v26_task-lckm.csv")
delayed_recall_3b <- read.csv("Data_220520/Long_format/data_exp_43283-v26 (4)/data_exp_43283-v26_task-9rx3.csv")

#Read data from participants collected between XXX and 2022-05-20
delayed_recall_4a <- read.csv("Data_220520/Long_format/data_exp_43283-v27/data_exp_43283-v27_task-lckm.csv")
delayed_recall_4b <- read.csv("Data_220520/Long_format/data_exp_43283-v27/data_exp_43283-v27_task-9rx3.csv")


delayed_recall <- rbind(delayed_recall_1a, delayed_recall_1b, delayed_recall_3a, delayed_recall_3b, delayed_recall_4a, delayed_recall_4b)

#loop over unique subject numbers
for(i in 1:length(unique(immediate_recall$Participant.Private.ID))){
  
  # Create a new data frame where you put the new data
  data <- data.frame(matrix(ncol = 4, nrow = 1))
  x <- c("Participant_ID", "Correct_answers_evening", "Correct_answers_morning", "Correct_answers_wordpairs_change")
  colnames(data) <- x
  data$Participant_ID[1] <- unique(immediate_recall$Participant.Private.ID)[[i]]
  
  #create a temporary file with data from just one subject 
  data_temp_immediate <- subset(immediate_recall, immediate_recall$Participant.Private.ID == unique(immediate_recall$Participant.Private.ID)[[i]])
  
  # calculate corect ansvwers for evening  
  data$Correct_answers_evening <- sum(data_temp_immediate$Correct, na.rm = TRUE)
  
  
  #create a temporary file with data from just one subject 

  data_temp_delayed <- subset(delayed_recall, delayed_recall$Participant.Private.ID == unique(immediate_recall$Participant.Private.ID)[[i]])
    
  if(length(data_temp_delayed$Response) == 0){
  }else{
    
    # calculate corect ansvwers for morning  
    data$Correct_answers_morning <- sum(data_temp_delayed$Correct, na.rm = TRUE)
    
    #calculate overnight change
    data$Correct_answers_wordpairs_change <- data$Correct_answers_morning - data$Correct_answers_evening
    
  }

  
  # Comine data for all participants
  if(i == 1){
    data_out <- data
  }else{
    data_out <- rbind(data_out, data)
  }
  
}

summary(data_out$Correct_answers_evening)
summary(data_out$Correct_answers_morning)
summary(data_out$Correct_answers_wordpairs_change)


hist(data_out$Correct_answers_evening)
hist(data_out$Correct_answers_morning)
hist(data_out$Correct_answers_wordpairs_change)

# Set working directory to wherever you want to save the file
setwd("/Users/sandratamm/Desktop/Data")

# save file
write.csv(data_out, file = "wordpairs_data.csv")
save(data_out, file = "wordpairs.RData")
