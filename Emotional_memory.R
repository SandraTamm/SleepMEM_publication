#Set this to the folder where you have the data
library(dplyr)
library(Rmisc)

setwd("~/Desktop/Data/") # set as applicable

load("~/Desktop/Data/morning_image.RData")

picture_recall <- valence_morning

#loop over unique subject numbers
for(i in 1:length(unique(picture_recall$Participant.Private.ID))){
  # Create a new data frame where you put the new data
  data <- data.frame(matrix(ncol = 8, nrow = 1))
  x <- c("Participant_ID", "Correct_answers_picture", "Correct_answers_picture_negative", "Correct_answers_picture_neutral", "Hit_rate_neg", "Hit_rate_neu",
         "False_alarm_neg", "False_alarm_neu")
  colnames(data) <- x
  data$Participant_ID[1] <- unique(picture_recall$Participant.Private.ID)[[i]]
  
  # create a temporary file with data from just one subject 
  data_temp_picture <- subset(picture_recall, picture_recall$Participant.Private.ID == unique(picture_recall$Participant.Private.ID)[[i]])
  data_temp_picture <- subset(data_temp_picture, data_temp_picture$Response == "Yes" | data_temp_picture$Response == "No")
  data_temp_picture <- unique(data_temp_picture)
  
  # calculate correct answers  
  data$Correct_answers_picture <- sum(data_temp_picture$Correct, na.rm = TRUE)
  
  # create a temporary file with negative data from just one subject 
  data_temp_picture_negative <- subset(data_temp_picture, data_temp_picture$Type == "negative")
  
  # add all correctly identified negative pictures
  data$Correct_answers_picture_negative <- sum(data_temp_picture_negative$Correct, na.rm = TRUE)
  
  # create a temporary file with neutral data from just one subject 
  data_temp_picture_neutral <- subset(data_temp_picture, data_temp_picture$Type == "neutral")
  
  # add all correctly identified neutral pictures
  data$Correct_answers_picture_neutral <- sum(data_temp_picture_neutral$Correct, na.rm = TRUE)
  
  
  # Calculate hit rate
  data$Hit_rate_neg <- (sum(subset(data_temp_picture_negative, Response == "Yes")$Correct, na.rm = TRUE)/15)*100
  data$Hit_rate_neu <- (sum(subset(data_temp_picture_neutral, Response == "Yes")$Correct, na.rm = TRUE)/15)*100
  
  # Calculate false alarms
  data$False_alarm_neg <- (sum(subset(data_temp_picture_negative, Response == "Yes")$Incorrect, na.rm = TRUE)/15)*100
  data$False_alarm_neu <- (sum(subset(data_temp_picture_neutral, Response == "Yes")$Incorrect, na.rm = TRUE)/15)*100
  
  # Comine data for all participants
  if(i == 1){
    data_out <- data
  }else{
    data_out <- rbind(data_out, data)
  }
  
}



# Recognition accuracy (d')

data_out$z_Hit_Rate_neg <- (data_out$Hit_rate_neg - mean(data_out$Hit_rate_neg))/sd(data_out$Hit_rate_neg)
data_out$z_Hit_Rate_neu <- (data_out$Hit_rate_neu - mean(data_out$Hit_rate_neu))/sd(data_out$Hit_rate_neu)


data_out$z_False_alarm_neg <- (data_out$False_alarm_neg - mean(data_out$False_alarm_neg))/sd(data_out$False_alarm_neg)
data_out$z_False_alarm_neu <- (data_out$False_alarm_neu - mean(data_out$False_alarm_neu))/sd(data_out$False_alarm_neu)


data_out$d_prime_neg <- data_out$z_False_alarm_neg - data_out$z_Hit_Rate_neg
data_out$d_prime_neu <- data_out$z_False_alarm_neu - data_out$z_Hit_Rate_neu



# Set working directory to wherever you want to save the file
setwd("/Users/sandratamm/Desktop/Data")

# save file
write.csv(data_out, file = "emotional_memory.csv")
save(data_out, file = "emotional_memory.RData")
