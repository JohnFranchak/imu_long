collapse_classes <- function(outcome, ds) {

  if (outcome == "restraint") {
    #RESTRAINT =  #HELD, RESTRAINED, INDEPENDENT
    ds$classe[ds$classe %in% c(2,3,4,7,10)] <- 1 #Classify all upright together
    ds$classe[ds$classe == 6 | ds$classe == 8] <- 5 #Classify all holding together
    ds$classe <- factor(ds$classe, levels = c(1,5,9), labels = c("indp","held","restrained"))
  } else if (outcome == "activity") {
    #ACTIVITY = SELF MOVING VS PARENT MOVING VS STATIONARY
    ds$classe[ds$classe %in% c(1,3,6,7,8,9,10)] <- 1 #Classify all stationary
    ds$classe[ds$classe %in% c(2,4)] <- 2 #Classify all moving
    ds$classe <- factor(ds$classe, levels = c(1,2,5), labels = c("stationary","self moving","held moving"))
  } else if (outcome == "posture") {
    #POSTURE
    ds$classe[ds$classe == 2] <- 1 #Classify all upright together
    ds$classe[ds$classe == 4] <- 3 #Classify all prone together
    ds$classe[ds$classe == 9 | ds$classe == 8] <- 7 #Classify all sitting together
    ds$classe[ds$classe == 6] <- 5 #Classify all holding together
    ds$classe <- factor(ds$classe,levels = c(10,3,7,1,5),labels = c("Supine","Prone","Sitting","Upright","Held"))
  } else if (outcome == "posture_loc") {
    #POSTURE
    #ds$classe[ds$classe == 2] <- 1 #Classify all upright together
    #ds$classe[ds$classe == 4] <- 3 #Classify all prone together
    ds$classe[ds$classe == 9 | ds$classe == 8] <- 7 #Classify all sitting together
    ds$classe[ds$classe == 6] <- 5 #Classify all holding together
    ds$classe <- factor(ds$classe,levels = c(10,3,7,1,5,2,4),labels = c("Supine","Crawling","Sitting","Walking","Held","Upright","Prone"))
  } else if (outcome == "sitting") {
    #BINARY SITTING VS NOT
    ds$classe[ds$classe %in% c(1,2,3,4,5,6,10)] <- 12 #Classify all non sitting
    ds$classe[ds$classe %in% c(7,8,9)] <- 11 #Classify all sitting
    ds$classe <- factor(ds$classe, levels = c(11,12), labels = c("sitting","not sitting"))
  } else if (outcome == "upright") {
    #BINARY UPRIGHT VS NOT
    ds$classe[ds$classe %in% c(3:10)] <- 12 
    ds$classe[ds$classe %in% c(1,2)] <- 11 
    ds$classe <- factor(ds$classe, levels = c(11,12), labels = c("upright","not upright"))
  } else if (outcome == "prone") {
    #BINARY UPRIGHT VS NOT
    ds$classe[ds$classe %in% c(1:2, 5:10)] <- 12 
    ds$classe[ds$classe %in% c(3,4)] <- 11 
    ds$classe <- factor(ds$classe, levels = c(11,12), labels = c("prone","not prone"))
  } else if (outcome == "held") {
    #BINARY HELD VS NOT
    ds$classe[ds$classe %in% c(1,2,3,4,7:10)] <- 12 #Classify all non sitting
    ds$classe[ds$classe %in% c(5,6)] <- 11 #Classify all sitting
    ds$classe <- factor(ds$classe, levels = c(11,12), labels = c("held","not held"))
  } else if (outcome == "moving") {
    #BINARY MOVING VS STATIONARY
    ds$classe[ds$classe %in% c(1,3,6,7,8,9,10)] <- 1 #Classify all stationary
    ds$classe[ds$classe %in% c(2,4,5)] <- 2 #Classify all moving
    ds$classe <- factor(ds$classe, levels = c(1,2), labels = c("sitting","not sitting"))
  }
  
  output <- ds
  
}
  
  
  
  
  
  