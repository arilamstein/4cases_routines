# Constructor function for sequences withOUT timestamps
# Probably better to have the function name indicate that it should only be used 
# for sequences without timestamps e.g. SequenceObjectNoTimestamps
SequenceObject <- function(data, startdate, stopdate, exclude = c()){
  
  # basic reformatting
  data <- read.table(data, sep = ",", header = TRUE)
  data <- subset(data, select = c("pull_req_id", "user", "action", "created_at"))

  # exclude all activity types which are not wanted in the data
  data <- subset(data,!(action %in% exclude))
  
  # select only data between startdate and stopdate
  colnames(data) <- c("id", "actor", "event", "time")
  data <- sqldf(paste0("SELECT * FROM data WHERE strftime('%Y-%m-%d', time,
                       'unixepoch', 'localtime') >= '",startdate,"' AND strftime('%Y-%m-%d', time,
                       'unixepoch', 'localtime') <= '",stopdate,"'"))
}

SequenceObjectSTS <- function(data, startdate, stopdate, exclude = c()){
  
  # call parent SequenceObject class somehow

  data.split <- split(data$event, data$id)
  list.to.df <- function(arg.list) {
    max.len  <- max(sapply(arg.list, length))
    arg.list <- lapply(arg.list, `length<-`, max.len)
    as.data.frame(arg.list)
  }
  data <- list.to.df(data.split)
  data <- t(data)
  
  (arrange(data, id))
}
