library(dplyr)

data_old = data.frame(Time = 1:4, Data = runif(4))

data_old

data_new = data.frame(Time = 3:8, Data = runif(6))

data_new

data_keep <- anti_join(data_old, data_new, by = "Time")

data_keep

data_merged <- rbind(data_keep, data_new)

data_merged
