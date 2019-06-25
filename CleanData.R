library(tidyverse)
library(kernlab)
library(gridExtra)

data = read_csv("LyftData.csv")

data$Date = strptime(data$Date, "%m/%d/%Y")
data$DOW = factor(format(data$Date, "%u"))
data$Month = factor(as.numeric(format(data$Date, "%m")))

data$StartTime = strptime(data$`Start Time`, "%H:%M")
data$StartTime = as.numeric(format(data$StartTime, "%H")) + as.numeric(format(data$StartTime, "%M")) / 60

data$PickupTime = strptime(data$`Pickup Time`, "%H:%M")
data$PickupTime = as.numeric(format(data$PickupTime, "%H")) + as.numeric(format(data$PickupTime, "%M")) / 60

data$EndTime = strptime(data$`End Time`, "%H:%M")
data$EndTime = as.numeric(format(data$EndTime, "%H")) + as.numeric(format(data$EndTime, "%M")) / 60


data$Cancel = as.logical(data$Cancel)
data$Shared = as.logical(data$Shared)
data$Conversation = as.logical(data$Conversation)

data$StartLocation = data$`Start Location`

data$StartGas = data$`Start Gas`
data$EndGas = data$`End Gas`
data$GasUsage = data$EndGas - data$StartGas

data$Earnings = as.numeric(gsub("[$]", "", data$Earnings))
data$Tips = as.numeric(gsub("[$]", "", data$Tips))
data$Wage = (data$Earnings + data$Tips) * (60 / data$Duration)

data = data %>% select(-one_of(c("Start Gas", "End Gas", "Start Time", "Pickup Time", "End Time", "Start Location", "X3")))

write_csv(data, "CleanLyftData.csv")