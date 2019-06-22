library(tidyverse)
library(kernlab)
library(gridExtra)

data = read_csv("LyftData.csv")

data$Date = strptime(data$Date, "%m/%d/%Y")
data$DOW = factor(format(data$Date, "%u"))
data$Month = factor(as.numeric(format(data$Date, "%m")))

data$RequestTime = strptime(data$`Request Time`, "%H:%M")
data$RequestTime = as.numeric(format(data$RequestTime, "%H")) + as.numeric(format(data$RequestTime, "%M")) / 60

data$PickupTime = strptime(data$`Pickup Time`, "%H:%M")
data$PickupTime = as.numeric(format(data$PickupTime, "%H")) + as.numeric(format(data$PickupTime, "%M")) / 60

data$EndTime = strptime(data$`End Time`, "%H:%M")
data$EndTime = as.numeric(format(data$EndTime, "%H")) + as.numeric(format(data$EndTime, "%M")) / 60


data$Cancel = as.logical(data$Cancel)
data$Shared = as.logical(data$Shared)
data$Conversation = as.logical(data$Conversation)
data$GapTime = data$`Gap Time`
data$GapAction = data$`Gap Action`

data$StartLocation = data$`Start Location`

data$GasUsage = data$`End Gas` - data$`Start Gas`

data$Amount = as.numeric(gsub("[$]", "", data$Amount))
data$Tip = as.numeric(gsub("[$]", "", data$Tip))
data$Wage = (data$Amount + data$Tip) * (60 / data$Duration)

data = data %>% select(-one_of(c("Start Gas", "End Gas", "Gap Time", "Request Time", "Pickup Time", "End Time", "Gap Action", "Start Location", "X3", "X8", "X14")))

write_csv(data, "CleanLyftData.csv")