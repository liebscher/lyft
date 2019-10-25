setwd("~/Documents/Projects/lyft/")

library(tidyverse)

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

label = function (t, d, o, g) {
  if (d %in% c(1, 2, 3, 4, 5) & !anyNA(c(o, g))) {
    if (t > 5.0 & t < 10) {
      if (g == "Work" | g == "School") {
        return("MorningCommute")
      }
    } else if (t >= 10 & t < 15) {
      if (o == "Work" | g == "Work") {
        return("AfternoonCommute")
      }
    } else if (t >= 15.0 & t < 20) {
      if (o == "Work" | o == "School") {
        return("EveningCommute")
      }
    }
  }
  if (d %in% c(6, 7)) {
    if (t >= 9 & t < 18) {
      if (!anyNA(c(o, g))) {
        if ((o == "Hotel" & g %in% c("Leisure", "Food", "Social")) |
            (g == "Hotel" & o %in% c("Leisure", "Food", "Social"))) {
          return("Tourism")
        }
        
      }
    }
  }
  if (d %in% c(5, 6, 7)) {
    if (t < 3.0 | t > 18.0) {
      if (!anyNA(c(o, g))) {
        if ((g == "Social") | (g == "Home" & o == "Social")) {
          return("Nightlife")
        }
      }
    }
  }
  return("Other")
}

data$TimeLabel = mapply(label, data$StartTime, data$DOW, data$Origin, data$Goal)

data$Shared = as.logical(data$Shared)
data$Conversation = as.logical(data$Conversation)

data$StartLocation = data$`Start Location`

data$StartGas = data$`Start Gas`
data$EndGas = data$`End Gas`
data$GasUsage = data$EndGas - data$StartGas

data$Earnings = as.numeric(gsub("[$]", "", data$Earnings))
data$Tips = as.numeric(gsub("[$]", "", data$Tips))
data$Wage = (data$Earnings + data$Tips) * (60 / data$Duration)

data$RatingSum = data$RatingConversation + data$RatingRoute + data$RatingComfortability
data$RatingMean = data$RatingSum / 3

data = data %>% select(-one_of(c("Start Gas", "End Gas", "Start Time", "Pickup Time", "End Time", "Start Location", "X3")))

write_csv(data, "CleanLyftData_All.csv")

#####

# create a new dataframe
result = NULL

# iterate over each session (necessary for knowing the first and last rides of a session)
for (session in unique(data$Session)) {
  
  # get only periods for this session
  s = data[data$Session == session, ]
  
  # create a few new columsn of interest
  # these better explain how much effort is put into a ride
  s$AdjDuration = NA
  s$AdjDistance = NA
  s$AdjGas = NA
  s$Position = NA
  
  # get the indices of periods which were actual drives. We'll modify and keep these rows
  w = which(s$Period == "Drive")
  # iterate over each drive index
  for (ix in 1:length(w)) {
    # if this is the first drive in the session:
    if (ix == 1) {
      # get the indices of any periods which came before this drive
      pre = head(seq(1, w[ix]), -1)
      # get the indices of any periods which came after this drive (but not after the next drive)
      post = tail(seq(w[ix], w[ix+1]-1), -1)
      
      adj_dr = s[w[ix], "Duration"] + sum(s[pre, "Duration"]) + sum(s[post, "Duration"]) / 2
      adj_ds = s[w[ix], "Distance"] + sum(s[pre, "Distance"]) + sum(s[post, "Distance"]) / 2
      adj_gs = s[w[ix], "GasUsage"] + sum(s[pre, "GasUsage"]) + sum(s[post, "GasUsage"]) / 2
      
      s[w[ix], "Position"] = "First"
      
      
    } else if (ix == max(1:length(w))) {
      # if this is the last drive in the session:
      
      # get the indices of any periods which came before this drive (but not before the drive prior)
      pre = head(seq(w[ix-1]+1, w[ix]), -1)
      # get the indices of any periods which came after this drive (until the end of the session)
      post = tail(seq(w[ix], nrow(s)), -1)
      
      adj_dr = s[w[ix], "Duration"] + sum(s[pre, "Duration"]) / 2 + sum(s[post, "Duration"])
      adj_ds = s[w[ix], "Distance"] + sum(s[pre, "Distance"]) / 2 + sum(s[post, "Distance"])
      adj_gs = s[w[ix], "GasUsage"] + sum(s[pre, "GasUsage"]) / 2 + sum(s[post, "GasUsage"])
      
      s[w[ix], "Position"] = "Last"
      
    } else {
      # if this is neither the first nor the last drive in the session:
      
      # get the indices of any periods which came before this drive (but not before the drive prior)
      pre = head(seq(w[ix-1]+1, w[ix]), -1)
      # get the indices of any periods which came after this drive (but not after the next drive)
      post = tail(seq(w[ix], w[ix+1]-1), -1)
      
      # the new columns take into account the context of the surrounding periods
      # each drive takes responsibility for how much driving is neccessary to get to it and to get to the next drive
      adj_dr = s[w[ix], "Duration"] + sum(s[pre, "Duration"]) / 2 + sum(s[post, "Duration"]) / 2
      adj_ds = s[w[ix], "Distance"] + sum(s[pre, "Distance"]) / 2 + sum(s[post, "Distance"]) / 2
      adj_gs = s[w[ix], "GasUsage"] + sum(s[pre, "GasUsage"]) / 2 + sum(s[post, "GasUsage"]) / 2
      
      s[w[ix], "Position"] = "Middle"
      
    }
    # append data to our new columns
    s[w[ix], "AdjDuration"] = adj_dr
    s[w[ix], "AdjDistance"] = adj_ds
    s[w[ix], "AdjGas"] = adj_gs
  }
  
  # save the session into our new dataframe
  result = rbind(result, s[w, ])
}


result = mutate(result, AdjWage = (Earnings + Tips - 3.4124 * AdjGas) * (60 / AdjDuration))

write_csv(result, "CleanLyftData_Drives.csv")