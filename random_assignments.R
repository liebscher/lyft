sessions = 15

minimum_start_hour = 5.0
maximum_start_hour = 23

session_length = 2.0 # hours

weekdays = c("Mon", "Tue", "Wed", "Thur", "Fri")
weekends = c("Sat", "Sun")

days = c(weekdays, weekends)

hour = round(runif(1, min = minimum_start_hour, max = maximum_start_hour),1)
day = days[round(runif(1, min = 1, max = length(days)))]

assignment = data.frame(day=factor(day, levels = days), hour=c(hour))

for (i in 2:sessions) {
  while (TRUE) {
    hour = round(runif(1, min = minimum_start_hour, max = maximum_start_hour), 1)
    day = days[round(runif(1, min = 1, max = length(days)))]
    
    onday = assignment[assignment$day == day, ]
    
    if (sum(hour - onday$hour < session_length + 1) == 0) {
      break
    }
  }
  
  assignment = rbind(assignment, data.frame(day=day, hour=hour))
  
}

assignment[order(assignment$day, assignment$hour), ]



