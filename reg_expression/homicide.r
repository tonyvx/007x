homicides <- readLines("./homicides.txt")

r <- regexec("<dd>[F|f]ound on (.*?)</dd>", homicides)
m <- regmatches(homicides, r)
dates <- sapply(m, function(x)
  x[2])
dates <- as.Date(dates, '%B %d, %Y')
hist(dates, "month", freq = TRUE)