temp <- tempfile()
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",temp)
data <- read.csv(unz(temp, "activity.csv"), sep = ",", header = TRUE, stringsAsFactors = FALSE)
unlink(temp)


