## Simple plotting script

##############
## Plotting ##
##############
library(ggplot2)

updateList$date <- as.Date(strptime(updateList$date, format = "%d %B %Y"))

ggplot(updateList) + theme_bw() +
  geom_bar(aes(date, fill = desease), stat = "bin", size = 1.3) 