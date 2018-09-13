library(readxl)
install.packages("plater")
library(plater)

##check to make sure plater format is okay for our file
check_plater_format("~/Projects/TEAMM/Data/june14_nh4_plater.csv")

##import data
data <- read_plate("~/Projects/TEAMM/Data/june14_nh4_plater.csv", well_ids_column = "Wells")

data
str(data)

library(ggplot2)
library(tidyverse)
library(dplyr)

#get mean blank absorbance
a = 0.0456
b = .0492
x <- mean(c(a,b))

#add row for blanked absorbances, i.e. subtract off average absorbance for blank wells, here x
data1 <- mutate(data, blanked_abs = nh4_abs-x)
data1
#trim data set to just standards and absorbances
stds <- data1[c(-1,-2,-3)]

#get rid of NA value
stds <- na.omit(stds)

#get rid of outlier in row 10, column 2, then run na.omit command again
stds[10, 2] = NA

#print to verify that outlier is gone
stds

#get means for each standards concentration
stds_avg_abs <- stds %>% group_by(nh4_stds) %>% summarise(avg_bl_abs = mean(blanked_abs))

#store new tibble as data frame
stds_avg_abs <- as.data.frame(stds_avg_abs)

#WHAT IS THE DIFFERENCE BETWEEN A TIBBLE AND A REGULAR DATA FRAME??

plot(stds_avg_abs$nh4_stds, stds_avg_abs$avg_bl_abs)
#fit line
line <- abline(lm(stds_avg_abs$avg_bl_abs~stds_avg_abs$nh4_stds), col = "red")
#how to get r2 and m for line?
#how to just look at line for 0-10 uM and compare slopes between that and full conc set
##what is detection limit for this? around 0.1 uM I think



##NOT WORKING:
##ggplot(data = stds_avg_abs, aes(x = nh4_stds, y = avg_bl_abs)) +
  #theme_bw()
