
# This script cleans the raw data

library(readr)
library(dplyr)
source('code/functions.R')

rawscores <- read.csv('data/rawdata/rawscores.csv')

# sink summary statsitics
sink('output/summary-rawscores.txt')
  str(rawscores)
  for (i in 1: 16) {
    print_stats(summary_stats(rawscores[, i]))
  }
sink()


# replace NA with 0
for (i in 1:dim(rawscores)[1]) {
  for (j in 1: dim(rawscores)[2]) {
    if (is.na(rawscores[i,j]) == T) {
      rawscores[i,j] = 0
    }
  }
}


# rescale quiz scores
rawscores <- mutate(rawscores, QZ1 = 
                      rescale100(rawscores$QZ1, xmin = 0, xmax = 12))
rawscores <- mutate(rawscores, QZ2 = 
                      rescale100(rawscores$QZ2, xmin = 0, xmax = 18))
rawscores <- mutate(rawscores, QZ3 = 
                      rescale100(rawscores$QZ3, xmin = 0, xmax = 20))
rawscores <- mutate(rawscores, QZ4 = 
                      rescale100(rawscores$QZ4, xmin = 0, xmax = 20))


# rescale exam scores
Test1 <- rescale100(rawscores$EX1, xmin = 0, xmax = 80)
Test2 <- rescale100(rawscores$EX2, xmin = 0, xmax = 90)

rawscores <- mutate(rawscores, Test1)
rawscores <- mutate(rawscores, Test2)


# Calculate homework scores
Homework <- rep(0, dim(rawscores)[1])
for (i in 1: dim(rawscores)[1]) {
  k <- rawscores[i, 1:9]
  names(k) = NULL
  a <- rep(0, 9)
  for (j in 1: 9) {
    a[j] = k[[j]]
  }
  Homework[i] <- score_homework(a, drop = T)
}

rawscores <- mutate(rawscores, Homework)


# Calculate quiz scores
Quiz <- rep(0, dim(rawscores)[1])
for (i in 1: dim(rawscores)[1]) {
  k <- rawscores[i, 11:14]
  names(k) = NULL
  a <- rep(0, 4)
  for (j in 1: 4) {
    a[j] = k[[j]]
  }
  Quiz[i] <- score_quiz(a, drop = T)
}

rawscores <- mutate(rawscores, Quiz)


# Calculate lab scores
Lab <- rep(0, dim(rawscores)[1])
for (i in 1: dim(rawscores)[1]) {
  k <- rawscores[i, 10]
  names(k) = NULL
  a = k[[1]]
  Lab[i] <- score_lab(a)
}

rawscores <- mutate(rawscores, Lab)


# Calculate overall scores
Overall <- rep(0, dim(rawscores)[1])
for (i in 1: dim(rawscores)[1]) {
  hw = rawscores[i,19][[1]]
  qz = rawscores[i,20][[1]]
  test1 = rawscores[i,17][[1]]
  test2 = rawscores[i,18][[1]]
  lab = rawscores[i,21][[1]]
  Overall[i] <- 0.3*hw + 0.1*lab + 0.15*qz + 0.2*test1 + 0.25*test2
}

rawscores <- mutate(rawscores, Overall)


# Calculate letter grades
Grade <- rep(0, dim(rawscores)[1])
for (i in 1: dim(rawscores)[1]) {
  k <- rawscores[i, 22]
  a = k[[1]]
  if (a < 50) {
    Grade[i] = "F"
  } else if (a < 60 & a >= 50) {
    Grade[i] = "D"
  } else if (a < 70 & a >= 60) {
    Grade[i] = "C-"
  } else if (a < 77.5 & a >= 70) {
    Grade[i] = "C"
  } else if (a < 79.5 & a >= 77.5) {
    Grade[i] = "C+"
  } else if (a < 82 & a >= 79.5) {
    Grade[i] = "B-"
  } else if (a < 86 & a >= 82) {
    Grade[i] = "B"
  } else if (a < 88 & a >= 86) {
    Grade[i] = "B+"
  } else if (a < 90 & a >= 88) {
    Grade[i] = "A-"
  } else if (a < 95 & a >= 90) {
    Grade[i] = "A"
  } else if (a <= 100 & a >= 95) {
    Grade[i] = "A+"
  } 
}

rawscores <- mutate(rawscores, Grade)
cleanscores <- rawscores


# sink files
sink('output/summary-cleanscores.txt')
  str(cleanscores)
sink()

for (i in 17:22) {
  sink(sprintf('output/ %s-stats.txt', colnames(cleanscores)[i]))
    print_stats(summary_stats(cleanscores[,i]))
  sink()
}

# Export the csv file
write.csv(cleanscores, file = 'data/cleandata/cleanscores.csv', row.names = F)









