
# Script for HW3

# This is the script that contains the code for HW3, which manipulates two data frames and creates a new data frame.

# The inputs of the script are NBA data from the github website of stat-133. This includes two files, one about "stats" and one about "roster".

# The outputs of the script will be several plots, tables, as well as a new csv file.

library(readr)
library(dplyr)
library(ggplot2)
dat <- read_csv(file = "data/nba2017-stats.csv")
dat2 <- read_csv(file = "data/nba2017-roster.csv")

# Adding new variables
dat <- mutate(dat, missed_fg = field_goals_atts - field_goals_made)
dat <- mutate(dat, missed_ft = points1_atts - points1_made)
dat <- mutate(dat, points = points1_made + 2*points2_made + 3*points3_made)
dat <- mutate(dat, rebounds = off_rebounds + def_rebounds)
dat <- mutate(dat, efficiency = (points + rebounds + assists + steals + blocks - missed_fg - missed_ft - turnovers)/games_played)

sink(file = "output/efficiency-summary.txt")
  summary(dat$efficiency)
sink()

# Merging tables
dat1 <- merge(dat, dat2)

# Creating nba2017-teams.csv
teams <- dat1 %>%
  group_by(team) %>%
    summarise(experience = round(sum(experience), digits = 2),
              salary = round(sum(salary/1000000), digits = 2),
              points3 = sum(points3_made),
              points2 = sum(points2_made),
              free_throws = sum(points1_made),
              points = sum(sum(points1_made) + 2*sum(points2_made) + 3*sum(points3_made)),
              off_rebounds = sum(off_rebounds),
              def_rebounds = sum(def_rebounds),
              assists = sum(assists),
              steals = sum(steals),
              blocks = sum(blocks),
              turnovers = sum(turnovers),
              fouls = sum(fouls),
              efficiency = sum(efficiency))

summary(teams)
  
sink(file = "output/teams-summary.txt")
  summary(teams)
sink()

write.csv(teams, file = "data/nba2017-teams.csv", row.names = T)
  
# Some graphics
pdf(file = "images/teams_star_plot.pdf")
  stars(teams[ , -1], labels = teams$team)
dev.off()

pdf(file = "images/experience_salary.pdf")
  ggplot(data = teams) + geom_point(aes(x = teams$experience, y = teams$salary), alpha = 0) + 
    geom_text(x = teams$experience, y = teams$salary, label = teams$team) + 
    labs(x = "Experience", y = "Salary")
dev.off()







