#Setting directory

setwd("C:/Users/daily/Desktop/Baseball-America-Analysis/R")

#Loading libraries

library(tidyr)
library(ggplot2)
library(dplyr)

#Grabbing csv file

team_data <- read.csv(file = "C:/Users/daily/Desktop/Baseball-America-Analysis/Csvs/War by Team and Year.csv")

#Transforming data so individual seasons are in one column

team_data_long <- gather(team_data, Season, WAR, WARYearOne, WARYearTwo, WARYearThree, WARYearFour, WARYearFive, WARYearSix)

#Copying to a new sheet for summary statistics

summary_team_data <- team_data

#Calculating the total WAR accumulated by a farm system over six seasons

summary_team_data$TotalWAR <- summary_team_data$WARYearOne + summary_team_data$WARYearTwo + summary_team_data$WARYearThree +
summary_team_data$WARYearFour + summary_team_data$WARYearFive + summary_team_data$WARYearSix

#Creating a new column that combines year with team

summary_team_data$Year_TeamName <- paste(summary_team_data$YearID, summary_team_data$TeamName, sep = " ")

#Creating a visual for WAR produced over six years by farm ranking

ggplot(data = summary_team_data, aes(x = factor(TeamRank), y = TotalWAR)) +
geom_boxplot(alpha = .75, fill = "orange") +
geom_smooth(aes(group = 1)) +
scale_y_continuous(breaks = seq(-10, 140, 10)) +
labs(title = "WAR Produced by Farm System Over Six Years", x = "Team Rank", y = "WAR") +
theme(plot.title = element_text(hjust = 0.5))

#Calculating summary statistics for WAR produced over six years

summary(summary_team_data$TotalWAR)

#Calculating summary statistics for WAR in a single season

summary(team_data_long$WAR)

#Calculating summary statistics by farm ranking

stats_by_team_rank <- summary_team_data %>%
group_by(TeamRank) %>%
summarize(Average = mean(TotalWAR), Minimum = min(TotalWAR), FirstQuartile = quantile(TotalWAR, .25), Median = median(TotalWAR), ThirdQuartile = quantile(TotalWAR, .75), Maximum = max(TotalWAR), StDev = sd(TotalWAR))

#Calculating the correlation of TeamRank to WAR over the next six years

summary(lm(TotalWAR ~ TeamRank, data = summary_team_data))$r.squared

#Creating a density plot for WAR produced over six years

ggplot(data = summary_team_data, aes(x = TotalWAR)) +
geom_histogram(aes(y=..density..), binwidth = 10, color = "black", fill = "white") +
geom_density(alpha = .2, fill = "red") +
scale_x_continuous(breaks = seq(-10, 150, 10)) +
geom_vline(aes(xintercept=mean(TotalWAR)), color = "black", linetype = "dashed", size = 1.25) +
geom_text(aes(x = mean(TotalWAR), y = .0075, hjust = 1.2, label = "Mean")) +
geom_text(aes(x = mean(TotalWAR), y = .0075, hjust = -.1, label = "45.83 WAR")) +
labs(title = "Density Plot of WAR Produced by Farm System Over Six Years", x = "Total WAR", y = "Density") +
theme(plot.title = element_text(hjust = 0.5))

#Creating a density plot for WAR produced in a single season

ggplot(data = team_data_long, aes(x = WAR)) +
geom_histogram(aes(y=..density..), binwidth = 2.5, color = "black", fill = "white") +
geom_density(alpha = .2, fill = "red") +
scale_x_continuous(breaks = seq(-10, 30, 5)) +
geom_vline(aes(xintercept=mean(WAR)), color = "black", linetype = "dashed", size = 1.25) +
geom_text(aes(x = mean(WAR), y = .0275, hjust = 1.2, label = "Mean")) +
geom_text(aes(x = mean(WAR), y = .0275, hjust = -.1, label = "7.64 WAR")) +
labs(title = "Density Plot of WAR Produced by Farm System in a Single Year", x = "Single Year WAR", y = "Density") +
theme(plot.title = element_text(hjust = 0.5))

#Re-ordering season for density plot

ordered_team_data_long <- factor(team_data_long$Season, levels = c("WARYearOne", "WARYearTwo", "WARYearThree", "WARYearFour", "WARYearFive", "WARYearSix"))

#Creating a density plot to show differences between individual seasons

ggplot(data = team_data_long, aes(x = WAR, fill = ordered_team_data_long)) +
geom_density(alpha = .2) +
scale_fill_discrete(labels = c("Same Season", "Second Season", "Third Season", "Fourth Season", "Fifth Season", "Sixth Season")) +
scale_x_continuous(breaks = seq(-10, 30, 5)) +
labs(title = "Density Plot of Single Season WAR in Relation to Time of Ranking", x = "Single Year WAR", y = "Density", fill = "Season") +
theme(plot.title = element_text(hjust = 0.5), legend.title = element_text(hjust = 0.5))

#Filtering data to only include seasons four through six

Years_Four_thru_Six <- filter(team_data_long, Season == "WARYearFour" | Season == "WARYearFive" | Season == "WARYearSix")

#Calculating summary statistics for new dataset

summary(Years_Four_thru_Six$WAR)

#Creating a density plot for WAR produced in a single season for seasons four through six

ggplot(data = Years_Four_thru_Six, aes(x = WAR)) +
geom_histogram(aes(y=..density..), binwidth = 2.5, color = "black", fill = "white") +
geom_density(alpha = .2, fill = "red") +
scale_x_continuous(breaks = seq(-10, 30, 5)) +
geom_vline(aes(xintercept=mean(WAR)), color = "black", linetype = "dashed", size = 1.25) +
geom_text(aes(x = mean(WAR), y = .0275, hjust = 1.2, label = "Mean")) +
geom_text(aes(x = mean(WAR), y = .0275, hjust = -.1, label = "10.3 WAR")) +
labs(title = "Density Plot of WAR Produced by Farm System in a Single Year (Seasons 4-6 Only)", x = "Single Year WAR", y = "Density") +
theme(plot.title = element_text(hjust = 0.5))

#Calculating summary statistics by farm ranking for individual seasons

stats_by_team_rank_ind_season <- team_data_long %>%
group_by(TeamRank) %>%
summarize(Average = mean(WAR), Minimum = min(WAR), FirstQuartile = quantile(WAR, .25), Median = median(WAR), ThirdQuartile = quantile(WAR, .75), Maximum = max(WAR), StDev = sd(WAR))

#Creating a visual for individual season production for seasons four through six

ggplot(data = Years_Four_thru_Six, aes(x = factor(TeamRank), y = WAR)) +
geom_boxplot(alpha = .75, fill = "orange") +
geom_smooth(aes(group = 1)) +
scale_y_continuous(breaks = seq(-10, 40, 5)) +
labs(title = "WAR Produced by Farm System in a Single Year (Years 4-6)", x = "Team Rank", y = "WAR") +
theme(plot.title = element_text(hjust = 0.5))
