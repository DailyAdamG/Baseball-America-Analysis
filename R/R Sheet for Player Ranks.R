#Setting directory

setwd("C:/Users/daily/Desktop/Repositories/Baseball-America-Analysis/R")

#Loading libraries

library(tidyr)
library(ggplot2)
library(dplyr)
theme_set(theme_bw())

#Grabbing csv file

rank_data <- read.csv(file = "C:/Users/daily/Desktop/Repositories/Baseball-America-Analysis/Csvs/Player Ranks Through 2020.csv")

#Filter to only include players that have completed their six season time frame

rank_data <- rank_data %>%
  filter(YearID <= 2015)

#Replace null values with zeros to create WAR totals for years where player is no longer active

rank_data[is.na(rank_data)] <- 0.0

#Combining WAR into one column

rank_data$TotalWAR <- round(rank_data$WARYearOne + rank_data$WARYearTwo + rank_data$WARYearThree +
  rank_data$WARYearFour + rank_data$WARYearFive + rank_data$WARYearSix, 1)

#Find summary stats for player rank in organization

summary_position <- rank_data %>%
  group_by(OrgRank)%>%
  summarize(AverageWAR = round(mean(TotalWAR), 1),
            MedianWAR = round(median(TotalWAR), 1),
            MaxWAR = round(max(TotalWAR), 1)) 

#Find summary stats for each ranking combo

summary_war_table <- rank_data %>%
  group_by(FranchRank, OrgRank) %>%
  summarize(AverageWAR = round(mean(TotalWAR), 1),
            MedianWAR = round(median(TotalWAR), 1),
            MaxWAR = round(max(TotalWAR), 1)) %>%
  ungroup()

#Creating a visual for average WAR by rank in organization alone

summary_position %>%
  ggplot(aes(x = OrgRank, y = AverageWAR)) +
  geom_point(size = 1.5, color = "red") +
  geom_line(color = "red") +
  scale_x_continuous(breaks = seq(1, 30, 1)) +
  scale_y_continuous(breaks = seq(0, 10, 0.5)) +
  labs(title = "Average WAR by Ranking in Organization", x = "Player Rank in Organization", y = "Average WAR")

#Create heat map for Average WAR

summary_war_table %>%
  ggplot(aes(x = FranchRank, y = OrgRank)) +
  geom_tile(aes(fill = AverageWAR), color = "black") +
  scale_fill_distiller(palette = "Spectral") +
  geom_text(aes(label = sprintf("%0.1f", round(AverageWAR, digits = 1)))) +
  scale_x_continuous(breaks = seq(1,30,1)) +
  scale_y_reverse(breaks = seq(1,30,1)) +
  labs(title = "Heat Map of Average WAR by Baseball America Prospect Rankings", 
       x = "Organization's Rank in MLB",
       y = "Player Rank in Organization",
       fill = "Average WAR")

#Create heat map for Median WAR

summary_war_table %>%
  ggplot(aes(x = FranchRank, y = OrgRank)) +
  geom_tile(aes(fill = MedianWAR), color = "black") +
  scale_fill_distiller(palette = "Spectral") +
  geom_text(aes(label = sprintf("%0.1f", round(MedianWAR, digits = 1)))) +
  scale_x_continuous(breaks = seq(1,30,1)) +
  scale_y_reverse(breaks = seq(1,30,1)) +
  labs(title = "Heat Map of Median WAR by Baseball America Prospect Rankings", 
       x = "Organization's Rank in MLB",
       y = "Player Rank in Organization",
       fill = "Median WAR")

#Create heat map for Maximum WAR

summary_war_table %>%
  ggplot(aes(x = FranchRank, y = OrgRank)) +
  geom_tile(aes(fill = MaxWAR), color = "black") +
  scale_fill_distiller(palette = "Spectral") +
  geom_text(aes(label = sprintf("%0.1f", round(MaxWAR, digits = 1)))) +
  scale_x_continuous(breaks = seq(1,30,1)) +
  scale_y_reverse(breaks = seq(1,30,1)) +
  labs(title = "Heat Map of Maximum WAR by Baseball America Prospect Rankings", 
       x = "Organization's Rank in MLB",
       y = "Player Rank in Organization",
       fill = "Maximum WAR")

#Double digit ranked prospects who produced

unheralded_prospect_stars <- rank_data %>%
  filter(OrgRank >= 11 & TotalWAR >= 10)

#List of distinct unheralded prospects who produced

unheralded_prospect_stars_list <- unheralded_prospect_stars[!duplicated(unheralded_prospect_stars$PlayerID),]

#Breakdown of amateur background

table(unheralded_prospect_stars_list$AmateurType)

prop.table(table(unheralded_prospect_stars_list$AmateurType)) * 100

#Breakdown by player's level

table(unheralded_prospect_stars$HighestLevel)

prop.table(table(unheralded_prospect_stars$HighestLevel)) * 100

#Import table for visual

visual_list <- read.csv(file = "C:/Users/daily/Desktop/Repositories/Baseball-America-Analysis/Csvs/Unheralded Players with Position.csv")

#Create vector for level order

visual_list$Highest.Level.Played <- factor(visual_list$Highest.Level.Played, levels = c("MLB", "AAA", "AA", "A+", "A", "A-", "Rk"))

#Create visual using list

visual_list %>%
  ggplot(aes(x = Highest.Level.Played, fill = Amateur.Type)) +
  geom_bar(position = "dodge", color = "black") +
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(breaks = seq(0,15,1)) +
  labs(title = "Amount of Times a Player Accumulated Over 10 WAR",
       x = "Highest Level Played", 
       y = "Number of Occurrences", 
       fill = "Amateur Type") +
  facet_wrap(~Position)

#Seeing how many unheralded prospects from each amateur type

unheralded_prospects <- rank_data %>%
  filter(OrgRank >= 11)

#List of distinct unheralded prospects

unheralded_prospects_list <- unheralded_prospects[!duplicated(unheralded_prospects$PlayerID),]

#Breakdown of amateur background

table(unheralded_prospects_list$AmateurType)

prop.table(table(unheralded_prospects_list$AmateurType)) * 100