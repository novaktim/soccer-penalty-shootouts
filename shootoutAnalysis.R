
library(plyr)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(data.table)
library(readxl)


# Load scraped histories of match results
all_results = do.call(rbind.fill, lapply(list.files("data/fbref_match_results", full.names = T), readRDS))


# Percentage of games that lead to a Penalty Shootout by League -> excluded Men's Olympics tournament, here we only have shootouts

tab = table(all_results$Round[grepl("won on penalty kicks following", all_results$Notes)])
print(tab)
# Remove games where no shootout possible by rule
results = data.table(all_results[all_results$Round %in% names(tab),])

# Shootouts happening by League
data = results[, .(Shootouts = mean(grepl("won on penalty kicks following", Notes)) * 100), by = "Competition_Name"]
ggplot(data, aes(y = reorder(Competition_Name, Shootouts), x = Shootouts)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  #theme_minimal() +
  labs(title = "% Penalty Shootout happening by League",
       x = "Percentage of matches",
       y = "League")
#ggsave("plots/percentage of shootout by league.png")

# Shootouts happening by Decade
decades = floor(year(results$Date) / 10) * 10
data = results[, .(Shootouts = mean(grepl("won on penalty kicks following", Notes)) * 100), by = decades]
colnames(data)[1] = "Decade"
data$Decade = paste0("[", data$Decade, "-", data$Decade + 10, ")")
data$Decade[match("[2020-2030)", data$Decade)] = "[2020-now)"
ggplot(data, aes(x = Decade, y = Shootouts)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  geom_text(aes(label = paste0(round(Shootouts, 3), "%")), vjust = -0.5) + # Add mean values on top of bars
  labs(title = "% Penalty Shootout happening per game by Decade",
       x = "Decade",
       y = "Percentage of matches")
# ggsave("plots/percentage of shootout happening by decade.png")

# Shootouts happening by Stage
results = results %>%
  mutate(Stage = case_when(
    grepl("Quarter-finals", Round) ~ "Quarter-finals",
    grepl("Semi-finals", Round) ~ "Semi-finals",
    grepl("Round of 16", Round) ~ "Round of 16",
    grepl("Final", Round) | grepl("Gold-medal match", Round) ~ "Final",
    grepl("Third-place match", Round) ~ "Third-place match",
    # grepl("Repechage", Round) ~ "Repechage", #sample size too small to look at these special cases
    #  grepl("Fifth-place", Round) ~ "Fifth-place match",
    TRUE ~ NA_character_ # Assign NA to rows that don't match any of the conditions
  ))
data = results[, .(Shootouts = mean(grepl("won on penalty kicks following", Notes)) * 100, .N), by = "Stage"]
data = data[!is.na(data$Stage),]
ggplot(data, aes(y = reorder(Stage, Shootouts), x = Shootouts)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  #theme_minimal() +
  labs(title = "% Penalty Shootout happening by Stage",
       x = "Percentage of matches",
       y = "Stage")
#ggsave("plots/percentage of shootout by stage.png")


########################################################################################################################
################ Load in all penalty shootouts ###########################
########################################################################################################################

files = list.files("data/fbref_match_summary", full.names = T) # all events happened for all matches scraped
shots = do.call(rbind.fill, lapply(files, readRDS))
shots$Gender = all_results$Gender[match(shots$Game_URL, all_results$MatchURL)] # Extract Gender from match results dataset
shots = shots[order(shots$Game_URL, shots$Penalty_Number),]
# Remove other events like yellow-card, goal, penalty, substitute as we only need penalty shootout shots
shots = shots[shots$Event_Type == "Penalty Shootout", ] 
# Add manually-entered Men Olympics shootouts which are missing on fbref.com
olympics = readxl::read_xlsx("data/olympics.xlsx")
olympics$Home_Away = ifelse(olympics$Penalty_Number %% 2 == 1, "Home", "Away")
shots = rbind.fill(olympics, shots) # Concatenate shots from fbref and manually entered men olympics
stopifnot(!is.na(shots$Home_Away))


shootouts = split(shots, shots$Game_URL) # Create list for each shootout containing their shots


# Count the years of shootouts
matchdates = as.Date(sapply(shootouts, function(x) x$Match_Date[1])) # [1] because is same for each shot
leagues = sapply(shootouts, function(x) x$League[1])
genders = sapply(shootouts, function(x) x$Gender[1])

# Create the bar plot by Year
tab = table(year(matchdates))
data = data.frame(Year = names(tab), Count = as.numeric(tab))


# ggplot(data, aes(x = Year, y = Count)) +
#   geom_bar(stat = "identity", fill = "skyblue", color = "black") +
#   labs(title = "Penalty Shootouts by Year",
#        x = "Year",
#        y = "# Penalty Shootouts")
#ggsave("plots/shootouts by year.png")


# Create the bar plot by League
tab = table(leagues)
data = data.frame(League = names(tab), Count = as.numeric(tab))
data = data[order(data$Count),]
ggplot(data, aes(y = reorder(League, Count), x = Count)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Penalty Shootouts by League",
       x = "# Penalty Shootouts",
       y = "League")
#ggsave("plots/shootouts by league.png")

# Create the bar plot by Gender
# tab = table(genders)
# data = data.frame(Gender = ifelse(names(tab) == "F", "Female", "Male"), Count = as.numeric(tab))
# ggplot(data, aes(x = Gender, y = Count)) +
#   geom_bar(stat = "identity", fill = "skyblue", color = "black") +
#   labs(title = "Penalty Shootouts by Gender",
#        x = "Gender",
#        y = "# Penalty Shootouts")
#ggsave("plots/shootouts by gender.png")


################### iterate through score progression, check which pens were decisive and which scored ###############################
for (i in seq_along(shootouts)) {
  shootout = shootouts[[i]]
  
  scores = strsplit(shootout$Score_Progression, ":")
  shootout$Score_Progression_Home = as.numeric(sapply(scores, function(x) x[1]))
  shootout$Score_Progression_Away = as.numeric(sapply(scores, function(x) x[2]))
  
  score_before_home = c(0, shootout$Score_Progression_Home[-nrow(shootout)])
  score_before_away = c(0, shootout$Score_Progression_Away[-nrow(shootout)])
  shootout$Has_Scored_Home = shootout$Score_Progression_Home > score_before_home
  shootout$Has_Scored_Away = shootout$Score_Progression_Away > score_before_away
  
  team_started_shootout = shootout$Penalty_Number %% 2 == 1
  
  
  # If the team shooting is leading in goals, behind in goals or tie
  # Pessimistic ignores that the second team has still a shot in the round, while optimistic look at the goal differences after the round.
  score_own = ifelse(shootout$Home_Away == "Home", score_before_home, score_before_away)
  score_opponent = ifelse(shootout$Home_Away == "Home", score_before_away, score_before_home)
  shootout[, "Goal_Difference_Pessimistic"] = score_own - score_opponent
  shootout[team_started_shootout, "Goal_Difference_Optimistic"] = score_own[team_started_shootout] - score_opponent[team_started_shootout]
  shootout[!team_started_shootout, "Goal_Difference_Optimistic"] = score_own[!team_started_shootout] + 1 - score_opponent[!team_started_shootout]
  
  
  # If the next scored goal is deciding to win
  # e.g. after 5 rounds: 1:0, 2:1, 3:2, 4:3,... for starter team, 0:0, 1:1, 2:2, 3:3, 4:4, 5:5,... for non-starter team
  
  score_own = ifelse(shootout$Home_Away == "Home", score_before_home, score_before_away)[team_started_shootout]
  score_opponent = ifelse(shootout$Home_Away == "Home", score_before_away, score_before_home)[team_started_shootout]
  possible_remaining_goals = pmax(1, 6 - 1:length(score_own))
  score_opponent_possible = score_opponent + possible_remaining_goals
  shootout[team_started_shootout, "Is_Decisive_To_Win"] = score_own + 1 > score_opponent_possible
  
  score_own = ifelse(shootout$Home_Away == "Home", score_before_home, score_before_away)[!team_started_shootout]
  score_opponent = ifelse(shootout$Home_Away == "Home", score_before_away, score_before_home)[!team_started_shootout]
  possible_remaining_goals = pmax(0, 5 - 1:length(score_own)) # one less because opponent already shot first
  score_opponent_possible = score_opponent + possible_remaining_goals
  shootout[!team_started_shootout, "Is_Decisive_To_Win"] = score_own + 1 > score_opponent_possible
  
  # If the next missed goal is deciding to lose
  # e.g. after 3 rounds: 1:3 for starter team, 1:3, for non-starter team
  
  score_own = ifelse(shootout$Home_Away == "Home", score_before_home, score_before_away)[team_started_shootout]
  score_opponent = ifelse(shootout$Home_Away == "Home", score_before_away, score_before_home)[team_started_shootout]
  possible_remaining_goals = pmax(0, 5 - 1:length(score_own))
  score_possible = score_own + possible_remaining_goals
  shootout[team_started_shootout, "Is_Decisive_To_Lose"] = score_possible < score_opponent
  
  score_own = ifelse(shootout$Home_Away == "Home", score_before_home, score_before_away)[!team_started_shootout]
  score_opponent = ifelse(shootout$Home_Away == "Home", score_before_away, score_before_home)[!team_started_shootout]
  possible_remaining_goals = pmax(0, 5 - 1:length(score_own))
  score_possible = score_own + possible_remaining_goals
  shootout[!team_started_shootout, "Is_Decisive_To_Lose"] = score_possible < score_opponent
  
  shootouts[[i]] = shootout
}

# Test and check implementation manually of random decisives
#this opens 5 random shootouts with had a decisive to win or to lose shot in them
ind = which(sapply(shootouts, function(x) any(x$Is_Decisive_To_Win)))
ind = which(sapply(shootouts, function(x) any(x$Is_Decisive_To_Lose)))
for (i in sample(ind, 5)) {
  shootouts[[i]] %>% 
    select(c(1:4,26:38)) %>% 
  View()
}


# Myth: Is the starting team winning more often?
winner_side = sapply(shootouts, function(x) if(x$Score_Progression_Home[nrow(x)] < x$Score_Progression_Away[nrow(x)]) "Home" else "Away")
starting_side = sapply(shootouts, function(x) x$Home_Away[x$Penalty_Number == 1])
gender = sapply(shootouts, function(x) x$Gender[1])
table(winner_side == starting_side)
table(winner_side[gender == "M"] == starting_side[gender == "M"])
table(winner_side[gender == "F"] == starting_side[gender == "F"])
# Yes they are
data = data.table(winner_side, starting_side, gender)
data = data[, .(win_chance = mean(winner_side == starting_side)), by = "gender"]
data$gender[data$gender == "M"] = "male"
data$gender[data$gender == "F"] = "female"
ggplot(data, aes(y = win_chance, x = gender)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  geom_text(aes(label = paste0(round(win_chance * 100, 3), "%")), vjust = -0.5) + # Add mean values on top of bars
  labs(title = "Win chance of starting team by Gender",
       x = "Gender",
       y = "Win chance")
#ggsave("plots/percentage win of starting team by gender.png")


all_shootouts = do.call(rbind, shootouts)
all_shootouts$hasScored = all_shootouts$Has_Scored_Home | all_shootouts$Has_Scored_Away
all_shootouts$team_started_shootout = all_shootouts$Penalty_Number %% 2 == 1
print(mean(all_shootouts$hasScored))
print(data.table(all_shootouts)[, .("Mean True Win" = mean(Is_Decisive_To_Win), 
                                    "Mean True Lose" = mean(Is_Decisive_To_Lose)), by = c("Home_Away", "team_started_shootout")])

print(data.table(all_shootouts)[, .("Mean Scored" = mean(hasScored), .N), by = 
  ifelse(all_shootouts$Goal_Difference_Pessimistic >= 1, "pess leading", ifelse(all_shootouts$Goal_Difference_Pessimistic < 0, "pess behind", "pess tie"))])

#when behind, the success rate goes down!

all_shootouts %>% 
  mutate(score_type = ifelse(Goal_Difference_Pessimistic >= 1, 
                             "pess leading", ifelse(Goal_Difference_Pessimistic < 0, "pess behind", "pess tie"))) %>% 
  group_by(score_type) %>% 
  summarise(avg_success = mean(hasScored), count = n()) %>% 
  arrange(avg_success)

all_shootouts = all_shootouts %>% 
  mutate(score_type_pessimistic = ifelse(Goal_Difference_Pessimistic >= 1, 
                             "pess leading", ifelse(Goal_Difference_Pessimistic < 0, "pess behind", "pess tie")))%>% 
  mutate(score_type_pessimistic = gsub("pess ", "", score_type_pessimistic))


#optimistic view: you are only behind when the score is against you AND you have even amount of shots taken
print(data.table(all_shootouts)[, .("Mean Scored" = mean(hasScored), .N), by = 
  ifelse(all_shootouts$Goal_Difference_Optimistic >= 1, "opti leading", ifelse(all_shootouts$Goal_Difference_Optimistic < 0, "opti behind", "opti tie"))])

all_shootouts %>% 
  mutate(score_type = ifelse(Goal_Difference_Optimistic >= 1, "opti leading",
                             ifelse(Goal_Difference_Optimistic < 0, "opti behind", "opti tie"))) %>% 
  group_by(score_type) %>% 
  summarise(avg_success = mean(hasScored), count = n()) %>% 
  arrange(avg_success)

all_shootouts = all_shootouts %>% 
  mutate(score_type_optimistic = ifelse(Goal_Difference_Optimistic >= 1,
                                        "opti leading", ifelse(Goal_Difference_Optimistic < 0, "opti behind", "opti tie"))) %>% 
  mutate(score_type_optimistic = gsub("opti ", "", score_type_optimistic))
 
all_shootouts %>% 
  group_by(score_type_pessimistic) %>% 
  summarise(avg_success = mean(hasScored), count = n()) %>% 
  arrange(avg_success) %>% 
  ggplot(aes(x = score_type_pessimistic, y = avg_success)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  geom_text(aes(label = round(avg_success, 3)), vjust = -0.5) + # Add mean values on top of bars
  # geom_hline(aes(yintercept = 0.714, linetype = "M = 0.714"), color = "red") +
  # scale_linetype_manual(name = "", values = "dashed") +
  labs(title = "Pessimistic view: Amount of shots taken is NOT considered",
       x = "Score situation",
       y = "Average success rate") 
#ggsave("plots/Pessimistic viewpoint.png")

all_shootouts %>% 
  group_by(score_type_optimistic) %>% 
  summarise(avg_success = mean(hasScored), count = n()) %>% 
  arrange(avg_success) %>% 
  ggplot(aes(x = score_type_optimistic, y = avg_success)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  geom_text(aes(label = round(avg_success, 3)), vjust = -0.5) + # Add mean values on top of bars
  # geom_hline(aes(yintercept = 0.714, linetype = "M = 0.714"), color = "red") +
  # scale_linetype_manual(name = "", values = "dashed") +
  labs(title = "Optimistic view: Amount of shots taken IS considered",
       x = "Score situation",
       y = "Average success rate") 
#ggsave("plots/Optimistic viewpoint.png")



all_shootouts %>% 
  group_by(Gender, score_type_pessimistic) %>% 
  summarise(avg_success = mean(hasScored), count = n()) %>% 
  ggplot(aes(
    x = score_type_pessimistic,
    y = avg_success,
    fill = Gender
  )) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(avg_success, 3)), position = position_dodge(width = 0.9), hjust = +0.5) +
  labs(title = "Pessimistic view: Amount of shots taken is NOT considered",
       x = "Score situation",
       y = "Average success rate")
#ggsave("plots/gender x pessimisitc on success.png")

all_shootouts %>% 
  group_by(Gender, score_type_optimistic) %>% 
  summarise(avg_success = mean(hasScored), count = n()) %>% 
  ggplot(aes(
    x = score_type_optimistic,
    y = avg_success,
    fill = Gender
  )) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(avg_success, 3)), position = position_dodge(width = 0.9), hjust = +0.5) +
  labs(title = "Optimistic view: Amount of shots taken IS considered",
       x = "Score situation",
       y = "Average success rate")
#ggsave("plots/gender x optimistic on success.png")
#For woman, it is reversed! But small sample size

all_shootouts %>% 
  group_by(Gender, score_type_optimistic) %>% 
  summarise(avg_success = mean(hasScored), count = n())


####### Penalty Shootouts by Decade
data = data.table(all_shootouts)[, .(avg_success = mean(hasScored), .N), by = floor(year(Match_Date) / 10) * 10]
colnames(data)[1] = "Decade"
data$Decade = paste0("[", data$Decade, "-", data$Decade + 10, ")")
data$Decade[match("[2020-2030)", data$Decade)] = "[2020-now)"
ggplot(data, aes(x = Decade, y = N)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Penalty Shots by Decade",
       x = "Decade",
       y = "# Penalty Shots")
#ggsave("plots/shots by decade.png")
ggplot(data, aes(x = Decade, y = avg_success)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  geom_text(aes(label = round(avg_success, 3)), vjust = -0.3) + # Add mean values on top of bars
  labs(title = "Goal success rate by Decade",
       x = "Decade",
       y = "Average success rate")
#ggsave("plots/success_rate by decade.png")


####### results by tournament stage

all_shootouts = all_shootouts %>%
  mutate(Stage = case_when(
    grepl("Quarter-finals", Matchweek) ~ "Quarter-finals",
    grepl("Semi-finals", Matchweek) ~ "Semi-finals",
    grepl("Round of 16", Matchweek) ~ "Round of 16",
    grepl("Final", Matchweek) | grepl("Gold-medal match", Matchweek) ~ "Final",
    grepl("Third-place match", Matchweek) ~ "Third-place match",
    # grepl("Repechage", Matchweek) ~ "Repechage", #sample size too small to look at these special cases
    #  grepl("Fifth-place", Matchweek) ~ "Fifth-place match",
    TRUE ~ NA_character_ # Assign NA to rows that don't match any of the conditions
  ))

success_stage = all_shootouts %>% 
  group_by(Stage) %>% 
  summarise(avg_success = mean(hasScored), count = n()) %>% 
  filter(!is.na(Stage))
# reorder tournament stages
stage_order = c("Round of 16", "Quarter-finals", "Semi-finals", "Third-place match", "Final")
success_stage$Stage = factor(success_stage$Stage, levels = stage_order)


success_stage %>% 
  ggplot(aes(x = Stage, y = avg_success)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  geom_text(aes(label = round(avg_success, 3)), vjust = -0.5) + # Add mean values on top of bars
  geom_hline(aes(yintercept = 0.714, linetype = "M = 0.714"), color = "red") +
  scale_linetype_manual(name = "", values = "dashed") +
  labs(title = "",
       x = "Tournament stage",
       y = "Average success rate") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
#ggsave("plots/stages - success.png")

#difference from mean
# success_stage %>% 
#   mutate(difference = avg_success - 0.714) %>% 
#   ggplot(aes(x = Stage, y = difference)) +
#   geom_bar(stat = "identity", fill = "skyblue", color = "black") +
#   scale_linetype_manual(name = "", values = "dashed") +
#   labs(title = "",
#        x = "Tournament stage",
#        y = "Average success rate") 
# ggsave("plots/stages - success with differences.png")

# df_quarter = all_shootouts[grepl("Quarter-finals", all_shootouts$Matchweek) ,]
# df_semi = all_shootouts[grepl("Semi-finals", all_shootouts$Matchweek) ,]
# df_round16 = all_shootouts[grepl("Round of 16", all_shootouts$Matchweek) ,]
# df_final = all_shootouts[grepl("Final", all_shootouts$Matchweek) | grepl("Gold-medal match", all_shootouts$Matchweek)  ,]
# df_thirdplace= all_shootouts[grepl("Third-place match", all_shootouts$Matchweek)   ,]
# 
# 
# # Combine means into a single dataframe
# means_df = data.frame(
#   stage = c("Overall", "Round of 16", "Quarter-finals", "Semi-finals", "Third-place match", "Final"),
#   avg_Success = c(mean(all_shootouts$hasScored), mean(df_round16$hasScored), 
#                   mean(df_quarter$hasScored), mean(df_semi$hasScored),
#                   mean(df_thirdplace$hasScored), mean(df_final$hasScored))
#   
# )
# 
# means_df$stage = factor(means_df$stage,
#                         levels = c("Overall", "Round of 16", "Quarter-finals",
#                                    "Semi-finals", "Third-place match", "Final"))
# # Create a bar plot
# ggplot(means_df, aes(x = stage, y = avg_Success)) +
#   geom_bar(stat = "identity", fill = "skyblue", color = "black") +
#   geom_text(aes(label = round(avg_Success, 3)), vjust = -0.5) + # Add mean values on top of bars
#   labs(title = "",
#        x = "Tournament stage",
#        y = "Average success rate") 
# ggsave("plots/avg succes for stages.png")


# Combine means into a single dataframe
means_df = data.frame(
  stress = c("Not decisive", "Decisive to win", "Decisive to lose"),
  avg_Success = c(mean(all_shootouts$hasScored[!all_shootouts$Is_Decisive_To_Win & !all_shootouts$Is_Decisive_To_Lose]),
                  mean(all_shootouts$hasScored[all_shootouts$Is_Decisive_To_Win]), 
                  mean(all_shootouts$hasScored[all_shootouts$Is_Decisive_To_Lose])),
  size = c(sum(!all_shootouts$Is_Decisive_To_Win & !all_shootouts$Is_Decisive_To_Lose), 
           sum(all_shootouts$Is_Decisive_To_Win), sum(all_shootouts$Is_Decisive_To_Lose))
) # TODO maybe plot size

# Create a bar plot
ggplot(means_df, aes(x = stress, y = avg_Success)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  geom_text(aes(label = round(avg_Success, 3)), vjust = -0.5) + # Add mean values on top of bars
  labs(title = "",
       x = "Stress level",
       y = "Average success rate")

#ggsave("plots/avg success for stress.png")

all_shootouts = all_shootouts %>% 
  mutate(decisive = ifelse(Is_Decisive_To_Win, "Winning decisive", "Not decisive")) %>% 
  mutate(decisive = ifelse(Is_Decisive_To_Lose, "Losing decisive", decisive))
all_shootouts %>% 
  group_by(Gender, decisive) %>% 
  summarise(avg_success = mean(hasScored), count = n()) %>% 
  ggplot(aes(
    x = decisive,
    y = avg_success,
    fill = factor(Gender)
  )) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(avg_success, 3)), position = position_dodge(width = 0.9), hjust = 0.3) +
  labs(title = "",
       #  subtitle = "Numbers on top of bars represent number of shots in database",
       x = "",
       y = "Average success rate",
       fill = "Gender")
#ggsave("plots/avg success for stress x gender.png")


all_shootouts %>% 
  group_by(Gender, decisive) %>% 
  summarise(avg_success = mean(hasScored), count = n())

####
# Calculate average success rates and sample size by League and Gender
success_rates = all_shootouts %>%
  group_by(League, Gender) %>%
  summarise(avg_success = mean(hasScored), count = n()) %>%
  ungroup()

overall_mean = mean(all_shootouts$hasScored)

success_rates = success_rates %>%
  mutate(diff_from_mean = avg_success - overall_mean)

success_rates$League_with_count = paste(success_rates$League, "(N =", success_rates$count, ")")

success_rates %>% 
  ggplot(aes(
    x = reorder(League_with_count, diff_from_mean),
    y = diff_from_mean,
    fill = factor(Gender)
  )) +
  geom_bar(stat = "identity", position = "dodge") +
  # geom_text(aes(label = count), position = position_dodge(width = 0.9), hjust = -0.3) +
  labs(title = "",
       #  subtitle = "Numbers on top of bars represent number of shots in database",
       x = "Penalty Shots by League",
       y = "Difference from Overall Mean (M = 0.714)",
       fill = "Gender") +
  
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip()
#ggsave("plots/Difference from Mean by League and Gender.png")



aggregate(hasScored ~ Gender, all_shootouts, mean)
round(aggregate(hasScored ~ Gender, all_shootouts, mean)[1,2] - aggregate(hasScored ~ Gender, all_shootouts, mean)[2,2],3)
#3.4% difference between men and woman



#TODO

#### success rate by shot number

success_shotNumber = all_shootouts %>% 
  group_by(Penalty_Number) %>% 
  summarise(avg_success = mean(hasScored), count = n())

success_shotNumber %>% 
  ggplot(aes(x = Penalty_Number, y = avg_success)) +
  geom_bar(stat = "identity", position = "dodge", fill = "skyblue", color = "black") +
  geom_hline(aes(yintercept = 0.714, linetype = "M = 0.714"), color = "red") +
  scale_linetype_manual(name = "", values = "dashed") +
  geom_text(aes(label = count), position = position_dodge(width = 0.9), hjust = +0.5) +
  labs(x = "Penalty Number within a shootout",
       y = "Average success rate",
       subtitle = "Samplesize of each shot number indicated by number on top of bar")

#ggsave("plots/success rate by penalty number.png")



#################################### stress influence on success rate grouped by  gender  ###################
success_gender_stage = all_shootouts %>% 
  group_by(Gender, Stage) %>% 
  summarise(avg_success = mean(hasScored), count = n()) %>% 
  filter(!is.na(Stage)) %>% 
  arrange(avg_success)
success_gender_stage

# reorder tournament stages
stage_order = c("Round of 16", "Quarter-finals", "Semi-finals", "Third-place match", "Final")
success_gender_stage$Stage = factor(success_gender_stage$Stage, levels = stage_order)


ggplot(success_gender_stage, aes(
  x = Stage,
  y = avg_success,
  fill = factor(Gender)
)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = count), position = position_dodge(width = 0.9), hjust = +0.3) +
  labs(subtitle = "Samplesize indicated by number on top of the bar",
       y = "Average Success Rate",
       fill = "Gender") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_hline(aes(yintercept = 0.714, linetype = "M = 0.714"), color = "red") +
  scale_linetype_manual(name = "", values = "dashed") 
#ggsave("plots/gender x stage on success.png")

####################### influence of starting vs non starting ###################

all_shootouts %>% 
  group_by(Gender, team_started_shootout) %>% 
  summarise(avg_success = mean(hasScored), count = n())

# all_shootouts %>% 
#   group_by(Gender, team_started_shootout) %>% 
#   summarise(avg_success = mean(hasScored), count = n()) %>% 
#   ggplot(aes(
#     x = Gender,
#     y = avg_success,
#     fill = factor(team_started_shootout)
#   )) +
#     geom_bar(stat = "identity", position = "dodge") +
#     geom_text(aes(label = round(avg_success, 3)), position = position_dodge(width = 0.9), hjust = +0.3) +
#     labs(subtitle = "Samplesize indicated by number on top of the bar",
#          y = "Average Success Rate",
#          fill = "Starts shootout") +
#     geom_hline(aes(yintercept = 0.714, linetype = "M = 0.714"), color = "red") +
#     scale_linetype_manual(name = "", values = "dashed") 

#this is wrong, dont need the avg success rate over all shots here but if the team won the shootout or not
#TODO finish this
#TODO success rate per decade

 all_shootouts %>% 
  mutate(team_hasWon = NA) %>% 
  mutate(team_hasWon = ifelse(Is_Decisive_To_Win & hasScored, TRUE, team_hasWon)) %>% 
  mutate(team_hasWon = ifelse(Is_Decisive_To_Lose & !hasScored, FALSE, team_hasWon)) %>% 
   group_by(team_hasWon) %>% 
   summarise(count = n())


############################### Modelling ###################################

# all_shootouts %>% 
#   mutate(shot_type = "normal") %>% 
#   mutate(shot_type = ifelse(Is_Decisive_To_Win), "decisive to win", shot_type) %>% 
#   mutate(shot_type = ifelse(Is_Decisive_To_Lose), "decisive to lose", shot_type)



# Create a new column for the non-gendered version of the tournament league
all_shootouts = all_shootouts %>%
  mutate(Non_Gendered_League = case_when(
    League %in% c("AFC Asian Cup", "AFC Women's Asian Cup") ~ "AFC Asian Cup",
    League %in% c("Africa Cup of Nations", "Africa Women Cup of Nations") ~ "Africa Cup",
    League %in% c("CONCACAF Gold Cup", "CONCACAF Women's Championship") ~ "CONCACAF Championship",
    League %in% c("CONMEBOL Copa América", "CONMEBOL Copa América Centenario", "Copa América Femenina") ~ "Copa América",
    League %in% c("FIFA Women's World Cup", "FIFA World Cup") ~ "FIFA World Cup",
    League %in% c("FIFA Confederations Cup") ~ "FIFA Confed Cup",
    League %in% c("Olympics - Men's Tournament", "Olympics — Women's Tournament") ~ "Olympics",
    League %in% c("UEFA European Football Championship", "UEFA Women's Championship") ~ "UEFA Euros",
    League == "UEFA Nations League" ~ "UEFA Nations League",
    TRUE ~ League
  ))


table(all_shootouts$Non_Gendered_League)


#TODO
#need penalty number has an ordered factor
all_shootouts = all_shootouts %>%
  mutate(Penalty_Number = factor(Penalty_Number, ordered = TRUE))
str(all_shootouts$Penalty_Number)


#add starting
mdl = glm(hasScored ~ Gender + Non_Gendered_League + Stage + Is_Decisive_To_Win  + Is_Decisive_To_Lose + Penalty_Number +
            Goal_Difference_Optimistic,
          family = binomial(), data = all_shootouts)
summary(mdl)
anova(mdl, test = "LRT")


all_shootouts %>% 
  group_by(Gender, decisive) %>% 
  summarise(count =n())



  
table(sapply(shootouts, function(x) x$Gender[1]))
table(sapply(shootouts, nrow))



