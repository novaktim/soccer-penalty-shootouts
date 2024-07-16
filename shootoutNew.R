
#devtools::install_github("https://github.com/JaseZiv/worldfootballR")

library(worldfootballR)
library(plyr)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(data.table)
library(readxl)


########################################################################################################################
##### Scrape all cup match results - Warning: can take some time, can also skip this and load pre-scraped data  ########
########################################################################################################################


# non_dom_league_url the URL for Cups and Competitions found at https://fbref.com/en/comps/
cups = c("https://fbref.com/en/comps/685/history/Copa-America-Seasons",
         "https://fbref.com/en/comps/158/history/Copa-America-Femenina-Seasons",
         "https://fbref.com/en/comps/676/history/European-Championship-Seasons",
         "https://fbref.com/en/comps/677/history/UEFA-Nations-League-Seasons",
         "https://fbref.com/en/comps/162/history/UEFA-Womens-Euro-Seasons",
         "https://fbref.com/en/comps/664/history/Asian-Cup-Seasons",
         "https://fbref.com/en/comps/161/history/AFC-Womens-Asian-Cup-Seasons",
         "https://fbref.com/en/comps/1/history/World-Cup-Seasons",
         "https://fbref.com/en/comps/666/history/FIFA-Confederations-Cup-Seasons",
         "https://fbref.com/en/comps/106/history/Womens-World-Cup-Seasons",
         "https://fbref.com/en/comps/180/history/Olympics-W-Seasons",
         "https://fbref.com/en/comps/656/history/Africa-Cup-of-Nations-Seasons",
         "https://fbref.com/en/comps/156/history/Africa-Women-Cup-of-Nations-Seasons",
         "https://fbref.com/en/comps/681/history/Gold-Cup-Seasons",
         "https://fbref.com/en/comps/157/history/CONCACAF-W-Championship-Seasons")




years = 1982:2023
for (cup in cups) {
  message("Scraping cup: ", cup)
  for (year in years) {
    message("Scraping year: ", year)
    
    id = paste0(year, "_", gsub("https://fbref.com/en/comps/[0-9]*/history/", "", cup))
    dir.create("data/fbref_match_results/", showWarnings = F, recursive = T)
    
    file_M = paste0("data/fbref_match_results/M_", id, ".rds")
    if(!file.exists(file_M)) {
      tryCatch({
        results = fb_match_results(country = "", gender = "M", season_end_year = year, tier = "", non_dom_league_url = cup)
        
        saveRDS(results, file_M)
        
      }, error = function(e) {
        message("Error encountered: ", e$message)
        if(e$message == "Data not available for the season(s) selected") {
          saveRDS(NULL, file_M)
        }
        return()
      })
      Sys.sleep(5)
    }
    
    # #same for woman
    file_F = paste0("data/fbref_match_results/F_", id, ".rds")
    if(!file.exists(file_F)) {
      tryCatch({
        results = fb_match_results(country = "", gender = "F", season_end_year = year, tier = "", non_dom_league_url = cup)
        saveRDS(results, file_F)
        
      }, error = function(e) {
        message("Error encountered: ", e$message)
        if(e$message == "Data not available for the season(s) selected") {
          saveRDS(NULL, file_F)
        }
        return()
      })
      Sys.sleep(5)
    }
  }
}


########################################################################################################################
##### Load scraped game histories to find those which ended in a penalty shootout ############################
########################################################################################################################



all_results = do.call(rbind.fill, lapply(list.files("data/fbref_match_results", full.names = T), readRDS))
results = all_results[grepl("won on penalty kicks following", all_results$Notes),] #this identifies all penalty shootouts
ids = sub(".*/matches/([^/]+)/.*", "\\1", results$MatchURL)
for (i in seq_along(ids)) {
  dir.create("data/fbref_match_summary/", showWarnings = F, recursive = T)
  file = paste0("data/fbref_match_summary/", ids[i], ".rds")
  if(!file.exists(file)) {
    
    tryCatch({
      events = fb_match_summary(results$MatchURL[i], time_pause = 5)
      saveRDS(events, file)
      
    }, error = function(e) {
      message("Error encountered: ", e$message)
      saveRDS(NULL, file)
      return()
    })
  }
}



# Percentage of games that lead to a Penalty Shootout by League -> excluded Men's Olympics tournament, here we only have shootouts

tab = table(all_results$Round[grepl("won on penalty kicks following", all_results$Notes)])
print(tab)
# Remove games where no shootout possible by rule
results = data.table(all_results[all_results$Round %in% names(tab),])
relative_shootouts = results[, .(Shootouts = mean(grepl("won on penalty kicks following", Notes)) * 100), by = "Competition_Name"]

ggplot(relative_shootouts, aes(y = reorder(Competition_Name, Shootouts), x = Shootouts)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  #theme_minimal() +
  labs(title = "% Penalty Shootout happening by League",
       x = "Percentage of matches",
       y = "League")
#ggsave("percentage of shootout by league.png")


##### Load in all penalty shootouts #####


files = list.files("data/fbref_match_summary", full.names = T)
shots = do.call(rbind.fill, lapply(files, readRDS))
shots$Gender = all_results$Gender[match(shots$Game_URL, all_results$MatchURL)]
shots = shots[order(shots$Game_URL, shots$Penalty_Number),]
shots = shots[shots$Event_Type == "Penalty Shootout", ]
# Add manually-entered Men Olympics shootouts which are missing on fbref.com
olympics = readxl::read_xlsx("data/olympics.xlsx")
olympics$Home_Away = ifelse(olympics$Penalty_Number %% 2 == 1, "Home", "Away")
shots = rbind.fill(olympics, shots)
stopifnot(!is.na(shots$Home_Away))

shootouts = split(shots, shots$Game_URL)


# Count the years of shootouts
matchdates = as.Date(sapply(shootouts, function(x) x$Match_Date[1]))
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
#ggsave("shootouts by year.png")

# Create the bar plot by League
tab = table(leagues)
data = data.frame(League = names(tab), Count = as.numeric(tab))
data = data[order(data$Count),]
ggplot(data, aes(y = reorder(League, Count), x = Count)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Penalty Shootouts by League",
       x = "# Penalty Shootouts",
       y = "League")
#ggsave("shootouts by league.png")

# Create the bar plot by Gender
# tab = table(genders)
# data = data.frame(Gender = ifelse(names(tab) == "F", "Female", "Male"), Count = as.numeric(tab))
# ggplot(data, aes(x = Gender, y = Count)) +
#   geom_bar(stat = "identity", fill = "skyblue", color = "black") +
#   labs(title = "Penalty Shootouts by Gender",
#        x = "Gender",
#        y = "# Penalty Shootouts")
#ggsave("shootouts by gender.png")


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
ind = which(sapply(shootouts, function(x) any(x$Is_Decisive_To_Win)))
ind = which(sapply(shootouts, function(x) any(x$Is_Decisive_To_Lose)))
for (i in sample(ind, 5)) {
  View(shootouts[[i]])
}


all_shootouts = do.call(rbind, shootouts)
all_shootouts$hasScored = all_shootouts$Has_Scored_Home | all_shootouts$Has_Scored_Away
all_shootouts$team_started_shootout = all_shootouts$Penalty_Number %% 2 == 1
print(mean(all_shootouts$hasScored))
print(data.table(all_shootouts)[, .("Mean True Win" = mean(Is_Decisive_To_Win), 
                                    "Mean True Lose" = mean(Is_Decisive_To_Lose)), by = c("Home_Away", "team_started_shootout")])


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
stage_order <- c("Round of 16", "Quarter-finals", "Semi-finals", "Third-place match", "Final")
success_stage$Stage <- factor(success_stage$Stage, levels = stage_order)


success_stage %>% 
  ggplot(aes(x = Stage, y = avg_success)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  geom_text(aes(label = round(avg_success, 3)), vjust = -0.5) + # Add mean values on top of bars
  geom_hline(aes(yintercept = 0.714, linetype = "M = 0.714"), color = "red") +
  scale_linetype_manual(name = "", values = "dashed") +
  labs(title = "",
       x = "Tournament stage",
       y = "Average success rate") 
#ggsave("stages - success.png")

#difference from mean
success_stage %>% 
  mutate(difference = avg_success - 0.714) %>% 
  ggplot(aes(x = Stage, y = difference)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  scale_linetype_manual(name = "", values = "dashed") +
  labs(title = "",
       x = "Tournament stage",
       y = "Average success rate") 
ggsave("stages - success with differences.png")

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
# ggsave("avg succes for stages.png")


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
ggsave("avg succes for stress.png")


####
# Calculate average success rates and sample size by League and Gender
success_rates = all_shootouts %>%
  group_by(League, Gender) %>%
  summarise(avg_success = mean(hasScored), count = n()) %>%
  ungroup()

overall_mean = mean(all_shootouts$hasScored)

success_rates = success_rates %>%
  mutate(diff_from_mean = avg_success - overall_mean)

success_rates$League_with_count <- paste(success_rates$League, "(N =", success_rates$count, ")")

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
ggsave("Difference from Mean by League and Gender.png")



aggregate(hasScored ~ Gender, all_shootouts, mean)
round(aggregate(hasScored ~ Gender, all_shootouts, mean)[1,2] - aggregate(hasScored ~ Gender, all_shootouts, mean)[2,2],3)
#3.4% difference between men and woman

##################### add mens olympic
#tmp = all_shootouts %>% filter(League == "Olympics - Men's Tournament")
#xlsx::write.xlsx(tmp, "olympics.xlsx", row.names = F)

# olympics_men = xlsx::read.xlsx("olympics.xlsx", sheetIndex = 1)
# all_shootouts = rbind(all_shootouts, olympics_men)
# all_shootouts %>% filter(League == "Olympics - Men's Tournament") %>% 
#   select(League, Gender, Match_Date, Score_Progression)


#missing
#results per number of shot
#stress * gender
#model
#per decade

#### success rate by shot number

success_shotNumber = all_shootouts %>% 
  group_by(Penalty_Number) %>% 
  summarise(avg_success = mean(hasScored), count = n())

success_shotNumber %>% 
  ggplot(aes(x = Penalty_Number, y = avg_success)) +
  geom_bar(stat = "identity", position = "dodge", fill = "skyblue", color = "black") +
  geom_hline(aes(yintercept = 0.714, linetype = "M = 0.714"), color = "red") +
  scale_linetype_manual(name = "", values = "dashed") +
  geom_text(aes(label = count), position = position_dodge(width = 0.9), hjust = +0.3) +
  labs(x = "Penalty Number within a shootout",
       y = "Average success rate",
       subtitle = "Samplesize of each shot number indicated by number on top of the bar")

ggsave("success rate by penalty number.png")



#stress x gender



success_gender_stage = all_shootouts %>% 
  group_by(Gender, Stage) %>% 
  summarise(avg_success = mean(hasScored), count = n()) %>% 
  filter(!is.na(Stage)) %>% 
  arrange(avg_success)
success_gender_stage

# reorder tournament stages
stage_order <- c("Round of 16", "Quarter-finals", "Semi-finals", "Third-place match", "Final")
success_gender_stage$Stage <- factor(success_gender_stage$Stage, levels = stage_order)


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
ggsave("gender x stage on success.png")



############################### Modelling ###################################

# all_shootouts %>% 
#   mutate(shot_type = "normal") %>% 
#   mutate(shot_type = ifelse(Is_Decisive_To_Win), "decisive to win", shot_type) %>% 
#   mutate(shot_type = ifelse(Is_Decisive_To_Lose), "decisive to lose", shot_type)

#TODO
#need penalty number has an ordered factor, maybe seperated by team?
mdl = glm(hasScored ~  Is_Decisive_To_Win  + Is_Decisive_To_Lose + Penalty_Number,
          family = binomial(), data = all_shootouts)
summary(mdl)
anova(mdl)