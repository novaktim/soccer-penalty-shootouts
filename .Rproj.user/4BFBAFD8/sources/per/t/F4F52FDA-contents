
devtools::install_github("https://github.com/JaseZiv/worldfootballR")
library(worldfootballR)
library(plyr)

shots_list = lapply(c("EPL", "La liga", "Bundesliga", "Serie A", "Ligue 1", "RFPL"), worldfootballR::load_understat_league_shots)
shots = do.call(rbind.fill, shots_list)
shots = shots[shots$situation == "Penalty",]



test = worldfootballR::fb_match_report("https://fbref.com/en/matches/56228480/Italy-England-July-11-2021-European-Championship")
test = worldfootballR::fb_advanced_match_stats("https://fbref.com/en/matches/56228480/Italy-England-July-11-2021-European-Championship", stat_type = "misc", "player")
test = worldfootballR::fb_match_summary("https://fbref.com/en/matches/56228480/Italy-England-July-11-2021-European-Championship")
test = worldfootballR::fb_match_shooting("https://fbref.com/en/matches/56228480/Italy-England-July-11-2021-European-Championship")

test2 = worldfootballR::load_fb_match_summary("GER", "M", "1st")
test2 = worldfootballR::load_fb_match_summary(c("ITA", "ENG"), "M", "1st")


# non_dom_league_url the URL for Cups and Competitions found at https://fbref.com/en/comps/

##### Scrape all cup match results #####
cups = c("https://fbref.com/en/comps/8/history/Champions-League-Seasons",
         "https://fbref.com/en/comps/19/history/Europa-League-Seasons",
         "https://fbref.com/en/comps/882/history/Europa-Conference-League-Seasons",
         "https://fbref.com/en/comps/122/history/UEFA-Super-Cup-Seasons")

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
          "https://fbref.com/en/comps/157/history/CONCACAF-W-Championship-Seasons"
          
          )
 

years = 1982:2023
for (cup in cups) {
  message("Scraping cup: ", cup)
  for (year in years) {
    message("Scraping year: ", year)
    
    id = paste0(year, "_", gsub("https://fbref.com/en/comps/[0-9]*/history/", "", cup))
    file = paste0("data/fbref_match_results/", id, ".rds")
    if(!file.exists(file)) {
      file_M = paste(file, "_M")
      file_F = paste(file, "_F")
      tryCatch({
        results = fb_match_results(country = "", gender = "M", season_end_year = year, tier = "", non_dom_league_url = cup)
        
        saveRDS(results, file_M)
        
      }, error = function(e) {
        message("Error encountered: ", e$message)
        saveRDS(NULL, file_M)
        return()
      })
      Sys.sleep(5)
      
  
      # #same for woman
      tryCatch({
        results = fb_match_results(country = "", gender = "F", season_end_year = year, tier = "", non_dom_league_url = cup)
        saveRDS(results, file_F)

      }, error = function(e) {
        message("Error encountered: ", e$message)
        saveRDS(NULL, file_F)
        return()
      })
      Sys.sleep(5)
    }
  }
}



##### Scrape only matches with a penalty shootout #####
library(plyr)
setwd("C:/Users/Helga/Desktop/Sports Data/Shootout")
results = do.call(rbind.fill, lapply(list.files("data/fbref_match_results", full.names = T), readRDS))
results = results[endsWith(results$Notes, "won on penalty kicks following extra time"),]
ids = sub(".*/matches/([^/]+)/.*", "\\1", results$MatchURL)
for (i in seq_along(ids)) {
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


##### Load in all penalty shootouts #####
library(plyr)
library(ggplot2)
shots = do.call(rbind.fill, lapply(list.files("data/fbref_match_summary", full.names = T), readRDS))


shootouts = shots[shots$Event_Type == "Penalty Shootout", ]

# Create the data frame from the provided data
years <- c(1982, 1986, 1990, 1994, 1995, 1998, 2000, 2002, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023)
counts <- c(1, 3, 4, 3, 1, 3, 1, 2, 5, 1, 4, 4, 2, 1, 3, 4, 4, 6, 5, 10, 8, 4, 8, 12, 3, 12, 11, 14)

data <- data.frame(Year = years, Count = counts)

# Create the bar plot using ggplot2
ggplot(data, aes(x = Year, y = Count)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  #theme_minimal() +
  labs(title = "Penalty Shootouts by Year",
       x = "Year",
       y = "# Penalty Shootouts")
ggsave("shootouts by year.png")


##

shootouts$hasScored = NA
isGoal = function(data) {
  
  matches = names(table(data$Match_Date)) 
  
  #iterate through matches
  for(i in 1:length(matches)){
    
    current_match = data[data$Match_Date == matches[i],]
    if(length(table(current_match$Team)) != 2){ #multiple shootouts happened on the same date
      print("something went wrong here")
      print(length(table(current_match$Team)))
      
      
    cups  = names(table(data$Match_Date)) 
      next
    }
    
    #iterate through score progression
    first_pen = current_match[current_match$Penalty_Number == 1, ]
    
    data[data$Match_Date == matches[i] & data$Penalty_Number == 1, "hasScored"] = 
      first_pen[, "Score_Progression"] != "0:0"
    #0:0 after first penalty means that the player did not score
    
    for(j in 2:(nrow(current_match))) {
      current_pen = current_match[current_match$Penalty_Number == j, ]
      
      
      prev_pen = current_match[current_match$Penalty_Number == (j-1), ]
      
      #If the score progression changed form one penalty to the next, it means the player has scored
      if(current_pen$Score_Progression == prev_pen$Score_Progression){

        data[data$Match_Date == matches[i] & data$Penalty_Number == j, "hasScored"] = FALSE
      } else {
        data[data$Match_Date == matches[i] & data$Penalty_Number == j, "hasScored"] = TRUE
      }
    }
  }
 return(data)
}
test = isGoal(shootouts)
shootouts = isGoal(shootouts)

#some are missing right now

mean(test$hasScored, na.rm = T)


#TODO isDecisice, fix NA, make some simple plots and statistics



df_quarter = shootouts[grepl("Quarter-finals", shootouts$Matchweek) ,]
df_semi = shootouts[grepl("Semi-finals", shootouts$Matchweek) ,]
df_round16 = shootouts[grepl("Round of 16", shootouts$Matchweek) ,]
df_final = shootouts[grepl("Final", shootouts$Matchweek) | grepl("Gold-medal match", shootouts$Matchweek)  ,]
df_thirdplace= shootouts[grepl("Third-place match", shootouts$Matchweek)   ,]

probs = list("overall" = mean(shootouts$hasScored, na.rm = T))

# Combine means into a single dataframe
means_df <- data.frame(
  stage = c("Overall", "Round of 16", "Quarter-finals", "Semi-finals", "Third-place match", "Final"),
  avg_Success = c(mean(shootouts$hasScored, na.rm = T), mean(df_round16$hasScored, na.rm = T), 
                 mean(df_quarter$hasScored, na.rm = T), mean(df_semi$hasScored, na.rm = T),
                 mean(df_thirdplace$hasScored, na.rm = T), mean(df_final$hasScored, na.rm = T))
  
)

means_df$stage = factor(means_df$stage,
                        levels = c("Overall", "Round of 16", "Quarter-finals",
                                   "Semi-finals", "Third-place match", "Final"))
# Create a bar plot
ggplot(means_df, aes(x = stage, y = avg_Success)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = round(avg_Success, 2)), vjust = -0.5) + # Add mean values on top of bars
  #theme_minimal() +
  labs(title = "",
       x = "Tournament stage",
       y = "Average success rate")
ggsave("avg succes for stages.png")
