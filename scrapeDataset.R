
# Download the latest version of our package to scrape fbref.com, as CRAN-version outdated
# devtools::install_github("https://github.com/JaseZiv/worldfootballR")
library(worldfootballR)
library(plyr)

########################################################################################################################
##### Scrape all cup match results - Warning: can take some time, can also skip this and load pre-scraped data  ########
########################################################################################################################


# which cups to scrape from fbref.com
# the URL for Cups and Competitions found at https://fbref.com/en/comps/
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

# years interval to scrape for each cup, if found
years = 1982:2023
for (cup in cups) {
  message("Scraping cup: ", cup)
  for (year in years) {
    message("Scraping year: ", year)
    
    # year + cupname
    id = paste0(year, "_", gsub("https://fbref.com/en/comps/[0-9]*/history/", "", cup))
    dir.create("data/fbref_match_results/", showWarnings = F, recursive = T)
    
    file_male = paste0("data/fbref_match_results/M_", id, ".rds")
    if(!file.exists(file_male)) {
      tryCatch({
        # Scrape match results for any country, any tier of competition, given year, gender and cup.
        results = fb_match_results(country = "", gender = "M", season_end_year = year, 
                                   tier = "", non_dom_league_url = cup)
        saveRDS(results, file_male)
        
      }, error = function(e) {
        message("Error encountered: ", e$message)
        
        # If there was no cup in the year, we save a NULL file, to not repeat scraping.
        if(e$message == "Data not available for the season(s) selected") {
          saveRDS(NULL, file_male)
        }
        return()
      })
      # Avoid Too-Many-Requests HTTP error
      Sys.sleep(5)
    }
    
    # #same for woman
    file_female = paste0("data/fbref_match_results/F_", id, ".rds")
    if(!file.exists(file_female)) {
      tryCatch({
        results = fb_match_results(country = "", gender = "F", season_end_year = year, tier = "", non_dom_league_url = cup)
        saveRDS(results, file_female)
        
      }, error = function(e) {
        message("Error encountered: ", e$message)
        if(e$message == "Data not available for the season(s) selected") {
          saveRDS(NULL, file_female)
        }
        return()
      })
      Sys.sleep(5)
    }
  }
}


########################################################################################################################
##### Scrape game histories for those matches which ended in a penalty shootout ############################
########################################################################################################################

# Scraping all matches would take too long, hence we filter matches beforehand that ended 
# in a shootout, identifiable by a specific string in variable "Notes".


all_results = do.call(rbind.fill, lapply(list.files("data/fbref_match_results", full.names = T), readRDS))
# csv dataset for publication for match results
write.csv(all_results, "data/all_results.csv", row.names = F) 

results = all_results[grepl("won on penalty kicks following", all_results$Notes),] #this identifies all penalty shootouts
ids = sub(".*/matches/([^/]+)/.*", "\\1", results$MatchURL) # extract hexadecimal matchId for filename
for (i in seq_along(ids)) {
  dir.create("data/fbref_match_summary/", showWarnings = F, recursive = T)
  file = paste0("data/fbref_match_summary/", ids[i], ".rds")
  if(!file.exists(file)) {
    
    tryCatch({
      # Scrape all the events (shots) that happened in a specific match
      events = fb_match_summary(results$MatchURL[i], time_pause = 5)
      saveRDS(events, file)
      
    }, error = function(e) {
      message("Error encountered: ", e$message)
      # In case of unexpected error, we write NULL file, to not scrape match again.
      saveRDS(NULL, file)
      return()
    })
  }
}

# csv dataset for publication for shots
files = list.files("data/fbref_match_summary", full.names = T)
shots = do.call(rbind.fill, lapply(files, readRDS))
write.csv(shots, "data/all_shots.csv", row.names = F)

