#' This script will fetch data from the fan wiki pages of popular CBS game shows:
#' Amazing Race, Big Brother, and Survivor. It will also fetch the list of all
#' the names of black players. Then it will compile the the standings of all
#' the players from every season and annotate whether the player is black.
#' 
#' This data is then used in the analyzed_data.R script to analyze the standings
#' of black vs not black players.
#' 
#' @import tidyverse, rvest
#' @author George Gruenhagen

##############################################################################
# Amazing Race ===============================================================
##############################################################################

# Load Libraries
library("tidyverse")
library("rvest")

## Fetch Data ================================================================

# Amazing Race Season Names
setwd("/mnt/c/Users/ggruenhagen/Downloads/game_show/data/html_amazing_race/")
season_names = read.csv("../amazing_race_season_names.csv", header = F)[,1]
season_names_html = gsub(" ", "_", season_names)
season_names_html = paste0("_", season_names_html)

# Scrape all the season pages
for (season in season_names_html) {
    system(paste0("wget https://amazingrace.fandom.com/wiki/", season))
}

## Parse Data ================================================================

# Get all the names of african american teams
black_players = rvest::read_html("Category_African-American_Teams")
black_players = black_players %>% html_nodes("a.category-page__member-link") %>% html_attr("title")
black_players = gsub("/.*", "", black_players) # remove any characters after / from the names
black_players = unique(black_players)

# Parse the html files for the cast of each season
amazing_race_df = data.frame()
for (i in 1:length(season_names_html)) {
    season = season_names_html[i]
    tables = read_html(season) %>% html_nodes("table")
    table_idx = 3
    cast = html_table(tables[table_idx], fill = TRUE) %>% as.data.frame
    if (season %in% c("_The_Amazing_Race_33")) { cast = head(cast, 12); colnames(cast) = cast[1,]; cast = cast[2:nrow(cast),]; }
    cast = cast[which( !grepl("Legend:", cast$Team) & !grepl("Notes", cast$Team) ),]
    cast = cast[, c("Place", "Team", "Average")]
    cast$player_name = cast$Team
    cast$race = "not_black"
    cast$race[cast$Team %in% black_players] = "black"
    cast$season = season_names[i]
    cast$season_n = i
    cast$lose_rank = nrow(cast):1
    cast$win_rank = cast$Place
    cast_lose_quantile = ecdf(cast$lose_rank)
    cast$lose_quantile = cast_lose_quantile(cast$lose_rank)
    amazing_race_df = rbind(amazing_race_df, cast)
}

# Save Data
write.csv(amazing_race_df, "../../amazing_race_standings.csv", row.names=F)

##############################################################################
# Big Brother ================================================================
##############################################################################

# Load Libraries
library("tidyverse")
library("rvest")

## Fetch Data ================================================================

# Big Brother Season Names
setwd("/mnt/c/Users/ggruenhagen/Downloads/game_show/data/html_big_brother/")
season_names = read.csv("../big_brother_season_names.csv", header = F)[,1]
season_names_html = gsub(" ", "_", season_names)
season_names_html = gsub(":", "", season_names_html)
season_names_html_escape = gsub("\\(", "\\\\(", season_names_html)
season_names_html_escape = gsub("\\)", "\\\\)", season_names_html_escape)

# Scrape all the season pages
str_to_write = c("#!/bin/bash")
for (season in season_names_html_escape) {
    str_to_write = c(str_to_write, paste0("wget https://bigbrother.fandom.com/wiki/", season))
}
fileConn<-file("wget_script.sh")
writeLines(str_to_write, fileConn)
close(fileConn)
system("/mnt/c/Users/ggruenhagen/Downloads/game_show/data/html_big_brother/wget_script.sh")

## Parse Data ================================================================

# Get all names of all the african american contestants
black_players = rvest::read_html("Category_African-American_Contestants")
black_players = black_players %>% html_nodes("a.category-page__member-link") %>% html_attr("title")
black_players = gsub("/.*", "", black_players) # remove any characters after / from the names
black_players = unique(black_players)

# Parse the html files for the cast of each season
big_brother_df = data.frame()
for (i in 1:length(season_names_html)) {

    # Don't analyze celebrity big brother seasons or season 9
    season = season_names_html[i]
    is_celebrity = F
    if (grepl("Celebrity", season)) { print(paste0("WARNING: excluding celebrity big brother season [", season, "]")); is_celebrity=T; }

    if ( !i %in% c(9) & !is_celebrity ) {

        # Read the correct table from the html files
        tables = read_html(season) %>% html_nodes("table")
        table_class = unlist(lapply(1:length(tables), function(x) html_attr(tables[x], "class")))
        table_idx = which(table_class == "wikitable")
        table_idx = table_idx[which(table_idx < 25)]
        table_idx = table_idx[length(table_idx)]
        if (season %in% c("Big_Brother_1_(US)"))   { table_idx = 6 }

        # Get the cast names from the descriptions in the cast pictures
        cast_full_names = read_html(season) %>% html_nodes("img")
        cast_full_names = html_attrs(cast_full_names)
        cast_full_names = do.call('rbind', cast_full_names)
        cast_width = "100"
        cast_height = "125"
        if (i %in% c(13, 22, 24, 25)) { 
            cast_full_names = cast_full_names[which(cast_full_names[,"alt"] == cast_width & grepl(" ", cast_full_names[,"data-src"]) & !grepl('\"', cast_full_names[,"data-src"])), c("data-src")]
        } else {
            cast_full_names = cast_full_names[which(cast_full_names[,"width"] == cast_width & grepl(" ", cast_full_names[,"alt"]) & !grepl('\"', cast_full_names[,"alt"])), c("alt")]
        }
        cast_full_names = unique(cast_full_names)
        if (i == 2) { cast_full_names = c(cast_full_names, "Mike Malin") }
        cast_first_names = cast_full_names %>% str_split_i(" ", 1)
        if (length(cast_first_names) != length(unique(cast_first_names))) { message(paste0("Warning: not all first names are unique [", season, "]")) }

        # Manual entry of cast names and results for seasons with html pages that are too hard to parse
        if (i == 4) {
            cast = data.frame(  Result = c("Amanda", "Michelle", "David", "Dana", "Nathan", "Justin", "Jack", "Jee", "Erika", "Robert", "Alison", "Jun"), Result.1 = "")
        } else if(i == 6) {
            cast = data.frame(  Result = c("Ashlea", "Michael", "Eric", "Kaysar", "Sarah", "Kaysar", "Jennifer", "Rachel", "James", "Beau", "Howie", "April", "Janelle", "Ivette", "Maggie"), Result.1 = "" )
        } else if (i == 11) {
            cast = data.frame(  Result = c("Brandon", "Laura", "Casey", "Ronnie", "Jessie", "Chisma", "Lydia", "Russell", "Jeff", "Michelle", "Kevin", "Natalie", "Jordan"), Result.1 = "" )
        } else if (i == 12) {
            cast = data.frame(  Result = c("Annie", "Monet", "Andrew", "Kristen", "Rachel", "Kathy", "Matt", "Brendon", "Ragan", "Britney", "Enzo", "Lane", "Hayden"), Result.1 = "" )
        } else if (i == 13) {
            cast = data.frame(  Result = c("Dick", "Keith", "Cassi", "Dominic", "Brendon", "Lawon", "Brendon", "Daniele", "Jeff", "Shelly", "Kalia", "Jordan", "Adam", "Porsche", "Rachel"), Result.1 = "" )
        } else {
            cast = html_table(tables[table_idx], fill = TRUE) %>% as.data.frame
            cast =  cast[2:nrow(cast),]
        }

        # Format the dataframe
        cast$Vote = cast$Result.1
        cast$first_name = cast$Result
        if (season %in% c("Big_Brother_10_(US)")) { cast$first_name[which(cast$first_name == "Ollie")] = "Bryan" }
        if (i %in% c(22, 24, 25, 26, 28, 29)) { cast$first_name = cast$Evicted }
        cast$full_name = cast_full_names[match(cast$first_name, cast_first_names)]
        cast$player_name = cast$full_name

        # Deal with typos and exceptions
        if (season %in% c("Big_Brother_1_(US)"))  { cast = cast[which(!duplicated(cast$Result)),]; cast[which(is.na(cast$player_name)), "player_name"] = "Jean Jordan" }
        if (season %in% c("Big_Brother_20_(US)")) { cast[which(is.na(cast$player_name)), "player_name"] = "Chris Williams" }
        if (season %in% c("Big_Brother_11_(US)")) { cast[which(is.na(cast$player_name)), "player_name"] = c("Braden Bacha", "Chima Simone", "Michele Noonan") }
        if (season %in% c("Big_Brother_22_(US)")) { cast[which(is.na(cast$player_name)), "player_name"] = c("Nicole Anthony", NA, "Daniele Briones", "Nicole Franzel") }
        if (season %in% c("Big_Brother_23_(US)")) { cast[which(is.na(cast$player_name)), "player_name"] = c(NA, "Derek Xiao", NA, "Sarah Beth Steagall", "Derek Frazier") }
        if (season %in% c("Big_Brother_24_(US)")) { cast[which(is.na(cast$player_name)), "player_name"] = c("Matt Turner") }
        if (season %in% c("Big_Brother_25_(US)")) { cast[which(is.na(cast$player_name)), "player_name"] = c("Bowie Jane Ball") }
        cast$race = "not_black"
        cast$race[cast$player_name %in% black_players] = "black"
        cast$season = season_names[i]
        cast$season_n = i
        
        # Put the results together into a big dataframe (big_brother_df)
        if (season %in% c("Big_Brother_14_(US)", "Big_Brother_16_(US)", "Big_Brother_19_(US)", "Big_Brother_22_(US)", "Big_Brother_23_(US)")) { cast = cast[which(!is.na(cast$player_name)),] }
        # cast = cast[, c("season", "season_n", "player_name", "race", "Vote")]
        cast = cast[, c("season", "season_n", "player_name", "race")]
        cast$lose_rank = 1:nrow(cast)
        big_brother_df = rbind(big_brother_df, cast)
    }
}

# Create IDs from season + player
big_brother_df$season_num = reshape2::colsplit(reshape2::colsplit(big_brother_df$season, "Big Brother ", c('1', '2'))[,2], " ", c('1','2'))[,1]
big_brother_df$s_n = paste0(big_brother_df$season_num, "_", big_brother_df$player_name)
big_brother_df$id = paste0(big_brother_df$s_n, "_", big_brother_df$lose_rank)

# Remove players that entered/re-entered the game that accidentally got classified as evicted
big_brother_df = big_brother_df[which(!big_brother_df$id %in% c("3_Amy Crews_6", "5_Natalie Carroll_5", "6_Kaysar Ridha_6", "15_Judd Daugherty_10", "16_Nicole Franzel_10", "17_John McGuire_12", "18_Glenn Garcia_2", "18_Jozea Flores_4", "18_Victor Arroyo_8", "18_Victor Arroyo_15", "19_Cody Nickson_6", "20_Kaitlyn Herman_5", "20_Scottie Salton_11", "21_David Alexander_2", "21_David Alexander_4", "21_Cliff Hogg III_7", "24_Paloma Aguilar_2", "25_Jag Bains_6", "25_Cameron Hardin_11")),]

# Remove players that walked/expelled
big_brother_df = big_brother_df[which(!big_brother_df$id %in% c("2_Justin Sebik_1", "11_Chima Simone_6", "13_Dick Donato_1", "14_Willie Hantz_3", "19_Megan Lowder_2", "24_Paloma Aguilar_1", "24_Paloma Aguilar_2", "25_Luke Valentine_1")),]

# Re-rank
big_brother_df = big_brother_df %>% group_by(season) %>% mutate(id = paste0(season_num, "_", player_name), lose_rank = row_number(), win_rank = rev(row_number())) 
big_brother_df = big_brother_df %>% group_by(season) %>% mutate(lose_quantile = ecdf(lose_rank)(lose_rank)) %>% as.data.frame()

# Remove certain seasons (9 - couples and 23 - Cookout)
big_brother_df_backup = big_brother_df
big_brother_df = big_brother_df[which(!big_brother_df$season %in% c("Big Brother: Over The Top", "Big Brother 23 (US)")),]
big_brother_df[,c("season", "player_name", "race", "lose_rank")]
big_brother_df[,c("id", "lose_rank")]

# Save the data
write.csv(big_brother_df, "../../big_brother_standings.csv", row.names=F)

##############################################################################
# Survivor ===================================================================
##############################################################################

# Load Libraries
library("tidyverse")
library("rvest")

## Fetch Data ================================================================

# Survivor Season Names
setwd("/mnt/c/Users/ggruenhagen/Downloads/game_show/data/html_survivor")
season_names = read.csv("../survivor_season_names.csv", header = F)[,1]
season_names_html = gsub(" ", "_", season_names)
season_names_html = paste0("_", season_names_html)
season_names_html = season_names_html[season_names_html != "_Survivor_46"]

# Scrape all the season pages
for (season in season_names_html) {
    system(paste0("wget https://survivor.fandom.com/wiki/", season))
}

## Parse Data ================================================================

# Get all names of all the african american contestants
black_players = rvest::read_html("Category_African_American_Contestants")
black_players = black_players %>% html_nodes("a.category-page__member-link") %>% html_attr("title")
black_players = gsub("/.*", "", black_players) # remove any characters after / from the names
black_players = unique(black_players)

# Parse the html files for the cast of each season
survivor_df = data.frame()
for (i in 1:length(season_names_html)) {
    season = season_names_html[i]
    tables = read_html(season) %>% html_nodes("table")
    table_idx = 1
    if (season %in% c("_San_Juan_del_Sur", "_Worlds_Apart", "_Cambodia")) { table_idx = 2 }
    cast = html_table(tables[table_idx], fill = TRUE) %>% as.data.frame
    cast$player_name = gsub("\\d.*", "", cast$Contestant.1)
    cast$race = "not_black"
    cast$race[cast$player_name %in% black_players] = "black"
    cast$season = season_names[i]
    cast$season_n = i
    cast = cast[2:nrow(cast), c("season", "season_n", "player_name", "race", "Finish", "VotesAgainst")]
    cast = cast[which(!is.na(cast$Finish) & !grepl(":", cast$VotesAgainst)),]
    cast = cast[which(!grepl("Quit", cast$Finish)),] # remove rows where players quit
    cast$lose_rank = 1:nrow(cast)
    cast$win_rank = nrow(cast):1
    cast_lose_quantile = ecdf(cast$lose_rank)
    cast$lose_quantile = cast_lose_quantile(cast$lose_rank)
    survivor_df = rbind(survivor_df, cast)
}

# Save the results
write.csv(survivor_df, "../../survivor_standings.csv", row.names=F)
