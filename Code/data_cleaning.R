## Data Cleaning ##

library(funr)
library(usefun)
library(dplyr)
library(tidyr)
library(fastDummies)
library(writexl)
library(tidyverse)
library(runner)

script_dir <- funr::get_script_path() # path to script's directory
repo_dir <- usefun::get_parent_dir(script_dir)
data_dir <- file.path(repo_dir, "Data", "Raw")
out_dir <- file.path(repo_dir, "Data", "Cleaned")

# read in both data sets
fight_data <- read.csv(file.path(data_dir, "ufc_data_till_UFC_292.csv"))
ppv_data <- read.csv(file.path(data_dir, "ufc_ppv_buys.csv"))

############### Fighter Records ###############
# We're doing this first because we want to include any draws (which we filter out when we clean fight_data)
# date to datetype
fight_data$date <- mdy(fight_data$date)
# create vector of losers
Loser <- c()

for (i in 1:nrow(fight_data)) {
  if (fight_data[i, ncol(fight_data)] == fight_data[i, 1]) Loser[i] <- fight_data[i, 2] # if winner = red, then blue
  if (fight_data[i, ncol(fight_data)] == fight_data[i, 2]) Loser[i] <- fight_data[i, 1] # if winner = blue, then red
}

# join losers to fight_data
df_loser <- as.data.frame(Loser)
fight_data <- merge(fight_data, df_loser, by = "row.names", all = TRUE)
fight_data <- subset(fight_data, select = -c(Row.names))

R_df <- fight_data %>% select(date, fighter = R_fighter, Winner, Loser)
B_df <-  fight_data %>% select(date, fighter = B_fighter, Winner, Loser)

df_records <- rbind(R_df, B_df)
df_records <- df_records %>%
  mutate(is_winner = ifelse(fighter == Winner, 1, 0), 
         # if it's a draw, the Loser column will have an na value
         is_loser = ifelse(!is.na(Loser) & fighter == Loser, 1, 0), 
         is_draw = ifelse(is.na(Loser), 1, 0)) %>%
  group_by(fighter) %>%
  arrange(date) %>%
  # We use a lag to figure out what their most recent result was (before the match on a given date)
  mutate(matches = row_number() - 1,
         won_last_game = lag(is_winner, default = 0), 
         lost_last_game = lag(is_loser, default = 0), 
         draw_last_game = lag(is_draw, default = 0)) %>%
  # Get total wins, losses, and draws (W-L-D)
  mutate(total_wins = cumsum(won_last_game), 
         total_losses = cumsum(lost_last_game), 
         total_draws = cumsum(draw_last_game)) %>%
  # Calculate win percentage (if this was their debut, return 0)
  mutate(win_percentage = ifelse(total_wins+total_losses == 0, 0, (total_wins / (total_wins + total_losses)))) %>%
  # Calculate their win streak and losing streak
  mutate(win_streak_run = streak_run(won_last_game), 
         win_streak = ifelse(won_last_game == 0 , 0, win_streak_run), 
         lose_streak_run = streak_run(lost_last_game), 
         lose_streak = ifelse(lost_last_game == 0, 0, lose_streak_run)) %>%
  select(date, fighter, total_wins, total_losses, total_draws, win_percentage, win_streak, lose_streak)


############### fight_data - stats by individual fight ###############

# split r_sig_str, b_sig_str, etc to landed and attempted. currently showing as "17 of 35". Want to split to show 17 landed, 35 attempted. Split into 3 columns by space and remove "of" column
fight_data <- fight_data %>% separate(R_SIG_STR., c("R_SIG_STR_LND", "delete", "R_SIG_STR_ATT"), convert = TRUE)
fight_data <- fight_data %>% separate(B_SIG_STR., c("B_SIG_STR_LND", "delete", "B_SIG_STR_ATT"), convert = TRUE)
fight_data <- fight_data %>% separate(R_TOTAL_STR., c("R_TOT_STR_LND", "delete", "R_TOT_STR_ATT"), convert = TRUE)
fight_data <- fight_data %>% separate(B_TOTAL_STR., c("B_TOT_STR_LND", "delete", "B_TOT_STR_ATT"), convert = TRUE)
fight_data <- fight_data %>% separate(R_TD, c("R_TD_LND", "delete", "R_TD_ATT"), convert = TRUE)
fight_data <- fight_data %>% separate(B_TD, c("B_TD_LND", "delete", "B_TD_ATT"), convert = TRUE)
fight_data <- fight_data %>% separate(R_HEAD, c("R_HEAD_LND", "delete", "R_HEAD_ATT"), convert = TRUE)
fight_data <- fight_data %>% separate(B_HEAD, c("B_HEAD_LND", "delete", "B_HEAD_ATT"), convert = TRUE)
fight_data <- fight_data %>% separate(R_BODY, c("R_BODY_LND", "delete", "R_BODY_ATT"), convert = TRUE)
fight_data <- fight_data %>% separate(B_BODY, c("B_BODY_LND", "delete", "B_BODY_ATT"), convert = TRUE)
fight_data <- fight_data %>% separate(R_LEG, c("R_LEG_LND", "delete", "R_LEG_ATT"), convert = TRUE)
fight_data <- fight_data %>% separate(B_LEG, c("B_LEG_LND", "delete", "B_LEG_ATT"), convert = TRUE)
fight_data <- fight_data %>% separate(R_DISTANCE, c("R_DIST_LND", "delete", "R_DIST_ATT"), convert = TRUE)
fight_data <- fight_data %>% separate(B_DISTANCE, c("B_DIST_LND", "delete", "B_DIST_ATT"), convert = TRUE)
fight_data <- fight_data %>% separate(R_CLINCH, c("R_CLINCH_LND", "delete", "R_CLINCH_ATT"), convert = TRUE)
fight_data <- fight_data %>% separate(B_CLINCH, c("B_CLINCH_LND", "delete", "B_CLINCH_ATT"), convert = TRUE)
fight_data <- fight_data %>% separate(R_GROUND, c("R_GRND_LND", "delete", "R_GRND_ATT"), convert = TRUE)
fight_data <- fight_data %>% separate(B_GROUND, c("B_GRND_LND", "delete", "B_GRND_ATT"), convert = TRUE)

fight_data <- subset(fight_data, select = -c(delete))

# deal with bad data
fight_data$R_SIG_STR_pct <- replace(fight_data$R_SIG_STR_pct, fight_data$R_SIG_STR_pct == "---", 0) # can see that for pct, they are 0/0
fight_data$B_SIG_STR_pct <- replace(fight_data$B_SIG_STR_pct, fight_data$B_SIG_STR_pct == "---", 0)
fight_data$R_TD_pct <- replace(fight_data$R_TD_pct, fight_data$R_TD_pct == "---", 0)
fight_data$B_TD_pct <- replace(fight_data$B_TD_pct, fight_data$B_TD_pct == "---", 0)
fight_data$R_CTRL <- replace(fight_data$R_CTRL, fight_data$R_CTRL == "--", 0) # can't figure out what -- mean from other stats
fight_data$B_CTRL <- replace(fight_data$B_CTRL, fight_data$B_CTRL == "--", 0)

# convert pct to numeric
pct_cols <- c("R_SIG_STR_pct", "B_SIG_STR_pct", "R_TD_pct", "B_TD_pct")
fight_data[pct_cols] <- fight_data[pct_cols] %>% mutate(across(everything(), parse_number))
fight_data[pct_cols] <- fight_data[pct_cols] / 100

# control time and last round time to total seconds
fight_data$R_CTRL <- period_to_seconds(ms(fight_data$R_CTRL))
fight_data$B_CTRL <- period_to_seconds(ms(fight_data$B_CTRL))
fight_data$last_round_time <- period_to_seconds(ms(fight_data$last_round_time))

# dummy variables for categorical values - not all may be needed
fight_data <- fastDummies::dummy_cols(fight_data, select_columns = c("win_by", "Format", "Fight_type"))

#Simplifying fight types
simplify_fight_types <- function(fight_types) {
  simplified_types <- gsub("(The Ultimate Fighter|Ultimate Fighter|TUF|Ultimate Japan|UFC|Tournament|Title|Bout|Fight|Championship|Interim)", "", fight_types)
  simplified_types <- gsub("(Australia|China|Brazil|Canada|UK|Nations|Latin America)", "", simplified_types)
  simplified_types <- gsub("\\d+|vs\\.", "", simplified_types)
  simplified_types <- gsub("\\s+", " ", simplified_types)
  simplified_types <- trimws(simplified_types)
  
  simplified_types <- ifelse(grepl("Women's", simplified_types), 
                             paste("Women's", gsub("Women's", "", simplified_types)), 
                             simplified_types)
  
  simplified_types <- ifelse(grepl("Superfight|Open Weight|Openweight|Super", simplified_types),
                             "Special",
                             simplified_types)
  
  simplified_types <- gsub("\\s+", " ", simplified_types)
  simplified_types <- trimws(simplified_types)
  
  simplified_types <- ifelse(simplified_types == "", "No type", simplified_types)
  
  return(simplified_types)
}
fight_data$Fight_type <- simplify_fight_types(fight_data$Fight_type)

#Deleting features after creation of dummy variables and other with no value such as Referee and Location
fight_data$win_by <- NULL
fight_data$Fight_type <- NULL
fight_data$location <- NULL
fight_data$Referee<- NULL

############### Fighter Aggregated Stats ###############

# create a variable match_time that is the time of a match in minutes
# this was determined by the format of the game and what the last round/last round time was
fight_data <- fight_data %>% 
  mutate(match_time = case_when(Format == "No Time Limit" ~ last_round_time / 60,
                                # For all of the matches that didn't have overtime
                                last_round <= as.integer(substr(Format, 1, 1)) ~ (last_round-1)*5 + last_round_time / 60,
                                # The last two cases are when we went to overtime (last_round > # of rounds)
                                substr(Format, 1, 10) == "1 Rnd + OT" ~ as.integer(substr(Format, 13, 14)) + last_round_time / 60,
                                substr(Format, 1, 11) == "1 Rnd + 2OT" ~ as.integer(substr(Format, 14, 15)) + (last_round-2)*3 + last_round_time / 60,
                                # should never get here
                                .default = 0)) 

# group by red and blue fighter, normalize col names, join and average
df_groupby_r <- fight_data %>% select(date, match_time, starts_with("R_"))
for (col in 1:ncol(df_groupby_r)) {
  colnames(df_groupby_r)[col] <- sub("R_", "", colnames(df_groupby_r)[col])
}

df_groupby_b <- fight_data %>% select(date, match_time, starts_with("B_"))
for (col in 1:ncol(df_groupby_b)) {
  colnames(df_groupby_b)[col] <- sub("B_", "", colnames(df_groupby_b)[col])
}

fighter_agg <- bind_rows(df_groupby_r, df_groupby_b)

# (Added April 14th): Some older matches don't have values for CTRL. If CTRL is NA, replace that with 0 (otherwise, the cumulative sum w)
fighter_agg$CTRL <- coalesce(fighter_agg$CTRL, 0)

# Calculate aggregated statistics
fighter_agg <- fighter_agg %>%
  mutate(row = 1) %>%
  group_by(fighter) %>%
  arrange(date) %>%
  mutate(total_time = cumsum(match_time), 
         # For non-percent columns, cumsum(stat) / cumsum(match_time)
         across(c(KD:SIG_STR_ATT, TOT_STR_LND:TD_ATT, SUB_ATT:GRND_ATT), ~cumsum(.x) / total_time), 
         # For percentage columns, cumsum(stat) / # of rows
         SIG_STR_pct = cumsum(SIG_STR_pct) / cumsum(row),
         TD_pct = cumsum(TD_pct) / cumsum(row)) %>%
  # add a lag so the aggregated stats only include matches strictly before the match
  mutate(across(KD:GRND_ATT, ~ lag(.x))) %>%
  select(-c(match_time, total_time, row))

# remove the created match_time variable
fight_data <- fight_data %>% select(-c(match_time))

############### PPV Data ###############

# This is essentially a way for us to standardize the names with what is present in the UFC dataset
# For this, I considered an opponent's last name to be the last word in their name string
change_names_ppv <- function(name){
  case_when(name == "GSP" | name == "St. Pierre" ~ "St-Pierre", 
            name == "Kimo" ~ "Leopoldo", 
            name == "Liddel" ~ "Liddell", 
            name == "T.Silva" | name == "W. Silva" ~ "Silva", 
            name == "Bisbing" ~ "Bisping",
            name == "Cro Cop" ~ "Filipovic", 
            name == "Shogun" ~ "Rua", 
            name == "Rampage" ~ "Jackson",
            name == "Dos Santos" | name == "Bigfoot" | name == "JDS" ~ "Santos",
            name == "B.Henderson" ~ "Henderson",
            name == "Bigfoot" ~ "Silva", 
            name == "Dos Anjos" ~ "Anjos",  
            name == "St. Preux" ~ "Preux",  
            name == "de Randamie" ~ "Randamie",  
            name == "Cyborg" ~ "Justino", 
            name == "Nogueria" | name == "Nog" ~ "Nogueira",
            .default = name)
}

# To join the PPV data with other data sets, we'll just need to join by date, fighter_1, and fighter_2
# fighter_1 and fighter_2 are sorted alphabetically between each other
ppv_data <- ppv_data %>%
  # Convert Year, Month, Day columns to a single date column
  mutate(date = as.Date(with(ppv_data,paste(Year,Month,Day,sep="-")),"%Y-%m-%d")) %>%
  select(-c(Year, Month, Day)) %>%
  # Fix incorrect dates based on UFC number (cross-referenced these with ufc dataset and confirmed these dates are correct on google)
  mutate(date = case_when(
    UFC_Number == 71 ~ as.Date("2007-05-26"), 
    UFC_Number == 110 ~ as.Date("2010-02-20"),
    UFC_Number == 127 ~ as.Date("2011-02-26"),
    UFC_Number == 141 ~ as.Date("2011-12-30"), 
    UFC_Number == 144 ~ as.Date("2012-02-25"),
    UFC_Number == 193 ~ as.Date("2015-11-14"), 
    UFC_Number == 212 ~ as.Date("2017-06-03"), 
    .default = date
  )) %>%
  # Fix typos in names 
  mutate(Opponent1 = sapply(Opponent1, change_names_ppv), 
         Opponent2 = sapply(Opponent2, change_names_ppv)) %>%
  # create fighter_1 and fighter_2 for easier joins
  mutate(fighter_1 = pmin(Opponent1, Opponent2),
         fighter_2 = pmax(Opponent1, Opponent2)) %>%
  select(-c(Opponent1, Opponent2))

############### fight_data (round 2) ###############
# drop rows where winner is blank
# this needs to be at the end because both records and aggregated stats should include matches that resulted in a draw
fight_data <- fight_data[!(fight_data$Winner %in% ""), ]
# drop the Format column (this was needed to create the aggregated statistics)
fight_data$Format <- NULL

write_csv(fight_data, file.path(out_dir, "fight_data.csv"))
write_csv(df_records, file.path(out_dir, "df_records.csv"))
write_csv(fighter_agg, file.path(out_dir, "fighter_agg.csv"))
write_csv(ppv_data, file.path(out_dir, "ppv_data.csv"))
