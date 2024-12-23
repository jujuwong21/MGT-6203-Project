#################################################### EXPLORATORY DATA ANALYSIS ####################################################

library(ggplot2)
library(fuzzyjoin)
# library(reshape2)

################### EDA for question of who wins?

# win percentage for fighters on 1 fight win streak, 2 fight win streak, 3+ win streak?
win_fight_streaks <- fight_data[, c('date','Winner')]
win_fight_streaks <- win_fight_streaks %>% inner_join(df_records, by=c('date'='date', 'Winner'='fighter'))
win_fight_streaks <- win_fight_streaks[, c('date','win_streak')]
win_fight_streaks$win_streak[win_fight_streaks$win_streak >= 3] <- '3+'
win_fight_streak_outcome <- win_fight_streaks %>% group_by(win_streak) %>% summarise(Percentage=n() / nrow(win_fight_streaks) * 100)

# win streak outcomes
win_fight_streak_outcome
ggplot(win_fight_streak_outcome, aes(x=win_streak, y=Percentage, fill=win_streak)) + geom_bar(stat='identity')+labs(title='Winner Win Streak') + geom_text(aes(label=round(Percentage,2)), nudge_y=3)

# win percentage for fighters on 1 fight, 2 fight, 3+ fight lose streak?
lose_fight_streaks <- fight_data[, c('date','Loser')]
lose_fight_streaks <- lose_fight_streaks %>% inner_join(df_records, by=c('date'='date','Loser'='fighter'))
lose_fight_streaks <- lose_fight_streaks[, c('date','lose_streak')]
lose_fight_streaks$lose_streak[lose_fight_streaks$lose_streak >= 3] <- '3+'
lose_fight_streak_outcome <- lose_fight_streaks %>% group_by(lose_streak) %>% summarise(Percentage=n() / nrow(lose_fight_streaks) * 100)

# lose streak outcomes
lose_fight_streak_outcome
ggplot(lose_fight_streak_outcome, aes(x=lose_streak, y=Percentage,fill=lose_streak))+labs(title='Winner Lose Streak') + geom_bar(stat='identity') + geom_text(aes(label=round(Percentage,2)), nudge_y=3)


# win percentage of fighters with most strikes landed
win_strikes_landed <- fight_data[, c('R_fighter','B_fighter','R_TOT_STR_LND','B_TOT_STR_LND','Winner')]
win_strikes_landed <- mutate(win_strikes_landed, strike_winner = ifelse(win_strikes_landed$R_TOT_STR_LND > win_strikes_landed$B_TOT_STR_LND,
                                                                        win_strikes_landed$R_fighter, win_strikes_landed$B_fighter))
win_strikes_landed <- mutate(win_strikes_landed, fight_str_winner = ifelse(win_strikes_landed$Winner == win_strikes_landed$strike_winner, 'Win','Loss'))
win_strikes_landed_outcome <- win_strikes_landed %>% group_by(fight_str_winner) %>% summarise(Percentage=n() / nrow(win_strikes_landed) * 100)

# strike winner outcomes
win_strikes_landed_outcome
ggplot(win_strikes_landed_outcome, aes(x=fight_str_winner, y=Percentage, fill=fight_str_winner))+labs(title='Winner Strike Advantage') + geom_bar(stat='identity') + geom_text(aes(label=round(Percentage,2)), nudge_y=3)

# win percentage of fighters with most control time
win_ctrl_time <- fight_data[, c('R_fighter','B_fighter','R_CTRL','B_CTRL','Winner')]
win_ctrl_time <- win_ctrl_time %>% drop_na()
win_ctrl_time <- mutate(win_ctrl_time, ctrl_win = ifelse(win_ctrl_time$R_CTRL > win_ctrl_time$B_CTRL, win_ctrl_time$R_fighter, win_ctrl_time$B_fighter))
win_ctrl_time <- mutate(win_ctrl_time, fight_ctrl_win = ifelse(win_ctrl_time$Winner == win_ctrl_time$ctrl_win, 'Win','Loss'))
win_ctrl_time_outcome <- win_ctrl_time %>% group_by(fight_ctrl_win) %>% summarise(Percentage=n() / nrow(win_ctrl_time) * 100)

# control winner outcomes
win_ctrl_time_outcome
ggplot(win_ctrl_time_outcome, aes(x=fight_ctrl_win, y=Percentage, fill=fight_ctrl_win))+labs(title='Winner Control Advantage') + geom_bar(stat='identity') + geom_text(aes(label=round(Percentage,2)), nudge_y=3)

# EDA summary/thoughts for "Who will win?"
# It appears that win streaks aren't as common as I would've thought. The winner has been on a win streak of 0 for 50.6% of the fights which means they are
# going 1-1 in wins-losses for their career. To check the other side, the loser of the fight has on a 0 fight lose streak 63.8% of the time which is consistent
# with the 1-1 in wins-losses. The winner landed more overall strikes 78% of the time and had more control time 69.6% of the time. This makes sense as it shows
# a dominant skill set to control the fight.
