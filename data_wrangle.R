###########################################
# Load and then munge the data to our will
###########################################

library("tidyverse")
library("lubridate")

#start with a single week
wk1.2018 <- read_csv("data/2018/PBP - 2018 - Week 1.csv")

plays <- wk1.2018 %>%
   # Flip the offenses when a punt or kickoff occurs
   mutate(offense = ifelse(down == 4 & type == "Punt" | type == "Kickoff Return (Offense)", defenseTeam, offenseTeam),
          defense = ifelse(down == 4 & type == "Punt" | type == "Kickoff Return (Offense)", offenseTeam, defenseTeam),
          # so convert home/away score to offense/defense score
          off_points = ifelse(homeTeam == offense, homeScore, awayScore),
          def_points = ifelse(homeTeam == defense, homeScore, awayScore),
          # point differential
          pts_diff = off_points - def_points,
          # the import screws up the clock column. fix it.
          clock.new = hms(substr(paste0("00:",clock), 1, 8)),
          # seconds left in the game
          seconds = seconds(clock.new) + 15 * 60 * (4- quarter),
          # adjusted score as defined by Lock & Nettleton
          adjusted_score = pts_diff / sqrt(as.numeric(seconds) + 1),
          total_points = off_points + def_points)

games <- plays %>%
   group_by(gameId) %>%
   filter(driveIndex == max(driveIndex)) %>%
   group_by(gameId) %>%
   filter(playIndex == max(playIndex)) %>%
   mutate(home_won = ifelse(homeScore > awayScore, 1, 0)) %>%
   select(gameId, homeTeam, awayTeam, home_won)

# create an "offense won" variable
model_data <- plays %>%
   left_join(games) %>%
   mutate(off_won = ifelse(home_won == 1 & homeTeam == offense, 1, 0))
