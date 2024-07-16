# Filename: "NSL_tables.R"

# Reads in data from wikipedia of history of all National Soccer League tables
# Note that the format of the input data may change as people change wikipedia entries

# Team colours sourced from https://sportsfancovers.com/a-league-color-codes/
# or from https://imagecolorpicker.com/en.

# Wikipedia data up to end of 2003-04 season has been checked against RSSSF & ozfootball.net

# Retrieve previous work from:
#setwd(output_path) 
#load(file = "nsl_tables_raw.Rdata")     # list - "tables"
#load(file="nsl_tables.Rdata")


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Libraries & directories

# Set directory paths
path = "C:/Users/fallo/OneDrive/Documents/Pete/R-files"
input_path = paste(path, "/Input", sep="")
output_path = paste(path, "/R_output", sep="")
setwd(path)

# Specify packages (libraries) that are used
library(lubridate)
library(tidyverse)
library(scales)
library(rvest)    # Reading tables from a web page


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Parameters 

# From 1977 to 2003-04
end_yr = c(seq(1977, 1983, by = 1), rep(1984,2), rep(1985,2), rep(1986,2), seq(1987, 2004, by = 1))
start_yr = end_yr - 1
seasons = ifelse(end_yr <= 1989, substr(end_yr,1,4), 
                 paste(start_yr, "-", ifelse(end_yr == 2000, substr(end_yr,1,4), substr(end_yr,3,4)), sep = ""))

# Note: need to update this line each year to value of table number in wikipedia for latest season
wiki_table_no = c(rep(2,7), 3, 4, 3, 4, 1, rep(2,2), rep(1,5), rep(2,5), rep(1,7))
wiki_table_no[which(seasons %in% c("1979", "1993-94"))] = 1
wiki_table_no[which(seasons == "2002-03")] = 2

wiki_name = c(rep("_National_Soccer_League", length(seasons)))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Functions 
make_graph = function(team_abbrev) {
  data_for_graph = nsl_tables %>% 
    filter(abbrev == team_abbrev) %>%
    filter(!(Pld == 0 & Qualification == "Withdrew"))
  
  max_teams_in_season = max(data_for_graph$count_teams_div)
  start_yr = min(data_for_graph$season)
  end_yr = max(data_for_graph$season)
  min_yr = min(data_for_graph$yr_end)
  max_yr = max(data_for_graph$yr_end)
#  releg_yr_1 = case_when(
#    team_abbrev == "SYO" ~ 1979,
#    team_abbrev == "STG" ~ 1980,
#    team_abbrev == "BLA" ~ 1981,
#    team_abbrev == "MEA" ~ 1984,
#    team_abbrev == "BRL" ~ 1986,
#    team_abbrev == "HEI" ~ 1986,
#    team_abbrev == "WOL" ~ 1986,
#    team_abbrev == "WSA" ~ 1986,
#    team_abbrev == "BRU" ~ 1988,
#    team_abbrev == "NBR" ~ 1994,
#    TRUE ~ 2099)
#  releg_yr_2 = case_when(
#    team_abbrev == "BLA" ~ 1986,
#    team_abbrev == "HEI" ~ 1989,
#    team_abbrev == "WSA" ~ 1990,
#    TRUE ~ 2099)

  #Breaks for background rectangles, other formatting
  # Update these values whenever the no. of teams in the league changes
  rects = data.frame(xstart = c(-Inf, 1980.5, 1983.5, 1986.5, 1987.5, 1994.5, 1995.5, 1996.5, 1998.5, 1999.5, 2001.5), 
                     xend = c(1980.5, 1983.5, 1986.5, 1987.5, 1994.5, 1995.5, 1996.5, 1998.5, 1999.5, 2001.5, 2099.5),
                     ystart = c(rep(16,11)), yend = c(14, 16, 12, 13, 14, 13, 12, 14, 15, 16, 13))
  x_intercepts = data_for_graph$yr_end[(data_for_graph$yr_end %% 5) == 0]
  x_intercepts = x_intercepts[!(x_intercepts ==max_yr)]
  
  # Graph of league position
  graph_1 = ggplot(data_for_graph, aes(x = yr_end, y = Pos, group=nsl_stint)) +
    geom_line(linewidth=1.15, colour = data_for_graph$team_colours[1]) +
    geom_point(aes(colour=as.factor(champion), size = as.factor(champion))) +
    scale_colour_manual(values = c(data_for_graph$second_colour[1], data_for_graph$champ_colour[1])) +
    scale_size_manual(values = c(2,4)) +
    
    # axes
    geom_rect(data = rects, aes(xmin = xstart, xmax = xend, ymin = Inf, ymax = yend+0.1),  # 0.1 for margin
              fill = "white", alpha = 1.0, inherit.aes = FALSE) +
    scale_y_continuous(trans = "reverse", expand = c(0,0.1), breaks= pretty_breaks()) +
    scale_x_continuous(breaks= pretty_breaks()) +
    coord_cartesian(xlim = c(min_yr, max_yr), ylim = c(max_teams_in_season, 1)) +
    geom_vline(xintercept=x_intercepts,  linetype="dotted") +
    theme(panel.border = element_rect(fill=NA)) +
    
    # titles
    ggtitle(paste("Position of", data_for_graph$current_name[1], "in NSL from", start_yr, "to", end_yr)) + 
    theme(plot.title = element_text(lineheight=1.0, face="bold", hjust = 0.5)) +
    labs(x="Year", y="Position") +
    theme(axis.title = element_text(face = "bold")) +
    theme(plot.margin=unit(c(0.5,1,1.5,1.2),"cm")) +
    theme(legend.position = "none") +

    # horizontal lines for number of finals teams (approximated as 4 in years pre-1983 when no finals were held)
    {if(min_yr<1984)geom_segment(aes(x = min(yr_end), xend = min(max_yr,1983.5), y = 4.5, yend = 4.5), linetype="dotted", colour = "black", linewidth = 1)} +
    {if((min_yr<1984)&(max_yr>1984))geom_segment(aes(x = 1983.5, xend = 1983.5, y = 4.5, yend = 5.5), linetype="dotted", colour = "black", linewidth = 1)} +
    {if(max_yr>1984)geom_segment(aes(x = 1983.5, xend = 1992.5, y = 5.5, yend = 5.5), linetype="dotted", colour = "black", linewidth = 1)} +
    {if((min_yr<1993)&(max_yr>1993))geom_segment(aes(x = 1992.5, xend = 1992.5, y = 5.5, yend = 6.5), linetype="dotted", colour = "black", linewidth = 1)} +
    {if(max_yr>1993)geom_segment(aes(x = max(1992.5,min_yr), xend = max(yr_end), y = 6.5, yend = 6.5), linetype="dotted", colour = "black", linewidth = 1)}
  
  graph_1
}


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Read in external data
# read all league tables in one loop
# to read a league table manually, see code at bottom, read_html("https://en.wikipedia.org/wiki/2002-03_National_Soccer_League")
tables = list()
for (j in 1:length(seasons)) {
  table = read_html(paste("https://en.wikipedia.org/wiki/", seasons[j], wiki_name[j], sep = ""))
  tables_wiki <- table %>%
    html_nodes(".wikitable") %>%
    html_table(fill = TRUE)
  
  tables[[j]] <- tables_wiki[[wiki_table_no[j]]]  %>% # added to my list
    mutate(season_no = j, season = seasons[j])
  
  if (j%%5==0) print(paste("season = ", seasons[j])) 
}

# Review headers in each of the tables - need consistency of names for combining tables
headers_all = c()
for (j in 1:length(seasons)) {
  header_fmt1 = colnames(tables[[j]])
  headers_all = rbind(header_fmt1, headers_all)
}
for (j in 7) {
  header_fmt2 = colnames(tables[[j]])
  headers_all = rbind(header_fmt2, headers_all)
}
for (j in 22) {
  header_fmt3 = colnames(tables[[j]])
  headers_all = rbind(header_fmt3, headers_all)
}

header_fmt1 = colnames(tables[[1]]) %>%
  str_replace("\\.mw-parser.*","") %>%
  str_replace("Relegation", "Qualification")
header_fmt2 = colnames(tables[[7]])
header_fmt3 = colnames(tables[[22]])

for (j in 1:length(seasons)) {  
  colnames(tables[[j]]) = header_fmt1
}
for (j in 7) {
  colnames(tables[[j]]) = header_fmt2              # exception - 1983 season
}
for (j in 22) {
  colnames(tables[[j]]) = header_fmt3              # exception - 1994-95 season
}

# convert from list to data frame
tables_all_fmt1 = do.call(rbind, lapply(tables[c(1:6, 8:21, 23:length(seasons))], as.data.frame))
tables_all_fmt2 = do.call(rbind, lapply(tables[7], as.data.frame))
tables_all_fmt3 = do.call(rbind, lapply(tables[22], as.data.frame))

tables_all_fmt2_adj = tables_all_fmt2 %>%
  mutate(Qualification = "None") %>%
  select(Pos:Pts, Qualification, season_no:season)

tables_all_fmt3_adj = tables_all_fmt3 %>%
  mutate(D = PW + PL) %>%
  select(Pos:W, D, L:season)

tables_all = rbind(tables_all_fmt1, tables_all_fmt2_adj, tables_all_fmt3_adj) %>%
  arrange(season_no, Pos)

# read in premiers & runners-up
table_premiers_clean = read_html("https://en.wikipedia.org/wiki/List_of_Australian_soccer_champions") %>%
  html_nodes(".wikitable") %>%
  html_table(fill = TRUE)

table_other = table_premiers_clean[[2]] %>%
  select(c(Season, "Runners-up")) %>%
  rename("Runners_up" = "Runners-up") %>%
  mutate(len = nchar(Season),
         season_year_end = as.numeric(substr(Season, 1, 4)) + ifelse(len <= 4, 0, 1))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Read in input files
setwd(input_path)
nsl_teams = read_csv("nsl_teams.csv")


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Select relevant data, and then data manipulations
nsl_tables = tables_all %>% 
  mutate(yr_end = as.numeric(substr(season, 1, 4)) + ifelse(season_no <= 16, 0, 1),
         Team = str_replace(Team, "\\[.*\\]", ""),            # remove text inside square brackets
         champion = ifelse(substr(Team, nchar(Team) - 2, nchar(Team)) == "(C)", 1, 0),
         premiers = ifelse(Pos == 1 & yr_end > 1987, 1, 0),
         conference_winners = ifelse(Pos == 1 & yr_end %in% c(1984, 1985, 1986), 1, 0),
         finals = ifelse(str_detect(tolower(Qualification), pattern = "finals") |
                           str_detect(tolower(Qualification), pattern = "play-offs"), 1, 0),
         relegated = ifelse(str_detect(Qualification, pattern = "Relega"), 1, 0),
         Team = str_replace(Team, " \\(C\\)", ""),            # to get consistency in team name
         Team = str_replace(Team, " \\(R\\)", ""),            
         Pts = as.numeric(str_replace(Pts, "\\[.*\\]", "")),
         pts_per_win = case_when(
           yr_end <= 1982 ~ 2,
           yr_end >= 1984 & yr_end <= 1992 ~ 2,
           yr_end == 1995 ~ 4,
           TRUE ~ 3),
         pts_bonus = ifelse(season == "1979", as.numeric(Pts) - (pts_per_win * W + D), 0),
         pts_WPen = ifelse(season == "1994-95", as.numeric(Pts) - (pts_per_win * W + D), 0),
         pts_deducted = as.numeric(Pts) - (pts_per_win * W + D + pts_bonus + pts_WPen),
         max_avail_pts = Pld * pts_per_win,
         pts_achieved_perc = Pts / max_avail_pts,
         goal_diff = GF - GA,
         GD_prefix = substr(GD,1,1),
         GD_sign = case_when(
           GD_prefix == "+" ~ 1,
           GD_prefix == "-" ~ -1,
           GD_prefix == "0" ~ 1,
           TRUE ~ -1),
         GD_numeric = ifelse(GD_prefix == "0", 0, as.numeric(substr(GD,2,nchar(GD)))) * GD_sign,
         GD_check = GD_numeric - goal_diff) %>%
  left_join(table_other, by = c("yr_end" = "season_year_end")) %>%
  mutate(runners_up = ifelse(Team == Runners_up, 1, 0),
         runners_up = ifelse(Team == "Eastern Suburbs" & season == "1978", 1, runners_up)) %>%
  group_by(season) %>%
  mutate(count_teams = n(),
         wooden_spoon = ifelse(Pos == max(Pos), 1, 0),
         wooden_spoon = ifelse(Pld == 0, 0, wooden_spoon),
         wooden_spoon = ifelse(yr_end == 1987 & Pos == max(Pos) - 1, 1, wooden_spoon)) %>%
  ungroup() %>%
  mutate(conference = case_when(
    season_no %in% c(8, 10, 12) ~ "Nth",
    season_no %in% c(9, 11, 13) ~ "Sth",
    TRUE ~ "Aust"),
    count_teams_div = ifelse(yr_end %in% c(1984, 1985, 1986), count_teams / 2, count_teams)) %>%
  select(Pos:champion, runners_up, premiers:relegated, count_teams, conference:count_teams_div, wooden_spoon, 
         pts_per_win:GD_check, yr_end)

# Create a table of team names, including history & past team name changes
teams = as_tibble(unique(nsl_tables$Team))
colnames(teams) = c("previous_name")
teams = teams %>% 
  mutate(current_name = previous_name) %>%
  mutate(current_name = case_when(                            # to get consistency of team names
    previous_name == "Fitzroy United" ~ "Heidelberg United",
    previous_name == "Eastern Suburbs" ~ "Sydney City",
    previous_name == "Canberra Arrows" ~ "Canberra City",
    previous_name == "Melbourne JUST" ~ "Footscray JUST",
    previous_name == "Melita Eagles" ~ "Parramatta Eagles",
    previous_name == "Brisbane United" ~ "Brisbane Strikers",
    previous_name == "Brunswick Pumas" ~ "Brunswick Juventus",
    previous_name == "Melbourne SC" ~ "Brunswick Juventus",
    previous_name == "Melbourne Croatia" ~ "Melbourne Knights",
    previous_name == "Sydney Croatia" ~ "Sydney United",
    previous_name == "Sydney CSC" ~ "Sydney United",
    previous_name == "UTS Olympic" ~ "Sydney Olympic",
    previous_name == "Olympic Sharks" ~ "Sydney Olympic",
    previous_name == "Gippsland Falcons" ~ "Morwell Falcons",
    previous_name == "Eastern Pride" ~ "Morwell Falcons",
    previous_name == "Wollongong City" ~ "Wollongong Wolves",
    previous_name == "Adelaide Force" ~ "Adelaide City",
    previous_name == "Auckland Kingz" ~ "Football Kingz",
    previous_name == "Marconi Fairfield" ~ "Marconi Stallions",
    TRUE ~ current_name))

teams_all = left_join(teams, nsl_teams, by = c("current_name" = "current_name"))

nsl_tables_all = left_join(nsl_tables, teams_all, by = c("Team" = "previous_name"))

# Add additional information of previous season's finishing position
nsl_tables = nsl_tables_all %>%
  arrange(current_name, season_no) %>%
  mutate(prev_pos = ifelse(current_name == lag(current_name), lag(Pos), NA)) %>%
  mutate(next_pos = ifelse(current_name == lead(current_name), lead(Pos), NA)) %>%
  arrange(season_no, Pos) %>%
  mutate(pos_diff = ifelse(is.na(prev_pos), NA, -(Pos - prev_pos)),
         pos_abs_diff = abs(pos_diff)) %>%
  group_by(current_name) %>%
  mutate(cum_champions = cumsum(champion),
         cum_premiers = cumsum(premiers),
         cum_finals = cumsum(finals),
         streak_finals = c(ave(c(0, finals), cumsum(c(0, finals) == 0), FUN = seq_along) - 1)[-1],
         streak_missed_finals = c(ave(c(0, finals), cumsum(c(0, finals) > 0), FUN = seq_along) - 1)[-1]) %>%
  ungroup() %>%
  mutate(releg_yr_1 = case_when(
           abbrev == "SYO" ~ 1979,
           abbrev == "STG" ~ 1980,
           abbrev == "BLA" ~ 1981,
           abbrev == "MEA" ~ 1984,
           abbrev == "BRL" ~ 1986,
           abbrev == "HEI" ~ 1986,
           abbrev == "WOL" ~ 1986,
           abbrev == "WSA" ~ 1986,
           abbrev == "BRU" ~ 1988,
           abbrev == "NBR" ~ 1994,
           TRUE ~ 2099),
         releg_yr_2 = case_when(
           abbrev == "BLA" ~ 1986,
           abbrev == "HEI" ~ 1989,
           abbrev == "WSA" ~ 1990,
           TRUE ~ 2099),
         nsl_stint = case_when(
           yr_end > releg_yr_2 ~ 3,
           yr_end > releg_yr_1 ~ 2,
           TRUE ~ 1))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Analysis of NSL tables data
# Make all-time league table
nsl_all_time = group_by(nsl_tables, current_name) %>%
  summarise(count = n(),
            Total_Pld = sum(Pld),
            Total_W = sum(W),
            Total_D = sum(D),
            Total_L = sum(L),
            Total_Bon = sum(pts_bonus),
            Total_WPen = sum(pts_WPen),
            Total_Ded = sum(pts_deducted),
            Total_GF = sum(GF),
            Total_GA = sum(GA),
            Total_GD = sum(goal_diff),
            Total_Pts = sum(Pts),
            pts_per_game = round(sum(Pts) / sum(Pld), 2),
            win_perc = round(Total_W / Total_Pld * 100, 2),
            count_champions = sum(champion),
            count_runners_up = sum(runners_up),
            count_premiers = sum(premiers),
            conference_winners = sum(conference_winners),
            count_finals = sum(finals),
            count_1st = sum(Pos == 1),
            count_2nd = sum(Pos == 2),
            count_3rd = sum(Pos == 3),
            count_4th = sum(Pos == 4),
            best = min(Pos),
            count_spoon = sum(wooden_spoon),
            count_relegated = sum(relegated),
            first_season = min(season),
            last_season = max(season)) %>%
  arrange(desc(Total_Pts), desc(Total_GD), desc(Total_GF))

# champions by final position
champions = filter(nsl_tables, champion == 1)
champions_by_Pos = group_by(champions, Pos) %>%
  summarise(count = n())

# totals by season
season_totals = group_by(nsl_tables, season) %>%
  summarise(count = n(),
            Total_Pld = sum(Pld),
            Total_W = sum(W),
            Total_D = sum(D),
            Total_L = sum(L),
            Total_GF = sum(GF),
            Total_GA = sum(GA),
            Total_GD = sum(goal_diff),
            Total_Pts = sum(Pts))

season_totals_div = group_by(nsl_tables, season, conference) %>%
  summarise(count = n(),
            Total_Pld = sum(Pld),
            Total_W = sum(W),
            Total_D = sum(D),
            Total_L = sum(L),
            Total_GF = sum(GF),
            Total_GA = sum(GA),
            Total_GD = sum(goal_diff),
            Total_Pts = sum(Pts))

title_race_totals = group_by(nsl_tables, season, yr_end) %>%
  summarise(count = n(),
            Total_Pts_1 = sum(Pts[Pos == 1]),
            Total_Pts_2 = sum(Pts[Pos == 2]),
            Total_GD_1 = sum(goal_diff[Pos == 1]),
            Total_GD_2 = sum(goal_diff[Pos == 2]),
            Total_GF_1 = sum(GF[Pos == 1]),
            Total_GF_2 = sum(GF[Pos == 2])) %>%
  mutate(margin_pts = Total_Pts_1 - Total_Pts_2,
         margin_GD = Total_GD_1 - Total_GD_2,
         margin_GF = Total_GF_1 - Total_GF_2)

# totals by club
club_records = group_by(nsl_tables, current_name) %>%
  summarise(highest_GF = max(GF),
            lowest_GF = min(GF),
            highest_GA = max(GA),
            lowest_GA = min(GA),
            highest_Pts = max(Pts),
            lowest_Pts = min(Pts))

# Records for each team in a season
highest_GF_team = club_records %>%
  left_join(nsl_tables, by = c("current_name" = "current_name",
                                         "highest_GF" = "GF")) %>%
  select(current_name, highest_GF, Pld, season)

lowest_GF_team = club_records %>%
  left_join(nsl_tables, by = c("current_name" = "current_name",
                                         "lowest_GF" = "GF")) %>%
  select(current_name, lowest_GF, Pld, season)

highest_GA_team = club_records %>%
  left_join(nsl_tables, by = c("current_name" = "current_name",
                                         "highest_GA" = "GA")) %>%
  select(current_name, highest_GA, Pld, season)

lowest_GA_team = club_records %>%
  left_join(nsl_tables, by = c("current_name" = "current_name",
                                         "lowest_GA" = "GA")) %>%
  select(current_name, lowest_GA, Pld, season)

highest_Pts_team = club_records %>%
  left_join(nsl_tables, by = c("current_name" = "current_name",
                                         "highest_Pts" = "Pts")) %>%
  select(current_name, highest_Pts, Pld, season)

lowest_Pts_team = club_records %>%
  left_join(nsl_tables, by = c("current_name" = "current_name",
                                         "lowest_Pts" = "Pts")) %>%
  select(current_name, lowest_Pts, Pld, season)

# Records for a single season - not adjusted for no. of games
# most & least points
most_pts_season = arrange(nsl_tables, desc(Pts)) %>%
  select(season, Team, Pld, Pts)
head(most_pts_season, 5)

least_pts_season = arrange(nsl_tables, Pts) %>%
  select(season, Team, Pld, Pts)
head(least_pts_season, 5)

# most & least wins
most_wins_season = arrange(nsl_tables, desc(W)) %>%
  select(season, Team, Pld, W)
head(most_wins_season, 5)

least_wins_season = arrange(nsl_tables, W) %>%
  filter(Pld > 0) %>%
  select(season, Team, Pld, W)
head(least_wins_season, 5)

# most & least losses
most_losses_season = arrange(nsl_tables, desc(L)) %>%
  select(season, Team, Pld, L)
head(most_losses_season, 5)

least_losses_season = arrange(nsl_tables, L) %>%
  filter(Pld > 0) %>%
  select(season, Team, Pld, L)
head(least_losses_season, 5)

# most & least draws
most_draws_season = arrange(nsl_tables, desc(D)) %>%
  select(season, Team, Pld, D)
head(most_draws_season, 5)

least_draws_season = arrange(nsl_tables, D) %>%
  filter(Pld > 0) %>%
  select(season, Team, Pld, D)
head(least_draws_season, 5)

# most & least goals scored
most_goals_season = arrange(nsl_tables, desc(GF)) %>%
  select(season, Team, Pld, GF)
head(most_goals_season, 5)

least_goals_season = arrange(nsl_tables, GF) %>%
  filter(Pld > 0) %>%
  select(season, Team, Pld, GF)
head(least_goals_season, 5)

# most & least goals conceded
most_goals_against_season = arrange(nsl_tables, desc(GA)) %>%
  select(season, Team, Pld, GA)
head(most_goals_against_season, 5)

least_goals_against_season = arrange(nsl_tables, GA) %>%
  filter(Pld > 0) %>%
  select(season, Team, Pld, GA)
head(least_goals_against_season, 5)

# best & worst goal difference
best_goals_diff_season = arrange(nsl_tables, desc(goal_diff)) %>%
  select(season, Team, Pld, goal_diff)
head(best_goals_diff_season, 5)

worst_goals_diff_season = arrange(nsl_tables, goal_diff) %>%
  select(season, Team, Pld, goal_diff)
head(worst_goals_diff_season, 5)

# highest & lowest points achieved percentage
highest_pts_perc_season = arrange(nsl_tables, desc(pts_achieved_perc)) %>%
  select(season, Team, Pld, Pts, max_avail_pts, pts_achieved_perc)
head(highest_pts_perc_season, 5)

lowest_pts_perc_season = arrange(nsl_tables, pts_achieved_perc) %>%
  select(season, Team, Pld, Pts, max_avail_pts, pts_achieved_perc)
head(lowest_pts_perc_season, 5)

# most points to not win the league
most_pts_not_premiers_season = arrange(nsl_tables, desc(Pts)) %>%
  filter(premiers == 0) %>%
  select(season, Team, Pld, Pts) 
head(most_pts_not_premiers_season, 5)

# least points to win the league
least_pts_premiers_season = arrange(nsl_tables, Pts) %>%
  filter(premiers == 1) %>%
  select(season, Team, Pld, Pts)
head(least_pts_premiers_season, 5)

# biggest & smallest winning margin in league
most_winning_margin_season = title_race_totals %>%
  arrange(desc(margin_pts), desc(margin_GD), desc(margin_GF)) %>%
  left_join(nsl_tables, by = c("season" = "season")) %>%
  filter(Pos == 1) %>%
  select(season, Team, margin_pts, margin_GD, margin_GF)
head(most_winning_margin_season, 5)

least_winning_margin_season = title_race_totals %>%
  arrange(margin_pts, margin_GD, margin_GF) %>%
  left_join(nsl_tables, by = c("season" = "season")) %>%
  filter(Pos == 1) %>%
  select(season, Team, margin_pts, margin_GD, margin_GF)
head(least_winning_margin_season, 5)

# highest movement in final position
highest_mvmt_up_season = arrange(nsl_tables, desc(pos_diff)) %>%
  select(season, Team, Pos, prev_pos, pos_diff)
head(highest_mvmt_up_season, 5)

highest_mvmt_down_season = arrange(nsl_tables, pos_diff) %>%
  select(season, Team, Pos, prev_pos, pos_diff)
head(highest_mvmt_down_season, 5)


# lowest position to champion in one season
prev_pos_champion = nsl_tables %>%
  filter(champion == 1) %>%
  select(season, Team, prev_pos) %>%
  arrange(desc(prev_pos), season)
head(prev_pos_champion, 5)

# lowest position after being champion in one season
next_pos_champion = nsl_tables %>%
  filter(champion == 1) %>%
  select(season, Team, next_pos) %>%
  arrange(desc(next_pos), season)
head(next_pos_champion, 5)


# volatility of position from year to year
pos_changes = nsl_tables %>%
  group_by(current_name) %>%
  summarise(count_seasons = n(),
            total_pos_diff = sum(pos_abs_diff, na.rm = TRUE)) %>%
  mutate(ave_mvmt = total_pos_diff / (count_seasons - 1)) %>%
  arrange(desc(ave_mvmt))
pos_changes


# Longest streaks
longest_streaks_finals = arrange(nsl_tables, desc(streak_finals)) %>%
  select(season, Team, streak_finals)
head(longest_streaks_finals, 5)

longest_streaks_missed_finals = arrange(nsl_tables, desc(streak_missed_finals)) %>%
  select(season, Team, streak_missed_finals)
head(longest_streaks_missed_finals, 5)


# no. of teams in finals
finals_teams = nsl_tables %>% 
  filter(str_detect(tolower(Qualification), pattern = "finals") |
           str_detect(tolower(Qualification), pattern = "play-offs")) %>% 
  group_by(season, yr_end) %>% 
  summarise(finals_teams = max(Pos))

# list of all team abbreviations
teams_unique = unique(nsl_tables$abbrev)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# checks on data for consistency
error_check_pts = nsl_tables %>% 
  filter(!Pts == (pts_per_win * W + D))    

error_check_pld = nsl_tables %>%
  filter(!Pld == (W + D + L))

error_check_results = season_totals %>%
  filter(!Total_W == Total_L)

error_check_gd_season = season_totals %>%
  filter(!Total_GD == 0)

error_check_gd = nsl_tables %>%
  filter(!(GD_check == 0))

error_check_pos = group_by(nsl_tables, season, conference) %>%
  summarise(count = n(),
            sum_pos = sum(Pos)) %>%
  mutate(exp_sum_pos = count * (count + 1) / 2,
         pos_diff = sum_pos - exp_sum_pos) %>%   # error if calculated difference (pos_diff) is not zero
  filter(!(pos_diff == 0))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# run function to produce graph for a specific team
make_graph("ADC") 
make_graph("API")
make_graph("BRC")
make_graph("BRL") 
make_graph("CAN")
make_graph("FOO")
make_graph("HEI")
make_graph("MAR")
#make_graph("MOO"). Only 1 season in NSL
make_graph("NUN")
make_graph("SOU") 
make_graph("STG")
make_graph("SYC")
make_graph("SYO") 
make_graph("WSA")
#make_graph("WST"). Only 2 seasons in NSL to 1978
#make_graph("BLA"). Never more than 3 consecutive seasons in NSL
make_graph("WOL")
make_graph("PRE")

# New teams from 1984
make_graph("SUN")
#make_graph("PEN"). Only 2 seasons in NSL
#make_graph("MEA"). Graph is mostly blank space 
#make_graph("NRU"). Only 3 seasons in NSL
make_graph("MKN")
#make_graph("BRU"). Graph is mostly blank space
#make_graph("GRE"). Only 3 seasons in NSL
make_graph("SGC")
#make_graph("INT"). Only 2 seasons in NSL
#make_graph("CBY"). Only 1 season in NSL
#make_graph("WMA"). Only 1 season in NSL
make_graph("NBR")
make_graph("BRS")
make_graph("MOR")

# new after 1995
make_graph("CCO")
make_graph("PER")
#make_graph("COL"). Only 1 season in NSL
#make_graph("CAR"). Only 4 seasons in NSL
make_graph("NSP")
make_graph("FKZ")
make_graph("PAR")
#make_graph("NEW"). Only 4 seasons in NSL
#make_graph("ADE"). Only 1 season in NSL 


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# export file to csv format
names(nsl_all_time) <- gsub(x = names(nsl_all_time), pattern = "_", replacement = " ") 

setwd(output_path)
save(tables, file = "nsl_tables_raw.Rdata")
save(nsl_tables, file = "nsl_tables.Rdata")
write.csv(nsl_tables, file = "nsl_tables_full.csv")
write.csv(nsl_all_time, file = "nsl_all_time.csv")
setwd(path) 

# export single graph
#setwd(output_path)
#ggsave("graph_ggsave.pdf")
#setwd(path)

# export multiple graphs
for (i in 1:length(teams_unique)) {
  make_graph(teams_unique[i])
  setwd(output_path)
  #  ggsave(paste("graph_nsl_", teams_unique[i], ".pdf", sep=""))
  ggsave(paste("performance_chart_nsl_", teams_unique[i], ".png", sep=""))
  ggsave(paste("performance_chart_nsl_", teams_unique[i], ".svg", sep=""))
}
setwd(path)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# End


# To do:
# Have combined NSL/A-League chart for the clubs which were in both competitions


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Test
# read one league table manually
table = read_html("https://en.wikipedia.org/wiki/2002-03_National_Soccer_League")
tables_all <- table %>%
  html_nodes(".wikitable") %>%
  html_table(fill = TRUE)
table_yyyymm = tables_all[[2]]
table_yyyymm

