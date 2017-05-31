#devtools::install_github("dashee87/footballR") you may need to install footballR
library(footballR)
library(dplyr)
#you'll have to wait to find out the purpose of this mysterious package
library(skellam)
library(ggplot2)
library(purrr)
library(tidyr)
# abettor is an R wrapper for the Betfair API, 
# which we'll use to obtain betting odds
#devtools::install_github("phillc73/abettor")
library(abettor)
library(RCurl)

options(stringsAsFactors = FALSE)

# get id for 2016/17 EPL season
epl_id <-fdo_listComps(season = 2016,response = "minified") %>% filter(league=="PL") %>% .$id
# get all matches in 2016/17 EPL season
epl_data <- fdo_listCompFixtures(id = epl_id, response = "minified")$fixtures %>%
  jsonlite::flatten() %>% filter(status=="FINISHED") %>%
  rename(home=homeTeamName, away=awayTeamName, homeGoals=result.goalsHomeTeam,
         awayGoals=result.goalsAwayTeam) %>%
  select(home,away,homeGoals,awayGoals) %>%
  # some formatting of team names so that the names returned by footballR are
  # compatible with those returned by the Betfair API
  mutate(home=gsub(" FC| AFC|AFC |wich Albion|rystal| Hotspur","",home)) %>% 
  mutate(home=ifelse(home=="Manchester United","Man Utd",
                     ifelse(home=="Manchester City","Man City",
                            gsub(" City| United","",home)))) %>%
  mutate(away=gsub(" FC| AFC|AFC |wich Albion|rystal| Hotspur","",away)) %>% 
  mutate(away=ifelse(away=="Manchester United","Man Utd",
                     ifelse(away=="Manchester City","Man City",
                            gsub(" City| United","",away))))
head(epl_data)

# ggplot theme used in all of the plots
my_post_theme=
  theme_minimal() +
  theme(axis.text.x = element_text(face="bold", color="#666666", 
                                   size=10, margin = margin(t = -5)),
        axis.title = element_text(color="black", face="bold",size=12),
        plot.title = element_text(color="black", face="bold", size=14),
        axis.text.y = element_text(face="bold", color="#666666", 
                                   size=10),
        legend.text=element_text(size=12),
        legend.title=element_text(size=13),
        legend.key=element_blank(),
        axis.ticks.length=unit(0, "cm"))

# remove gameweek 38 from data frame
epl_data <- head(epl_data,-10)
data.frame(avg_home_goals = mean(epl_data$homeGoals),
           avg_away_goals = mean(epl_data$awayGoals))

# goals (home team vs away team)
bind_rows(list(
  epl_data %>% group_by(homeGoals) %>% summarize(actual=n()/nrow(.)) %>% 
    mutate(pred=dpois(0:max(max(epl_data$homeGoals),max(epl_data$awayGoals)), 
                      mean(epl_data$homeGoals)), type="home") %>% rename(goals=homeGoals),
  epl_data %>% group_by(awayGoals) %>% summarize(actual=n()/nrow(.)) %>% 
    mutate(pred=dpois(0:max(max(epl_data$homeGoals),max(epl_data$awayGoals)),
                      mean(epl_data$awayGoals)), type="away") %>% rename(goals=awayGoals))) %>%
  mutate(type=factor(type, levels=c("home", "away"), labels = c("Home", "Away"))) %>%
  ggplot(aes(x=as.factor(goals))) + 
  geom_bar(aes(y=actual,fill=type),stat="identity",position="dodge") +
  geom_line(aes(group=type, y = pred,color=type),size=1.25)  +
  #  scale_fill_manual(values=c("#FFA07A", "#20B2AA"))  +
  scale_fill_manual(values=c("#FFA07A", "#20B2AA"), 
                    name = "Actual",
                    guide = guide_legend(override.aes = list(linetype = c(0,1)))) +
  scale_color_manual(values=c("#CD5C5C", "#006400"),
                     name="Poisson")  +
  ggtitle("Number of Goals per Match (EPL 2016/17 Season)")  + xlab("Goals per Match") + ylab("Proportion of Matches") +
  my_post_theme

# probability of draw between home and away team
skellam::dskellam(0,mean(epl_data$homeGoals),mean(epl_data$awayGoals))
# probability of home team winning by one goal
skellam::dskellam(1,mean(epl_data$homeGoals),mean(epl_data$awayGoals))

# skelam distribution/goal difference plot
mutate(epl_data, goal_diff=homeGoals-awayGoals) %>% group_by(goal_diff) %>%
  summarize(actual=n()/nrow(.)) %>% 
  inner_join(data.frame(goal_diff=-5:5,
                        pred=skellam::dskellam(-5:5,mean(epl_data$homeGoals),
                                               mean(epl_data$awayGoals))),
             by=c("goal_diff")) %>%
  ggplot(aes(x=as.factor(goal_diff))) + 
  geom_bar(aes(y = actual,fill="d2"), stat="identity") +
  geom_line(aes(y = pred, group = 1, color = "d1"), size=1.25) +
  scale_colour_manual(name="Skellam",labels="", values=c("d1" = "blue", "d2" = "red"))+
  scale_fill_manual(name="Actual",labels="",values="red")  +
  ggtitle("Difference in Goals Scored (Home Team vs Away Team)")  + xlab("Home Goals - Away Goals") + ylab("Proportion of Matches") +
  my_post_theme

# goals per teams (e.g. Chelsea & Sunderland)
bind_rows(list(
  left_join(
    epl_data %>% group_by(home) %>% summarize(avg_goals=mean(homeGoals)) %>% 
      split(.$home) %>% 
      map_df(~dpois(0:6,.$avg_goals)) %>% mutate(goals=0:6) %>% gather(team,pred,-goals),
    inner_join(
      epl_data %>% group_by(home,homeGoals) %>% summarize(num_games=n()),
      epl_data %>% group_by(home) %>% summarize(tot_games=n()),
      by=c("home")) %>% mutate(actual=num_games/tot_games) %>% 
      rename(goals=homeGoals, team=home) %>%
      select(-num_games,-tot_games),
    by=c("team","goals")) %>% 
    mutate(actual=ifelse(is.na(actual),0,actual), type="home"),
  left_join(
    epl_data %>% group_by(away) %>% summarize(avg_goals=mean(awayGoals)) %>% 
      split(.$away) %>% 
      map_df(~dpois(0:6,.$avg_goals)) %>% mutate(goals=0:6) %>% gather(team,pred,-goals),
    inner_join(
      epl_data %>% group_by(away,awayGoals) %>% summarize(num_games=n()),
      epl_data %>% group_by(away) %>% summarize(tot_games=n()),
      by=c("away")) %>% mutate(actual=num_games/tot_games) %>% 
      rename(goals=awayGoals, team=away) %>%
      select(-num_games,-tot_games),
    by=c("team","goals")) %>% 
    mutate(actual=ifelse(is.na(actual),0,actual), type="away"))) %>%
  mutate(type=factor(type,levels=c("home","away"),labels=c("Home","Away"))) %>%
  filter(team %in% c("Chelsea","Sunderland")) %>%
  ggplot(aes(x=as.factor(goals))) + 
  geom_bar(aes(y=actual,fill=team),stat="identity",position="dodge") +
  geom_line(aes(group=team, y = pred,color=team),size=1.25)  +
  facet_grid(type ~ .) +
  scale_fill_manual(values=c("#034694", "#EB172B"), 
                    name = "Actual",
                    labels= c("Chelsea","Sunderland"),
                    guide = guide_legend(override.aes = list(linetype = c(0,1)))) +
  scale_color_manual(values=c("#0a7bff", "#ff7c89"),
                     name="Poisson",
                     labels= c("Chelsea","Sunderland"))  +
  ggtitle("Number of Goals per Match (EPL 2016/17 Season)")  + xlab("Goals") + ylab("Proportion of Matches") +
  my_post_theme +
  theme(strip.text.y = element_text(size=12, face="bold"),
        strip.background = element_rect(colour="black", fill="#CCCCFF")) 

# building the Poisson model
poisson_model <- 
  rbind(
    data.frame(goals=epl_data$homeGoals,
               team=epl_data$home,
               opponent=epl_data$away,
               home=1),
    data.frame(goals=epl_data$awayGoals,
               team=epl_data$away,
               opponent=epl_data$home,
               home=0)) %>%
  glm(goals ~ home + team +opponent, family=poisson(link=log),data=.)
summary(poisson_model)

# calculate expected number of goals for each team in match
predict(poisson_model, 
        data.frame(home=1, team="Chelsea", 
                   opponent="Sunderland"), type="response")
predict(poisson_model, 
        data.frame(home=0, team="Sunderland", 
                   opponent="Chelsea"), type="response")

# wrap match predictions in a function
simulate_match <- function(foot_model, homeTeam, awayTeam, max_goals=10){
  home_goals_avg <- predict(foot_model,
                            data.frame(home=1, team=homeTeam, 
                                       opponent=awayTeam), type="response")
  away_goals_avg <- predict(foot_model, 
                            data.frame(home=0, team=awayTeam, 
                                       opponent=homeTeam), type="response")
  dpois(0:max_goals, home_goals_avg) %o% dpois(0:max_goals, away_goals_avg) 
}

simulate_match(poisson_model, "Chelsea", "Sunderland", max_goals=4)

# looking at the odds for Chelsea v Sunderland
chel_sun <- simulate_match(poisson_model, "Chelsea", "Sunderland", max_goals=10)
# chelsea win
sum(chel_sun[lower.tri(chel_sun)])
# draw
sum(diag(chel_sun))
# sunderland win
sum(chel_sun[upper.tri(chel_sun)])

# the first half goals is missing for a few matches from the API
# so we'll load in a csv instead
epl_1617 <- read.csv(text=getURL("http://www.football-data.co.uk/mmz4281/1617/E0.csv"), 
                     stringsAsFactors = FALSE) %>% 
  mutate(FHgoals= HTAG+HTHG, SHgoals= FTHG+FTAG-HTAG-HTHG) 

# plotting number of goals per half
left_join(
  epl_1617 %>% group_by(SHgoals) %>% summarize(SH_goals=n())  %>%
    mutate(SH_goals=SH_goals/sum(SH_goals)) %>%
    mutate(SH_pred=dpois(SHgoals,sum(SHgoals*SH_goals))) %>% 
    rename(goals=SHgoals),
  epl_1617 %>% group_by(FHgoals) %>% summarize(FH_goals=n()) %>% 
    mutate(FH_goals=FH_goals/sum(FH_goals)) %>% 
    mutate(FH_pred=dpois(FHgoals,sum(FHgoals*FH_goals))) %>% 
    rename(goals=FHgoals),
  by=c("goals"="goals")) %>%
  mutate_each(funs(replace(., which(is.na(.)), 0))) %>%
  gather(type, prob, SH_goals,FH_goals) %>%
  mutate(pred=ifelse(type=="SH_goals",SH_pred,FH_pred)) %>%
  select(-SH_pred,-FH_pred) %>%
  ggplot(aes(x=as.factor(goals))) + 
  geom_bar(aes(y=prob,fill=type),stat="identity",position="dodge") +
  geom_line(aes(group=type, y = pred,color=type),size=1.25)  +
  #  scale_fill_manual(values=c("#FFA07A", "#20B2AA"))  +
  scale_fill_manual(values=c("#FFA07A", "#20B2AA"), 
                    name = "Actual",
                    labels= c("1st Half","2nd Half"),
                    guide = guide_legend(override.aes = list(linetype = c(0,1)))) +
  scale_color_manual(values=c("#CD5C5C", "#006400"),
                     name="Poisson",
                     labels= c("1st Half","2nd Half"))  +
  ggtitle("Number of Goals per Half (EPL 2016/17 Season)")  + xlab("Goals") + ylab("Proportion of Matches") +
  my_post_theme