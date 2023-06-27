library(cricketdata)
library(dplyr)
library(purrr)
adjusted_efficiency <- function(cricStats){
  cricStats$wides[is.na(cricStats$wides)] <- 0
  cricStats$noballs[is.na(cricStats$noballs)] <- 0
  bat_df <- cricStats %>% 
    mutate(w_n = wides)
  #avg_runs = mean(bat_df$runs_off_bat)
  avg_runs = sum(bat_df$runs_off_bat) / 
    (nrow(bat_df) - nrow(bat_df[bat_df$w_n>0, ]))
  
  #getting run modifier by over (over avg runs per ball / total avg runs per ball)
  avg_runs_over <- c()
  for (o in 1:20){
    over_df <- bat_df %>% filter(over == o)
    avg_runs_over[o] <- ( sum(over_df$runs_off_bat) / 
                            (nrow(over_df) - nrow(over_df[over_df$w_n>0, ])) ) / avg_runs
  }
  avg_runs_over <- data.frame(over = c(1:20), over_avg = avg_runs_over)
  
  #getting average runs by venue
  avg_runs_venue <- bat_df %>% group_by(venue) %>% 
    summarise(venue_avg = sum(runs_off_bat) / sum(w_n == 0))
  
  #making an expected runs column
  bat_df <- bat_df %>% left_join(avg_runs_venue, by = "venue")%>% 
    left_join(avg_runs_over, by = "over") %>% 
    mutate(exp_runs = case_when(w_n == 0 ~ over_avg * venue_avg,w_n > 0 ~ 0), 
           run_diff = runs_off_bat - exp_runs)
  
  best_batters <- bat_df %>% group_by(striker) %>% 
    summarise(avg_bat_rd = sum(run_diff) / sum(w_n == 0), total_bat_rd = sum(run_diff),
              balls =sum(w_n == 0),
              strike_rate = sum(runs_off_bat) * 100 / sum(w_n ==0)) %>%
    arrange(desc(total_bat_rd))
  
  
  #Doing the same for bowlers
  bowl_df <- cricStats %>% mutate(bowling_runs = runs_off_bat + wides + noballs,
                                 w_n = wides + noballs)
  avg_runs = sum(bowl_df$bowling_runs) / 
    (nrow(bowl_df) - nrow(bowl_df[bowl_df$w_n>0, ]))
  avg_runs
  #getting run modifier by over (over avg runs per ball / total avg runs per ball)
  avg_runs_over <- c()
  for (o in 1:20){
    over_df <- bowl_df %>% filter(over == o)
    avg_runs_over[o] <- ( sum(over_df$bowling_runs) / 
                            (nrow(over_df) - nrow(over_df[over_df$w_n>0, ])) ) / avg_runs
  }
  avg_runs_over <- data.frame(over = c(1:20), over_avg = avg_runs_over)
  
  #getting average runs by venue
  avg_runs_venue <- bowl_df %>% 
    group_by(venue) %>% 
    summarise(venue_avg = sum(bowling_runs) / sum(w_n == 0))
  
  #making an expected runs column
  bowl_df <- bowl_df %>% left_join(avg_runs_venue, by = "venue")%>% 
    left_join(avg_runs_over, by = "over") %>% 
    mutate(exp_runs = case_when(w_n == 0 ~ over_avg * venue_avg,w_n > 0 ~ 0), 
           run_diff = bowling_runs - exp_runs)
  
  best_bowlers <- bowl_df %>% group_by(bowler) %>% 
    summarise(avg_bowl_rd = sum(run_diff) / sum(w_n == 0),
              total_bowl_rd = sum(run_diff), balls =sum(w_n == 0),
              econ = 6 * sum(bowling_runs) / sum(w_n == 0)) %>%
    arrange(total_bowl_rd)
  
  
  
  #doing it all again but adjusting for opposition strength
  bat_df_adj <- bat_df %>% left_join(best_bowlers, by = "bowler") %>%
    mutate(adj_exp = case_when(w_n == 0 ~ exp_runs + avg_bowl_rd, w_n > 0 ~ 0), # exp_runs * (1 + avg_bowl_rd), 
           adj_run_diff = runs_off_bat - adj_exp)
  
  best_batters_adj <- bat_df_adj %>% group_by(striker) %>% 
    summarise(adj_avg_bat_rd = sum(adj_run_diff) / sum(w_n == 0), 
              adj_bat_rd = sum(adj_run_diff),
              avg_bat_rd = sum(run_diff) / sum(w_n == 0), 
              total_bat_rd = sum(run_diff),
              balls =sum(w_n == 0), 
              strike_rate = sum(runs_off_bat) * 100 / sum(w_n == 0)) %>%
    arrange(desc(adj_bat_rd))
  
  #doing same but for bowlers
  bowl_df_adj <- bowl_df %>% left_join(best_batters, by = "striker") %>%
    mutate(adj_exp =  case_when(w_n == 0 ~ exp_runs + avg_bat_rd, w_n > 0 ~ 0), #exp_runs * (1 + avg_bat_rd), 
           adj_run_diff = bowling_runs - adj_exp)
  
  best_bowlers_adj <- bowl_df_adj %>% group_by(bowler) %>% 
    summarise(adj_avg_bowl_rd = sum(adj_run_diff) / sum(w_n == 0),
              adj_bowl_rd = sum(adj_run_diff), balls = sum(w_n == 0),
              avg_bowl_rd = sum(run_diff) / sum(w_n == 0),
              total_bowl_rd = sum(run_diff), balls =n(),
              econ = 6 * sum(bowling_runs) / sum(w_n == 0)) %>%
    arrange(adj_bowl_rd)
  
  return(list(best_batters_adj, best_bowlers_adj))
}

adjusted_average <- function(cric2018){
  cric2018$wides[is.na(cric2018$wides)] <- 0
  cric2018$noballs[is.na(cric2018$noballs)] <- 0
  cric2018$wicket <-ifelse(cric2018$wicket == "TRUE", 1, 0)
  
  #bat.df has no wides
  bat_df <- cric2018 #%>% filter(wides == 0, noballs == 0)
  avg_runs = sum(bat_df$runs_off_bat) / sum(bat_df$wicket)
  
  #getting run modifier by over (over avg runs per ball / total avg runs per ball)
  avg_runs_over <- c()
  for (o in 1:20){
    over_df <- bat_df %>% filter(over == o)
    avg_runs_over[o] <- (sum(over_df$runs_off_bat) / sum(over_df$wicket)) / avg_runs
  }
  avg_runs_over <- data.frame(over = c(1:20), over_avg = avg_runs_over)
  
  #getting average runs by venue
  avg_runs_venue <- bat_df %>% group_by(venue) %>% summarise(venue_avg = sum(runs_off_bat) / sum(wicket))
  
  #getting bat dismissals calculated accurately
  dissmisal_df <- bat_df %>% filter(wicket == 1) %>% 
    group_by(player_dismissed) %>% summarise(dismissals = n())
  
  
  #making an expected runs column
  bat_df <- bat_df %>% left_join(avg_runs_venue, by = "venue")%>% 
    left_join(avg_runs_over, by = "over") %>% 
    mutate(exp_avg = over_avg * venue_avg)
  
  best_batters <- bat_df %>% group_by(striker) %>% 
    summarise(mexp_avg = mean(exp_avg), #adj_bat_avg = ((sum(runs_off_bat) / max(sum(wicket), 1)) - mean(exp_avg)) / mean(exp_avg),
              balls =n(), runs = sum(runs_off_bat), strike_rate = mean(runs_off_bat) * 100) %>%
    left_join(dissmisal_df, by = c( "striker" = "player_dismissed")) %>%
    mutate(bat_average = runs / ifelse(is.na(dismissals), 1.0, dismissals), 
           adj_bat_avg = ((runs / ifelse(is.na(dismissals), 1.0, dismissals)) - mexp_avg) / mexp_avg) %>%
    arrange(desc(adj_bat_avg))
  
  #Doing the same for bowlers
  bowl_df <- cric2018 %>% mutate(bowling_runs = runs_off_bat + wides + noballs,
                                 w_n = wides + noballs)
  bowl_df$wicket <- ifelse(bowl_df$wicket_type == "run out", 0, bowl_df$wicket)
  
  
  avg_runs = sum(bowl_df$bowling_runs) / sum(bowl_df$wicket)
  
  #getting bowling dismissals accurately
  bowl_df <- bowl_df %>% 
    mutate(dismissal = if_else(wicket == TRUE & wicket_type != "run out", 1, 0))
  #getting run modifier by over (over avg runs per ball / total avg runs per ball)
  avg_runs_over <- c()
  for (o in 1:20){
    over_df <- bowl_df %>% filter(over == o)
    avg_runs_over[o] <- (sum(over_df$bowling_runs) / sum(over_df$dismissal)) / avg_runs
  }
  avg_runs_over <- data.frame(over = c(1:20), over_avg = avg_runs_over)
  
  #getting average runs by venue
  avg_runs_venue <- bowl_df %>% group_by(venue) %>% summarise(venue_avg = sum(bowling_runs) / sum(dismissal))
  
  
  #making an expected runs column
  bowl_df <- bowl_df %>% left_join(avg_runs_venue, by = "venue")%>% 
    left_join(avg_runs_over, by = "over") %>% 
    mutate(exp_avg = over_avg * venue_avg)
  
  best_bowlers <- bowl_df %>% group_by(bowler) %>% 
    summarise(adj_bowl_avg = ((sum(bowling_runs) / max(sum(dismissal), 1)) - mean(exp_avg)) / mean(exp_avg),
              wickets = sum(wicket),strike_rate = mean(runs_off_bat) * 100,
              bowl_average = (sum(bowling_runs) /max(sum(dismissal), 1)),
              econ = 6 * sum(bowling_runs) / sum(w_n == 0)) %>%
    arrange(adj_bowl_avg)
  
  
  #doing it all again but adjusting for opposition - batters first
  bat_df_adj <- bat_df %>% left_join(best_bowlers, by = "bowler") %>%
    mutate(adj_exp = exp_avg * (1 + adj_bowl_avg))
  
  #best_batters_adj <- bat_df_adj %>% group_by(striker) %>% 
  #  summarise(bowl_adj_bat_avg = ((sum(runs_off_bat) / max(sum(wicket), 1))- mean(adj_exp)) / mean(adj_exp),
  #            adj_bat_avg = ((sum(runs_off_bat) / max(sum(wicket), 1)) - mean(exp_avg)) / mean(exp_avg),
  #            balls =n(), runs = sum(runs_off_bat), strike_rate = mean(runs_off_bat) * 100,
  #            bat_average = (sum(runs_off_bat) /max(sum(wicket), 1)), wickets = sum(wicket)) %>%
  #  arrange(desc(bowl_adj_bat_avg))
  
  best_batters_adj <- bat_df_adj %>% group_by(striker) %>% 
    summarise(adj_exp = mean(adj_exp), exp_avg = mean(exp_avg), 
              runs = sum(runs_off_bat)) %>%
    left_join(dissmisal_df, by = c( "striker" = "player_dismissed")) %>%
    mutate(bat_average = runs / ifelse(is.na(dismissals), 1.0, dismissals), 
           adj_bat_avg = ((runs / ifelse(is.na(dismissals), 1.0, dismissals)) - exp_avg) / exp_avg,
           bowl_adj_bat_avg = ((runs / ifelse(is.na(dismissals), 1.0, dismissals))- adj_exp) / adj_exp) %>%
    arrange(desc(adj_bat_avg))
  
  #doing same but for bowlers
  bowl_df_adj <- bowl_df %>% left_join(best_batters, by = "striker") %>%
    mutate(adj_exp = exp_avg * (1 + adj_bat_avg))
  
  best_bowlers_adj <- bowl_df_adj %>% group_by(bowler) %>% 
    summarise(bat_adj_bowl_avg = ((sum(bowling_runs) / max(sum(dismissal), 1))- mean(adj_exp)) / mean(adj_exp),
              adj_bowl_avg = ((sum(bowling_runs) / max(sum(wicket), 1))- mean(exp_avg)) / mean(exp_avg),
              balls =n(), wickets = sum(dismissal),strike_rate = mean(runs_off_bat) * 100,
              bowl_average = (sum(bowling_runs) /max(sum(dismissal), 1)),
              econ = 6 * sum(bowling_runs) / sum(w_n == 0)) %>%
    arrange(bat_adj_bowl_avg)
  
  return(list(best_batters_adj, best_bowlers_adj))
}