###################### Preparation work ######################

#Load necessary packages
{
  library(foreach)
  library(tidyverse)
  library(nflseedR)
  library(data.table)
  library(mefa)
  library(parallel)
  library(doParallel)
  library(listr)
}

#Read in list of games, initial elo scores, week 18 game combinations 
games <- read.csv("C:/Users/andrew.cohen_betfana/Desktop/nfl_futures/futures_sim/games_2023_test.csv")
initial_elo <- read.csv("C:/Users/andrew.cohen_betfana/Downloads/FBG Elo 2023-05-17.csv")
wk18combos <- read.csv("C:/Users/andrew.cohen_betfana/Desktop/nfl_futures/futures_sim/wk18combos.csv")

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
#create functions to be used in season simulation
simulate_current_week=function(df,df2){
  x=df%>%
    #minimum week=current week
    filter(week==min(week))%>% 
    #need these next 3 lines to join in elo ratings
    left_join(df2,by = c("away_team" = "team"))%>%
    left_join(df2, by = c("home_team" = "team"))%>%
    rename(away_elo=elo.x,home_elo=elo.y)%>%
    mutate(
      #using -100 as placeholder elo shift for week 18 rest (further research needed) 
      away_elo=ifelse(away_locked==1,away_elo-250,away_elo),
      home_elo=ifelse(home_locked==1,home_elo-250,home_elo),
      elo_diff = home_elo - away_elo, 
      #using 50 as placeholder for home field
      elo_diff = elo_diff + ifelse(location == "Home", 50, 0), 
      #rest adjustment     
      elo_diff = elo_diff + (home_rest - away_rest)/7 * 25, 
      #playoff adjustment
      elo_diff = elo_diff * ifelse(game_type == "REG", 1, 1.2), 
      #elo diff to win probability
      wp = 1/(10^(-elo_diff/400) +1), 
      #elo diff to point spread
      estimate = elo_diff/25, 
      #game result is normally distributed around point spread
      result =   round(rnorm(n(),  estimate, 13)), 
      #account for ties
      result=case_when(result!=0~result,TRUE~sample(c(0,-1,1),size=1,prob =c(.15,.425,.425))),
      #get outcome from game result
      outcome = case_when(result > 0 ~ 1, result < 0 ~ 0, TRUE ~ 0.5), 
      #compute elo shifts
      elo_input = case_when(is.na(result) ~ NA_real_, result > 0 ~ elo_diff * 0.001 + 2.2, result< 0 ~ -elo_diff * 0.001 + 2.2, TRUE ~ 1), 
      elo_mult = log(pmax(abs(result), 1) + 1) * 2.2/elo_input, 
      elo_shift = 20 * elo_mult * (outcome - wp)) 
}
shift_elo=function(df,df2){
  #incorporate elo shifts
  home_eloshift=transmute(df,team=home_team,shift=elo_shift)
  away_eloshift=transmute(df,team=away_team,shift=-elo_shift)
  elo_shift=rbind(home_eloshift,away_eloshift) 
  df2=left_join(df2,elo_shift)%>%replace_na(list(shift=0))%>%mutate(elo=elo+shift)%>%select(-shift)
}
get_WC_matchups=function(df,df2){
  df[which(df$game_id=="AFC_WC_27"),"away_team"]=df2[which(df2$conf=="AFC"&df2$seed==7),"team"]
  df[which(df$game_id=="AFC_WC_27"),"home_team"]=df2[which(df2$conf=="AFC"&df2$seed==2),"team"]
  df[which(df$game_id=="AFC_WC_36"),"away_team"]=df2[which(df2$conf=="AFC"&df2$seed==6),"team"]
  df[which(df$game_id=="AFC_WC_36"),"home_team"]=df2[which(df2$conf=="AFC"&df2$seed==3),"team"]
  df[which(df$game_id=="AFC_WC_45"),"away_team"]= df2[which(df2$conf=="AFC"&df2$seed==5),"team"]
  df[which(df$game_id=="AFC_WC_45"),"home_team"]= df2[which(df2$conf=="AFC"&df2$seed==4),"team"]
  df[which(df$game_id=="NFC_WC_27"),"away_team"]= df2[which(df2$conf=="NFC"&df2$seed==7),"team"]
  df[which(df$game_id=="NFC_WC_27"),"home_team"]=df2[which(df2$conf=="NFC"&df2$seed==2),"team"]
  df[which(df$game_id=="NFC_WC_36"),"away_team"]= df2[which(df2$conf=="NFC"&df2$seed==6),"team"]
  df[which(df$game_id=="NFC_WC_36"),"home_team"]= df2[which(df2$conf=="NFC"&df2$seed==3),"team"]
  df[which(df$game_id=="NFC_WC_45"),"away_team"]= df2[which(df2$conf=="NFC"&df2$seed==5),"team"]
  df[which(df$game_id=="NFC_WC_45"),"home_team"]= df2[which(df2$conf=="NFC"&df2$seed==4),"team"]
  return(df)
}
get_DIV_matchups=function(df,df2){
  df[which(df$game_id=="AFC_DIV_1"),"away_team"]= df2[which(df2$conf=="AFC"&df2$seed==4),"team"]
  df[which(df$game_id=="AFC_DIV_1"),"home_team"]= df2[which(df2$conf=="AFC"&df2$seed==1),"team"]
  df[which(df$game_id=="AFC_DIV_2"),"away_team"]= df2[which(df2$conf=="AFC"&df2$seed==3),"team"]
  df[which(df$game_id=="AFC_DIV_2"),"home_team"]= df2[which(df2$conf=="AFC"&df2$seed==2),"team"]
  df[which(df$game_id=="NFC_DIV_1"),"away_team"]= df2[which(df2$conf=="NFC"&df2$seed==4),"team"]
  df[which(df$game_id=="NFC_DIV_1"),"home_team"]= df2[which(df2$conf=="NFC"&df2$seed==1),"team"]
  df[which(df$game_id=="NFC_DIV_2"),"away_team"]= df2[which(df2$conf=="NFC"&df2$seed==3),"team"]
  df[which(df$game_id=="NFC_DIV_2"),"home_team"]= df2[which(df2$conf=="NFC"&df2$seed==2),"team"]
  return(df)
  
}
get_CC_matchups=function(df,df2){
  df[which(df$game_id=="AFC_CC"),"away_team"]= df2[which(df2$conf=="AFC"&df2$seed==2),"team"]
  df[which(df$game_id=="AFC_CC"),"home_team"]= df2[which(df2$conf=="AFC"&df2$seed==1),"team"]
  df[which(df$game_id=="NFC_CC"),"away_team"]= df2[which(df2$conf=="NFC"&df2$seed==2),"team"]
  df[which(df$game_id=="NFC_CC"),"home_team"]= df2[which(df2$conf=="NFC"&df2$seed==1),"team"]
  
  return(df)
}
get_SB_matchup=function(df,df2){
  df[which(df$game_id=="SB"),"away_team"]= df2[which(df2$conf=="AFC"),"team"]
  df[which(df$game_id=="SB"),"home_team"]= df2[which(df2$conf=="NFC"),"team"]
  return(df)
  
}

#Exact Division Order
exact_order_fun=function(df){
  x1=distinct(df,division,team)%>%arrange(division)
  x2=x1%>%mutate(tm=rep(c("tm1","tm2","tm3","tm4"),8))%>%
    pivot_wider(id_cols = division,names_from = tm,values_from = team) 
  x2=as.matrix(x2)
  
  combfunc=function(df,i){
    z1=permn(df[i,2:5])%>%unlist(use.names = FALSE)%>%matrix(24,4,byrow = TRUE)%>%
      as.data.frame(.)%>%rename(team=1)%>%left_join(x1)%>%
      mutate(code=paste0(team,V2,V3,V4))%>%relocate(division,.before = team)%>%
      relocate(code,.after = division)%>%
      rename(div_winner=team,div2=V2,div3=V3,div4=V4)
    return(z1)}
  
  div_order <- foreach(a = 1:8,.packages = c("combinat","tidyverse")) %dopar% {combfunc(x2,a)}
  div_order=div_order |> list_flatten()
  div_order=as.data.frame(rbindlist(div_order,use.names = TRUE))
  
  exact_order=filter(df,!is.na(code))%>%
    right_join(div_order)%>%
    group_by(code,div_winner,div2,div3,div4)%>%
    mutate(prob=n())%>%
    ungroup()%>%
    distinct(division,code,div_winner,div2,div3,div4,prob)%>%
    mutate(prob=prob/sims)%>%
    select(-code)%>%
    arrange(division,desc(prob))}

#Any Top 2
top_two_fun=function(df){
  
  twofunc=function(df,i){
    z1=combn(df[i,2:5],2)%>%unlist(use.names = FALSE)%>%matrix(6,2,byrow = TRUE)%>%
      as.data.frame(.)%>%mutate(code3=seq(1,6,1),code2=paste0(V1,V2))
    z2=z1%>%rename(V1=V2,V2=V1)%>%relocate(V2,.after = V1)
    z3=rbind(z1,z2)%>%rename(team=V1,team2=V2)%>%left_join(x1)
    return(z3)}
  
  x1=distinct(df,division,team)%>%arrange(division)
  x2=x1%>%mutate(tm=rep(c("tm1","tm2","tm3","tm4"),8))%>%
    pivot_wider(id_cols = division,names_from = tm,values_from = team) 
  x2=as.matrix(x2)
  
  
  code2=filter(df,!is.na(code2))%>%select(code2)%>%group_by(code2)%>%mutate(prop=n())
  div_top2 <- foreach(a = 1:8,.packages = c("combinat","tidyverse")) %dopar% {twofunc(x2,a)}
  div_top2=div_top2 |> list_flatten()
  div_top2=as.data.frame(rbindlist(div_top2,use.names = TRUE))%>%
    left_join(code2)%>%
    distinct()%>%
    replace_na(list(prop=0))%>%
    group_by(division,code3)%>%
    mutate(prop=sum(prop))%>%
    distinct()%>%ungroup()%>%arrange(division,code3)%>%
    mutate(prop=prop/sims)%>%
    transmute(division=division,team1=team,team2=team2,prop=prop)%>%
    arrange(division,desc(prop))}

#win possibilities
win_possibilities_fun=function(df){
  
  team_wins=summarise(group_by(df,team,wins),ct=n())%>%ungroup()
  
  teams=distinct(df,team)%>%
    mefa:::rep.data.frame(.,35)%>%
    arrange(team)
  
  medwins=summarise(group_by(df,team),median_wins=median(wins))%>%ungroup()
  
  win_possibilities=data.frame(wins=seq(0,17,.5))%>%
    mefa:::rep.data.frame(.,32)%>%
    cbind(teams)%>%
    left_join(team_wins)%>%
    replace_na(list(ct=0))%>%
    mutate(ct=ct/sims)%>%
    pivot_wider(id_cols = team,names_from = wins,values_from = ct)%>%
    left_join(medwins)%>%
    relocate(median_wins,.after = team)
}

###################### Simulate season function ######################
simulate_season=function(games,initial_elo,wk18combos,simnum,wk18rest=0,tank=0,injury=0){
  #create df of games that have already occurred
  games_past=filter(games,!is.na(home_score))
  
  #create df of games not yet played
  games_future=filter(games,is.na(home_score))%>%select(-away_score,-home_score,-result)
  
  #prior to starting simulation, current elo = initial elo
  current_elo=initial_elo
  
  #for all weeks prior to the final week of the regular season...
  while (min(games_future$week)<18){ 
    
    #simulate current week
    current_week=simulate_current_week(games_future,current_elo)
    #incorporate elo shifts
    current_elo=shift_elo(current_week,current_elo)
    
    #move current games to past games, remove past games from future games
    common_cols <- intersect(colnames(games_past), colnames(current_week))
    games_past=rbind( games_past[, common_cols], current_week[, common_cols])
    games_future=games_future[-which(games_future$game_id%in%games_past$game_id),]
    
    #break when week 17 completed
    if  (min(games_future$week)==18){ 
      break
    }}
  
  
  #if we DON'T want to account for teams who are locked into a seed 
  #resting players in the final week of the regular season ...
  if(wk18rest==0){
    
    #simulate week 18 (there are no elo shifts for week 18)
    current_week=simulate_current_week(games_future,current_elo)
    
    #move current games to past games, remove past games from future games
    games_past=rbind(games_past, current_week[,which(colnames(current_week)%in%colnames(games_past)) ])
    games_future=games_future[-which(games_future$game_id%in%games_past$game_id),]
  }
  
  #if we DO want to account for teams who are locked into a seed 
  #resting players in the final week of the regular season ...
  if(wk18rest==1){
    
    #create df of columns from past games needed for nflseedR::compute_division_ranks and nflseedR::compute_conference_seeds
    df=games_past%>%transmute(game_type=game_type,week=week,away_team=away_team, home_team=home_team, result=result)
    
    #repeat each row 256 times as there are 256 possible week 18 outcomes for each conference (ties excluded)
    df=mefa:::rep.data.frame(df,256)%>%mutate(sim=rep(c(1:256),each=256))
    
    #combine df with the 256 potential week 18 outcomes
    df=rbind(df,wk18combos)%>%
      
      #compute seeds for each team in all potential outcomes
      nflseedR::compute_division_ranks(tiebreaker_depth = 2) %>%
      nflseedR::compute_conference_seeds(h2h = .$h2h ) 
    
    #extract standings
    df=df[["standings"]]%>%replace_na(list(seed=8))
    
    #determine which teams are locked into a seed
    locked_seeds=summarise(group_by(df,team),minseed=min(seed),maxseed=max(seed))%>%ungroup()%>%
      transmute(team=team,locked=case_when(maxseed==minseed&maxseed<8~1,TRUE~0))
    
    #Add in locked seeds to games_future
    games_future=games_future%>%left_join(locked_seeds,by = c("away_team" = "team"))%>% 
      left_join(locked_seeds, by = c("home_team" = "team"))%>%
      mutate(away_locked=locked.x,home_locked=locked.y)%>%
      select(-locked.x,-locked.y)
    
    
    #simulate week 18 *there are no elo shifts for week 18)
    current_week=simulate_current_week(games_future,current_elo)
    #change locked seeds back to 0 so this adjustment doesn't carry over to the playoffs
    games_future=games_future%>%mutate(away_locked=0,home_locked=0) 
    
    #move current games to past games, remove past games from future games
    games_past=rbind( games_past, current_week[,which(colnames(current_week)%in%colnames(games_past)) ])
    games_future=games_future[-which(games_future$game_id%in%games_past$game_id),]
    
  }  
  #calculate regular season final standings
  final_reg_standings=games_past%>%
    select(game_type,week,away_team,home_team,result)%>%
    mutate(sim=simnum)%>%
    nflseedR::compute_division_ranks(tiebreaker_depth = 2) %>%
    nflseedR::compute_conference_seeds(h2h = .$h2h ) 
  final_reg_standings=final_reg_standings[["standings"]]
  
  #compute playoff teams
  playoff_teams=final_reg_standings%>%filter(seed<8)%>%select(conf,team,seed) 
  #compute WC matchups
  games_future=get_WC_matchups(games_future,playoff_teams)
  #simulate WC round
  current_week=simulate_current_week(games_future,current_elo) #simulate WC matchups
  #pull WC winners
  WC_winners=current_week%>%mutate(winner=ifelse(outcome==1,home_team,away_team))%>%transmute(team=winner,winner=1) #get WC winners
  playoff_teams=playoff_teams%>%left_join(WC_winners)%>%filter(winner==1|seed==1)%>%select(-winner)%>%
    group_by(conf)%>%arrange(conf,seed)%>%mutate(seed=row_number())
  #move current games to past games, remove past games from future games
  games_past=rbind( games_past, current_week[,which(colnames(current_week)%in%colnames(games_past)) ])
  games_future=games_future[-which(games_future$game_id%in%games_past$game_id),]
  #incorporate elo shifts
  current_elo=shift_elo(current_week,current_elo)
  
  #compute DIV matchups
  games_future=get_DIV_matchups(games_future,playoff_teams)
  #simulate DIV round
  current_week=simulate_current_week(games_future,current_elo) 
  #pull DIV winners
  DIV_winners=current_week%>%mutate(winner=ifelse(outcome==1,home_team,away_team))%>%transmute(team=winner,winner=1) #get DIV winners
  playoff_teams=playoff_teams%>%left_join(DIV_winners)%>%filter(winner==1)%>%select(-winner)%>%
    group_by(conf)%>%arrange(conf,seed)%>%mutate(seed=row_number())
  #move current games to past games, remove past games from future games
  games_past=rbind( games_past, current_week[,which(colnames(current_week)%in%colnames(games_past)) ])
  games_future=games_future[-which(games_future$game_id%in%games_past$game_id),]
  #incorporate elo shifts
  current_elo=shift_elo(current_week,current_elo)
  
  #compute CC matchups
  games_future=get_CC_matchups(games_future,playoff_teams)
  #simulate CC round
  current_week=simulate_current_week(games_future,current_elo) 
  #pull CC winners
  CC_winners=current_week%>%mutate(winner=ifelse(outcome==1,home_team,away_team))%>%transmute(team=winner,winner=1) #get CC winners
  playoff_teams=playoff_teams%>%left_join(CC_winners)%>%filter(winner==1)%>%select(-winner) 
  #move current games to past games, remove past games from future games
  games_past=rbind( games_past, current_week[,which(colnames(current_week)%in%colnames(games_past)) ])
  games_future=games_future[-which(games_future$game_id%in%games_past$game_id),]
  #incorporate elo shifts
  current_elo=shift_elo(current_week,current_elo)
  
  
  #compute SB matchup
  games_future=get_SB_matchup(games_future,playoff_teams)
  
  #simulate SB
  current_week=simulate_current_week(games_future,current_elo)%>%
    mutate(SBwin=ifelse(result<0,away_team,home_team))
  #move current games to past games, remove past games from future games
  games_past=rbind( games_past, current_week[,which(colnames(current_week)%in%colnames(games_past)) ])
  
  #calculate exit for playoff teams
  home_finish=transmute(games_past,team=home_team,week=week)
  away_finish=transmute(games_past,team=away_team,week=week)
  tm_finish=rbind(home_finish,away_finish)%>%
    group_by(team)%>%mutate(finish=max(week))%>%ungroup()%>%
    distinct(team,finish)
  tm_finish$finish=ifelse(tm_finish$team%in%current_week$SBwin,tm_finish$finish+1,tm_finish$finish)
  
  #join exit with standings
  final_reg_standings=final_reg_standings%>%left_join(tm_finish)
  #add in simnum
  games_past$sim=simnum
  
  return(list(#games_past=games_past,
    final_reg_standings=final_reg_standings))
  
}

###################### Simulate NFL season ######################
cl <- makeCluster(10)
registerDoParallel(cl)
number_of_sims=1#10000
system.time(simulation_output <- foreach(a = 1:number_of_sims,.packages = c("mefa","data.table","nflseedR","dplyr","tidyr")) %dopar% {simulate_season(games,initial_elo,wk18combos,simnum=a,wk18rest = 0)})
simulation_output=simulation_output |> list_flatten()
standings=simulation_output[which(substrRight(names(simulation_output),4)=="ings")]
standings=as.data.frame(rbindlist(standings,use.names = TRUE))%>%
  replace_na(list(seed=8))%>%
  arrange(sim,wins,sos)%>%
  group_by(sim)%>%
  mutate(dp=row_number())%>%
  ungroup()%>%
  mutate(divwinner=ifelse(div_rank==1,1,0),#Division Winner
         confwinner=ifelse(finish>21,1,0),#Conference Winner
         sbwinner=ifelse(finish>22,1,0),#Super Bowl Winner
         no1seed=ifelse(seed==1,1,0),#No.1 Seed
         WC=ifelse(seed==5|seed==6|seed==7,1,0),#To be a Wildcard
         makeplayoffs=ifelse(finish>18,1,0),#Make Playoffs
         firstpick=ifelse(dp==1,1,0),#Worst Record/1st Pick
         reachCC=ifelse(finish>20,1,0),#Reach Conf Championship
         undefeated=ifelse(wins==17,1,0),#Team to go 17-0
         winless=ifelse(wins==0,1,0),#Team to go 0-17
         div2nd=ifelse(div_rank==2,1,0),#Division Finishing Position--2nd
         div3rd=ifelse(div_rank==3,1,0),#Division Finishing Position--3rd
         div4th=ifelse(div_rank==4,1,0))%>% #Division Finishing Position--4th
  arrange(sim,division,div_rank)%>%
  mutate(code=case_when(div_rank==1~paste0(team,lead(team),lead(team,2),lead(team,3)),TRUE~NA),
         code2=case_when(div_rank==1~paste0(team,lead(team)),TRUE~NA),
         winning_division=case_when(sbwinner==1~division,TRUE~"x")
  )
sims=nrow(standings)/32

###################### Summarize results ######################
 

main=summarise(group_by(standings,division,team),Div_Winner=mean(divwinner),Conf=mean(confwinner),SB=mean(sbwinner),
                 No1_Seed=mean(no1seed),Wildcard=mean(WC),Playoffs=mean(makeplayoffs),No1_Pick=mean(firstpick),
                 Make_CC=mean(reachCC),Undefeated=mean(undefeated),Winless=mean(winless),Div_2nd=mean(div2nd),
                 Div_3rd=mean(div3rd),Div_4th=mean(div4th))%>%arrange(division,SB)

#Exact Division Order
exact_order=exact_order_fun(standings)


#Any Top 2
div_top2=top_two_fun(standings)

#Winning Division/#Winning Conference
win_possibilities=win_possibilities_fun(standings)
   
#write CSVs
write.csv(output,"Main.csv",row.names = FALSE)
write.csv(exact_order,"Exact_Division_Order.csv",row.names = FALSE)
write.csv(winning_div_conf2,"Winning_Div_Conf.csv",row.names = FALSE)
write.csv(div_top2,"Division_Top2.csv",row.names = FALSE)
write.csv(win_possibilities,"win_possibilities.csv",row.names = FALSE)

