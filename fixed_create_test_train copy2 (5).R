rm(list = ls())
library(data.table)
library(caret)
library(ISLR)
library(Metrics)


#load data
ncaa<- fread("./project/volume/data/interim/Stage2DataFiles/NCAATourneyDetailedResults.csv")
regular<-fread("./project/volume/data/interim/Stage2DataFiles/RegularSeasonDetailedResults.csv")
#created all games table
all_games_table<- rbind(regular,ncaa)
W_stat<- all_games_table[,.(Season,DayNum,WTeamID,WScore,WLoc,NumOT,WFGM,WFGA,WFGM3,WFGA3,WFTM,WFTA,WOR,WDR,WAst,WTO,WStl,WBlk,WPF)]
L_stat<- all_games_table[,.(Season,DayNum,LTeamID,LScore,WLoc,NumOT,LFGM,LFGA,LFGM3,LFGA3,LFTM,LFTA,LOR,LDR,LAst,LTO,LStl,LBlk,LPF)]
W_stat$result<- 1
L_stat$result<- 0

W_stat<- setnames(W_stat,c("Season","DayNum","WTeamID","WScore","WLoc","NumOT","WFGM","WFGA","WFGM3"
,"WFGA3","WFTM","WFTA","WOR","WDR","WAst","WTO","WStl","WBlk","WPF","result"),c("Season","DayNum","TeamID","Score",
"Loc","NumOT","FGM","FGA","FGM3","FGA3","FTM","FTA","OR","DR","Ast","TO","Stl","Blk","PF","result"))

L_stat<- setnames(L_stat,c("Season","DayNum","LTeamID","LScore","WLoc","NumOT","LFGM","LFGA","LFGM3","LFGA3","LFTM","LFTA","LOR","LDR","LAst",
"LTO","LStl","LBlk","LPF","result"),
c("Season","DayNum","TeamID","Score","Loc","NumOT",
  "FGM","FGA","FGM3","FGA3","FTM","FTA","OR","DR","Ast","TO","Stl","Blk","PF","result"))
master_stats_table<- rbind(W_stat,L_stat)
master_stats_table<-data.table(master_stats_table)

master_stats_table<- master_stats_table[,-c(5,6)]
#for loop
stats_by_day<-NULL

for (i in 1:max(master_stats_table$DayNum)){

  sub_master_stats_table<-master_stats_table[DayNum < i]
  team_stats_by_day<-dcast(sub_master_stats_table, TeamID+Season~.,mean,value.var=c("FGM","FGA","FGM3","FGA3","FTM","FTA","OR","DR","Ast","TO","Stl","Blk","PF"))
  
  team_stats_by_day$dayNum<- i
  
  stats_by_day<-rbind(stats_by_day,team_stats_by_day)
  
}


#figure out who plays who
W_matches <- all_games_table[,.(Season, WTeamID, LTeamID, DayNum)]
L_matches <- all_games_table[,.(Season, WTeamID, LTeamID, DayNum)]
W_matches$result <- 1
L_matches$result <- 0
setnames(
  W_matches,
  c('Season','WTeamID','LTeamID','DayNum', 'result'),
  c('Season','TeamID','oppTeamID', 'DayNum', 'result')
) 
setnames(
  L_matches,
  c('Season', 'WTeamID', 'LTeamID','DayNum','result'),
  c('Season','oppTeamID', 'TeamID', 'DayNum','result')
) 
matches<- rbind(W_matches,L_matches)


#merge train set
#you need to do left join with stats by day so I changed to stats by day
train_w <- merge(W_matches, stats_by_day, by.x= c('Season', 'DayNum', 'TeamID'), by.y=c('Season', 'dayNum', 'TeamID'))
train_l<- merge(L_matches, stats_by_day, by.x= c('Season', 'DayNum', 'TeamID'), by.y=c('Season', 'dayNum', 'TeamID'))
train<- merge(train_w,train_l, by.x = c('Season','TeamID','DayNum'),by.y=c('Season','oppTeamID','DayNum'))



train$FGM <- train$FGM.x-train$FGM.y
train$FGA <- train$FGA.x-train$FGA.y
train$FGM3 <- train$FGM3.x-train$FGM3.y
train$FGA3 <- train$FGA3.x-train$FGA3.y
train$FTM <- train$FTM.x-train$FTM.y
train$FTA <- train$FTA.x-train$FTA.y
train$OR <- train$OR.x-train$OR.y
train$DR <- train$DR.x-train$DR.y
train$Ast <- train$Ast.x-train$Ast.y
train$TO <- train$TO.x-train$TO.y
train$Stl <- train$Stl.x-train$Stl.y
train$Blk <- train$Blk.x-train$Blk.y
train$PF <- train$PF.x-train$PF.y


new_train_diff<-train[,.(Season,TeamID,oppTeamID,FGM,FGA,FGM3,FGA3,FTM,FTA,OR,DR,Ast,TO,Stl,Blk,PF)]
new_train_diff$result <- 1
#create the losing observation, they will have the - stats of the winning 
new_train_diff1 <-train[,.(Season,TeamID,oppTeamID,FGM = -1*FGM,FGA = -1*FGA,FGM3 = -1*FGM3,FGA3 = -1*FGA3,FTM = -1*FTM,FTA = -1*FTA, OR = -1*OR,DR = -1*DR,Ast = -1*Ast,TO= -1*TO, Stl = -1*Stl,Blk = -1*Blk,PF = -1*PF)]
new_train_diff1$result <- 0
#cbind to create new new_train_diff
new_train_diff <- rbind(new_train_diff, new_train_diff1)

test <- fread("project/volume/data/interim/Stage2DataFiles/examp_sub.csv")
test$WTeamID <- as.integer(matrix(unlist(strsplit(test$id, "_")), ncol=2, byrow=T)[,1])
#test
test$LTeamID <- as.integer(matrix(unlist(strsplit(test$id, "_")), ncol=2, byrow=T)[,2])
test
setnames(
  test,
  c( 'WTeamID', 'LTeamID', 'Result'),
  c( 'TeamID', 'oppTeamID', 'result')
) 

test$Season <- 2019
test$DayNum<- 133
#again, merge with stats by day
test <- merge(test, stats_by_day, by.x= c('Season', 'DayNum', 'TeamID'), by.y=c('Season', 'dayNum', 'TeamID'))
test <- merge(test, stats_by_day, by.x= c('Season', 'DayNum', 'oppTeamID'), by.y=c('Season', 'dayNum', 'TeamID'))



test$FGM <- test$FGM.x-test$FGM.y
test$FGA <- test$FGA.x-test$FGA.y
test$FGM3 <- test$FGM3.x-test$FGM3.y
test$FGA3 <- test$FGA3.x-test$FGA3.y
test$FTM <- test$FTM.x-test$FTM.y
test$FTA <- test$FTA.x-test$FTA.y
test$OR <- test$OR.x-test$OR.y
test$DR <- test$DR.x-test$DR.y
test$Ast <- test$Ast.x-test$Ast.y
test$TO <- test$TO.x-test$TO.y
test$Stl <- test$Stl.x-test$Stl.y
test$Blk <- test$Blk.x-test$Blk.y
test$PF <- test$PF.x-test$PF.y


test<-data.table(test)


new_test_diff<- test[,.(Season,TeamID,oppTeamID,FGM,FGA,FGM3,FGA3,FTM,FTA,OR,DR,Ast,TO,Stl,Blk,PF)]
#you assigned new_train_diff$result <- 0 so that is why it throws a not converging error
new_test_diff$result<-0
#model
model <- glm(result ~ FGM+FGM3+FGA3+FGA+FTA+FTM+OR+TO+DR+Stl+Blk+Ast+PF, family = binomial, data=new_train_diff)
new_test_diff$results<-predict(model,newdata=new_test_diff, type='response')


submit <-new_test_diff[,.(TeamID, oppTeamID, results)]
#create the id column that matches with the submission guidelines
submit$id = paste(submit$TeamID, submit$oppTeamID, sep ="_")
setnames(submit,c("results"),c("Result"))
final_submit<- submit[,.(id,Result)]


fwrite(final_submit,"./final_submit.csv")




