rm(list=ls())
load("C:/Users/Ryan/Dropbox/two.for.one/lateplays-list.RDA")
library(stringr)

type <- ""
pts <- 0
pos <- 0
tm <- ""
id <- ""
q <- ""

#BIG LOOP
#for (j in 1:20){
for (j in 1:length(list)) { #j<-2

  #initialize 2x4=8 entries in table
  team1<-""
  team2<-""
  team1score<-0
  team2score<-0
  team1pos<-0
  team2pos<-0
  team1type<-""
  team2type<-""
  
  #dummy initializations for test
#   j <- 1
#   i <- 1

### 1. Team names
# team1 is initializing team
  
for(i in 1:nrow(list[[j]])){
  if(team2 == ""){
    if(team1 == ""){
      team1 <- substring(list[[j]][[2]][i],2,4)
    }else{
      if(substring(list[[j]][[2]][i],2,4)!= team1){
        team2 <- substring(list[[j]][[2]][i],2,4)
      }
    }
  }
}

#loop to extract points
  
  for(i in 1:nrow(list[[j]])){
    if(team1 == substring(list[[j]][[2]][i],2,4)){
      if(str_detect(list[[j]][[2]][i],"SHOT: MADE")==TRUE){
        if(str_detect(list[[j]][[2]][i],"3PT SHOT: MADE")==TRUE){
          team1score <- team1score+3
        }else{team1score <- team1score+2
        }
      }
      if(str_detect(list[[j]][[2]][i], "FREE THROW")){ 
        
        if(str_detect(list[[j]][[2]][i], "MISSED")){
          team1score <- team1score +0
        }else{
          team1score <-team1score +1
        }
        
        
      }
    }
    if(team2 == substring(list[[j]][[2]][i],2,4)){
      if(str_detect(list[[j]][[2]][i],"SHOT: MADE")==TRUE){
        if(str_detect(list[[j]][[2]][i],"3PT SHOT: MADE")==TRUE){
          team2score <- team2score+3
        }else{team2score <- team2score+2
        }
      }
      if(str_detect(list[[j]][[2]][i], "FREE THROW")){ 
        
        if(str_detect(list[[j]][[2]][i], "MISSED")){
          team2score <- team2score +0
        }else{
          team2score <-team2score +1
        }
        
        
      }
    }
  }
  
#loop to extract poss
  #take out rebounds with 0 seconds left
  frame <- list[[j]][!(str_detect(list[[j]]$Entry, "REBOUND") & list[[j]]$sec==0),]
  poss <- substr(frame$Entry, 2,4)
  t1 <- which(poss == team1)
  t2 <- which(poss == team2)
  team1pos <- 1 + sum(t1[2:length(t1)] - t1[1:length(t1)-1] != 1)
  team2pos <- 1 + sum(t2[2:length(t2)] - t2[1:length(t2)-1] != 1)
  
#loop to extract type (same for both)

  if(list[[j]][!str_detect(list[[j]]$Entry,"REBOUND"),][[4]][1] > 28){
    team1type <- "2f1"
    team2type <- "2f1"
  } else {
    team1type <- "1f1"
    team2type <- "1f1"
  }
  
  type <- c(type, team1type, team2type)
  pts <- c(pts, team1score, team2score)
  pos <- c(pos, team1pos, team2pos)
  tm <- c(tm, team1, team2)
  id <- c(id, list[[j]][[1]][1], list[[j]][[1]][1])
  q <- c(q, list[[j]][[3]][1], list[[j]][[3]][1])
  
} #end BIG LOOP

res <- data.frame(type, pts, pos, tm, id, q)
res1 <- res[-1,]
res1$init <- rep(c(1,0),nrow(res1)/2)
res1[res1$tm=="",]

write.csv(res1, "C:/Users/Ryan/Dropbox/stat140sl/nba/twoforone.csv", row.names=FALSE)

##manual clean (insert missing team names)

##code to take out fourth quarters/OTs, add opponent column
res <- res1[!(res$q %in% c("q4", "ot1" , "ot2" , "ot3", "ot4")),]

res$opp <- ""
res[seq(1,nrow(res)-1,2),8] <- res$tm[seq(2,nrow(res),2)]
res[seq(2,nrow(res),2),8] <- res$tm[seq(1,nrow(res)-1,2)]

write.csv(res, "C:/Users/Ryan/Dropbox/stat140sl/nba/twoforone.csv", row.names=FALSE)