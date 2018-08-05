####Working parts######


#Assigns to team 1 or team 2
#Where j is the current list
team1<-0
team2<-0
team1score<-0
team2score<-0

j<-2
list[[2]][[2]][1:8]


for(i in 1:nrow(list[[j]])){
  if(team2 == 0){
    if(team1 == 0){
      team1 <- substring(list[[j]][[2]][i],2,4)
    }else{
      if(substring(list[[j]][[2]][i],2,4)!= team1){
        team2 <- substring(list[[j]][[2]][i],2,4)
      }
    }
  }
}



###Assigns team points###

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














