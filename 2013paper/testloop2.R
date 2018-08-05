team1typecolumn<-0
team1namecolumn<-0
team1scorecolumn<-0
  
team1<-0
team2<-0
team1score<-0
team2score<-0
team1pos<-0
team1p1<-0
team2pos<-0
team2p1<-0
team1type<-0
team2type<-0
output<- matrix()

j<-1
for(j in 1:100){
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




###Assigns team points###

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



#Assigns teams to play type



  if(team1 == substring(list[[j]][[2]][i],2,4)){
    if(team1p1==0){
      
      if(str_detect(list[[j]][[2]][i],"MISSED")==TRUE){
        team1p1 <-1
        if(list[[j]][[4]][i] >=28 ){
          team1type <- 1
        }else{team1type <- 2
        }
      }
      if(str_detect(list[[j]][[2]][i], "PTS)")){ 
        
        team1p1 <-1
        if(list[[j]][[4]][i] >=28 ){
          team1type <- 1
        }else{team1type <- 2
        }
        
      }
      if(str_detect(list[[j]][[2]][i], "TURNOVER")){ 
        
        team1p1 <-1
        if(list[[j]][[4]][i] >=28 ){
          team1type <- 1
        }else{team1type <- 2
        }
        
      }
    }
  }
  if(team2 == substring(list[[j]][[2]][i],2,4)){
    if(team2p1==0){
      
      if(str_detect(list[[j]][[2]][i],"MISSED")==TRUE){
        team2p1 <-1
        if(list[[j]][[4]][i] >=28 ){
          team2type <- 1
        }else{team2type <- 2
        }
      }
      if(str_detect(list[[j]][[2]][i], "PTS)")){ 
        
        team2p1 <-1
        if(list[[j]][[4]][i] >=28 ){
          team2type <- 1
        }else{team2type <- 2
        }
        
      }
      if(str_detect(list[[j]][[2]][i], "TURNOVER")){ 
        
        team2p1 <-1
        if(list[[j]][[4]][i] >=28 ){
          team2type <- 1
        }else{team2type <- 2
        }
        
      }
    }
  }
}

team1typecolumn<- c(team1typecolumn,team1type)
team2typecolumn<- c(team2typecolumn,team2type)
team1scorecolumn<- c(team1scorecolumn,team1score)
team1namecolumn<- c(team1namecolumn,team1)

team1<-0
team2<-0
team1score<-0
team2score<-0
team1pos<-0
team1p1<-0
team2pos<-0
team2p1<-0
team1type<-0
team2type<-0

  }

