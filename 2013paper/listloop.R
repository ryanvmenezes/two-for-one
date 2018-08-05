load("C:/Users/Shane/Desktop/140slfiles/Basketball/lateplays-list.RDA")

test3<-test2[test2$sec<36,]

data<-test3[1:8,]


output<- data.frame()


team1<-0
team2<-0
team1score<-0
team2score<-0
team1pos<-0
team2pos<-0
team1type<-0
team2type<-0

nrow(list[[1]])

for(j in 1:3987){
for(i in 1:nrow(list[[j]])){
  if(team2 == 0){
    if(team1 == 0){
      team1 <- substring(substring(list[[j]][[2]][i],2,4))
    }else{
      if(substring(substring(list[[j]][[2]][i],2,4)!= team1){
        team2 <- substring(substring(list[[j]][[2]][i],2,4)
      }
    }
    }
  
  
  
  
  
  if(substring(list[[j]][[2]][i],2,4) != substring(test2$Entry[i+1],1,4)){
   
        
    
    
    if(team1 == substring(test2$Entry[i],1,4)){
      if(str_detect(test2$Entry[i],"SHOT: MADE")==TRUE){
        if(str_detect(test2$Entry[i],"3PT SHOT: MADE")==TRUE){
          team1score <- team1score+3
        }else{temp.teamvalue1 <- temp.teamvalue1+2
        }
      }
    }
    
    
    if(team2 == substring(test2$Entry[i],1,4)){
      if(str_detect(test2$Entry[i],"SHOT: MADE")==TRUE){
        if(str_detect(test2$Entry[i],"3PT SHOT: MADE")==TRUE){
          team2score <- team2score+3
        }else{temp.teamvalue2 <- temp.teamvalue2+2
              last.team <- team2
        }
      }
    }
    
    
    
   
    
}
}
}

nrow()
if(substring(list[[1]][[2]][8],2,4) != substring(list[[1]][[2]][8+1],2,4))
