#read raw text data
pbyp <- read.delim("C:/Users/Ryan/Dropbox/analytics/nba1112/playbyplay20120510040.txt",
                   stringsAsFactors=FALSE)

#split by games to order lines
gms <- split(pbyp, as.factor(pbyp$GameID))
plays <- data.frame(matrix(nrow=0, ncol=4))
names(plays) <- names(pbyp)
for (i in 1:length(gms)){
  frame <- as.data.frame(gms[i])
  names(frame) <- names(pbyp)
  frame <- frame[order(frame[,2]),]
  plays <- rbind(plays,frame)
}

#save object
save(plays, file="C:/Users/Ryan/Dropbox/stat140sl/nba/plays.RDA")

#load raw RDA
rm(list=ls())
load("C:/Users/Ryan/Dropbox/stat140sl/nba/plays.RDA")

#order data frame (for combination later)
plays$num <- 1:nrow(plays)

#split data into regulation and overtime
reg <- plays[substr(plays$TimeRemaining, 1, 1) != "-",]
ot <- plays[substr(plays$TimeRemaining, 1, 1) == "-",]
nrow(reg) + nrow(ot) == nrow(plays)

#keep only last minute
#mark period
q1 <- reg[as.numeric(substr(reg$TimeRemaining,4,5)) < 37 & 
      as.numeric(substr(reg$TimeRemaining,4,5)) >= 36,]
q1$q <- rep("q1", nrow(q1))
q1$sec <- as.numeric(substr(q1$TimeRemaining,7,8))

q2 <- reg[as.numeric(substr(reg$TimeRemaining,4,5)) < 25 & 
            as.numeric(substr(reg$TimeRemaining,4,5)) >= 24,]
q2$q <- rep("q2", nrow(q2))
q2$sec <- as.numeric(substr(q2$TimeRemaining,7,8))

q3 <- reg[as.numeric(substr(reg$TimeRemaining,4,5)) < 13 & 
            as.numeric(substr(reg$TimeRemaining,4,5)) >= 12,]
q3$q <- rep("q3", nrow(q3))
q3$sec <- as.numeric(substr(q3$TimeRemaining,7,8))

q4 <- reg[as.numeric(substr(reg$TimeRemaining,4,5)) < 1 & 
            as.numeric(substr(reg$TimeRemaining,4,5)) >= 0,]
q4$q <- rep("q4", nrow(q4))
q4$sec <- as.numeric(substr(q4$TimeRemaining,7,8))

ot1 <- ot[as.numeric(substr(ot$TimeRemaining,5,6)) == 4 |
            (as.numeric(substr(ot$TimeRemaining,5,6))==5 &
                 as.numeric(substr(ot$TimeRemaining,8,9))==0 &
               substr(ot$Entry,1,1) == "E"),]
ot1$q <- rep("ot1", nrow(ot1))
ot1$sec <- 60 - as.numeric(substr(ot1$TimeRemaining,8,9))
ot1[ot1$sec==60,]$sec <- 0

ot2 <- ot[as.numeric(substr(ot$TimeRemaining,5,6)) == 9 |
            (as.numeric(substr(ot$TimeRemaining,5,6))==10 &
               as.numeric(substr(ot$TimeRemaining,8,9))==0 &
               substr(ot$Entry,1,1) == "E"),]
ot2$q <- rep("ot2", nrow(ot2))
ot2$sec <- 60 - as.numeric(substr(ot2$TimeRemaining,8,9))
ot2[ot2$sec==60,]$sec <- 0

ot3 <- ot[as.numeric(substr(ot$TimeRemaining,5,6)) == 14 |
            (as.numeric(substr(ot$TimeRemaining,5,6))==15 &
               as.numeric(substr(ot$TimeRemaining,8,9))==0 &
               substr(ot$Entry,1,1) == "E"),]
ot3$q <- rep("ot3", nrow(ot3))
ot3$sec <- 60 - as.numeric(substr(ot3$TimeRemaining,8,9))
ot3[ot3$sec==60,]$sec <- 0

ot4 <- ot[as.numeric(substr(ot$TimeRemaining,5,6)) == 19 |
            (as.numeric(substr(ot$TimeRemaining,5,6))==20 &
               as.numeric(substr(ot$TimeRemaining,8,9))==0 &
               substr(ot$Entry,1,1) == "E"),]
ot4$q <- rep("ot4", nrow(ot4))
ot4$sec <- 60 - as.numeric(substr(ot4$TimeRemaining,8,9))
ot4[ot4$sec==60,]$sec <- 0


late <- rbind(q1, q2, q3, q4, ot1, ot2, ot3, ot4)
late <- late[order(late$num),]

write.csv(late, "C:/Users/Ryan/Dropbox/stat140sl/nba/lateplays.csv", row.names=FALSE)

lateplays <- read.csv("C:/Users/Ryan/Dropbox/stat140sl/nba/lateplays.csv",
                      stringsAsFactors=FALSE)

l <- lateplays[,-5]

rm(lateplays)

# #get away and home team names
# l$a <- substr(l$GameID, 9, 11)
# l$h <- substr(l$GameID, 12, 14)

write.csv(l, "C:/Users/Ryan/Dropbox/stat140sl/nba/lateplays.csv", row.names=FALSE)

#####################

library(stringr)

l <- read.csv("C:/Users/Ryan/Dropbox/stat140sl/nba/lateplays.csv", 
              stringsAsFactors=FALSE)

#keep chars consistent
l$Entry <- toupper(l$Entry)

#plays <= 35 seconds only
l1 <- l[l$sec <= 35,]

#no *defensive* fouls
l2<- l1[!(str_detect(l1$Entry, "FOUL") & !str_detect(l1$Entry, "TURNOVER")),]

#no subs
l3<- l2[!str_detect(l2$Entry, "SUBSTITUTION"),]

#take out unncessary info for list
l4 <- l3[,-c(2,3)]

#take out timeouts
l5 <- l4[!str_detect(l4$Entry, "TIMEOUT"),]

#take out "End of" lines
l6 <- l5[!str_detect(l5$Entry, "END OF"),]

l7 <- l6[!(str_detect(l6$Entry, "VIOLATION") & !str_detect(l6$Entry, "TO)")),]
l8 <- l7[!(str_detect(l7$Entry,"TECHNICAL") & !str_detect(l7$Entry,"FREE THROW")),]
l9 <- l8[!str_detect(l8$Entry,"EJECTION"),]
l10 <- l9[!str_detect(l9$Entry,"REPLAY"),]
l11 <- l10[!str_detect(l10$Entry,"JUMP BALL"),]

# final CSV for list
write.csv(l11, "C:/Users/Ryan/Dropbox/stat140sl/nba/lateplays.csv", row.names=FALSE)

rm(list=ls())

l.new <- read.csv("C:/Users/Ryan/Dropbox/stat140sl/nba/lateplays.csv",
                  stringsAsFactors=FALSE)

#check for consistency
test <- l.new
test1 <- test[!str_detect(test$Entry,"PTS)"),]
test2 <- test1[!str_detect(test1$Entry,"MISSED"),]
test3 <- test2[!str_detect(test2$Entry,"REBOUND"),]
test4 <- test3[!str_detect(test3$Entry,"TURNOVER"),] #should be 0

## MAKE LIST
list <- split(l.new, as.factor(paste(l.new$GameID, l.new$q)))

## SAVE LIST
save(list, file="C:/Users/Ryan/Dropbox/stat140sl/nba/lateplays-list.RDA")

rm(list=ls()) ## clear all objects

load("C:/Users/Ryan/Dropbox/stat140sl/nba/lateplays-list.RDA")
