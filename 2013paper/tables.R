res <- read.csv("C:/Users/Ryan/Dropbox/stat140sl/nba/twoforone.csv", stringsAsFactors=FALSE)

two.1 <- res[res$init==1 & res$type=="2f1",] #initiating 2-for-1
two.0 <- res[res$init==0 & res$type=="2f1",] #defending 2-for-1
one.1 <- res[res$init==1 & res$type=="1f1",] #initiating 1-for-1
one.0 <- res[res$init==0 & res$type=="1f1",] #defending 1-for-1

#average number of possessions: 2-for-1
mean(two.1$pos)
mean(two.0$pos)

#average number of possessions: 1-for-1
mean(one.1$pos)
mean(one.0$pos)

#average result: 2-for-1
mean(two.1$pts)
mean(two.0$pts)
mean(two.1$pts) - mean(two.0$pts)
t.test(two.1$pts, two.0$pts, alternative="greater")

#average result: 1-for-1
mean(one.1$pts)
mean(one.0$pts)
mean(one.1$pts) - mean(one.0$pts)
t.test(one.1$pts, one.0$pts, alternative="greater")

#comparing 2-for-1 against 1-for-1
t.test(two.1$pts - two.0$pts, one.1$pts - one.0$pts, alternative="greater")

#comparing point differential histograms
hist(two.1$pts - two.0$pts, freq=FALSE, breaks=seq(-6,7,1), ylim=c(0,0.4))
hist(one.1$pts - one.0$pts, freq=FALSE, breaks=seq(-6,7,1), ylim=c(0,0.4))

library(ggplot2)
d <- c(two.1$pts - two.0$pts,
          one.1$pts - one.0$pts)
pd <- data.frame(diff = d, type = c(rep("2f1", nrow(two.1)), rep("1f1", nrow(one.1))))
ggplot(pd, aes(x=diff)) + 
  geom_density(data = subset(pd, type=="2f1"), fill = guide_legend("red"), alpha = 0.7, aes(y=..count../sum(..count..))) + 
  geom_density(data = subset(pd, type=="1f1"), fill = "grey", alpha = 0.7, aes(y=..count../sum(..count..))) +
  scale_y_continuous("Frequency") +
  scale_x_continuous("Point differential on sequence", breaks=-6:7) +
  geom_vline(xintercept=0)

t1 <- sort(table(two.1$tm), decreasing=TRUE)
count <- data.frame(rownames(t1), t1)
names(count) <- c("tm", "count2f1")
pts <- aggregate(two.1$pts, list(two.1$tm), sum)
names(pts) <- c("tm", "pts")
opppts <- aggregate(two.0$pts, list(two.1$opp), sum)
names(opppts) <- c("tm", "opppts")
pos <- aggregate(two.1$pos, list(two.1$tm), sum)
names(pos) <- c("tm", "pos")
opppos <- aggregate(two.0$pos, list(two.1$opp), sum)
names(opppos) <- c("tm", "opppos")

teams <- merge(count, pts)
teams <- merge(teams, opppts)
teams$ptmargin <- teams$pts - teams$opppts
teams$avgmargin <- teams$ptmargin / teams$count
teams <- teams[order(teams$count, decreasing=TRUE),]

library(ggplot2)
qplot(teams$count2f1, teams$avgmargin, label=teams$tm, geom="text") + 
  #geom_text(size=3) +
  geom_abline(intercept=0.355, slope=0)+
  scale_x_continuous("Number of 2-for-1s initiated") +
  scale_y_continuous("Average margin") +
  labs(title="Two-for-ones by team")