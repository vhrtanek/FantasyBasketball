#Fantasy Basketball Stats

library(rvest)
library(stringr)
library(tidyr)
library(methods)
library(httr)
library(jsonlite)
library(dplyr)

#scoring
url <- 'http://www.espn.com/nba/statistics/player/_/stat/scoring-per-game'
site <- read_html(url)
player_stats <- html_nodes(site, 'table')
scoring <- html_table(player_stats,fill=TRUE)[[1]]

url <- 'http://www.espn.com/nba/statistics/player/_/stat/scoring-per-game/sort/avgPoints/count/41'
site <- read_html(url)
player_stats <- html_nodes(site, 'table')
scoring1 <- html_table(player_stats,fill=TRUE)[[1]]

#rebounds
url <- 'http://www.espn.com/nba/statistics/player/_/stat/rebounds'
site <- read_html(url)
player_stats <- html_nodes(site, 'table')
reb <- html_table(player_stats,fill=TRUE)[[1]]

url <- 'http://www.espn.com/nba/statistics/player/_/stat/rebounds/sort/avgRebounds/count/41'
site <- read_html(url)
player_stats <- html_nodes(site, 'table')
reb1 <- html_table(player_stats,fill=TRUE)[[1]]

#Assists
url <- 'http://www.espn.com/nba/statistics/player/_/stat/assists'
site <- read_html(url)
player_stats <- html_nodes(site, 'table')
ast <- html_table(player_stats,fill=TRUE)[[1]]

url <- 'http://www.espn.com/nba/statistics/player/_/stat/assists/sort/avgAssists/count/41'
site <- read_html(url)
player_stats <- html_nodes(site, 'table')
ast1 <- html_table(player_stats,fill=TRUE)[[1]]

#Steals
url <- 'http://www.espn.com/nba/statistics/player/_/stat/steals'
site <- read_html(url)
player_stats <- html_nodes(site, 'table')
steal <- html_table(player_stats,fill=TRUE)[[1]]

url <- 'http://www.espn.com/nba/statistics/player/_/stat/steals/sort/avgSteals/count/41'
site <- read_html(url)
player_stats <- html_nodes(site, 'table')
steal1 <- html_table(player_stats,fill=TRUE)[[1]]

#blocks
url <- 'http://www.espn.com/nba/statistics/player/_/stat/blocks'
site <- read_html(url)
player_stats <- html_nodes(site, 'table')
block <- html_table(player_stats,fill=TRUE)[[1]]

url <- 'http://www.espn.com/nba/statistics/player/_/stat/blocks/sort/avgBlocks/count/41'
site <- read_html(url)
player_stats <- html_nodes(site, 'table')
block1 <- html_table(player_stats,fill=TRUE)[[1]]

score <- rbind(scoring, scoring1)
temp <- names(score) %in% c("X2", "X6", "X8", "X9", "X10") 
score <- score[temp]
colnames(score) <- c('PLAYER','PTS','FG%','3PM-3PA','3P%')
score <- subset(score, score$PLAYER != 'PLAYER')

rebounds <- rbind(reb, reb1)
temp <- names(rebounds) %in% c("X2", "X5", "X11") 
rebounds <- rebounds[temp]
colnames(rebounds) <- c('PLAYER','MPG','REB')
rebounds <- subset(rebounds, rebounds$PLAYER != 'PLAYER')

assists <- rbind(ast, ast1)
temp <- names(assists) %in% c("X2", "X7", "X11") 
assists <- assists[temp]
colnames(assists) <- c('PLAYER','APG','AST/TUR RATION')
assists <- subset(assists, assists$PLAYER != 'PLAYER')

steals <- rbind(steal, steal1)
temp <- names(steals) %in% c("X2", "X7") 
steals <- steals[temp]
colnames(steals) <- c('PLAYER','STEALS')
steals <- subset(steals, steals$PLAYER != 'PLAYER')

blocks <- rbind(block, block1)
temp <- names(blocks) %in% c("X2", "X8") 
blocks <- blocks[temp]
colnames(blocks) <- c('PLAYER','bLOCKS')
blocks <- subset(blocks, blocks$PLAYER != 'PLAYER')

fanBask <- merge(x = score, y = rebounds, by = "PLAYER", all = TRUE)
fanBask <- merge(x = fanBask, y = assists, by = "PLAYER", all = TRUE)
fanBask <- merge(x = fanBask, y = steals, by = "PLAYER", all = TRUE)
fanBask <- merge(x = fanBask, y = blocks, by = "PLAYER", all = TRUE)

backup <- fanBask

str(fanBask)

fanBask$PTS <- sapply(fanBask$PTS, function(y) as.numeric(y))
fanBask$`FG%` <- sapply(fanBask$`FG%`, function(y) as.numeric(y))
fanBask$`3P%` <- sapply(fanBask$`3P%`, function(y) as.numeric(y))
fanBask$MPG <- sapply(fanBask$MPG, function(y) as.numeric(y))
fanBask$REB <- sapply(fanBask$REB, function(y) as.numeric(y))
fanBask$APG <- sapply(fanBask$APG, function(y) as.numeric(y))
fanBask$bLOCKS <- sapply(fanBask$bLOCKS, function(y) as.numeric(y))
fanBask$STEALS <- sapply(fanBask$STEALS, function(y) as.numeric(y))
fanBask$`AST/TUR RATION` <- sapply(fanBask$`AST/TUR RATION`, function(y) as.numeric(y))

fanBask$TreePoints <- as.numeric(substring(fanBask$`3PM-3PA`,1,3))

mPTS <- mean(fanBask$PTS, na.rm = T)
mFGp <- mean(fanBask$`FG%`, na.rm = T)
m3pp <- mean(fanBask$`3P%`, na.rm = T)
mMPG <- mean(fanBask$MPG, na.rm = T)
mREB <- mean(fanBask$REB, na.rm = T)
mAPG <- mean(fanBask$APG, na.rm = T)
mBLK <- mean(fanBask$bLOCKS, na.rm = T)
mSTL <- mean(fanBask$STEALS, na.rm = T)
mATR <- mean(fanBask$`AST/TUR RATION`, na.rm = T)
mTRP <- mean(fanBask$TreePoints, na.rm = T)

fanBask$iPTS <- ifelse(is.na(fanBask$PTS) == T, 0, (fanBask$PTS/mPTS)*100)
fanBask$iFGp <- ifelse(is.na(fanBask$`FG%`) == T, 0, (fanBask$`FG%`/mFGp)*100)
fanBask$i3pp <- ifelse(is.na(fanBask$`3P%`) == T, 0, (fanBask$`3P%`/m3pp)*100)
fanBask$iMPG <- ifelse(is.na(fanBask$MPG) == T, 0, (fanBask$MPG/mMPG)*100)
fanBask$iREB <- ifelse(is.na(fanBask$REB) == T, 0, (fanBask$REB/mREB)*100)
fanBask$iAPG <- ifelse(is.na(fanBask$APG) == T, 0, (fanBask$APG/mAPG)*100)
fanBask$iBLK <- ifelse(is.na(fanBask$bLOCKS) == T, 0, (fanBask$bLOCKS/mBLK)*100)
fanBask$iSTL <- ifelse(is.na(fanBask$STEALS) == T, 0, (fanBask$STEALS/mSTL)*100)
fanBask$iATR <- ifelse(is.na(fanBask$`AST/TUR RATION`) == T, 0, (fanBask$`AST/TUR RATION`/mATR)*100)
fanBask$iTRP <- ifelse(is.na(fanBask$TreePoints) == T, 0, (fanBask$TreePoints/mTRP)*100)

# get 3 points

fanBask$Ranking <- (fanBask$iPTS + fanBask$iFGp + fanBask$i3pp + fanBask$iMPG + fanBask$iREB +
  fanBask$iAPG + fanBask$iBLK + fanBask$iSTL + fanBask$iATR + fanBask$iTRP) 


fanBask <- arrange(fanBask, desc(Ranking))

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

fanBask$Position <- substrRight(fanBask$PLAYER, 2)
fanBask$Position <- trimws(fanBask$Position, "l")

split <- split(fanBask, fanBask$Position)

C <- arrange(split$C, desc(split$C$Ranking))
FW <- (arrange(split$F, desc(split$F$Ranking)))
G <- (arrange(split$G, desc(split$G$Ranking)))
PF <- (arrange(split$PF, desc(split$PF$Ranking)))
PG <- (arrange(split$PG, desc(split$PG$Ranking)))
SF <- (arrange(split$SF, desc(split$SF$Ranking)))
SG <- (arrange(split$SG, desc(split$SG$Ranking)))


