library(RSQLite)
library(sendmailR)
library(plyr)

drv <- dbDriver("SQLite")
con <- dbConnect(drv, "/home/ec2-user/sports2015/NCF/sports.db")

tables <- dbListTables(con)

lDataFrames <- vector("list", length=length(tables))

## create a data.frame for each table
for (i in seq(along=tables)) {
  if(tables[[i]] == 'NCFSBHalfLines' | tables[[i]] == 'NCFSBLines'){
  lDataFrames[[i]] <- dbGetQuery(conn=con, statement=paste("SELECT away_team, home_team, game_date, line, spread, max(game_time) as
game_time from ", tables[[i]], " group by away_team, home_team, game_date;"))
  } else {
	lDataFrames[[i]] <- dbGetQuery(conn=con, statement=paste("SELECT * FROM '", tables[[i]], "'", sep=""))
  }
  cat(tables[[i]], ":", i, "\n")
}

halflines <- lDataFrames[[which(tables == "NCFSBHalfLines")]]
lines <- lDataFrames[[which(tables == "NCFSBLines")]]
lookup <- lDataFrames[[which(tables == "NCFSBTeamLookup")]]
halfbox <- lDataFrames[[which(tables == "halfBoxScore")]] 
finalbox <- lDataFrames[[which(tables == "finalBoxScore")]] 
games <- lDataFrames[[which(tables == "games")]]

## Add ESPN abbreviations to line data
halflines$away_espn<-lookup[match(halflines$away_team, lookup$sb_team),]$espn_abbr
halflines$home_espn<-lookup[match(halflines$home_team, lookup$sb_team),]$espn_abbr
lines$away_espn<-lookup[match(lines$away_team, lookup$sb_team),]$espn_abbr
lines$home_espn<-lookup[match(lines$home_team, lookup$sb_team),]$espn_abbr
halflines <- halflines[c("away_espn", "home_espn", "line", "game_date")]
lines <- lines[c("away_espn", "home_espn", "line", "spread", "game_date")]

## Join games and half lines data
games$game_date<-substr(games$game_date,0,10)
games$key <- paste(games$team1, games$team2, games$game_date)
halflines$key <- paste(halflines$away_espn, halflines$home_espn, halflines$game_date)
games <- merge(halflines, games)

lines$key <- paste(lines$away_espn, lines$home_espn, lines$game_date)
games <- merge(lines, games, by="key")

halfbox <- merge(games, halfbox)[c("game_id", "game_date.x", "away_espn.x", "home_espn.x", "team", "line.x", "spread", "line.y", "first_downs", "third_downs", "fourth_downs", "total_yards", "passing", "comp_att", 
			           "yards_per_pass", "rushing", "rushing_attempts", "yards_per_rush","penalties","turnovers", "fumbles_lost","ints_thrown", "possession", "score")]
halfbox <- halfbox[order(halfbox$game_id),]
halfbox$tempteam <- ""
halfbox$tempteam[which(halfbox$team == halfbox$away_espn)] <- "team1"
halfbox$tempteam[which(halfbox$team != halfbox$away_espn)] <- "team2"

finalbox <- merge(games, finalbox, by="game_id")
finalbox <- finalbox[,c(-2,-7:-10, -12:-13)]
finalbox <- finalbox[order(finalbox$game_id),]
finalbox$tempteam <- ""
finalbox$tempteam[which(finalbox$team == finalbox$away_espn.x)] <- "team1"
finalbox$tempteam[which(finalbox$team != finalbox$away_espn.x)] <- "team2"


wide<-reshape(halfbox[,c(-2:-5)], direction = "wide", idvar="game_id", timevar="tempteam")
widefinal<-reshape(finalbox[,c(-2:-3)], direction = "wide", idvar="game_id", timevar="tempteam")

colnames(widefinal) <- paste0("final_", colnames(widefinal))
colnames(wide)[1] <- "final_game_id"

all <- merge(wide, widefinal, by="final_game_id")

vals<-apply(all, 2, function(x) grep("\\d+-|:", x))
quote.cols <- which(lapply(vals, length) != 0)
all[,which(lapply(vals, length) != 0)] <- apply(all[,which(lapply(vals, length) != 0)], 2, function(x) paste0('="', x, '"'))

write.csv(all, file="/home/ec2-user/sports2015/NCF/testfile.csv", row.names=FALSE)

sendmailV <- Vectorize( sendmail , vectorize.args = "to" )
#emails <- c( "<tanyacash@gmail.com>" , "<malloyc@yahoo.com>", "<sschopen@gmail.com>")
emails <- c("<tanyacash@gmail.com>")

from <- "<tanyacash@gmail.com>"
subject <- "Weekly NCF Data Report"
body <- c(
  "Chris -- see the attached file.",
  mime_part("/home/ec2-user/sports2015/NCF/testfile.csv", "WeeklyData.csv")
)
sendmailV(from, to=emails, subject, body)



dbDisconnect(con)
