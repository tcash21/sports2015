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
lines <- lines[c("away_espn", "home_espn", "line", "game_date")]

## Join games and half lines data
games$game_date<-substr(games$game_date,0,10)
games$key <- paste(games$team1, games$team2, games$game_date)
halflines$key <- paste(halflines$away_espn, halflines$home_espn, halflines$game_date)
games <- merge(halflines, games)

halfbox <- merge(games, halfbox)[c("game_id", "game_date", "away_espn", "home_espn", "team", "line", "first_downs", "third_downs", "fourth_downs", "total_yards", "passing", "comp_att", 
			           "yards_per_pass", "rushing", "rushing_attempts", "yards_per_rush","penalties","turnovers", "fumbles_lost","ints_thrown", "possession", "score")]
halfbox <- halfbox[order(halfbox$game_id),]
halfbox$tempteam <- ""
halfbox$tempteam[which(halfbox$team == halfbox$away_espn)] <- "team1"
halfbox$tempteam[which(halfbox$team != halfbox$away_espn)] <- "team2"

wide<-reshape(halfbox[,c(-2:-4)], direction = "wide", idvar="game_id", timevar="tempteam")


