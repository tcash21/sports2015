library(data.table)
library(RSQLite)
library(sendmailR)
library(plyr)
library(dplyr)

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

halfbox <- merge(games, halfbox)[c("game_id", "game_date.x", "away_espn.x", "home_espn.x", "team", "line.x", "spread", "line.y", "first_downs", 
            "third_downs", "fourth_downs", "total_yards", "passing", "comp_att", "yards_per_pass", "rushing", "rushing_attempts", 
            "yards_per_rush","penalties","turnovers", "fumbles_lost","ints_thrown", "possession", "score")]
halfbox <- halfbox[order(halfbox$game_id),]
halfbox$tempteam <- ""
halfbox$tempteam[which(halfbox$team == halfbox$away_espn)] <- "team1"
halfbox$tempteam[which(halfbox$team != halfbox$away_espn)] <- "team2"

## calculate running first half season averages and merge with all data
halfbox$game_date.x<-as.Date(halfbox$game_date.x, format='%m/%d/%Y')
halfbox<-halfbox[order(halfbox$game_date),]

## Split out the meaures with '-' in them into 2 numeric variables
halfbox<-cbind(halfbox, do.call('rbind', strsplit(halfbox$third_downs, "-")))
colnames(halfbox)[26:27] <- c("third_downs", "third_down_att")
halfbox<-cbind(halfbox, do.call('rbind', strsplit(halfbox$fourth_downs, "-")))
colnames(halfbox)[28:29] <- c("fourth_downs", "fourth_down_att")
halfbox<-cbind(halfbox, do.call('rbind', strsplit(halfbox$penalties, "-")))
colnames(halfbox)[30:31] <- c("penalties", "penalty_yards")
halfbox[,26:31] <- apply(halfbox[,26:31], 2, as.numeric)
halfbox <- halfbox[,c(-10,-11,-14,-19)]

## Split out the meaures with '-' in them into 2 numeric variables
finalbox<-cbind(finalbox, do.call('rbind', strsplit(finalbox$third_downs, "-")))
colnames(finalbox)[19:20] <- c("third_downs", "third_down_att")
finalbox<-cbind(finalbox, do.call('rbind', strsplit(finalbox$fourth_downs, "-")))
colnames(finalbox)[21:22] <- c("fourth_downs", "fourth_down_att")
finalbox<-cbind(finalbox, do.call('rbind', strsplit(finalbox$penalties, "-")))
colnames(finalbox)[23:24] <- c("penalties", "penalty_yards")
finalbox[,19:24] <- apply(finalbox[,19:24], 2, as.numeric)
finalbox <- finalbox[,c(-4, -5, -8, -13)]

## Calculate 2nd half stats using halfbox and finalbox
both.halves <- merge(halfbox, finalbox, by=c("game_id", "team"))
both.halves$third_downs_2H <- both.halves$third_downs.y - both.halves$third_downs.x
both.halves$third_down_att_2H <- both.halves$third_down_att.y - both.halves$third_down_att.x
both.halves$fourth_downs_2H <- both.halves$fourth_downs.y - both.halves$fourth_downs.x
both.halves$fourth_down_att_2H <- both.halves$fourth_down_att.y - both.halves$fourth_down_att.x
both.halves$penalties_2H <- both.halves$penalties.y - both.halves$penalties.x
both.halves$penalty_yards_2H <- both.halves$penalty_yards.y - both.halves$penalty_yards.x
both.halves$total_yards_2H <- both.halves$total_yards.y - both.halves$total_yards.x
both.halves$pass_yards_2H <- both.halves$passing.y - both.halves$passing.x
both.halves$turnovers_2H <- both.halves$turnovers.y - both.halves$turnovers.x


## Calculate first half season totals and averages
halfbox<-data.frame(halfbox %>% group_by(team) %>% mutate(count = sequence(n())))
dt <- data.table(halfbox)
dt <- dt[, season_1H_third_down_total:=cumsum(third_downs), by = "team"]
dt <- dt[, season_1H_third_down_att_total:=cumsum(third_down_att), by = "team"]
dt <- dt[, season_1H_third_down_conv:=season_1H_third_down_total /season_1H_third_down_att_total, by = "team"]
dt <- dt[, season_1H_fourth_down_total:=cumsum(fourth_downs), by = "team"]
dt <- dt[, season_1H_yards_total:=cumsum(total_yards), by = "team"]
dt <- dt[, season_1H_pass_yards_total:=cumsum(passing), by = "team"]
dt <- dt[, season_1H_fourth_down_avg:=season_1H_fourth_down_total / count, by = "team"]
dt <- dt[, season_1H_yards_avg:=season_1H_yards_total / count, by = "team"]
dt <- dt[, season_1H_pass_yards_avg:=season_1H_pass_yards_total / count, by = "team"]
dt <- dt[, season_1H_penalty_yards_total:=cumsum(penalty_yards), by = "team"]
dt <- dt[, season_1H_penalty_yards_avg:=season_1H_penalty_yards_total / count, by = "team"]
dt <- dt[, season_1H_turnovers_total:=cumsum(turnovers), by = "team"]
dt <- dt[, season_1H_turnovers_avg:=season_1H_turnovers_total / count, by = "team"]

## Lag every season variable by 1 game so stats are going into the game
nm1<-grep("season_1H", colnames(dt), value=TRUE)
nm2 <- paste("lag", nm1, sep=".")
dt[, (nm2):=lapply(.SD, function(x) c(NA, x[-.N])), by=team, .SDcols=nm1]

halfbox <- data.frame(dt)
halfbox <- halfbox[,-grep("^season", colnames(halfbox))]

secondhalf.box <- both.halves[,c("game_id", "game_date.x", "team", "third_downs_2H", "third_down_att_2H", "fourth_downs_2H", "fourth_down_att_2H", "penalties_2H", 
                  "penalty_yards_2H", "total_yards_2H", "pass_yards_2H", "turnovers_2H", "score.y")]


## calculate running second half season averages and merge with all data
secondhalf.box$game_date.x<-as.Date(secondhalf.box$game_date.x, format='%m/%d/%Y')
secondhalf.box<-secondhalf.box[order(secondhalf.box$game_date),]

secondhalf.box<-data.frame(secondhalf.box %>% group_by(team) %>% mutate(count = sequence(n())))
dt <- data.table(secondhalf.box)
dt <- dt[, season_2H_third_down_total:=cumsum(third_downs_2H), by = "team"]
dt <- dt[, season_2H_third_down_att_total:=cumsum(third_down_att_2H), by = "team"]
dt <- dt[, season_2H_third_down_conv:=season_2H_third_down_total /season_2H_third_down_att_total, by = "team"]
dt <- dt[, season_2H_fourth_down_total:=cumsum(fourth_downs_2H), by = "team"]
dt <- dt[, season_2H_yards_total:=cumsum(total_yards_2H), by = "team"]
dt <- dt[, season_2H_pass_yards_total:=cumsum(pass_yards_2H), by = "team"]
dt <- dt[, season_2H_fourth_down_avg:=season_2H_fourth_down_total / count, by = "team"]
dt <- dt[, season_2H_yards_avg:=season_2H_yards_total / count, by = "team"]
dt <- dt[, season_2H_pass_yards_avg:=season_2H_pass_yards_total / count, by = "team"]
dt <- dt[, season_2H_penalty_yards_total:=cumsum(penalty_yards_2H), by = "team"]
dt <- dt[, season_2H_penalty_yards_avg:=season_2H_penalty_yards_total / count, by = "team"]
dt <- dt[, season_2H_turnovers_total:=cumsum(turnovers_2H), by = "team"]
dt <- dt[, season_2H_turnovers_avg:=season_2H_turnovers_total / count, by = "team"]

## Lag every season variable by 1 game so stats are going into the game
nm1<-grep("season_2H", colnames(dt), value=TRUE)
nm2 <- paste("lag", nm1, sep=".")
dt[, (nm2):=lapply(.SD, function(x) c(NA, x[-.N])), by=team, .SDcols=nm1]

secondhalf.box <- data.frame(dt)
secondhalf.box <- secondhalf.box[,-grep("^season", colnames(secondhalf.box))]


secondhalf.box <- merge(games, secondhalf.box, by="game_id")
secondhalf.box <- secondhalf.box[,c(-2,-8:-10, -11:-14)]
secondhalf.box <- secondhalf.box[order(secondhalf.box$game_id),]
secondhalf.box$tempteam <- ""
secondhalf.box$tempteam[which(secondhalf.box$team == secondhalf.box$away_espn.x)] <- "team1"
secondhalf.box$tempteam[which(secondhalf.box$team != secondhalf.box$away_espn.x)] <- "team2"


wide<-reshape(halfbox[,c(-2:-5)], direction = "wide", idvar="game_id", timevar="tempteam")
widefinal<-reshape(secondhalf.box[,c(-2:-3)], direction = "wide", idvar="game_id", timevar="tempteam")
#colnames(widefinal) <- paste0("final_", colnames(widefinal))


wide <- subset(wide, select = -c(line.x.team2, spread.team2, line.y.team2 ))
widefinal<-subset(widefinal, select=-c(line.x.team2, spread.team2, line.x.team1, spread.team1))

# widefinal<-cbind(widefinal, do.call('rbind', strsplit(widefinal$final_third_downs.team1, "-")))
# widefinal<-cbind(widefinal, do.call('rbind', strsplit(widefinal$final_fourth_downs.team1, "-")))
# widefinal<-cbind(widefinal, do.call('rbind', strsplit(widefinal$final_comp_att.team1, "-")))
# widefinal<-cbind(widefinal, do.call('rbind', strsplit(widefinal$final_penalties.team1, "-")))
# widefinal<-cbind(widefinal, do.call('rbind', strsplit(widefinal$final_third_downs.team2, "-")))
# widefinal<-cbind(widefinal, do.call('rbind', strsplit(widefinal$final_fourth_downs.team2, "-")))
# widefinal<-cbind(widefinal, do.call('rbind', strsplit(widefinal$final_comp_att.team2, "-")))
# widefinal<-cbind(widefinal, do.call('rbind', strsplit(widefinal$final_penalties.team2, "-")))

# colnames(widefinal)[38:53] <- c("final_third_downs.team1", "final_third_down_att.team1", "final_fourth_downs.team1", "final_fourth_down_att.team1", 
#                 "final_comp.team1","final_comp_att.team1","final_penalties.team1", "final_penalty_yards.team1", "final_third_downs.team2", 
#                 "final_third_downs_att.team2", "final_fourth_downs.team2","final_fourth_down_att.team2", "final_comp.team2", 
#                 "final_comp_att.team2", "final_penalties.team2", "final_penalty_yards.team2")
# widefinal <- widefinal[,c(-5:-6, -9,-14, -23:-24,-27,-32)]
# widefinal[,30:45]<-apply(widefinal[,30:45], 2, as.numeric)


all <- merge(wide, widefinal, by="game_id")
colnames(all) <- gsub("lag\\.", "", colnames(all))



# passing$game_date<-as.Date(passing$the_date, format='%m/%d/%Y')
# passing$key <- paste(passing$game_date, passing$team)
# i<-grep("\\s{1}0",passing$the_date)
# passing$game_date[i] <- passing[i,]$game_date - 1
# passing$game_date<-format(passing$game_date, "%m/%d/%Y")
# passing$key <- paste(passing$game_date, passing$team)
# pass_stats<-passing[match(all$key, passing$key),c(3:8)]
# colnames(pass_stats) <- paste0("season_pass_team1_", colnames(pass_stats))
# all <- cbind(all, pass_stats)

# all$key<-paste(all$final_game_date.x.team2, all$final_team.team2)
# pass_stats<-passing[match(all$key, passing$key),c(3:8)]
# colnames(pass_stats) <- paste0("season_pass_team2_", colnames(pass_stats))
# all <- cbind(all, pass_stats)

# ## Merge in season stats rushing
# rushing$game_date<-substr(rushing[,2], 0,10)
# all$key<-paste(all$final_game_date.x.team1, all$final_team.team1)
# rushing$game_date<-as.Date(rushing$the_date, format='%m/%d/%Y')
# rushing$key <- paste(rushing$game_date, rushing$team)
# i<-grep("\\s{1}0",rushing$the_date)
# rushing$game_date[i] <- rushing[i,]$game_date - 1
# rushing$game_date<-format(rushing$game_date, "%m/%d/%Y")
# rushing$key <- paste(rushing$game_date, rushing$team)
# rush_stats<-rushing[match(all$key, rushing$key),c(3:7)]
# colnames(rush_stats) <- paste0("season_rush_team1_", colnames(rush_stats))
# all <- cbind(all, rush_stats)

# all$key<-paste(all$final_game_date.x.team2, all$final_team.team2)
# rush_stats<-rushing[match(all$key, rushing$key),c(3:7)]
# colnames(rush_stats) <- paste0("season_rush_team2_", colnames(rush_stats))
# all <- cbind(all, rush_stats)


# ## Merge in receiving season data
# receiving$game_date<-substr(receiving[,2], 0,10)
# all$key<-paste(all$final_game_date.x.team1, all$final_team.team1)
# receiving$game_date<-as.Date(receiving$the_date, format='%m/%d/%Y')
# receiving$key <- paste(receiving$game_date, receiving$team)
# i<-grep("\\s{1}0",receiving$the_date)
# receiving$game_date[i] <- receiving[i,]$game_date - 1
# receiving$game_date<-format(receiving$game_date, "%m/%d/%Y")
# receiving$key <- paste(receiving$game_date, receiving$team)
# rec_stats<-receiving[match(all$key, receiving$key),c(3:7)]
# colnames(rec_stats) <- paste0("season_rec_team1_", colnames(rec_stats))
# all <- cbind(all, rec_stats)

# all$key<-paste(all$final_game_date.x.team2, all$final_team.team2)
# rec_stats<-rushing[match(all$key, receiving$key),c(3:7)]
# colnames(rec_stats) <- paste0("season_rec_team2_", colnames(rec_stats))
# all <- cbind(all, rec_stats)


#vals<-apply(all, 2, function(x) grep("\\d+-|:", x))
#quote.cols <- which(lapply(vals, length) != 0)
#all[,which(lapply(vals, length) != 0)] <- apply(all[,which(lapply(vals, length) != 0)], 2, function(x) paste0('="', x, '"'))

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


