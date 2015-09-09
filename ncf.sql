CREATE TABLE "halfBoxScore"(
game_id INT NOT NULL,
team CHAR(5) NOT NULL,
first_downs INT NOT NULL,
third_downs TEXT NOT NULL,
fourth_downs TEXT NOT NULL,
total_yards INT NOT NULL,
passing INT NOT NULL,
comp_att TEXT NOT NULL,
yards_per_pass NUMERIC NOT NULL,
rushing INT NOT NULL,
rushing_attempts INT NOT NULL,
yards_per_rush NUMERIC NOT NULL,
penalties TEXT NOT NULL,
turnovers INT NOT NULL,
fumbles_lost INT NOT NULL,
ints_thrown INT NOT NULL,
possession TEXT NOT NULL,
score INT NOT NULL,
PRIMARY KEY (game_id, team)
);
CREATE TABLE "finalBoxScore"(
game_id INT NOT NULL,
team CHAR(5) NOT NULL,
first_downs INT NOT NULL,
third_downs TEXT NOT NULL,
fourth_downs TEXT NOT NULL,
total_yards INT NOT NULL,
passing INT NOT NULL,
comp_att TEXT NOT NULL,
yards_per_pass NUMERIC NOT NULL,
rushing INT NOT NULL,
rushing_attempts INT NOT NULL,
yards_per_rush NUMERIC NOT NULL,
penalties TEXT NOT NULL,
turnovers INT NOT NULL,
fumbles_lost INT NOT NULL,
ints_thrown INT NOT NULL,
possession TEXT NOT NULL,
score INT NOT NULL,
PRIMARY KEY (game_id, team)
);

CREATE TABLE NCFSBTeamLookup (sb_team CHAR(50) NOT NULL, espn_abbr CHAR(5), espn_name CHAR(30), PRIMARY KEY (sb_team));

CREATE TABLE games(
        game_id INT PRIMARY KEY NOT NULL,
        team1 CHAR(5) NOT NULL,
        team2 CHAR(5) NOT NULL,
        game_date TEXT NOT NULL
);


CREATE TABLE NCFSBLines (  away_team CHAR(40) NOT NULL,
        home_team CHAR(40) NOT NULL,
        line CHAR(10) NOT NULL,
        spread CHAR(10) NOT NULL,
        game_date TEXT NOT NULL,
        game_time TEXT NOT NULL,
        PRIMARY KEY (away_team, home_team, game_date, line, spread)
);
CREATE TABLE NCFSBHalfLines (away_team CHAR(40) NOT NULL,
        home_team CHAR(40) NOT NULL,
        line CHAR(10) NOT NULL,
        spread CHAR(10) NOT NULL,
        game_date TEXT NOT NULL,
        game_time TEXT NOT NULL,
        PRIMARY KEY (away_team, home_team, game_date, line, spread)
);

