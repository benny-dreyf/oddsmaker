# Oddsmaker: A Data Informed Approach Betting American Football

`oddsmaker` is a set of functions that enables the collection, tracking, and analysis of the public betting consensus for individual NFL game spreads and over-unders.

### What is public consensus and how can it be used?

> From the website **oddsshark.com:** "consensus is ... the percentage of the general public betting on each side of a matchup or total. You can bet with or against the public. When you bet against the public, it is called 'fading' the public." 
###### Source: https://www.oddsshark.com/nfl/consensus-picks

The inverse of the popular gambling adage "the house always wins," would probably be something like "the public always loses." While this isn't strictly true (or a good analogy), it prompts the question: can the public consensus picks be used to better inform our own? The purpose of the `oddsmaker` package is to provide trended spread, over-under, and the respective consensus data to help us make better informed picks each week of the NFL season.

### How do we create our data set?

The NFL "Pick Consensus" page on [Oddsshark](http://oddsshark.com) shows a single point-in-time "snapshot" of the share of bets are going for either side for any given game. 

![oddsshark super bowl 2022 consensus](https://github.com/benny-dreyf/oddsmaker/blob/master/super-bowl-2022_oddsshark.png)

This is helpful, but variation over time could add important context, such as:
  - Where did the line open?
  - Has public consensus changed over time? 
  - What is the relationship between % share of bets and the line?

All of these are still observational but more data could help us make better decisions. 

### How does it work?

Each day (or multiple times a day), run both the `odds_consensus` and  `nfl_season_schedule` scripts, providing the required arguments for each, respectively. When done, check to ensure that the dates and times in each table match and then join the data sets. Keep in-mind that the weekly "flex" game will create the need for a manual fix in your final data set if one site updates the date/time and the other does not.



```
picks_con<- oddsmaker::picks_consensus(season = 2022)
picks_con |> glimpse()

sched_2022 <- oddsmaker::nfl_season_schedule(year = 2022)
sched_2022 |> glimpse()

final<- sched_2022 |> 
  mutate(game_time= as_datetime(format(game_time, format="%Y-%m-%d %I:%M:%S"))) |>  
  inner_join(picks_con, by= c('team_abbrev' = 'team', 'home_away', 'game_time')) |> 
  select(-c(10,18), team_full= team, team= team_abbrev, game_num = game_num.x) |> 
  mutate(matchup= str_replace(matchup, pattern = 'VS', replacement = '@')) |> 
  select(date_pulled, everything()) 

final |> glimpse()
```

After you've run the code several times, you'll now have enough data to compile a data set:

```
# create a csv for the most recent join
time<- lubridate::now()
final |> print(n=Inf)
final |> write_csv(glue::glue('season_2022/oddsshark_{time}.csv'))

# create a list of all of the files you've run/created over time
files<- paste('season_2023/', list.files('season_2023'), sep = "")
files

# combine all files into a single master data frame object
master<-map(.x = files, .f= read_csv) |>  
  bind_rows(.id= 'id') |> 
  mutate(team= str_extract(team, '^([A-Z]{3}|[A-Z]{2})')) |> 
  arrange(game_num)

# write that data frame to a master csv
master |> write_csv('master_2022.csv')

glimpse(master)

```

With data compiled in a single dataframe, we can now build a timeseries of our data:

```
master |> 
  oddsmaker::gridiron_all(week_no = 20)
  
```

![oddsshark Divisional Round Spread consensus](https://github.com/benny-dreyf/oddsmaker/blob/master/div-rd_2022-spread.png)

And we can do the same for the over-under:

```
master |> 
  oddsmaker::gridiron_ou(week_no = 20)
  
![oddsshark Divisional Round Spread consensus](https://github.com/benny-dreyf/oddsmaker/blob/master/div-rd_2022-spread.png)
  
```

Or you can use the app, "app.R" script. 
  
  
