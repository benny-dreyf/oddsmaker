# Oddsmaker: Using Data to Inform Your Weekly Picks

This is a tool to use public betting consensus to find the strongest opportunities to "fade the public."

### What is public consensus and how can it be used?

> From **Oddsshark.com:** "The NFL consensus is ... the percentage of the general public betting on each side of a matchup or total. You can bet with or against the public. When you bet against the public, it is called 'fading' the public." 

"the house always wins" always applies in sports betting because of the vigorish, however, if we assume that the majority of the betting public is generally wrong, what can we use to better inform our picks?
  * Changes in the spread over-time
  * Changes in the public pick consensus over-time

###### Source: https://www.oddsshark.com/nfl/consensus-picks

The NFL "Pick Consensus" page on oddsshark.com shows a single point-in-time 'snapshot' of where highest share of bets are going for any given game. 


![oddsshark super bowl 2022 consensus](https://github.com/benny-dreyf/oddsmaker/blob/master/super-bowl-2022_oddsshark.png)


This is helpful, but variation over time could add important context, such as:
  - Where did the line open?
  - How has public consensus changed over time? 
  - What is the relationship between % share of bets and the line?

All of these are still observational but more data coudl help us make better decisions. 

### How does it work?

Each day (or multiple times a day), you first need to run the `oddsmaker` script, detailing the 'number of games (`num_games`) and 'week number' (`week_num`) on the NFL calendar. 

```
oddsshark<- oddsmaker::odds_consensus(week_num = 22, num_games = 1) 
oddsshark %>% print()
time<- lubridate::now()
week<- '22'
oddsshark %>% 
  readr::write_csv(path= glue::glue('/Users/BenDreyfuss/oddsmaker/playoffs_2022/week_{week}/oddsshark_{time}.csv'))

```

After you've run the code several times, you'll now have enough data to compile a data set:

```
# create a list of all of the files you've run/created over time
files<- paste(glue::glue('playoffs_2022/week_{week}/'), list.files(glue::glue('playoffs_2022/week_{week}')), sep= '')
files

# use map to compile everything into a single dataframe
week_22<- map(.x = files, .f= read_csv) %>% 
  map(mutate_all, as.character) %>% 
  bind_rows(.id= 'id') %>% 
  mutate(date_pulled= lubridate::as_date(lubridate::parse_date_time(date_pulled, orders = c('%m/%d/%y', '%y-%m-%d'))),
         team= str_extract(team, '^([A-Z]{3}|[A-Z]{2})')) %>% 
  mutate_at(vars(4,7:9, 11:13), as.numeric) %>% 
  select(date_pulled, week, game_num, everything())

glimpse(week_22)

```

With data compiled in a single dataframe, we can now build a timeseries of our data:

```
oddsmaker::gridiron_all(week_22)
```

![oddsshark super bowl 2022 consensus](https://github.com/benny-dreyf/oddsmaker/blob/master/super-bowl_2022.png)

And we can do the same for the over-under:

```
oddsmaker::gridiron_ou(week_22)
```

![oddsshark super bowl 2022 consensus](https://github.com/benny-dreyf/oddsmaker/blob/master/super-bowl_2022_ou.png)
