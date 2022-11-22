
####################################################

## PREAMBLE - EXPLANATORY LOG - DO NOT RUN FOLLOWING CODE

####################################################

## My query key: e6054421-5208-4713-895f-f918b3403c4a

query_key <- "api_key=e6054421-5208-4713-895f-f918b3403c4a"

## Get league data to start figuring out what TI11's League ID is

dota_leagues_raw <- GET("https://api.opendota.com/api/leagues")
dota_leagues <- fromJSON(rawToChar(dota_leagues_raw$content))

## Examine likely candidate strings to find TI11 League ID

intl_cand1 <- grep("international", dota_leagues$name, value = TRUE, ignore.case = TRUE)
dota_leagues %>%
    filter(name == "The International 2022")
ti11_league_id <- 14268

## Pulling all matches from TI 11 in the "Leagues" dataset on OpenDota API, then
## using this data to form a list of Match IDs so I can pull more extensive
## data from the "Matches" dataset. I'll save this vector locally to minimize
## calls to API in future.

ti_leagues_raw <- GET("https://api.opendota.com/api/leagues/14268/matches",
                      query = query_key)
ti_leagues <- fromJSON(rawToChar(ti_leagues_raw$content))
ti_matchIDs <- ti_leagues$match_id
saveRDS(ti_matchIDs, file = "ti_11_match_ids.RData")
ti_matchID <- readRDS("ti_11_match_ids.RData")

## Now I'll use those match IDs to pull more extensive data from the "Matches"
## dataset on OpenDota API, which I'll save locally.

ti_11_new <- list()

for (i in 1:length(ti_matchID)) {
  ti_data_raw <- GET(paste0("https://api.opendota.com/api/matches/",
                            ti_matchID[i]), query = query_key
                     )
  ti_data <- fromJSON(rawToChar(ti_data_raw$content))
  ti_11_new[[i]] <- list("Match_ID" = ti_data$match_id,
                            "Draft_Timings" = ti_data$draft_timings,
                            "Picks_Bans" = ti_data$picks_bans,
                            "Radiant_Win" = ti_data$radiant_win,
                            "Radiant_Team" = ti_data$radiant_team,
                            "Dire_Team" = ti_data$dire_team,
                            "Time" = ti_data$start_time
                            )
                               }

saveRDS(ti_11_new, "ti_11_match_data.RData")

## The hero names also need to be mapped to their number. Again, I'll save this
## locally to minimize future calls to the API.

heroes_raw <- GET("https://api.opendota.com/api/heroes",
                  query = query_key)
saveRDS(heroes_raw, file = "hero_map.RData")

###################################################

## END PREAMBLE - RUN CODE AFTER THIS

###################################################

## Load relevant libraries

library(jsonlite)
library(httr)
library(plyr)
library(tidyverse)
library(wrapr)
library(lubridate)

######## PHASE ONE: WHICH HEROES WERE PLAYED/BANNED MOST? #############

## First, I'll call up the TI 11 draft data I downloaded from the OpenDota
## API as described in the preamble.

ti_11_match_data <- readRDS("ti_11_match_data.RData")

## Next, I'll call up saved Hero vector to map hero names onto Hero IDs

heroes_raw <- readRDS("hero_map.RData")
heroes <- fromJSON(rawToChar(heroes_raw$content))
hero_name_map <- heroes %>%
  select(id, localized_name)

## Now, I'll map these hero names onto each of the drafts in the draft data
## list, since they're only saved there as numeric IDs

for (i in 1:231) {
  ti_11_match_data[[i]]$Picks_Bans$hero_name <- 
    mapvalues(ti_11_match_data[[i]]$Picks_Bans$hero_id,
              hero_name_map$id, hero_name_map$localized_name
              )
                  }

## Now let's create a data frame with all the data we want to analyze for our first
## few questions. First, we'll add basic draft info.

all_drafts <- as_tibble(ti_11_match_data[[1]]$Picks_Bans)

for (i in 2:231) {
  all_drafts <- rbind(all_drafts, as_tibble(ti_11_match_data[[i]]$Picks_Bans))
}

## Next, we'll add a "matchday" variable.

matchdays_raw <- day(with_tz(as_datetime(ti_11_match_data[[1]]$Time),
                         tz = "Singapore"))
for (i in 2:231) {
  matchdays_raw[i] <- day(with_tz(as_datetime(ti_11_match_data[[i]]$Time),
                              tz = "Singapore"))
}

for (i in 1:231) {
  if (matchdays_raw[i] %in% c(15:18)) {
    matchdays_raw[i] <- matchdays_raw[i] - 14
  } else if (matchdays_raw[i] %in% c(20:23)) {
    matchdays_raw[i] <- matchdays_raw[i] - 15
  } else {
    matchdays_raw[i] <- matchdays_raw[i] - 20
  }
}

matchdays <- c()

for (i in 1:231) {
  matchdays <- c(matchdays, rep(matchdays_raw[i], 24))
}

all_drafts$matchday <- matchdays

## "ord" and "order" are redundant and I'd rather they start at 1 rather
## than 0 to make the variable's value match the pick number.

all_drafts <- subset(all_drafts, select = -ord)

rename(all_drafts, pick = order)

## Finally, it will be important to know which team won each match, so let's
## create a variable for which side corresponds to each pick, as well as which
## side ultimately won each match.

winning_teams_raw <- c()

for (i in 1:231) {
  winning_teams_raw[i] <- ifelse(ti_11_match_data[[i]]$Radiant_Win == TRUE,
                                 yes = "radiant", no = "dire")
}

winning_sides <- c()

for (i in 1:231) {
  winning_sides <- c(winning_teams, rep(winning_teams_raw[i], 24))
}

all_drafts$winning_side <- winning_teams

side_pick <- c()

for (i in 1:231) {
  side_pick <- c(side_pick, ti_11_match_data[[i]]$draft_timings$active_team)
}

rad_dire <- data.frame("dire" = 3, "radiant" = 2)

side_pick_names <- mapvalues(side_pick, rad_dire, names(rad_dire))

all_drafts$pick_side <- side_pick_names

all_drafts$result <- ifelse(all_drafts$pick_side == all_drafts$winning_team,
                            1, 0)

## Okay, I believe we're ready to do some analysis. First, let's just
## see how often each hero was contested overall.

all_drafts %>%
  group_by(hero_name) %>%
  summarize(total_contested = n()) %>%
  arrange(desc(total_contested)) %>%
  slice(1:25) %>%

  ggplot(mapping = aes(x = total_contested,
                       y = reorder(hero_name, total_contested)
                      )
         ) +
    geom_bar(mapping = aes(fill = hero_name),stat = "identity") +
    geom_text(mapping = aes(label = total_contested),
              size = 3, hjust = 2, vjust = 0.35) +
    labs(x = "Total Picks/Bans", y = "Hero") +
    theme(legend.position = "none",
          panel.background = element_rect(fill = "white",
                                          color = "grey75")
          )

## Next, let's differentiate between picks and bans

all_drafts %>%
  group_by(hero_name) %>%
  summarize(total_contested = n(),
            picked = sum(is_pick),
            banned = n() - sum(is_pick)
            ) %>%
  arrange(desc(total_contested)) %>%
  slice(1:25) %>%

  ggplot(mapping = aes(y = reorder(hero_name, total_contested),
                       x = total_contested)
         ) +
    geom_bar(mapping = aes(x = banned), width = 0.42,
             fill = "brown", stat = "identity", position = position_nudge(y = -0.21)
             ) +
    geom_bar(mapping = aes(x = picked), width = 0.42,
             fill = "blue", stat = "identity", position = position_nudge(y = 0.21)
             ) +
    theme(panel.background = element_rect(fill = "white", color = "grey75")) +
    labs(x = "Blue = Pick | Brown = Ban", y = NULL)

## Now, let's look at win rates of the 20 heroes who appeared in the most matches.

all_drafts %>%
  group_by(hero_name) %>%
  summarize(appearances = sum(is_pick),
            win_rate = (sum(result[which(is_pick == 1)])/sum(is_pick))
            ) %>%
  arrange(desc(appearances)) %>%
  slice(1:20) %>%

  ggplot(mapping = aes(x = win_rate, y = reorder(hero_name, appearances))) +
    geom_bar(stat = "identity", mapping = aes(fill = hero_name)) +
    geom_text(mapping = aes(label = appearances,
                            size = 3.65, hjust = 4.3, vjust = 0.35)) +
    geom_text(mapping = aes(label = paste0("(", signif(win_rate, digits = 2), ")"),
                            size = 3.65, hjust = 1.2, vjust = 0.35)
              ) +
    geom_vline(xintercept = 0.5, linetype = "dashed", color = "grey50") +
    theme(legend.position = "none",
          panel.background = element_rect(fill = "white", color = "grey75")
          ) +
    labs(x = "Win Rate", y = NULL)
  
## EXPERIMENTAL ZONE

ti_data_raw <- GET("https://api.opendota.com/api/matches/6815355000",
                   query = query_key)

ti_data <- fromJSON(rawToChar(ti_data_raw$content))

## END EXPERIMENTAL ZONE