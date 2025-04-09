# The below script attempts to identify best flight pairs by looking
# at hubs (using many different metrics) of the big four airlines which
# dominate the market share in the USA.

#####################
### Download data ###
#####################
# Only need to do once
# Code from https://community.amstat.org/dataexpo/home

# base_url <- "https://blobs.duckdb.org/flight-data-partitioned/"
# files <- paste0("Year=", 2024, "/data_0.parquet") # OG: files <- paste0("Year=", 1987:2024, "/data_0.parquet")
# for (dir in dirname(files)) dir.create(dir, showWarnings = FALSE)
# out <- curl::multi_download(paste0(base_url, files), files, resume = TRUE)

########################
### Read in packages ###
########################

library(arrow) # to read parquet
library(tidyverse)
library(ggridges) # for ridgeline plots

#########################################
### Read in datasets and clean/format ###
#########################################

## Read dataset for JSM project
# Data dictionary here https://www.transtats.bts.gov/Fields.asp?gnoyr_VQ=FGJ
# Column Dest has the destination airport, FAA code
flight_df <- read_parquet("Year=2024/data_0.parquet")

# Stratify the df by only the big four airlines (discussed in 4/3/25 mtg)
big_four <- c("DL","AA","WN","UA") # Delta American Southwest United
flight_df <- flight_df %>%
  filter(Reporting_Airline %in% big_four)

# Make a by-airline version of flight df (helps with computing time)
flight_list <- vector(mode = 'list', length = length(big_four))
names(flight_list) <- big_four
for (airline in big_four){
  flight_list[[airline]] <- flight_df %>%
    filter(Reporting_Airline == airline)
}

## Read in hub dataset
hub_df <- read.csv("data/airport_hub_classification_2019_source_wikipedia.csv")

# Get the FAA codes of hubs for each of the four categories in hub_df.
# Store each tier in object hub_list which can be indexed by hub_types.
hub_types <- hub_df$Role %>%
  unique()

hub_list <- vector(mode = 'list',length = length(hub_types))
names(hub_list) <- hub_types

for (hub in hub_types){
  hub_list[[hub]] <- hub_df %>%
    filter(Role == hub) %>%
    select(FAA) %>%
    unique()
}

########################################
### Merge datasets for visualization ###
########################################

### Visualization 1: For a given Airline and Hub Type
# What do the average departure delays look like?
for (airline in big_four){
  working_flight_df <- flight_list[[airline]]

  for (hub in hub_types){
    working_hubs <- (hub_list[[hub]] %>% as.list())[[1]]
    p_df <- working_flight_df %>%
      filter(Dest %in% working_hubs) %>%
      group_by(Dest) %>%
      summarise(AvgDist = mean(Distance, na.rm = TRUE),
                AvgDelay = mean(DepDelay, na.rm = TRUE))

    title <- paste('Airline',airline,'and Hub Type',hub)

    p <- p_df %>% ggplot(aes(x = AvgDist, y = AvgDelay, group = Dest)) +
      geom_point() +
      stat_summary(aes(label = Dest), # Round the mean to 2 decimal places
        fun = "mean",
        geom = "text",
        vjust = -0.5,  # Adjust vertical position of the label
        size = 4,     # Adjust text size
      ) +
      labs(title = title,
           x = 'Average Flight Distance (miles)',
           y = 'Average Flight Departure Delay (minutes)')

    print(p)
  }
}

# Conclusions: SFO sucks.

### Visualization 2: For each Destination Airport in the "Large" hubs category,
# what is the distribution of arrival times, stratified by both Origin Airport
# and Airline?

large_hubs <- hub_list$`P-L`[[1]]

hub <- large_hubs[1]
airline <- big_four[1]


for (airline in big_four){
  for (hub in large_hubs){
    p_df <- flight_df %>%
      filter(Reporting_Airline == airline & Dest == hub)
    title <- paste("Airline",airline,"and Large Destination Hub",hub)

    p <- p_df %>% ggplot(aes(x = DepDelay, y = Origin, fill = Origin)) +
      geom_density_ridges() +
      theme_ridges() +
      theme(legend.position = "none") +
      labs(title = title,
           x = 'Departure Delay (Minutes; Smoothed)',
           y = 'Origin Airline (Can be any hub class)')

    print(p)
  }
}

# Conclusion: This is way too many plots.

### Visualization 3: For each airline's self-declared hubs (as Destinations),
# what do the arrival distributions look like? Found via Google
#### NOTE I FOUND THIS AND IT MAY BE VERY HELPFUL https://en.wikipedia.org/wiki/Airline_hub

dl_hubs <- c("ATL","BOS","DTW","LAX","MSP","JFK","LGA","SLC","SEA") # Source https://news.delta.com/corporate-stats-and-facts
aa_hubs <- c("CLT","ORD","DFW","LAX","MIA","JFK","LGA","PHL","PHX","DCA") # Source https://www.aa.com/i18n/customer-service/about-us/american-airlines-group.jsp and wikipedia
wn_hubs <- c("ATL", "BWI", "MDW", "DAL", "DEN", "HOU", "LAS", "LAX", "BNA", "OAK", "MCO", "PHX") # source https://en.wikipedia.org/wiki/Southwest_Airlines
# note southwest says they don't do the hub and spoke thing so i used wikipedia
ua_hubs <- c("ORD", "DEN", "GUM", "IAH", "LAX", "EWR", "SFO", "IAD") # Source https://www.united.com/en/us/fly/travel/airport/maps.html

self_declared_hubs <- c(dl_hubs, aa_hubs, wn_hubs, ua_hubs)
matching_airlines <- c(rep("DL",length(dl_hubs)),
                       rep("AA",length(aa_hubs)),
                       rep("WN",length(wn_hubs)),
                       rep("UA",length(ua_hubs)))

for (i in 1:length(self_declared_hubs)){
  hub <- self_declared_hubs[i]
  airline <- matching_airlines[i]

  p_df <- flight_df %>%
    filter(Reporting_Airline == airline & Dest == hub)
  title <- paste("Airline",airline,"and Self-Declared Hub",hub)

  p <- p_df %>% ggplot(aes(x = DepDelay, y = Origin, fill = Origin)) +
    geom_density_ridges() +
    theme_ridges() +
    theme(legend.position = "none") +
    labs(title = title,
         x = 'Departure Delay (Minutes; Smoothed)',
         y = 'Origin Airline (Can be any hub class)')

  print(p)
}

# Conclusion: it's time to take a break.

### Visualization 4: What if we define hub by traffic volume at a particular
# airport? Data source: https://en.wikipedia.org/wiki/Airline_hub

vol_df <- read.csv("data/2022_top_20_hubs_airline_share.csv")
vol_df <- vol_df %>%
  pivot_longer(cols = 'AA':'Not.Specified',
               names_to = 'Airline',
               values_to = 'Passengers')

vol_df %>% ggplot(aes(y = Airport, x = Passengers, fill = Airline)) +
  geom_bar(position = 'stack', stat = 'identity') +
  theme(legend.position = 'none') +
  geom_text(aes(label = Airline), position = position_stack(vjust = .5)) +
  labs(x = 'Millions of Passengers',
       title = 'Top 20 Airports in 2022 by Airline Share')

hubs_found_this_way <- vol_df %>%
  mutate(Share = Passengers/X2022.Millions.of.Departing.Passengers) %>%
  filter(Airline != 'Not.Specified' & Share > 0.5) %>%
  drop_na()

# Yes I did the below manually sue me.
hubs_vis_4 <- c("ATL","DFW","MIA","CLT","EWR","IAH","MSP","DTW")
airlines_vis_4 <- c("DL","AA","AA","AA","UA","UA","DL","DL")

for (i in 1:length(hubs_vis_4)){
  hub <- hubs_vis_4[i]
  airline <- airlines_vis_4[i]

  p_df <- flight_df %>%
    filter(Reporting_Airline == airline & Dest == hub)
  title <- paste("Airline",airline,"and High Traffic Hub",hub)

  p <- p_df %>% ggplot(aes(x = DepDelay, y = Origin, fill = Origin)) +
    geom_density_ridges() +
    theme_ridges() +
    theme(legend.position = "none") +
    labs(title = title,
         x = 'Departure Delay (Minutes; Smoothed)',
         y = 'Origin Airline (Can be any hub class)')

  print(p)
}
