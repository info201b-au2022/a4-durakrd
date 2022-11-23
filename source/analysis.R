library(tidyverse)
library(dplyr)
library(ggplot2)
library(usdata)

# The functions might be useful for A4
source("../source/a4-helpers.R")

## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}

## Section 2  ---- 
#----------------------------------------------------------------------------#
# Your functions and variables might go here ... <todo: update comment>
#----------------------------------------------------------------------------#

## Load data frame ---- 
incarceration_df <- get_data()

# Data frame for Data Summary functions
summary_df <- incarceration_df %>%
  filter(year == max(year)) %>%
  select(year, black_pop_15to64, total_pop_15to64, black_jail_pop, total_jail_pop)

# Function takes no parameter and returns percent of the total population 
# that is black in 2018
total_perc_black <- function() {
  perc_black <- summary_df %>%
    select(year, black_pop_15to64, total_pop_15to64) %>%
    group_by(year) %>%
    summarize(final = (sum(black_pop_15to64, na.rm = TRUE)/sum(total_pop_15to64, na.rm = TRUE))*100) %>%
    pull(final)
  return(paste0(toString(round(perc_black)), "%"))
}

# Function takes no parameter and returns percent of the prison population 
# that is black in 2018
jail_perc_black <- function() {
  perc_black_jail <- summary_df %>%
    select(year, black_jail_pop, total_jail_pop) %>%
    group_by(year) %>%
    summarize(final = (sum(black_jail_pop, na.rm = TRUE)/sum(total_jail_pop, na.rm = TRUE))*100) %>%
    pull(final)
  return(paste0(toString(round(perc_black_jail)), "%"))
}

# Function takes no parameter and returns total jail population in 2018
jail_total_pop <- function() {
  total_pop <- summary_df %>%
    select(year, total_jail_pop) %>%
    group_by(year) %>%
    summarize(final = sum(total_jail_pop, na.rm = TRUE)) %>%
    pull(final)
  comma <- prettyNum(round(total_pop), big.mark = ",")
  return(comma)
}

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Your functions might go here ... <todo:  update comment>
#----------------------------------------------------------------------------#
# This function returns a data frame that can be used for visualizing
# the jail population throughout the years. This function takes no parameters.
get_year_jail_pop <- function() {
  jail_pop_df <- incarceration_df %>%
    select(year, total_jail_pop) %>%
    group_by(year) %>%
    summarize(tot_jail_pop = sum(total_jail_pop, na.rm = TRUE)) %>%
    ungroup()
  return(jail_pop_df)   
}

# This function creates a bar plot of the US total jail populations by year. This
# function takes no parameters.
plot_jail_pop_for_us <- function()  {
  jail_pop_df <- get_year_jail_pop()
  jail_plot <- ggplot(data = jail_pop_df) +
    geom_col(mapping = aes(x = year, y = tot_jail_pop)) +
    labs(
      title = "Increase of Jail Population in U.S. (1970-2018)",
      x = "Year",
      y = "Total Jail Population",
      caption = "Bar plot demonstrates growth in the U.S. jail population from
      1970 to 2018"
    ) +
    scale_y_continuous(labels = scales::comma)
  return(jail_plot)   
} 

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

# Function takes parameter of states and returns data frame for visualization on
# the increase of jail population by year for each state
get_jail_pop_by_states <- function(states) {
  states_df <- incarceration_df %>%
    select(year, state, total_jail_pop) %>%
    filter(state %in% states) %>%
    group_by(year, state) %>%
    summarize(state_tot = sum(total_jail_pop, na.rm = TRUE)) %>%
    ungroup()
  return(states_df)
}

# Function takes parameter of states and returns visualization on
# the increase of jail population by year for each state provided. Will raise
# an error if the state does not exist.
plot_jail_pop_by_states <- function(states) {
  # Checks if correct state names are provided
  states <- sapply(states, states_in_region_or_division)
  # Calls function for states data frame
  states_df <- get_jail_pop_by_states(states)
  # Creates line plot visualization color coded by state
  states_plot <- ggplot(data = states_df) +
    geom_line(mapping = aes(x = year, y = state_tot, color = state)) +
    labs(
      title = "Increase of Jail Population in U.S. States (1970-2018)",
      x = "Year",
      y = "Total Jail Population per State",
      color = "States",
      caption = "Line plot depicts the growth in incarceration numbers for
      several states in the US from 1970 to 2018"
    ) +
    scale_y_continuous(labels = scales::comma)
  return(states_plot)
}

## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

# Function takes no parameters and sets up a data frame for incarcerations rates
# black and white people. Also keeps info on county type.
set_up_stats <- function() {
  recode_names <- c("rural" = "Rural", "small/mid" = "Small/Middle",
                    "suburban" = "Suburban", "urban" = "Urban")
  stats_df <- incarceration_df %>%
    select(year, black_pop_15to64, white_pop_15to64, black_jail_pop,
           white_jail_pop, urbanicity) %>%
    mutate(urbanicity = recode(urbanicity, !!!recode_names)) %>%
    group_by(year, urbanicity) %>%
    summarise(
      black_rate =
        (sum(black_jail_pop, na.rm = TRUE)/sum(black_pop_15to64, na.rm = TRUE)) * 1000,
      white_rate =
        sum(white_jail_pop, na.rm = TRUE)/sum(white_pop_15to64, na.rm = TRUE) * 1000,
    ) %>%
    filter(is.finite(black_rate), urbanicity != "") %>%
    ungroup()
  return(stats_df)
}

# Function takes no parameters and creates line plot visualizations for white
# and black incarceration rates. Function creates 4 subplots that are seperated
# by county type.
plot_trends <- function() {
  stats_df <- set_up_stats()
  stats_plot <- ggplot(data = stats_df, aes(x = year)) +
    geom_line(aes(y = black_rate, color = "Black Rate")) +
    geom_line(aes(y = white_rate, color = "White Rate")) +
    facet_wrap(vars(urbanicity)) +
    labs(
      title = "Incarceration Rates in the U.S. by County Type and Race (1990-2018)",
      x = "Year",
      y = "People Incarcerated per 1,000",
      color = "Race",
      caption = "Visualizations depict drastic differences in incarceratiosn
      rates between white and black people by county type"
    ) +
    scale_x_continuous(limits = c(1990, 2020)) +
    scale_y_continuous(limits = c(0, 16)) +
    scale_color_manual(values = c("Black Rate" = "blue", "White Rate" = "red"))
  return(stats_plot)
}
## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

# Function takes no parameters and returns a shape data frame for US states. The
# data frame has data on the percents of state prison populations that are black.
get_map_stats <- function() {
  map_stats <- incarceration_df %>%
    select(year, state, total_jail_pop, black_jail_pop) %>%
    filter(year == 2018) %>%
    group_by(state) %>%
    summarize(black_perc = (sum(black_jail_pop, na.rm = TRUE)/sum(total_jail_pop, na.rm = TRUE))*100) %>%
    mutate(state = tolower(abbr2state(state))) %>% # Convert abbreviation to state name
    mutate_at(vars(black_perc), ~replace(., is.nan(.), 0)) %>%
    ungroup() %>%
    right_join(map_data("state"), by=c("state" = "region"))
  return(map_stats)
}

# Function takes no parameters and returns map for US states. The choropleth map
# depicts percent of totatl jail populations that are black.
plot_map <- function() {
  map_stats <- get_map_stats()
  trends_map <- ggplot(map_stats) +
    geom_polygon(
      mapping = aes(x = long, y = lat, group = group, fill = black_perc),
      color = "black",
      linewidth = .1
    ) +
    scale_fill_continuous(low = "Beige", high = "Red") +
    labs(
      title = "Percent of Black Prisoners by U.S. State in 2018",
      fill = "Percent",
      caption = "Choropleth map of the 48 contigous US states that corresponds
      to the percent of each states total prison population that is black"
    ) +
    theme_bw() +
    theme(
      axis.line = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      plot.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank()
    ) +
    coord_map()
  return(trends_map)
}

