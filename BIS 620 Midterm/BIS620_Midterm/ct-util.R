library(dplyr)
library(duckdb)
library(dplyr)
library(DBI)
library(DT)
library(ggplot2)
library(tidyr)
library(purrr)
library(countrycode)
library(rnaturalearth)
library(rnaturalearthdata)
library(RColorBrewer)
library(wordcloud)
library(RColorBrewer)
library(wordcloud2)
library(tm)



con = dbConnect(
  duckdb(
    file.path("..", "..", "2023-09-27", "ctgov1.duckdb"), 
    read_only = TRUE
  )
)

if (length(dbListTables(con)) == 0) {
  stop("Problem reading from connection.")
}
studies = tbl(con, "studies")
sponsors = tbl(con, "sponsors")
conditions = tbl(con, "conditions")
countries = tbl(con, "countries")
outcomes = tbl(con, "outcomes")
interventions = tbl(con, "interventions")
interventions_local=interventions |>
  collect()

##### Feature 6: Filter out details on individual countries
countries_df <- data.frame(countries)
filtered_countries <- countries_df %>%
  filter(!removed) %>%
  group_by(name) %>%
  summarise(n =n()) %>% 
  rename(ID_count = n)
names_countries <- filtered_countries$name

# Q1: get unique phase_levels
phase_levels <- studies |>
  select(phase) |>
  mutate(phase = ifelse(is.na(phase), 'NA', phase)) |>
  distinct(phase) |>
  collect()

#' @title Query keywords from a database table.
#' @description Description goes here.
#' @param d the database table.
#' @param kwds the keywords to look for.
#' @param column the column to look for the keywords in.
#' @param ignore_case should the case be ignored when searching for a keyword?
#' (default TRUE)
#' @param match_all should we look for values that match all of the keywords 
#' (intersection) or any of the keywords (union)? (default FALSE; union).

query_kwds <- function(d, kwds, column, ignore_case = TRUE, match_all = FALSE) {
  kwds = kwds[kwds != ""]
  kwds = paste0("%", kwds, "%") |>
    gsub("'", "''", x = _)
  if (ignore_case) {
    like <- " ilike "
  } else{
    like <- " like "
  }
  query = paste(
    paste0(column, like, "'", kwds, "'"),
    collapse = ifelse(match_all, " AND ", " OR ")
  )
  filter(d, sql(query)) 
}

# Create a histogram of the phases returned by a brief title keyword search and sponsor type
#' @param d the database table.
#' @param brief_title_kw the brief title keywords to look for. This is optional.
plot_phase_histogram = function(x) {
  
  x$phase[is.na(x$phase)] = "NA"
  
  x_summarized = x |>
    select(phase) |>
    group_by(phase) |>
    summarize(n = n(), .groups = "drop")
  
  x_complete = left_join(phase_levels, x_summarized, by = "phase")
  x_complete$n[is.na(x_complete$n)] = 0
  
  ggplot(x_complete, aes(x = phase, y = n)) +
    geom_col() +
    theme_bw() +
    xlab("Phase") +
    ylab("Count")
}

#' Get the number of concurrent trials for each date in a set of studies
#' @param d the studies to get the number of concurrent trials for.
#' @return A tibble with a `date` column and a `count` of the number of
#' concurrent trials at that date.
get_concurrent_trials = function(d) {
  
  # Get all of the unique dates.
  all_dates = d |> 
    pivot_longer(cols = everything()) |> # make start_date, completion_date in one column
    select(-name) |>
    distinct() |> # distinct value (date)
    arrange(value) |>
    na.omit() |> 
    rename(date = value)
  # find the unique dates, see if trials are active on those dates
  within_date = function(date, starts, ends) {
    date >= starts & date <= ends
  }
  
  # Get the number of concurrent trials at each of the unique dates.
  all_dates$count = 
    map_dbl(
      
      all_dates$date, 
      ~ .x |> 
        within_date(d$start_date, d$completion_date) |>
        sum(na.rm = TRUE)
    )
  return(all_dates)
}

# Get top 8 frequent number of conditions by a brief title keyword search and sponsor type
#' @param d the studies to get the number of studies trials for.

get_condition_histogram = function(study, condition) {
  
  num_con = study |>
    inner_join(condition, by="nct_id") |>
    select(name)|>
    group_by(name)|>
    summarize(n=n(), .groups = "drop") |>
    arrange(desc(n))|>
    head(8)
  
  return(num_con)
}

# Feature 1: Map of trials
# Create a map showing the number of trials in each country by a brief title keyword search
#' @param d the studies to get the number of studies trials for.

plot_country_map <- function(d){
  countries$name[is.na(countries$name)] = "NA"
  
  aggregated_data = d |>
    inner_join(countries|> collect(), by="nct_id") |>
    group_by(name) |>
    summarize(n=n()) |>
    ungroup()
  aggregated_data$name <- countrycode(aggregated_data$name, "country.name", "iso3c")
  
  world <- ne_countries(scale = "medium", returnclass = "sf")
  map_data <- world |>
    left_join(aggregated_data, by = c("iso_a3" = "name"))
  
  ggplot(map_data) +
    geom_sf(aes(fill = n), color = "white", size = 0.2) +
    scale_fill_continuous(
      low = "lightblue", high = "darkblue",
      na.value = "grey50",  # Color for countries with no data
      name = "Number of Trials"
    ) +
    labs(title = "Number of Clinical Trials per Country") +
    theme_void()
}


plot_concurrent_studies = function(studies) {
  plot(mtcars$mpg, mtcars$cyl)
}


###############################################################
########### #Feature 4 : Search on Interventions.##############
###############################################################

get_outcome_pie_for_intervention <- function(interventionType) {
  
  
  num_outcomes <- interventions |>
    filter(intervention_type %in% interventionType) |>
    left_join(outcomes, by = "nct_id") |>
    count(outcome_type) |>
    arrange(desc(n))
  
  # Generate pie chart
  ggplot(num_outcomes, aes(x = "", y = n, fill = outcome_type)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    theme_void() +
    labs(fill = "Outcome Type")
}


##################################################################
####### Feature 3 : Interventions on Condition Mapping ############
##################################################################
get_conditions_for_intervention_type <- function(interventionType) {
  
  intervention_studies <- interventions |>
    filter(intervention_type == interventionType) |>
    select(nct_id) |>
    distinct()
  
  conditions_for_intervention <- conditions |>
    inner_join(intervention_studies, by = "nct_id") |>
    select(name) |>
    count(name, sort = TRUE) |>
    collect()
  
  top_conditions <- conditions_for_intervention %>%
    slice_max(n, n = 10)
  top_conditions
  
  ggplot(top_conditions, aes(x = reorder(name, n), y = n)) +
    geom_bar(stat = "identity", fill = 'steelblue') +
    coord_flip() +  
    labs(y = "Condition", x = "Count", 
         title = paste("Top 10 Conditions for", interventionType)) +
    theme_minimal() +
    theme(axis.text.y = element_text(angle = 0))
  
}
#################################################################

##################################################################
####### Feature 5: Word Cloud of Conditions ######################
##################################################################
word_cloud <- function(study, condition){
  num_con = study |>
    inner_join(condition, by="nct_id")
  
  corpus <- Corpus(VectorSource(num_con$name))
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, stopwords("en"))
  corpus <- tm_map(corpus, stripWhitespace)
  
  wordcloud(words = corpus, scale=c(5,0.5), max.words=100, random.order=FALSE, colors=brewer.pal(8, "Dark2"))
}
#################################################################
