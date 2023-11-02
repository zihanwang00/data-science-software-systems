library(dplyr)
library(duckdb)
library(dplyr)
library(DBI)
library(DT)
library(ggplot2)
library(tidyr)
library(purrr)


con = dbConnect(
  duckdb(
    file.path("..", "..", "2023-09-27", "ctgov.duckdb"), 
    read_only = TRUE
  )
)

if (length(dbListTables(con)) == 0) {
  stop("Problem reading from connection.")
}
studies = tbl(con, "studies")
sponsors = tbl(con, "sponsors")
conditions = tbl(con, "conditions")

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

# Get top 10 frequent number of conditions by a brief title keyword search and sponsor type
#' @param d the studies to get the number of conditions trials for.
#' @return A tibble with a `name` column and a `n` of the number of conditions
get_condition_histogram = function(d) {
  num_con = d |>
    inner_join(conditions|> collect(), by="nct_id")|>
    select(name)|>
    group_by(name)|>
    summarize(n=n())|>
    arrange(desc(n))|>
    head(10)|>
    collect()
  
  ggplot(num_con, aes(x=name, y=n))+
    geom_col()+
    theme_bw()+
    xlab("Condition")+
    ylab("Count")

}

plot_concurrent_studies = function(studies) {
  plot(mtcars$mpg, mtcars$cyl)
}
