library(dplyr)
library(tidyr)
library(wordcloud)
library(RColorBrewer)
library(tm)
source("ct-util.R")

############################################################################
#################### Test for feature 1: World Map #########################
#################### test if a map can be returned #########################

library(countrycode)
library(rnaturalearth)
library(rnaturalearthdata)
library(RColorBrewer)


countries$name[is.na(countries$name)] = "NA"

aggregated_data = countries |>
  collect() |>
  select(-removed) |>
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
  labs(title = "Trials per Country") +
  theme_void()

############################################################################
#################### Test for feature 2: Keyword Search on Conditions, #####
####################  test if histogram works as expected ##################
max_num_studies = 1000

study = studies |> 
  head(max_num_studies) |>
  collect()

condition = conditions |> 
  collect()

num_con = study |>
  left_join(condition, by="nct_id") |>
  select(name)|>
  group_by(name)|>
  summarize(n=n(), .groups = "drop") |>
  arrange(desc(n))|>
  head(10)
ggplot(num_con, aes(x=name, y=n)) +
  geom_col()+
  theme_bw()+
  xlab("Condition")+
  ylab("Count")



######################################################################
####### Test For feature 3: Intervention - Condition Mapping. ################
######################################################################
get_conditions_for_intervention_type <- function(interventionType) {
  # Ensure global variables are accessible
  interventions <- tbl(con, "interventions")
  conditions <- tbl(con, "conditions")
  
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

get_conditions_for_intervention_type("Drug")



######################################################################
#################### Test for feature 4: Search on Interventions, ####
####################  return pie chart of outcomes. ##################
# Function to generate a pie chart based on outcome types for a given intervention
get_outcome_pie_for_intervention <- function(interventionType) {
  
  interventions <- tbl(con, "interventions")
  outcomes <- tbl(con, "outcomes")
  
  selected_interventions <- interventions %>%
    filter(intervention_type == interventionType)      
  # join outcomes and interventions:  
  intervention_outcomes <- selected_interventions %>%
    left_join(outcomes, by = "nct_id")
  
  num_outcomes <- intervention_outcomes |>
    count(outcome_type) |>
    arrange(desc(n))
  
  num_outcomes
  
  # Generate pie chart
  ggplot(num_outcomes, aes(x = "", y = n, fill = outcome_type)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    theme_void() +
    labs(fill = "Outcome Type")
}

get_outcome_pie_for_intervention("Drug")

######################################################################
#################### Test for feature 5: ID count ##################
#################### Count number of IDs per country ################
# Function to number of ID's from selection of individual countries 

count_country_id <- function(country, countries) {
  countries_df <- data.frame(countries)
  filtered_countries <- countries_df %>%
    filter(!removed) %>%
    group_by(name) %>%
    summarise(n = n()) %>% 
    rename(ID_count = n)
  
  # Find the row matching the specified country
  country_row <- filtered_countries %>%
    filter(name == country)
  
  # Print the row (if found)
  if (nrow(country_row) > 0) {
    print(country_row)
  } else {
    cat("Country not found.")
  }
}

count_country_id("United States", countries)

######################################################################
#################### Test for feature 6: Word Cloud ##################
#################### Create Word Cloud of Conditions. ################
# Function to generate a pie chart based on outcome types for a given intervention
num_con = studies |>
  inner_join(conditions, by="nct_id") |>
  collect() # delete later

corpus <- Corpus(VectorSource(num_con$name))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("en"))
corpus <- tm_map(corpus, stripWhitespace)

wordcloud(words = corpus, scale=c(5,0.5), max.words=100, random.order=FALSE, colors=brewer.pal(8, "Dark2"))

##############################################

