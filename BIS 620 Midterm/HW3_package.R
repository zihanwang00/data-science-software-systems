#install.packages("devtools")
#install.packages("roxygen2")



#' World Map of Trials
#' Create a map showing the number of trials in each ountry by a brief keyword 
#' search
#'
#' This feature prints out a world map so that the viewer may have an overview 
#' of the number of trials across different countries. 
#'
#' @param infile Path to the input file
#' @param the studies to get the number of studies trials for.
#' @return A map showing the number of trials in each ountry by a brief keyword 
#' search
#' @export
countries$name[is.na(countries$name)] = "NA"

aggregated_data = countries |>
  collect() |>
  select(-removed) |>
  group_by(name) |>
  summarize(n=n()) |>
  ungroup()
aggregated_data$name <- countrycode(aggregated_data$name, 
                                    "country.name","iso3c")

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


#' Condition Keyword Search
#' This feature is accessible through the buttons labeled “Condition Names”. 
# The user search allows us to see the brief titles or types of trials that they
#' are interested in. From there on, one can do visualizations or analyses based
#' on those types of trials. 
#' search
#'
#' This feature prints out a world map so that the viewer may have an overview 
#' of the number of trials across different countries. 
#'
#' @param infile Path to the input file
#' @param input$condition_kw
#' @return A condition keyword search to filter condition plot
#' @export
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


#' Intervention and Condition Mapping
#'
#' This feature is accessible through the buttons labeled “Choose an 
#' Intervention” and “Condition Bar Plot”. This provides users with the ability 
#' to select an intervention type and see a list of conditions that are 
#' associated with studies involving that type of intervention.
#'
#' @param infile Path to the input file
#' @param input$interventionType
#' @return Bar Charts of Top 10 Conditions based on different Intervention
#' @export
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


#' Pie Chart for outcomes
#'
#' In our R shiny app, we have a new tab named, Condition Pie Chart, where it 
#' displays the top 10 conditions based on the intervention chosen. For example 
#' in the situation where “drug” is chosen as the intervention, we see that most
#' results are “health” and then in descending order, the types of outcomes that
#' can usually follow. 
#'
#' @param infile Path to the input file
#' @param input$interventionType
#' @return Pie Charts of Outcome Types based on different Interventions
#' @export
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


#' ID count of individual countries
#'
#' If we reference back to feature #1, we notice that within the world heat map,
#' we can see a tool that allows us to select one of the 223 presented countries
#' that have information for the number of ID’s. After selecting a country, the 
#' viewer can see the following information presented: 
#' Country : “Country name” | Number of ID’s #
#'
#' @param infile Path to the input file
#' @param input$country
#' @param input$countries
#' @return ID count of selected country from dropdown
#' @export
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


#' Word cloud
#'
#' Within the tab, we generated a word cloud of varying sizes. We notice here 
#' that the biggest word is cancer with disease following as second, which is 
#' understandable since a large subject at hand is about the disease, cancer, 
#' and the conditions as well as interventions that may come along with it. The 
#' coloring also helps to categorize the weight of each of the words. We see 
#' that the green words, which are smaller, are often referring to conditions, 
#' interventions, and parts of the body that appear less often, whereas the 
#' red and blue words appear more often in the data. 
#'
#' @param infile Path to the input file
#' @param study
#' @param condition
#' @return Word cloud graph of conditions with varying frequencies
#' @export
num_con = studies |>
  inner_join(conditions, by="nct_id") |>
  collect() # delete later

corpus <- Corpus(VectorSource(num_con$name))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("en"))
corpus <- tm_map(corpus, stripWhitespace)

wordcloud(words = corpus, scale=c(5,0.5), max.words=100, random.order=FALSE, 
          colors=brewer.pal(8, "Dark2"))

