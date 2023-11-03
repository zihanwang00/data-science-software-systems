library(dplyr)
library(tidyr)

source("ct-util.R")



# test for Feature 1
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

# Test For Feature 2
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

