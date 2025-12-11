library(tidyverse)
library(ggplot2)
library(forcats)
library(opendatatoronto)
library(dplyr)
library(janitor)

# get package
package <- show_package("153ea449-b7f4-4c4d-889a-ec0f89b3bbc9")
package

# get all resources for this package
resources <- list_package_resources("153ea449-b7f4-4c4d-889a-ec0f89b3bbc9")

# identify datastore resources; by default, Toronto Open Data sets datastore resource format to CSV for non-geospatial and GeoJSON for geospatial resources
datastore_resources <- filter(resources, tolower(format) %in% c('csv', 'geojson'))

# create dataframes
df_building <- filter(datastore_resources, row_number()==1) %>% get_resource() |> clean_names()

df_units <- filter(datastore_resources, row_number()==2) %>% get_resource() |> clean_names()

df_units$units_available_in_the_last_12_months <- as.numeric(df_units$units_available_in_the_last_12_months)

df_units$number_of_market_rent_units <- as.numeric(df_units$number_of_market_rent_units)

df_units$number_of_subsidized_units <- as.numeric(df_units$number_of_subsidized_units)

# types of housing available in Parkdale
df_building <- df_building |> filter(ward==4)

# exploring these buildings
df_building |> 
  ggplot(aes(x=forcats::fct_infreq(provider_type))) +
  geom_bar() +
  labs(
    title="TCHC is the most common provider in PHP",
    x="Provider type",
    y="Building count"
  )

df_building |>
  filter(provider_type=="PNP") |>
  ggplot(aes(x=forcats::fct_infreq(provider_name))) +
  geom_bar() +
  coord_flip() +
  labs(
    title="Most PNP providers run one building",
    x="Provider name",
    y="Building count"
  )

df_building |>
  filter(provider_type=="CO-OP") |>
  ggplot(aes(x=forcats::fct_infreq(provider_name))) +
  geom_bar() +
  coord_flip() +
  labs(
    title="Only two CO-OP providers are active in PHP",
    x="Provider name",
    y="Building count"
  )

# prep units overall in PHP
php_buildings <- df_building |> select(building_complex_name) |> pull() # pull for vector
df_units <- df_units |> filter(building_complex_name %in% php_buildings)

# TODO: Fix scales of geom_col() and fix order
# Fix 1: Needed to convert values to numeric

# market housing compared to subsidized housing counts
# TODO: Create into a pie chart? Polar coord bar chart
df_units |> 
  select(number_of_market_rent_units, number_of_subsidized_units) |>
  summarize(
    number_of_market_rent_units = sum(number_of_market_rent_units),
    number_of_subsidized_units = sum(number_of_subsidized_units)
  )

# market units by unit size
df_units |> 
  select(unit_size, number_of_market_rent_units) |>
  group_by(unit_size) |>
  summarize(
    number_of_market_rent_units = sum(number_of_market_rent_units)
  ) |>
  ggplot(aes(x=unit_size, y=number_of_market_rent_units)) +
  geom_col(just=1) +
  coord_flip() + 
  labs(
    title="1 and 2-bedroom units provide the most market housing",
    x="Unit size",
    y="Number of subsidized rental units"
  )

# subsidized units by unit size
df_units |> 
  select(unit_size, number_of_subsidized_units) |>
  group_by(unit_size) |>
  summarize(
    number_of_subsidized_units = sum(number_of_subsidized_units)
  ) |>
  ggplot(aes(x=unit_size, y=number_of_subsidized_units)) +
  geom_col(just=1) +
  coord_flip() + 
  labs(
    title="1 bedroom units provide by far the most subsidized housing",
    x="Unit size",
    y="Number of subsidized rental units"
  )

# Overall stats
df_units_overall <- df_units |>
  select(number_of_market_rent_units, number_of_subsidized_units) |>
  pivot_longer(
    cols = starts_with("number_of"),
    names_to = "housing_type",
    values_to = "housing_capacity"
  )

df_units_overall |>
  group_by(housing_type) |>
  summarise(housing_capacity = sum(housing_capacity)) |>
  ggplot(aes(x="", y=housing_capacity, fill=housing_type)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  labs(
    title="Over 80% of units are subsidized",
    x = "",
    y = "Housing capacity",
    fill = "Housing type"
  ) +
  geom_text(aes(label=housing_capacity), position = position_stack(vjust = 0.5)) +
  theme_void()

# Housing distribution for TCHC
# prep units overall in PHP

housing_dist <- function(provider) {
  provider_buildings <- df_building |> 
    filter(provider_type==provider) |> 
    select(building_complex_name) |>  
    pull()
  
  df_prov_units <- df_units |> 
    filter(building_complex_name %in% provider_buildings) |>
    pivot_longer(
      cols = starts_with("number_of"),
      names_to = "housing_type",
      values_to = "housing_capacity"
    )
  
  df_prov_units |>
    group_by(housing_type) |>
    summarise(housing_capacity = sum(housing_capacity)) |>
    ggplot(aes(x="", y=housing_capacity, fill=housing_type)) +
    geom_bar(stat="identity", width=1, color="white") +
    coord_polar("y", start=0) +
    labs(
      title=sprintf("%s housing units", provider),
      x = "",
      y = "Housing capacity",
      fill = "Housing type"
    ) +
    geom_text(aes(label=housing_capacity), position = position_stack(vjust = 0.5)) +
    theme_void()
}

housing_dist("TCHC")
housing_dist("PNP")
housing_dist("CO-OP")


































