library(ggdag)
library(fixest)
library(lme4)

write.csv(all_data, "rent_model_all_data.csv")

rent_diagram_full <- dagify(
  rent_price ~ supply + demand + cost,
  supply ~ vacancy + rental_supply + location,
  vacancy ~ location,
  rental_supply ~ location,
  demand ~ population + avg_hourly_wage + location,
  population ~ location,
  avg_hourly_wage ~ location,
  cost ~ mortgage + electricity + water + location,
  mortgage ~ location,
  electricity ~ location,
  water ~ location,
  outcome = "rent_price",
  labels = c(
    rent_price = "rent price",
    supply = "supply",
    demand = "demand",
    cost = "cost",
    vacancy = "vacancy",
    rental_supply = "rental supply",
    population = "population",
    avg_hourly_wage = "average_wage",
    mortgage = "mortgage",
    water = "water",
    electricity = "electricity",
    location = "location"
  ),
  coords = list(
    x = c(rent_price = 4, supply = 3, demand = 3, cost = 3, vacancy = 2, 
          rental_supply = 2, population = 2, avg_hourly_wage = 2, mortgage = 2, 
          water = 2, electricity = 2, location = 1),
    y = c(rent_price = 4, supply = 1, demand = 4, cost = 7, vacancy = 1,
          rental_supply = 2, population = 3, avg_hourly_wage = 5, mortgage = 6, 
          water = 7, electricity = 8, location = 4)
  )
)

ggdag_status(
  rent_diagram_full, 
  use_labels = "label", 
  text = FALSE,
) + 
  theme_dag()

rent_diagram_simple <- dagify(
  rent_price ~ supply + demand + cost,
  supply ~ vacancy + location,
  vacancy ~ location,
  demand ~ population + location,
  population ~ location,
  cost ~ mortgage + location,
  mortgage ~ location,
  outcome = "rent_price",
  labels = c(
    rent_price = "rent price",
    supply = "supply",
    demand = "demand",
    cost = "cost",
    vacancy = "vacancy",
    location = "location",
    population = "population",
    mortgage = "mortgage"
  ),
  coords = list(
    x = c(rent_price = 4, supply = 3, demand = 3, cost = 3, vacancy = 2,
          location = 1, population = 2, mortgage = 2),
    y = c(rent_price = 4, supply = 1, demand = 4, cost = 7, vacancy = 1,
          location = 4, population = 3.5, mortgage = 7)
  )
)

ggdag_status(
  rent_diagram_simple, 
  use_labels = "label", 
  text = FALSE,
) + 
  theme_dag()

casual_data_simple <- all_data |>
  filter(
    room_type == "all",
    year < 2023
  ) |>
  select(
    year,
    geography,
    avg_rent_cpi_adj,
    mortgage_interest_paid_cpi_pop_adj,
    population,
    vacancy_rate
  )

casual_model_simple <- feols(
  log(avg_rent_cpi_adj) ~  
    log(mortgage_interest_paid_cpi_pop_adj) + log(population) + 
    vacancy_rate | year + geography,
  data = casual_data
)

summary(casual_model_simple, vcov = "twoway")

casual_data_full <- all_data |>
  filter(
    room_type == "all",
    year < 2023
  ) |>
  select(
    year,
    geography,
    avg_rent_cpi_adj,
    mortgage_interest_paid_cpi_pop_adj,
    electricity_index,
    water_index,
    population,
    avg_hourly_wage,
    vacancy_rate,
    rental_supply
  )

casual_model_full <- feols(
  log(avg_rent_cpi_adj) ~ 
    log(mortgage_interest_paid_cpi_pop_adj) + log(population) +
    vacancy_rate + log(rental_supply) + log(avg_hourly_wage) +
    log(electricity_index) + log(water_index) | year + geography,
  data = casual_data_full
)

summary(casual_model_full, vcov = "twoway")

casual_data_lagged <- all_data |>
  filter(room_type == "all") |>
  group_by(
    geography
  ) |>
  mutate(
    lagged_mortgage_interest_paid_cpi_pop_adj = lag(mortgage_interest_paid_cpi_pop_adj, 1),
    lagged_population = lag(population, 1),
    lagged_vacancy_rate = lag(vacancy_rate, 1),
    lagged_rental_supply = lag(rental_supply, 1),
    lagged_avg_hourly_wage = lag(avg_hourly_wage, 1),
    lagged_electricity_index = lag(electricity_index, 1),
    lagged_water_index = lag(water_index, 1)
  ) |>
  ungroup() |>
  select(
    year,
    geography,
    avg_rent_cpi_adj,
    contains("lagged")
  ) |>
  filter(year > 2001)

casual_model_lagged <- feols(
  log(avg_rent_cpi_adj) ~ 
    log(lagged_mortgage_interest_paid_cpi_pop_adj) + log(lagged_population) +
    lagged_vacancy_rate + log(lagged_rental_supply) + log(lagged_avg_hourly_wage) +
    log(lagged_electricity_index) + log(lagged_water_index) | year + geography,
  data = casual_data_lagged
)

summary(casual_model_lagged, vcov = "twoway")
  
get_fitted_and_residuals <- function(x, y) {
  tibble(
    id = 1:length(residuals(x)),
    model = y,
    residuals = residuals(x),
    standardized_residuals = residuals(x) / sigma(x),
    rescaled_residuals = sqrt(abs(standardized_residuals)),
    fitted = fitted(x)
  )
}

casual_model_list <- list(
  casual_model_simple = casual_model_simple,
  casual_model_full = casual_model_full,
  casual_model_lagged = casual_model_lagged
)

models_residuals <- mapply(
  get_fitted_and_residuals,
  x = casual_model_list,
  y = names(casual_model_list),
  SIMPLIFY = FALSE
) |>
  bind_rows()

ggplot(models_residuals, aes(x = id, y = standardized_residuals)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(vars(model))

ggplot(models_residuals, aes(x = standardized_residuals)) +
  geom_histogram() +
  facet_wrap(vars(model))

ggplot(models_residuals, aes(sample = standardized_residuals)) +
  geom_qq() +
  geom_qq_line() +
  facet_wrap(vars(model))

ggplot(models_residuals, aes(x = fitted, y = residuals)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(vars(model))

ggplot(models_residuals, aes(x = fitted, y = rescaled_residuals)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(vars(model))

get_residuals_against_vars <- function(model) {
  fixest_data(model) |>
    mutate(
      standardized_residuals = residuals(model) / sigma(model)
    ) |>
    select(      
      -year,
      -geography
    ) |>
    gather(
      -standardized_residuals,
      key = "key",
      value = "value"
    ) |>
    ggplot(aes(x = value, y = standardized_residuals)) +
    geom_point() +
    geom_smooth() +
    facet_wrap(~key, scales = "free")
}

get_residuals_against_vars(casual_model_simple)  
get_residuals_against_vars(casual_model_full)  
get_residuals_against_vars(casual_model_lagged)

get_acf_estimates <- function(model) {
  data <- fixest_data(model) |>
    mutate(
      standardized_residuals = residuals(model) / sigma(model)
    ) |>
    select(
      year,
      geography,
      standardized_residuals
    )
  
  geography_vec <- unique(data$geography)
  
  divide_and_acf <- function(geography, data) {
    acf_data <- data |>
      filter(geography == geography) |>
      select(-geography) |>
      arrange(desc(year))
    
    acf_df <- acf(acf_data$standardized_residuals, plot = FALSE)
    
    with(acf_df, data.frame(lag, acf)) |>
      mutate(
        geography = geography
      )
  }
  
  lapply(
    geography_vec,
    divide_and_acf,
    data = data
  ) |>
    bind_rows() |>
    ggplot(aes(x = lag, y = acf)) +
    geom_hline(aes(yintercept = 0)) +
    geom_segment(mapping = aes(xend = lag, yend = 0)) +
    facet_wrap(~geography)
}

get_acf_estimates(casual_model_simple)
get_acf_estimates(casual_model_full)
get_acf_estimates(casual_model_lagged)

get_clustered_geography_errors <- function(model) {
  fixest_data(model) |>
    mutate(
      standardized_residuals = residuals(model) / sigma(model)
    ) |>
    select(
      year,
      geography,
      standardized_residuals
    ) |>
    ggplot(aes(x = geography, y = standardized_residuals)) +
    geom_boxplot() +
    geom_point()
}

get_clustered_geography_errors(casual_model_simple)
get_clustered_geography_errors(casual_model_full)
get_clustered_geography_errors(casual_model_lagged)

display_fit <- function(model) {
  fixest_data(model) |>
    mutate(
      fitted_values = fitted(model)
    ) |>
    select(
      year,
      geography,
      avg_rent_cpi_adj,
      fitted_values
    ) |>
    ggplot(aes(x = year, y = log(avg_rent_cpi_adj))) +
    geom_point() +
    geom_line(mapping = aes(x = year, y = fitted_values)) +
    facet_wrap(~geography)
}

display_fit(casual_model_full)

###############################################################################

rent_diagram_updated <- dagify(
  rent_price ~ supply + demand + cost,
  supply ~ rental_supply + location,
  rental_supply ~ location,
  demand ~ population + location,
  population ~ location,
  cost ~ mortgage + location,
  mortgage ~ location,
  outcome = "rent_price",
  labels = c(
    rent_price = "rent price",
    supply = "supply",
    demand = "demand",
    cost = "cost",
    rental_supply = "rental availability",
    population = "population",
    mortgage = "mortgage burden",
    location = "location"
  ),
  coords = list(
    x = c(rent_price = 4, supply = 3, demand = 3, cost = 3, rental_supply = 2, 
          population = 2, mortgage = 2, location = 1),
    y = c(rent_price = 2, supply = 1, demand = 2, cost = 3, rental_supply = 1, 
          population = 1.75, mortgage = 3, location = 2)
  )
)

ggdag_status(
  rent_diagram_updated, 
  use_labels = "label", 
  text = FALSE,
) + 
  theme_dag()

casual_data_revised <- all_data |>
  mutate(
    rental_availability = (bachelor_rental_supply + `1-bedroom_rental_supply` +
      `2-bedroom_rental_supply` * 2 + `3-bedroom_rental_supply` * 3) * 
      vacancy_rate / 100,
    rental_capacity = bachelor_rental_supply + `1-bedroom_rental_supply` +
      `2-bedroom_rental_supply` * 2 + `3-bedroom_rental_supply` * 3
  ) |>
  filter(
    room_type == "all",
    year < 2023
  ) |>
  select(
    year,
    geography,
    avg_rent_cpi_adj,
    mortgage_interest_paid_cpi_pop_adj,
    population,
    vacancy_rate,
    rental_capacity,
    rental_availability,
    electricity_index,
    water_index,
    avg_hourly_wage,
    vacancy_rate,
    rental_supply
  )

casual_model_revised <- feols(
  log(avg_rent_cpi_adj) ~  
    log(mortgage_interest_paid_cpi_pop_adj) + log(population) + 
    log(rental_supply) + log(avg_hourly_wage) + log(electricity_index) | year + geography,
  data = casual_data_revised
)

summary(casual_model_revised, vcov = "twoway")

wald(casual_model_revised, keep = "electricity_index", vcov = "twoway")

#########################################################
casual_data_interaction <- all_data |>
  mutate(
    rental_availability = (bachelor_rental_supply + `1-bedroom_rental_supply` +
      `2-bedroom_rental_supply` * 2 + `3-bedroom_rental_supply` * 3) * 
      vacancy_rate / 100,
    rental_capacity = bachelor_rental_supply + `1-bedroom_rental_supply` +
      `2-bedroom_rental_supply` * 2 + `3-bedroom_rental_supply` * 3
  ) |>
  filter(
    room_type == "all",
    year < 2023
  ) |>
  select(
    year,
    geography,
    avg_rent_cpi_adj,
    mortgage_interest_paid_cpi_pop_adj,
    population,
    vacancy_rate,
    rental_capacity,
    rental_availability,
    electricity_index,
    water_index,
    avg_hourly_wage,
    vacancy_rate,
    rental_supply
  )

casual_model_interaction <- feols(
  log(avg_rent_cpi_adj) ~  
    log(mortgage_interest_paid_cpi_pop_adj) + log(population)| 
    year + geography,
  data = casual_data_interaction
)

summary(casual_model_lagged, vcov = "twoway")
display_fit(casual_model_lagged)

wald_variables <- list(
  "lagged_water_index"
)

###############################################################

get_wald_scores <- function(model, variables) {
  get_wald_tibble <- function(variable, model) {
    wald(model, variable, vcov = "twoway", print = FALSE) |>
      as_tibble() |>
      mutate(
        variable = paste({{ variable }}, collapse = " and ")
      ) |>
      relocate(variable)
  }
  
  lapply(
    variables,
    get_wald_tibble,
    model = model
  ) |>
    bind_rows()
}

get_wald_scores(causal_model_lagged, wald_variables)

################################################