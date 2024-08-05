yearly_combined_data <- combined_statcan_cmhc_data |>
  arrange(date) |>
  group_by(
    year = year(date),
    geography
  ) |>
  summarize(
    housing_price_index = mean(housing_price_index),
    consumer_price_index = mean(consumer_price_index),
    excluding_energy_consumer_index = mean(excluding_energy_consumer_index),
    energy_consumer_index = mean(energy_consumer_index),
    water_index = mean(water_index),
    electricity_index = mean(electricity_index),
    gdp = sum(gdp, na.rm = TRUE),
    immigrants = sum(immigrants, na.rm = TRUE),
    emigrants = sum(emigrants, na.rm = TRUE),
    avg_hourly_wage = mean(avg_hourly_wage),
    population = last(population, na_rm = TRUE),
    `5-year_mortgage_rate` = last(`5-year_mortgage_rate`, na_rm = TRUE) * 100,
    mortgage_interest_paid = mean(mortgage_interest_paid, na.rm = TRUE),
    bachelor_avg_rent = mean(bachelor_avg_rent, na.rm = TRUE),
    `1-bedroom_avg_rent` = mean(`1-bedroom_avg_rent`, na.rm = TRUE),
    `2-bedroom_avg_rent` = mean(`2-bedroom_avg_rent`, na.rm = TRUE),
    `3-bedroom_avg_rent` = mean(`3-bedroom_avg_rent`, na.rm = TRUE),
    all_avg_rent = mean(all_avg_rent, na.rm = TRUE),
    bachelor_vacancy_rate = mean(bachelor_vacancy_rate, na.rm = TRUE),
    `1-bedroom_vacancy_rate` = mean(`1-bedroom_vacancy_rate`, na.rm = TRUE),
    `2-bedroom_vacancy_rate` = mean(`2-bedroom_vacancy_rate`, na.rm = TRUE),
    `3-bedroom_vacancy_rate` = mean(`3-bedroom_vacancy_rate`, na.rm = TRUE),
    all_vacancy_rate = mean(all_vacancy_rate, na.rm = TRUE),
    bachelor_rental_supply = last(bachelor_rental_supply, na_rm = TRUE),
    `1-bedroom_rental_supply` = last(`1-bedroom_rental_supply`, na_rm = TRUE),
    `2-bedroom_rental_supply` = last(`2-bedroom_rental_supply`, na_rm = TRUE),
    `3-bedroom_rental_supply` = last(`3-bedroom_rental_supply`, na_rm = TRUE),
    all_rental_supply = last(all_rental_supply, na_rm = TRUE)
  ) |>
  filter(year != year(today())) |>
  ungroup()

unpivot_and_join <- function(data, containing, pivot_key, common_key) {
  unpivot <- function(x, data, common_key, pivot_key) {
    data |>
      select(all_of(common_key), contains(x)) |>
      rename_with(.fn = ~ str_remove_all(.x, str_c("_", x)), .cols = contains(x)) |>
      pivot_longer(
        !common_key,
        names_to = pivot_key,
        values_to = x
      )
  }
  
  pivoted_data <- lapply(
    containing, 
    unpivot, 
    data = data, 
    common_key = common_key,
    pivot_key = pivot_key
  ) |>
    reduce(left_join, by = c(common_key, pivot_key))
  
  result <- left_join(data, pivoted_data, by = c("year", "geography"))
  
  return(result)
}

all_data <- yearly_combined_data |>
  group_by(
    geography
  ) |>
  mutate(
    gdp_change = difference(gdp),
    gdp_pct_change = (gdp - lag(gdp))/lag(gdp) * 100,
    gdp_per_capita = gdp/population,
    gdp_per_capita_change = difference(gdp_per_capita),
    gdp_per_capita_pct_change = (gdp_per_capita - lag(gdp_per_capita))/lag(gdp_per_capita) * 100,
    housing_price_pct_change = (housing_price_index - lag(housing_price_index))/lag(housing_price_index) * 100,
    five_year_mortgage_change = `5-year_mortgage_rate` - lag(`5-year_mortgage_rate`),
    consumer_price_pct_change = (consumer_price_index - lag(consumer_price_index))/lag(consumer_price_index) * 100,
    population_pct_change = (population - lag(population))/lag(population) * 100,
    immigration_pct_change = (immigrants - emigrants)/lag(population) * 100,
    wage_pct_change = (avg_hourly_wage - lag(avg_hourly_wage))/lag(avg_hourly_wage) * 100
  ) |>
  ungroup() |>
  unpivot_and_join(
    containing = c("vacancy_rate", "avg_rent", "rental_supply"),
    pivot_key = "room_type",
    common_key = c("year", "geography")
  ) |>
  group_by(
    geography,
    room_type
  ) |>
  mutate(
    rental_supply_per_person = rental_supply/population * 1000,
    rental_supply_per_person_pct_change = (rental_supply_per_person - lag(rental_supply_per_person))/lag(rental_supply_per_person) * 100,
    rental_supply_pct_change = (rental_supply - lag(rental_supply))/lag(rental_supply) * 100,
    mortgage_interest_paid_cpi_adj = mortgage_interest_paid/consumer_price_index * 100,
    mortgage_interest_paid_cpi_pop_adj = mortgage_interest_paid_cpi_adj/population,
    avg_rent_cpi_adj = avg_rent/consumer_price_index * 100,
    avg_rent_cpi_adj_change = difference(avg_rent_cpi_adj),
    avg_rent_cpi_adj_pct_change = (avg_rent_cpi_adj - lag(avg_rent_cpi_adj))/lag(avg_rent_cpi_adj) * 100,
    avg_rent_change = difference(avg_rent),
    avg_rent_pct_change = (avg_rent - lag(avg_rent))/lag(avg_rent) * 100,
    vacancy_change = vacancy_rate - lag(vacancy_rate)
  ) |>
  ungroup()
