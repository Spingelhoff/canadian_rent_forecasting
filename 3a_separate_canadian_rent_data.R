excel_model_data <- all_data |>
  filter(room_type != "all")

avg_rent_data <- excel_model_data |>
  filter(year > min(year)) |>
  select(
    year,
    geography,
    room_type,
    avg_rent,
    avg_rent_pct_change
  ) |>
  mutate(
    avg_rent_pct_change = avg_rent_pct_change / 100
  )

yearly_provincial_room_indicators_data <- excel_model_data |>
  filter(year == max(year)) |>
  select(
    year,
    geography,
    room_type,
    vacancy_rate,
    rental_supply_per_person,
    rental_supply_pct_change,
    avg_rent_pct_change
  ) |>
  mutate(
    vacancy_rate = vacancy_rate / 100,
    rental_supply_pct_change = rental_supply_pct_change / 100,
    avg_rent_pct_change = avg_rent_pct_change / 100
  )

yearly_provincial_indicators_data <- excel_model_data |>
  filter(year == max(year)) |>
  select(
    year,
    geography,
    population_pct_change,
    consumer_price_pct_change
  ) |>
  distinct() |>
  mutate(
    population_pct_change = population_pct_change / 100,
    consumer_price_pct_change = consumer_price_pct_change / 100
  )

yearly_indicators_data <- excel_model_data |>
  filter(year == max(year)) |>
  select(
    year,
    `5-year_mortgage_rate`
  ) |>
  distinct() |>
  mutate(
    `5-year_mortgage_rate` = `5-year_mortgage_rate` / 100
  )
