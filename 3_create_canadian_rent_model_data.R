modeling_data <- all_data |>
  filter(
    year > min(year) + 1
  ) |>
  as_tsibble(index = year, key = c("geography", "room_type"))

lagged_modeling_data <- modeling_data |>
  mutate(
    lagged_rental_supply_per_person_pct_change = lag(rental_supply_per_person_pct_change),
    lagged_five_year_mortgage_change = lag(five_year_mortgage_change)
  ) |>
  bind_rows(
    modeling_data |>
      mutate(
        year = year + 1,
        lagged_rental_supply_per_person_pct_change = rental_supply_pct_change,
        lagged_five_year_mortgage_change = five_year_mortgage_change,
        avg_rent_cpi_adj = NA,
        avg_rent_cpi_adj_change = NA,
        avg_rent_cpi_adj_pct_change = NA
      ) |>
      filter(year == year(today()))
  ) |>
  select(
    year,
    geography,
    room_type,
    contains("lagged"),
    contains("avg_rent_cpi_adj")
  ) |>
  filter(
    year > min(year)
  )

lagged_modeling_training_data <- lagged_modeling_data |>
  filter(year < max(year))

tscv_model_data <- modeling_data |>
  stretch_tsibble(.init = 10)

lagged_tscv_model_data <- lagged_modeling_training_data |>
  stretch_tsibble(.init = 10)