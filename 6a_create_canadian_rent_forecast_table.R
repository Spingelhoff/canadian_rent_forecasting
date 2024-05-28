supplemental_forecast_data <- excel_model_data |>
  filter(year == max(year)) |>
  select(
    geography,
    room_type,
    consumer_price_index,
    last_avg_rent = avg_rent,
    last_avg_rent_cpi_adj = avg_rent_cpi_adj,
  ) |>
  distinct()

yearly_forecast_table <- selected_forecasts |>
  filter(room_type != "all") |>
  left_join(supplemental_forecast_data, by = c("room_type", "geography")) |>
  mutate(
    converted_forecast = case_when(
      format == "value" ~ .mean * consumer_price_index / 100,
      format == "pct_change" ~ (1 + .mean/100) * last_avg_rent_cpi_adj * consumer_price_index / 100,
      TRUE ~ NA
    ),
    converted_upper = case_when(
      format == "value" ~ `80%_upper` * consumer_price_index / 100,
      format == "pct_change" ~ (1 + `80%_upper`/100) * last_avg_rent_cpi_adj * consumer_price_index / 100,
      TRUE ~ NA
    ),
    converted_lower = case_when(
      format == "value" ~ `80%_lower` * consumer_price_index / 100,
      format == "pct_change" ~ (1 + `80%_lower`/100) * last_avg_rent_cpi_adj * consumer_price_index / 100,
      TRUE ~ NA
    ),
    pct_forecast = (converted_forecast - last_avg_rent) / last_avg_rent,
    pct_upper = (converted_upper - last_avg_rent) / last_avg_rent,
    pct_lower = (converted_lower - last_avg_rent) / last_avg_rent
  ) |>
  select(
    year,
    geography,
    room_type,
    forecast_model = .model,
    model_RMSSE = RMSSE,
    last_avg_rent,
    forecast = converted_forecast,
    upper = converted_upper,
    lower = converted_lower,
    pct_forecast,
    pct_upper,
    pct_lower
  )
