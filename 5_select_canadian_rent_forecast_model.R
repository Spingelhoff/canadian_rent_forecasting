select_model <- function(full_report) {
  selected_model <- full_report |>
    filter(lb_pvalue > 0.05) |>
    group_by(geography, room_type) |>
    arrange(RMSSE, by_group = TRUE) |>
    slice_head(n = 1) |>
    select(
      geography,
      room_type,
      .model,
      RMSSE
    )
  
  return(selected_model)
}

selected_models <- select_model(full_report)

extract_forecasts <- function(forecast_table) {
  forecast_table |>
    filter(year == year(today())) |>
    select(1:7)
}

pct_change_forecasts <- lapply(
  list(
    base_forecast,
    arima_forecast,
    linear_forecast
  ),
  extract_forecasts
) |>
  bind_rows() |>
  hilo() |>
  unpack_hilo(cols = c("80%", "95%")) |>
  mutate(
    format = "pct_change"
  )

value_forecasts <- lapply(
  list(
    rw_forecast,
    ets_forecast
  ),
  extract_forecasts
) |>
  bind_rows() |>
  hilo() |>
  unpack_hilo(cols = c("80%", "95%")) |>
  mutate(
    format = "value"
  )

all_forecasts <- list(
  value_forecasts,
  pct_change_forecasts
)

extract_forecasts <- function(all_forecasts) {
  clean_forecasts <- function(forecast) {
    forecast |>
      as_tibble() |>
      select(
        year,
        geography, 
        room_type, 
        .model,
        .mean, 
        format,
        `80%_upper`, 
        `80%_lower`
      )
  }
  
  combined_forecasts <- lapply(
    all_forecasts,
    clean_forecasts
  ) |>
    bind_rows()
  
  return(combined_forecasts)
}

full_forecasts <- extract_forecasts(all_forecasts)

selected_forecasts <- selected_models |>
  left_join(full_forecasts, by = c("geography", "room_type", ".model"))