base_models <- tscv_model_data |>
  model(
    mean = MEAN(avg_rent_cpi_adj_pct_change),
    naive = NAIVE(avg_rent_cpi_adj_pct_change)
  )
rw_models <- tscv_model_data |>
  model(
    drift = RW(avg_rent_cpi_adj ~ drift())
  )
arima_models <- tscv_model_data |>
  model(
    arima = ARIMA(avg_rent_cpi_adj_pct_change)
  )  
ets_models <- tscv_model_data |>
  model(
    ets = ETS(avg_rent_cpi_adj)
  )
linear_models <- lagged_tscv_model_data |>
  model(
    base_linear = TSLM(avg_rent_cpi_adj_pct_change ~ lagged_rental_supply_per_person_pct_change + lagged_five_year_mortgage_change),
    trended_linear = TSLM(avg_rent_cpi_adj_pct_change ~ trend() + lagged_rental_supply_per_person_pct_change + lagged_five_year_mortgage_change)
  )

base_glance <- glance(base_models)
rw_glance <- glance(rw_models)
arima_glance <- glance(arima_models)
ets_glance <- glance(ets_models)
linear_glance <- glance(linear_models)

get_ljung_box <- function(model, lag) {
  ljung_box_stats <- model |>
    augment() |>
    features(.innov, ljung_box, lag = lag) |>
    filter(`.id` == max(`.id`, na.rm = TRUE)) |> 
    select(-`.id`)
  
  return(ljung_box_stats)
}

base_residuals <- base_models |>
  get_ljung_box(lag = 4)
rw_residuals <- rw_models |>
  get_ljung_box(lag = 4)
arima_residuals <- arima_models |>
  get_ljung_box(lag = 4)
ets_residuals <- ets_models |>
  get_ljung_box(lag = 4)
linear_residuals <- linear_models |>
  get_ljung_box(lag = 4)

new_data <- new_data(lagged_tscv_model_data, n = 1) |>
  left_join(lagged_modeling_data, by = c("year", "geography", "room_type"))

base_forecast <- base_models |>
  forecast(h = 1)
rw_forecast <- rw_models |>
  forecast(h = 1)
arima_forecast <- arima_models |>
  forecast(h = 1)
ets_forecast <- ets_models |>
  forecast(h = 1)
linear_forecast <- linear_models |>
  forecast(new_data = new_data)

base_accuracy <- base_forecast |>
  accuracy(modeling_data)
rw_accuracy <- rw_forecast |>
  accuracy(modeling_data)
arima_accuracy <- arima_forecast |>
  accuracy(modeling_data)
ets_accuracy <- ets_forecast |>
  accuracy(modeling_data)
linear_accuracy <- linear_forecast |>
  accuracy(lagged_modeling_training_data)

glance_report <- bind_rows(list(base_glance, rw_glance, arima_glance, ets_glance, linear_glance)) |>
  filter(`.id` == max(`.id`, na.rm = TRUE)) |> 
  select(-`.id`)
residual_report <- bind_rows(list(base_residuals, rw_residuals, arima_residuals, ets_residuals, linear_residuals))
accuracy_report <- bind_rows(list(base_accuracy, arima_accuracy, rw_accuracy, ets_accuracy, linear_accuracy)) |>
  arrange(geography, room_type)
full_report <- left_join(accuracy_report, glance_report, by = c(".model", "geography", "room_type")) |>
  left_join(residual_report, by = c(".model", "geography", "room_type"))