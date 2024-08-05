library(cansim)
library(cmhc)
library(tsibble)
library(fable)
library(feasts)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(purrr)

# Province to SGC List
provincial_sgc_list <- read.csv("sgc_provincial.csv")

# Statistics Canada Data
get_statcan_provincial_data <- function(provincial_sgc_list, from) {
  province_list <- provincial_sgc_list$province
  
  # Housing Data
  
  five_year_mortgage_data <- get_cansim("34-10-0145-01") |> #Monthly, 1951-present, Canada wide
    filter(
      year(Date) > from
    ) |>
    select(
      date = Date,
      "5-year_mortgage_rate" = val_norm
    )
  
  housing_price_index_data <- get_cansim("18-10-0205-01") |> #Monthly, 1981-present, Provincial, UOM 201612 = 100
    filter(
      year(Date) > from,
      `New housing price indexes` == "Total (house and land)"
    ) |>
    select(
      date = Date,
      geography = GEO,
      "housing_price_index" = VALUE,
    ) |>
    filter(
      geography %in% province_list
    )
  
  mortgage_interest_data <- get_cansim("36-10-0226-01") |> #Yearly, 1981-2022, Provincial
    filter(
      year(Date) > 2000,
      Estimates == "Mortgage interest paid",
      GEO %in% province_list
    ) |>
    select(
      date = Date,
      geography = GEO,
      "mortgage_interest_paid" = val_norm,
    )
  
  consumer_price_index_data <- get_cansim("18-10-0004-01") |> #Monthly, 1914-present, Provincial, UOM 2002 = 100
    filter(
      year(Date) > from,
      `Products and product groups` %in% c("Energy", "All-items excluding energy", "All-items", "Water", "Electricity"),
      GEO %in% c(province_list),
    ) |>
    select(
      date = Date,
      geography = GEO,
      value = VALUE,
      products = `Products and product groups`
    ) |> #UOM 2002=100
    pivot_wider(
      names_from = products,
      values_from = value
    ) |>
    rename(
      consumer_price_index = `All-items`,
      energy_consumer_index = Energy,
      excluding_energy_consumer_index = `All-items excluding energy`,
      water_index = Water,
      electricity_index = Electricity 
    )
  
  # Immigration Data 
  international_migration_data <- get_cansim("17-10-0040-01") |> #Quarterly, 1946-present, Provincial
    filter(
      year(Date) > from,
      `Components of population growth` %in% c("Immigrants", "Net emigration"),
      GEO %in% province_list
    ) |>
    select(
      date = Date,
      geography = GEO,
      classification = `Components of population growth`,
      people = VALUE
    ) |>
    pivot_wider(
      names_from = classification,
      values_from = people
    ) |>
    rename(
      immigrants = Immigrants,
      emigrants = `Net emigration`
    )
  
  # Income Data
  avg_hourly_wage_data <- get_cansim("14-10-0209-01") |> #Monthly, 2001-present, Provincial
    filter(
      year(Date) > from,
      `North American Industry Classification System (NAICS)` == "Industrial aggregate excluding unclassified businesses [11-91N]",
      GEO %in% province_list
    ) |>
    select(
      date = Date,
      geography = GEO,
      avg_hourly_wage = VALUE
    )
  
  # GDP Data
  
  gdp_data <- get_cansim("36-10-0402-01") |> #Yearly, 1997-present, Provincial
    filter(
      year(Date) > from,
      `North American Industry Classification System (NAICS)` == "All industries [T001]",
      GEO %in% province_list
    ) |>
    select(
      date = Date,
      geography = GEO,
      gdp = val_norm
    )
  
  # Population Data 
  provincial_pop_estimates_data <- get_cansim("17-10-0009-01") |> #Quarterly, 1946-present, Provincial
    filter(
      year(Date) > from,
      GEO %in% province_list
    ) |>
    select(
      date = Date,
      geography = GEO,
      population = VALUE
    )
  
  # Combined Data
  
  all_data_list <- list(
    housing_price_index_data,
    consumer_price_index_data,
    mortgage_interest_data,
    international_migration_data,
    avg_hourly_wage_data,
    provincial_pop_estimates_data,
    gdp_data
  )
  
  combined_data <- reduce(all_data_list, left_join, by = c("date", "geography")) |>
    left_join(five_year_mortgage_data, by = "date")
  
  return(combined_data)
}

combined_statcan_data <- get_statcan_provincial_data(provincial_sgc_list, from = 2000)

# CMHC Data
get_cmhc_provincidal_data <- function(provincial_sgc_list, from) {
  
  get_cmhc_data <- function(sgc_code, from) {
    year_var <- year(today()) - 20
    
    avg_rent_data <- get_cmhc( #Yearly
      survey = "Rms", 
      series = "Average Rent", 
      dimension = "Bedroom Type", 
      breakdown = "Historical Time Periods", 
      geo_uid = sgc_code
    ) |>
      filter(
        year(Date) > from
      ) |>
      select(
        date = Date,
        bedroom_type = `Bedroom Type`,
        avg_rent = Value,
        geography_number = `GeoUID`
      ) |>
      pivot_wider(
        names_from = bedroom_type,
        values_from = avg_rent
      ) |>
      rename(
        all_avg_rent = Total,
        bachelor_avg_rent = Bachelor,
        "1-bedroom_avg_rent" = `1 Bedroom`,
        "2-bedroom_avg_rent" = `2 Bedroom`,
        "3-bedroom_avg_rent" = `3 Bedroom +`
      )
    
    vacancy_data <- get_cmhc( #Yearly
      survey = "Rms", 
      series = "Vacancy Rate", 
      dimension = "Bedroom Type", 
      breakdown = "Historical Time Periods", 
      geo_uid = sgc_code
    ) |>
      filter(
        year(Date) > from
      ) |>
      select(
        date = Date,
        bedroom_type = `Bedroom Type`,
        vacancy_rate = Value,
        geography_number = GeoUID
      ) |>
      pivot_wider(
        names_from = bedroom_type,
        values_from = vacancy_rate
      ) |>
      rename(
        all_vacancy_rate = Total,
        bachelor_vacancy_rate = Bachelor,
        "1-bedroom_vacancy_rate" = `1 Bedroom`,
        "2-bedroom_vacancy_rate" = `2 Bedroom`,
        "3-bedroom_vacancy_rate" = `3 Bedroom +`
      )
    
    rental_universe_data <- get_cmhc( #Yearly
      survey = "Rms", 
      series = "Rental Universe", 
      dimension = "Bedroom Type", 
      breakdown = "Historical Time Periods", 
      geo_uid = sgc_code
    ) |>
      filter(
        year(Date) > from
      ) |>
      select(
        date = Date,
        bedroom_type = `Bedroom Type`,
        rental_supply = Value,
        geography_number = GeoUID
      ) |>
      pivot_wider(
        names_from = bedroom_type,
        values_from = rental_supply
      ) |>
      rename(
        all_rental_supply = Total,
        bachelor_rental_supply = Bachelor,
        "1-bedroom_rental_supply" = `1 Bedroom`,
        "2-bedroom_rental_supply" = `2 Bedroom`,
        "3-bedroom_rental_supply" = `3 Bedroom +`
      )
    
    data_list <- list(
      avg_rent_data,
      vacancy_data,
      rental_universe_data
    )
    
    combined_data <- reduce(data_list, left_join, by = c("date", "geography_number")) |>
      left_join(provincial_sgc_list, by = c("geography_number" = "sgc")) |>
      rename(
        geography = province
      )
    
    return(combined_data)
  }
  
  combined_data <- lapply(
    provincial_sgc_list$sgc,
    get_cmhc_data,
    from = from
  ) |>
    bind_rows() |>
    arrange(date)
  
  return(combined_data)
}

combined_cmhc_data <- get_cmhc_provincidal_data(provincial_sgc_list, from = 2000)

# combined_data
combined_statcan_cmhc_data <- left_join(combined_statcan_data, combined_cmhc_data, by = c("date", "geography"))
