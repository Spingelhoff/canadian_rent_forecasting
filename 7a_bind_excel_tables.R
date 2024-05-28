library(openxlsx)

named_excel_table_list <- list(
  avg_rent = avg_rent_data,
  provincial_room_indicators = yearly_provincial_room_indicators_data,
  provincial_indicators = yearly_provincial_indicators_data,
  indicators = yearly_indicators_data,
  forecast = yearly_forecast_table
)

create_canadian_rent_excel_tables <- function(named_excel_table_list) {
  workbook <- createWorkbook()
  
  table_names <- names(named_excel_table_list)
  
  create_excel_pages_and_tables <- function(table_name, table) {
    addWorksheet(workbook, table_name)
    writeDataTable(workbook, sheet = table_name, x = table, tableName = table_name)
  }
  
  mapply(
    create_excel_pages_and_tables,
    table_name = table_names,
    table = named_excel_table_list
  )
  
  return(workbook)
}

excel_workbook <- named_excel_table_list |>
  create_canadian_rent_excel_tables()

# saveWorkbook(excel_workbook, file = "canadian_rent_excel_model.xlsx", overwrite = TRUE)
