# skript to initally fill all station data into messstationen filled

library(fairqGapFilling)

station_history_source <- single_source(
  name = "messstationen_filled",
  variables = list("pm10", "pm25", "no2"),
  location_id_vars = "station_id",
  is_active_only = FALSE,
  optimize = TRUE
)

# nolint start
fairqGapFilling:::etl(station_history_source)
# nolint end
