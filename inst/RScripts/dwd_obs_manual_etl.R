# The DWD observations were only a substitute for the forecasts because
# there are no historical forecasts.
# Therefore we don't need to import them day by day now that we have started
# collecting a forecast history.
# However, in case we start importing DWD observations again, this script does the
# gap filling.

library(fairqGapFilling)

dwd_source <- single_source(
  name = "dwd_observations_filled",
  variables = list(
    "wind_direction",
    "wind_speed",
    "precipitation",
    "temperature",
    "cloud_cover",
    "pressure_msl",
    "sunshine"
  ),
  location_id_vars = list("x", "y"),
  daily_only = TRUE
)

# nolint start
fairqGapFilling:::etl(dwd_source)
# nolint end
