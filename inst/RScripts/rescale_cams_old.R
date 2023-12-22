cams_old_rescaled <- send_query("rescale_cams_old")

cams_old_rescaled <-
  cams_old_rescaled %>%
  arrange(lat, lon, date_time) %>%
  mutate(no2 = na_interpolation(no2, option = "linear")) %>%
  mutate(no2 = no2 + 2)

send_data(cams_old_rescaled, "cams_old_rescaled", mode = "truncate")
