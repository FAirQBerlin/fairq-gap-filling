with latest_forecast_dates_cams AS (
  select
    lat_int,
    lon_int,
    date_time,
    max(date_forecast) AS date_forecast
  from
    fairq_{{ mode }}features.cams_all
  group by lat_int, lon_int, date_time
)
SELECT
  date_time, 
  lat_int, 
  lon_int,
  no2, 
  pm25, 
  pm10,
  no2 is null as no2_was_null,
  pm25 is null as pm25_was_null,
  pm10 is null as pm10_was_null
FROM
  fairq_{{ mode }}features.cams_all
where (lat_int, lon_int, date_time, date_forecast) in (select lat_int, lon_int, date_time, date_forecast from latest_forecast_dates_cams)
order by lat_int, lon_int, date_time
;
