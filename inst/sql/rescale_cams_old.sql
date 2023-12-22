select 
 date_time,
 date_forecast,
 lat,
 lon,
 (no2 - 2.64) * 0.54 as no2,
 (pm25 - 1.35) * 0.37 as pm25,
 (pm10 - 1.67) * 0.35 as pm10,
 lat_int,
 lon_int
from 
 fairq_raw.cams_old_processed 
where 
 date_time < (select min(date_time) from fairq_raw.cams_processed) and
 date_forecast < (select min(date_forecast) from fairq_raw.cams_processed)
