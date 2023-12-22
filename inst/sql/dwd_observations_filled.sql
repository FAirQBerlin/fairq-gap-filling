-- Step 1: Sun never shines at night, so we can fill those values immediately
with dwd_observations_no_sun_at_night as (
select
	x,
	y,
	date_time,
	wind_direction,
	wind_speed,
	precipitation,
	temperature,
	cloud_cover,
	pressure_msl,
	if(toHour(date_time) in (21, 22, 23, 0, 1, 2, 3), 0, sunshine) sunshine,
	  
	-- Keep information whether the delivered value was null
	dwd_obs.wind_direction is null as wind_direction_was_null,
	dwd_obs.wind_speed is null as wind_speed_was_null,
	dwd_obs.precipitation is null as precipitation_was_null,
	dwd_obs.temperature is null as temperature_was_null,
	dwd_obs.cloud_cover is null as cloud_cover_was_null,
	dwd_obs.pressure_msl is null as pressure_msl_was_null,
	dwd_obs.sunshine is null as sunshine_was_null
from
	dwd_observations_processed dwd_obs
inner join
 fairq_{{ mode }}features.mapping_reprojection using(lat_int, lon_int)
 ),
-- Step 2: If there is already a filled value from a previous gap filling run, use it
data_with_already_filled_gaps as (
select
	x,
	y,
	date_time,
	
	-- try to use the filled value if the new value is NULL
  if(wind_direction is null,
	  wind_direction_filled,
	  wind_direction) wind_direction,
	if(wind_speed is null,
	  wind_speed_filled,
	  wind_speed) wind_speed,
	if(precipitation is null,
	  precipitation_filled,
	  precipitation) precipitation,
	if(temperature is null,
	  temperature_filled,
	  temperature) temperature,
	if(cloud_cover is null,
	  cloud_cover_filled,
	  cloud_cover) cloud_cover,
	if(pressure_msl is null,
	  pressure_msl_filled,
	  pressure_msl) pressure_msl,
	if(sunshine is null,
	  sunshine_filled,
	  sunshine) sunshine,
	
	wind_direction_was_null,
	wind_speed_was_null,
	precipitation_was_null,
	temperature_was_null,
	cloud_cover_was_null,
	pressure_msl_was_null,
	sunshine_was_null
	
from
	dwd_observations_no_sun_at_night
left join fairq_{{ mode }}features.dwd_observations_filled
		using(date_time,
	x,
	y)
),
-- Step 3: If there is no information about a variable for a pair of coordinates, use the average over Berlin for the respective hour
averages as (
select
	date_time,
	-- https://github.com/INWT/fairq-gap-filling/issues/28#issuecomment-1202552860
	round((360 + atan2(avg(sin(wind_direction * pi()/180)) , avg(cos(wind_direction * pi()/180))) * 180/pi()) % 360) avg_wind_direction,
	round(avg(wind_speed)) avg_wind_speed,
	round(avg(precipitation)) avg_precipitation,
	round(avg(temperature)) avg_temperature,
	toUInt8(round(avg(cloud_cover))) avg_cloud_cover,
	round(avg(pressure_msl)) avg_pressure_msl,
	toUInt8(round(avg(sunshine))) avg_sunshine
from
	dwd_observations_no_sun_at_night
group by
	date_time
order by
	date_time
)

select
	x,
	y,
	date_time,
	if(wind_direction is null,
	  averages.avg_wind_direction,
	  data_with_already_filled_gaps.wind_direction) as wind_direction,
	if(wind_speed is null,
	  averages.avg_wind_speed,
	  data_with_already_filled_gaps.wind_speed) as wind_speed,
	if(precipitation is null,
	  averages.avg_precipitation,
	  data_with_already_filled_gaps.precipitation) as precipitation,
	if(temperature is null,
	  averages.avg_temperature,
	  data_with_already_filled_gaps.temperature) as temperature,
	if(cloud_cover is null,
	  averages.avg_cloud_cover,
	  data_with_already_filled_gaps.cloud_cover) as cloud_cover,
	if(pressure_msl is null,
	  averages.avg_pressure_msl,
	  data_with_already_filled_gaps.pressure_msl) as pressure_msl,
	if(sunshine is null,
	  averages.avg_sunshine,
	  data_with_already_filled_gaps.sunshine) as sunshine,
	wind_direction_was_null,
	wind_speed_was_null,
	precipitation_was_null,
	temperature_was_null,
	cloud_cover_was_null,
	pressure_msl_was_null,
	sunshine_was_null
from
	data_with_already_filled_gaps
left join averages
		using(date_time);
