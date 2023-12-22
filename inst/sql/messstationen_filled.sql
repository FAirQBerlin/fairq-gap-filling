with station_data as (
select
	substring(id, 4, 3) as station_id,
	`timestamp` as date_time,
	lat_int,
	lon_int,
	stattyp,
	feinstaub_pm10 as pm10,
	feinstaub_pm2_5 as pm25,
	stickstoffdioxid as no2
from
	stadtstruktur_measuring_stations_processed
inner join  
  messstationen_daten	using(station_id)
where id in (
select id from fairq_{{ mode }}features.stations_for_predictions {{ if (is_active_only) "where is_active" }}
)
order by station_id,	date_time
),

station_data_mapped as (
-- With x/y coordinate mapped
select
	station_id,
	date_time,
	x,
	y,
	stattyp as station_type,
	pm10,
	pm25,
	no2
from
	station_data
inner join 
 fairq_{{ mode }}features.mapping_reprojection
		using(lat_int,	lon_int)
),

filled_data as (
select
	station_id,
	date_time,
	pm10_filled,
	pm25_filled,
	no2_filled
from
	fairq_{{ mode }}features.messstationen_filled
 )

 select
	station_id,
	date_time,
	x,
	y,
	station_type,
	-- If there is already a filled value, use it, but only if there is no new value
	if(pm10 is null,
	  pm10_filled,
	  pm10) pm10,
	if(pm25 is null,
	  pm25_filled,
	  pm25) pm25,
	if(no2 is null,
	  no2_filled,
	  no2) no2,
	-- Keep information whether the delivered value was null
	station_data_mapped.pm10 is null as pm10_was_null,
	station_data_mapped.pm25 is null as pm25_was_null,
	station_data_mapped.no2 is null as no2_was_null
from
	station_data_mapped
left join filled_data
		using(station_id,
	date_time);
