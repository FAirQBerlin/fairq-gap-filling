# fairq-gap-filling

This repository contains R code to fill gaps in data sources.

## General Gap Filling Procedure

- Gaps <= 2 time steps (hours) are filled with preceding value (function `fill_small_gaps()`)
- Remaining gaps are filled with the daily cycle and a SeasonalAR(1)MA(1) process on the deviations from the daily cycle.

## How to get started

- Create an .Renviron file in the project folder, see `.Renviron_template` for 
the structure
- Build the R package
- Create a database schema as described in https://github.com/INWT/fairq-data/tree/public/inst/db

## Most important script

`inst/RScripts/main.R` runs the gap filling process for all relevant sources.
These are:
- Observed pollutant data from measuring stations
- Latest pollutant forecast by CAMS

## Input and output

### Input

- Database, schema `fairq_raw`

### Output

- Database, schema `fairq_features`
 
## Code style

Please use the RStudio code autoformatter by pressing `Ctrl + Shift + A` to format the selected code.

# In case on station is deactivated:

remove the station from automatic gap filling and making predictions in the db
```SQL
insert into fairq_features.stations_for_predictions (*) values ('XXX', 'MC XXX', False);
optimize table fairq_features.stations_for_predictions final
```
This will lead to automatically stop making predictions for the station in fairq-model and to gap-fill the values until now.
In case the automatic gap-filling did run longer than the station was active: Remove the gap-filled values from the db:
```SQL
delete from fairq_features.messstationen_filled where station_id = 'XXX';
```
then re-run the script in inst/RScripts/station_history.R, which ensures that alls stations (i.e. those for which we make predictions
and those with only historical data) are gap-filled and used for training the air quality model. 

