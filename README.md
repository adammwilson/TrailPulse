# TrailPulse 🚵 ⛷️

Ever drive 45 minutes to the trailhead only to find the singletrack is a slip-n-slide or the Nordic loops are brown mush? Yeah, same. TrailPulse fixes that.

It's a Quarto website that pulls live weather, streamflow, and forecast data and mashes it into a plain-language trail conditions report for each park. Right now it covers **Hunters Creek Park** (Elma, NY) — 18 miles of forested singletrack.

## What it shows

- **Mud score** — a model-based 1–10 rating of how wrecked the trails probably are, based on soil moisture, recent rain, and drainage. If it's a 9, stay home and clean your bike instead.
- **Snow conditions** — snow depth, recent snowfall, and whether the base is holding. Useful for deciding if it's worth waxing up.
- **Current + 7-day forecast** — temperature, precipitation, humidity, and streamflow plotted together so you can time your ride or ski around the weather window.
- **Climatological context** — shaded ribbons showing how current conditions compare to the historical average. Handy for knowing whether that 3-inch dump is actually unusual or just a Tuesday in February.

## How it works

Data flows in automatically:

- **Weather** — [Open-Meteo](https://open-meteo.com) (historical archive + 7-day forecast)
- **Streamflow** — [USGS](https://waterdata.usgs.gov) gauge on the Buffalo River near Elma
- **Forecast text** — [NWS](https://www.weather.gov)

GitHub Actions rebuilds the site every morning at 4 AM Eastern so the report is fresh before you load the car. 

## Adding a park

Drop a new entry in `data/parks.yml`, create a `parks/<park-id>/index.qmd` (copy Hunters Creek as a template), tune the mud model calibration for the local soils, and you're in.

## Stack

R · Quarto · dygraphs · httr2 · renv · GitHub Actions
