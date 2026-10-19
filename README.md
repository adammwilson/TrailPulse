# TrailPulse 🚵 ⛷️

Ever drive 45 minutes to the trailhead only to find the singletrack is a slip-n-slide or the Nordic loops are brown mush? Yeah, same. TrailPulse fixes that.

It's a Quarto website that pulls live weather, streamflow, and forecast data and mashes it into a plain-language trail conditions report for each park. Right now it covers **Hunters Creek Park** (Wales, NY) — 18 miles of forested singletrack.

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

GitHub Actions rebuilds the site twice daily at **6:00 AM and 1:00 PM EST** (UTC crons `0 11` / `0 18`; during EDT that is 7:00 AM / 2:00 PM local). 

## Adding a park

Drop a new entry in `data/parks.yml`, create a `parks/<park-id>/index.qmd` (copy Hunters Creek as a template), tune the mud model calibration for the local soils, and you're in.

For Erie County parks, park boundaries for HLS EVI (AppEEARS) live at `parks/<park-id>/data/*.geojson`, referenced from `boundary_file` in `data/parks.yml` (path is relative to the park page directory). Source layer: [Erie County Parks Locator](https://gis.erie.gov/server/rest/services/EC_Parks/Parks_Locator/MapServer/1) (`NAME` / `SHORT_NAME` / `Location` / `acres`).

## NASA Earthdata credentials (NDVI)

The NDVI panel fetches HLS Landsat + Sentinel-2 data via the [NASA AppEEARS API](https://appeears.earthdatacloud.nasa.gov), which requires a free [NASA Earthdata account](https://urs.earthdata.nasa.gov/users/new).

### Local development

Add credentials to your `.Renviron` (run `usethis::edit_r_environ()` or edit `~/.Renviron` directly):

```
EARTHDATA_USER=your_username
EARTHDATA_PASSWORD=your_password
```

Restart R after saving. The first render submits an async task to AppEEARS; the NDVI chart appears on the next render once the task completes (~5–20 min).

### GitHub Actions

Add the same two values as repository secrets (**Settings → Secrets and variables → Actions → New repository secret**):

| Secret name | Value |
|---|---|
| `EARTHDATA_USER` | your Earthdata username |
| `EARTHDATA_PASSWORD` | your Earthdata password |

Then expose them as environment variables in the workflow file:

```yaml
env:
  EARTHDATA_USER: ${{ secrets.EARTHDATA_USER }}
  EARTHDATA_PASSWORD: ${{ secrets.EARTHDATA_PASSWORD }}
```

If the credentials are missing or invalid, the NDVI panel is silently skipped — all other panels render normally.

## Stack

R · Quarto · dygraphs · httr2 · renv · GitHub Actions
