# Renewable Energy Plant System (REP)

A Scala-based system for monitoring and analyzing renewable energy production data from Finland using the Fingrid open data API.

## Features

- Fetches real-time energy data for Wind, Hydro, and Solar sources from Fingrid
- Stores data locally as CSV files
- View energy production summary
- Analyse data with filtering, sorting, searching and statistics
- Alerts for low output and sudden drops in Wind and Hydro production

## Requirements (Proven to work with)

- Java JDK 21
- Scala 3.8.3
- SBT
- sttp client4 (added automatically via build.sbt)

## Setup

1. Clone the repository
2. Register for a free API key at https://data.fingrid.fi
3. Add your API key to config.properties: fingrid.api.key=YOUR_KEY_HERE
4. Run with: sbt run

## Data Sources

All energy data is fetched from Fingrid Open Data (https://data.fingrid.fi):

- Wind: Tuulivoimatuotanto - varttitieto, ID 245, 15 min
- Hydro: Vesivoimatuotanto - reaaliaikatieto, ID 191, 3 min (Sampled to 15 minutes )
- Solar: Aurinkovoiman tuotantoennuste, ID 248, 15 min

Data covers the last 90 days from the time of fetching.

## Project Structure

data/
- Stores csv files to write and read from
    - Three files: hydro.csv, solar.csv, wind.csv

src/main/scala/
- Main.scala          - Entry point and main menu
- EnergySource.scala  - Data models
- FileHandling.scala  - API calls and CSV I/O
- Analysis.scala      - Filtering, sorting, search, statistics
- Display.scala       - Console output
- Alert.scala         - Alert detection

## AI Declaration

Claude AI was used for learning, ideation, structure and debugging throughout the project. All final implementation is our own. Where AI was used more specifically it has been mentioned in the relevant file.

## Authors

- Jussi Grönroos
- Iikka Harjamäki 