# geo-analysis

Proof of Concept of geographic analysis of climate related misinformation.

Repo structure is:
- `R` = #rstats code to be run
- `data` = local storage of interim data
- `output` = final storage of images produced & related result

Of most interest are two files:
- `/R/001 - dump CS.R` which connects to CS SPARQL endpoint and dumps the data to a GeoPackage
- `/data/cs_data_dump.gpkg` which is the actual data dump (not included in git)

Due to the GeoPackage size (~50 MB) it is not practical to be included in git, and the script 001 needs to be executed locally. Takes about 20 minutes to run.