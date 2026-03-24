# geo-analysis

Proof of Concept of geographic analysis of climate related misinformation.

<p align="center">
  <img src="https://github.com/climatesense-project/geo-analysis/blob/main/output/spatial%20share.png?raw=true" alt="spatial share overview"/>
</p>

<p align="center">
  <img src="https://github.com/climatesense-project/geo-analysis/blob/main/output/spatial%20count%20time.png?raw=true" alt="spatial count over time"/>
</p>

Repo structure is:
- `R` = #rstats code to be run
- `data` = local storage of interim data
- `output` = final storage of images produced & related result

From the technical point of view the most relevant files are:
- `/R/001 - dump CS.R` which connects to CS SPARQL endpoint and dumps the data to a GeoPackage
- `/R/010 - overview dumped CD.R` builds on results of script 001 to create a basic visualization of climate related claims (only).
- `/R/011 - overview related × unrelated CS.R` builds on results of script 001 to create a comparative analysis of climate related and unrelated claims.
- `/data/cs_data_dump.gpkg` which is the actual data dump (not included in git)

From GIS point of view the most relevant files are:
- `/output/spatial overview.png` geographical overview of any mention as individual place
- `/output/spatial count any.png` geographical overview of any mention as count per country or equivalent
- `/output/spatial count time.png` evolution in time of the count of any mention as count per country or equivalent
- `/output/spatial share.png` percentage of climate related mentions of total mentions per country or equivalent

Due to the GeoPackage size (~250 MB) it is not practical to be included in git, and the script 001 needs to be executed locally. Takes about 90 minutes to run.
