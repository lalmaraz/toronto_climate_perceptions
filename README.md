# Climate Perceptions in Toronto

This repo contains an R project file for "Women and Highly Educated Toronto Residents Most Likely to Be Concerned About Local Impacts of Climate Change".

It contains four folders: inputs, outputs, scripts, and shiny.
Inputs:
- Source data from the City of Toronto's Open Data Portal
- Reference literature

Outputs:
- The paper folder contains the R Markdown file to generate the paper, a pdf version of the paper, and a complete bibliography.

Scripts:
- EDA
- map_regions
- compare_demographics

Shiny:
- Code for an interactive map, which lives here: https://lorena-almaraz.shinyapps.io/climatestudy/

## How to generate the paper
1. Open `toronto_climate_perceptions.Rproj` in RStudio
2. Open `outputs/paper/paper.Rmd` 
3. Install libraries using `install.packages()` if necessary
4. Run all code chunks
5. The paper is generated to`outputs/paper/`
