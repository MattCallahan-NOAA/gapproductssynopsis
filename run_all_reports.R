library(quarto)
library(keyring)
library(akfingapdata)
library(magrittr)
library(dplyr)

# load species list from Lewis
fishes<-readRDS("common_fishes.RDS")

# add common names for title
token <- create_token("akfin_secret")
txn <- get_gap_taxonomic_groups()

fishes <- fishes %>%
  left_join(txn%>%dplyr::select(common_name, species_code), by="species_code")

fishes <- fishes %>%
  mutate(report_name= gsub(" ", "_", common_name))


# Function to render Quarto document for each species
quarto_file <- "draft_figs_quarto.qmd"

render_synopsis_qmd <- function(name, species_code, survey_definition_id, area_id) {
  quarto_render(
    input = quarto_file,
    execute_params = list(species_code = species_code, survey_definition_id = survey_definition_id, area_id = area_id),
    output_file = paste0(name, ".html")
  )
}

# test
render_synopsis_qmd(
  name = fishes$report_name[33],
  species_code = fishes$species_code[33],
  survey_definition_id = 47,
  area_id = 99903)

# Run synopsis for listed fishes
for (i in 1:nrow(fishes)) {
  render_synopsis_qmd(
    name = fishes$report_name[i],
    species_code = fishes$species_code[i],
    survey_definition_id = 47,
    area_id = 99903
  )
}

# Failures:
# Alaska skate
# Kamchatka flounder
# Greenland turbot
# Bering flounder
# sawback poacher

fishes2<- fishes[28:nrow(fishes),]

for (i in 1:nrow(fishes2)) {
  render_synopsis_qmd(
    name = fishes2$report_name[i],
    species_code = fishes2$species_code[i],
    survey_definition_id = 47,
    area_id = 99903
  )
}

