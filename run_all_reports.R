library(quarto)
library(keyring)
library(akfingapdata)
library(magrittr)
library(dplyr)
library(here)

# load species list from Lewis
fishes<-readRDS("common_fishes.RDS")

# add common names for title
token <- create_token("akfin_secret")
txn <- get_gap_taxonomic_groups()

fishes <- fishes %>%
  left_join(txn%>%dplyr::select(common_name, species_code), by="species_code")

fishes <- fishes %>%
  mutate(report_name= gsub(" ", "_", common_name))

# specify commercially important fishes
fishes_a <- fishes %>%
  filter(species_code %in% c(21740,
                             21720,
                             10120,
                             10110,
                             10130,
                             10261,
                             10200,
                             10210,
                             10285,
                             471,
                             10112,
                             20510,
                             10140,
                             21921))

#define region for running report
survey_definition_id <- 98

region <- ifelse(survey_definition_id==47, "goa",
                 ifelse(survey_definition_id==98, "bs",
                        iselse(survey_definition_id==51, "ai")))

area_id <- ifelse(survey_definition_id==47, 99903,
                 ifelse(survey_definition_id==98, 99900,
                        iselse(survey_definition_id==51, 99904)))


# Function to render Quarto document for each species
quarto_file <- "draft_figs_quarto.qmd"

render_synopsis_qmd <- function(name, species_code, survey_definition_id, area_id) {
  quarto_render(
    input = quarto_file,
    execute_params = list(species_code = species_code, survey_definition_id = survey_definition_id, area_id = area_id),
    output_file = paste0(region, "_", name, ".html")
  )
}

# test
render_synopsis_qmd(
  name = fishes_a$report_name[1],
  species_code = fishes_a$species_code[1],
  survey_definition_id = survey_definition_id,
  area_id = area_id)

# Run synopsis for listed fishes
for (i in 13:nrow(fishes_a)) {
  render_synopsis_qmd(
    name = fishes_a$report_name[i],
    species_code = fishes_a$species_code[i],
    survey_definition_id = survey_definition_id,
    area_id = area_id
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

#move files to bs subdirectory
bs_files <- list.files(pattern = "^bs_.*\\.html$")

file.rename(bs_files, file.path("bs", bs_files))

#### update to fix formatting
# I tried to loop biomass and sample size plots
# 3 species didn't even have either specimen or length data
# I kept the first two reports and moved on
#I don't think anyone cares about these in the GOA anyway

quarto_file <- "abridged_figs.qmd"

fishes3 <- fishes %>% filter(species_code %in% c(471,10112,10140,20006,10115))

for (i in 3:nrow(fishes3)) {
  render_synopsis_qmd(
    name = fishes3$report_name[i],
    species_code = fishes3$species_code[i],
    survey_definition_id = 47,
    area_id = 99903
  )
}


#### Now the more substantial one where i just need to adjust the figure sizing
# use draft_figs_lite
quarto_file <- "draft_figs_noage.qmd"
fishes4 <- fishes %>% filter(species_code %in% c(10285,21420,21341, 23010, 23041, 21370, 21110, 21371, 20720
, 24191, 30020, 21390, 20040, 24185, 21347, 10210))

#test
render_synopsis_qmd(
  name = fishes4$report_name[1],
  species_code = fishes4$species_code[1],
  survey_definition_id = 47,
  area_id = 99903)

#run
for (i in 2:nrow(fishes4)) {
  render_synopsis_qmd(
    name = fishes4$report_name[i],
    species_code = fishes4$species_code[i],
    survey_definition_id = 47,
    area_id = 99903
  )
}

# shortraker
srkr <- txn %>% filter(grepl("shortraker", common_name))

render_synopsis_qmd(name="shortaker_rockfish",
                    species_code=30576,
                    survey_definition_id = 47,
                    area_id = 99903)
