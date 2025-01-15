library(quarto)
library(keyring)
library(akfingapdata)
library(magrittr)
library(dplyr)
library(here)
# rockfish script for Kristen. This is just a copy of the run_all_reports script
# I had to run the dusky and dark quarto separately since there is no recent data and the cpue maps failed.
#rf <- c(30150, 30152, 30535, 30475, 30420, 30100) %>% data.frame()
rf <- 30430 %>% data.frame()
names(rf)<-"species_code"

# add common names for title
token <- create_token("akfin_secret")
txn <- get_gap_taxonomic_groups()

rf <- rf %>%
  left_join(txn%>%dplyr::select(common_name, species_name, species_code), by="species_code")

rf <- rf %>%
  mutate(report_name= gsub(" ", "_", common_name))

survey_definition_id <- 47

region <- ifelse(survey_definition_id==47, "goa",
                 ifelse(survey_definition_id==98, "bs",
                        ifelse(survey_definition_id==52, "ai")))

area_id <- ifelse(survey_definition_id==47, 99903,
                  ifelse(survey_definition_id==98, 99900,
                         ifelse(survey_definition_id==52, 99904)))

# determine which have agecomps
ages<-lapply(rf$species_code, function(X) 
  get_gap_agecomp(survey_definition_id = survey_definition_id,
                  area_id = area_id,
                  species_code = X,
                  start_year = 1980,
                  end_year = 2024)) %>%
  bind_rows()

age_vec <- unique(ages$species_code)

rfa <-rf %>%
  filter(species_code %in% age_vec)

rfna <-rf %>%
  filter(!species_code %in% age_vec)

quarto_file <- "draft_figs_quarto.qmd"

render_synopsis_qmd <- function(name, species_code, survey_definition_id, area_id) {
  quarto_render(
    input = quarto_file,
    execute_params = list(species_code = species_code, survey_definition_id = survey_definition_id, area_id = area_id),
    output_file = paste0(region, "_", name, ".html")
  )
}

for (i in 1:nrow(rfa)) {
  render_synopsis_qmd(
    name = rfa$report_name[i],
    species_code = rfa$species_code[i],
    survey_definition_id = survey_definition_id,
    area_id = area_id
  )
}

quarto_file <- "draft_figs_noage.qmd"
render_synopsis_qmd(
  name = rfna$report_name[1],
  species_code = rfna$species_code[1],
  survey_definition_id = survey_definition_id,
  area_id = area_id)
