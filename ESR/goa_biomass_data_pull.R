# assemble bridget's list

library(keyring)
library(akfingapdata)
library(magrittr)
library(dplyr)
library(ggplot2)

fishes<-readRDS("common_fishes.RDS")
# add common names for title
token <- create_token("akfin_secret")
txn <- get_gap_taxonomic_groups()

fishes <- fishes %>%
  left_join(txn%>%dplyr::select(common_name, species_code), by="species_code")

Bfishes<-fishes %>%
  filter(species_code %in% c(21740, # pollock
                             21720, # pcod
                             10110, # arrowtooth
                             10130, # fhs
                             10261, # n rock sole
                             10262, # s sock sole
                             10200, # rex sole
                             10180, # Dover sole
                             30060, # POP
                             20510, # BK
                             30020, # idiot
                             30420, # Northern rockfish
                             21921)) # Atka
txn %>% filter(grepl("skate", tolower(common_name)))
# 420
# 440

txn %>% filter(grepl("yelloweye", tolower(common_name)))
# 30470

txn %>% filter(grepl("rougheye", tolower(common_name)))
# 30050
# 30051
txn %>% filter(grepl("blackspotted", tolower(common_name)))
# 30052

txn %>% filter(grepl("dusky", tolower(common_name)))
# 30150
# 30152

txn %>% filter(grepl("shortraker", tolower(common_name)))
# 30576

Bfishes2 <- txn %>%
  filter(species_code %in% c(420,
                             440,
                             30470,
                             30050,
                             30051,
                             30052,
                             30150,
                             30151,
                             30152,
                             30576)) %>%
           mutate(scientific_name=species_name) %>%
           dplyr::select(scientific_name, species_code, common_name)

Bfishes <- Bfishes %>%
  bind_rows(Bfishes2)

goa_biomass <-lapply(Bfishes$species_code, FUN = function(x) get_gap_biomass(
  survey_definition_id = 47,
  area_id = 99903,
  species_code = x,
  start_year=1990,
  end_year=2023)) %>%
  bind_rows() %>%
  arrange(year)

goa_biomass <- goa_biomass %>%
  left_join(Bfishes, by="species_code")

  biomass_plot <- ggplot(data=goa_biomass)+
    geom_ribbon(aes(x=year, 
                    ymin=biomass_mt-sqrt(biomass_var),
                    ymax=biomass_mt+sqrt(biomass_var),
                    fill=common_name), alpha=0.2)+
  geom_line(aes(x=year, y=biomass_mt, color=common_name))+
  geom_point(aes(x=year, y=biomass_mt))+
  ylab("Biomass (mt)")+
    theme_bw()
biomass_plot    

# assign groups
goa_biomass <-goa_biomass %>%
  mutate(group=case_when(species_code %in% c(#30060, # POP
                                             30020, # idiot
                                             30420,
                                             30470,
                                             30050,
                                             30051,
                                             30052,
                                             30150,
                                             30151,
                                             30152,
                                             30576) ~ "rockfish nonPOP",
                         species_code %in% c(10110, # arrowtooth
                                             10130, # fhs
                                             10261, # n rock sole
                                             10262, # s sock sole
                                             10200, # rex sole
                                             10180) ~ "flatfish",# Dover sole)
                         .default = "other" ))

biomass_plot <- ggplot(data=goa_biomass)+
  geom_ribbon(aes(x=year, 
                  ymin=biomass_mt-sqrt(biomass_var),
                  ymax=biomass_mt+sqrt(biomass_var),
                  fill=common_name), alpha=0.2)+
  geom_line(aes(x=year, y=biomass_mt, color=common_name))+
  facet_wrap(~group, scales="free")+
  ylab("Biomass (mt)")+
  theme_bw()
biomass_plot 

#save
saveRDS(goa_biomass, "ESR/goa_biomass.RDS")
