# draft NBS figure
library(keyring)
library(akfingapdata)
library(magrittr)
library(dplyr)
library(ggplot2)

token <- create_token("callahan_akfin_api")
txn <- get_gap_taxonomic_groups()

# biomass pull

ebsfish <- c(#21740,
               10210,
               10261,
               10130,
               #81742,
               21720,
               10110,
               471,
               10285,
               #83020
               10120
               )

nbsfish <- c(#21740,
             10210,
             10285,
             #81742,
             #71800,
             #82511,
             #69086,
             21720,
             #40500,
             #80590
             21735,
             471
             )

ebs_biomass <- lapply(ebsfish, FUN=function(x) get_gap_biomass(survey_definition_id=98, area_id=99900, species_code=x)) %>%
  dplyr::bind_rows() %>%
  left_join(txn %>% dplyr::select(species_code, common_name, species_name), by=c("species_code")) %>% 
  mutate(name=paste(common_name, species_name))

nbs_biomass<-lapply(nbsfish, FUN=function(x) get_gap_biomass(survey_definition_id=143, area_id=99902, species_code=x)) %>%
  dplyr::bind_rows() %>%
  left_join(txn %>% dplyr::select(species_code, common_name, species_name), by=c("species_code")) %>% 
  mutate(name=paste(common_name, species_name))

biomass_plot_fun <- function(x, var=TRUE) {
  if(var==TRUE) {
    ggplot(data=x)+
      geom_ribbon(aes(x=year, 
                      ymin=biomass_mt-sqrt(biomass_var),
                      ymax=biomass_mt+sqrt(biomass_var),
                      fill=name), alpha=0.2)+
      geom_line(aes(x=year, y=biomass_mt, color=name))+
      geom_point(aes(x=year, y=biomass_mt))+
      ylab("Biomass (mt)")+
      theme_bw()
  } else {
    ggplot(data=x)+
      geom_line(aes(x=year, y=biomass_mt, color=name))+
      geom_point(aes(x=year, y=biomass_mt))+
      ylab("Biomass (mt)")+
      theme_bw()
  }
}

ebs_plot <- biomass_plot_fun(ebs_biomass) +
  ggtitle("EBS biomass trends for top fish in 2025 minus pollock")
ebs_plot  

png("ESR/ebs_biomass.png",width=12,height=6,units="in",res=300)
ebs_plot  
dev.off()


nbs_plot <- biomass_plot_fun(nbs_biomass) +
  ggtitle("NBS biomass trends for top fish in 2025 minus pollock")
nbs_plot  

png("ESR/nbs_biomass.png",width=12,height=6,units="in",res=300)
nbs_plot  
dev.off()

# 
nbsyears <- unique(nbs_biomass$year)

ebs_in_nbsyears  <- biomass_plot_fun(ebs_biomass %>% filter(year %in% nbsyears)) +
  ggtitle("EBS biomass trends for top ten species in 2025 (NBS years only)")
ebs_in_nbsyears 
ggsave("ESR/ebs_biomass_in_nbs_years.png")
