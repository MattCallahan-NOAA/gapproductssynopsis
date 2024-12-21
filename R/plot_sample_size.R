plot_sample_size <- function() {
  length_count <- gap_length %>%
    group_by(year) %>%
    summarize(n_samples=sum(frequency, na.rm=T))%>%
    mutate(sample_type = "length")
  weight_count <- gap_specimen %>%
    filter(weight_g > 0) %>%
    group_by(year) %>%
    summarize(n_samples=n())%>%
    mutate(sample_type = "weight")
  if(any(!is.na(gap_specimen$age))) {
    age_count <-gap_specimen %>%
      filter(age > 0) %>%
      group_by(year) %>%
      summarize(n_samples=n())%>%
      mutate(sample_type = "age") }
  else {
    age_count<-gap_specimen %>%
      group_by(year) %>%
      summarize() %>%
      mutate(n_samples=0,
             sample_type = "age")
  }
  biodata_count <-length_count %>%
    bind_rows(weight_count) %>%
    bind_rows(age_count) %>%
    arrange(year) %>%
    mutate(sample_type=factor(sample_type, levels=c("length", "weight", "age")))
  
  min_break<-min(biodata_count$year)
  max_break<-max(biodata_count$year)
  
  ggplot(data=biodata_count)+
    geom_text(aes(x=sample_type, y=year, label=n_samples))+
    scale_y_continuous(breaks=seq(min_break, max_break, by=1))+
    xlab("")+ylab("")+
    ggtitle("sample sizes")+
    theme_bw()+
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.x=element_blank())
}

#plot_sample_size()
