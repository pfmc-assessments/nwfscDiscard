

plot_wcgop_bio <- function(dir, data, column = "LENGTH"){

  get_name <- unique(data$COMMON_NAME)
  species <- gsub(" ", "_", get_name)

  samples_by_year <- data %>%
  	group_by(gear, RYEAR) %>%
  	reframe(
  	  n_lengths = length(!is.na(LENGTH))
    )
  samples_by_year <- as.data.frame(samples_by_year)

  data$bio_plot <- data[, column]
  if(column == "LENGTH"){
  	y_lab = "Lenghth (cm)"
  } else {
  	y_lab = "Age"
  }

  data$catch_shares <- "non_catch_shares"
  data$catch_shares[
  	data$sector %in% c("Catch Shares", "Catch Shares EM", "Midwater Hake", "LE CA Halibut") & 
  	data$RYEAR > 2011] <- "catch_shares"

  ggplot(data, aes(y = LENGTH, x = RYEAR, group = RYEAR)) + 
	geom_boxplot() + 
    xlab("Year") + ylab(y_lab) +
    facet_wrap(facets = c("gear")) +
    scale_fill_viridis_d()

  ggsave(filename = file.path(dir, paste0(species, "length_by_year_gear.png")),
	width = 14, height = 7)

  ggplot(data, aes(y = LENGTH, x = RYEAR, group = RYEAR)) + 
	geom_boxplot() + 
    xlab("Year") + ylab(y_lab) +
    facet_wrap(facets = c("catch_shares")) +
    scale_fill_viridis_d()

  ggsave(filename = file.path(dir, paste0(species, "length_by_year_catch_share.png")),
	width = 14, height = 7)	

  save(samples_by_year, file = file.path(dir, "samples_by_year.Rdata"))

  return(samples_by_year)
}