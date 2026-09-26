# R script for "carob"
# license: GPL (>=3)

## ISSUES

carob_script <- function(path) {

"
Enhancing farmer's access to technology  for varietal demonstration trial ATASP-1 increased Sorghum productivity in the selected staple crop processing zones

The Agricultural Transformation Agenda Support Program Phase 1 (AT ASP-1) of the Federal Government of Nigeria was launched m 2015 as a follow up to the previous. Agricultural  Transformation Agenda (ATA). It is expected to create in 120.000jobs along the value chain of priority commodities and add additional 20 million metric tons of food. Project activities included thematic training, on-farm technology  demonstrations, community seed production and formation of Innovation Platforms for market linkages. The project has made remarkable progress in enhancing access to quality seeds and other inputs to over 34.300 farmers while expanding knowledge of best-bet productron technologies in over 100 communities across three staple crop processing zones (SCPZ). Duiing the 2016 cropping season, farmers produced over 70,268 Mt of grains valued at £i9.135billion (US$29M). The use of improved varieties increased yields by 32%. 42% and 64% in Bida-Badeggi. Kano-Jigawa and Sokoto-Kebbi SCPZ. respectively. Seed dressing increased yields by 38%. 27%. and 30% m the three SCPZs respectively, while tillage practices increased yields by 20% and 55% m Kano - Jigawa and Sokoto - Kebbi SCPZs. Through Innovation Platforms set up with other stakeholders and market linkages to large scale processors, 109.76 tons of seeds were procured and planted. Average yield obtained on the improved technologies was 1.5 tha compared to 1 1 t/ha by other farmers giving a 40% increase. A total of 1,093 women farmers comprising of about 34.2% of the total number of participating farmers benefited directly from the project. Seed fairs, rural radios and audio-visual broadcasts on improved sorghum production technologies were used to reach non-participating farmers within the zones.        Experiment location on Google Map-Department of Meteorology and Climate Science    

      Experiment location on Google Maps-Federal University of Technology Akure(FUTA)
"

	uri <- "doi:10.21421/D2/SJQNAI"
	group <- "varieties"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "ICRISAT",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield", 
		notes = NA,
		carob_contributor = "Blessing Dzuda",
		carob_date = "2026-09-26",
		carob_completion = 100,	
		carob_effort = 7
	)
	
	f1 <- ff[basename(ff) == "Data file of ICRISAT on Varietal demonstration trial ATASP-1.xlsx"]

	r <- carobiner::read.excel(f1)

	d <- data.frame(
	  country="Nigeria",
	  adm1=r$State,
	  adm2=r$LGA,
	  adm3=r$Community,
	  crop="sorghum",
	  treatment=r$Treatment,
	  variety=r$Treatment,
	  yield=r$GHvYld_C_kgha,
	  yield_part="grain",
	  yield_moisture=NA,
	  yield_isfresh=NA,
	  on_farm=FALSE,
	  is_survey=FALSE, 
	  irrigated=FALSE,
	  geo_from_source=FALSE,
	  planting_date=NA,
	  harvest_date=NA
	)
	
  d$P_fertilizer <- d$K_fertilizer <- d$N_fertilizer <- as.numeric(NA)
	
	adm3_lookup <- c(
	  "Rurum T/Gari"="Rurum Tsohon Gari",
	  "Rurum S/Gari"="Rurum Sabon Gari",
	  "M/Madori"="Malam Madori",
	  "G/Kwano"="Gidan Kwano"
	)
	
	d$adm3[d$adm3 %in% names(adm3_lookup)] <- adm3_lookup[d$adm3[d$adm3 %in% names(adm3_lookup)]]
  d$adm1 <- tools::toTitleCase(tolower(d$adm1))	
  
  adm2_lookup <- c(
    "G/Mallam"="Garum Mallam",
    "M/Madori"="Malam Madori"
  )
  
  d$adm2[d$adm2 %in% names(adm2_lookup)] <- adm2_lookup[d$adm2[d$adm2 %in% names(adm2_lookup)]]
  
  #ADDING LOCATION DATA
  #1. GADM does not have adm3 level for Nigeria
  #2. For adm3 places that were more common, i used the Nominatim service, then for the hits left, i used Geonames
  #3. For the remaining places not in Nominatim and Geonames i resorted to GADM level 2 to complete near accurate georeferencing
  
  geo <- data.frame(
    adm3 = c(
      "Auyo", "Ayama", "Dunari", "Fagam", "Farin Dutse", "Gagulmari", "Gamsarka", "Kila", "Makaddari", "Malam Madori", "Ruba", "Rumfa", "Sara", "Yankoli", "Zandam",
      "Bebeji", "Dalili", "Damau", "Dan Hassan", "Dawakin Kudu", "Gafan", "Gurjiya", "Gwarmai", "Kofa", "Rurum Tsohon Gari", "Saji", "Unguwar Duniya", "Wak", "Yadakwari", "Yalwa",
      "Zurgu", "Buya", "Fana", "Geza", "Gorun Yamma", "Kamba", "Kambuwa", "Kwakware", "Kyangakwai", "Libata", "Ngaski", "Sawashi", "Shanga", "Takalafiya", "Utono",
      "Gbangba", "Kutigi", "Lanle", "Makusidi", "Durbawa", "Hamma Ali", "Kware", "Gamahuwai", "Atafi 1", "Atafi 2", "Sarawa", "Shamakeri", "Mai Rakumi", "Shayya", "Tonikutara",
      "Tamburawa Zango", "Tamburawa Tambari", "Fankurin", "Rurum Sabon Gari", "Karallaje", "Koaoje", "Gidan Kwano", "Giron Masa", "Duguraha", "Fana Sabo", "Sabuwar Tunga", "Kwandage", "Kutirko", "Nankokan", "Magaji",
      "Shabalegbo", "Somazhiko", "Mantuntu", "Rugachibo", "Ndayako", "Kpaki", "Tungankawu"
    ),
    latitude = c(
      12.3395, 12.26713, 12.59855, 11.04851, 11.17843, 12.45572, 12.31283, 11.30876, 12.49612, 12.5276, 12.0197, 12.4504, 11.30516, 12.44872, 11.30447,
      11.6828, 11.77408, 11.6818, 11.78504, 11.83228, 11.6753, 11.7501, 11.5313, 11.5549, 11.4176, 11.4619, 11.8778, 11.5921, 11.69748, 11.4855,
      11.54765, 11.07551, 11.69324, 12.01238, 11.97263, 11.85172, 10.91329, 12.89691, 11.9692, 10.14951, 10.37475, 11.09381, 11.20466, 10.98333, 10.58367,
      9.40824, 9.42786, 9.22721, 9.57538, 13.0639, 13.16667, 13.21132, 12.3155, 12.4271, 12.4271, 12.1324, 12.1324, 12.5263, 12.5263, 12.5263,
      11.7965, 11.7965, 11.6469, 11.4665, 11.3169, 11.3169, 10.5439, 11.2084, 11.2084, 11.7707, 11.7707, 11.7707, 8.9306, 8.9306, 8.9306,
      9.2846, 9.2846, 9.1167, 9.2735, 9.2437, 9.2437, 9.6762
    ),
    longitude = c(
      9.93575, 9.86975, 9.89019, 9.97915, 9.9177, 10.0418, 9.85911, 9.76966, 9.82247, 9.88986, 9.86667, 10.0449, 9.68904, 10.06629, 9.84625,
      8.2448, 8.40969, 8.3033, 8.53028, 8.69164, 8.4493, 8.5553, 8.2573, 8.2635, 8.4817, 8.5872, 8.6283, 8.3579, 8.41032, 8.1853,
      8.52853, 4.06292, 3.90533, 3.87842, 3.69925, 3.65478, 4.96962, 4.32385, 3.74786, 4.59317, 4.83074, 4.69478, 4.64921, 4.8, 4.67388,
      6.12915, 6.18114, 5.64641, 6.14659, 5.3221, 5.31667, 5.26721, 9.9969, 10.0311, 10.0311, 10.0102, 10.0102, 9.9821, 9.9821, 9.9821,
      8.6459, 8.6459, 8.373, 8.4988, 3.9641, 3.9641, 4.7743, 4.7267, 4.7267, 4.1363, 4.1363, 4.1363, 6.4082, 6.4082, 6.4082,
      5.9861, 5.9861, 6.2422, 5.6833, 5.1464, 5.1464, 5.9333
    ),
    geo_source = c(
      "geonames", "geonames", "geonames", "geonames", "geonames", "geonames", "geonames", "geonames", "geonames", "geonames", "geonames", "geonames", "geonames", "geonames", "geonames",
      "nominatim", "geonames", "nominatim", "geonames", "geonames", "nominatim", "nominatim", "nominatim", "nominatim", "nominatim", "nominatim", "nominatim", "nominatim", "geonames", "nominatim",
      "geonames", "geonames", "geonames", "geonames", "geonames", "geonames", "geonames", "geonames", "geonames", "geonames", "geonames", "geonames", "geonames", "geonames", "geonames",
      "geonames", "geonames", "geonames", "geonames", "geonames", "geonames", "geonames", "GADM 4.1, adm2", "GADM 4.1, adm2", "GADM 4.1, adm2", "GADM 4.1, adm2", "GADM 4.1, adm2", "GADM 4.1, adm2", 
      "GADM 4.1, adm2", "GADM 4.1, adm2", "GADM 4.1, adm2", "GADM 4.1, adm2", "GADM 4.1, adm2", "GADM 4.1, adm2", "GADM 4.1, adm2", "GADM 4.1, adm2", "GADM 4.1, adm2", "GADM 4.1, adm2", "GADM 4.1, adm2",
      "GADM 4.1, adm2", "GADM 4.1, adm2", "GADM 4.1, adm2", "GADM 4.1, adm2", "GADM 4.1, adm2", "GADM 4.1, adm2", "GADM 4.1, adm2", "GADM 4.1, adm2", "GADM 4.1, adm2", "GADM 4.1, adm2", "GADM 4.1, adm2", "GADM 4.1, adm2", "GADM 4.1, adm2"
    ),
    geo_uncertainty = c(
      NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
      NA, NA, NA, NA, NA, NA, NA, 32584, 4913, 4913, 27090, 27090, 25498, 25498, 25498, 17536, 17536, 13222, 20758, 56649, 56649, 62519, 39141, 39141, 41146, 41146, 41146, 40982, 40982, 40982,
      39850, 39850, 55993, 75197, 87356, 87356, 40941
    ))                
  
  d <- merge(d,geo,by="adm3",all.x = TRUE)
  
  d$trial_id <- paste(d$adm3,seq(nrow(d)),sep = "_")

  carobiner::write_files(path, meta, d)
} 

