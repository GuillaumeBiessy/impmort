# HMD----

read_HMD_data <- function(what, country, username, password, verbose = T) {

  path <- paste0("https://www.mortality.org/hmd/", country, "/STATS/", what, "_1x1.txt")
  userpwd <- paste(username, ":", password, sep = "")
  txt <- RCurl::getURL(path, userpwd = userpwd)
  con <- textConnection(txt)
  data <- try(utils::read.table(con, skip = 2, header = TRUE, na.strings = ".",
                                stringsAsFactors = F), TRUE)
  close(con)
  if(verbose) cat("Retrieving", what, "data for country:", country, "\n")
  if(class(data) == "try-error") stop("Connection error at www.mortality.org.
                                   Please check username, password and country label.")
  return(data)
}

tidy_HMD_data <- function(data, what) {

  data <- data %>%
    tibble %>%
    select(- Total) %>%
    (tidyr::pivot_longer)(cols = c("Male", "Female"),
                 names_to = "Gender",
                 values_to = what) %>%
    mutate(Age = ifelse(Age == "110+", 110, Age) %>% as.integer,
           Gender = Gender %>% (forcats::fct_inorder))

  return(data)
}

read_HMD_data2 <- function(country, username, password) {

  out <- list(E = "Exposures", D = "Deaths") %>%
    map(read_HMD_data, country %>% identify_country, username, password) %>%
    imap(tidy_HMD_data) %>%
    reduce(left_join)

  return(out)
}

#' Download HMD data
#'
#' @param countries A vector of country names. Names, if provided will be used
#'   as Country IDs in the returned tibble.
#' @param username a valid HMD user name
#' @param password a valid HMD password
#'
#' @return A tibble containing HMD data for the desired countries
#' @export
get_HMD_data <- function(countries, username, password) {

  if (countries %>% names %>% is.null) countries <- countries %>% (purrr::set_names)

  out <- countries %>%
    map(read_HMD_data2, username, password) %>%
    imap(~(tibble::add_column)(.x, Country = .y, .before = 1)) %>%
    reduce(bind_rows) %>%
    mutate(Country = Country %>% (forcats::fct_inorder))

  return(out)
}


identify_country <- function(country) {

  if(country %in% names(HMD_lookup)) HMD_lookup[[country]] else country
}

HMD_lookup_en <- c(Australia = "AUS",
                   Austria = "AUT",
                   Belarus = "BLR",
                   Belgium = "BEL",
                   Bulgaria = "BGR",
                   Canada = "CAN",
                   Chile = "CHL",
                   Croatia = "HRV",
                   Czechia = "CZE",
                   Denmark = "DNK",
                   Estonia = "EST",
                   Finland = "FIN",
                   France = "FRATNP",
                   Germany = "DEUTNP",
                   Greece = "GRC",
                   Hong_Kong = "HKG",
                   Hungary = "HUN",
                   Iceland = "ISL",
                   Ireland = "IRL",
                   Israel = "ISR",
                   Italy = "ITA",
                   Japan = "JPN",
                   Latvia = "LVA",
                   Lithuania = "LTU",
                   Luxembourg = "LUX",
                   Netherlands = "NLD",
                   New_Zealand = "NZL_NP",
                   Norway = "NOR",
                   Poland = "POL",
                   Portugal = "PRT",
                   Korea = "KOR",
                   Russia = "RUS",
                   Slovakia = "SVK",
                   Slovenia = "SVN",
                   Spain = "ESP",
                   Sweden = "SWE",
                   Switzerland = "CHE",
                   Taiwan = "TWN",
                   UK = "GBR_NP",
                   USA = "USA",
                   Ukraine = "UKR",
                   West_Germany = "DEUTW",
                   East_Germany = "DEUTE")

HMD_lookup_fr <- c(Australie = "AUS",
                   Autriche = "AUT",
                   Bielorussie = "BLR",
                   Belgique = "BEL",
                   Bulgarie = "BGR",
                   Canada = "CAN",
                   Chili = "CHL",
                   Croatie = "HRV",
                   Tchequie = "CZE",
                   Danemark = "DNK",
                   Estonie = "EST",
                   Finlande = "FIN",
                   France = "FRATNP",
                   Allemagne = "DEUTNP",
                   Grece = "GRC",
                   Hong_Kong = "HKG",
                   Hongrie = "HUN",
                   Islande = "ISL",
                   Irlande = "IRL",
                   Israel = "ISR",
                   Italie = "ITA",
                   Japon = "JPN",
                   Lettonie = "LVA",
                   Lithuanie = "LTU",
                   Luxembourg = "LUX",
                   Pays_Bas = "NLD",
                   Nouvelle_Zelande = "NZL_NP",
                   Norvege = "NOR",
                   Pologne = "POL",
                   Portugal = "PRT",
                   Coree = "KOR",
                   Russie = "RUS",
                   Slovaquie = "SVK",
                   Slovenie = "SVN",
                   Espagne = "ESP",
                   Suede = "SWE",
                   Suisse = "CHE",
                   Taiwan = "TWN",
                   Royaume_Uni = "GBR_NP",
                   Etats_Unis = "USA",
                   Ukraine = "UKR",
                   Allemagne_Ouest = "DEUTW",
                   Allemagne_Est = "DEUTE")

HMD_lookup <- c(HMD_lookup_en, HMD_lookup_fr)

# FRD----

read_FRD_data <- function(gender, chunk, granularity, verbose = T) {

  path <- paste0("https://frdata.org/data/fhmd/1x1-NUTS", granularity, "/", chunk, gender, ".txt")
  data <- try(utils::read.table(path,
                                sep = ";",
                                header = TRUE,
                                na.strings = ".",
                                stringsAsFactors = F,
                                colClasses = c("integer", "character", rep("double", 8))), TRUE)
  if(verbose) cat("Retrieving", gender, "data for chunk:", chunk, "\n")
  if(class(data) == "try-error") stop("Connection error at frdata.org.")
  return(data)
}

tidy_FRD_data <- function(data) {

  data <- data %>%
    tibble %>%
    select(Year, Age, dx, Lx) %>%
    rename(E = Lx, D = dx) %>%
    mutate(Age = ifelse(Age == "105+", 105, Age) %>% as.integer)

  return(data)
}

read_FRD_data2 <- function(chunk, granularity) {

  out <- list(Male = "M", Female = "F") %>%
    map(read_FRD_data, chunk, granularity) %>%
    map(tidy_FRD_data) %>%
    imap(~(tibble::add_column)(.x, Gender = .y, .before = 1)) %>%
    reduce(bind_rows) %>%
    mutate(Gender = Gender %>% (forcats::fct_inorder))

  return(out)
}

#' Download FRD data
#'
#' @param granularity Numeric. The desired granularity for the ouput, 1 for new regions,
#' 2 for older regions and 3 for department.
#'
#' @return A tibble containing FRD data for the desired granularity
#' @export
get_FRD_data <- function(granularity = 1) {

  out <- names_chunk[[granularity]] %>%
    set_names(seq_along(.), .) %>%
    map(read_FRD_data2, granularity) %>%
    imap(~(tibble::add_column)(.x, Region = .y, .before = 1)) %>%
    reduce(bind_rows) %>%
    mutate(Region = Region %>% (forcats::fct_inorder))

  return(out)
}

nuts1 <- c("Ile de France",
           "Centre-Val de Loire",
           "Bourgogne-Franche-Comte",
           "Normandie",
           "Hauts-de-France",
           "Grand Est",
           "Pays de la Loire",
           "Bretagne",
           "Nouvelle-Aquitaine",
           "Occitanie",
           "Auvergne-Rhone-Alpes",
           "Provence-Alpes-Cote d Azur",
           "Corse")

nuts2 <- c("Ile de France",
           "Centre-Val de Loire",
           "Bourgogne",
           "Franche-Comte",
           "Basse-Normandie",
           "Haute-Normandie",
           "Nord-Pas-de-Calais",
           "Picardie",
           "Alsace",
           "Champagne-Ardenne",
           "Lorraine",
           "Pays de la Loire",
           "Bretagne",
           "Aquitaine",
           "Limousin",
           "Poitou-Charentes",
           "Languedoc-Roussillon",
           "Midi-Pyrenees",
           "Auvergne",
           "Rhone-Alpes",
           "Provence-Alpes-Cote d Azur",
           "Corse")

nuts3 <- c("Ain",
           "Aisne",
           "Allier",
           "Alpes-de-Haute-Provence",
           "Hautes-Alpes",
           "Alpes-Maritimes",
           "Ardeche",
           "Ardennes",
           "Ariege",
           "Aube",
           "Aude",
           "Aveyron",
           "Bouches-du-Rhone",
           "Calvados",
           "Cantal",
           "Charente",
           "Charente-Maritime",
           "Cher",
           "Correze",
           "Corse",
           "Cote-d Or",
           "Cotes d Armor",
           "Creuse",
           "Dordogne",
           "Doubs",
           "Drome",
           "Eure",
           "Eure-et-Loir",
           "Finistere",
           "Gard",
           "Haute-Garonne",
           "Gers",
           "Gironde",
           "Herault",
           "Ille-et-Vilaine",
           "Indre",
           "Indre-et-Loire",
           "Isere",
           "Jura",
           "Landes",
           "Loir-et-Cher",
           "Loire",
           "Haute-Loire",
           "Loire-Atlantique",
           "Loiret",
           "Lot",
           "Lot-et-Garonne",
           "Lozere",
           "Maine-et-Loire",
           "Manche",
           "Marne",
           "Haute-Marne",
           "Mayenne",
           "Meurthe-et-Moselle",
           "Meuse",
           "Morbihan",
           "Moselle",
           "Nievre",
           "Nord",
           "Oise",
           "Orne",
           "Pas-de-Calais",
           "Puy-de-Dome",
           "Pyrenees-Atlantiques",
           "Hautes-Pyrenees",
           "Pyrenees-Orientales",
           "Bas-Rhin",
           "Haut-Rhin",
           "Rhone",
           "Haute-Saone",
           "Saone-et-Loire",
           "Sarthe",
           "Savoie",
           "Haute-Savoie",
           "Seine",
           "Seine-Maritime",
           "Seine-et-Marne",
           "Seine-et-Oise",
           "Deux-Sevres",
           "Somme",
           "Tarn",
           "Tarn-et-Garonne",
           "Var",
           "Vaucluse",
           "Vendee",
           "Vienne",
           "Haute-Vienne",
           "Vosges",
           "Yonne",
           "Territoire de Belfort",
           "Essonne",
           "Hauts-de-Seine",
           "Seine-St-Denis",
           "Val-de-Marne",
           "Val-D Oise",
           "Paris",
           "Yvelines")

names_chunk <- list(nuts1, nuts2, nuts3)
