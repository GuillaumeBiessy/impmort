# HMD----

#' View Countries With HMD Data
#'
#' @return A tibble containing HMD countries
#' @export
get_HMD_countries <- function() {

  countries <- "https://www.mortality.org/cgi-bin/hmd/DataAvailability.php" |>
    rvest::read_html() |>
    rvest::html_nodes("table") |>
    (\(x) x[[2]])() |>
    rvest::html_table(header = TRUE, na.strings = "-")

  colnames(countries)[1:3] <- c("Country", "Code", "Period")

  countries <- countries |>
    dplyr::mutate(Period_start = substr(Period,1,4) |> as.integer(),
                  Period_end = substr(Period,6,9) |> as.integer(),
                  .before = 4) |>
    (\(x) x[,1:5])()

  n_France <- which(countries$Country == "France")
  countries[n_France, - 1] <- countries[n_France + 1, - 1]
  countries <- countries[- (n_France + 1:2),]

  n_Germany <- which(countries$Country == "Germany")
  countries[n_Germany, - 1] <- countries[n_Germany + 1, - 1]
  countries <- countries[- (n_Germany + 1),]

  n_NZ <- which(countries$Country == "New Zealand")
  countries[n_NZ, - 1] <- countries[n_NZ + 1, - 1]
  countries <- countries[- (n_NZ + 1),]
  countries[n_NZ + 1,1] <- "New Zealand Maori"
  countries[n_NZ + 2,1] <- "New Zealand Non-Maori"

  n_UK <- which(countries$Country == "U.K.")
  countries[n_UK, - 1] <- countries[n_UK + 1, - 1]
  countries <- countries[- (n_UK + c(1, 3)),]

  return(countries)
}

read_HMD_data <- function(what, country, username, password, verbose = TRUE) {

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

  data <- data |>
    tibble() |>
    select(- Total) |>
    tidyr::pivot_longer(cols = c("Male", "Female"),
                 names_to = "Gender",
                 values_to = what) |>
    mutate(Age = ifelse(Age == "110+", 110, Age) |> as.integer(),
           Gender = Gender |> forcats::fct_inorder())

  return(data)
}

read_HMD_data2 <- function(country, username, password) {

  out <- list(E = "Exposures", D = "Deaths") |>
    map(read_HMD_data, country, username, password) |>
    imap(tidy_HMD_data) |>
    reduce(left_join)

  return(out)
}

#' Download HMD data
#'
#' @param countries A vector of country names or codes. If empty all data will
#'   be downloaded.
#' @param username a valid HMD user name
#' @param password a valid HMD password
#'
#' @return A tibble containing HMD data for the desired countries
#' @export
get_HMD_data <- function(countries = NULL, username, password) {

  HMD_countries <- get_HMD_countries()

  if (countries |> is.null()) {

    countries_codes <- HMD_countries$Code
    countries_names <- HMD_countries$Country

  } else {

    HMD_lookup <- HMD_countries$Code |> purrr::set_names(HMD_countries$Country)
    HMD_lookup2 <- HMD_countries$Country |> purrr::set_names(HMD_countries$Code)

    countries_codes <- if_else(countries %in% HMD_countries$Code,
                               countries,
                               HMD_lookup[countries])
    countries_names <- if (countries |> names() |> is.null())
      HMD_lookup2[countries_codes] else countries |> names()
  }

  out <- countries_codes |>
    set_names(countries_names) |>
    map(read_HMD_data2, username, password) |>
    imap(~tibble::add_column(.x, Country = .y, .before = 1)) |>
    reduce(bind_rows) |>
    mutate(Country = Country |> forcats::fct_inorder())

  return(out)
}

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

  data <- data |>
    tibble() |>
    mutate(Age = ifelse(Age == "105+", 105, Age) |> as.integer())

  return(data)
}

read_FRD_data2 <- function(chunk, granularity) {

  out <- list(Male = "M", Female = "F") |>
    map(read_FRD_data, chunk, granularity) |>
    map(tidy_FRD_data) |>
    imap(~tibble::add_column(.x, Gender = .y, .before = 1)) |>
    reduce(bind_rows) |>
    mutate(Gender = Gender |> forcats::fct_inorder())

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
    set_names(seq_along(.), .) |>
    map(read_FRD_data2, granularity) |>
    imap(~tibble::add_column(.x, Region = .y, .before = 1)) |>
    reduce(bind_rows) |>
    mutate(Region = Region |> forcats::fct_inorder())

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
           "Cote-Dor",
           "Cotes-Darmor",
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
           "Seine-Saint-Denis",
           "Val-de-Marne",
           "Val-Doise",
           "Paris",
           "Yvelines")

names_chunk <- list(nuts1, nuts2, nuts3)
