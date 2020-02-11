# Helper functions

country_continent <- function(carnames, data) {
  for (i in 1:length(carnames)) {
    s <- tolower(carnames[i])
    if (startsWith(s, "alfa-romero")) {
      data[i, "car"] <- "alfa-romero"
      data[i, "country"] <- "italy"
      data[i, "continent"] <- "europe"
    } else if (startsWith(s, "audi")) {
      data[i, "car"] <- "audi"
      data[i, "country"] <- "germany"
      data[i, "continent"] <- "europe"
    } else if (startsWith(s, "bmw")) {
      data[i, "car"] <- "bmw"
      data[i, "country"] <- "germany"
      data[i, "continent"] <- "europe"
    } else if (startsWith(s, "chevrolet")) {
      data[i, "car"] <- "chevrolet"
      data[i, "country"] <- "usa"
      data[i, "continent"] <- "usa"
    } else if (startsWith(s, "dodge")) {
      data[i, "car"] <- "dodge"
      data[i, "country"] <- "usa"
      data[i, "continent"] <- "usa"
    } else if (startsWith(s, "honda")) {
      data[i, "car"] <- "honda"
      data[i, "country"] <- "japan"
      data[i, "continent"] <- "asia"
    } else if (startsWith(s, "isuzu")) {
      data[i, "car"] <- "isuzu"
      data[i, "country"] <- "japan"
      data[i, "continent"] <- "asia"
    } else if (startsWith(s, "jaguar")) {
      data[i, "car"] <- "jaguar"
      data[i, "country"] <- "uk"
      data[i, "continent"] <- "europe"
    } else if (startsWith(s, "maxda") || startsWith(s, "mazda")) {
      data[i, "car"] <- "mazda"
      data[i, "country"] <- "japan"
      data[i, "continent"] <- "asia"
    } else if (startsWith(s, "buick")) {
      data[i, "car"] <- "buick"
      data[i, "country"] <- "usa"
      data[i, "continent"] <- "usa"
    } else if (startsWith(s, "mercury")) {
      data[i, "car"] <- "mercury"
      data[i, "country"] <- "usa"
      data[i, "continent"] <- "usa"
    } else if (startsWith(s, "mitsubishi")) {
      data[i, "car"] <- "mitsubishi"
      data[i, "country"] <- "japan"
      data[i, "continent"] <- "asia"
    } else if (startsWith(s, "nissan")) {
      data[i, "car"] <- "nissan"
      data[i, "country"] <- "japan"
      data[i, "continent"] <- "asia"
    } else if (startsWith(s, "peugeot")) {
      data[i, "car"] <- "peugeot"
      data[i, "country"] <- "france"
      data[i, "continent"] <- "europe"
    } else if (startsWith(s, "plymouth")) {
      data[i, "car"] <- "plymouth"
      data[i, "country"] <- "usa"
      data[i, "continent"] <- "usa"
    } else if (startsWith(s, "porsche") || startsWith(s, "porcshce")) {
      data[i, "car"] <- "porsche"
      data[i, "country"] <- "germany"
      data[i, "continent"] <- "europe"
    } else if (startsWith(s, "renault")) {
      data[i, "car"] <- "renault"
      data[i, "country"] <- "france"
      data[i, "continent"] <- "europe"
    } else if (startsWith(s, "saab")) {
      data[i, "car"] <- "saab"
      data[i, "country"] <- "sweden"
      data[i, "continent"] <- "europe"
    } else if (startsWith(s, "subaru")) {
      data[i, "car"] <- "subaru"
      data[i, "country"] <- "japan"
      data[i, "continent"] <- "asia"
    } else if (startsWith(s, "toyota") || startsWith(s, "toyouta")) {
      data[i, "car"] <- "toyota"
      data[i, "country"] <- "japan"
      data[i, "continent"] <- "asia"
    } else if (startsWith(s, "vokswagen") || startsWith(s, "vw") || startsWith(s, "volkswagen")) {
      data[i, "car"] <- "vw"
      data[i, "country"] <- "germany"
      data[i, "continent"] <- "europe"
    } else if (startsWith(s, "volvo")) {
      data[i, "car"] <- "volvo"
      data[i, "country"] <- "sweden"
      data[i, "continent"] <- "europe"
    }
  }
  return(data)
}

  