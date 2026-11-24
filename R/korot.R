# =============================================================================
# Korkotaso: EKP:n tietopalvelu (ECB Data Portal, SDMX-rajapinta)
# (lähdetään skriptistä R/paivita_data.R).
#
# Haetaan 12 kk euriborin kuukausikeskiarvot (FM).
# Ennuste: data/korko_ennuste.csv sisältää korkotason ennustepisteet
# (neljännes, %). Viimeisimmästä toteutuneesta neljänneksestä pisteiden välille
# interpoloidaan suoraan, ja viimeisen pisteen jälkeen korko pysyy ennallaan.
# Kuukaudet keskiarvoistetaan neljänneksiksi, ja muutos edellisestä
# neljänneksestä lasketaan prosenttiyksikköinä (ei prosentteina).
#
# Onnistunut haku tallennetaan tiedostoon data/korot.csv. Jos rajapintaan ei
# saada yhteyttä, käytetään viimeksi tallennettua aineistoa.
# =============================================================================

KOROT_VALIMUISTI <- "data/korot.csv"
EKP_API <- "https://data-api.ecb.europa.eu/service/data/"
KOROT_SARJAT <- list(
  list(nimi = "12 kk euribor", avain = "FM/M.U2.EUR.RT.MM.EURIBOR1YD_.HSTA")
)
KOROT_ENNUSTE <- "data/korko_ennuste.csv"

ekp_hae <- function(avain) {
  vastaus <- request(paste0(EKP_API, avain)) |>
    req_url_query(format = "csvdata", startPeriod = "2013-01") |>
    req_retry(max_tries = 3) |>
    req_timeout(60) |>
    req_perform()
  d <- read.csv(text = resp_body_string(vastaus), stringsAsFactors = FALSE)
  if (!all(c("TIME_PERIOD", "OBS_VALUE") %in% names(d))) stop("odottamaton vastaus")
  kk <- sub("^(\\d{4})-(\\d{2})$", "\\1M\\2", d$TIME_PERIOD)
  data.frame(kuukausi = kk, arvo = as.numeric(d$OBS_VALUE))
}

korot_paivita <- function() {
  osat <- list()
  for (s in KOROT_SARJAT) {
    d <- tryCatch(ekp_hae(s$avain), error = function(e) {
      message("    Korkohaku epäonnistui (", s$avain, "): ", conditionMessage(e))
      NULL
    })
    if (is.null(d) || nrow(d) == 0) next
    d$sarja <- s$nimi
    message(sprintf("    %s: %d kk (%s–%s)", s$nimi, nrow(d), min(d$kuukausi), max(d$kuukausi)))
    osat[[length(osat) + 1]] <- d[, c("sarja", "kuukausi", "arvo")]
  }
  if (length(osat) == 0) {
    message("  Korkoja ei saatu; käytetään tallennettua aineistoa.")
    return(invisible(FALSE))
  }
  uusi <- do.call(rbind, osat)
  # Epäonnistuneet sarjat säilytetään edellisestä tallennuksesta
  if (file.exists(KOROT_VALIMUISTI)) {
    vanha <- read.csv(KOROT_VALIMUISTI, encoding = "UTF-8", stringsAsFactors = FALSE)
    vanha <- vanha[vanha$sarja %in% vapply(KOROT_SARJAT, `[[`, "", "nimi"), ]
    uusi <- rbind(uusi, vanha[!vanha$sarja %in% uusi$sarja, ])
  }
  uusi <- uusi[!is.na(uusi$arvo), ]
  write.csv(uusi[order(uusi$sarja, uusi$kuukausi), ], KOROT_VALIMUISTI,
            row.names = FALSE, fileEncoding = "UTF-8")
  invisible(TRUE)
}

# Viimeisin toteutunut neljännes ja ennusteen lähde sarjoittain
# (paivita_data.R merkitsee niiden perusteella ennusterivit)
KOROT_VIIMEINEN <- list()
KOROT_ENNUSTE_LAHDE <- list()

# Palauttaa listan neljännessarjoja: arvo = neljänneksen keskikorko (%),
# muutos = muutos edellisestä neljänneksestä prosenttiyksikköinä. Sarjaan lisätään
# ennuste kuluvan vuoden + 2 vuoden loppuun, jos ennustepisteitä on.
korot_neljannessarjat <- function() {
  if (!file.exists(KOROT_VALIMUISTI)) return(list())
  k <- read.csv(KOROT_VALIMUISTI, encoding = "UTF-8", stringsAsFactors = FALSE)
  k <- k[k$sarja %in% vapply(KOROT_SARJAT, `[[`, "", "nimi"), ]
  e <- if (file.exists(KOROT_ENNUSTE))
    read.csv(KOROT_ENNUSTE, encoding = "UTF-8", stringsAsFactors = FALSE) else NULL
  loppu <- paste0(as.integer(format(Sys.Date(), "%Y")) + 2, "Q4")
  numero <- function(q) as.integer(substr(q, 1, 4)) * 4 + as.integer(substr(q, 6, 6)) - 1
  lapply(split(k, k$sarja), function(x) {
    nimi <- x$sarja[1]
    df <- kk_neljannessarjaksi(x$kuukausi, x$arvo, mean)
    df <- df[order(df$neljannes), ]
    viimeinen <- max(df$neljannes)
    KOROT_VIIMEINEN[[nimi]] <<- viimeinen
    pisteet <- if (!is.null(e)) e[e$sarja == nimi & e$neljannes > viimeinen & e$neljannes <= loppu, ] else NULL
    if (!is.null(pisteet) && nrow(pisteet) && viimeinen < loppu) {
      pisteet <- pisteet[order(pisteet$neljannes), ]
      tulevat <- neljannekset_valilla(neljannes_siirto(viimeinen, -1), loppu)
      xs <- c(numero(viimeinen), numero(pisteet$neljannes))
      ys <- c(df$arvo[df$neljannes == viimeinen], pisteet$arvo)
      arvo <- stats::approx(xs, ys, xout = numero(tulevat), rule = 2)$y
      df <- rbind(df, data.frame(neljannes = tulevat, arvo = arvo))
      KOROT_ENNUSTE_LAHDE[[nimi]] <<- paste(unique(pisteet$lahde), collapse = "; ")
    }
    edellinen <- df$arvo[match(neljannes_siirto(df$neljannes, 1), df$neljannes)]   # edellinen neljännes
    df$muutos <- df$arvo - edellinen
    df
  })
}
