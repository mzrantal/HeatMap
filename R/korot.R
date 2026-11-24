# =============================================================================
# Korkotaso: EKP:n tietopalvelu (ECB Data Portal, SDMX-rajapinta)
# (lähdetään skriptistä R/paivita_data.R).
#
# Haetaan kuukausittaiset korot:
#   - 12 kk euribor (FM)
#   - Suomen rahalaitosten uudet asuntolainat kotitalouksille, keskikorko (MIR)
#   - Suomen rahalaitosten uudet lainat yrityksille, keskikorko (MIR)
# Kuukaudet keskiarvoistetaan neljänneksiksi, ja muutos vuoden takaisesta
# lasketaan prosenttiyksikköinä (ei prosentteina).
#
# Onnistunut haku tallennetaan tiedostoon data/korot.csv. Jos rajapintaan ei
# saada yhteyttä, käytetään viimeksi tallennettua aineistoa.
# =============================================================================

KOROT_VALIMUISTI <- "data/korot.csv"
EKP_API <- "https://data-api.ecb.europa.eu/service/data/"
KOROT_SARJAT <- list(
  list(nimi = "12 kk euribor", avain = "FM/M.U2.EUR.RT.MM.EURIBOR1YD_.HSTA"),
  list(nimi = "Uudet asuntolainat: keskikorko", avain = "MIR/M.FI.B.A2C.A.R.A.2250.EUR.N"),
  list(nimi = "Uudet yrityslainat: keskikorko", avain = "MIR/M.FI.B.A2A.A.R.A.2240.EUR.N")
)

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
    uusi <- rbind(uusi, vanha[!vanha$sarja %in% uusi$sarja, ])
  }
  uusi <- uusi[!is.na(uusi$arvo), ]
  write.csv(uusi[order(uusi$sarja, uusi$kuukausi), ], KOROT_VALIMUISTI,
            row.names = FALSE, fileEncoding = "UTF-8")
  invisible(TRUE)
}

# Palauttaa listan neljännessarjoja: arvo = neljänneksen keskikorko (%),
# muutos = muutos vuoden takaisesta prosenttiyksikköinä
korot_neljannessarjat <- function() {
  if (!file.exists(KOROT_VALIMUISTI)) return(list())
  k <- read.csv(KOROT_VALIMUISTI, encoding = "UTF-8", stringsAsFactors = FALSE)
  lapply(split(k, k$sarja), function(x) {
    df <- kk_neljannessarjaksi(x$kuukausi, x$arvo, mean)
    df <- df[order(df$neljannes), ]
    edellinen <- df$arvo[match(neljannes_siirto(df$neljannes, 4), df$neljannes)]
    df$muutos <- df$arvo - edellinen
    df
  })
}
