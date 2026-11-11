# =============================================================================
# Tullin Uljas-tietokanta: toimistokoneiden ja atk-laitteiden tuonti
# (lähdetään skriptistä R/paivita_data.R).
#
# Haetaan SITC-luokituksen ryhmän 75 "Toimistokoneet ja atk-laitteet" ja sen
# alaryhmän 752 "Atk-laitteet" tuonnin arvo (euroa) kuukausittain koko maasta
# kaikista maista. Datakeskusten palvelimet ja muut IT-laitteet tuodaan
# pääosin ulkomailta, joten tuonnin kasvu kuvaa datakeskusten
# laiteinvestointeja.
#
# Haku käyttää R-pakettia uljas (https://github.com/jhuovari/uljas).
# Onnistunut haku tallennetaan tiedostoon data/uljas_tuonti.csv. Jos
# rajapintaan ei saada yhteyttä (esim. paikallinen ajo rajatussa verkossa),
# käytetään viimeksi tallennettua aineistoa.
# =============================================================================

ULJAS_VALIMUISTI <- "data/uljas_tuonti.csv"
# Pitkät sarjat ovat kuutiossa ULJAS_SITC2, vuodesta 2025 alkaen myös ULJAS_SITC
ULJAS_KUUTIOT <- c("/DATABASE/01 ULKOMAANKAUPPATILASTOT/02 SITC/ULJAS_SITC2",
                   "/DATABASE/01 ULKOMAANKAUPPATILASTOT/02 SITC/ULJAS_SITC")
ULJAS_TUOTTEET <- list(
  list(taso = "SITC2", koodi = "75", nimi = "Toimistokoneet ja atk-laitteet (SITC 75)"),
  list(taso = "SITC3", koodi = "752", nimi = "Atk-laitteet (SITC 752)")
)

# Hakee yhden tuoteryhmän kuukausittaisen tuonnin yhdestä kuutiosta
uljas_hae_tuote <- function(ifile, taso, koodi) {
  dims <- uljas::uljas_dims(ifile = ifile)
  nimet <- unlist(lapply(dims, function(d) as.character(d$label)))
  tuote <- grep(paste0("Products ", taso, "$"), nimet, value = TRUE)[1]
  if (is.na(tuote)) stop("Luokitusta ", taso, " ei löydy kuutiosta ", ifile)
  luokittimet <- list(tuote = koodi, aika = "=ALL", Flow = 1,
                      Country = "=FIRST 1",   # ensimmäinen arvo = kaikki maat yhteensä
                      Indicators = "V1")      # arvo euroina
  names(luokittimet)[1:2] <- c(tuote, "Time period")
  d <- uljas::uljas_data(ifile = ifile, classifiers = luokittimet)
  d <- as.data.frame(d)
  message(sprintf("    %s %s: maa '%s', %d riviä", basename(ifile), koodi,
                  as.character(d$Country[1]), nrow(d)))
  aika <- as.character(d$`Time period`)
  kk <- grepl("^[0-9]{6}$", aika)          # kuukaudet muodossa 202606
  data.frame(kuukausi = paste0(substr(aika[kk], 1, 4), "M", substr(aika[kk], 5, 6)),
             arvo = as.numeric(d$values[kk]))
}

uljas_paivita <- function() {
  if (!requireNamespace("uljas", quietly = TRUE)) {
    message("  Paketti uljas puuttuu; käytetään tallennettua tuontiaineistoa.")
    return(invisible(FALSE))
  }
  osat <- list()
  for (t in ULJAS_TUOTTEET) {
    sarjat <- list()
    for (f in ULJAS_KUUTIOT) {
      s <- tryCatch(uljas_hae_tuote(f, t$taso, t$koodi), error = function(e) {
        message("    Uljas-haku epäonnistui (", basename(f), " ", t$koodi, "): ", conditionMessage(e))
        NULL
      })
      if (!is.null(s)) sarjat[[length(sarjat) + 1]] <- s
    }
    if (length(sarjat) == 0) next
    s <- do.call(rbind, sarjat)
    s <- s[!duplicated(s$kuukausi, fromLast = TRUE), ]   # uudempi kuutio ensisijainen
    s$sarja <- t$nimi
    osat[[length(osat) + 1]] <- s
  }
  if (length(osat) == 0) return(invisible(FALSE))
  tulos <- do.call(rbind, osat)
  tulos <- tulos[!is.na(tulos$arvo), c("sarja", "kuukausi", "arvo")]
  tulos <- tulos[order(tulos$sarja, tulos$kuukausi), ]
  write.csv(tulos, ULJAS_VALIMUISTI, row.names = FALSE, fileEncoding = "UTF-8")
  message(sprintf("  Uljas: tallennettu %d kuukausiarvoa (%s–%s)", nrow(tulos),
                  min(tulos$kuukausi), max(tulos$kuukausi)))
  invisible(TRUE)
}

# Palauttaa listan neljännessarjoja (M€) sarjan nimen mukaan
uljas_neljannessarjat <- function() {
  if (!file.exists(ULJAS_VALIMUISTI)) return(list())
  u <- read.csv(ULJAS_VALIMUISTI, encoding = "UTF-8", stringsAsFactors = FALSE)
  lapply(split(u, u$sarja), function(x) {
    tasosta(kk_neljannessarjaksi(x$kuukausi, x$arvo / 1e6, sum))
  })
}
