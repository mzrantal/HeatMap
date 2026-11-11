# =============================================================================
# Ennusteet vuoden 2029 loppuun (lähdetään skriptistä R/paivita_data.R).
#
# Jokaiselle sarjalle ennustetaan muutos vuoden takaisesta (%) neljänneksittäin
# viimeisimmän havainnon jälkeen kahdella tavalla:
#
# 1. Julkaistu ennuste (data/ennusteoletukset.csv): jos sarjalle on vuoden
#    muutosennuste (VM, RT, pankit), vuoden jäljellä olevat neljännekset
#    asetetaan niin, että koko vuoden muutos on ennusteen mukainen. Jo
#    toteutuneet neljännekset painotetaan edellisen vuoden tasoilla.
# 2. Tilastollinen jatke: vuosina, joille ei ole julkaistua ennustetta,
#    vuosimuutos palautuu edellisen neljänneksen muutoksesta kohti sarjan
#    pitkän aikavälin mediaania (2015-) kertoimella PALAUTUS neljänneksessä.
#    Mediaani rajataan välille TAVOITE_RAJAT, jottei yksittäisen nousu- tai
#    laskukauden trendi jatku loputtomiin.
#
# Tasot lasketaan vuoden takaisesta tasosta ennustetulla muutoksella.
# =============================================================================

ENNUSTE_LOPPU <- "2029Q4"
PALAUTUS <- 0.8           # osuus poikkeamasta, joka säilyy seuraavaan neljännekseen
TAVOITE_RAJAT <- c(-3, 5) # pitkän aikavälin vuosimuutoksen rajat, %

lue_oletukset <- function(polku = "data/ennusteoletukset.csv") {
  if (!file.exists(polku)) return(NULL)
  read.csv(polku, encoding = "UTF-8", stringsAsFactors = FALSE)
}

# Palauttaa sarjan oletukset: data.frame(vuosi, muutos_pct, lahde)
sarjan_oletukset <- function(oletukset, aihe, sarja) {
  if (is.null(oletukset)) return(NULL)
  o <- oletukset[oletukset$aihe == aihe &
                   vapply(oletukset$sarja_regex, function(rx) grepl(rx, sarja), logical(1)), ]
  if (nrow(o) == 0) NULL else o
}

# d: yhden sarjan historia (neljannes, arvo, muutos). Palauttaa ennusterivit.
ennusta_sarja <- function(d, aihe, sarja, oletukset, loppu = ENNUSTE_LOPPU) {
  d <- d[order(d$neljannes), ]
  hist <- d[!is.na(d$muutos), ]
  o <- sarjan_oletukset(oletukset, aihe, sarja)
  if (nrow(hist) == 0 || (nrow(hist) < 4 && is.null(o))) return(NULL)
  viimeinen <- max(hist$neljannes)
  if (viimeinen >= loppu) return(NULL)
  tulevat <- neljannekset_valilla(neljannes_siirto(viimeinen, -1), loppu)

  mu <- if (nrow(hist) >= 8) median(hist$muutos[hist$neljannes >= "2015Q1"], na.rm = TRUE) else 0
  mu <- min(max(mu, TAVOITE_RAJAT[1]), TAVOITE_RAJAT[2])
  m_ed <- mean(tail(hist$muutos, 4))

  # Kaikkien neljännesten tasot ja muutokset (historia + ennuste)
  arvo <- setNames(d$arvo, d$neljannes)
  muutos <- setNames(d$muutos, d$neljannes)
  lahde <- character(0)

  for (q in tulevat) {
    v <- as.integer(substr(q, 1, 4))
    g <- if (!is.null(o)) o$muutos_pct[o$vuosi == v] else numeric(0)
    if (length(g)) {
      # Vuoden neljännekset; painot = edellisen vuoden saman neljänneksen tasot
      vq <- paste0(v, "Q", 1:4)
      w <- arvo[neljannes_siirto(vq, 4)]
      if (anyNA(w) || any(w <= 0)) w <- rep(1, 4)
      tunnettu <- vq %in% names(muutos)[!is.na(muutos)]
      jaljella <- !tunnettu
      m <- (g[1] * sum(w) - sum(w[tunnettu] * muutos[vq[tunnettu]])) / sum(w[jaljella])
      m <- max(min(m, 150), -60)
      sel <- paste0(o$lahde[o$vuosi == v][1], " (vuosi ", v, ": ",
                    formatC(g[1], format = "fg", decimal.mark = ","), " %)")
    } else {
      m <- mu + (m_ed - mu) * PALAUTUS
      sel <- sprintf("Tilastollinen jatke: palautuu kohti pitkän aikavälin mediaania %s %%",
                     formatC(round(mu, 1), format = "fg", decimal.mark = ","))
    }
    muutos[q] <- m
    edellinen <- arvo[neljannes_siirto(q, 4)]
    arvo[q] <- if (length(edellinen) && !is.na(edellinen)) edellinen * (1 + m / 100) else NA
    lahde[q] <- sel
    m_ed <- if (length(g)) g[1] else m   # julkaistun vuoden jälkeen jatketaan vuoden ennusteesta
  }
  data.frame(neljannes = tulevat, arvo = unname(arvo[tulevat]),
             muutos = unname(muutos[tulevat]), ennusteen_lahde = unname(lahde[tulevat]))
}

# Lisää ennusterivit koko aineistoon. Sarjat, joiden nimi on listassa ohita,
# jätetään ennustamatta (esim. vuositiedot ja valmiiksi tulevaisuuteen ulottuvat).
lisaa_ennusteet <- function(data, oletukset, ohita = character(0)) {
  data$ennuste <- FALSE
  data$ennusteen_lahde <- ""
  osat <- split(data, paste(data$aihe, data$sarja, sep = "||"))
  uudet <- lapply(osat, function(d) {
    if (d$sarja[1] %in% ohita) return(NULL)
    e <- ennusta_sarja(d, d$aihe[1], d$sarja[1], oletukset)
    if (is.null(e)) return(NULL)
    data.frame(aihe = d$aihe[1], sarja = d$sarja[1], neljannes = e$neljannes,
               arvo = e$arvo, muutos = e$muutos, yksikko = d$yksikko[1],
               lahde = d$lahde[1], menetelma = d$menetelma[1],
               ennuste = TRUE, ennusteen_lahde = e$ennusteen_lahde)
  })
  rbind(data, do.call(rbind, uudet[!vapply(uudet, is.null, logical(1))]))
}
