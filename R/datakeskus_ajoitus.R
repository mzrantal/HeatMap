# =============================================================================
# Datakeskushankkeiden rakennusajan ajoitus uutisteksteistä
# (lähdetään skriptistä R/paivita_data.R).
#
# Jokaiselle hankkeelle päätellään rakentamisen alkuneljännes ja
# valmistumisneljännes:
#   1. Uutistekstistä (Dataikkunan Info-sarake): esim. "valmistuu syksyllä 2026",
#      "valmistua vuoden 2024 kolmannella neljänneksellä", "rakennustyöt on
#      tarkoitus aloittaa vuoden 2027 ensimmäisellä vuosipuoliskolla",
#      "rakentaminen voisi alkaa ensi vuonna".
#   2. Jos tekstissä ei ole tietoa, käytetään oletuksia:
#      - valmistumisneljännes: valmistumisvuoden Q3 (VALMISTUMIS_OLETUS)
#      - rakennusaika: 6 neljännestä (alle 500 M€) tai 8 neljännestä
#        (vähintään 500 M€) ennen valmistumista
#      - jos hanke on uutisoitu käynnistyneeksi, alku on viimeistään
#        uutisoinnin neljännes
# Investointi jaetaan tasan rakennusajan neljänneksille.
# =============================================================================

VALMISTUMIS_OLETUS <- 3
RAKENNUSAIKA_PIENI <- 6
RAKENNUSAIKA_SUURI <- 8
SUURI_RAJA <- 500   # M€

# Neljänneksen tunnistus tekstistä (kuukaudet ennen vuodenaikoja)
NELJANNES_SANAT <- list(
  "1" = c("ensimmäisellä neljänneksellä", "ensimmäisen neljänneksen", "alkuvuo", "tammikuu",
          "helmikuu", "maaliskuu", "talvella", "q1"),
  "2" = c("toisella neljänneksellä", "toisen neljänneksen", "ensimmäisellä vuosipuoliskolla",
          "ensimmäisen vuosipuoliskon", "keväällä", "kevään", "huhtikuu", "toukokuu", "kesäkuu", "q2"),
  "3" = c("kolmannella neljänneksellä", "kolmannen neljänneksen", "heinäkuu", "elokuu",
          "syyskuu", "kesällä", "kesän", "keskivuo", "q3"),
  "4" = c("neljännellä neljänneksellä", "viimeisellä neljänneksellä", "toisella vuosipuoliskolla",
          "jälkimmäisellä vuosipuoliskolla", "lokakuu", "marraskuu", "joulukuu", "syksyllä",
          "syksyn", "loppuvuo", "vuoden lopussa", "vuoden loppuun", "q4")
)

tunnista_neljannes <- function(lause) {
  for (q in names(NELJANNES_SANAT)) {
    if (any(vapply(NELJANNES_SANAT[[q]], function(s) grepl(s, lause, fixed = TRUE), logical(1))))
      return(as.integer(q))
  }
  NA_integer_
}

# Vuosi lauseesta: nelinumeroinen vuosi tai suhteellinen ilmaus
tunnista_vuosi <- function(lause, uutisvuosi) {
  v <- regmatches(lause, gregexpr("\\b20[2-4][0-9]\\b", lause))[[1]]
  if (length(v)) return(as.integer(v[1]))
  if (grepl("ensi vuon|ensi vuoden", lause)) return(uutisvuosi + 1)
  if (grepl("tänä vuonna|tämän vuoden|kuluvan vuoden|kuluvana vuonna", lause)) return(uutisvuosi)
  NA_integer_
}

lauseet <- function(teksti) {
  if (is.na(teksti) || !nzchar(teksti)) return(character(0))
  x <- unlist(strsplit(tolower(teksti), "(?<=[.!?])\\s+", perl = TRUE))
  x[nzchar(x)]
}

# Palauttaa list(valm_q, alku_v, alku_q, valm_lahde, alku_lahde)
jasenna_ajoitus <- function(teksti, valm_vuosi, uutisvuosi) {
  tulos <- list(valm_q = NA_integer_, alku_v = NA_integer_, alku_q = NA_integer_)
  for (l in lauseet(teksti)) {
    # Valmistumisen ajankohta
    if (is.na(tulos$valm_q) && !is.na(valm_vuosi) &&
        grepl("valmistu|käyttöön|käyttöönot|toiminnassa", l) &&
        !grepl("valmistuttua|valmistuttuaan", l)) {
      v <- tunnista_vuosi(l, uutisvuosi)
      if (is.na(v) || v == valm_vuosi) tulos$valm_q <- tunnista_neljannes(l)
    }
    # Rakentamisen alku
    if (is.na(tulos$alku_v) &&
        grepl("(rakentami\\w*|rakennustyö\\w*|työmaa\\w*)[^.]*(alka|alkaa|alkoi|aloit|käynnisty)", l)) {
      v <- tunnista_vuosi(l, uutisvuosi)
      if (!is.na(v)) {
        tulos$alku_v <- v
        tulos$alku_q <- tunnista_neljannes(l)
      }
    }
  }
  tulos
}

neljannes_numero <- function(v, q) v * 4 + (q - 1)
numero_neljannes <- function(i) paste0(i %/% 4, "Q", i %% 4 + 1)

# Laskee jokaiselle hankkeelle rakennusajan. valmistumisvuosi-vektori voi
# sisältää arvioituja vuosia. Palauttaa data.frame(alku, loppu, ajoitus).
hankkeiden_ajoitus <- function(DI, valmistumisvuosi) {
  uutisvuosi <- as.integer(format(DI$pvm, "%Y"))
  uutis_i <- neljannes_numero(uutisvuosi, (as.integer(format(DI$pvm, "%m")) - 1) %/% 3 + 1)
  kaynnistynyt <- grepl("^3\\.", DI$vaihe)
  out <- lapply(seq_len(nrow(DI)), function(k) {
    vv <- valmistumisvuosi[k]
    if (is.na(vv)) return(data.frame(alku = NA, loppu = NA, ajoitus = NA))
    a <- jasenna_ajoitus(DI$info[k], vv, uutisvuosi[k])
    lahteet <- character(0)
    vq <- if (is.na(a$valm_q)) VALMISTUMIS_OLETUS else { lahteet <- c(lahteet, "valmistumisneljännes uutisesta"); a$valm_q }
    loppu <- neljannes_numero(vv, vq)
    kesto <- if (!is.na(DI$summa_num[k]) && DI$summa_num[k] >= SUURI_RAJA) RAKENNUSAIKA_SUURI else RAKENNUSAIKA_PIENI
    alku <- loppu - kesto + 1
    if (!is.na(a$alku_v) && a$alku_v <= vv) {
      alku <- neljannes_numero(a$alku_v, if (is.na(a$alku_q)) 2 else a$alku_q)
      lahteet <- c(lahteet, "rakentamisen alku uutisesta")
    } else if (kaynnistynyt[k] && uutis_i[k] < alku) {
      alku <- uutis_i[k]
      lahteet <- c(lahteet, "alku käynnistysuutisesta")
    }
    alku <- min(alku, loppu)
    data.frame(alku = alku, loppu = loppu,
               ajoitus = if (length(lahteet)) paste(lahteet, collapse = "; ") else "oletukset")
  })
  do.call(rbind, out)
}

# Neljännessarja: investointi jaetaan tasan rakennusajalle ("jaa") tai
# lasketaan rakenteilla olevien hankkeiden kanta ("kanta").
rakennusaikasarja <- function(ajoitus, rivit, arvo, nelj, tapa = c("jaa", "kanta")) {
  tapa <- match.arg(tapa)
  i_nelj <- neljannes_numero(as.integer(substr(nelj, 1, 4)), as.integer(substr(nelj, 6, 6)))
  tot <- numeric(length(nelj))
  for (k in which(rivit & !is.na(ajoitus$alku) & !is.na(arvo))) {
    mukana <- i_nelj >= ajoitus$alku[k] & i_nelj <= ajoitus$loppu[k]
    n <- ajoitus$loppu[k] - ajoitus$alku[k] + 1
    tot[mukana] <- tot[mukana] + if (tapa == "jaa") arvo[k] / n else arvo[k]
  }
  data.frame(neljannes = nelj, arvo = tot)
}
