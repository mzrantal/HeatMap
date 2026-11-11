# =============================================================================
# Päivittää heatmap-sovelluksen aineiston.
#
# Hakee Tilastokeskuksen StatFin-tietokannasta (PxWeb-rajapinta) neljännesvuosi-
# sarjat ja laskee jokaiselle neljännekselle muutoksen vuoden takaisesta (%).
# Datakeskusten uutisaineisto luetaan kansion data/ uusimmasta
# Dataikkuna*.xlsx-tiedostosta.
#
# Ajo repositorion juuresta:
#   Rscript R/paivita_data.R
#
# Tulos: app/data/heatmap.csv ja app/data/paivitetty.txt
# Paketit: httr2, jsonlite, readxl
# =============================================================================

suppressPackageStartupMessages({
  library(httr2)
  library(jsonlite)
  library(readxl)
})

PX_API <- "https://pxdata.stat.fi/PxWeb/api/v1/fi/StatFin/"
ALKU <- "2015Q1"          # Aikaisin tallennettava neljännes
ULOS <- "app/data"

# -----------------------------------------------------------------------------
# PxWeb-apufunktiot
# -----------------------------------------------------------------------------

# Hakee taulukon valitut solut. valinnat: nimetty lista muuttujakoodi -> arvot.
# Aikamuuttujaa ei tarvitse antaa: silloin haetaan kaikki ajankohdat.
px_hae <- function(taulu, valinnat) {
  kysely <- lapply(names(valinnat), function(k) {
    list(code = k, selection = list(filter = "item", values = I(valinnat[[k]])))
  })
  runko <- list(query = kysely, response = list(format = "json-stat2"))
  vastaus <- request(paste0(PX_API, taulu)) |>
    req_body_json(runko, auto_unbox = TRUE) |>
    req_retry(max_tries = 5, backoff = function(i) 2^i) |>
    req_throttle(rate = 20 / 10) |>   # rajapinta sallii n. 30 kyselyä / 10 s
    req_perform()
  js <- resp_body_json(vastaus, simplifyVector = TRUE)

  # json-stat2: viimeinen ulottuvuus vaihtuu nopeimmin, expand.grid päinvastoin
  ulot <- js$id
  koodit <- lapply(ulot, function(u) {
    ind <- js$dimension[[u]]$category$index
    if (is.list(ind)) names(sort(unlist(ind))) else ind
  })
  names(koodit) <- ulot
  ruudukko <- expand.grid(rev(koodit), stringsAsFactors = FALSE)[, rev(ulot), drop = FALSE]
  arvot <- js$value
  arvot[vapply(arvot, is.null, logical(1))] <- NA
  ruudukko$arvo <- as.numeric(unlist(arvot))
  ruudukko
}

# Kuukausi (2024M03) -> neljännes (2024Q1)
kk_neljannekseksi <- function(kk) {
  v <- substr(kk, 1, 4)
  m <- as.integer(substr(kk, 6, 7))
  paste0(v, "Q", (m - 1) %/% 3 + 1)
}

# Kuukausisarja neljännessarjaksi. Vain täydet neljännekset (3 kk) otetaan mukaan.
kk_neljannessarjaksi <- function(kk, arvo, fun = sum) {
  q <- kk_neljannekseksi(kk)
  ok <- !is.na(arvo)
  tulos <- aggregate(arvo[ok], list(neljannes = q[ok]), fun)
  n <- aggregate(arvo[ok], list(neljannes = q[ok]), length)
  tulos <- tulos[n$x == 3, ]
  data.frame(neljannes = tulos$neljannes, arvo = tulos$x)
}

# Siirtää neljännestä k neljännestä taaksepäin
neljannes_siirto <- function(q, k = 4) {
  v <- as.integer(substr(q, 1, 4))
  n <- as.integer(substr(q, 6, 6))
  i <- v * 4 + (n - 1) - k
  paste0(i %/% 4, "Q", i %% 4 + 1)
}

# Laskee muutoksen vuoden takaisesta (%) tasosarjasta
vuosimuutos <- function(df) {
  edellinen <- df$arvo[match(neljannes_siirto(df$neljannes, 4), df$neljannes)]
  m <- 100 * (df$arvo / edellinen - 1)
  m[!is.finite(m)] <- NA
  m
}

# Kaikki neljännekset välillä a..b
neljannekset_valilla <- function(a, b) {
  va <- as.integer(substr(a, 1, 4)) * 4 + as.integer(substr(a, 6, 6)) - 1
  vb <- as.integer(substr(b, 1, 4)) * 4 + as.integer(substr(b, 6, 6)) - 1
  i <- va:vb
  paste0(i %/% 4, "Q", i %% 4 + 1)
}

# -----------------------------------------------------------------------------
# Sarjojen rakentajat. Jokainen palauttaa data.frame(neljannes, arvo, muutos).
# -----------------------------------------------------------------------------

# Tasosarjasta lasketaan vuosimuutos
tasosta <- function(df) {
  df <- df[order(df$neljannes), ]
  df$muutos <- vuosimuutos(df)
  df
}

# Kansantalouden tilinpito (ntp/132h): valmis volyymin vuosimuutos alkuperäisestä
# sarjasta sekä käypähintainen taso (M€)
ntp_sarjat <- function(taloustoimet) {
  d <- px_hae("ntp/132h.px", list(
    taloustoimi_1_20180101 = taloustoimet,
    contentscode = c("tasmcp", "vol_vv_tasmvv2015")))
  d <- d[d$timeperiod_q >= ALKU, ]
  lapply(setNames(taloustoimet, taloustoimet), function(tt) {
    x <- d[d$taloustoimi_1_20180101 == tt, ]
    taso <- x[x$contentscode == "tasmcp", ]
    muut <- x[x$contentscode == "vol_vv_tasmvv2015", ]
    data.frame(neljannes = taso$timeperiod_q, arvo = taso$arvo,
               muutos = muut$arvo[match(taso$timeperiod_q, muut$timeperiod_q)])
  })
}

# Rakennusten uudistuotanto (raku/156f), kuukausisummat neljänneksiksi
raku_sarja <- function(raku, vaihe, luokka, tieto) {
  x <- raku[raku$rakennusvaihe_1_20250101 == vaihe &
              raku$rakennus_6_20180101 == luokka &
              raku$contentscode == tieto, ]
  tasosta(kk_neljannessarjaksi(x$timeperiod_m, x$arvo, sum))
}

# Uudisrakentamisen volyymi-indeksi (raku/156g), neljänneksen keskiarvo
urvi_sarja <- function(urvi, luokka) {
  x <- urvi[urvi$rakennus_6_20180101 == luokka, ]
  tasosta(kk_neljannessarjaksi(x$timeperiod_m, x$arvo, mean))
}

# Rakentamisen liikevaihtokuvaaja (rlv/14m1): valmis vuosimuutos, alkuperäinen sarja
rlv_sarja <- function(rlv, toimiala, muuttuja) {
  taso <- rlv[rlv$nace_8_20180101 == toimiala & rlv$luokkaryhma_2_20180301 == muuttuja &
                rlv$contentscode == "rlv-alkuperainen", ]
  muut <- rlv[rlv$nace_8_20180101 == toimiala & rlv$luokkaryhma_2_20180301 == muuttuja &
                rlv$contentscode == "rlv-muutosprosentti", ]
  data.frame(neljannes = taso$timeperiod_q, arvo = taso$arvo,
             muutos = muut$arvo[match(taso$timeperiod_q, muut$timeperiod_q)])
}

# -----------------------------------------------------------------------------
# 1. Haetaan Tilastokeskuksen aineistot
# -----------------------------------------------------------------------------
message("Haetaan kansantalouden tilinpito (ntp/132h)...")
NTP <- ntp_sarjat(c(
  "P51K", "P51KXS13", "P51KS13",
  "P51KN111", "P51KN1121", "P51KN1122_N1123",
  "P51KN113_N114", "P51KN115_N117"))

message("Haetaan rakennusten ja asuntojen uudistuotanto (raku/156f)...")
RAKU <- px_hae("raku/156f.px", list(
  rakennusvaihe_1_20250101 = c("1", "2", "3"),
  alue_23_20260101 = "SSS",
  rakennus_6_20180101 = c("01", "011", "012", "02T19", "03T05", "06T08_13", "09T12"),
  contentscode = c("raku-uusiAsuntoLkm", "raku-tilavuusToimenpide")))
RAKU <- RAKU[RAKU$timeperiod_m >= "2014M01", ]

message("Haetaan uudisrakentamisen volyymi-indeksi (raku/156g)...")
URVI <- px_hae("raku/156g.px", list(
  rakennus_6_20180101 = c("01", "02T19"),
  contentscode = "urvi2020"))

message("Haetaan rakentamisen liikevaihtokuvaaja (rlv/14m1)...")
RLV <- px_hae("rlv/14m1.px", list(
  nace_8_20180101 = c("F41", "F42"),
  luokkaryhma_2_20180301 = c("lv", "mi"),
  contentscode = c("rlv-alkuperainen", "rlv-muutosprosentti")))

message("Haetaan vanhojen osakeasuntojen hinnat ja kaupat (ashi)...")
# Hintaindeksi: ketjutettu neljännesvuosi-indeksi 2015=100 (sarja alkaa 2015)
ASHI_HINTA <- px_hae("ashi/15it.px", list(
  alue_43_20260625 = c("SSS", "pks", "msu"),
  talotyyppi_5_20111209 = c("0", "1", "3"),
  huoneluku_1_20111212 = "00",
  contentscode = "ashivq_indeksi_2015"))
# Kauppamäärät kiinteistönvälittäjien kautta: 2020-2024 taulusta 13ms,
# 2025- taulusta 15iq (molemmat KVKL:n välittäjäkaupat)
ASHI_LKM_VANHA <- px_hae("ashi/13ms.px", list(
  alue_43_20220407 = c("ksu", "pks", "msu"),
  talotyyppi_5_20111209 = "0",
  contentscode = "lkm_julk_kvkl"))
ASHI_LKM_UUSI <- px_hae("ashi/15iq.px", list(
  alue_43_20260625 = c("SSS", "pks", "msu"),
  talotyyppi_5_20111209 = "0",
  contentscode = c("ashivm_kauppamaara_kvkl", "ashivm_kauppamaara_vvero")))

ashi_hinta <- function(alue, talotyyppi) {
  x <- ASHI_HINTA[ASHI_HINTA$alue_43_20260625 == alue &
                    ASHI_HINTA$talotyyppi_5_20111209 == talotyyppi, ]
  tasosta(data.frame(neljannes = x$timeperiod_q, arvo = x$arvo))
}
ashi_lkm <- function(alue_vanha, alue_uusi) {
  v <- ASHI_LKM_VANHA[ASHI_LKM_VANHA$alue_43_20220407 == alue_vanha &
                        ASHI_LKM_VANHA$timeperiod_m < "2025M01", ]
  u <- ASHI_LKM_UUSI[ASHI_LKM_UUSI$alue_43_20260625 == alue_uusi &
                       ASHI_LKM_UUSI$contentscode == "ashivm_kauppamaara_kvkl", ]
  tasosta(kk_neljannessarjaksi(c(v$timeperiod_m, u$timeperiod_m), c(v$arvo, u$arvo), sum))
}
ashi_lkm_vvero <- function() {
  u <- ASHI_LKM_UUSI[ASHI_LKM_UUSI$alue_43_20260625 == "SSS" &
                       ASHI_LKM_UUSI$contentscode == "ashivm_kauppamaara_vvero", ]
  tasosta(kk_neljannessarjaksi(u$timeperiod_m, u$arvo, sum))
}

# -----------------------------------------------------------------------------
# 2. Datakeskusten uutisaineisto (Dataikkuna)
# -----------------------------------------------------------------------------
dataikkuna <- sort(list.files("data", pattern = "^Dataikkuna.*\\.xlsx$", full.names = TRUE),
                   decreasing = TRUE)
if (length(dataikkuna) == 0) stop("Kansiosta data/ ei löydy Dataikkuna*.xlsx-tiedostoa")
message("Luetaan uutisaineisto: ", dataikkuna[1])
DI <- as.data.frame(read_excel(dataikkuna[1], sheet = 1))
# Sarakkeet nimetään järjestyksen mukaan (Dataikkunan vientimuoto):
# Päivämäärä, Teema, Investoija, Paikka, Summa (M€), Onko arvio?, Työpaikat,
# Pysyvät työpaikat, Vaihe, Valmistuminen, Kapasiteetti (MW), Linkki, Info, ID
names(DI)[1:11] <- c("pvm", "teema", "investoija", "paikka", "summa", "arvio",
                     "tyopaikat", "pysyvat", "vaihe", "valmistuminen", "mw")
# Hankkeet kohdistetaan valmistumisvuoteen (ei uutisoinnin päivämäärään).
# Mukaan otetaan hankkeet, joille on ilmoitettu valmistumisvuosi; keskeytetyt
# hankkeet jätetään pois.
DI <- DI[DI$teema == "Datakeskus", ]
DI$valmistuminen <- suppressWarnings(as.integer(DI$valmistuminen))
DI$valmistuminen[!is.na(DI$valmistuminen) & DI$valmistuminen == 0] <- NA
keskeytetty <- grepl("^5\\.", DI$vaihe)
message(sprintf("  Datakeskushankkeita %d, joista valmistumisvuosi %d, keskeytettyjä %d",
                nrow(DI), sum(!is.na(DI$valmistuminen)), sum(keskeytetty)))
DI <- DI[!keskeytetty, ]

# Hankkeille, joilta valmistumisvuosi puuttuu, arvioidaan vuosi uutisoinnin
# vuodesta ja hankevaiheesta (esiselvitys +4, suunnittelu +3,
# investointipäätös +2, käynnistys +1 vuotta). Käytetään vain erillisellä
# ennusterivillä.
VIIVE <- c("0" = 4, "1" = 3, "2" = 2, "3" = 1)
viive <- VIIVE[substr(DI$vaihe, 1, 1)]
viive[is.na(viive)] <- 3
DI$valmistuminen_arvio <- ifelse(is.na(DI$valmistuminen),
                                 as.integer(format(DI$pvm, "%Y")) + viive,
                                 DI$valmistuminen)
vuodet <- c(DI$valmistuminen, DI$valmistuminen_arvio)
DI_NELJ <- neljannekset_valilla(paste0(min(vuodet, na.rm = TRUE), "Q1"),
                                paste0(max(vuodet, na.rm = TRUE), "Q4"))

# Valmistumisvuoden summa jaetaan tasan vuoden neljälle neljännekselle.
# Vuodet, joilta ei ole hankkeita, saavat arvon 0.
valmistumissarja <- function(rivit, arvo, vuosi_hanke = DI$valmistuminen) {
  rivit <- rivit & !is.na(vuosi_hanke)
  vuosisumma <- tapply(arvo[rivit], vuosi_hanke[rivit], sum, na.rm = TRUE)
  vuosi <- substr(DI_NELJ, 1, 4)
  df <- data.frame(neljannes = DI_NELJ,
                   arvo = ifelse(vuosi %in% names(vuosisumma), vuosisumma[vuosi] / 4, 0))
  df$arvo[is.na(df$arvo)] <- 0
  tasosta(df)
}
kaikki <- rep(TRUE, nrow(DI))
etenevat <- grepl("^(2|3)\\.", DI$vaihe)   # investointipäätös tai käynnistys
summa <- as.numeric(DI$summa)
mw <- as.numeric(DI$mw)

# -----------------------------------------------------------------------------
# 3. Heatmapien rivit aihealueittain
# -----------------------------------------------------------------------------
TK <- "Tilastokeskus"
r <- function(aihe, sarja, df, lahde, yksikko, menetelma = "Neljännes / sama neljännes vuotta aiemmin") {
  df <- df[df$neljannes >= ALKU, c("neljannes", "arvo", "muutos")]
  if (nrow(df) == 0) return(NULL)
  data.frame(aihe = aihe, sarja = sarja, df, yksikko = yksikko,
             lahde = lahde, menetelma = menetelma)
}
VALM <- "Valmistumisvuoden summa jaettu tasan neljänneksille / sama neljännes vuotta aiemmin"
VOL <- "Volyymin muutos, alkuperäinen sarja (Tilastokeskus)"

rivit <- list(
  # --- Asuntorakentaminen
  r("Asuntorakentaminen", "Rakennusluvat, uudet asunnot", raku_sarja(RAKU, "1", "01", "raku-uusiAsuntoLkm"),
    paste(TK, "raku/156f"), "kpl"),
  r("Asuntorakentaminen", "Aloitetut asunnot", raku_sarja(RAKU, "2", "01", "raku-uusiAsuntoLkm"),
    paste(TK, "raku/156f"), "kpl"),
  r("Asuntorakentaminen", "Aloitetut asunnot: kerrostalot", raku_sarja(RAKU, "2", "012", "raku-uusiAsuntoLkm"),
    paste(TK, "raku/156f"), "kpl"),
  r("Asuntorakentaminen", "Aloitetut asunnot: pientalot", raku_sarja(RAKU, "2", "011", "raku-uusiAsuntoLkm"),
    paste(TK, "raku/156f"), "kpl"),
  r("Asuntorakentaminen", "Valmistuneet asunnot", raku_sarja(RAKU, "3", "01", "raku-uusiAsuntoLkm"),
    paste(TK, "raku/156f"), "kpl"),
  r("Asuntorakentaminen", "Uudisrakentamisen volyymi-indeksi", urvi_sarja(URVI, "01"),
    paste(TK, "raku/156g"), "indeksi 2020=100", "Neljänneksen keskiarvo / sama neljännes vuotta aiemmin"),
  r("Asuntorakentaminen", "Investoinnit asuinrakennuksiin", NTP$P51KN111,
    paste(TK, "ntp/132h"), "M€ (käypä)", VOL),

  # --- Käytettyjen asuntojen kauppa
  r("Käytettyjen asuntojen kauppa", "Kaupat (välittäjät): koko maa", ashi_lkm("ksu", "SSS"),
    paste(TK, "ashi/13ms + 15iq"), "kpl"),
  r("Käytettyjen asuntojen kauppa", "Kaupat (välittäjät): pääkaupunkiseutu", ashi_lkm("pks", "pks"),
    paste(TK, "ashi/13ms + 15iq"), "kpl"),
  r("Käytettyjen asuntojen kauppa", "Kaupat (välittäjät): muu Suomi", ashi_lkm("msu", "msu"),
    paste(TK, "ashi/13ms + 15iq"), "kpl"),
  r("Käytettyjen asuntojen kauppa", "Kaupat (varainsiirtovero): koko maa", ashi_lkm_vvero(),
    paste(TK, "ashi/15iq"), "kpl"),
  r("Käytettyjen asuntojen kauppa", "Hintaindeksi: koko maa", ashi_hinta("SSS", "0"),
    paste(TK, "ashi/15it"), "indeksi 2015=100"),
  r("Käytettyjen asuntojen kauppa", "Hintaindeksi: pääkaupunkiseutu", ashi_hinta("pks", "0"),
    paste(TK, "ashi/15it"), "indeksi 2015=100"),
  r("Käytettyjen asuntojen kauppa", "Hintaindeksi: muu Suomi", ashi_hinta("msu", "0"),
    paste(TK, "ashi/15it"), "indeksi 2015=100"),
  r("Käytettyjen asuntojen kauppa", "Hintaindeksi: kerrostalot", ashi_hinta("SSS", "3"),
    paste(TK, "ashi/15it"), "indeksi 2015=100"),
  r("Käytettyjen asuntojen kauppa", "Hintaindeksi: rivitalot", ashi_hinta("SSS", "1"),
    paste(TK, "ashi/15it"), "indeksi 2015=100"),

  # --- Muu talorakentaminen
  r("Muu talorakentaminen", "Rakennusluvat, muut kuin asuinrakennukset", raku_sarja(RAKU, "1", "02T19", "raku-tilavuusToimenpide"),
    paste(TK, "raku/156f"), "m³"),
  r("Muu talorakentaminen", "Aloitukset, muut kuin asuinrakennukset", raku_sarja(RAKU, "2", "02T19", "raku-tilavuusToimenpide"),
    paste(TK, "raku/156f"), "m³"),
  r("Muu talorakentaminen", "Aloitukset: liike- ja toimistorakennukset", raku_sarja(RAKU, "2", "03T05", "raku-tilavuusToimenpide"),
    paste(TK, "raku/156f"), "m³"),
  r("Muu talorakentaminen", "Aloitukset: julkiset palvelurakennukset", raku_sarja(RAKU, "2", "06T08_13", "raku-tilavuusToimenpide"),
    paste(TK, "raku/156f"), "m³"),
  r("Muu talorakentaminen", "Aloitukset: teollisuus- ja varastorakennukset", raku_sarja(RAKU, "2", "09T12", "raku-tilavuusToimenpide"),
    paste(TK, "raku/156f"), "m³"),
  r("Muu talorakentaminen", "Valmistuneet, muut kuin asuinrakennukset", raku_sarja(RAKU, "3", "02T19", "raku-tilavuusToimenpide"),
    paste(TK, "raku/156f"), "m³"),
  r("Muu talorakentaminen", "Uudisrakentamisen volyymi-indeksi", urvi_sarja(URVI, "02T19"),
    paste(TK, "raku/156g"), "indeksi 2020=100", "Neljänneksen keskiarvo / sama neljännes vuotta aiemmin"),
  r("Muu talorakentaminen", "Investoinnit muihin talorakennuksiin", NTP$P51KN1121,
    paste(TK, "ntp/132h"), "M€ (käypä)", VOL),

  # --- Maa- ja vesirakentaminen
  r("Maa- ja vesirakentaminen", "Maa- ja vesirakennusinvestoinnit", NTP$P51KN1122_N1123,
    paste(TK, "ntp/132h"), "M€ (käypä)", VOL),
  r("Maa- ja vesirakentaminen", "Liikevaihto (TOL 42)", rlv_sarja(RLV, "F42", "lv"),
    paste(TK, "rlv/14m1"), "indeksi 2021=100", "Alkuperäisen sarjan vuosimuutos (Tilastokeskus)"),
  r("Maa- ja vesirakentaminen", "Myynnin määrä (TOL 42)", rlv_sarja(RLV, "F42", "mi"),
    paste(TK, "rlv/14m1"), "indeksi 2021=100", "Alkuperäisen sarjan vuosimuutos (Tilastokeskus)"),

  # --- Datakeskusten rakentaminen
  r("Datakeskusten rakentaminen", "Valmistuvien hankkeiden investoinnit", valmistumissarja(kaikki, summa),
    "Uutisointi (Dataikkuna)", "M€", VALM),
  r("Datakeskusten rakentaminen", "Valmistuva kapasiteetti", valmistumissarja(kaikki, mw),
    "Uutisointi (Dataikkuna)", "MW", VALM),
  r("Datakeskusten rakentaminen", "Valmistuvat hankkeet", valmistumissarja(kaikki, rep(1, nrow(DI))),
    "Uutisointi (Dataikkuna)", "kpl (vuoden hankkeet / 4)", VALM),
  r("Datakeskusten rakentaminen", "Investointipäätös tai käynnistys: investoinnit", valmistumissarja(etenevat, summa),
    "Uutisointi (Dataikkuna)", "M€", VALM),
  r("Datakeskusten rakentaminen", "Investoinnit, myös arvioidut valmistumisvuodet",
    valmistumissarja(kaikki, summa, DI$valmistuminen_arvio),
    "Uutisointi (Dataikkuna)", "M€",
    "Kuten yllä; puuttuva valmistumisvuosi arvioitu hankevaiheesta (esiselvitys +4, suunnittelu +3, päätös +2, käynnistys +1 v.)"),
  r("Datakeskusten rakentaminen", "Aloitukset: teollisuus- ja varastorakennukset", raku_sarja(RAKU, "2", "09T12", "raku-tilavuusToimenpide"),
    paste(TK, "raku/156f (välillinen mittari)"), "m³"),

  # --- T&K-investoinnit
  r("T&K-investoinnit", "Henkiset omaisuustuotteet (T&K, ohjelmistot)", NTP$P51KN115_N117,
    paste(TK, "ntp/132h"), "M€ (käypä)", VOL),

  # --- Investoinnit yhteensä
  r("Investoinnit yhteensä", "Kiinteän pääoman bruttomuodostus", NTP$P51K,
    paste(TK, "ntp/132h"), "M€ (käypä)", VOL),
  r("Investoinnit yhteensä", "Yksityiset investoinnit", NTP$P51KXS13,
    paste(TK, "ntp/132h"), "M€ (käypä)", VOL),
  r("Investoinnit yhteensä", "Julkiset investoinnit", NTP$P51KS13,
    paste(TK, "ntp/132h"), "M€ (käypä)", VOL),
  r("Investoinnit yhteensä", "Asuinrakennukset", NTP$P51KN111,
    paste(TK, "ntp/132h"), "M€ (käypä)", VOL),
  r("Investoinnit yhteensä", "Muut talorakennukset", NTP$P51KN1121,
    paste(TK, "ntp/132h"), "M€ (käypä)", VOL),
  r("Investoinnit yhteensä", "Maa- ja vesirakennukset", NTP$P51KN1122_N1123,
    paste(TK, "ntp/132h"), "M€ (käypä)", VOL),
  r("Investoinnit yhteensä", "Koneet, laitteet ja kuljetusvälineet", NTP$P51KN113_N114,
    paste(TK, "ntp/132h"), "M€ (käypä)", VOL),
  r("Investoinnit yhteensä", "Henkiset omaisuustuotteet", NTP$P51KN115_N117,
    paste(TK, "ntp/132h"), "M€ (käypä)", VOL)
)

# T&K-menot (tkke) ovat vain vuositasolla, joten T&K-heatmapiin lisätään
# vuosittainen rivi, jonka arvo näytetään vuoden neljännellä neljänneksellä.
message("Haetaan tutkimus- ja kehittämistoiminnan menot (tkke/13ri)...")
tkke <- tryCatch({
  meta <- request(paste0(PX_API, "tkke/13ri.px")) |> req_perform() |>
    resp_body_json(simplifyVector = FALSE)
  muuttujat <- vapply(meta$variables, `[[`, "", "code")
  valinta <- list()
  for (v in meta$variables) {
    if (isTRUE(v$time)) next
    koodit <- unlist(v$values)
    tekstit <- unlist(v$valueTexts)
    if (v$code == "contentscode") {
      valinta[[v$code]] <- koodit[grep("menot", tekstit, ignore.case = TRUE)[1]]
    } else {
      yht <- grep("yhteensä|kaikki|SSS", paste(koodit, tekstit), ignore.case = TRUE)
      valinta[[v$code]] <- koodit[if (length(yht)) yht[1] else 1]
    }
  }
  d <- px_hae("tkke/13ri.px", valinta)
  aika <- setdiff(names(d), c(names(valinta), "arvo"))[1]
  vuosi <- substr(d[[aika]], 1, 4)
  df <- data.frame(neljannes = paste0(vuosi, "Q4"), arvo = d$arvo)
  df <- df[order(df$neljannes), ]
  df$muutos <- 100 * (df$arvo / c(NA, head(df$arvo, -1)) - 1)
  r("T&K-investoinnit", "T&K-menot yhteensä (vuositieto)", df,
    paste(TK, "tkke/13ri"), "M€", "Vuosi / edellinen vuosi (näytetään Q4:llä)")
}, error = function(e) { message("  T&K-menojen haku epäonnistui: ", conditionMessage(e)); NULL })
rivit <- c(rivit, list(tkke))

data <- do.call(rbind, rivit[!vapply(rivit, is.null, logical(1))])

# -----------------------------------------------------------------------------
# 4. Ennusteet vuoden 2029 loppuun
# -----------------------------------------------------------------------------
message("Lasketaan ennusteet...")
sarjajarjestys <- unique(paste(data$aihe, data$sarja))
source("R/ennuste.R", encoding = "UTF-8")
uutisrivit <- data$lahde == "Uutisointi (Dataikkuna)"
vuositiedot <- unique(data$sarja[grepl("vuositieto", data$sarja)])
data <- lisaa_ennusteet(data, lue_oletukset(),
                        ohita = c(unique(data$sarja[uutisrivit]), vuositiedot))
# Datakeskusten hankeputki: päättymättömät neljännekset ovat ennustetta
tama_q <- paste0(format(Sys.Date(), "%Y"), "Q", (as.integer(format(Sys.Date(), "%m")) - 1) %/% 3 + 1)
putki <- data$lahde == "Uutisointi (Dataikkuna)" & data$neljannes >= tama_q
data$ennuste[putki] <- TRUE
data$ennusteen_lahde[putki] <- "Uutisoitu hankeputki (Dataikkuna), ilmoitetut valmistumisvuodet"
data <- data[data$neljannes <= ENNUSTE_LOPPU, ]
data <- data[order(match(paste(data$aihe, data$sarja), sarjajarjestys), data$neljannes), ]
data$jarjestys <- ave(seq_len(nrow(data)), data$aihe, FUN = function(i) match(data$sarja[i], unique(data$sarja[i])))
data$arvo <- signif(data$arvo, 6)
data$muutos <- round(data$muutos, 1)

dir.create(ULOS, showWarnings = FALSE, recursive = TRUE)
write.csv(data, file.path(ULOS, "heatmap.csv"), row.names = FALSE, fileEncoding = "UTF-8")
file.copy("data/ennusteoletukset.csv", file.path(ULOS, "ennusteoletukset.csv"), overwrite = TRUE)
writeLines(format(Sys.time(), "%Y-%m-%d %H:%M %Z", tz = "Europe/Helsinki"),
           file.path(ULOS, "paivitetty.txt"))

message(sprintf("Valmis: %d riviä, %d sarjaa, viimeisin neljännes %s",
                nrow(data), length(unique(paste(data$aihe, data$sarja))), max(data$neljannes)))
