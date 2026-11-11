# =============================================================================
# Shiny-sovellus: neljännesvuosikehitys suhteessa vuoden takaiseen (heatmapit)
#
# Aineisto: data/heatmap.csv, jonka R/paivita_data.R tuottaa Tilastokeskuksen
# rajapinnasta ja datakeskusten uutisaineistosta.
# Paikallinen ajo repositorion juuresta: shiny::runApp("app")
# =============================================================================

library(shiny)
library(bslib)
library(ggplot2)

DATA <- read.csv("data/heatmap.csv", encoding = "UTF-8", stringsAsFactors = FALSE)
DATA$ennuste <- as.logical(DATA$ennuste)
OLETUKSET <- if (file.exists("data/ennusteoletukset.csv"))
  read.csv("data/ennusteoletukset.csv", encoding = "UTF-8", stringsAsFactors = FALSE) else NULL
PAIVITETTY <- if (file.exists("data/paivitetty.txt")) readLines("data/paivitetty.txt", warn = FALSE)[1] else "?"

AIHEET <- c(
  "Asuntorakentaminen",
  "Käytettyjen asuntojen kauppa",
  "Muu talorakentaminen",
  "Maa- ja vesirakentaminen",
  "Datakeskusten rakentaminen",
  "T&K-investoinnit",
  "Investoinnit yhteensä"
)
AIHEET <- AIHEET[AIHEET %in% DATA$aihe]

# Yleiskuvan rivit: yksi tai kaksi pääsarjaa kustakin aihealueesta
YLEISKUVA <- list(
  c("Asuntorakentaminen", "Aloitetut asunnot"),
  c("Käytettyjen asuntojen kauppa", "Kaupat (välittäjät): koko maa"),
  c("Käytettyjen asuntojen kauppa", "Hintaindeksi: koko maa"),
  c("Muu talorakentaminen", "Aloitukset, muut kuin asuinrakennukset"),
  c("Maa- ja vesirakentaminen", "Maa- ja vesirakennusinvestoinnit"),
  c("Datakeskusten rakentaminen", "Rakennusaikaiset investoinnit"),
  c("T&K-investoinnit", "Henkiset omaisuustuotteet (T&K, ohjelmistot)"),
  c("Investoinnit yhteensä", "Kiinteän pääoman bruttomuodostus")
)

KUVAUKSET <- list(
  "Asuntorakentaminen" = "Asuntojen rakennusluvat, aloitukset ja valmistumiset (kpl) neljänneksittäin, uudisrakentamisen volyymi-indeksi sekä asuinrakennusinvestointien volyymi.",
  "Käytettyjen asuntojen kauppa" = "Vanhojen osakeasuntojen kauppamäärät (kiinteistönvälittäjät ja varainsiirtoverotiedot) sekä hintaindeksi alueittain ja talotyypeittäin.",
  "Muu talorakentaminen" = "Muiden kuin asuinrakennusten rakennusluvat, aloitukset ja valmistumiset tilavuutena (m³), uudisrakentamisen volyymi-indeksi ja investoinnit muihin talorakennuksiin.",
  "Maa- ja vesirakentaminen" = "Maa- ja vesirakennusinvestointien volyymi (kansantalouden tilinpito) sekä maa- ja vesirakentamisen toimialan (TOL 42) liikevaihto ja myynnin määrä.",
  "Datakeskusten rakentaminen" = "Uutisoidut datakeskushankkeet (Dataikkuna-aineisto) rakennusajan mukaan. Hankkeen investointi jaetaan tasan rakennusajan neljänneksille. Rakentamisen alku ja valmistumisneljännes poimitaan uutisteksteistä (esim. \"rakentaminen alkaa 2027 ensimmäisellä vuosipuoliskolla\", \"valmistuu syksyllä 2026\"); muuten valmistuminen on valmistumisvuoden Q3 ja rakennusaika 6 neljännestä (alle 500 M€) tai 8 neljännestä. Välillisenä mittarina teollisuus- ja varastorakennusten aloitukset (Tilastokeskus).",
  "T&K-investoinnit" = "Neljännesvuosittain: investoinnit henkisiin omaisuustuotteisiin (pääosin T&K ja ohjelmistot). Vuosittain: T&K-menot yhteensä, näytetään vuoden neljännellä neljänneksellä.",
  "Investoinnit yhteensä" = "Kiinteän pääoman bruttomuodostus ja sen osat, volyymin muutos vuoden takaisesta (alkuperäinen sarja, kansantalouden tilinpito)."
)

VARI_NEG <- "#b2182b"
VARI_POS <- "#2166ac"
VARI_KESKI <- "#f7f7f7"
VARI_PUUTTUU <- "#e4e3df"

# -----------------------------------------------------------------------------
# Heatmapin piirto
# -----------------------------------------------------------------------------
heatmap_kuva <- function(d, raja, nayta_arvot) {
  d$sarja_f <- factor(d$sarja, levels = rev(unique(d$sarja)))
  d$neljannes_f <- factor(d$neljannes, levels = sort(unique(d$neljannes)))
  pyor <- round(d$muutos)
  pyor[!is.na(pyor) & pyor == 0] <- 0   # ei "-0"
  d$teksti <- ifelse(is.na(pyor), "", formatC(pyor, format = "d"))
  d$tumma <- !is.na(d$muutos) & abs(d$muutos) > 0.55 * raja & !d$ennuste
  nelj <- levels(d$neljannes_f)
  merkit <- ifelse(substr(nelj, 6, 6) == "1", nelj, substr(nelj, 5, 6))

  ennuste <- d[d$ennuste, ]
  p <- ggplot(d, aes(x = neljannes_f, y = sarja_f, fill = muutos)) +
    geom_tile(aes(alpha = ennuste), colour = "white", linewidth = 0.6, show.legend = FALSE) +
    scale_alpha_manual(values = c(`FALSE` = 1, `TRUE` = 0.5)) +
    scale_fill_gradient2(
      low = VARI_NEG, mid = VARI_KESKI, high = VARI_POS, midpoint = 0,
      limits = c(-raja, raja), oob = scales::squish, na.value = VARI_PUUTTUU,
      name = "Muutos vuoden\ntakaisesta, %",
      labels = function(x) ifelse(abs(x) >= raja, paste0(ifelse(x > 0, "≥ ", "≤ "), x), x)
    ) +
    scale_x_discrete(labels = merkit, expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) +
    labs(x = NULL, y = NULL) +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid = element_blank(),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 10, colour = "#52514e"),
      axis.text.y = element_text(size = 12, colour = "#0b0b0b"),
      legend.position = "top",
      legend.justification = "left",
      legend.key.width = unit(2.2, "cm"),
      legend.key.height = unit(0.35, "cm"),
      legend.title = element_text(size = 10, colour = "#52514e"),
      plot.margin = margin(16, 8, 4, 4)
    )
  # Ennustesolut: vaaleampi väri, katkoviivareunus ja kursivoitu luku
  if (nrow(ennuste)) {
    p <- p + geom_tile(data = ennuste, fill = NA, colour = "#8a8984",
                       linewidth = 0.35, linetype = "22", width = 0.86, height = 0.8)
    eka <- match(min(ennuste$neljannes), nelj)
    p <- p + geom_vline(xintercept = eka - 0.5, colour = "#52514e", linewidth = 0.5, linetype = "dashed") +
      annotate("text", x = eka - 0.4, y = length(levels(d$sarja_f)) + 0.62, label = "ennuste →",
               hjust = 0, vjust = 0, size = 3.2, colour = "#52514e") +
      coord_cartesian(clip = "off")
  }
  if (nayta_arvot) {
    koko <- if (length(nelj) > 32) 2.2 else 3   # pitkillä aikaväleillä pienempi teksti
    p <- p + geom_text(aes(label = teksti, colour = tumma,
                           fontface = ifelse(ennuste, "italic", "plain")),
                       size = koko, show.legend = FALSE) +
      scale_colour_manual(values = c(`FALSE` = "#0b0b0b", `TRUE` = "white"))
  }
  p
}

kuvan_korkeus <- function(n_rivit) 110 + 38 * n_rivit

# -----------------------------------------------------------------------------
# Käyttöliittymä
# -----------------------------------------------------------------------------
heatmap_paneeli <- function(id, otsikko, kuvaus) {
  nav_panel(
    otsikko,
    card(
      card_header(otsikko),
      p(class = "text-body-secondary small mb-2", kuvaus),
      uiOutput(paste0("kuva_", id)),
      uiOutput(paste0("info_", id))
    )
  )
}

paneelit <- c(
  list(heatmap_paneeli("yleis", "Yleiskuva",
                       "Kunkin aihealueen pääsarja. Solu = neljänneksen muutos (%) samasta neljänneksestä vuotta aiemmin.")),
  lapply(seq_along(AIHEET), function(i) heatmap_paneeli(i, AIHEET[i], KUVAUKSET[[AIHEET[i]]])),
  list(nav_panel("Lähteet ja menetelmät", card(
    card_header("Sarjat, lähteet ja menetelmät"),
    tableOutput("lahteet"),
    p(class = "small text-body-secondary",
      "Tilastokeskuksen aineistot haetaan StatFin-tietokannan PxWeb-rajapinnasta. ",
      "Kuukausisarjat summataan (määrät) tai keskiarvoistetaan (indeksit) neljänneksiksi; ",
      "vain täydet neljännekset ovat mukana. Kansantalouden tilinpidon ja rakentamisen ",
      "liikevaihtokuvaajan sarjoissa käytetään Tilastokeskuksen julkaisemaa vuosimuutosta. ",
      "Viimeisimmät tiedot ovat ennakollisia ja voivat tarkentua."),
    h6("Ennusteet", class = "mt-3"),
    p(class = "small text-body-secondary",
      "Ennusteet ulottuvat vuoden 2029 loppuun. Jos sarjalle on julkaistu vuosiennuste (taulukko alla), ",
      "vuoden jäljellä olevat neljännekset asetetaan niin, että koko vuoden muutos vastaa ennustetta. ",
      "Muina vuosina vuosimuutos palautuu tilastollisesti edellisestä neljänneksestä kohti sarjan pitkän aikavälin ",
      "mediaania (2015 alkaen, rajattu välille −3…+5 %): poikkeamasta säilyy 80 % neljänneksessä. ",
      "Datakeskusten ennuste on uutisoitu hankeputki ilmoitettujen valmistumisvuosien mukaan; hankkeet ilman ",
      "valmistumisvuotta eivät ole mukana."),
    tableOutput("oletukset"),
    p(class = "small text-body-secondary",
      "Datakeskusten uutisaineisto on Dataikkuna-hankeluettelosta (uutisointien perusteella koottu). ",
      "Hankkeen rakennusaika päätellään uutisteksteistä (rakentamisen alku, valmistumisen neljännes) tai oletuksista, ",
      "ja investointi jaetaan tasan rakennusajan neljänneksille. Kapasiteetti ja hankemäärä ovat rakenteilla olevien ",
      "hankkeiden kanta. Hankkeet ilman valmistumisvuotta ja keskeytetyt hankkeet eivät ole mukana. ",
      "Hankekohtainen ajoitus: data/datakeskushankkeiden_ajoitus.csv. ",
      "Puuttuvat summat (0 M€) eivät ole mukana summissa.")
  )))
)

ui <- page_sidebar(
  title = "Investointien ja rakentamisen neljännesvuosikehitys",
  theme = bs_theme(version = 5, primary = VARI_POS, base_font = font_collection(
    "system-ui", "-apple-system", "Segoe UI", "Roboto", "sans-serif")),
  fillable = FALSE,
  sidebar = sidebar(
    width = 280,
    sliderInput("vuodet", "Aikaväli", min = as.integer(substr(min(DATA$neljannes), 1, 4)),
                max = as.integer(substr(max(DATA$neljannes), 1, 4)),
                value = c(max(as.integer(format(Sys.Date(), "%Y")) - 5,
                              as.integer(substr(min(DATA$neljannes), 1, 4))),
                          as.integer(substr(max(DATA$neljannes), 1, 4))),
                step = 1, sep = ""),
    sliderInput("raja", "Väriasteikon raja (± %)", min = 5, max = 100, value = 25, step = 5),
    checkboxInput("arvot", "Näytä luvut soluissa", value = TRUE),
    checkboxInput("ennusteet", "Näytä ennusteet (vuoden 2029 loppuun)", value = TRUE),
    hr(),
    p(class = "small text-body-secondary",
      span(style = paste0("color:", VARI_POS, ";font-weight:600"), "Sininen"), " = kasvua, ",
      span(style = paste0("color:", VARI_NEG, ";font-weight:600"), "punainen"), " = laskua vuoden takaisesta. ",
      "Harmaa = ei tietoa. Vaaleat, katkoviivalla reunustetut solut ovat ennustetta."),
    p(class = "small text-body-secondary", "Vie hiiri solun päälle nähdäksesi tarkat luvut."),
    p(class = "small text-body-secondary", paste("Aineisto päivitetty:", PAIVITETTY))
  ),
  do.call(navset_card_underline, c(list(id = "valilehti"), paneelit))
)

# -----------------------------------------------------------------------------
# Palvelin
# -----------------------------------------------------------------------------
server <- function(input, output, session) {

  rajattu <- reactive({
    v <- input$vuodet
    d <- DATA[as.integer(substr(DATA$neljannes, 1, 4)) >= v[1] &
                as.integer(substr(DATA$neljannes, 1, 4)) <= v[2], ]
    if (!isTRUE(input$ennusteet)) d <- d[!d$ennuste, ]
    d
  })

  aiheen_data <- function(id) {
    d <- rajattu()
    if (id == "yleis") {
      osat <- lapply(YLEISKUVA, function(x) {
        o <- d[d$aihe == x[1] & d$sarja == x[2], ]
        if (nrow(o)) o$sarja <- if (x[2] %in% c("Aloitetut asunnot", "Rakennusaikaiset investoinnit"))
          paste0(x[1], ": ", tolower(x[2])) else x[2]
        o
      })
      d <- do.call(rbind, osat)
      d$jarjestys <- match(d$sarja, unique(d$sarja))
    } else {
      d <- d[d$aihe == AIHEET[as.integer(id)], ]
    }
    d[order(d$jarjestys, d$neljannes), ]
  }

  piirra <- function(id) {
    output[[paste0("kuva_", id)]] <- renderUI({
      d <- aiheen_data(id)
      plotOutput(paste0("plot_", id), height = paste0(kuvan_korkeus(length(unique(d$sarja))), "px"),
                 hover = hoverOpts(paste0("hover_", id), delay = 80, delayType = "throttle"))
    })
    output[[paste0("plot_", id)]] <- renderPlot({
      d <- aiheen_data(id)
      validate(need(nrow(d) > 0, "Ei tietoja valitulla aikavälillä."))
      heatmap_kuva(d, input$raja, input$arvot)
    }, res = 96)
    output[[paste0("info_", id)]] <- renderUI({
      h <- input[[paste0("hover_", id)]]
      d <- aiheen_data(id)
      teksti <- "Vie hiiri solun päälle."
      if (!is.null(h) && nrow(d)) {
        nelj <- sort(unique(d$neljannes))
        sarjat <- rev(unique(d$sarja))
        xi <- round(h$x); yi <- round(h$y)
        if (xi >= 1 && xi <= length(nelj) && yi >= 1 && yi <= length(sarjat)) {
          rivi <- d[d$sarja == sarjat[yi] & d$neljannes == nelj[xi], ]
          if (nrow(rivi)) {
            muutos <- if (is.na(rivi$muutos)) "ei tietoa" else
              paste0(formatC(rivi$muutos, format = "f", digits = 1, decimal.mark = ","), " %")
            arvo <- if (is.na(rivi$arvo)) "–" else
              formatC(rivi$arvo, format = "f", digits = if (abs(rivi$arvo) < 1000) 1 else 0,
                      big.mark = " ", decimal.mark = ",")
            ennusteteksti <- if (isTRUE(rivi$ennuste))
              sprintf("<br><b>Ennuste</b>: %s", htmltools::htmlEscape(rivi$ennusteen_lahde)) else ""
            teksti <- HTML(sprintf("<b>%s, %s</b>: muutos vuoden takaisesta %s &nbsp;·&nbsp; taso %s %s &nbsp;·&nbsp; <span class='text-body-secondary'>%s; %s</span>%s",
                                   htmltools::htmlEscape(rivi$sarja), rivi$neljannes, muutos, arvo,
                                   htmltools::htmlEscape(rivi$yksikko),
                                   htmltools::htmlEscape(rivi$lahde), htmltools::htmlEscape(rivi$menetelma),
                                   ennusteteksti))
          }
        }
      }
      div(class = "small mt-2", style = "min-height:4em", teksti)
    })
  }

  piirra("yleis")
  for (i in seq_along(AIHEET)) local({ ii <- as.character(i); piirra(ii) })

  output$lahteet <- renderTable({
    s <- unique(DATA[, c("aihe", "sarja", "yksikko", "lahde", "menetelma")])
    tot <- !is.na(DATA$muutos) & !DATA$ennuste
    viim <- tapply(DATA$neljannes[tot], paste(DATA$aihe, DATA$sarja)[tot], max)
    s$viimeisin <- viim[paste(s$aihe, s$sarja)]
    names(s) <- c("Aihealue", "Sarja", "Yksikkö", "Lähde", "Vertailu", "Viimeisin toteutunut")
    s
  }, striped = TRUE, spacing = "s", width = "100%")

  output$oletukset <- renderTable({
    validate(need(!is.null(OLETUKSET), "Ennusteoletuksia ei löydy."))
    o <- OLETUKSET[, c("aihe", "sarja_regex", "vuosi", "muutos_pct", "lahde", "julkaistu")]
    o$sarja_regex <- gsub("\\^|\\$|\\\\", "", o$sarja_regex)
    o$sarja_regex[o$sarja_regex == "."] <- "kaikki sarjat"
    o$vuosi <- as.character(o$vuosi)
    names(o) <- c("Aihealue", "Sarjat", "Vuosi", "Muutos, %", "Lähde", "Julkaistu")
    o
  }, striped = TRUE, spacing = "s", width = "100%")
}

shinyApp(ui, server)
