# Investointien ja rakentamisen neljännesvuosikehitys (heatmapit)

Shiny-sovellus, jossa on jokaisesta aihealueesta oma heatmap. Jokainen solu kertoo,
paljonko neljänneksen luku muuttui (%) samasta neljänneksestä vuotta aiemmin.
Sininen tarkoittaa kasvua ja punainen laskua.

| Aihealue | Sarjat | Lähde |
|---|---|---|
| Asuntorakentaminen | asuntojen rakennusluvat, aloitukset (myös kerros- ja pientalot) ja valmistumiset; uudisrakentamisen volyymi-indeksi; asuinrakennusinvestoinnit | Tilastokeskus raku/156f, raku/156g, ntp/132h |
| Käytettyjen asuntojen kauppa | kauppamäärät (välittäjät: koko maa, PKS, muu Suomi; varainsiirtovero); hintaindeksi alueittain ja talotyypeittäin | Tilastokeskus ashi/13ms, 15iq, 15it |
| Muu talorakentaminen | muiden kuin asuinrakennusten luvat, aloitukset (rakennusluokittain) ja valmistumiset, m³; volyymi-indeksi; investoinnit | Tilastokeskus raku/156f, raku/156g, ntp/132h |
| Maa- ja vesirakentaminen | maa- ja vesirakennusinvestoinnit; TOL 42 liikevaihto ja myynnin määrä | Tilastokeskus ntp/132h, rlv/14m1 |
| Datakeskusten rakentaminen | valmistumisvuoden mukaan: investoinnit (M€), kapasiteetti (MW), hankkeet (kpl), investointipäätöksen tai käynnistyksen saaneiden investoinnit; teollisuus- ja varastorakennusten aloitukset (välillinen mittari) | Uutisointi (Dataikkuna), Tilastokeskus raku/156f |
| T&K-investoinnit | investoinnit henkisiin omaisuustuotteisiin (neljännesvuosittain); T&K-menot (vuosittain, näkyy Q4:llä) | Tilastokeskus ntp/132h, tkke/13ri |
| Investoinnit yhteensä | kiinteän pääoman bruttomuodostus; yksityiset ja julkiset investoinnit; investointilajit | Tilastokeskus ntp/132h |

Sovelluksen *Lähteet ja menetelmät* -välilehdellä näkyy jokaisen sarjan yksikkö,
lähdetaulukko, vertailutapa ja viimeisin neljännes.

## Rakenne

```
R/paivita_data.R      hakee aineistot ja laskee vuosimuutokset -> app/data/heatmap.csv
app/app.R             Shiny-sovellus (shiny, bslib, ggplot2)
app/data/             sovelluksen käyttämä aineisto (päivittyy automaattisesti)
data/Dataikkuna*.xlsx datakeskusten uutisaineisto
.github/workflows/    päivittäinen päivitys ja julkaisu
```

## Päivitys

GitHub Actions (`.github/workflows/paivita.yml`) toimii näin joka aamu klo 04.17 UTC:

1. Ajaa skriptin `R/paivita_data.R`, joka hakee uusimmat tiedot Tilastokeskuksen
   StatFin-rajapinnasta.
2. Tallentaa muuttuneen aineiston repositorioon.
3. Kokoaa sovelluksen shinylive-sivustoksi ja julkaisee sen GitHub Pagesiin. Tällöin R
   ajetaan käyttäjän selaimessa, eikä palvelinta tarvita.

Ajon voi käynnistää myös käsin: *Actions → Päivitä aineisto ja julkaise → Run workflow*.
Ensimmäisellä kerralla GitHub Pages pitää ottaa käyttöön:
*Settings → Pages → Build and deployment → Source: GitHub Actions*.

### Datakeskusten uutisaineisto

Uutisiin perustuvaa aineistoa ei voi hakea rajapinnasta automaattisesti. Päivitä se
näin: tallenna uusi Dataikkunan vienti kansioon `data/` nimellä, joka alkaa sanalla
`Dataikkuna` ja päättyy `.xlsx` (esim. `Dataikkuna_2026_12_01.xlsx`). Skripti käyttää
aakkosjärjestyksessä viimeistä tiedostoa. Sarakkeiden järjestyksen pitää olla sama
kuin Dataikkunan viennissä: Päivämäärä, Teema, Investoija, Paikka, Summa (M€), …,
Vaihe, Valmistuminen, Kapasiteetti (MW). Mukaan otetaan rivit, joilla Teema on
`Datakeskus`.

Hanke kohdistetaan Dataikkunan Valmistuminen-sarakkeen vuoteen, ei uutisoinnin
päivämäärään. Hankkeen investointi, kapasiteetti ja lukumäärä jaetaan tasan
valmistumisvuoden neljälle neljännekselle. Siksi saman vuoden neljänneksillä on sama
muutos vuoden takaisesta, ja tulevat vuodet kuvaavat ilmoitettua hankeputkea.
Hankkeet, joilta valmistumisvuosi puuttuu, ja keskeytetyt hankkeet jätetään pois.
Ajon tulosteessa näkyy, montako hanketta on mukana. Prosenttimuutokset ovat suuria,
kun lähtötaso on pieni.

## Paikallinen ajo

```r
# repositorion juuresta
source("R/paivita_data.R")   # paketit httr2, jsonlite, readxl
shiny::runApp("app")         # paketit shiny, bslib, ggplot2
```

## Huomioita

- Kansantalouden tilinpidon sarjoissa (investoinnit) ja rakentamisen
  liikevaihtokuvaajassa käytetään Tilastokeskuksen julkaisemaa vuosimuutosta
  (volyymi, alkuperäinen sarja). Muissa sarjoissa vuosimuutos lasketaan tasoista.
- Kuukausisarjoista muodostetaan neljännekset summaamalla määrät ja laskemalla
  indekseille keskiarvo. Mukaan otetaan vain täydet neljännekset.
- Varainsiirtoverotietoihin perustuvat kauppamäärät alkavat vuodesta 2025, joten
  vuosimuutokset alkavat vuodesta 2026.
- Neljännesvuosittaista T&K-tilastoa ei ole. Sen sijaan käytetään investointeja
  henkisiin omaisuustuotteisiin, joista suurin osa on T&K:ta ja ohjelmistoja.
- Uusimmat tiedot ovat ennakollisia ja voivat tarkentua.
