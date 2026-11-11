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

## Ennusteet vuoden 2029 loppuun

Toteutuneiden tietojen jälkeen jokaiselle sarjalle lasketaan ennuste vuoden 2029
loppuun (`R/ennuste.R`). Sovelluksessa ennustesolut näkyvät vaaleampina ja
katkoviivalla reunustettuina, ja ennusteet voi piilottaa.

1. **Julkaistut ennusteet** (`data/ennusteoletukset.csv`): Taulukossa on vuosimuutokset,
   jotka on poimittu uutisoinnista ja ennustelaitosten julkaisuista. Lähteinä ovat
   VM:n taloudellinen katsaus syksyllä 2026 (investoinnit), RT:n suhdannekatsaus
   lokakuussa 2026 (asunto- ja infrarakentaminen, asuntoaloitukset) ja
   Sp-Kodin ja Säästöpankkiryhmän asuntokauppaennuste syyskuussa 2026 (kauppamäärät
   ja hinnat). Vuoden jäljellä olevat neljännekset asetetaan niin, että koko vuoden
   muutos vastaa ennustetta. Taulukkoa voi päivittää käsin, kun uusia ennusteita
   julkaistaan. Sarake `sarja_regex` kertoo, mihin sarjoihin rivi kohdistuu.
2. **Tilastollinen jatke**: Vuosina, joille ei ole julkaistua ennustetta, vuosimuutos
   palautuu edellisestä neljänneksestä kohti sarjan pitkän aikavälin mediaania
   (vuodesta 2015 alkaen, rajattu välille −3…+5 %). Poikkeamasta säilyy 80 %
   neljänneksestä toiseen.
3. **Datakeskukset**: Ennuste on uutisoitu hankeputki ilmoitettujen valmistumisvuosien
   mukaan. Erillisellä rivillä on mukana myös hankkeet, joilta valmistumisvuosi puuttuu.
   Niille vuosi arvioidaan hankevaiheesta: esiselvitys +4, suunnittelu +3,
   investointipäätös +2 ja käynnistys +1 vuotta uutisoinnista.

Kaikki sarjat katkaistaan neljännekseen 2029Q4.

## Rakenne

```
R/paivita_data.R      hakee aineistot ja laskee vuosimuutokset -> app/data/heatmap.csv
R/ennuste.R           ennusteet vuoden 2029 loppuun
data/ennusteoletukset.csv  julkaistut vuosiennusteet ja niiden lähteet
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
