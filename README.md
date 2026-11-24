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
| Datakeskusten rakentaminen | rakennusajan mukaan: rakennusaikaiset investoinnit (M€), rakenteilla oleva kapasiteetti (MW) ja hankkeet (kpl), investointipäätöksen tai käynnistyksen saaneiden investoinnit; toimistokoneiden ja atk-laitteiden tuonti (SITC 75, 752); teollisuus- ja varastorakennusten aloitukset (välillinen mittari) | Uutisointi (Dataikkuna), Tulli (Uljas), Tilastokeskus raku/156f |
| T&K-investoinnit | investoinnit henkisiin omaisuustuotteisiin (neljännesvuosittain); T&K-menot (vuosittain, näkyy Q4:llä) | Tilastokeskus ntp/132h, tkke/13ri |
| Investoinnit yhteensä | kiinteän pääoman bruttomuodostus; yksityiset ja julkiset investoinnit; investointilajit | Tilastokeskus ntp/132h |

Sovellus toimii myös puhelimessa. Kapealla näytöllä sarjan nimi näkyy oman
rivinsä yläpuolella, ja näkyvissä on niin monta neljännestä kuin näytölle mahtuu
(viimeisimmät toteutuneet ja lähiajan ennuste). Aikaväliä voi vaihtaa valikosta.
Solua napauttamalla näkee tarkat luvut.

Sovelluksen *Lähteet ja menetelmät* -välilehdellä näkyy jokaisen sarjan yksikkö,
lähdetaulukko, vertailutapa ja viimeisin neljännes.

## Ennusteet kuluvan vuoden + 2 vuoden loppuun

Toteutuneiden tietojen jälkeen jokaiselle sarjalle lasketaan ennuste kuluvan vuoden + 2 vuoden
loppuun (`R/ennuste.R`). Sovelluksessa ennustesolut näkyvät vaaleampina ja
katkoviivalla reunustettuina, ja ennusteet voi piilottaa.

1. **Julkaistut ennusteet** (`data/ennusteoletukset.csv`): Taulukossa on vuosimuutokset,
   jotka on poimittu uutisoinnista ja toimialojen julkaisuista. Lähteinä ovat
   RT:n suhdannekatsaus lokakuussa 2026 (asunto- ja infrarakentaminen,
   asuntoaloitukset) ja
   Sp-Kodin ja Säästöpankkiryhmän asuntokauppaennuste syyskuussa 2026 (kauppamäärät
   ja hinnat). Vuoden jäljellä olevat neljännekset asetetaan niin, että koko vuoden
   muutos vastaa ennustetta. Taloudellisia katsauksia (esim. VM) ei käytetä
   lähteinä. Taulukkoa voi päivittää käsin, kun uusia ennusteita
   julkaistaan. Sarake `sarja_regex` kertoo, mihin sarjoihin rivi kohdistuu.
2. **Tilastollinen jatke**: Vuosina, joille ei ole julkaistua ennustetta, vuosimuutos
   palautuu edellisestä neljänneksestä kohti sarjan pitkän aikavälin mediaania
   (vuodesta 2015 alkaen, rajattu välille −3…+5 %). Poikkeamasta säilyy 80 %
   neljänneksestä toiseen.
3. **Datakeskukset**: Ennuste on uutisoitu hankeputki ilmoitettujen valmistumisvuosien
   mukaan. Hankkeet, joilta valmistumisvuosi puuttuu, eivät ole mukana
   (valmistumisvuotta ei imputoida).

Kaikki sarjat katkaistaan kuluvan vuoden + 2 vuoden viimeiseen neljännekseen
(vuonna 2026: 2028Q4). Raja siirtyy automaattisesti vuodenvaihteessa.

## Korot

Yleiskuvassa on 12 kk euribor (EKP:n tietopalvelu, `R/korot.R`). Sen muutos
vuoden takaisesta näytetään prosenttiyksikköinä, ja sillä on oma väriasteikko
(sivuvalikko, oletus ± 1 %-yks. = täysi väri). Ennuste perustuu tiedoston
`data/korko_ennuste.csv` korkotason ennustepisteisiin: viimeisimmästä
toteutuneesta neljänneksestä pisteiden välille interpoloidaan suoraan, ja
viimeisen pisteen jälkeen korko pysyy ennallaan. Pisteitä voi päivittää käsin.

## Rakenne

```
R/paivita_data.R      hakee aineistot ja laskee vuosimuutokset -> app/data/heatmap.csv
R/ennuste.R           ennusteet kuluvan vuoden + 2 vuoden loppuun
data/ennusteoletukset.csv  julkaistut vuosiennusteet ja niiden lähteet
app/app.R             Shiny-sovellus (shiny, bslib, ggplot2)
app/data/             sovelluksen käyttämä aineisto (päivittyy automaattisesti)
data/Dataikkuna*.xlsx datakeskusten uutisaineisto
.github/workflows/    päivittäinen päivitys ja julkaisu
```

## Päivitys

GitHub Actions (`.github/workflows/paivita.yml`) toimii näin arkisin (ma–pe)
klo 06.33 UTC eli talviaikaan 8.33 ja kesäaikaan 9.33 Suomen aikaa, kun
Tilastokeskuksen klo 8 julkaisut ovat ilmestyneet. GitHub voi käynnistää
ajastetun ajon ruuhkan vuoksi myöhässä.

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

Hankkeet kohdistetaan rakennusajalleen (`R/datakeskus_ajoitus.R`):

- Valmistumisvuosi otetaan Dataikkunan Valmistuminen-sarakkeesta.
- Valmistumisneljännes ja rakentamisen alku poimitaan uutistekstistä (Info-sarake),
  esim. "valmistuu syksyllä 2026" tai "rakennustyöt on tarkoitus aloittaa vuoden
  2027 ensimmäisellä vuosipuoliskolla". Suhteelliset ilmaukset (esim. "ensi vuonna")
  lasketaan uutisoinnin päivämäärästä.
- Jos tietoa ei ole, valmistuminen on vuoden Q3 ja rakennusaika 6 neljännestä
  (alle 500 M€) tai 8 neljännestä (vähintään 500 M€). Käynnistyneeksi
  uutisoidun hankkeen rakentaminen alkaa viimeistään uutisoinnin neljänneksellä.
- Investointi jaetaan tasan rakennusajan neljänneksille. Kapasiteetti ja
  hankemäärä ovat rakenteilla olevien hankkeiden kanta.
- Hankkeet, joilta valmistumisvuosi puuttuu, ja keskeytetyt hankkeet jätetään pois.

Jokaisen hankkeen ajoitus ja sen peruste tallennetaan tiedostoon
`app/data/datakeskushankkeiden_ajoitus.csv`. Ajoitusta kannattaa tarkistaa
sieltä. Prosenttimuutokset ovat suuria, kun lähtötaso on pieni.

### Tullin tuontitilasto

Datakeskusten laiteinvestointeja kuvataan Tullin Uljas-tietokannan tuontitilastolla
(`R/uljas.R`): SITC 75 "Toimistokoneet ja atk-laitteet" ja SITC 752 "Atk-laitteet",
tuonnin arvo kaikista maista, kuukaudet summattuna neljänneksiksi. Haku käyttää
R-pakettia [uljas](https://github.com/jhuovari/uljas), joka asennetaan GitHub
Actionsissa kiinnitettynä tiettyyn versioon. Haettu aineisto tallennetaan tiedostoon
`data/uljas_tuonti.csv`. Jos haku epäonnistuu, käytetään viimeksi tallennettua
aineistoa.

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
