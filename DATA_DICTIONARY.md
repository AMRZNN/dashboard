# Datadictionary — AMR Dashboard

Overzicht van de datasets die het dashboard gebruikt.
Profiel opgemaakt op 1 oktober 2026; aantallen rijen en periodes veranderen bij elke data-update.

| Dataset | Bron / pad | Gebruikt in |
|---|---|---|
| [Certe BRMO](#certe-brmo--brmo_maandcsv) | GitHub `AMRZNN/dashboard_data` → `certe/brmo_maand.csv` | Laboratoria (BRMO) |
| [Certe respiratoir](#certe-respiratoir--respiratoir_maandcsv) | GitHub `AMRZNN/dashboard_data` → `certe/respiratoir_maand.csv` | GGD + Laboratoria (Respiratoir), Top 4 |
| [Regio-incidentie (fictief)](#regio-incidentie-fictief--dataregio_incidentiecsv) | `data/regio_incidentie.csv` | GGD (BRMO), Top 4 — **fictieve data** |
| [NUTS3-geodata](#nuts3-geodata--datageo_nuts3rds) | `data/geo_nuts3.rds` | Alle kaarten, noemer per 100.000 |

Paden staan in `config.yml` onder `paths:`; inlezen gebeurt in `R/data_service.R`.

---

## Gemeenschappelijke sleutels

Deze kolommen komen in meerdere datasets voor en worden gebruikt om te koppelen en te filteren.

| Kolom | Type | Betekenis | Waarden |
|---|---|---|---|
| `jaar` | geheel getal | Kalenderjaar van de meldingen | bv. `2015` … `2026` |
| `maand` | geheel getal | Kalendermaand | `1` … `12` |
| `provincie` | tekst | Provincie | `Groningen`, `Friesland`, `Drenthe` (+ `Overijssel`, `Flevoland` in Certe-data) |
| `nuts3` | tekst | COROP/NUTS3-regio; koppelsleutel met `geo_nuts3.rds` | zie hieronder |

De app maakt van `jaar` + `maand` een `datum` (eerste dag van de maand). De meest recente maand bepaalt de Top 4 en de KPI-titel.

**NUTS3-regio's Noord-Nederland** (lijst in `config.yml` → `geo.noord_nuts3`, met inwoners uit `geo_nuts3.rds`):

| Provincie | NUTS3-regio | Inwoners |
|---|---|---:|
| Groningen | Delfzijl en omgeving | 45.585 |
| Groningen | Oost-Groningen | 147.805 |
| Groningen | Overig Groningen | 393.530 |
| Friesland | Noord-Friesland | 327.095 |
| Friesland | Zuidoost-Friesland | 188.090 |
| Friesland | Zuidwest-Friesland | 136.245 |
| Drenthe | Noord-Drenthe | 192.810 |
| Drenthe | Zuidoost-Drenthe | 167.930 |
| Drenthe | Zuidwest-Drenthe | 133.965 |

De Certe-bestanden bevatten daarnaast `Flevoland` en `Noord-Overijssel`; die vallen buiten het dashboard (gefilterd op provincie/regio).

---

## Certe BRMO — `brmo_maand.csv`

Maandelijkse aantallen BRMO-meldingen uit het Certe-laboratorium, per NUTS3-regio.

- **Bron:** `https://raw.githubusercontent.com/AMRZNN/dashboard_data/main/certe/brmo_maand.csv` (`paths.certe`)
- **Granulariteit:** één rij per `jaar` × `maand` × `nuts3`; geen dubbele combinaties
- **Periode:** januari 2015 t/m september 2026 · 1.523 rijen · 14 kolommen
- **Verversing:** de app controleert elke 5 minuten (`reactiveFileReader`)
- **Ontbrekende combinaties:** niet elke maand × regio komt voor; een ontbrekende rij telt in de app als 0 meldingen

| Kolom | Type | Betekenis | Bereik |
|---|---|---|---|
| `jaar`, `maand`, `provincie`, `nuts3` | — | Zie [gemeenschappelijke sleutels](#gemeenschappelijke-sleutels) | — |
| `esbl` | aantal | ESBL-producerende Enterobacterales | 0 – 40 |
| `mrsa` | aantal | Meticilline-resistente *Staphylococcus aureus* | 0 – 26 |
| `vre` | aantal | Vancomycine-resistente enterococcen | 0 – 16 |
| `cpe` | aantal | Carbapenemase-producerende Enterobacterales | 0 – 3 |
| `cre` | aantal | Carbapenem-resistente Enterobacterales | 0 – 3 |
| `mrpa` | aantal | Multiresistente *Pseudomonas aeruginosa* | 0 – 3 |
| `facre` | aantal | BRMO-categorie FACRE ¹ | 0 – 4 |
| `fara` | aantal | BRMO-categorie FARA ¹ | 0 – 2 |
| `cpa` | aantal | BRMO-categorie CPA ¹ | 0 – 1 |
| `ca` | aantal | BRMO-categorie CA ¹ | 0 – 1 |

¹ De volledige definitie van deze afkortingen staat niet in de code; graag bevestigen bij Certe.

Alle telkolommen zijn gehele getallen ≥ 0 zonder ontbrekende waarden. De eenheid ("meldingen") is zoals de app ze noemt; of het om unieke patiënten of isolaten gaat, staat niet in de data.

Weergavenamen (hoofdletters) staan in `.BRMO_LABELS` in `R/mod_tab_laboratoria.R` en `R/mod_tab_ggd.R`.

---

## Certe respiratoir — `respiratoir_maand.csv`

Maandelijkse aantallen meldingen van respiratoire verwekkers uit het Certe-laboratorium, per NUTS3-regio.

- **Bron:** `https://raw.githubusercontent.com/AMRZNN/dashboard_data/main/certe/respiratoir_maand.csv` (`paths.respiratoir`)
- **Granulariteit:** één rij per `jaar` × `maand` × `nuts3`; geen dubbele combinaties
- **Periode:** juni 2024 t/m september 2026 · 296 rijen · 18 kolommen
- **Verversing:** de app controleert elke 5 minuten
- **Let op:** de kolomnamen zijn de volledige virusnamen, met spaties en `/`. In R verwijs je ernaar met backticks, bijvoorbeeld `` `Influenza A` ``.

| Kolom | Type | Betekenis | Bereik |
|---|---|---|---|
| `jaar`, `maand`, `provincie`, `nuts3` | — | Zie [gemeenschappelijke sleutels](#gemeenschappelijke-sleutels) | — |
| `Influenza A` | aantal | Influenza A-virus | 0 – 295 |
| `Influenza B` | aantal | Influenza B-virus | 0 – 28 |
| `RSV` | aantal | Respiratoir syncytieel virus | 0 – 62 |
| `SARS-CoV-2` | aantal | SARS-CoV-2 (COVID-19) | 0 – 91 |
| `Rhinovirus` | aantal | Rhinovirus | 0 – 11 |
| `Rhinovirus/enterovirus` | aantal | Rhino- of enterovirus, niet nader onderscheiden | 0 – 31 |
| `Enterovirus` | aantal | Enterovirus | 0 – 12 |
| `Adenovirus` | aantal | Adenovirus | 0 – 8 |
| `Humaan metapneumovirus` | aantal | Humaan metapneumovirus (hMPV) | 0 – 13 |
| `Mycoplasma pneumoniae` | aantal | *Mycoplasma pneumoniae* (bacterie) | 0 – 10 |
| `Parainfluenzavirus type 1` | aantal | Parainfluenzavirus type 1 | 0 – 2 |
| `Parainfluenzavirus type 2` | aantal | Parainfluenzavirus type 2 | 0 – 3 |
| `Parainfluenzavirus type 3` | aantal | Parainfluenzavirus type 3 | 0 – 7 |
| `Parainfluenzavirus type 4` | aantal | Parainfluenzavirus type 4 | 0 – 4 |

De lijst met virussen staat ook in `config.yml` (`respiratoir.alle_virussen`). Een nieuw virus in de data verschijnt pas in het dashboard als het daar én in `R/app_ui.R` (`resp_items_raw`) wordt toegevoegd. Verkorte KPI-labels staan in `korten()` in `R/components/mod_kpi.R`.

---

## Regio-incidentie (fictief) — `data/regio_incidentie.csv`

**Gegenereerde, fictieve BRMO-data** voor het tabblad "GGD (FICTIEF)". Zelfde structuur als Certe BRMO, maar alleen Noord-Nederland.

- **Pad:** `data/regio_incidentie.csv` (`paths.regio`)
- **Granulariteit:** één rij per `jaar` × `maand` × `nuts3`; volledig (10 jaar × 12 maanden × 9 regio's)
- **Periode:** januari 2015 t/m december 2024 · 1.080 rijen · 14 kolommen
- **Verversing:** elke 5 seconden gecontroleerd op wijzigingen

| Kolom | Type | Betekenis | Bereik |
|---|---|---|---|
| `jaar`, `maand`, `provincie`, `nuts3` | — | Zie [gemeenschappelijke sleutels](#gemeenschappelijke-sleutels) | — |
| `esbl` | aantal | Zie Certe BRMO | 0 – 16 |
| `mrsa` | aantal | idem | 0 – 7 |
| `vre` | aantal | idem | 0 – 2 |
| `cpe` | aantal | idem | 0 – 1 |
| `mrpa` | aantal | idem | 0 – 3 |
| `facre`, `cre`, `fara`, `cpa`, `ca` | aantal | idem — in deze dataset altijd 0 | 0 |

---

## NUTS3-geodata — `data/geo_nuts3.rds`

Grenzen en inwonertallen van alle 40 NUTS3-regio's in Nederland (`sf`-object).

- **Pad:** `data/geo_nuts3.rds` (`paths.shape`); eenmalig ingelezen bij opstarten
- **Geometrie:** `MULTIPOLYGON`, coördinatenstelsel EPSG:28992 (RD New); de app zet om naar WGS 84 voor de kaart
- **Rijen:** 40 · kolommen: 3 + geometrie

| Kolom | Type | Betekenis | Bereik |
|---|---|---|---|
| `nuts3` | tekst | Naam van de NUTS3-regio; koppelsleutel | 40 unieke namen |
| `inwoners` | aantal | Aantal inwoners; noemer voor "per 100.000" | 45.585 – 1.461.340 |
| `oppervlakte_km2` | getal | Oppervlakte in km² | 128 – 1.842 |
| `geometry` | geometrie | Regiogrens | — |

Berekening per 100.000: `meldingen / som(inwoners van de geselecteerde regio's) × 100.000`. Het peiljaar van de inwonertallen staat niet in het bestand.

---

## Aandachtspunten

- **Fictieve data:** het tabblad GGD draait volledig op `regio_incidentie.csv`. Vervang die door echte GGD-data zodra die beschikbaar is; dezelfde kolommen volstaan.
- **Afkortingen:** de definities van FACRE, FARA, CPA en CA moeten nog bevestigd worden.
