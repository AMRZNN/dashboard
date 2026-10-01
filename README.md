# AMR Dashboard

Shiny-dashboard van AMR Zorgnetwerk Noord-Nederland met geaggregeerde surveillancedata
(BRMO en respiratoire virussen).

## Installatie

Package-versies worden vastgelegd met [renv](https://rstudio.github.io/renv/) in `renv.lock`.

1. Open `amr-dashboard.Rproj` in RStudio (of start R in de projectmap).
   renv wordt automatisch geactiveerd via `.Rprofile`; ontbreekt renv, dan installeert het zichzelf.
2. Installeer de vastgelegde packages:

   ```r
   renv::restore()
   ```

   Op Linux zijn systeembibliotheken nodig (GDAL, GEOS, PROJ, udunits, libuv), bijvoorbeeld:
   `sudo apt-get install libgdal-dev libgeos-dev libproj-dev libudunits2-dev libuv1-dev`.

## Starten

```r
shiny::runApp()
```

## Packages toevoegen of bijwerken

```r
renv::install("pakketnaam")   # of renv::update()
renv::snapshot()              # leg de wijziging vast in renv.lock
```

Commit daarna `renv.lock`. Laad nieuwe packages met een expliciete `library()`-aanroep in
`R/bootstrap.R`; anders neemt renv ze niet op in `renv.lock`.
