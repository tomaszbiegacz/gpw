# Warsaw Stock Exchange playground with R

Data has been retrieved from <http://bossa.pl/notowania/metastock/>.

## Prerequisites

Before you can start, install [R](https://cran.r-project.org/) in version 3.5. Run `sudo ./install-imports.R` to ensure that all dependencies are in place.

## Getting started

Rebuild sample data set with

```bash
./playground/daily-import.R
```

This can take a while. Once done plot stock values with

```bash
./playground/price-plot.R
```

this will create `Rplots.pdf` in current directory.

## Development

Start with [RStudio](https://www.rstudio.com/).
Run tests with

```R
devtools::test()
```

## Price change density

To see price change density start with

```bash
./playground/price-close-rel-density-plot.R
```