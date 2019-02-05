# Warsaw Stock Exchange playground with R

Data has been retrieved from <http://bossa.pl/notowania/metastock/>.

## Getting started

Rebuild sample data set with

```bash
Rscript ./playground/gpw-daily-import-sample.R
```

This can take a while. Once done plot stock values with

```bash
Rscript ./playground/gpw-daily-data-plot.R
```

this will create `Rplots.pdf` in current directory.

## Development

Start with [RStudio](https://www.rstudio.com/).
Run tests with

```R
devtools::test()
```
