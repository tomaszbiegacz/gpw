# Warsaw Stock Exchange playground with R

Data has been retrieved from <http://bossa.pl/notowania/metastock/>.

## Getting started

Plot stock values with

```bash
Rscript ./tests/data/gpw-daily-data-plot.R
```

this will create `Rplots.pdf` in current directory.

## Development

Start with [RStudio](https://www.rstudio.com/).
Run tests with

```R
devtools::test()
```


