README.md: README.qmd
	quarto render README.qmd

install:
	Rscript -e 'devtools::install()'

check:
	Rscript -e 'devtools::check()'

docs:
	Rscript -e 'devtools::document()'
