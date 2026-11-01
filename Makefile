PY := PYTHONPATH=src/py .venv/bin/python
MOD := $(PY) -m btpdata

.PHONY: all test data analysis report

all: test data analysis report

test:
	PYTHONPATH=src/py .venv/bin/pytest -q
	Rscript -e 'testthat::test_dir("tests/R")'

data:
	$(MOD).check_raw
	$(MOD).fetch_phonepe
	$(MOD).parse_phonepe
	$(MOD).extract_ncp
	$(MOD).population
	$(MOD).cpi
	$(PY) -c 'from btpdata.boundaries import fetch; fetch()'
	$(MOD).boundaries
	$(MOD).build_panels
	$(MOD).npci
	$(MOD).validation

analysis:
	Rscript src/R/01_descriptives.R

report:
	cd thesis/btp1 && latexmk -pdf -interaction=nonstopmode main.tex
	cd thesis/btp1-slides && latexmk -pdf -interaction=nonstopmode slides.tex
	cd thesis/progress && for f in part*.tex; do latexmk -pdf -interaction=nonstopmode $$f || exit 1; done
