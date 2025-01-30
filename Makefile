# REMOTEDIR is the remote directory on the server
SSH_HOSTNAME=red.moffitt.org
REMOTEDIR = /share/dept_bbsr/Projects/Eschrich_Steven/Shiny_Reports

# PUBDIR is the local website output directory (output-dir option in _quarto.yml)
PUBDIR = $(shell grep output-dir _quarto.yml | sed -e's/^.*:[ ]*//' | sed -e's/[ ]*$$//')
# DESTDIR is the main directory on the remote server (the title of the index.qmd file).
DESTDIR = $(shell grep title index.qmd | sed -e's/^.*:[ ]*//' | sed -e's/"//g' | sed -e's/[ ]*$$//')

OS = $(shell uname)

# Fix for running under MacOS with decryption at border.
ifeq ($(OS), Darwin)
		export DENO_TLS_CA_STORE=system
endif


all:
	@echo "Targets are: init-site, render, publish, data-raw"

init-site:
	ssh $(SSH_HOSTNAME) mkdir -p $(REMOTEDIR)/$(DESTDIR)

render:
	quarto render

publish:
	rsync -avz $(PUBDIR)/ $(SSH_HOSTNAME):$(REMOTEDIR)/$(DESTDIR)

.PHONY: data-raw
data-raw:
	Rscript -e 'projcycle::build_data()'

clean:
	rm -rf public
