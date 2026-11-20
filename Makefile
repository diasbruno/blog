PYTHON ?= python3
PORT ?= 8000
DESTINATION ?= ../diasbruno.github.io
export SITE_DESTINATION = $(DESTINATION)

.PHONY: all build serve check clean
all: build

build:
	$(PYTHON) _site/scripts/build.py

serve: build
	$(PYTHON) _site/scripts/serve.py --port $(PORT)

check:
	$(PYTHON) _site/scripts/check.py

clean:
	$(PYTHON) _site/scripts/build.py --clean
