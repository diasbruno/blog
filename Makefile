PYTHON ?= python3
PORT ?= 8000
DESTINATION ?= ../diasbruno.github.io
export SITE_DESTINATION = $(DESTINATION)

.PHONY: all build serve check clean
all: build

build:
	$(PYTHON) scripts/build.py

serve: build
	$(PYTHON) scripts/serve.py --port $(PORT)

check:
	$(PYTHON) scripts/check.py

clean:
	$(PYTHON) scripts/build.py --clean
