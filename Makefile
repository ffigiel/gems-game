.PHONY: clean dev i18n
VERSION_ID := $(shell git describe --always --long --tags --dirty)

.DEFAULT_GOAL:= dist

clean:
	rm -rf dist

dist: clean
	npx vite build

dev:
	npx vite

i18n:
	npx travelm-agency --inline --elm_path=src/I18n.elm i18n
