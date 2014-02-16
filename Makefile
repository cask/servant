EMACS ?= emacs
CASK ?= cask

all: test

test: unit ecukes

unit:
	${CASK} exec ert-runner

ecukes:
	${CASK} exec ecukes

.PHONY:	all unit ecukes test
