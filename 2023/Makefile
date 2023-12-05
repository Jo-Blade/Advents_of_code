.PHONY: all dir clean latest

CC = gcc
FLAGS = -g

BUILDDIR = bin

SOURCES = $(wildcard *.c)
BINARIES = $(patsubst %.c,$(BUILDDIR)/%,$(SOURCES))
BINARIES_RUN = $(patsubst %.c,%,$(SOURCES))

LATEST_SOURCE = $(shell ls day*.c | tail -n 1)
LATEST = $(patsubst %.c,%,$(LATEST_SOURCE))

all: dir $(BINARIES)

dir:
	mkdir -p $(BUILDDIR)

# compiler et executer le dernier code
latest: $(LATEST)

# pouvoir compiler et executer un code
$(BINARIES_RUN): % : $(BUILDDIR)/%
	$<

# pouvoir compiler un binaire dans le dossier de compilation
$(BINARIES): $(BUILDDIR)/% : %.c
	$(CC) $(FLAGS) $< -o $@

clean:
	rm $(BUILDDIR)/*
