GUILD = guild
RUN64 = guile-run64

top_level_module_dir = dump
sources = $(wildcard $(top_level_module_dir)/*.scm)
objects = $(sources:.scm=.go)

# Build

all: $(objects)

%.go: %.scm
	$(GUILD) compile -L . -o $@ $<

# Tests

.PHONY: check
check: tests.scm
	GUILE_LOAD_PATH=$(GUILE_LOAD_PATH):. $(RUN64) $^

# Clean

clean:
	rm -f $(objects)
