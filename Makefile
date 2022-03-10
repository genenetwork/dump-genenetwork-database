GUILE = guile

.PHONY: check
check: tests.scm
	GUILE_LOAD_PATH=$(GUILE_LOAD_PATH):. $(GUILE) $^
