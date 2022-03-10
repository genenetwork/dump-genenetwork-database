RUN64 = guile-run64

.PHONY: check
check: tests.scm
	GUILE_LOAD_PATH=$(GUILE_LOAD_PATH):. $(RUN64) $^
