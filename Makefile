
.PHONY: all

TARGETS := $(wildcard *.hs)


doctest_all :
	@$(foreach file, $(TARGETS), echo $(file); doctest $(file);)
