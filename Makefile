-include Makefile.config

DEFAULT_FLAGS?=-default-type int64 -default-type word64

MPL_FLAGS?=-detect-entanglement true -disable-pass splitTypes1 -disable-pass splitTypes2

FFI_FLAGS?=-default-ann 'allowFFI true' -export-header export.h -link-opt '-lgsl -lgslcblas -lm -lblas -llapack -llapacke'
FFI_FILES=lapack-groupfactor.c

MPL=/home/jatina/entangle-allow-mpl/mpl/build/bin/mpl

MLTON=/home/swestric/installs/mlton/build/bin/mlton

.PHONY: all clean realclean phony

default:
	@echo "usage: make <file>.bin"

clean:
	rm -f bin/*.bin* bin/*.log bin/*.c bin/*.s

phony:

%.mlton.bin: phony
	@mkdir -p bin
	$(MLTON) -mlb-path-var 'COMPAT mlton' $(DEFAULT_FLAGS) -output bin/$@ $*.mlb

%.ffi.bin: phony
	@mkdir -p bin
	$(MPL) -mlb-path-var 'COMPAT mpl' $(FFI_FLAGS) $(DEFAULT_FLAGS) $(MPL_FLAGS) -output bin/$@ $*.mlb $(FFI_FILES)

%.bin: phony
	@mkdir -p bin
	$(MPL) -mlb-path-var 'COMPAT mpl' $(DEFAULT_FLAGS) $(MPL_FLAGS) -output bin/$@ $*.mlb
