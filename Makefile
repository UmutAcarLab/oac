-include Makefile.config

DEFAULT_FLAGS?=-default-type int64 -default-type word64

MPL_FLAGS?=-detect-entanglement true -disable-pass splitTypes1 -disable-pass splitTypes2

FFI_FLAGS?=-default-ann 'allowFFI true' -export-header export.h -link-opt 'quartz.o -lquartz_runtime -lstdc++'
FFI_FILES=lib/quartz/quartz_api.c

MPL=/root/mpl-em/build/bin/mpl

MLTON=mlton

.PHONY: all clean realclean phony

default:
	@echo "usage: make <file>.bin"

clean:
	rm -f bin/*.bin* bin/*.log bin/*.c bin/*.s

phony:


%.mlton.quartz.bin: phony
	@mkdir -p bin
	g++ -Wall -Wextra -Wconversion -Wno-unused-result -Werror -c lib/quartz/quartz.cpp -lquartz_runtime
	$(MLTON) -mlb-path-var 'COMPAT mlton' $(FFI_FLAGS) -debug true -const 'Exn.keepHistory true' $(DEFAULT_FLAGS) -output bin/$@ $*.mlb $(FFI_FILES)

%.mlton.bin: phony
	@mkdir -p bin
	$(MLTON) -mlb-path-var 'COMPAT mlton' -const 'Exn.keepHistory true' $(DEFAULT_FLAGS) -output bin/$@ $*.mlb

%.mpl.quartz.bin: phony
	@mkdir -p bin
	g++ -Wall -Wextra -Wconversion -Wno-unused-result -Werror -c lib/quartz/quartz.cpp -lquartz_runtime
	$(MPL) -mlb-path-var 'COMPAT mpl' $(FFI_FLAGS) $(DEFAULT_FLAGS) $(MPL_FLAGS) -debug true -output bin/$@ $*.mlb $(FFI_FILES)

%.bin: phony
	@mkdir -p bin
	$(MPL) -mlb-path-var 'COMPAT mpl' $(DEFAULT_FLAGS) $(MPL_FLAGS) -output bin/$@ $*.mlb
