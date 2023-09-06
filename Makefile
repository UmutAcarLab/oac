-include Makefile.config

DEFAULT_FLAGS?=-default-type int64 -default-type word64

MPL_FLAGS?=-detect-entanglement true -disable-pass splitTypes1 -disable-pass splitTypes2

QUARTZ_LINKS=-link-opt 'quartz.o -lquartz_runtime -lstdc++'


FFI_FLAGS?=-default-ann 'allowFFI true' -export-header export.h

MPL=/root/mpl-em/build/bin/mpl

MLTON=/home/swestric/installs/mlton/build/bin/mlton

JAVA_HOME=/usr/lib/jvm/java-17-openjdk-amd64
QUESO_INCLUDE=-I$(JAVA_HOME)/include -I$(JAVA_HOME)/include/linux -L$(JAVA_HOME)/lib/server -ljvm
QUESO_LINKS=-link-opt 'queso.o -lstdc++ -ljvm $(QUESO_INCLUDE)'

.PHONY: all clean realclean phony

default:
	@echo "usage: make <file>.bin"

clean:
	rm -f bin/*.bin* bin/*.log bin/*.c bin/*.s

phony:


%.mlton.quartz.bin: phony
	@mkdir -p bin
	g++ -Wall -Wextra -Wconversion -Wno-unused-result -Werror -c  lib/quartz/quartz.cpp -lquartz_runtime
	$(MLTON) -mlb-path-var 'COMPAT mlton' $(FFI_FLAGS) $(QUARTZ_LINKS) $(DEFAULT_FLAGS) -output bin/$@ $*.mlb lib/quartz/quartz_api.c

%.mlton.queso.bin: phony
	@mkdir -p bin
	g++ -Wall -Wextra -Wconversion -Wno-unused-result -Werror -c lib/queso/queso.cpp $(QUESO_INCLUDE)
	$(MLTON) -mlb-path-var 'COMPAT mlton' $(FFI_FLAGS) $(QUESO_LINKS) $(DEFAULT_FLAGS) -output bin/$@ $*.mlb lib/queso/queso_api.c

%.mlton.bin: phony
	@mkdir -p bin
	$(MLTON) -mlb-path-var 'COMPAT mlton' -const 'Exn.keepHistory true' $(DEFAULT_FLAGS) -output bin/$@ $*.mlb

%.mpl.quartz.bin: phony
	@mkdir -p bin
	g++ -Wall -Wextra -Wconversion -Wno-unused-result -Werror -c lib/quartz/quartz.cpp -lquartz_runtime
	$(MPL) -mlb-path-var 'COMPAT mpl' $(FFI_FLAGS) $(DEFAULT_FLAGS) $(MPL_FLAGS) -output bin/$@ $*.mlb lib/quartz/quartz_api.c

%.bin: phony
	@mkdir -p bin
	$(MPL) -mlb-path-var 'COMPAT mpl' $(DEFAULT_FLAGS) $(MPL_FLAGS) -output bin/$@ $*.mlb
