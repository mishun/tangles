Binaries := TestCDGenerator TestGraphDrawing TestTangleGenerator TestNonAlternatingGenerator
Libs := stdc++ gsl gslcblas

IncludePath := ../ext/gsl-1.15/include
LibPath := ../ext/gsl-1.15/lib

ObjDir := ../obj
BinDir := ../bin
BinExt := .exe

CFlags := -Wall -Wextra -pipe -O2 $(addprefix -I, $(CURDIR) $(IncludePath)) -MMD
CppFlags := $(CFlags) -std=c++0x
HaskellExts := BangPatterns FlexibleContexts
HaskellFlags := -osuf=hs.o -hisuf=hs.hi -outputdir=$(ObjDir) $(addprefix -X, $(HaskellExts)) -Wall -O2 -optl-static
#-rtsopts -prof -auto-all -caf-all

objFiles = $(addprefix $(ObjDir)/, $(addsuffix $(2), $(1)))
binFiles = $(addprefix $(BinDir)/, $(addsuffix $(BinExt), $(1)))

.PHONY: all
all: DirectoryStructure $(call binFiles, $(Binaries))

.PHONY: DirectoryStructure
DirectoryStructure:
	@mkdir -p $(ObjDir) $(BinDir)


HaskellSrc    := $(shell find */ -name '*.hs')
NonHaskellSrc := $(shell find . -name '*.c') $(shell find . -name '*.cpp')

-include $(call objFiles, $(NonHaskellSrc),.d)

$(call binFiles, %): ../test/%.hs $(HaskellSrc) $(call objFiles, $(NonHaskellSrc),.o)
	ghc --make $(HaskellFlags) -o $@ $< $(addprefix -L, $(LibPath)) $(addprefix -l, $(Libs)) $(call objFiles, $(NonHaskellSrc),.o)


.SECONDARY:
$(call objFiles, %.c,.o): %.c
	@mkdir -p $(dir $@)
	gcc $(CppFlags) -o $@ -c $<

.SECONDARY:
$(call objFiles, %.cpp,.o): %.cpp
	@mkdir -p $(dir $@)
	g++ $(CppFlags) -o $@ -c $<


.PHONY: clean
clean:
	rm -rf $(ObjDir)

.PHONY: hardclean
hardclean: clean
	rm -f $(call binFiles, $(Binaries))
