SHELL := $(or $(CONFIG_SHELL),bash)
BSB := ./node_modules/.bin/bsb
BSB_ARGS := -make-world
BSEXTENSIONS := "ml,mli,re,rei,res,resi"
SOURCEDIRS := ./lib/bs/.sourcedirs.json

all: serve

serve:
	trap 'kill %1' INT TERM
	pnpm run dev & $(MAKE) watch

$(SOURCEDIRS): bsconfig.json
	$(BSB) -install

bs:
	$(BSB) $(BSB_ARGS)

watch: $(SOURCEDIRS)
	./node_modules/.bin/redemon \
	--paths="$(shell jq -r 'include "./dirs"; dirs' $(SOURCEDIRS))" \
	--extensions=$(BSEXTENSIONS) \
	--delay 1000 \
	-v \
	$(MAKE) bs

print-%: ; @echo $*=$(*)

clean:
	$(BSB) -clean-world

bsdirs: $(SOURCEDIRS)
	echo $(shell jq -r 'include "./dirs"; dirs' $(SOURCEDIRS))

.PHONY: bs bsdirs all clean watch
