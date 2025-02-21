# SPDX-FileCopyrightText: 2025 Hilton Chain <hako@ultrarare.space>
#
# SPDX-License-Identifier: CC0-1.0

ARGS  := --verbosity=1

# Required from host environment.
EMACS := emacs
GUIX  := guix
SUDO  := sudo --preserve-env

# Available in guix shell environment, set up direnv to load.
FIND  := find
GIT   := git
GUILD := guild

%.scm: %.org
	@$(EMACS) -Q --batch --eval \
	"(progn \
	   (require 'ob-tangle) \
	   (setopt org-babel-load-languages '((shell . t)) \
	           org-confirm-babel-evaluate nil) \
	   (with-current-buffer (find-file-noselect \"$<\") \
	     (org-babel-tangle)))"

%.go: %.scm
	$(GUILD) compile --output=$@ $<

load_dirs = \
	external/nonguix \
	external/rosenthal \
	external/sops-guix/modules \
	modules

objects = $(subst .scm,.go,$(shell $(FIND) $(load_dirs) -name '*.scm'))

.PHONY: compile compile-guix compile-deps
compile: compile-deps config/dorphine.go config/gokuraku.go
compile-guix:
	@[ -x external/guix/scripts/guix ] || \
		(cd external/guix && ./bootstrap && ./configure)
	$(MAKE) -C external/guix

compile-deps: compile-guix $(objects)
config/dorphine.go config/gokuraku.go: $(objects)

.PHONY: build build-dorphine build-gokuraku
build: build-dorphine build-gokuraku
build-dorphine build-gokuraku: config/dorphine.scm config/gokuraku.scm compile-deps
	$(GUIX) system build $(subst build-,config/,$@).scm $(ARGS)

.PHONY: reconfigure
reconfigure: config/dorphine.scm compile-deps
	$(SUDO) $(GUIX) system reconfigure $< $(ARGS)

.PHONY: deploy
deploy: config/gokuraku.scm compile-deps
	$(GUIX) deploy files/blobs/deploy $(ARGS)

.PHONY: ares
# Load reader extensions before starting nREPL server.
ares: compile-deps
	@$(GUIX) shell guile-next guile-ares-rs -- guile -c \
	"(begin \
	   (use-modules (guix gexp) \
	                (gnu home services emacs)) \
	   ((@ (ares server) run-nrepl-server)))"

.PHONY: authenticate
authenticate:
	@$(GUIX) git authenticate c5d46fdfdfbc84fe413f1d930049d1f703f9a0ff \
		"F4C2 D1DF 3FDE EA63 D1D3  0776 ACC6 6D09 CA52 8292"

.PHONY: clean
clean:
	-$(RM) --recursive config/*.go config/*.scm $(objects)
	$(MAKE) -C external/guix clean
