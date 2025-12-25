EMACS ?= emacs

# Source files
SRCS = src/dragonruby.el \
       src/dragonruby-mode.el \
       src/core/dragonruby-config.el \
       src/core/dragonruby-project.el \
       src/features/dragonruby-colors.el \
       src/features/dragonruby-sprites.el \
       src/features/dragonruby-paths.el

# Compilation output
OBJS = $(SRCS:.el=.elc)

.PHONY: all compile clean

all: compile

compile: $(OBJS)

%.elc: %.el
	$(EMACS) -Q --batch -L src -L src/core -L src/features -f batch-byte-compile $<

clean:
	rm -f $(OBJS)
