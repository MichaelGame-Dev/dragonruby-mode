EMACS ?= emacs

# Dynamically find all .el files in src/
SRCS := $(shell find src -name "*.el")
OBJS := $(SRCS:.el=.elc)

.PHONY: all compile clean test

all: compile

compile: $(OBJS)

# Compile an elisp file to .elc
# We include src and subdirectories in load-path (-L) so (require ...) works
%.elc: %.el
	@echo "Compiling $<..."
	@$(EMACS) -Q --batch \
		-L src \
		-L src/core \
		-L src/features \
		-f batch-byte-compile $<

clean:
	@echo "Cleaning compiled files..."
	@rm -f $(OBJS)

test:
	@echo "Unit tests not yet implemented. Skipping."
