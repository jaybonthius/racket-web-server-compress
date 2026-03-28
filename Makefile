.PHONY: lint test fmt

RKT_FILES := $(shell find web-server-compress -name '*.rkt' -not -path './.git/*' 2>/dev/null)

lint:
	@for f in $(RKT_FILES); do raco review $$f; done

fmt:
	@for f in $(RKT_FILES); do raco fmt -i $$f; done

test:
	raco test web-server-compress/tests/
