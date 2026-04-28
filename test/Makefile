define SBCL_OPT
--eval "(require 'asdf)" \
--eval "(push #P\"${PWD}/\" asdf:*central-registry*)" \
--eval "(asdf:load-asd \"test.asd\")" \
--eval "(asdf:load-system \"unit-test\")"
endef

test:
	sbcl ${SBCL_OPT}

