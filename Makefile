define SBCL_OPT
--eval "(require 'asdf)" \
--eval "(push #P\"${PWD}/\" asdf:*central-registry*)" \
--eval "(asdf:load-asd \"monitor.asd\")" \
--eval "(asdf:load-system \"monitor\")"
endef

all:
	sudo sbcl ${SBCL_OPT}

