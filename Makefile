define SBCL_OPT
--eval "(require 'asdf)" \
--eval "(push #P\"${PWD}/\" asdf:*central-registry*)" \
--eval "(push #P\"${PWD}/src/\" asdf:*central-registry*)" \
--eval "(push #P\"${PWD}/dsl/\" asdf:*central-registry*)" \
--eval "(asdf:load-asd \"base-tools\")" \
--eval "(asdf:load-asd \"dsl\")" \
--eval "(asdf:load-asd \"monitor\")" \
--eval "(asdf:load-asd \"monitor.asd\")" \
--eval "(asdf:load-system \"monitor\")"
endef

all:
	sudo sbcl ${SBCL_OPT}

