.PHONY: test

test:
	ros run -e "(asdf:load-asd (probe-file \"./emacy.asd\")) (ql:quickload :emacy/tests) (asdf:test-system :emacy) (uiop:quit 0)"
