package = chez-sxml-mini
chez = scheme
install = install -D
prefix = ~/.chez.d
libdir = $(prefix)/lib

chezversion ::= $(shell echo '(call-with-values scheme-version-number (lambda (a b c) (format \#t "~d.~d" a b)))' | ${chez} -q)
schemedir = ${libdir}/csv${chezversion}-site

build:
	echo "(compile-library \"sxml-mini.sls\")" | ${chez} -q

install:
	find . -type f -regex ".*.so" -exec sh -c '${install} -t ${schemedir}/$$(dirname $$1) $$1' _ {} \;

install-src:
	find . -type f -regex ".*.scm" -exec sh -c '${install} -t ${schemedir}/$$(dirname $$1) $$1' _ {} \;
	find . -type f -regex ".*.sls" -exec sh -c '${install} -t ${schemedir}/$$(dirname $$1) $$1' _ {} \;

clean:
	find . -name "*.so" -exec rm {} \;
	find . -name "*.html" -exec rm {} \;
	find . -name "*~" -exec rm {} \;



