COMMON= src/source/ir.sml src/source/tiger.sml src/target/mips.sml src/target/temp.sml src/target/translate.sml


src/source/%.lex.sml: src/source/%.lex
	mllex $<

src/source/%.grm.sml: src/source/%.grm
	mlyacc $<


all: tc

.PHONY: all clean test

clean:
	rm -f src/source/*.lex.sml
	rm -f src/source/*.grm.sml src/source/*.grm.desc src/source/*.grm.sig tc

tc: src/tc.sml src/tc.mlb src/source/tiger.grm.sml src/source/tiger.lex.sml ${COMMON}
	mlton -output tc src/tc.mlb


