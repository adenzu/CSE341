parser: yacc_file lex_file
	gcc y.tab.c
yacc_file: gpp_interpreter.y
	yacc -d gpp_interpreter.y -Wcounterexamples
lex_file: gpp_lexer.l
	lex gpp_lexer.l
