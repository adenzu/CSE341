%{
#include <ctype.h>
int sel = 0;
char ses[1024];
int PSE() {
	if (!sel) return 0;
	printf("SYNTAX_ERROR ");
	for (int i = 0; i < sel; i++) {
		printf("%c", ses[i]);
	}
	printf(" cannot be tokenized\n");
	sel = 0;
	return 1;
}
void ASE() {
	ses[sel] = yytext[0];
	sel += 1;
}
%}

wsp	[ \t\r\n]+
dig	[0-9]
ltr	[a-zA-Z]
cmt	;;.*$
int	0|[1-9]{dig}*
frc	{int}+f[1-9]{int}*
idt	({ltr}|_)({dig}|{ltr}|_)*
str	\".*\"

%%
{wsp}		{if(PSE()) return 0;}
and			|
or			|
not			|
equal		|
less		|
nil			|
list		|
append		|
concat		|
set			|
deffun		|
for			|
if			|
exit		|
load		|
disp		|
true		|
false		{if(PSE()) return 0; printf("%s: KW_", yytext); for (int i = 0; yytext[i] != 0; i++) printf("%c", toupper(yytext[i])); printf("\n");}
"+"			{if(PSE()) return 0; printf("%s: OP_PLUS\n", yytext);}
"-"			{if(PSE()) return 0; printf("%s: OP_MINUS\n", yytext);}
"/"			{if(PSE()) return 0; printf("%s: OP_DIV\n", yytext);}
"*"			{if(PSE()) return 0; printf("%s: OP_MULT\n", yytext);}
"("			{if(PSE()) return 0; printf("%s: OP_OP\n", yytext);}
")"			{if(PSE()) return 0; printf("%s: OP_CP\n", yytext);}
"**"		{if(PSE()) return 0; printf("%s: OP_DBLMULT\n", yytext);}
","			{if(PSE()) return 0; printf("%s: OP_COMMA\n", yytext);}
{cmt}		{if(PSE()) return 0; for (int i = 0; yytext[i] != 0; i++) if (yytext[i] == '\r') yytext[i] = 0; printf("%s: COMMENT\n", yytext);}
{int}		{if(PSE()) return 0; printf("%s: VALUEI\n", yytext);}
{frc}		{if(PSE()) return 0; printf("%s: VALUEF\n", yytext);}
{str}		{if(PSE()) return 0; for (int i = 0; yytext[i] != 0; i++) if (yytext[i + 1] == 0) yytext[i] = 0; printf("\": OP_OC\n%s: VALUESTR\n\": OP_CC\n", &yytext[1]);}
{idt}		{if(PSE()) return 0; printf("%s: IDENTIFIER\n", yytext);}
{int}{ltr}+	|
0+			|
{int}+f0{int}*	{printf("SYNTAX_ERROR %s cannot be tokenized\n", yytext); return 0;}
.			{ASE();}
<<EOF>>		{return 0;}
%%


int yywrap(){}
int main(int argc, char ** argv) {
	if (argc > 1) yyin = fopen(argv[1], "r");
	yylex();
	return 0;
}
