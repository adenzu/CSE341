%{
#include "y.tab.h"
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int parc = 0;

int sel = 0;
char ses[1024];
int PSE() {
	if (!sel) return 0;
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
{wsp}		{if (PSE()) return SYNTAX_ERROR;}
and		    {if (PSE()) return SYNTAX_ERROR; return KW_AND;}
or		    {if (PSE()) return SYNTAX_ERROR; return KW_OR;}
not		    {if (PSE()) return SYNTAX_ERROR; return KW_NOT;}
equal		{if (PSE()) return SYNTAX_ERROR; return KW_EQUAL;}
less		{if (PSE()) return SYNTAX_ERROR; return KW_LESS;}
greater     {if (PSE()) return SYNTAX_ERROR; return KW_GREATER;}
list		{if (PSE()) return SYNTAX_ERROR; return KW_LIST;}
append		{if (PSE()) return SYNTAX_ERROR; return KW_APPEND;}
concat		{if (PSE()) return SYNTAX_ERROR; return KW_CONCAT;}
set		    {if (PSE()) return SYNTAX_ERROR; return KW_SET;}
deffun		{if (PSE()) return SYNTAX_ERROR; return KW_DEFFUN;}
defvar      {if (PSE()) return SYNTAX_ERROR; return KW_DEFVAR;}
for		    {if (PSE()) return SYNTAX_ERROR; return KW_FOR;}
while       {if (PSE()) return SYNTAX_ERROR; return KW_WHILE;}
progn       {if (PSE()) return SYNTAX_ERROR; return KW_RPOGN;}
if		    {if (PSE()) return SYNTAX_ERROR; return KW_IF;}
exit		{if (PSE()) return SYNTAX_ERROR; return KW_EXIT;}
true		{if (PSE()) return SYNTAX_ERROR; return KW_TRUE;}
false		{if (PSE()) return SYNTAX_ERROR; return KW_FALSE;}
"+"		    {if (PSE()) return SYNTAX_ERROR; return OP_PLUS;}
"-"		    {if (PSE()) return SYNTAX_ERROR; return OP_MINUS;}
"/"		    {if (PSE()) return SYNTAX_ERROR; return OP_DIV;}
"*"		    {if (PSE()) return SYNTAX_ERROR; return OP_MULT;}
"("		    {if (PSE()) return SYNTAX_ERROR; parc++; return OP_OP;}
")"		    {if (PSE()) return SYNTAX_ERROR; parc--; return OP_CP;}
"**"		{if (PSE()) return SYNTAX_ERROR; return OP_DBLMULT;}
","		    {if (PSE()) return SYNTAX_ERROR; return OP_COMMA;}
{cmt}		{if (PSE()) return SYNTAX_ERROR;}
{int}		{if (PSE()) return SYNTAX_ERROR; strcpy(yylval.str, yytext); return VALUEI;}
{frc}		{if (PSE()) return SYNTAX_ERROR; strcpy(yylval.str, yytext); return VALUEF;}
{idt}		{if (PSE()) return SYNTAX_ERROR; strcpy(yylval.str, yytext); return ID;}
{int}{ltr}+	|
0+		    |
{int}+f0{int}*	{return SYNTAX_ERROR;}
.		    {ASE();}
<<EOF>>		{}
%%


int yywrap(){return 1;}
