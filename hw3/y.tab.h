/* A Bison parser, made by GNU Bison 3.8.2.  */

/* Bison interface for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015, 2018-2021 Free Software Foundation,
   Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
   especially those whose name start with YY_ or yy_.  They are
   private implementation details that can be changed or removed.  */

#ifndef YY_YY_Y_TAB_H_INCLUDED
# define YY_YY_Y_TAB_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token kinds.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    YYEMPTY = -2,
    YYEOF = 0,                     /* "end of file"  */
    YYerror = 256,                 /* error  */
    YYUNDEF = 257,                 /* "invalid token"  */
    KW_AND = 258,                  /* KW_AND  */
    KW_OR = 259,                   /* KW_OR  */
    KW_NOT = 260,                  /* KW_NOT  */
    KW_EQUAL = 261,                /* KW_EQUAL  */
    KW_LESS = 262,                 /* KW_LESS  */
    KW_GREATER = 263,              /* KW_GREATER  */
    KW_SET = 264,                  /* KW_SET  */
    KW_DEFFUN = 265,               /* KW_DEFFUN  */
    KW_WHILE = 266,                /* KW_WHILE  */
    KW_IF = 267,                   /* KW_IF  */
    KW_EXIT = 268,                 /* KW_EXIT  */
    KW_TRUE = 269,                 /* KW_TRUE  */
    KW_FALSE = 270,                /* KW_FALSE  */
    KW_RPOGN = 271,                /* KW_RPOGN  */
    KW_LIST = 272,                 /* KW_LIST  */
    KW_APPEND = 273,               /* KW_APPEND  */
    KW_CONCAT = 274,               /* KW_CONCAT  */
    KW_DEFVAR = 275,               /* KW_DEFVAR  */
    KW_FOR = 276,                  /* KW_FOR  */
    OP_PLUS = 277,                 /* OP_PLUS  */
    OP_MINUS = 278,                /* OP_MINUS  */
    OP_DIV = 279,                  /* OP_DIV  */
    OP_MULT = 280,                 /* OP_MULT  */
    OP_OP = 281,                   /* OP_OP  */
    OP_CP = 282,                   /* OP_CP  */
    OP_DBLMULT = 283,              /* OP_DBLMULT  */
    OP_COMMA = 284,                /* OP_COMMA  */
    COMMENT = 285,                 /* COMMENT  */
    SYNTAX_ERROR = 286,            /* SYNTAX_ERROR  */
    VALUEI = 287,                  /* VALUEI  */
    VALUEF = 288,                  /* VALUEF  */
    ID = 289                       /* ID  */
  };
  typedef enum yytokentype yytoken_kind_t;
#endif
/* Token kinds.  */
#define YYEMPTY -2
#define YYEOF 0
#define YYerror 256
#define YYUNDEF 257
#define KW_AND 258
#define KW_OR 259
#define KW_NOT 260
#define KW_EQUAL 261
#define KW_LESS 262
#define KW_GREATER 263
#define KW_SET 264
#define KW_DEFFUN 265
#define KW_WHILE 266
#define KW_IF 267
#define KW_EXIT 268
#define KW_TRUE 269
#define KW_FALSE 270
#define KW_RPOGN 271
#define KW_LIST 272
#define KW_APPEND 273
#define KW_CONCAT 274
#define KW_DEFVAR 275
#define KW_FOR 276
#define OP_PLUS 277
#define OP_MINUS 278
#define OP_DIV 279
#define OP_MULT 280
#define OP_OP 281
#define OP_CP 282
#define OP_DBLMULT 283
#define OP_COMMA 284
#define COMMENT 285
#define SYNTAX_ERROR 286
#define VALUEI 287
#define VALUEF 288
#define ID 289

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
union YYSTYPE
{
#line 328 "gpp_interpreter.y"

	float 	real;
	char	str[50];
	exp *	exp_p;
	params * par_p;

#line 142 "y.tab.h"

};
typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;


int yyparse (void);


#endif /* !YY_YY_Y_TAB_H_INCLUDED  */
