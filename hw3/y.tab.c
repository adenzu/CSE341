/* A Bison parser, made by GNU Bison 3.8.2.  */

/* Bison implementation for Yacc-like parsers in C

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

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
   especially those whose name start with YY_ or yy_.  They are
   private implementation details that can be changed or removed.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output, and Bison version.  */
#define YYBISON 30802

/* Bison version string.  */
#define YYBISON_VERSION "3.8.2"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1




/* First part of user prologue.  */
#line 1 "gpp_interpreter.y"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

#define true 1.0
#define false 0.0

#define ID_LEN 50

typedef enum op {
	SUM,
	SUB,
	MUL,
	DIV,
	AND,
	OR,
	NOT,
	EQUAL,
	LESS,
	GREATER,
	RETURN,
	RETURN_FROM,
	SET,
	IF,
	EVAL_LIST,
	WHILE,
	CALL_FUNC,
	EXIT,
	NONE
} op;

typedef struct var {
	char name[ID_LEN];
	float value;
} var;

typedef struct scope_node {
	var * variable;
	struct scope_node * upper_scope;
	struct scope_node * next;
} scope_node;

typedef union exp_val {
	char str[ID_LEN];
	float real;
} exp_val;

typedef struct exp {
	op operation;
	struct exp * sub;
	struct exp * next;
	exp_val value;
} exp;

typedef struct params {
	char str[ID_LEN];
	struct params * next;
} params;

typedef struct func {
	char str[ID_LEN];
	params * pars;
	exp * e;
	struct func * next;
} func;

typedef struct float_node {
	float value;
	struct float_node * next;
} float_node;

func * funcs;
scope_node * scope;

float_node * create_float_node(float v) {
	float_node * new_fn = (float_node *) malloc(sizeof(float_node));
	new_fn->value = v;
	new_fn->next = NULL;
	return new_fn;
}

func * get_func(char * str) {
	func * f = funcs;
	while (f != NULL) {
		if (strcmp(f->str, str) == 0) return f;
		f = f->next;
	}
	return f;
}

func * create_func(char * str, params * pars, exp * e) {
	func * new_func = (func *) malloc(sizeof(func));
	strcpy(new_func->str, str);
	new_func->pars = pars;
	new_func->e = e;
	new_func->next = NULL;
	return new_func;
}

void add_func(char * str, params * pars, exp * e) {
	func * f = create_func(str, pars, e);
	f->next = funcs;
	funcs = f;
}

params * add_params(params * p1, params * p2) {
	p1->next = p2;
	return p1;
}

params * create_params(char * str) {
	params * new_params = (params *) malloc(sizeof(params));
	strcpy(new_params->str, str);
	return new_params;
}

exp * add_exp(exp * e1, exp * e2) {
	e1->next = e2;
	return e1;
}

exp * create_exp(op operation, exp * sub) {
	exp * new_exp = (exp *) malloc(sizeof(exp));
	new_exp->operation = operation;
	new_exp->sub = sub;
	new_exp->next = NULL;
	return new_exp;
}

exp * create_exp_str(op operation, exp * sub, char * str) {
	exp * new_exp = create_exp(operation, sub);
	strcpy(new_exp->value.str, str);
	return new_exp;
}

exp * create_exp_real(op operation, exp * sub, float real) {
	exp * new_exp = create_exp(operation, sub);
	new_exp->value.real = real;
	return new_exp;
}

void open_scope() {
	scope_node * new_scope = (scope_node *) malloc(sizeof(scope_node));
	new_scope->variable = NULL;
	new_scope->upper_scope = scope;
	new_scope->next = NULL;
	scope = new_scope;
}

void free_scope(scope_node * _scope) {
	if (_scope != NULL) {
		free_scope(_scope->next);
		free(_scope->variable);
		free(_scope);
	}
}

void close_scope() {
	scope_node * closed_scope = scope;
	scope = scope->upper_scope;
	free_scope(closed_scope);
}

void create_var(char * name) {
	var * new_var = (var *) malloc(sizeof(var));
	strcpy(new_var->name, name);
	new_var->value = 0;
	scope_node * new_scope = (scope_node *) malloc(sizeof(scope_node));
	new_scope->variable = new_var;
	if (scope != NULL) {
		new_scope->upper_scope = scope->upper_scope;
		scope->upper_scope = NULL;
	}
	new_scope->next = scope;
	scope = new_scope;
}

var * _get_var(char * name, scope_node * _scope) {
	if (_scope == NULL) return NULL;
	if (_scope->variable != NULL) {
		if (strcmp(_scope->variable->name, name) == 0) {
			return _scope->variable;
		}
	}
	return _get_var(name, _scope->next);
}

var * get_var(char * name) {
	scope_node * curr_scope = scope;
	var * result;
	while (curr_scope != NULL) {
		result = _get_var(name, curr_scope);
		if (result != NULL) return result;
		curr_scope = curr_scope->upper_scope;
	}
	return NULL;
}

void set_var(char * name, float value) {
	var * _var = get_var(name);
	if (_var == NULL) return;
	_var->value = value;
}

float eval(exp *);

int rec = 0;

float call_func(char * str, exp * args) {
	func * f = get_func(str);
	if (f == NULL) {
		yyerror("unknown identifier");
		return 0;
	}
	open_scope();
	rec++;
	params * pars = f->pars;
	exp * curr_arg = NULL;
	float_node * fn = NULL;
	float_node * curr_fn = NULL;
	if (args != NULL) {
		curr_arg = args->sub;
		fn = create_float_node(eval(curr_arg));
		curr_fn = fn;
		while (curr_arg->next != NULL) {
			curr_arg = curr_arg->next;
			curr_fn->next = create_float_node(eval(curr_arg));
			curr_fn = curr_fn->next;
		}
	}
	while (pars != NULL && fn != NULL) {
		create_var(pars->str);
		set_var(pars->str, fn->value);
		fn = fn->next;
		pars = pars->next;
	}
	float result = eval(f->e);
	close_scope();
	rec--;
	return result;
}

float read_fraction(char * str) {
	char int_str1[ID_LEN];
	char int_str2[ID_LEN];
	int i = 0;
	while (str[i] != 'f') i++;
	strncpy(int_str1, str, i);
	int_str1[i] = 0;
	strncpy(int_str2, &str[i + 1], strlen(str) - i - 1);
	return (float) atoi(int_str1) / atoi(int_str2);
}

int error_h = 0;

float eval(exp * e) {
	float result = 0;
	float temp;
	exp * curr_sub = e->sub;
	switch (e->operation) {
		case SUM:
			result = eval(e->sub) + eval(e->sub->next);
			break;
		case SUB:
			result = eval(e->sub) - eval(e->sub->next);
			break;
		case MUL:
			result = eval(e->sub) * eval(e->sub->next);
			break;
		case DIV:
			result = eval(e->sub) / eval(e->sub->next);
			break;
		case RETURN:
			result = e->value.real;
			break;
		case RETURN_FROM:
			var * vrb = get_var(e->value.str);
			if (vrb == NULL) yyerror("unknown identifier");
			else result = vrb->value;
			break;
		case SET:
			temp = eval(e->sub);
			if (get_var(e->value.str) == NULL) create_var(e->value.str);
			set_var(e->value.str, temp);
			result = get_var(e->value.str)->value;
			break;
		case IF:
			result = eval(e->sub) == false ? eval(e->sub->next->next) : eval(e->sub->next);
			break;
		case EVAL_LIST:
			while (curr_sub != NULL) {
				result = eval(curr_sub);
				curr_sub = curr_sub->next;
			}
			break;
		case WHILE:
			while (eval(e->sub)) result = eval(e->sub->next);
			break;
		case LESS:
			result = eval(e->sub) < eval(e->sub->next);
			break;
		case GREATER:
			result = eval(e->sub) > eval(e->sub->next);
			break;
		case EQUAL:
			result = eval(e->sub) == eval(e->sub->next);
			break;
		case NOT:
			result = eval(e->sub) == false;
			break;
		case CALL_FUNC:
			result = call_func(e->value.str, e->sub);
			break;
		case EXIT:
			exit(0);
			break;
	}
	return result;
}

void eval_print(exp *);

void lone_eval(exp *);

#line 398 "y.tab.c"

# ifndef YY_CAST
#  ifdef __cplusplus
#   define YY_CAST(Type, Val) static_cast<Type> (Val)
#   define YY_REINTERPRET_CAST(Type, Val) reinterpret_cast<Type> (Val)
#  else
#   define YY_CAST(Type, Val) ((Type) (Val))
#   define YY_REINTERPRET_CAST(Type, Val) ((Type) (Val))
#  endif
# endif
# ifndef YY_NULLPTR
#  if defined __cplusplus
#   if 201103L <= __cplusplus
#    define YY_NULLPTR nullptr
#   else
#    define YY_NULLPTR 0
#   endif
#  else
#   define YY_NULLPTR ((void*)0)
#  endif
# endif

/* Use api.header.include to #include this header
   instead of duplicating it here.  */
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

#line 526 "y.tab.c"

};
typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;


int yyparse (void);


#endif /* !YY_YY_Y_TAB_H_INCLUDED  */
/* Symbol kind.  */
enum yysymbol_kind_t
{
  YYSYMBOL_YYEMPTY = -2,
  YYSYMBOL_YYEOF = 0,                      /* "end of file"  */
  YYSYMBOL_YYerror = 1,                    /* error  */
  YYSYMBOL_YYUNDEF = 2,                    /* "invalid token"  */
  YYSYMBOL_KW_AND = 3,                     /* KW_AND  */
  YYSYMBOL_KW_OR = 4,                      /* KW_OR  */
  YYSYMBOL_KW_NOT = 5,                     /* KW_NOT  */
  YYSYMBOL_KW_EQUAL = 6,                   /* KW_EQUAL  */
  YYSYMBOL_KW_LESS = 7,                    /* KW_LESS  */
  YYSYMBOL_KW_GREATER = 8,                 /* KW_GREATER  */
  YYSYMBOL_KW_SET = 9,                     /* KW_SET  */
  YYSYMBOL_KW_DEFFUN = 10,                 /* KW_DEFFUN  */
  YYSYMBOL_KW_WHILE = 11,                  /* KW_WHILE  */
  YYSYMBOL_KW_IF = 12,                     /* KW_IF  */
  YYSYMBOL_KW_EXIT = 13,                   /* KW_EXIT  */
  YYSYMBOL_KW_TRUE = 14,                   /* KW_TRUE  */
  YYSYMBOL_KW_FALSE = 15,                  /* KW_FALSE  */
  YYSYMBOL_KW_RPOGN = 16,                  /* KW_RPOGN  */
  YYSYMBOL_KW_LIST = 17,                   /* KW_LIST  */
  YYSYMBOL_KW_APPEND = 18,                 /* KW_APPEND  */
  YYSYMBOL_KW_CONCAT = 19,                 /* KW_CONCAT  */
  YYSYMBOL_KW_DEFVAR = 20,                 /* KW_DEFVAR  */
  YYSYMBOL_KW_FOR = 21,                    /* KW_FOR  */
  YYSYMBOL_OP_PLUS = 22,                   /* OP_PLUS  */
  YYSYMBOL_OP_MINUS = 23,                  /* OP_MINUS  */
  YYSYMBOL_OP_DIV = 24,                    /* OP_DIV  */
  YYSYMBOL_OP_MULT = 25,                   /* OP_MULT  */
  YYSYMBOL_OP_OP = 26,                     /* OP_OP  */
  YYSYMBOL_OP_CP = 27,                     /* OP_CP  */
  YYSYMBOL_OP_DBLMULT = 28,                /* OP_DBLMULT  */
  YYSYMBOL_OP_COMMA = 29,                  /* OP_COMMA  */
  YYSYMBOL_COMMENT = 30,                   /* COMMENT  */
  YYSYMBOL_SYNTAX_ERROR = 31,              /* SYNTAX_ERROR  */
  YYSYMBOL_VALUEI = 32,                    /* VALUEI  */
  YYSYMBOL_VALUEF = 33,                    /* VALUEF  */
  YYSYMBOL_ID = 34,                        /* ID  */
  YYSYMBOL_YYACCEPT = 35,                  /* $accept  */
  YYSYMBOL_start = 36,                     /* start  */
  YYSYMBOL_input = 37,                     /* input  */
  YYSYMBOL_explist = 38,                   /* explist  */
  YYSYMBOL_exp = 39,                       /* exp  */
  YYSYMBOL_fcall = 40,                     /* fcall  */
  YYSYMBOL_func = 41,                      /* func  */
  YYSYMBOL_params = 42,                    /* params  */
  YYSYMBOL_args = 43,                      /* args  */
  YYSYMBOL_empty = 44                      /* empty  */
};
typedef enum yysymbol_kind_t yysymbol_kind_t;




#ifdef short
# undef short
#endif

/* On compilers that do not define __PTRDIFF_MAX__ etc., make sure
   <limits.h> and (if available) <stdint.h> are included
   so that the code can choose integer types of a good width.  */

#ifndef __PTRDIFF_MAX__
# include <limits.h> /* INFRINGES ON USER NAME SPACE */
# if defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stdint.h> /* INFRINGES ON USER NAME SPACE */
#  define YY_STDINT_H
# endif
#endif

/* Narrow types that promote to a signed type and that can represent a
   signed or unsigned integer of at least N bits.  In tables they can
   save space and decrease cache pressure.  Promoting to a signed type
   helps avoid bugs in integer arithmetic.  */

#ifdef __INT_LEAST8_MAX__
typedef __INT_LEAST8_TYPE__ yytype_int8;
#elif defined YY_STDINT_H
typedef int_least8_t yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef __INT_LEAST16_MAX__
typedef __INT_LEAST16_TYPE__ yytype_int16;
#elif defined YY_STDINT_H
typedef int_least16_t yytype_int16;
#else
typedef short yytype_int16;
#endif

/* Work around bug in HP-UX 11.23, which defines these macros
   incorrectly for preprocessor constants.  This workaround can likely
   be removed in 2023, as HPE has promised support for HP-UX 11.23
   (aka HP-UX 11i v2) only through the end of 2022; see Table 2 of
   <https://h20195.www2.hpe.com/V2/getpdf.aspx/4AA4-7673ENW.pdf>.  */
#ifdef __hpux
# undef UINT_LEAST8_MAX
# undef UINT_LEAST16_MAX
# define UINT_LEAST8_MAX 255
# define UINT_LEAST16_MAX 65535
#endif

#if defined __UINT_LEAST8_MAX__ && __UINT_LEAST8_MAX__ <= __INT_MAX__
typedef __UINT_LEAST8_TYPE__ yytype_uint8;
#elif (!defined __UINT_LEAST8_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST8_MAX <= INT_MAX)
typedef uint_least8_t yytype_uint8;
#elif !defined __UINT_LEAST8_MAX__ && UCHAR_MAX <= INT_MAX
typedef unsigned char yytype_uint8;
#else
typedef short yytype_uint8;
#endif

#if defined __UINT_LEAST16_MAX__ && __UINT_LEAST16_MAX__ <= __INT_MAX__
typedef __UINT_LEAST16_TYPE__ yytype_uint16;
#elif (!defined __UINT_LEAST16_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST16_MAX <= INT_MAX)
typedef uint_least16_t yytype_uint16;
#elif !defined __UINT_LEAST16_MAX__ && USHRT_MAX <= INT_MAX
typedef unsigned short yytype_uint16;
#else
typedef int yytype_uint16;
#endif

#ifndef YYPTRDIFF_T
# if defined __PTRDIFF_TYPE__ && defined __PTRDIFF_MAX__
#  define YYPTRDIFF_T __PTRDIFF_TYPE__
#  define YYPTRDIFF_MAXIMUM __PTRDIFF_MAX__
# elif defined PTRDIFF_MAX
#  ifndef ptrdiff_t
#   include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  endif
#  define YYPTRDIFF_T ptrdiff_t
#  define YYPTRDIFF_MAXIMUM PTRDIFF_MAX
# else
#  define YYPTRDIFF_T long
#  define YYPTRDIFF_MAXIMUM LONG_MAX
# endif
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned
# endif
#endif

#define YYSIZE_MAXIMUM                                  \
  YY_CAST (YYPTRDIFF_T,                                 \
           (YYPTRDIFF_MAXIMUM < YY_CAST (YYSIZE_T, -1)  \
            ? YYPTRDIFF_MAXIMUM                         \
            : YY_CAST (YYSIZE_T, -1)))

#define YYSIZEOF(X) YY_CAST (YYPTRDIFF_T, sizeof (X))


/* Stored state numbers (used for stacks). */
typedef yytype_int8 yy_state_t;

/* State numbers in computations.  */
typedef int yy_state_fast_t;

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif


#ifndef YY_ATTRIBUTE_PURE
# if defined __GNUC__ && 2 < __GNUC__ + (96 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_PURE __attribute__ ((__pure__))
# else
#  define YY_ATTRIBUTE_PURE
# endif
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# if defined __GNUC__ && 2 < __GNUC__ + (7 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_UNUSED __attribute__ ((__unused__))
# else
#  define YY_ATTRIBUTE_UNUSED
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YY_USE(E) ((void) (E))
#else
# define YY_USE(E) /* empty */
#endif

/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
#if defined __GNUC__ && ! defined __ICC && 406 <= __GNUC__ * 100 + __GNUC_MINOR__
# if __GNUC__ * 100 + __GNUC_MINOR__ < 407
#  define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                           \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")
# else
#  define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                           \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")              \
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# endif
# define YY_IGNORE_MAYBE_UNINITIALIZED_END      \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif

#if defined __cplusplus && defined __GNUC__ && ! defined __ICC && 6 <= __GNUC__
# define YY_IGNORE_USELESS_CAST_BEGIN                          \
    _Pragma ("GCC diagnostic push")                            \
    _Pragma ("GCC diagnostic ignored \"-Wuseless-cast\"")
# define YY_IGNORE_USELESS_CAST_END            \
    _Pragma ("GCC diagnostic pop")
#endif
#ifndef YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_END
#endif


#define YY_ASSERT(E) ((void) (0 && (E)))

#if !defined yyoverflow

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
      /* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's 'empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
             && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* !defined yyoverflow */

#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yy_state_t yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (YYSIZEOF (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (YYSIZEOF (yy_state_t) + YYSIZEOF (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)                           \
    do                                                                  \
      {                                                                 \
        YYPTRDIFF_T yynewbytes;                                         \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * YYSIZEOF (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / YYSIZEOF (*yyptr);                        \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, YY_CAST (YYSIZE_T, (Count)) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYPTRDIFF_T yyi;                      \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  26
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   66

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  35
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  10
/* YYNRULES -- Number of rules.  */
#define YYNRULES  30
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  77

/* YYMAXUTOK -- Last valid token kind.  */
#define YYMAXUTOK   289


/* YYTRANSLATE(TOKEN-NUM) -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, with out-of-bounds checking.  */
#define YYTRANSLATE(YYX)                                \
  (0 <= (YYX) && (YYX) <= YYMAXUTOK                     \
   ? YY_CAST (yysymbol_kind_t, yytranslate[YYX])        \
   : YYSYMBOL_YYUNDEF)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex.  */
static const yytype_int8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34
};

#if YYDEBUG
/* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_int16 yyrline[] =
{
       0,   350,   350,   352,   354,   355,   357,   358,   359,   360,
     361,   362,   363,   364,   365,   366,   367,   368,   369,   370,
     371,   372,   373,   374,   376,   378,   380,   381,   383,   384,
     386
};
#endif

/** Accessing symbol of state STATE.  */
#define YY_ACCESSING_SYMBOL(State) YY_CAST (yysymbol_kind_t, yystos[State])

#if YYDEBUG || 0
/* The user-facing name of the symbol whose (internal) number is
   YYSYMBOL.  No bounds checking.  */
static const char *yysymbol_name (yysymbol_kind_t yysymbol) YY_ATTRIBUTE_UNUSED;

/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "\"end of file\"", "error", "\"invalid token\"", "KW_AND", "KW_OR",
  "KW_NOT", "KW_EQUAL", "KW_LESS", "KW_GREATER", "KW_SET", "KW_DEFFUN",
  "KW_WHILE", "KW_IF", "KW_EXIT", "KW_TRUE", "KW_FALSE", "KW_RPOGN",
  "KW_LIST", "KW_APPEND", "KW_CONCAT", "KW_DEFVAR", "KW_FOR", "OP_PLUS",
  "OP_MINUS", "OP_DIV", "OP_MULT", "OP_OP", "OP_CP", "OP_DBLMULT",
  "OP_COMMA", "COMMENT", "SYNTAX_ERROR", "VALUEI", "VALUEF", "ID",
  "$accept", "start", "input", "explist", "exp", "fcall", "func", "params",
  "args", "empty", YY_NULLPTR
};

static const char *
yysymbol_name (yysymbol_kind_t yysymbol)
{
  return yytname[yysymbol];
}
#endif

#define YYPACT_NINF (-26)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-1)

#define yytable_value_is_error(Yyn) \
  0

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
static const yytype_int8 yypact[] =
{
     -18,    26,   -26,   -26,   -26,     4,   -26,   -26,   -18,   -26,
     -26,   -18,   -18,   -18,   -18,   -25,    -9,   -18,   -18,    -1,
     -18,   -18,   -18,   -18,   -18,   -18,   -26,   -26,    13,   -18,
     -18,   -18,   -18,    17,   -18,   -18,   -26,    18,   -18,   -18,
     -18,   -18,   -26,    19,   -26,   -26,    20,    25,    27,    28,
      10,    29,   -18,   -26,    30,    31,    32,    34,   -26,   -26,
     -26,   -26,   -26,    10,    36,   -26,   -26,    37,   -26,   -26,
     -26,   -26,   -26,   -18,   -26,    38,   -26
};

/* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
   Performed when YYTABLE does not specify something else to do.  Zero
   means the default is an error.  */
static const yytype_int8 yydefact[] =
{
       0,     0,    10,     8,     9,     0,     2,     3,     5,    22,
      23,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    30,     1,     4,     0,     0,
       0,     0,     0,     0,     0,     0,    21,     0,     0,     0,
       0,     0,    29,     0,    28,    19,     0,     0,     0,     0,
      30,     0,     0,    20,     0,     0,     0,     0,    24,    18,
      16,    17,    11,    30,     0,    26,     6,     0,    12,    13,
      15,    14,    27,     0,     7,     0,    25
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int8 yypgoto[] =
{
     -26,   -26,   -26,    -3,   -11,   -26,   -26,   -10,   -26,    41
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int8 yydefgoto[] =
{
       0,     5,     6,     7,     8,     9,    10,    64,    43,    65
};

/* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule whose
   number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int8 yytable[] =
{
      28,    29,    30,    31,    26,    27,    34,    35,     1,    32,
      38,    39,    40,    41,     2,     3,     4,    37,    46,    47,
      48,    49,    42,    51,    52,    33,    36,    54,    55,    56,
      57,    11,    12,    13,    14,    15,    16,    17,    18,    19,
      45,    67,    20,    50,    63,    53,    58,    59,    21,    22,
      23,    24,    60,    72,    61,    62,    66,    68,    69,    70,
      25,    71,    75,    73,    74,    76,    44
};

static const yytype_int8 yycheck[] =
{
      11,    12,    13,    14,     0,     8,    17,    18,    26,    34,
      21,    22,    23,    24,    32,    33,    34,    20,    29,    30,
      31,    32,    25,    34,    35,    34,    27,    38,    39,    40,
      41,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      27,    52,    16,    26,    34,    27,    27,    27,    22,    23,
      24,    25,    27,    63,    27,    27,    27,    27,    27,    27,
      34,    27,    73,    27,    27,    27,    25
};

/* YYSTOS[STATE-NUM] -- The symbol kind of the accessing symbol of
   state STATE-NUM.  */
static const yytype_int8 yystos[] =
{
       0,    26,    32,    33,    34,    36,    37,    38,    39,    40,
      41,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      16,    22,    23,    24,    25,    34,     0,    38,    39,    39,
      39,    39,    34,    34,    39,    39,    27,    38,    39,    39,
      39,    39,    38,    43,    44,    27,    39,    39,    39,    39,
      26,    39,    39,    27,    39,    39,    39,    39,    27,    27,
      27,    27,    27,    34,    42,    44,    27,    39,    27,    27,
      27,    27,    42,    27,    27,    39,    27
};

/* YYR1[RULE-NUM] -- Symbol kind of the left-hand side of rule RULE-NUM.  */
static const yytype_int8 yyr1[] =
{
       0,    35,    36,    37,    38,    38,    39,    39,    39,    39,
      39,    39,    39,    39,    39,    39,    39,    39,    39,    39,
      39,    39,    39,    39,    40,    41,    42,    42,    43,    43,
      44
};

/* YYR2[RULE-NUM] -- Number of symbols on the right-hand side of rule RULE-NUM.  */
static const yytype_int8 yyr2[] =
{
       0,     2,     1,     1,     2,     1,     5,     6,     1,     1,
       1,     5,     5,     5,     5,     5,     5,     5,     5,     4,
       4,     3,     1,     1,     4,     8,     1,     2,     1,     1,
       0
};


enum { YYENOMEM = -2 };

#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab
#define YYNOMEM         goto yyexhaustedlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                    \
  do                                                              \
    if (yychar == YYEMPTY)                                        \
      {                                                           \
        yychar = (Token);                                         \
        yylval = (Value);                                         \
        YYPOPSTACK (yylen);                                       \
        yystate = *yyssp;                                         \
        goto yybackup;                                            \
      }                                                           \
    else                                                          \
      {                                                           \
        yyerror (YY_("syntax error: cannot back up")); \
        YYERROR;                                                  \
      }                                                           \
  while (0)

/* Backward compatibility with an undocumented macro.
   Use YYerror or YYUNDEF. */
#define YYERRCODE YYUNDEF


/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)                        \
do {                                            \
  if (yydebug)                                  \
    YYFPRINTF Args;                             \
} while (0)




# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Kind, Value); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*-----------------------------------.
| Print this symbol's value on YYO.  |
`-----------------------------------*/

static void
yy_symbol_value_print (FILE *yyo,
                       yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep)
{
  FILE *yyoutput = yyo;
  YY_USE (yyoutput);
  if (!yyvaluep)
    return;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YY_USE (yykind);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/*---------------------------.
| Print this symbol on YYO.  |
`---------------------------*/

static void
yy_symbol_print (FILE *yyo,
                 yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep)
{
  YYFPRINTF (yyo, "%s %s (",
             yykind < YYNTOKENS ? "token" : "nterm", yysymbol_name (yykind));

  yy_symbol_value_print (yyo, yykind, yyvaluep);
  YYFPRINTF (yyo, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yy_state_t *yybottom, yy_state_t *yytop)
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)                            \
do {                                                            \
  if (yydebug)                                                  \
    yy_stack_print ((Bottom), (Top));                           \
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void
yy_reduce_print (yy_state_t *yyssp, YYSTYPE *yyvsp,
                 int yyrule)
{
  int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %d):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       YY_ACCESSING_SYMBOL (+yyssp[yyi + 1 - yynrhs]),
                       &yyvsp[(yyi + 1) - (yynrhs)]);
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, Rule); \
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args) ((void) 0)
# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif






/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg,
            yysymbol_kind_t yykind, YYSTYPE *yyvaluep)
{
  YY_USE (yyvaluep);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yykind, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YY_USE (yykind);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/* Lookahead token kind.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;
/* Number of syntax errors so far.  */
int yynerrs;




/*----------.
| yyparse.  |
`----------*/

int
yyparse (void)
{
    yy_state_fast_t yystate = 0;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus = 0;

    /* Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* Their size.  */
    YYPTRDIFF_T yystacksize = YYINITDEPTH;

    /* The state stack: array, bottom, top.  */
    yy_state_t yyssa[YYINITDEPTH];
    yy_state_t *yyss = yyssa;
    yy_state_t *yyssp = yyss;

    /* The semantic value stack: array, bottom, top.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs = yyvsa;
    YYSTYPE *yyvsp = yyvs;

  int yyn;
  /* The return value of yyparse.  */
  int yyresult;
  /* Lookahead symbol kind.  */
  yysymbol_kind_t yytoken = YYSYMBOL_YYEMPTY;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;



#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yychar = YYEMPTY; /* Cause a token to be read.  */

  goto yysetstate;


/*------------------------------------------------------------.
| yynewstate -- push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;


/*--------------------------------------------------------------------.
| yysetstate -- set current state (the top of the stack) to yystate.  |
`--------------------------------------------------------------------*/
yysetstate:
  YYDPRINTF ((stderr, "Entering state %d\n", yystate));
  YY_ASSERT (0 <= yystate && yystate < YYNSTATES);
  YY_IGNORE_USELESS_CAST_BEGIN
  *yyssp = YY_CAST (yy_state_t, yystate);
  YY_IGNORE_USELESS_CAST_END
  YY_STACK_PRINT (yyss, yyssp);

  if (yyss + yystacksize - 1 <= yyssp)
#if !defined yyoverflow && !defined YYSTACK_RELOCATE
    YYNOMEM;
#else
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYPTRDIFF_T yysize = yyssp - yyss + 1;

# if defined yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        yy_state_t *yyss1 = yyss;
        YYSTYPE *yyvs1 = yyvs;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * YYSIZEOF (*yyssp),
                    &yyvs1, yysize * YYSIZEOF (*yyvsp),
                    &yystacksize);
        yyss = yyss1;
        yyvs = yyvs1;
      }
# else /* defined YYSTACK_RELOCATE */
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        YYNOMEM;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yy_state_t *yyss1 = yyss;
        union yyalloc *yyptr =
          YY_CAST (union yyalloc *,
                   YYSTACK_ALLOC (YY_CAST (YYSIZE_T, YYSTACK_BYTES (yystacksize))));
        if (! yyptr)
          YYNOMEM;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
#  undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;

      YY_IGNORE_USELESS_CAST_BEGIN
      YYDPRINTF ((stderr, "Stack size increased to %ld\n",
                  YY_CAST (long, yystacksize)));
      YY_IGNORE_USELESS_CAST_END

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }
#endif /* !defined yyoverflow && !defined YYSTACK_RELOCATE */


  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;


/*-----------.
| yybackup.  |
`-----------*/
yybackup:
  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yypact_value_is_default (yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either empty, or end-of-input, or a valid lookahead.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token\n"));
      yychar = yylex ();
    }

  if (yychar <= YYEOF)
    {
      yychar = YYEOF;
      yytoken = YYSYMBOL_YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else if (yychar == YYerror)
    {
      /* The scanner already issued an error message, process directly
         to error recovery.  But do not keep the error token as
         lookahead, it is too special and may lead us to an endless
         loop in error recovery. */
      yychar = YYUNDEF;
      yytoken = YYSYMBOL_YYerror;
      goto yyerrlab1;
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);
  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  /* Discard the shifted token.  */
  yychar = YYEMPTY;
  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     '$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
  case 4: /* explist: exp explist  */
#line 354 "gpp_interpreter.y"
                                                                                                                {(yyvsp[-1].exp_p)->next = (yyvsp[0].exp_p)->sub; (yyvsp[0].exp_p)->sub = (yyvsp[-1].exp_p); (yyval.exp_p) = (yyvsp[0].exp_p);}
#line 1586 "y.tab.c"
    break;

  case 5: /* explist: exp  */
#line 355 "gpp_interpreter.y"
                                                                                                                                {(yyval.exp_p) = create_exp(EVAL_LIST, (yyvsp[0].exp_p));}
#line 1592 "y.tab.c"
    break;

  case 6: /* exp: OP_OP KW_WHILE exp exp OP_CP  */
#line 357 "gpp_interpreter.y"
                                                                                                {(yyval.exp_p) = create_exp(WHILE, add_exp((yyvsp[-2].exp_p), (yyvsp[-1].exp_p))); eval_print((yyval.exp_p));}
#line 1598 "y.tab.c"
    break;

  case 7: /* exp: OP_OP KW_IF exp exp exp OP_CP  */
#line 358 "gpp_interpreter.y"
                                                                                                {(yyval.exp_p) = create_exp(IF, add_exp((yyvsp[-3].exp_p), add_exp((yyvsp[-2].exp_p), (yyvsp[-1].exp_p)))); eval_print((yyval.exp_p));}
#line 1604 "y.tab.c"
    break;

  case 8: /* exp: VALUEF  */
#line 359 "gpp_interpreter.y"
                                                                                                                        {(yyval.exp_p) = create_exp_real(RETURN, NULL, read_fraction((yyvsp[0].str))); eval_print((yyval.exp_p));}
#line 1610 "y.tab.c"
    break;

  case 9: /* exp: ID  */
#line 360 "gpp_interpreter.y"
                                                                                                                                {(yyval.exp_p) = create_exp_str(RETURN_FROM, NULL, (yyvsp[0].str)); eval_print((yyval.exp_p));}
#line 1616 "y.tab.c"
    break;

  case 10: /* exp: VALUEI  */
#line 361 "gpp_interpreter.y"
                                                                                                                        {(yyval.exp_p) = create_exp_real(RETURN, NULL, atoi((yyvsp[0].str))); eval_print((yyval.exp_p));}
#line 1622 "y.tab.c"
    break;

  case 11: /* exp: OP_OP KW_SET ID exp OP_CP  */
#line 362 "gpp_interpreter.y"
                                                                                                        {(yyval.exp_p) = create_exp_str(SET, (yyvsp[-1].exp_p), (yyvsp[-2].str)); lone_eval((yyval.exp_p));}
#line 1628 "y.tab.c"
    break;

  case 12: /* exp: OP_OP OP_PLUS exp exp OP_CP  */
#line 363 "gpp_interpreter.y"
                                                                                                        {(yyval.exp_p) = create_exp(SUM, add_exp((yyvsp[-2].exp_p), (yyvsp[-1].exp_p))); eval_print((yyval.exp_p));}
#line 1634 "y.tab.c"
    break;

  case 13: /* exp: OP_OP OP_MINUS exp exp OP_CP  */
#line 364 "gpp_interpreter.y"
                                                                                                {(yyval.exp_p) = create_exp(SUB, add_exp((yyvsp[-2].exp_p), (yyvsp[-1].exp_p))); eval_print((yyval.exp_p));}
#line 1640 "y.tab.c"
    break;

  case 14: /* exp: OP_OP OP_MULT exp exp OP_CP  */
#line 365 "gpp_interpreter.y"
                                                                                                        {(yyval.exp_p) = create_exp(MUL, add_exp((yyvsp[-2].exp_p), (yyvsp[-1].exp_p))); eval_print((yyval.exp_p));}
#line 1646 "y.tab.c"
    break;

  case 15: /* exp: OP_OP OP_DIV exp exp OP_CP  */
#line 366 "gpp_interpreter.y"
                                                                                                        {(yyval.exp_p) = create_exp(DIV, add_exp((yyvsp[-2].exp_p), (yyvsp[-1].exp_p))); eval_print((yyval.exp_p));}
#line 1652 "y.tab.c"
    break;

  case 16: /* exp: OP_OP KW_LESS exp exp OP_CP  */
#line 367 "gpp_interpreter.y"
                                                                                                        {(yyval.exp_p) = create_exp(LESS, add_exp((yyvsp[-2].exp_p), (yyvsp[-1].exp_p))); eval_print((yyval.exp_p));}
#line 1658 "y.tab.c"
    break;

  case 17: /* exp: OP_OP KW_GREATER exp exp OP_CP  */
#line 368 "gpp_interpreter.y"
                                                                                                {(yyval.exp_p) = create_exp(GREATER, add_exp((yyvsp[-2].exp_p), (yyvsp[-1].exp_p))); eval_print((yyval.exp_p));}
#line 1664 "y.tab.c"
    break;

  case 18: /* exp: OP_OP KW_EQUAL exp exp OP_CP  */
#line 369 "gpp_interpreter.y"
                                                                                                {(yyval.exp_p) = create_exp(EQUAL, add_exp((yyvsp[-2].exp_p), (yyvsp[-1].exp_p))); eval_print((yyval.exp_p));}
#line 1670 "y.tab.c"
    break;

  case 19: /* exp: OP_OP KW_NOT exp OP_CP  */
#line 370 "gpp_interpreter.y"
                                                                                                        {(yyval.exp_p) = create_exp(NOT, (yyvsp[-1].exp_p)); eval_print((yyval.exp_p));}
#line 1676 "y.tab.c"
    break;

  case 20: /* exp: OP_OP KW_RPOGN explist OP_CP  */
#line 371 "gpp_interpreter.y"
                                                                                                {(yyval.exp_p) = (yyvsp[-1].exp_p); eval_print((yyval.exp_p));}
#line 1682 "y.tab.c"
    break;

  case 21: /* exp: OP_OP KW_EXIT OP_CP  */
#line 372 "gpp_interpreter.y"
                                                                                                                {(yyval.exp_p) = create_exp(EXIT, NULL); eval_print((yyval.exp_p));}
#line 1688 "y.tab.c"
    break;

  case 22: /* exp: fcall  */
#line 373 "gpp_interpreter.y"
                                                                                                                        {(yyval.exp_p) = (yyvsp[0].exp_p);}
#line 1694 "y.tab.c"
    break;

  case 23: /* exp: func  */
#line 374 "gpp_interpreter.y"
                                                                                                                        {(yyval.exp_p) = create_exp_real(RETURN, NULL, 0);}
#line 1700 "y.tab.c"
    break;

  case 24: /* fcall: OP_OP ID args OP_CP  */
#line 376 "gpp_interpreter.y"
                                                                                                        {(yyval.exp_p) = create_exp_str(CALL_FUNC, (yyvsp[-1].exp_p), (yyvsp[-2].str)); eval_print((yyval.exp_p));}
#line 1706 "y.tab.c"
    break;

  case 25: /* func: OP_OP KW_DEFFUN ID OP_OP params OP_CP exp OP_CP  */
#line 378 "gpp_interpreter.y"
                                                                        {add_func((yyvsp[-5].str), (yyvsp[-3].par_p), (yyvsp[-1].exp_p));}
#line 1712 "y.tab.c"
    break;

  case 26: /* params: empty  */
#line 380 "gpp_interpreter.y"
                                {(yyval.par_p) = NULL;}
#line 1718 "y.tab.c"
    break;

  case 27: /* params: ID params  */
#line 381 "gpp_interpreter.y"
                                        {(yyval.par_p) = (yyvsp[0].par_p) == NULL ? create_params((yyvsp[-1].str)) : add_params(create_params((yyvsp[-1].str)), (yyvsp[0].par_p));}
#line 1724 "y.tab.c"
    break;

  case 28: /* args: empty  */
#line 383 "gpp_interpreter.y"
                                {(yyval.exp_p) = NULL;}
#line 1730 "y.tab.c"
    break;

  case 29: /* args: explist  */
#line 384 "gpp_interpreter.y"
                                        {(yyval.exp_p) = (yyvsp[0].exp_p);}
#line 1736 "y.tab.c"
    break;


#line 1740 "y.tab.c"

      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
  YY_SYMBOL_PRINT ("-> $$ =", YY_CAST (yysymbol_kind_t, yyr1[yyn]), &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;

  *++yyvsp = yyval;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */
  {
    const int yylhs = yyr1[yyn] - YYNTOKENS;
    const int yyi = yypgoto[yylhs] + *yyssp;
    yystate = (0 <= yyi && yyi <= YYLAST && yycheck[yyi] == *yyssp
               ? yytable[yyi]
               : yydefgoto[yylhs]);
  }

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYSYMBOL_YYEMPTY : YYTRANSLATE (yychar);
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
      yyerror (YY_("syntax error"));
    }

  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
         error, discard it.  */

      if (yychar <= YYEOF)
        {
          /* Return failure if at end of input.  */
          if (yychar == YYEOF)
            YYABORT;
        }
      else
        {
          yydestruct ("Error: discarding",
                      yytoken, &yylval);
          yychar = YYEMPTY;
        }
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:
  /* Pacify compilers when the user code never invokes YYERROR and the
     label yyerrorlab therefore never appears in user code.  */
  if (0)
    YYERROR;
  ++yynerrs;

  /* Do not reclaim the symbols of the rule whose action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;      /* Each real token shifted decrements this.  */

  /* Pop stack until we find a state that shifts the error token.  */
  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYSYMBOL_YYerror;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYSYMBOL_YYerror)
            {
              yyn = yytable[yyn];
              if (0 < yyn)
                break;
            }
        }

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
        YYABORT;


      yydestruct ("Error: popping",
                  YY_ACCESSING_SYMBOL (yystate), yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", YY_ACCESSING_SYMBOL (yyn), yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturnlab;


/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturnlab;


/*-----------------------------------------------------------.
| yyexhaustedlab -- YYNOMEM (memory exhaustion) comes here.  |
`-----------------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  goto yyreturnlab;


/*----------------------------------------------------------.
| yyreturnlab -- parsing is finished, clean up and return.  |
`----------------------------------------------------------*/
yyreturnlab:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  YY_ACCESSING_SYMBOL (+*yyssp), yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif

  return yyresult;
}

#line 388 "gpp_interpreter.y"


#include "lex.yy.c"

void lone_eval(exp * e) {
	if (!parc) eval(e);
}

void eval_print(exp * e) {
	if (parc) return;
	float result = eval(e);
	if (!error_h) printf("%f\n", result);
	error_h = 0;
}

void yyerror(char * s) {
	error_h = 1;
    printf("SYNTAX_ERROR %s\n", s);
}

int main() {
	printf("Disclaimer: There will be no prints of 'SYNTAX OK', the interpreter behaves like a real one.\n");
	printf("Also unfortunately integer values are handled and printed values are floating point, but the interpreter written in lisp only accepts and prints fractions.\n");
    printf("Function bodies and expression lists are handled the same way lisp does, you need 'progn' keyword for expression lists, but not for function body.\n\n");
	printf("Upon a syntax error the interpreter terminates.\n");
	return yyparse();
}
