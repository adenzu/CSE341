%{
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
%}

%union {
	float 	real;
	char	str[50];
	exp *	exp_p;
	params * par_p;
}

%token KW_AND KW_OR KW_NOT KW_EQUAL KW_LESS KW_GREATER KW_SET KW_DEFFUN KW_WHILE KW_IF KW_EXIT KW_TRUE KW_FALSE KW_RPOGN KW_LIST KW_APPEND KW_CONCAT KW_DEFVAR KW_FOR
%token OP_PLUS OP_MINUS OP_DIV OP_MULT OP_OP OP_CP OP_DBLMULT OP_COMMA
%token COMMENT
%token SYNTAX_ERROR
%token <str> VALUEI
%token <str> VALUEF
%token <str> ID

%type <exp_p> exp
%type <exp_p> explist
%type <exp_p> args
%type <exp_p> fcall
%type <par_p> params

%%
start	:	input
		;
input	:	explist
		;
explist	:	exp explist											{$1->next = $2->sub; $2->sub = $1; $$ = $2;}
		|	exp													{$$ = create_exp(EVAL_LIST, $1);}
		;
exp		:	OP_OP KW_WHILE exp exp OP_CP						{$$ = create_exp(WHILE, add_exp($3, $4)); eval_print($$);}
		|	OP_OP KW_IF exp exp exp OP_CP						{$$ = create_exp(IF, add_exp($3, add_exp($4, $5))); eval_print($$);}
		|	VALUEF												{$$ = create_exp_real(RETURN, NULL, read_fraction($1)); eval_print($$);}
		|	ID													{$$ = create_exp_str(RETURN_FROM, NULL, $1); eval_print($$);}
		|	VALUEI												{$$ = create_exp_real(RETURN, NULL, atoi($1)); eval_print($$);}
		|	OP_OP KW_SET ID exp OP_CP							{$$ = create_exp_str(SET, $4, $3); lone_eval($$);}
		|	OP_OP OP_PLUS exp exp OP_CP							{$$ = create_exp(SUM, add_exp($3, $4)); eval_print($$);}
		|	OP_OP OP_MINUS exp exp OP_CP						{$$ = create_exp(SUB, add_exp($3, $4)); eval_print($$);}
		|	OP_OP OP_MULT exp exp OP_CP							{$$ = create_exp(MUL, add_exp($3, $4)); eval_print($$);}
		|	OP_OP OP_DIV exp exp OP_CP							{$$ = create_exp(DIV, add_exp($3, $4)); eval_print($$);}
		|	OP_OP KW_LESS exp exp OP_CP							{$$ = create_exp(LESS, add_exp($3, $4)); eval_print($$);}
		|	OP_OP KW_GREATER exp exp OP_CP						{$$ = create_exp(GREATER, add_exp($3, $4)); eval_print($$);}
		|	OP_OP KW_EQUAL exp exp OP_CP						{$$ = create_exp(EQUAL, add_exp($3, $4)); eval_print($$);}
		|	OP_OP KW_NOT exp OP_CP								{$$ = create_exp(NOT, $3); eval_print($$);}
		|	OP_OP KW_RPOGN explist OP_CP						{$$ = $3; eval_print($$);}
		|	OP_OP KW_EXIT OP_CP									{$$ = create_exp(EXIT, NULL); eval_print($$);}
		| 	fcall												{$$ = $1;}
		|	func												{$$ = create_exp_real(RETURN, NULL, 0);}
		;
fcall	:	OP_OP ID args OP_CP									{$$ = create_exp_str(CALL_FUNC, $3, $2); eval_print($$);}
		;
func	:	OP_OP KW_DEFFUN ID OP_OP params OP_CP exp OP_CP		{add_func($3, $5, $7);}
		;
params	:	empty		{$$ = NULL;}
		|	ID params 	{$$ = $2 == NULL ? create_params($1) : add_params(create_params($1), $2);}
		;
args	:	empty		{$$ = NULL;}
		|	explist		{$$ = $1;}
		;
empty	:
		;
%%

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
