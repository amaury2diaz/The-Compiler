/* *******************************************************************
* File Name:		parser.h
* Compiler:			Visual Studio Premium 2012
* Author:			Long Tran, 040815454
					Amuary Diaz Diaz, 040738985
* Course:			CST8152 - Compilers, Lab Section: 012, 011
* Assignment:		Assignment 4 - The Parser
* Date:				December 11, 2015
* Professor:		Svillen Ranev
* Purpose:			To declare the constant values and the function
					prototypes to be used in the parser.c file.

* Function list:	parser(),match(),syn_eh(),syn_printe(),
					gen_incode(),program(),opt_statements,
					statements(),statement(),prime_statement(),
					assignment_statement(),selection_statement(),
					iteration_statement(),input_statement(),
					variable_list(),variable_identifier(),
					prime_variable_list(),output_statement(),
					output_list(),assignment_expression(),
					arithmetic_expression(),
					unary_arithmetic_expression(),
					additive_arithmetic_expression(),
					prime_additive_arithmetic_expression(),
					multiplicative_arithmetic_expression(),
					prime_multiplicative_arithmetic_expression(),
					primary_arithmetic_expression(),
					string_expression(),prime_string_expression(),
					primary_string_expression(),conditional_expression(),
					logical_or_expression(),
					prime_logical_or_expression(),
					logical_and_expression(),
					prime_logical_and_expression(),
					relational_expression(),
					primary_a_relational_expression(),
					relational_operator(),
					primary_s_relational_expression()

* *******************************************************************/

#ifndef  PARSER_H_
#define PARSER_H_

#include "buffer.h"
#include "token.h"
#include "stable.h"
#include <stdlib.h>

#define NO_ATTR -1			/*No attribute required in the token*/

static Token lookahead;
static Buffer* sc_buf;
int synerrno;

extern Token mlwpar_next_token(Buffer * sc_buf);
extern int line;
extern STD sym_table;
extern Buffer* str_LTBL;
extern char * kw_table[];

/*Function prototypes*/
void parser(Buffer * in_buf);
void match(int  pr_token_code,int pr_token_attribute);
void syn_eh(int sync_token_code);
void syn_printe();
void gen_incode(char*);
void program(void);
void opt_statements(void);
void statements(void);
void statement(void);
void prime_statement(void);
void assignment_statement(void);
void selection_statement(void);
void iteration_statement(void);
void input_statement(void);
void variable_list(void);
void variable_identifier(void);
void prime_variable_list(void);
void output_statement(void);
void output_list(void);
void assignment_expression(void);
void arithmetic_expression(void);
void unary_arithmetic_expression(void);
void additive_arithmetic_expression(void);
void prime_additive_arithmetic_expression(void);
void multiplicative_arithmetic_expression(void);
void prime_multiplicative_arithmetic_expression(void);
void primary_arithmetic_expression(void);
void string_expression(void);
void prime_string_expression(void);
void primary_string_expression(void);
void conditional_expression(void);
void logical_or_expression(void);
void prime_logical_or_expression(void);
void logical_and_expression(void);
void prime_logical_and_expression(void);
void relational_expression(void);
void primary_a_relational_expression(void);
void relational_operator(void);
void primary_s_relational_expression(void);

enum {ELSE, IF, INPUT, OUTPUT, PLATYPUS, REPEAT, THEN, USING};

#endif
