/************************************************
Filename:                       parser.h
Compiler:						MS Visual Studio Enterprise 2019
Version:                        1.0
Authors:		                Kai Ekdal & Olivier Lauzon
Student Number:                 040918802 & 040918796
Course Name/Number:             Compilers CST8152
Lab Section:					312
Assignment # :                  3
Assignment Name:                Building a Semantic Analyzer (Scanner)
Date:							December 5, 2019
Submission Date:                December 5, 2019
Professor:						Svillen Ranev
List of Source and Header Files:buffer.c, scanner.c, parser.c, buffer.h, table.h, token.h, parser.h
Purpose:						Constants and externs required by the semantic analyzer
Function list:					parser()
								match()
								syn_eh(int)
								syn_printe()
								gen_incode()
								program()
								opt_statements()
								statements()
								statements_prime()
								statement()
								assignment_statement()
								assignment_expression()
								selection_statement()
								iteration_statement()
								pre_condition()
								input_statement()
								variable_list()
								variable_list_prime()
								variable_identifier()
								output_statement()
								output_var()
								arithmetic_expression()
								unary_arithmetic_expression()
								additive_arithmetic_expression()
								additive_arithmetic_expression_prime()
								multiplicative_arithmetic_expression()
								multiplicative_arithmetic_expression_prime()
								primary_arithmetic_expression()
								string_expression()
								string_expression_prime()
								primary_string_expression()
								conditional_expression()
								logical_OR_expression()
								logical_OR_expression_prime()
								logical_AND_expression()
								logical_AND_expression_prime()
								relational_expression()
								primary_a_relational_expression()
								primary_s_relational_expression()
								primary_a_relational_expression_prime()
								primary_s_relational_expression_prime()
*************************************************/

#ifndef PARSER_H
#define PARSER_H

#include "buffer.h" 
#include "token.h"

typedef enum eKW{
	ELSE,
	FALSE,
	IF,
	PLATYPUS,
	READ,
	REPEAT,
	THEN,
	TRUE,
	WHILE,
	WRITE
} eKW;

#define NO_ATTR (-1)

static Token lookahead;
extern Token malar_next_token();
extern Buffer* str_LTBL;
extern char* kw_table[];
extern int line;
int synerrno;

/* Function declration */
void parser();
void match(int pr_token_code, int pr_token_attribute);
void syn_eh(int);
void syn_printe();
void gen_incode(char*);

/*3.1 - PLATYPUS Program*/
void program(void);
void opt_statements(void);
void statements(void);
void statements_prime(void);

/*3.2 - Statements*/
void statement(void);

/*3.2.1 - Assignment Statement*/
void assignment_statement(void);
void assignment_expression(void);

/*3.2.2 - Selection Statement*/
void selection_statement(void);

/*3.2.3 - Iteration Statement*/
void iteration_statement(void);
void pre_condition(void);

/*3.2.4 - Input Statement*/
void input_statement(void);
void variable_list(void);
void variable_list_prime(void);
void variable_identifier(void);

/*3.2.5 - Output Statement*/
void output_statement(void);
void output_var(void);

/*3.3.1 - Arithmetic Expression*/
void arithmetic_expression(void);
void unary_arithmetic_expression(void);
void additive_arithmetic_expression(void);
void additive_arithmetic_expression_prime(void);
void multiplicative_arithmetic_expression(void);
void multiplicative_arithmetic_expression_prime(void);
void primary_arithmetic_expression(void);

/*3.3.2 - String Expression*/
void string_expression(void);
void string_expression_prime(void);
void primary_string_expression(void);

/*3.3.3 - Conditional Expression*/
void conditional_expression(void);
void logical_OR_expression(void);
void logical_OR_expression_prime(void);
void logical_AND_expression(void);
void logical_AND_expression_prime(void);

/*3.3.4 - Relational Expression*/
void relational_expression(void);
void primary_a_relational_expression(void);
void primary_s_relational_expression(void);
void primary_a_relational_expression_prime(void);
void primary_s_relational_expression_prime(void);
#endif // PARSER_H