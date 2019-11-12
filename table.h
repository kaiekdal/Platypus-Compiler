/************************************************
Filename:                       table.h
Compiler:						MS Visual Studio Enterprise 2019
Version:                        1.0
Authors:		                Kai Ekdal & Olivier Lauzon
Student Number:                 040918802 & 040918796
Course Name/Number:             Compilers CST8152
Lab Section:					312
Assignment # :                  2
Assignment Name:                Building a Lexical Analyzer (Scanner)
Date:							November 12, 2019
Submission Date:                November 12, 2019
Professor:						Svillen Ranev
List of Source and Header Files:buffer.c, scanner.c, buffer.h, table.h, token.h
Purpose:						Transition Table and function declarations necessary
								for the scanner implementation
Function list:					aa_func02();
								aa_func03();
								aa_func05();
								aa_func08();
								aa_func10();
								aa_func11_12();
*************************************************/

#ifndef  TABLE_H_
#define  TABLE_H_ 

#ifndef BUFFER_H_
#include "buffer.h"
#endif

#ifndef NULL
#include <_null.h> /* NULL pointer constant is defined there */
#endif

 /*   Source end-of-file (SEOF) sentinel symbol
  *    '\0' or one of 255,0xFF,EOF
  */

  /*  Special case tokens processed separately one by one
   *  in the token-driven part of the scanner
   *  '=' , ' ' , '(' , ')' , '{' , '}' , == , <> , '>' , '<' , ';',
   *  white space
   *  !!comment , ',' , ';' , '-' , '+' , '*' , '/', << ,
   *  .AND., .OR. , SEOF,
   */

/*Delimiters*/
#define LPAR_VAL '('
#define RPAR_VAL ')'
#define LBRACE_VAL '{'
#define RBRACE_VAL '}'
#define SEMI_COLON_VAL ';'
#define NOT_VAL '!'
#define COMMA_VAL ','
#define QUOTE_VAL '"'
#define DOT_VAL '.'
#define EOF_VAL1 '\0'
#define EOF_VAL2 '0xFF'
#define NULLTERM_VAL '\0'
#define AT_VAL '@'
/*Assignment and comparison operators*/
#define EQUALS_VAL '='
#define LESS_THAN_VAL '<'
#define GREATER_THAN_VAL '>'
/*Arithmetic operators*/
#define MINUS_VAL '-'
#define PLUS_VAL '+'
#define MULTIPLY_VAL '*'
#define DIVIDE_VAL '/'
/*Whitespace*/
#define SPACE_VAL ' '
#define NEWLINE_VAL '\n'
#define CR_VAL '\r'
#define TAB_VAL '\t'

#define ES  11 /* Error state  with no retract */
#define ER  12 /* Error state  with retract */
#define IS (-1)    /* Invalid state */

/* State transition table definition */
#define TABLE_COLUMNS 8

/*transition table - type of states defined in separate table */
int st_table[][TABLE_COLUMNS] = {
	/*[a-zA-Z], '0', [1-9], '.', '@', '"', 'SEOF', other*/
	/* State 0 */	{1, 6, 4, ES, ES, 9, ER, ES},
	/* State 1 */	{1, 1, 1, 2, 3, 2, 2, 2},
	/* State 2 */	{IS, IS, IS, IS, IS, IS, IS, IS},
	/* State 3 */	{IS, IS, IS, IS, IS, IS, IS, IS},
	/* State 4 */	{ES, 4, 4, 7, 5, 5, 5, 5},
	/* State 5 */	{IS, IS, IS, IS, IS, IS, IS, IS},
	/* State 6 */	{ES, 6, ES, 7, 5, 5, 5, 5},
	/* State 7 */	{8, 7, 7, 8, 8, 8, 8, 8},
	/* State 8 */	{IS, IS, IS, IS, IS, IS, IS, IS},
	/* State 9 */	{9, 9, 9, 9, 9, 10, ER, 9},
	/* State 10 */  {IS, IS, IS, IS, IS, IS, IS, IS},
	/* State 11 */  {IS, IS, IS, IS, IS, IS, IS, IS},
	/* State 12 */  {IS, IS, IS, IS, IS, IS, IS, IS},
};
/* Accepting state table definition */
#define ASWR     1  /* accepting state with retract */
#define ASNR     2  /* accepting state with no retract */
#define NOAS     0  /* not accepting state */

int as_table[] = {
	/* State 0 */	NOAS,
	/* State 1 */	NOAS,
	/* State 2 */	ASWR,
	/* State 3 */	ASNR,
	/* State 4 */	NOAS,
	/* State 5 */	ASWR,
	/* State 6 */	NOAS,
	/* State 7 */	NOAS,
	/* State 8 */	ASWR,
	/* State 9 */	NOAS,
	/* State 10 */  ASNR,
	/* State 11 */  ASNR,
	/* State 12 */  ASWR,
};

/* Accepting action function declarations */
Token aa_func02(char lexeme[]);
Token aa_func03(char lexeme[]);
Token aa_func05(char lexeme[]);
Token aa_func08(char lexeme[]);
Token aa_func10(char lexeme[]);
Token aa_func11_12(char lexeme[]); /*Will be used for state 12 as well*/

/* defining a new type: pointer to function (of one char * argument)
   returning Token
*/
typedef Token(*PTR_AAF)(char* lexeme);


/* Accepting function (action) callback table (array) definition */
/* If you do not want to use the typedef, the equvalent declaration is:
 * Token (*aa_table[])(char lexeme[]) = {
 */
PTR_AAF aa_table[] = {
	/* State 0 */	NULL,
	/* State 1 */	NULL,
	/* State 2 */	aa_func02,
	/* State 3 */	aa_func03,
	/* State 4 */	NULL,
	/* State 5 */	aa_func05,
	/* State 6 */	NULL,
	/* State 7 */	NULL,
	/* State 8 */	aa_func08,
	/* State 9 */	NULL,
	/* State 10 */  aa_func10,
	/* State 11 */  aa_func11_12,
	/* State 12 */  aa_func11_12,
};

/* Keyword lookup table (.AND. and .OR. are not keywords) */
#define KWT_SIZE  10

char* kw_table[] =
{
"ELSE",
"FALSE",
"IF",
"PLATYPUS",
"READ",
"REPEAT",
"THEN",
"TRUE",
"WHILE",
"WRITE"
};

#endif                    