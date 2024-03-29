/************************************************
Filename:                       scanner.c
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
Purpose:						Functions to implement a lexical analyzer
Function list:					scanner_init();
								malar_next_token();
								char_class();
								get_next_state();
								aa_func02();
								aa_func03();
								aa_func05();
								aa_func08();
								aa_func10();
								aa_func11_12();
								iskeyword();
*************************************************/

 /* The #define _CRT_SECURE_NO_WARNINGS should be used in MS Visual Studio projects
  * to suppress the warnings about using "unsafe" functions like fopen()
  * and standard sting library functions defined in string.h.
  * The define does not have any effect in Borland compiler projects.
  */

#define _CRT_SECURE_NO_WARNINGS

#include <stdio.h>   /* standard input / output */
#include <ctype.h>   /* conversion functions */
#include <stdlib.h>  /* standard library functions and constants */
#include <string.h>  /* string functions */
#include <limits.h>  /* integer types constants */
#include <float.h>   /* floating-point types constants */

  /*#define NDEBUG        to suppress assert() call */
#include <assert.h>  /* assert() prototype */

/* project header files */
#include "buffer.h"
#include "token.h"
#include "table.h"

#define DEBUG  /* for conditional processing */
#undef  DEBUG

/* Global objects - variables */
/* This buffer is used as a repository for string literals.
   It is defined in platy_st.c */
extern pBuffer str_LTBL; /*String literal table */
int line; /* current line number of the source code */
extern int scerrnum;     /* defined in platy_st.c - run-time error number */

/* Local(file) global objects - variables */
static pBuffer lex_buf;/*pointer to temporary lexeme buffer*/
static pBuffer sc_buf; /*pointer to input source buffer*/
/* No other global variable declarations/definitiond are allowed */

/* scanner.c static(local) function  prototypes */
static int char_class(char c); /* character class function */
static int get_next_state(int, char); /* state machine function */
static int iskeyword(char* kw_lexeme); /*keywords lookup function */

/* Initializes scanner */
int scanner_init(pBuffer psc_buf) {
	if (b_isempty(psc_buf)) return EXIT_FAILURE;/*1*/
	/* in case the buffer has been read previously  */
	b_rewind(psc_buf);
	b_clear(str_LTBL);
	line = 1;
	sc_buf = psc_buf;
	return EXIT_SUCCESS;/*0*/
/*   scerrnum = 0;  *//*no need - global ANSI C */
}

/*****************************************
Function Name:		malar_next_token
Purpose:			performs token recognition 
Authors:			Kai Ekdal & Olivier Lauzon
History/Versions:	1.0
Called functions:	b_getc(), b_retract(), sprintf(), b_getcoffset(), b_mark(), b_reset(), 
					strcat(), get_next_state(), malloc(), sizeof(), b_free()
Parameters:  		void
Return value:		Token, if successful
					Error token, if unsuccessful
Algorithm:			Read the lexeme from the input buffer, character by character
					Return a token structure when a token pattern matching the lexeme is found
					Ignore whitespace, comments, all symbols of the comment, including terminator			
*****************************************/
Token malar_next_token(void) {
	
	Token t = { 0 }; /* token to return after pattern recognition. Set all structure members to 0 */
	unsigned char c; /* input symbol */
	int state = 0; /* initial state of the FSM */
	short lexstart;  /*start offset of a lexeme in the input char buffer (array) */
	short lexend;    /*end   offset of a lexeme in the input char buffer (array)*/

	while (1) { /* endless loop broken by token returns it will generate a warning */

		/* Get the next symbol from the input buffer */
		c = b_getc(sc_buf);

		/* Part 1: Implementation of token driven scanner. Every token is possessed by its own dedicated code */
		/* Tokens to be handled in this part
		* ['=', ' ', '(', ')', '{', '}', ==, <>, '>', '<', ';', white space,
		* !!comment, ',', ';', '-', '+', '*', '/', <<, .AND., .OR., SEOF]
		*/
		switch (c) {
			/* Whitespace cases */
			case NEWLINE_VAL: case CR_VAL:
				line++;
			case SPACE_VAL: case TAB_VAL:
				continue;

			/* Tokens that begin with '=' */
			case EQUALS_VAL:
				c = b_getc(sc_buf);

				/* Case for the "==" lexeme */
				if (c == EQUALS_VAL) {
					t.code = REL_OP_T;
					t.attribute.rel_op = EQ;
					return t;
				}
				/* Case for the "=" lexeme */
				b_retract(sc_buf);
				t.code = ASS_OP_T;
				return t;

			/* Tokens that begin with '<' */
			case LESS_THAN_VAL:
				c = b_getc(sc_buf);

				/* Case for the "<<" lexeme */
				if (c == LESS_THAN_VAL) {
					t.code = SCC_OP_T;
					return t;
				}
				/* Case for the "<>" lexeme */
				if (c == GREATER_THAN_VAL) {
					t.code = REL_OP_T;
					t.attribute.rel_op = NE;
					return t;
				}
				/* Case for the "<" lexeme */
				b_retract(sc_buf);
				t.code = REL_OP_T;
				t.attribute.rel_op = LT;
				return t;

			/* Case for the ">" lexeme */
			case GREATER_THAN_VAL:
				t.code = REL_OP_T;
				t.attribute.rel_op = GT;
				return t;

			/* Tokens that begin with '!' */
			case NOT_VAL:
				c = b_getc(sc_buf);

				/* Dump the rest of the line if a comment is appropriately declared*/
				if (c == NOT_VAL) {
					while (c != NEWLINE_VAL && c != CR_VAL && c != EOF_VAL1 && c != EOF_VAL2) {
						c = b_getc(sc_buf);
					}
					line++;
					continue;
				}

				/* Create error token with '!' plus the cause of error */
				t.code = ERR_T;
				/* sprintf used to ensure that the \0 is inserted into the string */
				sprintf(t.attribute.err_lex, "!%c", c);
				/* In case of error, dump whole line */
				while (c != NEWLINE_VAL && c != CR_VAL && c != EOF_VAL1 && c != EOF && c != EOF_VAL2) {
					c = b_getc(sc_buf);
				}
				b_retract(sc_buf);
				return t;

			/* Tokens that begin with '.' */
			case DOT_VAL:
				lexstart = b_getcoffset(sc_buf);
				c = b_getc(sc_buf);

				/* Case for the ".AND." lexeme */
				if (c == 'A' && b_getc(sc_buf) == 'N' && b_getc(sc_buf) == 'D' && b_getc(sc_buf) == DOT_VAL) {
					t.code = LOG_OP_T;
					t.attribute.log_op = AND;
					return t;
				}
				/* Case for the ".OR." lexeme */
				if (c == 'O' && b_getc(sc_buf) == 'R' && b_getc(sc_buf) == DOT_VAL) {
					t.code = LOG_OP_T;
					t.attribute.log_op = OR;
					return t;
				}

				/* Generate error token if neither leveme is found */
				t.code = ERR_T;
				sprintf(t.attribute.err_lex, ".");
				b_mark(sc_buf, lexstart);
				b_reset(sc_buf);
				return t;

			/* Delimiter cases */
			case LPAR_VAL:
				t.code = LPR_T;
				return t;
			case RPAR_VAL:
				t.code = RPR_T;
				return t;
			case LBRACE_VAL:
				t.code = LBR_T;
				return t;
			case RBRACE_VAL:
				t.code = RBR_T;
				return t;
			case SEMI_COLON_VAL:
				t.code = EOS_T;
				return t;
			case COMMA_VAL:
				t.code = COM_T;
				return t;

			/* Arithmetic operator cases */
			case MINUS_VAL:
				t.code = ART_OP_T;
				t.attribute.arr_op = MINUS;
				return t;
			case PLUS_VAL:
				t.code = ART_OP_T;
				t.attribute.arr_op = PLUS;
				return t;
			case MULTIPLY_VAL:
				t.code = ART_OP_T;
				t.attribute.arr_op = MULT;
				return t;
			case DIVIDE_VAL:
				t.code = ART_OP_T;
				t.attribute.arr_op = DIV;
				return t;

			/* SEOF cases */
			case EOF_VAL1:
				t.code = SEOF_T;
				t.attribute.seof = SEOF_0;
				return t;
			case EOF_VAL2:
				t.code = SEOF_T;
				t.attribute.seof = SEOF_EOF;
				return t;

			/* Utilize finite state machine */
			default:
				lexstart = b_mark(sc_buf, b_getcoffset(sc_buf) - 1);
				int i;
				int lexLength;
				while (1) {
					/* Get the new state and check if it is accepting */
					state = get_next_state(state, c);
					if (as_table[state] == NOAS) {
						c = b_getc(sc_buf);
						continue;
					}

					/* Check if the accepting state has retract */
					if (as_table[state] == ASWR) {
						b_retract(sc_buf);
					}
					 
					/* Create temporary buffer for the lexeme */
					lexend = b_getcoffset(sc_buf);
					lexLength = lexend - lexstart;
					lex_buf = b_allocate((short)lexLength + 1, 15, 'a');
					b_reset(sc_buf);

					/* Check for runtime error */
					if (lex_buf == NULL) {
						b_reset(sc_buf);
						scerrnum = 1;
						t.code = RTE_T;
						sprintf(t.attribute.err_lex, "RUN TIME ERROR: ");
						return t;
					}

					/* Insert lexeme into its temporary buffer */
					for (i = 0; i < lexLength; i++) {
						/* Check for runtime error */
						if (b_addc(lex_buf, b_getc(sc_buf)) == NULL) {
							b_reset(sc_buf);
							scerrnum = 1;
							t.code = RTE_T;
							sprintf(t.attribute.err_lex, "RUN TIME ERROR: ");
							return t;
						}
					}
					/* Check for runtime error */
					if (b_addc(lex_buf, NULLTERM_VAL) == NULL) {
						b_reset(sc_buf);
						scerrnum = 1;
						t.code = RTE_T;
						sprintf(t.attribute.err_lex, "RUN TIME ERROR: ");
						return t;
					}

					/* Pass lexeme to accepting state function */
					t = aa_table[state](b_location(lex_buf));
					b_free(lex_buf);
					return t;
				}/*end while(1)*/
		}/*end switch*/
	}/*end while(1)*/
}/*end of malar_next_token*/

//#define DEBUG
/*****************************************
Function Name:		get_next_state
Purpose:			returns the next state
Author:				Sv. Ranev
History/Versions:	1.0
Called functions:	char_class(), assert()
Parameters:			char c - input character
Return value:		int - next state 
					error message, if illegal state
Algorithm:			check the next state and return it
*****************************************/
int get_next_state(int state, char c) {

	int col;
	int next;
	col = char_class(c);
	next = st_table[state][col];
#ifdef DEBUG
	printf("Input symbol: %c Row: %d Column: %d Next: %d \n", c, state, col, next);
#endif

	assert(next != IS);

#ifdef DEBUG
	if (next == IS) {
		printf("Scanner Error: Illegal state:\n");
		printf("Input symbol: %c Row: %d Column: %d\n", c, state, col);
		exit(1);
	}
#endif
	return next;
}

/*****************************************
Function Name:		char_class
Purpose:			returns the column index in the transition table 
					that represents a character or character class					
Author:				Olivier Lauzon
History/Versions:	1.0
Called functions:	isalpha()
Parameters:			char c - input character
Return value:		int - column number in the transition table for the input character
Algorithm:			Check if the current char value in the lexeme
					Return the related column number for the state to be assigned
*****************************************/
int char_class(char c) {

	/* return 0 for [a-zA-Z] */
	if (isalpha(c)) { 
		return 0; 
	}

	/* return 1 for 0 */
	if (c == '0') { 
		return 1; 
	}

	/* return 2 for [1-9] */
	if (c >= '1' && c <= '9') {
		return 2;
	}

	/* return 3 for the dot (.) value */
	if (c == DOT_VAL) {
		return 3;
	}

	/* return 4 for the @ character */
	if (c == AT_VAL) {
		return 4;
	}

	/* return 5 for the quotation (") symbol */
	if (c == QUOTE_VAL) {
		return 5;
	}

	/* return 6 for the SEOF */
	if (c == EOF_VAL1 || c == EOF_VAL2) {
		return 6;
	}

	/* return other if no other condition is met */
	return 7;
}

/* ACCEPTING FUNCTIONS */

/*****************************************
Function Name:		aa_func02
Purpose:			accepting function for the arithmentic variable identifier 
					and keywords (VID - AVID/KW)
Author:				Kai Ekdal
History/Versions:	1.0
Called functions:	iskeyword(), strlen(), sprintf()
Parameters:			char lexeme[] - character
Return value:		Token
Algorithm:			Check if the lexeme is a keyword
					If yes, return a token with the corresponding attribute 
					If not, set an AVID token
					Check the length of the lexeme and if it is longer than VID_LEN
					Store only the first VID_LEN characters if length of lexeme is longer
					Add a null terminator at the end to make it a string
*****************************************/
Token aa_func02(char lexeme[]) {

	Token t = { 0 };

	/* set a variable to hold the value of whether the input is a keyword */
	int index = iskeyword(lexeme);

	/* return a token with the corresponding attribute if the lexeme is a key word */
	if (index >= 0) {
		t.code = KW_T;
		/* set the keyword attribute to the index in the keyword table */
		t.attribute.kwt_idx = index;
		return t;
	}

	/* if the lexeme is not a keyword, set a AVID token */
	t.code = AVID_T;

	/* store only the first VID_LEN characters if the lexeme is longer than VID_LEN */
	if (strlen(lexeme) > VID_LEN) {
		sprintf(t.attribute.vid_lex, "%.8s", lexeme);
		return t;
	}
	
	sprintf(t.attribute.vid_lex, "%s", lexeme);
	return t;
}

/*****************************************
Function Name:		aa_func03
Purpose:			accepting function for the string variable identifier
					(VID - SVID)
Author:				Kai Ekdal
History/Versions:	1.0
Called functions:	strlen(), sprintf()
Parameters:			char lexeme[] - character
Return value:		Token - string variable identifier
					Error token, if unsuccessful
Algorithm:			Set an SVID token
					Store the first VID_LEN-1 characters in the attribute array
					Append @ and a null terminator at the end to make it a string
*****************************************/
Token aa_func03(char lexeme[]) {

	Token t = { 0 };

	/* set the SVID token */
	t.code = SVID_T;

	/* store the first VID_LEN-1 characters if lexeme is longer than VID_LEN */
	if (strlen(lexeme) > VID_LEN) {
		/* appends @ and /0 to the name */
		sprintf(t.attribute.vid_lex, "%.7s@", lexeme);
		return t;
	}

	sprintf(t.attribute.vid_lex, "%s", lexeme);
	return t;
}

/*****************************************
Function Name:		aa_func05
Purpose:			accepting function for the integer literal (IL) - decimal constant (DIL)
Author:				Olivier Lauzon
History/Versions:	1.0
Called functions:	atol(), strlen(), sprintf()
Parameters:			char lexeme[] - character
Return value:		Token - integer literal
					Error token, if unsuccessful
Algorithm:			Convert the decimal constant lexeme to a decimal integer value
					Check if the value is within the range of a short integer	
					Set the appropriate token code and attribute
*****************************************/
Token aa_func05(char lexeme[]) {

	Token t = { 0 };

	/* convert the decimal constant lexeme to a decimal integer value */
	long value = atol(lexeme);

	/* return error token lexeme if value out of range as the value of a 2-byte integer */
	if (value > SHRT_MAX || value < SHRT_MIN) {
		t.code = ERR_T;
		/* store only the first ERR_LEN-3 characters if error is longer
		than ERR_LEN and append ellipses to the err_lex */
		if (strlen(lexeme) > ERR_LEN) {
			sprintf(t.attribute.err_lex, "%.17s...", lexeme);
			return t;
		}
		sprintf(t.attribute.err_lex, "%s", lexeme);
		return t;
	}

	/* Check if integer has more than 5 digits and truncate excess digits */
	if (strlen(lexeme) > INL_LEN) {
		sprintf(lexeme, "%.5s", lexeme);
		value = atol(lexeme);
	}

	/* set the appropriate token code and attribute */
	t.code = INL_T;
	t.attribute.int_value = value;
	return t;
}

/*****************************************
Function Name:		aa_func08
Purpose:			accepting function for the floating - point literal (FPL)
Author:				Olivier Lauzon
History/Versions:	1.0
Called functions:	atof(), strlen(), sprintf()
Parameters:			char lexeme[] - character
Return value:		Token - floating point literal (FPL)
					Error token, if unsuccessful
Algorithm:			Convert the lexeme to a floating point value
					Check that the value range is the same range as a 4 byte float
					Set appropriate token code
*****************************************/
Token aa_func08(char lexeme[]) {

	Token t = { 0 };

	/* Convert lexeme into float value */
	double value = atof(lexeme);

	/* return error token lexeme if value out of range as the value of a 4-byte float */
	if ((value <= FLT_MAX && value >= FLT_MIN) || value == 0.0) {
		t.code = FPL_T;
		t.attribute.flt_value = (float)value;

		return t;
	}
	t.code = ERR_T;
	/* store only the first ERR_LEN-3 characters if error is longer
	than ERR_LEN and append ellipses to the err_lex */
	if (strlen(lexeme) > ERR_LEN) {
		sprintf(t.attribute.err_lex, "%.17s...", lexeme);
		return t;
	}

	sprintf(t.attribute.err_lex, "%s", lexeme);
	return t;
}

/*****************************************
Function Name:		aa_func10
Purpose:			accepting function for the string literal (SL)
Author:				Olivier Lauzon
History/Versions:	1.0
Called functions:	b_limit(), strlen(), b_addc()
Parameters:			char lexeme[] - character
Return value:		Token - string literal (SL)
					Error token, if unsuccessful
Algorithm:			Set the token attribute
					Copy the lexeme content into the string literal table
					Add a null terminator
					Increment line counter if the string contains line terminators
					Set the string token code
*****************************************/
Token aa_func10(char lexeme[]) {
	
	Token t = { 0 };
	unsigned int i = 0;

	/* Set offset atribute of the SL token */
	t.attribute.str_offset = b_limit(str_LTBL);
	b_mark(sc_buf, b_getcoffset(sc_buf));

	/* Loop to add all the characters of the lexeme into the SL table */
	for (i = 1; i < strlen(lexeme); i++) {
		/* Check for end of string in lexeme */
		if (lexeme[i] == QUOTE_VAL) {
			break;
		}
		/* Check if SL contains a new line */
		if (lexeme[i] == NEWLINE_VAL || lexeme[i] == CR_VAL) {
			line++;
		}
		
		/*Add char and check for null*/
		if (b_addc(str_LTBL, lexeme[i]) == NULL) {
			b_reset(sc_buf);
			scerrnum = 1;
			t.code = RTE_T;
			sprintf(t.attribute.err_lex, "RUN TIME ERROR: ");
			return t;
		}
	}

	/*Add null terminator and check for null*/
	if (b_addc(str_LTBL, NULLTERM_VAL) == NULL) {
		b_reset(sc_buf);
		scerrnum = 1;
		t.code = RTE_T;
		sprintf(t.attribute.err_lex, "RUN TIME ERROR: ");
		return t;
	}
	
	t.code = STR_T;
	return t;
}

/*****************************************
Function Name:		aa_func11_12
Purpose:			accepting function for the error token
Author:				Kai Ekdal
History/Versions:	1.0
Called functions:	strlen(), sprintf(), 
Parameters:			char lexeme[] - character
Return value:		Error token
Algorithm:			Sets the error token
					Add an ellipses if the error lexeme is longer than ERR_LEN
					Set the appropriate token code
					Return token
*****************************************/
Token aa_func11_12(char lexeme[]) {

	Token t = { 0 };
	unsigned int i = 0;

	/* increment line counter if the error lexeme contains line terminators */
	for (i = 0; i < strlen(lexeme); i++) {
		if (lexeme[i] == NEWLINE_VAL || lexeme[i] == CR_VAL) {
			line++;
		}
	}

	/* set the error token */
	t.code = ERR_T;

	/* store only the first ERR_LEN-3 characters if error is longer
	than ERR_LEN and append ellipses to the err_lex */
	if (strlen(lexeme) > ERR_LEN) {
		sprintf(t.attribute.err_lex, "%.17s...", lexeme);
		return t;
	}
	/* sprintf used to ensure that the \0 is inserted */
	sprintf(t.attribute.err_lex, "%s", lexeme);

	return t;
}

/*****************************************
Function Name:		iskeyword
Purpose:			keywords lookup function 
Author:				Kai Ekdal
History/Versions:	1.0
Called functions:	strcmp()
Parameters:			char* kw_lexeme - lexeme
Return value:		int index of the keyword in the array, if successful  
					-1, if unsuccessful
Algorithm:			Check if the input is available
					Iterate through the keyword array and compare with the input
					Return the index of the keyword
*****************************************/
int iskeyword(char* kw_lexeme) {
	
	/* return -1 if the keyword lexeme is null */
	if (kw_lexeme == NULL) {
		return -1;
	}

	/* iterate through the array */
	for (int i = 0; i < KWT_SIZE; i++) {
		/* if the strcmp returns 0, it's a match */
		if (strcmp(kw_lexeme, kw_table[i]) == 0) {
			/* return the index of the keyword */
			return i;
		}
	}

	/* returns -1 if not found or no matches available */
	return -1;
}