/* Filename: scanner.c
/* PURPOSE:
 *    SCANNER.C: Functions implementing a Lexical Analyzer (Scanner)
 *    as required for CST8152, Assignment #2
 *    scanner_init() must be called before using the scanner.
 *    The file is incomplete;
 *    Provided by: Svillen Ranev
 *    Version: 1.19.2
 *    Date: 2 October 2019
 *******************************************************************
 *    REPLACE THIS HEADER WITH YOUR HEADER
 *******************************************************************
 */

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
static int iskeyword(char* kw_lexeme); /*keywords lookup functuion */


/*Initializes scanner */
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

Token malar_next_token(void) {
	{
		Token t = { 0 }; /* token to return after pattern recognition. Set all structure members to 0 */
		unsigned char c; /* input symbol */
		int state = 0; /* initial state of the FSM */
		short lexstart;  /*start offset of a lexeme in the input char buffer (array) */
		short lexend;    /*end   offset of a lexeme in the input char buffer (array)*/

		int acceptState = NOAS;

		while (1) { /* endless loop broken by token returns it will generate a warning */

			/*Get the next symbol from the input buffer*/
			c = b_getc(sc_buf);

			/* Part 1: Implementation of token driven scanner. Every token is possessed by its own dedicated code */
			/* Tokens to be handled in this part
			 *  ['=', ' ', '(', ')', '{', '}', ==, <>, '>', '<', ';', white space, !!comment, ',', ';', '-', '+', '*', '/', <<, .AND., .OR., SEOF
			 */
			 /*['=', '(', ')', '{', '}', ==, <>, '>', '<', ';', !!comment, ',', '-', '+', '*', '/', <<, SEOF*/
			switch (c) {
				/*Whitespace cases*/
			case NEWLINE_VAL: case CR_VAL:
				line++;
			case SPACE_VAL: case TAB_VAL:
				continue;

				/*Tokens that begin with '='*/
			case EQUALS_VAL:
				lexstart = b_getcoffset(sc_buf);
			case LPAR_VAL:
			case RPAR_VAL:
			case LBRACE_VAL:
			case RBRACE_VAL:
			case GREATER_THAN_VAL:
			case LESS_THAN_VAL:
			case SEMI_COLON_VAL:
			case NOT_VAL:
			case COMMA_VAL:
			case MINUS_VAL:
			case PLUS_VAL:
			case MULTIPLY_VAL:
			case DIVIDE_VAL:
				/*SEOF cases*/
			case EOF:
			case EOF_VAL2:
			case NULLTERM_VAL:
				/*".AND.", ".OR."*/
			case DOT_VAL:
				/*Utilize finite state machine*/
			default:
				while (1) {
				
				}

			}

			WRITE YOUR CODE FOR PROCESSING THE SPECIAL - CASE TOKENS HERE.
				COMMENTS ARE PROCESSED HERE ALSO.

				WHAT FOLLOWS IS A PSEUDO CODE.YOU CAN USE switch STATEMENT
				INSTEAD OF if - else TO PROCESS THE SPECIAL CASES
				DO NOT FORGET TO COUNT THE PROGRAM LINES

				NOTE :
			IF LEXICAL ERROR OR ILLEGAL CHARACTER ARE FOUND THE SCANNER MUST RETURN AN ERROR TOKEN.
				ILLEGAL CHARACTER IS ONE THAT IS NOT DEFINED IN THE LANGUAGE SPECIFICATION
				OR IT IS OUT OF CONTEXT.
				THE  ILLEGAL CHAR IS THE ATTRIBUTE OF THE ERROR TOKEN
				THE ILLEGAL CHARACTERS ARE PROCESSED BY THE TRANSITION TABLE.
				SOME OF THE LEXICAL ERRORS ARE ALSO PROCESSED BY THE TRANSITION TABLE.

				IN A CASE OF RUNTIME ERROR, THE FUNCTION MUST STORE
				A NON - NEGATIVE NUMBER INTO THE GLOBAL VARIABLE scerrnum
				AND RETURN A RUN TIME ERROR TOKEN.THE RUN TIME ERROR TOKEN ATTRIBUTE
				MUST BE THE STRING "RUN TIME ERROR: "

				IF(c == SOME CHARACTER)
				...
				SKIP CHARACTER(FOR EXAMPLE SPACE)
				continue;
			OR SET TOKEN(SET TOKEN CODE AND TOKEN ATTRIBUTE(IF AVAILABLE))
				return t;
		EXAMPLE:
			if (c == ' ') continue;
			if (c == '{') {
				t.code = RBR_T; /*no attribute */ return t;
				if (c == '+') {
					t.code = ART_OP_T; t.attribute.arr_op = PLUS * / return t;
					...

						IF(c == '.') TRY TO PROCESS.AND. or .OR.
						IF SOMETHING ELSE FOLLOWS.OR THE LAST.IS MISSING
						RETURN AN ERROR TOKEN
						IF(c == '!') TRY TO PROCESS COMMENT
						IF THE FOLLOWING CHAR IS NOT !REPORT AN ERROR
						ELSE IN A LOOP SKIP CHARACTERS UNTIL line terminator is found THEN continue;
					...

						IF(c == ANOTHER CHARACTER)
						SET TOKEN
						return t;


					/* Part 2: Implementation of Finite State Machine (DFA)
							   or Transition Table driven Scanner
							   Note: Part 2 must follow Part 1 to catch the illegal symbols
					*/

					SET THE MARK AT THE BEGINING OF THE LEXEME AND SAVE IT IN lexstart
						lexstart = b_mark(sc_buf, ...);
					....
						PLACE THE CODE OF YOUR FINITE STATE MACHINE HERE(FSM or DFA)
						IT MUST IMPLEMENT THE FOLLOWING ALGORITHM :

					FSM0.Begin with state = 0 and the input character c.
						FSM1.Get the next state from the transition table calling
						state = get_next_state(state, c);
					FSM2.Use the as_table to get the type of the state.
						If the state is not an accepting(NOAS) state,
						get the next character from the input bufferand repeat FSM1.
						FSM3.If the state is an accepting state, token is found, leave the machineand
						call an accepting function as described below.

						IF THE ACCEPTING STATE IS A RETRACTING ACCEPTING STATE
						RETRACT  getc_offset USING AN APPROPRIATE BUFFER FUNCTION.

						SET lexend TO getc_offset USING AN APPROPRIATE BUFFER FUNCTION

						CREATE  A TEMPORRARY LEXEME BUFFER HERE;
					lex_buf = b_allocate(...);
					.RETRACT getc_offset to the MARK SET PREVIOUSLY AT THE BEGINNING OF THE LEXEME AND
						.USING b_getc() COPY THE LEXEME BETWEEN lexstart AND lexend FROM THE INPUT BUFFER INTO lex_buf USING b_addc(...),
						. WHEN VID(KEYWORDS INCLUDED), FPL, IL OR SL IS RECOGNIZED
						.YOU MUST CALL THE ACCEPTING FUNCTION USING THE ARRAY aa_table, WHICH
						.CONTAINS POINTERS TO FUNCTIONS.THE ARRAY INDEX OF THE FUNCTION TO BE
						.CALLED IS STORED IN THE VARIABLE state.
						.YOU ARE NOT ALLOWED TO CALL ANY OF THE ACCEPTING FUNCTIONS BY NAME.
						.THE ARGUMENT TO THE FUNCTION IS THE STRING STORED IN lex_buf char array.
						....
						b_free(lex_buf);
					return t;
				}//end while(1)
			}


			DO NOT MODIFY THE CODE OF THIS FUNCTION
				YOU CAN REMOVE THE COMMENTS ONLY

				int get_next_state(int state, char c) {
				int col;
				int next;
				col = char_class(c);
				next = st_table[state][col];
#ifdef DEBUG
				printf("Input symbol: %c Row: %d Column: %d Next: %d \n", c, state, col, next);
#endif
				/*
				The assert(int test) macro can be used to add run-time diagnostic to programs
				and to "defend" from producing unexpected results.
				assert() is a macro that expands to an if statement;
				if test evaluates to false (zero) , assert aborts the program
				(by calling abort()) and sends the following message on stderr:

				Assertion failed: test, file filename, line linenum

				The filename and linenum listed in the message are the source file name
				and line number where the assert macro appears.
				If you place the #define NDEBUG directive ("no debugging")
				in the source code before the #include <assert.h> directive,
				the effect is to comment out the assert statement.
				*/
				assert(next != IS);

				/*
				The other way to include diagnostics in a program is to use
				conditional preprocessing as shown bellow. It allows the programmer
				to send more details describing the run-time problem.
				Once the program is tested thoroughly #define DEBUG is commented out
				or #undef DEBUF is used - see the top of the file.
				*/
#ifdef DEBUG
				if (next == IS) {
					printf("Scanner Error: Illegal state:\n");
					printf("Input symbol: %c Row: %d Column: %d\n", c, state, col);
					exit(1);
				}
#endif
				return next;
			}

			int char_class(char c) {
				int val;

				/*THIS FUNCTION RETURNS THE COLUMN NUMBER IN THE TRANSITION
				TABLE st_table FOR THE INPUT CHARACTER c.
				SOME COLUMNS MAY REPRESENT A CHARACTER CLASS .
				FOR EXAMPLE IF COLUMN 2 REPRESENTS [A-Za-z]
				THE FUNCTION RETURNS 2 EVERY TIME c IS ONE
				OF THE LETTERS A,B,...,Z,a,b...z.
				PAY ATTENTION THAT THE FIRST COLOMN IN THE TT IS 0 (has index 0)*/

				return val;
			}

			int get_next_state(int, char)
			{
				return 0;
			}



			/*HERE YOU WRITE THE DEFINITIONS FOR YOUR ACCEPTING FUNCTIONS.
			************************************************************

			ACCEPTING FUNCTION FOR THE arithmentic variable identifier AND keywords (VID - AVID/KW)
			REPLACE XX WITH THE CORRESPONDING ACCEPTING STATE NUMBER*/

			Token aa_func02(char lexeme[]) {

				/*WHEN CALLED THE FUNCTION MUST
				1. CHECK IF THE LEXEME IS A KEYWORD.
				   IF YES, IT MUST RETURN A TOKEN WITH THE CORRESPONDING ATTRIBUTE
				   FOR THE KEYWORD. THE ATTRIBUTE CODE FOR THE KEYWORD
				   IS ITS INDEX IN THE KEYWORD LOOKUP TABLE (kw_table in table.h).
				   IF THE LEXEME IS NOT A KEYWORD, GO TO STEP 2.

				2. SET a AVID TOKEN.
				   IF THE lexeme IS LONGER than VID_LEN (see token.h) CHARACTERS,
				   ONLY FIRST VID_LEN CHARACTERS ARE STORED
				   INTO THE VARIABLE ATTRIBUTE ARRAY vid_lex[](see token.h) .
				   ADD \0 AT THE END TO MAKE A C-type STRING.*/
				return t;
			}

			/*ACCEPTING FUNCTION FOR THE string variable identifier (VID - SVID)
			REPLACE XX WITH THE CORRESPONDING ACCEPTING STATE NUMBER*/

			Token aa_func03(char lexeme[]) {

				/*WHEN CALLED THE FUNCTION MUST
				1. SET a SVID TOKEN.
				   IF THE lexeme IS LONGER than VID_LEN characters,
				   ONLY FIRST VID_LEN-1 CHARACTERS ARE STORED
				   INTO THE VARIABLE ATTRIBUTE ARRAY vid_lex[],
				   AND THEN THE @ CHARACTER IS APPENDED TO THE NAME.
				   ADD \0 AT THE END TO MAKE A C-type STRING.*/
				return t;
			}

			/*ACCEPTING FUNCTION FOR THE integer literal(IL) - decimal constant (DIL)*/

			Token aa_func05(char lexeme[]) {

				/*THE FUNCTION MUST CONVERT THE LEXEME REPRESENTING A DECIMAL CONSTANT
				TO A DECIMAL INTEGER VALUE, WHICH IS THE ATTRIBUTE FOR THE TOKEN.
				THE VALUE MUST BE IN THE SAME RANGE AS the value of 2-byte integer in C.
				IN CASE OF ERROR (OUT OF RANGE) THE FUNCTION MUST RETURN ERROR TOKEN
				THE ERROR TOKEN ATTRIBUTE IS  lexeme. IF THE ERROR lexeme IS LONGER
				than ERR_LEN characters, ONLY THE FIRST ERR_LEN-3 characters ARE
				STORED IN err_lex. THEN THREE DOTS ... ARE ADDED TO THE END OF THE
				err_lex C-type string.
				BEFORE RETURNING THE FUNCTION MUST SET THE APROPRIATE TOKEN CODE*/
				return t;
			}

			/*ACCEPTING FUNCTION FOR THE floating - point literal(FPL)*/

			Token aa_func08(char lexeme[]) {

				/*THE FUNCTION MUST CONVERT THE LEXEME TO A FLOATING POINT VALUE,
				WHICH IS THE ATTRIBUTE FOR THE TOKEN.
				THE VALUE MUST BE IN THE SAME RANGE AS the value of 4 - byte float in C.
				IN CASE OF ERROR(OUT OF RANGE) THE FUNCTION MUST RETURN ERROR TOKEN
				THE ERROR TOKEN ATTRIBUTE IS  lexeme.IF THE ERROR lexeme IS LONGER
				than ERR_LEN characters, ONLY THE FIRST ERR_LEN - 3 characters ARE
				STORED IN err_lex.THEN THREE DOTS ... ARE ADDED TO THE END OF THE
				err_lex C - type string.
				BEFORE RETURNING THE FUNCTION MUST SET THE APROPRIATE TOKEN CODE*/
				return t;
			}

			/*ACCEPTING FUNCTION FOR THE string literal(SL)*/

			Token aa_func10(char lexeme[]) {

				/*THE FUNCTION MUST STORE THE lexeme PARAMETER CONTENT INTO THE STRING LITERAL TABLE(str_LTBL)
				FIRST THE ATTRIBUTE FOR THE TOKEN MUST BE SET.
				THE ATTRIBUTE OF THE STRING TOKEN IS THE OFFSET FROM
				THE BEGINNING OF THE str_LTBL char buffer TO THE LOCATION
				WHERE THE FIRST CHAR OF THE lexeme CONTENT WILL BE ADDED TO THE BUFFER.
				USING b_addc(..)COPY THE lexeme content INTO str_LTBL.
				THE OPENING AND CLOSING " MUST BE IGNORED DURING THE COPING PROCESS.
				ADD '\0' AT THE END MAKE THE STRING C-type string
				IF THE STING lexeme CONTAINS line terminators THE line COUNTER MUST BE INCTREMENTED.
				SET THE STRING TOKEN CODE.*/
				return t;
			}

			/*ACCEPTING FUNCTION FOR THE ERROR TOKEN*/

			Token aa_func11_12(char lexeme[]) {

				/*THE FUNCTION SETS THE ERROR TOKEN. lexeme[] CONTAINS THE ERROR
				THE ATTRIBUTE OF THE ERROR TOKEN IS THE lexeme CONTENT ITSELF
				AND IT MUST BE STORED in err_lex. IF THE ERROR lexeme IS LONGER
				than ERR_LEN characters, ONLY THE FIRST ERR_LEN-3 characters ARE
				STORED IN err_lex. THEN THREE DOTS ... ARE ADDED TO THE END OF THE
				err_lex C-type string.
				IF THE ERROR lexeme CONTAINS line terminators THE line COUNTER MUST BE INCTREMENTED.
				BEFORE RETURNING THE FUNCTION MUST SET THE APROPRIATE TOKEN CODE*/
				return t;
			}

			/*HERE YOU WRITE YOUR ADDITIONAL FUNCTIONS (IF ANY). FOR EXAMPLE*/

			int iskeyword(char* kw_lexeme) {
				
			}