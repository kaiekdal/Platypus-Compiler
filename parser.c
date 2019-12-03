#include <stdio.h>
#include <stdlib.h>
#include "parser.h"

/*****************************************
Function Name:		parser
Purpose:			The production for a platypus program
Author:				Kai Ekdal & Olivier Lauzon
History/Versions:	1.0
Called functions:	malar_next_token(), program(), match(), gen_incode()
Algorithm:			Grab the first token and begin the parsing process
*****************************************/
void parser(void) {
	lookahead = malar_next_token();
	program(); match(SEOF_T, NO_ATTR);
	gen_incode("PLATY: Source file parsed");
}

/*****************************************
Function Name:		match
Purpose:			Attempts to match the current token with the one required by the parser
Author:				Kai Ekdal & Olivier Lauzon
History/Versions:	1.0
Called functions:	syn_eh(), malar_next_token(), syn_printe()
Parameters:			int pr_token_code - token code required by parser
					int pr_token_attribute - token attribute required by parser
Algorithm:			If match -> advance token, else -> call syn_eh()
*****************************************/
void match(int pr_token_code, int pr_token_attribute) {
	/*The match fails*/
	if (lookahead.code != pr_token_code) {
		syn_eh(pr_token_code);
		return;
	}
	/*Token is SEOF*/
	if (lookahead.code == SEOF_T) {
		return;
	}
	/*Check if the token has an attribute*/
	switch (pr_token_code) {
		case KW_T:
			if (pr_token_attribute != lookahead.attribute.get_int) {
				syn_eh(pr_token_code);
				return;
			}
			break;
		case LOG_OP_T:
			if (pr_token_attribute != lookahead.attribute.log_op) {
				syn_eh(pr_token_code);
				return;
			}
			break;
		case ART_OP_T:
			if (pr_token_attribute != lookahead.attribute.arr_op) {
				syn_eh(pr_token_code);
				return;
			}
			break;
		case REL_OP_T:
			if (pr_token_attribute != lookahead.attribute.rel_op) {
				syn_eh(pr_token_code);
				return;
			}
			break;
	}
	/*Advance to the next token*/
	lookahead = malar_next_token();
	/*If the new token is an error token*/
	if (lookahead.code == ERR_T) {
		syn_printe();
		lookahead = malar_next_token();
		synerrno++;
		return;
	}
}

/*****************************************
Function Name:		syn_eh
Purpose:			Advances the parser to a secure location to gracefully handle errors
Author:				Kai Ekdal & Olivier Lauzon
History/Versions:	1.0
Called functions:	syn_printe(), malar_next_token(), exit()
Parameters:			int sync_token_code - Safe token location
Algorithm:			print error, advance the parser to a safe token
*****************************************/
void syn_eh(int sync_token_code) {
	syn_printe();
	synerrno++;
	while (lookahead.code != sync_token_code) {
		lookahead = malar_next_token();
		if (lookahead.code == sync_token_code) {
			if (lookahead.code != SEOF_T) {
				lookahead = malar_next_token();
			}
			return;
		}
		if (lookahead.code == SEOF_T) {
			exit(synerrno);
			return;
		}
	}
}

/*****************************************
Function Name:		syn_printe
Purpose:			Print the error associated with the token
Author:				Kai Ekdal & Olivier Lauzon
History/Versions:	1.0
Parameters:			char* str - string to print
Algorithm:			Print error message for specific token
*****************************************/
void syn_printe() {
	Token t = lookahead;

	printf("PLATY: Syntax error:  Line:%3d\n", line);
	printf("*****  Token code:%3d Attribute: ", t.code);
	switch (t.code) {
		case  ERR_T: /* ERR_T     0   Error token */
			printf("%s\n", t.attribute.err_lex);
			break;
		case  SEOF_T: /*SEOF_T    1   Source end-of-file token */
			printf("SEOF_T\t\t%d\t\n", t.attribute.seof);
			break;
		case  AVID_T: /* AVID_T    2   Arithmetic Variable identifier token */
		case  SVID_T:/* SVID_T    3  String Variable identifier token */
			printf("%s\n", t.attribute.vid_lex);
			break;
		case  FPL_T: /* FPL_T     4  Floating point literal token */
			printf("%5.1f\n", t.attribute.flt_value);
			break;
		case INL_T: /* INL_T      5   Integer literal token */
			printf("%d\n", t.attribute.get_int);
			break;
		case STR_T:/* STR_T     6   String literal token */
			b_mark(str_LTBL, t.attribute.str_offset);
			printf("%s\n", b_location(str_LTBL));
			break;

		case SCC_OP_T: /* 7   String concatenation operator token */
			printf("NA\n");
			break;

		case  ASS_OP_T:/* ASS_OP_T  8   Assignment operator token */
			printf("NA\n");
			break;
		case  ART_OP_T:/* ART_OP_T  9   Arithmetic operator token */
			printf("%d\n", t.attribute.get_int);
			break;
		case  REL_OP_T: /*REL_OP_T  10   Relational operator token */
			printf("%d\n", t.attribute.get_int);
			break;
		case  LOG_OP_T:/*LOG_OP_T 11  Logical operator token */
			printf("%d\n", t.attribute.get_int);
			break;

		case  LPR_T: /*LPR_T    12  Left parenthesis token */
			printf("NA\n");
			break;
		case  RPR_T: /*RPR_T    13  Right parenthesis token */
			printf("NA\n");
			break;
		case LBR_T: /*    14   Left brace token */
			printf("NA\n");
			break;
		case RBR_T: /*    15  Right brace token */
			printf("NA\n");
			break;

		case KW_T: /*     16   Keyword token */
			printf("%s\n", kw_table[t.attribute.get_int]);
			break;

		case COM_T: /* 17   Comma token */
			printf("NA\n");
			break;
		case EOS_T: /*    18  End of statement *(semi - colon) */
			printf("NA\n");
			break;
		default:
			printf("PLATY: Scanner error: invalid token code: %d\n", t.code);
	}/*end switch*/
}

/*****************************************
Function Name:		gen_incode
Purpose:			Print the completed production passed as a string
Author:				Kai Ekdal & Olivier Lauzon
History/Versions:	1.0
Called functions:	printf()
Parameters:			char* str - string to print
Algorithm:			Print string
*****************************************/
void gen_incode(char* str) {
	printf("%s\n", str);
}

/*****************************************
Function Name:		program
Purpose:			Executes the production
Author:				Kai Ekdal & Olivier Lauzon
History/Versions:	1.0
Called functions:	match(), opt_statements(), gen_incode()
Production:			<program> -> PLATYPUS {<opt_statements>}
Fist Set:			FIRST(<program>) = {KW_T(PLATYPUS)}
*****************************************/
void program(void) {
	match(KW_T, PLATYPUS); match(LBR_T, NO_ATTR); opt_statements();
	match(RBR_T, NO_ATTR);
	gen_incode("PLATY: Program parsed");
}

/*****************************************
Function Name:		opt_statements
Purpose:			Executes the production
Author:				Kai Ekdal & Olivier Lauzon
History/Versions:	1.0
Called functions:	statements(), gen_incode()
Production:			<opt_statements> -> <statements> | e
Fist Set:			FIRST(<opt_statements>) = {AVID_T, SVID_T, KW_T(IF), KW_T(WHILE), KW_T(READ), KW_T(WRITE), e}
*****************************************/
void opt_statements() {
	switch (lookahead.code) {
	case AVID_T:
	case SVID_T: statements(); break;
	case KW_T:
		/* check for IF,WHILE,READ,WRITE and in statements_p()*/
		if (lookahead.attribute.get_int == IF
			|| lookahead.attribute.get_int == WHILE
			|| lookahead.attribute.get_int == READ
			|| lookahead.attribute.get_int == WRITE) {
			statements();
			break;
		}
	default: /*empty string – optional statements*/;
		gen_incode("PLATY: Opt_statements parsed");
	}
}

/*****************************************
Function Name:		statements
Purpose:			Executes the production
Author:				Kai Ekdal & Olivier Lauzon
History/Versions:	1.0
Called functions:	statement(), statements_prime()
Production:			<statements> -> <statement><statements_prime>
Fist Set:			FIRST(<statements>) = {AVID_T, SVID_T, KW_T(IF), KW_T(WHILE), KW_T(READ), KW_T(WRITE)}
*****************************************/
void statements(void) {
	statement();
	statements_prime();
}

/*****************************************
Function Name:		statements_prime
Purpose:			Executes the production
Author:				Kai Ekdal & Olivier Lauzon
History/Versions:	1.0
Called functions:	statement(), statements_prime()
Production:			<statements_prime> -> <statement><statements_prime>
Fist Set:			FIRST(<statements_prime>) = {AVID_T, SVID_T, KW_T(IF), KW_T(WHILE), KW_T(READ), KW_T(WRITE), e}
*****************************************/
void statements_prime(void){
	switch (lookahead.code) {
		case AVID_T:
		case SVID_T:
			statement();
			statements_prime();
			break;
		case KW_T:
			if (lookahead.attribute.get_int == IF
				|| lookahead.attribute.get_int == WHILE
				|| lookahead.attribute.get_int == READ
				|| lookahead.attribute.get_int == WRITE) {
				statement();
				statements_prime();
				break;
			}
		default: break;
	}
}

/*****************************************
Function Name:		statement
Purpose:			Executes the production
Author:				Kai Ekdal & Olivier Lauzon
History/Versions:	1.0
Called functions:	assignment_statement(), selection_statement(), iteration_statement(),
					input_statement(), output_statement(), syn_printe()
Production:			<statement> ->
									<assignment statement>
									|<selection statement>
									|<iteration statement>
									|<input statement>
									|<output statement>
Fist Set:			FIRST(<statement>) = {AVID_T, SVID_T, KW_T(IF), KW_T(WHILE), KW_T(READ), KW_T(WRITE)}
*****************************************/
void statement(void) {
	switch (lookahead.code) {
	case AVID_T: case SVID_T:
		assignment_statement();
		break;
	case KW_T:
		if (lookahead.attribute.get_int == IF) {
			selection_statement();
		}
		else if (lookahead.attribute.get_int == WHILE) {
			iteration_statement();
		}
		else if (lookahead.attribute.get_int == READ) {
			input_statement();
		}
		else if (lookahead.attribute.get_int == WRITE) {
			output_statement();
		}
		break;
	default:
		syn_printe();
	}
}

/*****************************************
Function Name:		assignment_statement
Purpose:			Executes the production
Author:				Kai Ekdal & Olivier Lauzon
History/Versions:	1.0
Called functions:	assignment_expression(), match(), gen_incode()
Production:			<assignment statement> -> <assignment expression>;
Fist Set:			FIRST(<assignment statement>) = {AVID_T, SVID_T}
*****************************************/
void assignment_statement(void){
	assignment_expression();
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Assignment statement parsed");
}

/*****************************************
Function Name:		assignment_expression
Purpose:			Executes the production
Author:				Kai Ekdal & Olivier Lauzon
History/Versions:	1.0
Called functions:	match(), gen_incode(), arithmetic_expression(), string_expression(), syn_printe()
Production:			<assignment expression> -> AVID = <arithmetic expression>|SVID = <string expression>
Fist Set:			FIRST(<assignment expression>) = {AVID_T, SVID_T}
*****************************************/
void assignment_expression(void){
	switch (lookahead.code) {
		case AVID_T:
			match(AVID_T, NO_ATTR);
			match(ASS_OP_T, NO_ATTR);
			arithmetic_expression();
			gen_incode("PLATY: Assignment expression (arithmetic) parsed");
			break;
		case SVID_T:
			match(SVID_T, NO_ATTR);
			match(ASS_OP_T, NO_ATTR);
			string_expression();
			gen_incode("PLATY: Assignment expression (string) parsed");
			break;
		default:
			syn_printe();
			break;
	}
}

/*****************************************
Function Name:		selection_statement
Purpose:			Executes the production
Author:				Kai Ekdal & Olivier Lauzon
History/Versions:	1.0
Called functions:	match(), gen_incode(), pre_condition(), conditional_expression(), opt_statements()
Production:			<selection statement> -> IF <pre-condition>  (<conditional expression>) THEN { <opt_statements> }
											 ELSE { <opt_statements> };
Fist Set:			FIRST(<selection statement>) = {KW_T(IF)}
*****************************************/
void selection_statement(void){
	match(KW_T, IF); pre_condition();
	match(LPR_T, NO_ATTR); conditional_expression();
	match(RPR_T, NO_ATTR); match(KW_T, THEN); match(LBR_T, NO_ATTR); opt_statements();
	match(RBR_T, NO_ATTR); match(KW_T, ELSE); match(LBR_T, NO_ATTR); opt_statements();
	match(RBR_T, NO_ATTR); match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Selection statement parsed");
}

/*****************************************
Function Name:		iteration_statement
Purpose:			Executes the production
Author:				Kai Ekdal & Olivier Lauzon
History/Versions:	1.0
Called functions:	match(), gen_incode(), pre_condition(), conditional_expression(), statements()
Production:			<iteration statement> -> WHILE <pre-condition> (<conditional expression>) REPEAT {<statements>};
Fist Set:			FIRST(<iteration statement>) = {KW_T(WHILE)}
*****************************************/
void iteration_statement(void){
	match(KW_T, WHILE); pre_condition();
	match(LPR_T, NO_ATTR); conditional_expression();
	match(RPR_T, NO_ATTR); match(KW_T, REPEAT); match(LBR_T, NO_ATTR); statements();
	match(RBR_T, NO_ATTR); match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Iteration statement parsed");
}

/*****************************************
Function Name:		pre_condition
Purpose:			Executes the production
Author:				Kai Ekdal & Olivier Lauzon
History/Versions:	1.0
Called functions:	match(), syn_printe()
Production:			<pre-condition> -> TRUE | FALSE
Fist Set:			FIRST(<pre-condition>) = {KW_T(TRUE), KW_T(FALSE)}
*****************************************/
void pre_condition(void){
	if (lookahead.code == KW_T) {
		switch (lookahead.attribute.get_int) {
		case TRUE:
			match(KW_T, TRUE);
			break;
		case FALSE:
			match(KW_T, FALSE);
			break;
		default:
			syn_printe();
			break;
		}
	}
}

/*****************************************
Function Name:		input_statement
Purpose:			Executes the production
Author:				Kai Ekdal & Olivier Lauzon
History/Versions:	1.0
Called functions:	match(), gen_incode(), variable_list()
Production:			<input statement> -> READ (<variable list>);
Fist Set:			FIRST(<input statement>) = {KW_T(READ)}
*****************************************/
void input_statement(void){
	match(KW_T, READ); match(LPR_T, NO_ATTR); variable_list();
	match(RPR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Input statement parsed");
}

/*****************************************
Function Name:		variable_list
Purpose:			Executes the production
Author:				Kai Ekdal & Olivier Lauzon
History/Versions:	1.0
Called functions:	variable_identifier(), variable_list_prime(), gen_incode()
Production:			<variable list> -> <variable identifier><variable list_prime>
Fist Set:			FIRST(<variable list>) = {AVID_T, SVID_T}
*****************************************/
void variable_list(void){
	variable_identifier();
	variable_list_prime();
	gen_incode("PLATY: Variable list parsed");
}

/*****************************************
Function Name:		variable_list_prime
Purpose:			Executes the production
Author:				Kai Ekdal & Olivier Lauzon
History/Versions:	1.0
Called functions:	match(), variable_identifier(), variable_list_prime()
Production:			<variable list_prime> -> ,<variable identifier><variable list_prime> | e
Fist Set:			FIRST(<variable list_prime>) = {COM_T, e}
*****************************************/
void variable_list_prime(void){
	if (lookahead.code == COM_T) {
		match(COM_T, NO_ATTR); variable_identifier();
		variable_list_prime();
	}
}

/*****************************************
Function Name:		variable_identifier
Purpose:			Executes the production
Author:				Kai Ekdal & Olivier Lauzon
History/Versions:	1.0
Called functions:	match(), syn_printe()
Production:			<variable identifier> -> AVID_T | SVID_T
Fist Set:			FIRST(<variable identifier>) = {AVID_T, SVID_T}
*****************************************/
void variable_identifier(void){
	switch (lookahead.code) {
		case AVID_T:
			match(AVID_T, NO_ATTR);
			break;
		case SVID_T:
			match(SVID_T, NO_ATTR);
			break;
		default:
			syn_printe();
			break;
	}
}

/*****************************************
Function Name:		output_statement
Purpose:			Executes the production
Author:				Kai Ekdal & Olivier Lauzon
History/Versions:	1.0
Called functions:	match(), gen_incode(), output_var()
Production:			<output statement> -> WRITE (<output_var>);
Fist Set:			FIRST(<output statement>) = {KW_T(WRITE)}
*****************************************/
void output_statement(void){
	match(KW_T, WRITE); match(LPR_T, NO_ATTR); output_var();
	match(RPR_T, NO_ATTR); match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Output statement parsed");
}

/*****************************************
Function Name:		output_var
Purpose:			Executes the production
Author:				Kai Ekdal & Olivier Lauzon
History/Versions:	1.0
Called functions:	match(), gen_incode(), variable_list()
Production:			<output_var> -> <variable list> | STR_T | e
Fist Set:			FIRST(<output_var>) = {AVID_T, SVID_T, STR_T, e}
*****************************************/
void output_var(void){
	switch (lookahead.code) {
		case AVID_T: case SVID_T:
			variable_list();
			break;
		case STR_T:
			match(STR_T, NO_ATTR);
			gen_incode("PLATY: Output list (string literal) parsed");
			break;
		default:
			gen_incode("PLATY: Output list (empty) parsed");
			break;
	}
}

/*****************************************
Function Name:		arithmetic_expression
Purpose:			Executes the production
Author:				Kai Ekdal & Olivier Lauzon
History/Versions:	1.0
Called functions:	gen_incode(), unary_arithmetic_expression(), additive_arithmetic_expression(), syn_printe()
Production:			<arithmetic expression> - > <unary arithmetic expression>|<additive arithmetic expression>
Fist Set:			FIRST(<arithmetic expression>) = {ARR_OP(PLUS), ARR_OP(MINUS), AVID_T, FPL_T, INL_T, LPR_T}
*****************************************/
void arithmetic_expression(void){
	switch (lookahead.code) {
		case ART_OP_T:
			if (lookahead.attribute.arr_op == PLUS || lookahead.attribute.arr_op == MINUS) {
				unary_arithmetic_expression();
				gen_incode("PLATY: Arithmetic expression parsed");
				break;
			}
			syn_printe();
			break;
		case AVID_T:case FPL_T:case INL_T:case LPR_T:
			additive_arithmetic_expression();
			gen_incode("PLATY: Arithmetic expression parsed");
			break;
		default:
			syn_printe();
			break;
	}
}

/*****************************************
Function Name:		unary_arithmetic_expression
Purpose:			Executes the production
Author:				Kai Ekdal & Olivier Lauzon
History/Versions:	1.0
Called functions:	match(), gen_incode(), primary_arithmetic_expression(), syn_printe()
Production:			<unary arithmetic expression> -> - <primary arithmetic expression> | + <primary arithmetic expression>
Fist Set:			FIRST(<unary arithmetic expression>) = {ARR_OP(PLUS), ARR_OP(MINUS)}
*****************************************/
void unary_arithmetic_expression(void){
	switch (lookahead.attribute.arr_op) {
		case PLUS:
			match(ART_OP_T, PLUS); primary_arithmetic_expression();
			break;
		case MINUS:
			match(ART_OP_T, MINUS); primary_arithmetic_expression();
			break;
		default:
			syn_printe();
			break;
	}
	gen_incode("PLATY: Unary arithmetic expression parsed");
}

/*****************************************
Function Name:		additive_arithmetic_expression
Purpose:			Executes the production
Author:				Kai Ekdal & Olivier Lauzon
History/Versions:	1.0
Called functions:	multiplicative_arithmetic_expression(), additive_arithmetic_expression_prime()
Production:			<additive arithmetic expression> -> <multiplicative arithmetic expression><additive arithmetic expression_prime>
Fist Set:			FIRST(<additive arithmetic expression>) = {AVID_T, FPL_T, INL_T, LPR_T}
*****************************************/
void additive_arithmetic_expression(void){
	multiplicative_arithmetic_expression();
	additive_arithmetic_expression_prime();
}

/*****************************************
Function Name:		additive_arithmetic_expression_prime
Purpose:			Executes the production
Author:				Kai Ekdal & Olivier Lauzon
History/Versions:	1.0
Called functions:	match(), gen_incode(),  multiplicative_arithmetic_expression(), 
					additive_arithmetic_expression_prime(), syn_printe()
Production:			<additive arithmetic expression_prime> ->
						+ <multiplicative arithmetic expression><additive arithmetic expression_prime> |
						- <multiplicative arithmetic expression><additive arithmetic expression_prime> | e
Fist Set:			FIRST(<additive arithmetic expression_prime>) = {ARR_OP(PLUS), ARR_OP(MINUS), e}
*****************************************/
void additive_arithmetic_expression_prime(void){
	if (lookahead.code == ART_OP_T) {
		switch (lookahead.attribute.arr_op) {
		case PLUS:
			match(ART_OP_T, PLUS); multiplicative_arithmetic_expression();
			additive_arithmetic_expression_prime();
			gen_incode("PLATY: Additive arithmetic expression parsed");
			break;
		case MINUS:
			match(ART_OP_T, MINUS); multiplicative_arithmetic_expression();
			additive_arithmetic_expression_prime();
			gen_incode("PLATY: Additive arithmetic expression parsed");
			break;
		default:
			syn_printe();
			break;
		}
	}
}

/*****************************************
Function Name:		multiplicative_arithmetic_expression
Purpose:			Executes the production
Author:				Kai Ekdal & Olivier Lauzon
History/Versions:	1.0
Called functions:	primary_arithmetic_expression(), multiplicative_arithmetic_expression_prime()
Production:			<multiplicative arithmetic expression> -> <primary arithmetic expression><multiplicative arithmetic expression_prime>
Fist Set:			FIRST(<multiplicative arithmetic expression>) = {AVID_T, FPL_T, INL_T, LPR_T}
*****************************************/
void multiplicative_arithmetic_expression(void){
	primary_arithmetic_expression();
	multiplicative_arithmetic_expression_prime();
}

/*****************************************
Function Name:		multiplicative_arithmetic_expression_prime
Purpose:			Executes the production
Author:				Kai Ekdal & Olivier Lauzon
History/Versions:	1.0
Called functions:	match(), gen_incode(), primary_arithmetic_expression(),
					multiplicative_arithmetic_expression_prime(), syn_printe()
Production:			<multiplicative arithmetic expression_prime> ->
						* <primary arithmetic expression><multiplicative arithmetic expression_prime> |
						/ <primary arithmetic expression><multiplicative arithmetic expression_prime> | e
Fist Set:			FIRST(<multiplicative arithmetic expression_prime>) = {ARR_OP(MULT), ARR_OP(DIV), e}
*****************************************/
void multiplicative_arithmetic_expression_prime(void){
	if (lookahead.code == ART_OP_T) {
		switch (lookahead.attribute.arr_op) {
			case MULT:
				match(ART_OP_T, MULT); primary_arithmetic_expression();
				multiplicative_arithmetic_expression_prime();
				gen_incode("PLATY: Multiplicative arithmetic expression parsed");
				break;
			case DIV:
				match(ART_OP_T, DIV); primary_arithmetic_expression();
				multiplicative_arithmetic_expression_prime();
				gen_incode("PLATY: Multiplicative arithmetic expression parsed");
				break;
		}
	}
}

/*****************************************
Function Name:		primary_arithmetic_expression
Purpose:			Executes the production
Author:				Kai Ekdal & Olivier Lauzon
History/Versions:	1.0
Called functions:	match(), gen_incode(), arithmetic_expression()
Production:			<primary arithmetic expression> -> AVID_T | FPL_T | INL_T | (<arithmetic expression>)	
Fist Set:			FIRST(<primary arithmetic expression>) = {AVID_T, FPL_T, INL_T, LPR_T}
*****************************************/
void primary_arithmetic_expression(void){
	switch (lookahead.code) {
	case AVID_T:
		match(AVID_T, NO_ATTR);
		break;
	case INL_T:
		match(INL_T, NO_ATTR);
		break;
	case FPL_T:
		match(FPL_T, NO_ATTR);
		break;
	case LPR_T:
		match(LPR_T, NO_ATTR); arithmetic_expression();
		match(RPR_T, NO_ATTR);
		break;
	}
	gen_incode("PLATY: Primary arithmetic expression parsed");
}

/*****************************************
Function Name:		string_expression
Purpose:			Executes the production
Author:				Kai Ekdal & Olivier Lauzon
History/Versions:	1.0
Called functions:	gen_incode(), primary_string_expression(), string_expression_prime()
Production:			<string expression> -> <primary string expression><string expression_prime>
Fist Set:			FIRST(<string expression>) = {SVID_T, STR_T}
*****************************************/
void string_expression(void){
	primary_string_expression();
	string_expression_prime();
	gen_incode("PLATY: String expression parsed");
}

/*****************************************
Function Name:		string_expression_prime
Purpose:			Executes the production
Author:				Kai Ekdal & Olivier Lauzon
History/Versions:	1.0
Called functions:	match(), primary_string_expression(), string_expression_prime()
Production:			<string expression_prime> -> << <primary string expression><string expression_prime> | e
Fist Set:			FIRST(<string expression_prime>) = {SCC_OP_T, e}
*****************************************/
void string_expression_prime(void){
	if (lookahead.code == SCC_OP_T) {
		match(SCC_OP_T, NO_ATTR); primary_string_expression();
		string_expression_prime();
	}
}

/*****************************************
Function Name:		primary_string_expression
Purpose:			Executes the production
Author:				Kai Ekdal & Olivier Lauzon
History/Versions:	1.0
Called functions:	match(), gen_incode()
Production:			<primary string expression> -> SVID_T | STR_T
Fist Set:			FIRST(<primary string expression>) = {SVID_T, STR_T}
*****************************************/
void primary_string_expression(void){
	switch (lookahead.code) {
		case SVID_T:
			match(SVID_T, NO_ATTR);
			break;
		case STR_T:
			match(STR_T, NO_ATTR);
			break;
	}
	gen_incode("PLATY: Primary string expression parsed");
}

/*****************************************
Function Name:		conditional_expression
Purpose:			Executes the production
Author:				Kai Ekdal & Olivier Lauzon
History/Versions:	1.0
Called functions:	gen_incode(), logical_OR_expression()
Production:			<conditional expression> -> <logical OR expression>
Fist Set:			FIRST(<conditional expression>) = {AVID_T, FPL_T, INL_T, SVID_T, STR_T}
*****************************************/
void conditional_expression(void){
	logical_OR_expression();
	gen_incode("PLATY: Conditional expression parsed");
}

/*****************************************
Function Name:		logical_OR_expression
Purpose:			Executes the production
Author:				Kai Ekdal & Olivier Lauzon
History/Versions:	1.0
Called functions:	logical_AND_expression(), logical_OR_expression_prime()
Production:			<logical OR expression> -> <logical AND expression><logical OR expression_prime>
Fist Set:			FIRST(<logical OR expression>) = {AVID_T, FPL_T, INL_T, SVID_T, STR_T}
*****************************************/
void logical_OR_expression(void){
	logical_AND_expression();
	logical_OR_expression_prime();
}

/*****************************************
Function Name:		logical_OR_expression_prime
Purpose:			Executes the production
Author:				Kai Ekdal & Olivier Lauzon
History/Versions:	1.0
Called functions:	match(), gen_incode(), logical_AND_expression(), logical_OR_expression_prime()
Production:			<logical OR expression_prime> -> .OR. <logical AND expression><logical OR expression_prime> | e
Fist Set:			FIRST(<logical OR expression_prime>) = {LOG_OP_T(OR), e}
*****************************************/
void logical_OR_expression_prime(void){
	if (lookahead.code == LOG_OP_T) {
		switch (lookahead.attribute.log_op) {
			case OR:
				match(LOG_OP_T, OR); logical_AND_expression();
				logical_OR_expression_prime();
				gen_incode("PLATY: Logical OR expression parsed");
				break;
		}
	}
}

/*****************************************
Function Name:		logical_AND_expression
Purpose:			Executes the production
Author:				Kai Ekdal & Olivier Lauzon
History/Versions:	1.0
Called functions:	relational_expression(), logical_AND_expression_prime()
Production:			<logical AND expression> -> <relational expression><logical AND expression_prime>
Fist Set:			FIRST(<logical AND expression>) = {AVID_T, FPL_T, INL_T, SVID_T, STR_T}
*****************************************/
void logical_AND_expression(void){
	relational_expression();
	logical_AND_expression_prime();
}

/*****************************************
Function Name:		logical_AND_expression_prime
Purpose:			Executes the production
Author:				Kai Ekdal & Olivier Lauzon
History/Versions:	1.0
Called functions:	match(), gen_incode(), relational_expression(), logical_AND_expression_prime()
Production:			<logical AND expression_prime> -> .AND. <relational expression><logical AND expression_prime> | e
Fist Set:			FIRST(<logical AND expression_prime>) = {LOG_OP_T(AND), e}
*****************************************/
void logical_AND_expression_prime(void){
	if (lookahead.code == LOG_OP_T) {
		switch (lookahead.attribute.log_op) {
		case AND:
			match(LOG_OP_T, AND); relational_expression();
			logical_AND_expression_prime();
			gen_incode("PLATY: Logical AND expression parsed");
			break;
		}
	}
}

/*****************************************
Function Name:		relational_expression
Purpose:			Executes the production
Author:				Kai Ekdal & Olivier Lauzon
History/Versions:	1.0
Called functions:	syn_printe(), gen_incode(), primary_a_relational_expression(),primary_a_relational_expression_prime(),
						primary_s_relational_expression(), primary_s_relational_expression_prime()
Production:			<relational expression> ->
						<primary a_relational expression><primary a_relational expression_prime> |
						<primary s_relational expression><primary s_relational expression_prime>
Fist Set:			FIRST(<relational expression>) = {AVID_T, FPL_T, INL_T, SVID_T, STR_T}
*****************************************/
void relational_expression(void){
	switch (lookahead.code) {
		case FPL_T:case INL_T:case AVID_T:
			primary_a_relational_expression();
			primary_a_relational_expression_prime();
			break;
		case STR_T:case SVID_T:
			primary_s_relational_expression();
			primary_s_relational_expression_prime();
			break;
		default:
			syn_printe();
			break;
	}
	gen_incode("PLATY: Relational expression parsed");
}

/*****************************************
Function Name:		primary_a_relational_expression
Purpose:			Executes the production
Author:				Kai Ekdal & Olivier Lauzon
History/Versions:	1.0
Called functions:	match(), gen_incode(), syn_printe()
Production:			<primary a_relational expression> -> AVID_T | FPL_T | INL_T
Fist Set:			FIRST(<primary a_relational expression>) = {AVID_T, FPL_T, INL_T}
*****************************************/
void primary_a_relational_expression(void){
	switch (lookahead.code) {
		case AVID_T:
			match(AVID_T, NO_ATTR);
			gen_incode("PLATY: Primary a_relational expression parsed");
			break;
		case FPL_T:
			match(FPL_T, NO_ATTR);
			gen_incode("PLATY: Primary a_relational expression parsed");
			break;
		case INL_T:
			match(INL_T, NO_ATTR);
			gen_incode("PLATY: Primary a_relational expression parsed");
			break;
		case SVID_T: case STR_T:
			syn_printe();
			gen_incode("PLATY: Primary a_relational expression parsed");
			break;
		default:
			syn_printe();
			break;
	}
}

/*****************************************
Function Name:		primary_s_relational_expression
Purpose:			Executes the production
Author:				Kai Ekdal & Olivier Lauzon
History/Versions:	1.0
Called functions:	gen_incode(), syn_printe(), primary_string_expression()
Production:			<primary s_relational expression> -> <primary string expression>
Fist Set:			FIRST(<primary s_relational expression>) = {SVID_T, STR_T}
*****************************************/
void primary_s_relational_expression(void){
	switch (lookahead.code) {
		case STR_T:case SVID_T:
			primary_string_expression();
			gen_incode("PLATY: Primary s_relational expression parsed");
			break;
		case AVID_T:case FPL_T:case INL_T:
			syn_printe();
			gen_incode("PLATY: Primary s_relational expression parsed");
			break;
		default:
			syn_printe();
			break;
	}
}

/*****************************************
Function Name:		primary_a_relational_expression_prime
Purpose:			Executes the production
Author:				Kai Ekdal & Olivier Lauzon
History/Versions:	1.0
Called functions:	match(), syn_printe(), primary_a_relational_expression()
Production:			<primary a_relational expression_prime> -> 
						== <primary a_relational expression> 
						| <> <primary a_relational  expression> 
						| > <primary a_relational  expression> 
						| < <primary a_relational expression>
Fist Set:			FIRST(<primary a_relational expression_prime>) = {REL_OP_T(EQ), REL_OP_T(NE), REL_OP_T(GT), REL_OP_T(LT)}
*****************************************/
void primary_a_relational_expression_prime(void){
	if (lookahead.code == REL_OP_T) {
		switch (lookahead.attribute.rel_op) {
			case EQ:
				match(REL_OP_T, EQ);
				break;
			case NE:
				match(REL_OP_T, NE);
				break;
			case GT:
				match(REL_OP_T, GT);
				break;
			case LT:
				match(REL_OP_T, LT);
				break;
			default:
				syn_printe();
				break;
		}
	}
	primary_a_relational_expression();
}

/*****************************************
Function Name:		primary_s_relational_expression_prime
Purpose:			Executes the production
Author:				Kai Ekdal & Olivier Lauzon
History/Versions:	1.0
Called functions:	match(), syn_printe(), primary_s_relational_expression()
Production:			<primary s_relational expression_prime> ->
						== <primary s_relational expression> 
						| <> <primary s_relational  expression> 
						| > <primary s_relational  expression> 
						| < <primary s_relational expression>
Fist Set:			FIRST(<primary s_relational expression_prime>) = {REL_OP_T(EQ), REL_OP_T(NE), REL_OP_T(GT), REL_OP_T(LT)}
*****************************************/
void primary_s_relational_expression_prime(void){
	if (lookahead.code == REL_OP_T) {
		switch (lookahead.attribute.rel_op) {
		case EQ:
			match(REL_OP_T, EQ);
			break;
		case NE:
			match(REL_OP_T, NE);
			break;
		case GT:
			match(REL_OP_T, GT);
			break;
		case LT:
			match(REL_OP_T, LT);
			break;
		default:
			syn_printe();
			break;
		}
	}
	primary_s_relational_expression();
}