/* *******************************************************************
* File Name:		parser.c
* Compiler:			Visual Studio Premium 2012
* Author:			Long Tran, 040815454
					Amuary Diaz Diaz, 040738985
* Course:			CST8152 - Compilers, Lab Section: 012, 011
* Assignment:		Assignment 4 - The Parser
* Date:				December 11, 2015
* Professor:		Svillen Ranev
* Purpose:			Responsible of implementing the Syntactical 
					Analysis of the PLATYPUS Language. It takes
					the tokens produced by the scanner and then
					finds if they sre in the proper syntactic 
					order.
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
#include "parser.h"
/* ************************************************************************
* Purpose:		To initialize the first lookahead used by the parser
				and start parsing the PLATYPUS program.
* Author:		Professor Svillen Ranev
* Version:		1.0
* Date:		1 December 2015
* ***********************************************************************/
void parser(Buffer * in_buf){
	sc_buf = in_buf;
	lookahead= mlwpar_next_token(sc_buf);
	program(); match(SEOF_T,NO_ATTR);
	gen_incode("PLATY: Source file parsed");
}
/* ***************************************************************************
* Purpose:			The purpose of this function is to match the token passed
					in the argument list ( and the attribute also if the
					token is one of KW_T, ART_OP_T, REL_OP_T, LOG_OP_T)
					with the token current token in the lookahead.
* Author:			Long Tran, Amaury Diaz Diaz
* History/Version: 1.0 - December 11, 2015
* Called Function:	syn_eh(),syn_printe(),mlwpar_next_token()
* Parameters:		Data Type: int; Name: pr_token_code; Range: 0 to 18
					Data Type: int; Name: pr_token_attribute; Range: -1 to 7
* Return Value:     void
* Algorithm:		Find if the token in the lookahed matches the token
					passed in the parameter(token required for a correct syntax)
					If they are not equal call the error handling function
					syn_eh(). Otherwise for tokens KW_T,ART_OPT,REL_OP_T,
					LOG_OP_T check that the attribute matches. If they are not
					equal call the error handling function syn_eh() and
					return. For any other token check that the lookahead
					is different from SEOF_T to avoid overrunning the buffer
					then move the lookahead and if the token is ERR_T 
					call the error printing function syn_printe(), 
					move the lookahead again and adds 1 to the error counting 
					variable synerrno
* ***************************************************************************/
void match(int pr_token_code,int pr_token_attribute){
	/*Compare first if the token code are the same if not go to syn_eh right away*/
	if(lookahead.code==pr_token_code){
		/*For this tokens find if the attribute matches as well*/
		if(pr_token_code==KW_T||pr_token_code==ART_OP_T||pr_token_code==REL_OP_T||pr_token_code==LOG_OP_T){
			switch(pr_token_code){
			case KW_T:
				/*If attribute does not match go to syn_eh and return*/
				if(lookahead.attribute.kwt_idx!=pr_token_attribute){
					syn_eh(pr_token_code);
					return;    
				}
				break;
			case ART_OP_T:
				/*If attribute does not match go to syn_eh and return*/
				if(lookahead.attribute.arr_op!=pr_token_attribute){
					syn_eh(pr_token_code);
					return;    
				}
				break;
			case REL_OP_T:
				/*If attribute does not match go to syn_eh and return*/
				if(lookahead.attribute.rel_op!=pr_token_attribute){
					syn_eh(pr_token_code);
					return;    
				}
				break;
			case LOG_OP_T:
				/*If attribute does not match go to syn_eh and return*/
				if(lookahead.attribute.log_op!=pr_token_attribute){
					syn_eh(pr_token_code);
					return;    
				}
				break;
			}
		}
		/*If the lookahead is SEOF_T return right away*/
		if(lookahead.code!=SEOF_T){
			/*Otherwise advance the lookahead*/
			lookahead=mlwpar_next_token(sc_buf);
			/*In case the lookahed is ERR_T token*/
			if(lookahead.code==ERR_T){
				/*Print the corresponding error*/
				syn_printe();
				/*Advance the lookahead again*/
				lookahead=mlwpar_next_token(sc_buf);
				/*Increment the number of errors found*/
				++synerrno;
			}
		}
		return;

	}
	syn_eh(pr_token_code);
	return;
}
/* ***************************************************************************
* Purpose:			The purpose of this function is to advance the lookahead
					until it finds a token required by the pareser to have 
					a proper syntax.
* Author:			Long Tran, Amaury Diaz Diaz
* History/Version: 1.0 - December 11, 2015
* Called Function:	exit(),syn_printe(),mlwpar_next_token()
* Parameters:		Data Type: int; Name: sync_token_code; Range: 0 to 18
* Return Value:     void
* Algorithm:		Calls the error printing function syn_printe() and 
					adds 1 to the error counting variable synerrno. Then
					it loops until it finds in the lookahead a token required
					by the parser for a proper syntax. If it finds the 
					source end of file in the lookahead and it does not
					match the required token it exits the function. If the
					lookahead is equal to the required token the lookahead
					is moved one more time and the function returns.
* ***************************************************************************/
void syn_eh(int sync_token_code){
	syn_printe();
	++synerrno;

	/*Keep advance lookahead, until it matches with token code required by parser*/
	while (lookahead.code != sync_token_code){
		lookahead = mlwpar_next_token(sc_buf);
		/*If lookahead is SEOF_T and the required token is not SEOF_T exit the function*/
		if(lookahead.code == SEOF_T && sync_token_code != SEOF_T){
			exit(synerrno);
		}
		/*Required token found in the lookahead*/
		if(lookahead.code == sync_token_code){
			/*Case SEOF_T return from the function right away*/
			if(lookahead.code == SEOF_T){
				return;
			}
			/*Otherwise advance the lookahead and return*/
			else{
				lookahead = mlwpar_next_token(sc_buf);
				return;
			}
		}
	}
}
/* ************************************************************************
* Purpose:		To print the syntax error produced by the parser based
				on the token code that generated the error.
* Author:		Professor Svillen Ranev
* Version:		1.0
* Date:		1 December 2015
* ***********************************************************************/
void syn_printe(){
	Token t = lookahead;

	printf("PLATY: Syntax error:  Line:%3d\n",line);
	printf("*****  Token code:%3d Attribute: ", t.code);
	switch(t.code){
	case  ERR_T: /* ERR_T     0   Error token */
		printf("%s\n",t.attribute.err_lex);
		break;
	case  SEOF_T: /*SEOF_T    1   Source end-of-file token */
		printf("NA\n" );
		break;
	case  AVID_T: /* AVID_T    2   Arithmetic Variable identifier token */
	case  SVID_T :/* SVID_T    3  String Variable identifier token */
		printf("%s\n",sym_table.pstvr[t.attribute.get_int].plex);
		break;
	case  FPL_T: /* FPL_T     4  Floating point literal token */
		printf("%5.1f\n",t.attribute.flt_value);
		break;
	case INL_T: /* INL_T      5   Integer literal token */
		printf("%d\n",t.attribute.get_int);
		break;
	case STR_T:/* STR_T     6   String literal token */
		printf("%s\n",b_setmark(str_LTBL,t.attribute.str_offset));
		break;

	case SCC_OP_T: /* 7   String concatenation operator token */
		printf("NA\n" );
		break;

	case  ASS_OP_T:/* ASS_OP_T  8   Assignment operator token */
		printf("NA\n" );
		break;
	case  ART_OP_T:/* ART_OP_T  9   Arithmetic operator token */
		printf("%d\n",t.attribute.get_int);
		break;
	case  REL_OP_T: /*REL_OP_T  10   Relational operator token */ 
		printf("%d\n",t.attribute.get_int);
		break;
	case  LOG_OP_T:/*LOG_OP_T 11  Logical operator token */
		printf("%d\n",t.attribute.get_int);
		break;

	case  LPR_T: /*LPR_T    12  Left parenthesis token */
		printf("NA\n" );
		break;
	case  RPR_T: /*RPR_T    13  Right parenthesis token */
		printf("NA\n" );
		break;
	case LBR_T: /*    14   Left brace token */
		printf("NA\n" );
		break;
	case RBR_T: /*    15  Right brace token */
		printf("NA\n" );
		break;

	case KW_T: /*     16   Keyword token */
		printf("%s\n",kw_table [t.attribute.get_int]);
		break;

	case COM_T: /* 17   Comma token */
		printf("NA\n");
		break;
	case EOS_T: /*    18  End of statement *(semi - colon) */
		printf("NA\n" );
		break; 		
	default:
		printf("PLATY: Scanner error: invalid token code: %d\n", t.code);
	}/*end switch*/
}/* end syn_printe()*/

/* ***************************************************************************
* Purpose:			Print the message passed in the parameter.
* Author:			Long Tran, Amaury Diaz Diaz
* History/Version: 1.0 - December 11, 2015
* Called Function:	exit(),syn_printe(),mlwpar_next_token()
* Parameters:		Data Type: char*; Name: message; Range: 0 to 255
* Return Value:     void
* Algorithm:		Just prints the message passed in the parameter.
* ***************************************************************************/
void gen_incode(char* message){
	printf("%s\n",message);
}
/*
AUTHOR:Long Tran

PRODUCTION:
 <program>  ->
  	PLATYPUS {<opt_statements>} 

FIRST SET:

FIRST(program) = {KW_T (PLATYPUS)}

*/
void program(void){
	match(KW_T,PLATYPUS);match(LBR_T,NO_ATTR);opt_statements();
	match(RBR_T,NO_ATTR);
	gen_incode("PLATY: Program parsed");
}

/*
AUTHOR: Long Tran

PRODUCTION:

opt_statements -> <statements> | EPSILON

FIRST SET:

FIRST(opt_statements) = {FIRST(statements), EPSILON}
FIRST(statements) = FIRST(statement)
FIRST(statement) = {AVID, SVID, KW_T(IF), KW_T(USING), KW_T(INPUT), KW_T(OUTPUT)}
*/

void opt_statements(){
	/* FIRST set: {AVID_T,SVID_T,KW_T(but not … see above),e} */
	switch(lookahead.code){
	case AVID_T:
	case SVID_T: statements();break;
	case KW_T:
		/* check for PLATYPUS, ELSE, THEN, REPEAT here and in
		statements_p()*/
		if (lookahead. attribute. get_int != PLATYPUS
			&& lookahead. attribute. get_int != ELSE
			&& lookahead. attribute. get_int != THEN
			&& lookahead. attribute. get_int != REPEAT){
				statements();
				break;
		}
	default: /*empty string – optional statements*/ ;
		gen_incode("PLATY: Opt_statements parsed");
	}
}


/*
AUTHOR: Long Tran

PRODUCTION:

<statements> ->
	<statement><prime_statements>

FIRST SET:

FIRST(statements) ={ FIRST(statement) }
FIRST(statement) = {AVID, SVID, KW_T(IF), KW_T(USING), KW_T(INPUT), KW_T(OUTPUT)}
*/
void statements(void){
	statement();
	prime_statement();
}

/*
AUTHOR: Long Tran

PRODUCTION:

<statement> ->
  <assignment statement>
	| <selection statement>
	| <iteration statement>
	| <input statement>
	| <output statement>

FIRST SET:

FIRST(statement)  = 
{FIRST(assignment statement), FIRST(selection statement), FIRST(iteration statement), FIRST(input statement), FIRST(output statement)}
= {AVID, SVID, KW_T(IF), KW_T(USING), KW_T(INPUT), KW_T(OUTPUT)}
*/
void statement(void){
	switch (lookahead.code){
	case AVID_T:
	case SVID_T:
		assignment_statement();
		break;

		/*If lookahead is a keyword, then check its attribute*/
	case KW_T:
		switch (lookahead.attribute.kwt_idx){
		case IF:
			selection_statement();
			break;

		case USING:
			iteration_statement();
			break;

		case INPUT:
			input_statement();
			break;

		case OUTPUT:
			output_statement();
			break;
		}
		break; /*break of KW_T*/
	default:  /*Print error if there is no match*/
		syn_printe();
		break;
	}
}
/*
AUTHOR: Long Tran

PRODUCTION:

<prime_statements> ->
	<statement><prime_statements> | EPSILON

FIRST SET:

FIRST(prime_statement) = { FIRST(statement), EPSILON }
FIRST(statement)  = 
{FIRST(assignment statement), FIRST(selection statement), FIRST(iteration statement), FIRST(input statement), FIRST(output statement)}
= {AVID, SVID, KW_T(IF), KW_T(USING), KW_T(INPUT), KW_T(OUTPUT)}
*/
void prime_statement(void){
	switch (lookahead.code){
	case AVID_T:
	case SVID_T:
		statement();
		prime_statement();
		break;

	case KW_T:
		/* If any other keywords rather than PLATYPUS, ELSE, THEN, 
		REPEAT here. Then call statement, followed by prime statement()
		*/
		if (lookahead.attribute.kwt_idx    != PLATYPUS
			&& lookahead.attribute.kwt_idx != ELSE
			&& lookahead.attribute.kwt_idx != THEN
			&& lookahead.attribute.kwt_idx != REPEAT){
				statements();
				prime_statement();
		}
		break;
	}
}
/*
AUTHOR: Long Tran

PRODUCTION:
<assignment statement> -> 
	<assignment expression>;

FIRST SET:
FIRST(assignment statement) 
= {FIRST (assignment expression)} 
= {AVID, SVID}

*/
void assignment_statement(void){
	assignment_expression();
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Assignment statement parsed");
}

/*
AUTHOR: Long Tran

PRODUCTION:
<selection statement> ->
  IF (<conditional expression>)  THEN  <opt_statements> 
  ELSE { <opt_statements> } ;

FIRST SET:
FIRST(<selection statement>)={KW_T(IF)}

*/
void selection_statement(void){
	match(KW_T, IF); match(LPR_T, NO_ATTR); conditional_expression(); match(RPR_T, NO_ATTR);
	match(KW_T, THEN);	opt_statements();
	match(KW_T, ELSE); match(LBR_T, NO_ATTR); opt_statements(); match(RBR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: IF statement parsed");
}

/*
AUTHOR: Long Tran

PRODUCTION:
<iteration statement> ->
	USING  (<assignment expression> , <conditional expression> , <assignment  expression> )
	REPEAT {
		< opt_statements>
	};

FIRST SET:
FIRST(<iteration statement>)={KW_T(USING)}

*/
void iteration_statement(void){
	match(KW_T, USING); match(LPR_T, NO_ATTR); assignment_expression(); 
	match(COM_T, NO_ATTR); conditional_expression();match(COM_T, NO_ATTR); assignment_expression(); match(RPR_T, NO_ATTR);
	match(KW_T, REPEAT); match(LBR_T, NO_ATTR); opt_statements(); match(RBR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: USING statement parsed");
}

/*
AUTHOR: Long Tran

PRODUCTION:
<input statement> ->
	INPUT (<variable list>);

FIRST SET:
FIRST SET:
FIRST(<input statement>)={KW_T(INPUT)}
*/
void input_statement(void){
	match(KW_T, INPUT); match(LPR_T, NO_ATTR); variable_list(); match(RPR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: INPUT statement parsed");
}

/*
AUTHOR: Long Tran

PRODUCTION:
<variable list> -> 
	<variable identifier> <prime_variable list>

FIRST SET:
FIRST(<variable list>)
={FIRST(<variable identifier>)}
={AVID_T,SVID_T}


*/
void variable_list(void){
	variable_identifier();
	prime_variable_list();
	gen_incode("PLATY: Variable list parsed");
}
/*
AUTHOR: Long Tran

PRODUCTION:
<variable identifier>-> AVID_T|SVID_T

FIRST SET:
FIRST(<variable identifier>)={AVID_T,SVID_T}

*/
void variable_identifier(void){
	switch (lookahead.code){
	case AVID_T:
	case SVID_T:
		match(lookahead.code, NO_ATTR);
		break;
	default:
		syn_printe();
		break;
	}
}

/*
AUTHOR: Long Tran

PRODUCTION:
<prime variable list>->
	,<variable identifier><prime variable list>|EPSILON

FIRST SET:
FIRST(<prime variable list>)={,,EPSILON}

*/
void prime_variable_list(void){
	/*Check for epsilon*/
	if(lookahead.code != COM_T)
		return;

	match(lookahead.code, COM_T);
	variable_identifier();
	prime_variable_list();
}

/*
AUTHOR: Long Tran

PRODUCTION:
<output statement> ->
  OUTPUT (<output list>);

FIRST SET:
FIRST(<output statement>)={KW_T(OUTPUT)}

*/
void output_statement(void){
	match(KW_T, OUTPUT);
	match(LPR_T, NO_ATTR);
	output_list();
	match(RPR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: OUTPUT statement parsed");
}

/*
AUTHOR: Long Tran

PRODUCTION:
<output list> ->
     <variable list>| STR_T| EPSILON

FIRST SET:
FIRST(<output list>)={SVID_T,AVID_T,STR_T, EPSILON}

*/
void output_list(void){
	switch (lookahead.code){
	case AVID_T:
	case SVID_T:
		variable_list();
		break;
	case STR_T:
		match(STR_T, NO_ATTR);
		gen_incode("PLATY: Output list (string literal) parsed");
		break;
	default:
		/* if it's EPSILON*/
		gen_incode("PLATY: Output list (empty) parsed");
		break;
	}

}

/*
AUTHOR: Long Tran

PRODUCTION:
< assignment expression> -> 
  AVID = <arithmetic expression>
| SVID = <string expression>

FIRST SET:
FIRST(assignment expression) = {AVID, SVID}

*/
void assignment_expression(void){
	switch (lookahead.code){
	case AVID_T:
		match(lookahead.code, NO_ATTR);
		match(ASS_OP_T, NO_ATTR);
		arithmetic_expression();
		gen_incode("PLATY: Assignment expression (arithmetic) parsed");
		break;
	case SVID_T:
		match(lookahead.code, NO_ATTR);
		match(ASS_OP_T, NO_ATTR);
		string_expression();
		gen_incode("PLATY: Assignment expression (string) parsed");
		break;
	default:
		syn_printe();
		break;
	}

}

/*
AUTHOR: Long Tran

PRODUCTION:
<arithmetic expression> - >
	<unary arithmetic expression>  
	| <additive arithmetic expression>

FIRST SET:
FIRST(<arithmetic expression>)={FIRST(<unary arithmetic expression>), FIRST(<additive arithmetic expression>)}
={ ART_OP_T(+),ART_OP_T(-),FIRST(<multiplicative arithmetic expression>)}
={ ART_OP_T(+),ART_OP_T(-),FIRST(<primary arithmetic expression>)}
={ ART_OP_T(+),ART_OP_T(-),AVID_T,FPL_T,INL_T,(}

*/
void arithmetic_expression(void){
	switch(lookahead.code){
	case ART_OP_T:
		switch(lookahead.attribute.arr_op){
		case MULT:
		case DIV:
			syn_printe();
			return;
		case PLUS:
		case MINUS:
			unary_arithmetic_expression();
			break;
		}
		break;
	case AVID_T:
	case FPL_T:
	case INL_T:
	case LPR_T:
		additive_arithmetic_expression();
		break;
	default:
		syn_printe();
		return;
	}
	gen_incode("PLATY: Arithmetic expression parsed");
}

/*
AUTHOR: Long Tran

PRODUCTION:
<unary arithmetic expression> ->
	-  <primary arithmetic expression> 
	| + <primary arithmetic expression>

FIRST SET:
FIRST(<unary arithmetic expression>)={ART_OP_T(+),ART_OP_T(-)}

*/
void unary_arithmetic_expression(void){
	switch(lookahead.attribute.arr_op){
	case PLUS:
	case MINUS:
		match(lookahead.code,lookahead.attribute.arr_op);
		primary_arithmetic_expression();
		break;
	default:
		syn_printe();
		return;
	}
	gen_incode("PLATY: Unary arithmetic expression parsed");
}

/*
AUTHOR: Amaury Diaz Diaz

PRODUCTION:
<additive arithmetic expression>->
	<multiplicative arithmetic expression> <prime additive arithmetic expression>

FIRST SET:
FIRST(<additive arithmetic expression>)
={FIRST(<multiplicative arithmetic expression>)}
={FIRST(<primary arithmetic expression>)}
={AVID_T,FPL_T,INL_T,(}

*/
void additive_arithmetic_expression(void){
	switch(lookahead.code){
	case AVID_T:
	case FPL_T:
	case INL_T:
	case LPR_T:
		multiplicative_arithmetic_expression();
		prime_additive_arithmetic_expression();
		break;
	default:
		syn_printe();
		return;
	}

}

/*
AUTHOR: Amaury Diaz Diaz

PRODUCTION:
<prime additive arithmetic expression>->
	+ <multiplicative arithmetic expression><prime additive arithmetic expression>|
	- <multiplicative arithmetic expression><prime additive arithmetic expression>|
	EPSILON

FIRST SET:
FIRST(<prime additive arithmetic expression>) = 
{ ART_OP_T(+),ART_OP_T(-),EPSILON}

*/
void prime_additive_arithmetic_expression(void){
	switch(lookahead.code){
	case ART_OP_T:
		switch(lookahead.attribute.arr_op){
		case DIV:
		case MULT:
			syn_printe();
			return;
		case PLUS:
		case MINUS:
			match(lookahead.code,lookahead.attribute.arr_op);
			multiplicative_arithmetic_expression();
			prime_additive_arithmetic_expression();
			gen_incode("PLATY: Additive arithmetic expression parsed");
			break;
		}
	default:;
	}
}

/*
AUTHOR: Amaury Diaz Diaz

PRODUCTION:
<multiplicative arithmetic expression>->
	<primary arithmetic expression> <prime multiplicative arithmetic expression>

FIRST SET:
FIRST(<multiplicative arithmetic expression>)
={FIRST(<primary arithmetic expression>)}
={AVID_T,FPL_T,INL_T,(}

*/
void multiplicative_arithmetic_expression(void){
	switch(lookahead.code){
	case AVID_T:
	case FPL_T:
	case INL_T:
	case LPR_T:
		primary_arithmetic_expression();
		prime_multiplicative_arithmetic_expression();
		break;
	default:
		syn_printe();
		return;
	}

}

/*
AUTHOR: Amaury Diaz Diaz

PRODUCTION:
<prime multiplicative arithmetic expression>->
	* <primary arithmetic expression><prime multiplicative arithmetic expression>|
	/ <primary arithmetic expression><prime multiplicative arithmetic expression>|
	EPSILON

FIRST SET:
FIRST(<prime multiplicative arithmetic expression>) =
{ ART_OP_T(*),ART_OP_T(/)},EPSILON}

*/
void prime_multiplicative_arithmetic_expression(void){
	switch(lookahead.code){
	case ART_OP_T:
		switch(lookahead.attribute.arr_op){
		case DIV:
		case MULT:
			match(lookahead.code,lookahead.attribute.arr_op);
			primary_arithmetic_expression();
			prime_multiplicative_arithmetic_expression();	
			gen_incode("PLATY: Multiplicative arithmetic expression parsed");
		case PLUS:
		case MINUS:
			return;
		}
	}
}

/*
AUTHOR: Amaury Diaz Diaz

PRODUCTION:
<primary arithmetic expression> ->
	AVID_T
	| FPL_T
	| INL_T
	| (<arithmetic expression>)	

FIRST SET:
FIRST(<primary arithmetic expression>)={AVID_T,FPL_T,INL_T,(}

*/
void primary_arithmetic_expression(void){
	switch(lookahead.code){
	case AVID_T:
	case FPL_T:
	case INL_T:
		match(lookahead.code,NO_ATTR);
		break;
	case LPR_T:
		match(lookahead.code,NO_ATTR);
		arithmetic_expression();
		match(RPR_T,NO_ATTR);
		break;
	default:
		syn_printe();
		return;
	}
	gen_incode("PLATY: Primary arithmetic expression parsed");
}

/*
AUTHOR: Amaury Diaz Diaz

PRODUCTION:
<string expression>->
	<primary string expression><prime string expression>

FIRST SET:
FIRST(<string expression>)={FIRST(<primary string expression>)}
={SVID_T,STR_T}

*/
void string_expression(void){
	switch(lookahead.code){
	case SVID_T:
	case STR_T:
		primary_string_expression();
		prime_string_expression();
		break;
	default:
		syn_printe();
		return;
	}
	gen_incode("PLATY: String expression parsed");
}

/*
AUTHOR: Amaury Diaz Diaz

PRODUCTION:
<prime string expression>->
	#<primary string expression><prime string expression>| EPSILON

FIRST SET:
FIRST(<prime string expression>)={SCC_OP_T,EPSILON}

*/
void prime_string_expression(void){
	switch(lookahead.code){
	case SCC_OP_T:
		match(lookahead.code,NO_ATTR);
		primary_string_expression();
		prime_string_expression();
		break;
	default:;
	}
}

/*
AUTHOR: Amaury Diaz Diaz

PRODUCTION:
<primary string expression> ->
	SVID_T
	| STR_T

FIRST SET:
FIRST(<primary string expression>)={SVID_T,STR_T}

*/
void primary_string_expression(void){
	switch(lookahead.code){
	case SVID_T:
	case STR_T:
		match(lookahead.code,NO_ATTR);
		break;
	default:
		syn_printe();
		return;
	}
	gen_incode("PLATY: Primary string expression parsed");
}

/*
AUTHOR: Amaury Diaz Diaz

PRODUCTION:
<conditional expression> ->
	<logical OR  expression>

FIRST SET:
FIRST(<conditional expression>)= FIRST(<logical OR expression>)
=FIRST(<logical AND expression>)
={FIRST(<relational expression>)}
={FIRST(<primary a_relational expression>), FIRST(<primary s_relational expression>)}
={ AVID_T,FPL_T,INL_T, FIRST(<primary string expression>)}
={ AVID_T,FPL_T,INL_T, SVID_T,STR_T }

*/
void conditional_expression(void){
	logical_or_expression();
	gen_incode("PLATY: Conditional expression parsed");
}

/*
AUTHOR: Amaury Diaz Diaz

PRODUCTION:
<logical OR expression>->
	<logical AND expression><prime logical OR expression>

FIRST SET:
FIRST(<logical OR expression>)= FIRST(<logical AND expression>)
={FIRST(<relational expression>)}
={FIRST(<primary a_relational expression>), FIRST(<primary s_relational expression>)}
={ AVID_T,FPL_T,INL_T, FIRST(<primary string expression>)}
={ AVID_T,FPL_T,INL_T, SVID_T,STR_T }

*/
void logical_or_expression(void){
	logical_and_expression();
	prime_logical_or_expression();
}

/*
AUTHOR: Amaury Diaz Diaz

PRODUCTION:
<prime logical OR expression>->
	.OR. <logical AND expression><prime logical OR expression>|
	EPSILON

FIRST SET:
FIRST(<prime logical OR expression>)={LOG_OP_T(.OR.),EPSILON}

*/
void prime_logical_or_expression(void){
	switch(lookahead.code){
	case LOG_OP_T:
		switch(lookahead.attribute.log_op){
		case OR:
			match(lookahead.code,lookahead.attribute.log_op);
			logical_and_expression();
			prime_logical_or_expression();
			gen_incode("PLATY: Logical OR expression parsed");
		}
	default:;
	}
}

/*
AUTHOR: Amaury Diaz Diaz

PRODUCTION:
<logical AND expression>->
	<relational expression><prime logical AND expression>

FIRST SET:
FIRST(<logical AND expression>)={FIRST(<relational expression>)}
={FIRST(<primary a_relational expression>), FIRST(<primary s_relational expression>)}
={ AVID_T,FPL_T,INL_T, FIRST(<primary string expression>)}
={ AVID_T,FPL_T,INL_T, SVID_T,STR_T }

*/
void logical_and_expression(void){
	relational_expression();
	prime_logical_and_expression();

}

/*
AUTHOR: Amaury Diaz Diaz

PRODUCTION:
<prime logical AND expression>->
	.AND. <relational expression><prime logical AND expression>|
	EPSILON

FIRST SET:
FIRST(<prime logical AND expression>)={ LOG_OP_T(.AND.),EPSILON}

*/
void prime_logical_and_expression(void){
	switch(lookahead.code){
	case LOG_OP_T:
		switch(lookahead.attribute.log_op){
		case AND:
			match(lookahead.code,lookahead.attribute.log_op);
			relational_expression();
			prime_logical_and_expression();
			gen_incode("PLATY: Logical AND expression parsed");
		}
	default:;
	}
}

/*
AUTHOR: Amaury Diaz Diaz

PRODUCTION:
<relational expression>->
	<primary a_relational expression> <relational operator> <primary a_relational expression> |
	<primary s_relational expression> <relational operator> <primary s_relational expression>

FIRST SET:
FIRST(<relational expression>)={FIRST(<primary a_relational expression>), FIRST(<primary s_relational expression>)}
={ AVID_T,FPL_T,INL_T, FIRST(<primary string expression>)}
={ AVID_T,FPL_T,INL_T, SVID_T,STR_T }

*/
void relational_expression(void){
	switch(lookahead.code){
	case AVID_T:
	case FPL_T:
	case INL_T:
		primary_a_relational_expression();
		relational_operator();
		primary_a_relational_expression();
		break;
	case SVID_T:
	case STR_T:
		primary_s_relational_expression();
		relational_operator();
		primary_s_relational_expression();
		break;
	default:
		syn_printe();
	}
	gen_incode("PLATY: Relational expression parsed");
}

/*
AUTHOR: Amaury Diaz Diaz

PRODUCTION:
<primary a_relational expression> ->
	AVID_T
	| FPL_T
	| INL_T

FIRST SET:	
FIRST(<primary a_relational expression>)={AVID_T,FPL_T,INL_T}

*/
void primary_a_relational_expression(void){
	switch(lookahead.code){
	case AVID_T:
	case FPL_T:
	case INL_T:
		match(lookahead.code,NO_ATTR);
		break;
	default:
		syn_printe();
	}
	gen_incode("PLATY: Primary a_relational expression parsed");
}

/*
AUTHOR: Amaury Diaz Diaz

PRODUCTION:
<relational operator>->
	one of >,<,==,<>

FIRST SET:
FIRST(<relational operator>) 
= {REL_OP_T } 
= { REL_OP_T(>),REL_OP_T(<),REL_OP_T(==),REL_OP_T(<>)}

*/
void relational_operator(void){
	switch(lookahead.code){
	case REL_OP_T:
		match(lookahead.code,lookahead.attribute.rel_op);
		break;
	default:
		syn_printe();
		return;
	}
}

/*
AUTHOR: Amaury Diaz Diaz

PRODUCTION:
<primary s_relational expression> ->
	<primary string expression>

FIRST SET:
FIRST(<primary s_relational expression>)
={FIRST(<primary string expression>)}
={SVID_T,STR_T}

*/
void primary_s_relational_expression(){
	switch(lookahead.code){
	case SVID_T:
	case STR_T:
		primary_string_expression();
		break;
	default:
		syn_printe();
	}
	gen_incode("PLATY: Primary s_relational expression parsed");
}