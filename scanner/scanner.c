/* *******************************************************************
* File Name:		scanner.c
* Compiler:		Visual Studio Premium 2012
* Author:			Long Tran, 040815454
Amuary Diaz Diaz, 040738985
* Course:			CST8152 - Compilers, Lab Section: 012, 011
* Assignment:		Assignment 2 - Scanner
* Date:			October 27, 2015
* Professor:		Svillen Ranev
* Purpose:			Functions implementing a Lexical Analyzer (Scanner)
The scanner reads a source program from a text file
and produces a stream of token representation
* Function list:	scanner_init(), char_class(), get_next_state(),
iskeyword(), atool(), mlwpar_next_token(),
aa_function02(), aa_function03(), aa_function05()
aa_function08(), aa_function10(), aa_function12()
* *******************************************************************/

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
#include <math.h>
/*#define NDEBUG        to suppress assert() call */
#include <assert.h>  /* assert() prototype */

/* project header files */
#include "buffer.h"
#include "token.h"
#include "table.h"
#include "stable.h"

#define DEBUG  /* for conditional processing */
#undef  DEBUG

/* Global objects - variables */
/* This buffer is used as a repository for string literals.
It is defined in platy_st.c */
extern Buffer * str_LTBL;						/*String literal table */
int line;										/* current line number of the source code */
extern int scerrnum;							/* defined in platy_st.c - run-time error number */
extern STD sym_table;
/* Local(file) global objects - variables */
static Buffer *lex_buf;							/*pointer to temporary lexeme buffer*/

/* No other global variable declarations/definitiond are allowed */

/* scanner.c static(local) function  prototypes */ 
static int char_class(char c);					/* character class function */
static int get_next_state(int, char, int *);	/* state machine function */
static int iskeyword(char * kw_lexeme);			/*keywords lookup functuion */
static long atool(char * lexeme);				/* converts octal string to decimal value */

/* ************************************************************************
* Purpose:		To initialize the scanner and buffer program by reseting all
data member to 0 and to the begining of the buffer
* Author:		Professor Svillen Ranev
* Version:		1.15.02
* Date:		29 September 2015
* ***********************************************************************/
int scanner_init(Buffer * sc_buf) {
	if(b_isempty(sc_buf)) return EXIT_FAILURE;/*1*/
	/* in case the buffer has been read previously  */
	b_setmark(sc_buf, 0);
	b_retract_to_mark(sc_buf);
	b_reset(str_LTBL);
	line = 1;
	return EXIT_SUCCESS;/*0*/
	/*   scerrnum = 0;  *//*no need - global ANSI C */
}



/* ***************************************************************************
* Purpose:			The purpose of this function is to be able to identify and 
the return the corrected token from the source, buffer.
* Author:			Long Tran, Amaury Diaz Diaz
* History/Version: 1.0 - September 23, 2015
* Called Function:	b_getc(), b_retract(), b_setmark(), b_retract_to_mark()
*					b_getc_offset(), b_addc(), isalnum(), b_create(), b_destroy()
* Parameters:		Data Type: Buffer; Name: sc_buf; Range: 0 to 255
* Return Value:    Token
* Algorithm:		Read the buffer, sc_buf character by character to identify
corrected token and set corresponding attribute based on
the token and return the token to calling function
* ***************************************************************************/
Token mlwpar_next_token(Buffer * sc_buf)
{
	Token t;				/* token to return after recognition */
	unsigned char c;		/* input symbol */
	int state = 0;			/* initial state of the FSM */
	short lexstart;			/*start offset of a lexeme in the input buffer */
	short lexend;			/*end   offset of a lexeme in the input buffer */
	int accept = NOAS;		/* type of state - initially not accepting */  

	int i = 0;				/* iteration variable*/
	int rangeOfLexeme = 0;  /* offset of lexeme from lexstart to lexend*/
	short lexemeCapacity;   /* number of lexeme char from lexstart to lexend*/
	/* 
	lexstart is the offset from the beginning of the char buffer of the
	input buffer (sc_buf) to the first character of the current lexeme,
	which is being processed by the scanner.
	lexend is the offset from the beginning of the char buffer of the
	input buffer (sc_buf) to the last character of the current lexeme,
	which is being processed by the scanner.
	*/    

	/*Check for the invalid buffer - return error token*/
	if(sc_buf == NULL){
		t.code = ERR_T;
		return t;
	}

	/* endless loop broken by token returns it will generate a warning */
	while (1){ 
		/*Get the current symbol using getc_offset as index in buffer array*/
		c = (unsigned char) b_getc(sc_buf);							
		/*Check for SOURCE END OF FILE TOKEN*/
		if((c == '\0') || c == SEOF ){
			t.code = SEOF_T; 
			return t;
		}
		switch (c){	
			/*Continue, reading character if character is line terminated symbol*/
		case ' ': case'\t': case'\v': case'\f':
			break;
			/*Check if new line or return line is read*/
		case '\n': case '\r':							
			line +=1;                            /*Increment line by 1*/
			break;
			/*Check if concatenated symbol is read - return SCC_OP_T token*/
		case '#':
			t.code = SCC_OP_T;	return t;		/*Set concatenation token*/
			break;

		case '=':
			c = b_getc(sc_buf);
			/*****************************************************************
			If first = is found, check the following character, 
			if next char is = then its "==" token, assigned with REL_OPT
			******************************************************************/
			if(c == '='){											
				t.attribute.rel_op = EQ;                            
				t.code = REL_OP_T;
				return t;
			}
			/* ******************************************************************
			Retract back the current character after first quotation; in case
			it's not relation operator
			* *****************************************************************/
			b_retract(sc_buf);										
			/*Otherwise, assigned and return Assignment token ASS_OP_T*/
			t.code = ASS_OP_T;	return t;							
			break;

			/* ******************************
			* Check for Arthmectic Operation
			* and return corresponded token
			* ******************************/
		case '+':
			t.attribute.arr_op = PLUS;
			t.code = ART_OP_T;
			return t;
			break;
		case '-':
			t.attribute.arr_op = MINUS;
			t.code = ART_OP_T;
			return t;
			break;
		case '*':
			t.attribute.arr_op = MULT;
			t.code = ART_OP_T;
			return t;
			break;
		case '/':
			t.attribute.arr_op = DIV;
			t.code = ART_OP_T;
			return t;
			break;

			/* ******************************
			* Check for Relation Operation
			* ******************************/
		case '<':
			c = b_getc(sc_buf);
			t.code = REL_OP_T;
			/*Check if the following character is >, then it's REL_T token*/
			if(c == '>'){																						
				t.attribute.rel_op = NE;
			}else{
				t.attribute.rel_op = LT;
				/*Retract the buffer so we don't miss following char after <*/
				b_retract(sc_buf);										
			}
			return t;
			break;
		case '>':
			t.code = REL_OP_T;
			t.attribute.rel_op = GT;
			return t;
			break;

			/* ******************************
			* Check for Logical Operation
			*      .AND.    .OR.
			* ******************************/
		case '.':
			/* *******************************************************************
			* Save the offset of source; sc_buffer - the position/index in 
			* sc_buffer so we can retract back to this location if it's not 
			* Logical Operation
			* *******************************************************************/ 
			b_setmark(sc_buf,b_getc_offset(sc_buf));                      
			c = (unsigned char)b_getc(sc_buf);
			switch (c){
			case 'A':
				c = (unsigned char)b_getc(sc_buf);
				if(c == 'N'){
					c = (unsigned char)b_getc(sc_buf);
					if(c == 'D'){
						c = (unsigned char)b_getc(sc_buf);
						if(c == '.'){
							t.code = LOG_OP_T;			/*set token - LOG_OP_T*/
							t.attribute.log_op = AND;   /*set the attribute of token*/
							return t;                   /*return t token*/
						}/*End of most inner if*/
					}/*End of second most inner if*/
				}/*End of the outer if*/
				break;									/*break for inner switch; case 'A'*/
			case 'O':
				c = (unsigned char)b_getc(sc_buf);
				if(c == 'R'){
					c = (unsigned char)b_getc(sc_buf);
					if(c == '.'){
						t.code = LOG_OP_T;
						t.attribute.log_op = OR;      /*set the attribute token; OR*/
						return t;
					}

				}
				break;
			}
			/*If the following characters are not either "AND" or "OR" */
			t.code = ERR_T;                   /*it's error token*/
			t.attribute.err_lex[0] = '.';     /*assign . and terminated ch as err_lex*/
			t.attribute.err_lex[1] = '\0';
			/*call b_retract_to_mark() method to retrack back to the possition after the '.' */
			b_retract_to_mark(sc_buf);					
			return t;
			break;

		case '{':
			t.code = LBR_T;	return t;			/*Set left bracket token*/
			break;

		case '}':
			t.code = RBR_T;	return t;			/*Set right bracket token*/
			break;

		case '(':
			t.code = LPR_T;						/*Set left parenthesis token*/
			return t;
			break;

		case ')':
			t.code = RPR_T;						/*Set right parenthesis token*/
			return t;
			break;

			/*Check for comma token*/
		case ',': 
			t.code = COM_T;
			return t;
			break;

			/*Check for semi-colon token*/
		case ';':
			t.code = EOS_T;
			return t;
			break;

			/* ***************************************************************
			* Check for Comment symbol and set proper requirements
			* **************************************************************/
		case '!':
			lexstart = b_getc_offset(sc_buf) - 1;
			/*set mark so we can retract using the mark*/
			b_setmark(sc_buf,lexstart);
			c = (unsigned char)b_getc(sc_buf);
			/* ***********************************************************
			* If it's comment sybmol, read until end of the line
			* by checking the line terminated character
			* **********************************************************/
			if(c == '<'){
				do{
					c=b_getc(sc_buf);
				}while(c !='\r' && c != '\n'  && c!= SEOF && c!='/0');						
				line++;                            /*increment line by 1*/
				continue;
			}else{
				/* *******************************************************
				* If it's not comment symbol, retrack back to mark
				* read ! and following character into err_lex array 
				* ignore the rest of the line 
				* set and return token to ERROR_T
				* *******************************************************/
				/*get the getc_offset of char after ! */
				lexend = b_getc_offset(sc_buf);												
				rangeOfLexeme = lexend - lexstart;					
				b_retract_to_mark(sc_buf);

				for(i = 0; i<rangeOfLexeme; i++){
					c = (unsigned char) b_getc(sc_buf);
					t.attribute.err_lex[i] = (signed char) c;
				}

				do{
					c = (unsigned char) b_getc(sc_buf);
					if(c == SEOF){
						b_retract(sc_buf);
					}
				}while(c != '\r' && c !='\n' && c!= '\0' && c!=SEOF );

				line++;				  /*increment the line by 1*/
				t.attribute.err_lex[i] = '\0';
				t.code = ERR_T;
				return t;
			}
			/* *****************************************************************
			* Check for String literal token.
			* ****************************************************************/
		case '"':
			/*set the mark at first quotation for retraction*/
			lexstart = b_getc_offset(sc_buf)-1 ;					
			/*Read char by char until the second quotation is found*/
			do{
				c = (unsigned char) b_getc(sc_buf);
				switch (c){
					/*Check if new line or return line termined is read*/
				case '\n': case '\r':							
					line +=1;				/*Increment line by 1*/
					break;
					/*Check source end of file or end of buffer is read*/ 
				case SEOF: case '\0':	
					/*get the distance from mark location to end of the file/buffer*/
					lexend = b_getc_offset(sc_buf);
					/*calculate the range of invalid lexeme*/
					rangeOfLexeme =(int)lexend - lexstart;				

					/*set the mark and retract to the mark; mark = lexstart*/
					b_setmark(sc_buf,lexstart);
					b_retract_to_mark(sc_buf);

					/*read invalid lexeme into err_lex based on condition*/
					/* **************************************************
					* if number char of lexeme is greater than number 
					* allowed char into err_lex, then only the first 
					* character to ERR_LEN is allowed to be added into
					* err_lex plus 3 dot ...Otherwise, read all lexeme
					* into the error_lex
					* *************************************************/
					if(rangeOfLexeme > ERR_LEN){
						for(i = 0; i<ERR_LEN; i++){
							c = (unsigned char)b_getc(sc_buf);
							if(i < ERR_LEN-3){
								t.attribute.err_lex[i] = (signed char)c;
							}
							else{
								t.attribute.err_lex[i]='.';
							}
						}
					}
					else{
						for(i = 0; i<rangeOfLexeme; i++){
							c = (unsigned char)b_getc(sc_buf);
							if(c != '"'){
								t.attribute.err_lex[i-1] = (signed char)c;
							}
						}
					}
					/*Add terminated character in the end of error_lex*/
					t.attribute.err_lex[i] = '\0';
					t.code = ERR_T;
					b_setmark(sc_buf,lexend);
					b_retract_to_mark(sc_buf);						
					return t;					/*set and return error token*/
					break;

				}/*End of switch statement*/
			}while(c != '"');

			/*if the following ch is second quotation mark " */
			lexend = b_getc_offset(sc_buf);							/*get "distance" of second double quotation from the beginning of the array*/
			b_setmark(sc_buf,lexstart);
			b_retract_to_mark(sc_buf);								/*Reset the buffer to the first double quotation " */

			rangeOfLexeme = (int)lexend- lexstart;

			t.attribute.str_offset = b_size(str_LTBL);              /*set the str_offset at index of first character in string*/
			for(i=0 ; i< rangeOfLexeme; i++){
				c = b_getc(sc_buf);									
				if(c != '"'){
					b_addc(str_LTBL, c);
				}
				if(str_LTBL == NULL){
					t.code = ERR_T;
					t.attribute.err_lex[0] = '"';
					t.attribute.err_lex[1] = '\0';
					return t;
				}
			}
			/*Done reading source buffer to str_LTBL - add '\0' to end of the string*/
			if(!b_addc(str_LTBL, '\0')){
				t.attribute.err_lex[0] = '\0';
				t.code = ERR_T;					/*Return token as ERROR_T token*/
				return t;
			}
			t.code = STR_T;                    /*Otherwise, it's string literal token*/
			return t;
			break;
		default:
			if(isalnum(c)){															
				/*Check if the c is digit or letter*/
				lexstart = b_getc_offset(sc_buf)-1;									
				/*set the mark for retracting purpose at begining of Lexeme*/
				b_setmark(sc_buf, lexstart);										
				/*get next stage */
				state = get_next_state(state, (signed char)c, &accept);

				/*Check if the STATE is NOAS*/
				if (accept == NOAS){													
					/*Read until the new accepting state is found*/
					do{																	
						c = (unsigned char)b_getc(sc_buf);
						/*Get state for every read char*/
						state = get_next_state(state, c, &accept);						
					}while(accept == NOAS);
				}

				/*if accepting state is ASWR, retract the buffer*/
				if(accept == ASWR){													
					b_retract(sc_buf);
				}

				/*get index at the end of the lexeme*/
				lexend = b_getc_offset(sc_buf);													
				/*get the range of the lexeme from start to end of lexeme*/
				lexemeCapacity = lexend - lexstart;									

				/*Create buffer with capicity calculated above to store all lexeme*/
				lex_buf = b_create(lexemeCapacity, 1, 'a');										
				/*Retract to the begining of lexeme*/
				b_retract_to_mark(sc_buf);

				for(i = 0; i<lexemeCapacity; i++){
					c = b_getc(sc_buf);               
					b_addc(lex_buf, c);     /*Add char by char into lex_buf*/
				}

				b_addc(lex_buf, '\0');		/*Adding trailing 0*/

				t = aa_table[state](b_setmark(lex_buf,0));
				b_destroy(lex_buf);
				return t;
			}else {							/*if char is not any of case captured above*/	
				t.code = ERR_T;             /*set and return token as error token*/
				t.attribute.err_lex[0] = c;
				t.attribute.err_lex[1] = '\0';
				return t;
			}  
		}
	}/*End of switch statement*/
}


/* *************************************************************************
* Purpose:			The purpose of this function is to find the corresponding 
state from function arguments via transition table
* Author:			Svillen Ranev
* History/Version: 1.15.2 - September 29, 2015
* Called Function: char_class()
* Parameter:		Data Type: int; name: state; range: 0 to 12
Data Type: char; name: c; range: 0 to 255
Data Type: pointer to int; name: accept
* Algorithm:		Validate the input and calling char_class function to 
get the column number corressponding to the char and 
use that number to find the next state of input using
transition table
* ************************************************************************/
int get_next_state(int state, char c, int *accept){
	int col;
	int next;
	col = char_class(c);
	next = st_table[state][col];
#ifdef DEBUG
	printf("Input symbol: %c Row: %d Column: %d Next: %d \n",c,state,col,next);
#endif
	assert(next != IS);

#ifdef DEBUG
	if(next == IS){
		printf("Scanner Error: Illegal state:\n");
		printf("Input symbol: %c Row: %d Column: %d\n",c,state,col);
		exit(1);
	}
#endif
	*accept = as_table[next];
	return next;
}

/* ************************************************************************
* Purpose:			This function is used to return the column of the character
on the transistion table
* Author:			Amaury Diaz Diaz
* History/Version:	1.0; October 25, 2015
* Called Function: isalpha()
* Parameters:		Data Type: char; name: c; range: 0 to 255
* Return value:	an integer
* Algorithm:		Check the condition of table column and return the col
that matches with corresponding character
* ***********************************************************************/
int char_class (char c)
{
	int val;
	int num = c - '0';
	if(isalpha(c)){
		val = 0;
	}else if(num == 0){
		val =  1;
	}else if( (num > 0) && (num < 8) ){
		val = 2;
	}else if((num > 7) && ( num < 10)){
		val = 3;
	}else if(c == '.'){
		val = 4;
	}else if(c == '%'){
		val = 5;
	}else {
		val = 6;
	}
	return val;
}

/* *************************************************************************
* Purpose:			The purpose of this function is to validate the specification
return the corrected token; AVID or KW_T token 
and set proper attributes based on lexeme buffer
* Author:			Long Tran
* History/Version:	1.0; October 25, 2015
* Called Function:	iskeyword(), strlen()
* Parameter:		Data Type: pointer to char; name: lexeme; range: 0 to 255
* Return value:	Token
* Algorithm:		Setting token attribute with content of lexeme buffer. 
Set and and return VID/KW_T token whenever the function is 
called
* *************************************************************************/
Token aa_function02 (char *lexeme){
	int numOfChInAVID =0;
	int i;
	Token t;
	char* tempLexeme = 0;
	char type;

	/*iskeyword() return index if it's a keyword [0-7] or 8 if it's not*/
	if(iskeyword(lexeme)!= NO_KEYWORD){
		t.attribute.kwt_idx = iskeyword(lexeme); /*set attribute with lexeme*/					
		t.code = KW_T; /*return token as KW_T if the keyword is found*/
		return t;
	}

	/*check length of lexeme buffer*/
	if(strlen(lexeme)> VID_LEN){
		numOfChInAVID = VID_LEN;
	}else{
		numOfChInAVID = strlen(lexeme);
	}

	/*Allocate memory for tempLexeme*/
	tempLexeme = (char*)malloc(sizeof(char) * numOfChInAVID+1);

	/*Copy the contents of the lexeme into the tempLexeme*/
	for(i=0; i< numOfChInAVID; i++){
		tempLexeme[i] = lexeme[i];
	}

	tempLexeme[numOfChInAVID] = '\0';
	lexeme = tempLexeme;
	/*Decide if the AVID is an INT or a FLOAT*/
	switch (tempLexeme[0]){
	case 'i': case 'o':case 'd': case 'w':
		type='I';
		break;
	default:
		type = 'F';
		break;
	}

	/*Install the AVID in the symbol table*/
	t.attribute.vid_offset = st_install(sym_table, lexeme, type, line);
	
	/*If the vid_offset is -1 The symbol table is full*/
	if(t.attribute.vid_offset < 0){
		printf("\nError: The Symbol Table is full - install failed.\n");
		st_store(sym_table);
		free(tempLexeme);
		exit(EXIT);
	}
	/*Deallocate the memory reserved for the tempLexeme*/
	free(tempLexeme);
	t.code = AVID_T; /*set and return token as AVID*/
	return t;
}

/* *************************************************************************
* Purpose:			The purpose of this function is to return the SVID token 
and set proper attributes based on lexeme buffer
* Author:			Amaury Diaz Diaz
* History/Version:	1.0; October 25, 2015
* Called Function:	strlen()
* Parameter:		Data Type: pointer to char; name: lexeme; range: 0 to 255
* Return value:	Token
* Algorithm:		Setting token attribute with content of lexeme buffer. 
Set and and return SVID token whenever the function is 
called
* *************************************************************************/
Token aa_function03 (char *lexeme){
	int numOfChSVID = 0,i;
	char * tempLexeme = 0;
	Token t;
	t.code = SVID_T;

	/*Allocate memory for tempLexeme*/
	tempLexeme = (char*)malloc(sizeof(char) * (VID_LEN+1));

	/*Check for length of lexeme*/
	if(strlen(lexeme) > VID_LEN){   
		/*if length is greater VID_LEN*/
		numOfChSVID = VID_LEN-1;

		/*Copy the contents of the lexeme into the tempLexeme*/
		for(i = 0; i<numOfChSVID; i++){
			tempLexeme[i] = lexeme[i];
		}
		/*Append % at index 8 of vid_lex buffer*/
		tempLexeme[numOfChSVID] = '%';                            
		/*Append trailing 0*/
		tempLexeme[numOfChSVID+1] = '\0';

		/*Install the AVID in the symbol table*/
		t.attribute.vid_offset=t.attribute.vid_offset=st_install(sym_table,tempLexeme,'S',line);
		/*If the vid_offset is -1 The symbol table is full*/
		if(t.attribute.vid_offset < 0){
			printf("\nError: The Symbol Table is full - install failed.\n");
			st_store(sym_table);
			free(tempLexeme);
			exit(EXIT);
		}
		/*Deallocate the memory reserved for the tempLexeme*/
		free(tempLexeme);
		return t; /*set and return token as SVID*/
	}

	/*If the length is less than VID_LEN*/
	numOfChSVID = strlen(lexeme);
	for(i = 0; i<numOfChSVID; i++){             
		tempLexeme[i] = lexeme[i];
	}
	tempLexeme[numOfChSVID] = '\0';
	t.attribute.vid_offset=t.attribute.vid_offset=st_install(sym_table,tempLexeme,'S',line);

	/*If the vid_offset is -1 The symbol table is full*/
	if(t.attribute.vid_offset < 0){
		printf("\nError: The Symbol Table is full - install failed.\n");
		st_store(sym_table);
		free(tempLexeme);
		exit(EXIT);
	}
	/*Deallocate the memory reserved for the tempLexeme*/
	free(tempLexeme);
	return t;
}

/* *************************************************************************
* Purpose:			The purpose of this function is to return the integer
literal token and set its proper attributes based on 
the lexeme buffer
* Author:			Amaury Diaz Diaz
* History/Version:	1.0; October 25, 2015
* Called Function:	strlen(),atol(),aa_function12()
* Parameter:		Data Type: pointer to char; name: lexeme; range: 0 to SHRT_MAX
* Return value:	Token
* Algorithm:		Finding if the length of the lexeme is bigger than 5.
if it is return error token. Otherwise, convert the lexeme
to long and compare its value with the integer limits(2 bytes).
If it is within the limits return INL_T token and
set its attribute equal to the interger number. Otherwise
return error token.
* *************************************************************************/
Token aa_function05 (char *lexeme){
	Token t;
	long intValue;
	if(strlen(lexeme) > INL_LEN){									/*Check for the length of Integer Literal*/
		t=aa_function12(lexeme);
		return t;
	}
	/*If the length is valid, we convert string to integer*/
	intValue = atol(lexeme);

	if(intValue >= MIN_INT && intValue <= MAX_INT){
		t.code = INL_T;
		t.attribute.int_value = (int)intValue;
		return t;
	}
	t=aa_function12(lexeme);
	return t;
}
/* *************************************************************************
* Purpose:			The purpose of this function is to return the floating
point literal token and set its proper attributes based on 
the lexeme buffer
* Author:			Long Tran
* History/Version:	1.0; October 25, 2015
* Called Function:	atof(),aa_function12()
* Parameter:		Data Type: pointer to char; name: lexeme; range: 0 to SHRT_MAX
* Return value:	Token
* Algorithm:		Convert the lexeme to double and compare its value with 
the float limits(4 bytes).If it is within the limits (including
0.0) return FPL_T token and set its attribute equal to the float 
number. Otherwise return error token.
* *************************************************************************/
Token aa_function08 (char *lexeme){
	Token t; 
	double floatValueInDouble = atof(lexeme);

	/*Check for the range*/
	if((floatValueInDouble >= MIN_FLOAT || floatValueInDouble == 0.0f) && floatValueInDouble <= MAX_FLOAT){
		t.code = FPL_T;
		t.attribute.flt_value = (float)floatValueInDouble;
		return t;				/*Return the token immediately*/
	}

	/*Otherwise, it's error token*/
	t=aa_function12(lexeme);
	return t;

}
/* *************************************************************************
* Purpose:			The purpose of this function is to return the integer
literal token and set its proper attributes based on 
the lexeme buffer
* Author:			Amaury Diaz Diaz
* History/Version:	1.0; October 25, 2015
* Called Function:	strlen(),atool(),aa_function12()
* Parameter:		Data Type: pointer to char; name: lexeme; range: 0 to SHRT_MAX
* Return value:	Token
* Algorithm:		Finding if the length of the lexeme is bigger than 6.
if it is return error token. Otherwise, convert the lexeme
to decimal integer and compare its value with the integer 
limits(2 bytes). If it is within the limits return INL_T 
token and set its attribute equal to the interger number. 
Otherwise return error token.
* *************************************************************************/
Token aa_function10 (char *lexeme){
	Token t;
	long intValueOfOctal;

	if(strlen(lexeme)>(INL_LEN+1)){
		t = aa_function12(lexeme);
		return t;
	}

	intValueOfOctal = atool(lexeme);
	if(intValueOfOctal >= INT_MIN && intValueOfOctal <= INT_MAX){
		t.code = INL_T;
		t.attribute.int_value =(int)intValueOfOctal;
		return t;
	}

	t=aa_function12(lexeme);
	return t;
}


/* *************************************************************************
* Purpose:			The purpose of this function is to return the error
token and set its proper attributes based on 
the lexeme buffer
* Author:			Long Tran
* History/Version:	1.0; October 25, 2015
* Called Function:	strlen()
* Parameter:		Data Type: pointer to char; name: lexeme; range: 0 to SHRT_MAX
* Return value:	Token
* Algorithm:		Return error token and set the attribute; err_lex 
* *************************************************************************/
Token aa_function12 (char *lexeme){
	Token t;
	int rangeOfLexeme,i;

	if(strlen(lexeme) > ERR_LEN){
		rangeOfLexeme = ERR_LEN;
	}else{
		rangeOfLexeme = strlen(lexeme);
	}

	for(i = 0; i<rangeOfLexeme; i++){
		t.attribute.err_lex[i] = lexeme[i];
	}
	t.attribute.err_lex[rangeOfLexeme] ='\0';
	t.code = ERR_T;
	return t;
}

/* *************************************************************************
* Purpose:			The purpose of this function is to convert a lexeme 
into a decimal value representing an octal number.
* Author:			Long Tran
* History/Version:	1.0; October 25, 2015
* Called Function:	atoi()
* Parameter:		Data Type: pointer to char; name: lexeme; range: 0 to SHRT_MAX
* Return value:	Token
* Algorithm:		Convert the lexeme to int using the atoi() function
and the convering the octal representation to decimal
value.
* *************************************************************************/

long atool(char * lexeme){
	int octalValue = atoi(lexeme);								/*Convert a string represent into octal number in in base 10; decimal*/
	int powerOfNumber = 0;
	int remainder = 0;
	long intValueOfOctal = 0;

	while(octalValue != 0){
		remainder = octalValue % 10;							/*Get remainder from octalValue*/
		octalValue = octalValue /10;							/*Update the octalValue*/
		intValueOfOctal += remainder * (int)pow((double)EIGHT, powerOfNumber);
		powerOfNumber ++;
	}
	return intValueOfOctal;
}

/* *************************************************************************
* Purpose:			The purpose of this function is to return an integer
as an indication of the keyword in the kw_table.
* Author:			Amaury Diaz Diaz
* History/Version:	1.0; October 25, 2015
* Called Function:	strcmp()
* Parameter:		Data Type: pointer to char; name: kw_lexeme; range: 0 to SHRT_MAX
* Return value:	Token
* Algorithm:		Validate the input with the keyword table and returning
the corresponding int value that matches the keyword
in the table.
* *************************************************************************/
int iskeyword(char * kw_lexeme){
	int numOfElement = sizeof(kw_table)/sizeof(kw_table[0]);			/*Get number of element in the array*/
	int index;
	for (index = 0; index < numOfElement; index++){                     /*Loop through the array to check for keyword and return the index of key word in kw_table array*/
		if (strcmp(kw_lexeme, kw_table[index]) == 0){
			return index;
		}
	}
	return NO_KEYWORD;
}