/* *******************************************************************
 * File Name:		table.h
 * Compiler:		Visual Studio Premium 2012
 * Author:			Long Tran, 040815454
					Amuary Diaz Diaz, 040738985
 * Course:			CST8152 - Compilers, Lab Section: 012, 011
 * Assignment:		Assignment 2 - Scanner
 * Date:			October 27, 2015
 * Professor:		Svillen Ranev
 * Purpose:			To define the transition table used for the VID's
					and interger and float literals. Define the constants
					used in the scanner and keyword and accepting state
					questions.
 * *******************************************************************/
#ifndef  TABLE_H_
#define  TABLE_H_ 

#ifndef BUFFER_H_
#include "buffer.h"
#endif

#ifndef NULL
#include <_null.h> /* NULL pointer constant is defined there */
#endif

/*   Source end-of-file (SEOF) sentinel symbol
 *    '\0' or only one of the folowing constants: 255, 0xFF , EOF
 */

/*  Single-lexeme tokens processed separately one by one
 *  in the token-driven part of the scanner
 *  '=' , ' ' , '(' , ')' , '{' , '}' , == , <> , '>' , '<' ,
 *       space
 *  !<comment , ',' , '"' , ';' , '-' , '+' , '*' , '/', # ,
 *  .AND., .OR. , SEOF, 'wrong symbol',
 */
 
#define ES  12   /* Error state */
#define IS -1    /* Inavalid state */


/*Define Constants*/
#define EIGHT 8					/*Constant value 8 used to convert from octal to interger*/
#define NO_KEYWORD 8			/*Flag used to differentiate an avid from a keyword */
#define MIN_INT -32768			/*Minimum limit of a 2-bytes interger*/
#define MAX_INT  32767			/*Maximum limit of a 2-bytes interger*/
#define MIN_FLOAT 1.17549E-38	/*Minimum limit of a 4-bytes float*/
#define MAX_FLOAT 3.40282E+38	/*Maximum limit of a 4-bytes float*/
#define SEOF	255				/*Inditcation of the source end of file*/

/* State transition table definition */

#define TABLE_COLUMNS 7
/*transition table - type of states defined in separate table */
int  st_table[ ][TABLE_COLUMNS] = {
/* State 0 */  {1, 6, 4, 4, IS, IS, IS},
/* State 1 */  {1, 1, 1, 1, 2, 3, 2},
/* State 2 */  {IS, IS, IS, IS, IS, IS, IS},
/* State 3 */  {IS, IS, IS, IS, IS, IS, IS},
/* State 4 */  {ES, 4, 4, 4, 7, 5, 5},
/* State 5 */  {IS, IS, IS, IS, IS, IS, IS},
/* State 6 */  {ES, ES, 9, ES, 7, ES, 5},
/* State 7 */  {8, 7, 7, 7, 8, 8, 8},
/* State 8 */  {IS, IS, IS, IS, IS, IS, IS},
/* State 9 */  {10, 9, 9, ES, ES, 10, 10},
/* State 10*/  {IS, IS, IS, IS, IS, IS, IS},
/* State 11*/  {IS, IS, IS, IS, IS, IS, IS},
/* State 12*/  {IS, IS, IS, IS, IS, IS, IS},
/* State 13*/  {IS, IS, IS, IS, IS, IS, IS},
};

/* Accepting state table definition */
#define ASWR     2  /* accepting state with retract */
#define ASNR     3  /* accepting state with no retract */
#define NOAS     0  /* not accepting state */

int as_table[ ] = {NOAS, NOAS, ASWR, ASNR, NOAS, ASWR, NOAS, NOAS, ASWR, NOAS, ASWR, ASNR, ASNR};

/* Accepting action function declarations */

/*
FOR EACH OF YOUR ACCEPTING STATES YOU MUST PROVIDE
ONE FUNCTION PROTOTYPE. THEY ALL RETURN Token AND TAKE
ONE ARGUMENT: A string REPRESENTING A TOKEN LEXEME. 

Token aa_funcXX(char *lexeme); 
Replace XX with the number of the accepting state: 02, 03 and so on.
*/

Token aa_function02 (char *lexeme);		/*Returns an AVID_T or KW_T*/
Token aa_function03 (char *lexeme);		/*Returns an SVID_T*/
Token aa_function05 (char *lexeme);		/*Returns an INL_T*/
Token aa_function08 (char *lexeme);		/*Returns a FPL_T*/
Token aa_function10 (char *lexeme);		/*Returns an INL_T(octal number)*/
Token aa_function12 (char *lexeme);		/*Returns ERR_T*/

/* defining a new type: pointer to function (of one char * argument) 
   returning Token
*/  

typedef Token (*PTR_AAF)(char *lexeme);


/* Accepting function (action) callback table (array) definition */
/* If you do not want to use the typedef, the equvalent declaration is:
 * Token (*aa_table[])(char lexeme[]) = {
 */

PTR_AAF aa_table[ ] ={ NULL, NULL, aa_function02, aa_function03, NULL, aa_function05, NULL, NULL, aa_function08, NULL, aa_function10, NULL , aa_function12};

/* Keyword lookup table (.AND. and .OR. are not keywords) */

#define KWT_SIZE  8

char * kw_table []= {
                      "ELSE",
                      "IF",
                      "INPUT",
                      "OUTPUT",
                      "PLATYPUS",
                      "REPEAT",
                      "THEN",
                      "USING"   
                     };

#endif
