/* *******************************************************************
 * File Name:		stable.c
 * Compiler:		Visual Studio Premium 2012
 * Author:			Long Tran, 040815454
					Amaury Diaz Diaz, 040738985
 * Course:			CST8152 - Compilers, Lab Section: 012, 011
 * Assignment:		Assignment 3 - Symbol Table
 * Date:			Nov 20, 2015
 * Professor:		Svillen Ranev
 * Purpose:			To implement and incorporate a symbol table components;
					symbol table manager and symbol table database 
					in the compiler project. The symbol table manager 
					responsible for providing utilities for manipulation
					of database, while the symbol table database is used
					to store the VID and its attribute
					
 * Function list:	st_create(), st_install(), st_lookup(),
					st_update_type(), st_update_value(), st_get_type(),
					st_destroy(), st_print(), st_setsize()
					st_incoffset(), st_store(), st_sort(), 
					ascending_compare(), descending_compare()
 * *******************************************************************/
#include "stable.h"
#include "buffer.h"
#include <stdlib.h>
#include <string.h>  /* string functions */

extern STD sym_table;

/*Function prototype*/
static void st_setsize(void);
static void st_incoffset(void);
static int ascending_compare(const void*,const void*);
static int descending_compare(const void*,const void*);

 /* *************************************************************************
 * Purpose:			To create a symbol table and set its attribute to 
					corresponding requirements
 * Author:			Long Tran, Amaury Diaz Diaz
 * History/Version: 1.2 - November 20, 2015
 * Called Function: b_create()
 * Parameter:		Data Type: int; name: st_size; range: 0 to 10
 * Return value:	STD, symbol table descriptor
 * Algorithm:		Validate the existence of the symbol table by checking
					the passing argument. Return immediately, it the received
					argument is invalid. Otherwise, create a symbol table
					descriptor and set corresponding its attribute to proper value
					especially its attribute; pstvr which is a pointer to array 
					of STVR object. Allocate memory for an array of STVR array 
					of object
 * ************************************************************************/
STD st_create(int st_size){
	STD stDescriptor={0};

	/*Check if the symbol table exists*/
	if(st_size <= 0){
		return stDescriptor;
	}

	/*Initialize the local variable of STD*/
	stDescriptor.st_offset = stDescriptor.st_size = 0;

	/*Allocate memory for an array of STVR with st_size; number of element*/
	stDescriptor.pstvr = (STVR*) malloc(sizeof(STVR) * st_size);

	/*Check if it successfully allocate the memory*/
	if(stDescriptor.pstvr == NULL){
		return stDescriptor;
	}

	stDescriptor.plsBD = b_create(1, 1, 'a');
	
	/*Check if the buffer has successfully created*/
	if(stDescriptor.plsBD == NULL){
		/*Delete allocated memory for array of STVR*/
		free (stDescriptor.pstvr);
		stDescriptor.st_size = 0;
		return stDescriptor;
	}

	stDescriptor.st_size = st_size;
	return stDescriptor;
}

 /* ************************************************************************
 * Purpose:			This function is used to install a new entry of VID 
					record in the symbol table or return corresponding
					st_offset if the VID record already exists
					
 * Author:			Amaury Diaz Diaz, Long Tran
 * History/Version:	1.2; November 20, 2015
 * Called Function: st_lookup(), b_addc(), b_flag(), st_incoffset();
 * Parameters:		Data Type: STD; name: sym_table; range: 0 to 100
					Data Type: char*; name: lexeme; range: 1 to 8
					Data Type: char; name: type; range: 'S', 'I', 'F'
 * Return value:	an integer
 * Algorithm:		The purpose of this function is to install a new entry
					of VID record in the symbol table. It must first call
					the st_lookup to validate if the existence of the VID
					record in the symbol table. If it's not stored yet, store
					the lexeme into the symbol table and set other attribute
					to corresponding values. Return st_offset if the VID 
					record exists
 * ***********************************************************************/
int st_install(STD sym_table,char *lexeme,char type,int line){
	int i;
	short bufferBegin=0;
	char r_flag=0;
	/*Check that there is a symbol table and that there is room available in it*/
	if(sym_table.st_size==0 ||strlen(lexeme)<=0){
		return FAIL;
	}
	/*If lookup returns -1 return the offset of the location of the vid in the symbol table*/
	if(st_lookup(sym_table,lexeme)!=-1){
		return sym_table.st_offset;
	}
	if( sym_table.st_offset>=sym_table.st_size){
		return FAIL;
	}
	/*Otherwise install the vid in the symbol table*/
	/*Set its line*/
	sym_table.pstvr[sym_table.st_offset].o_line=line;
	/*The location of the lexem in the corresponding buffer*/
	sym_table.pstvr[sym_table.st_offset].plex=b_setmark(sym_table.plsBD,b_size(sym_table.plsBD));
	/*The default value for the status field*/
	sym_table.pstvr[sym_table.st_offset].status_field=DEFAULT_MASK;
	/*Then reset it*/
	sym_table.pstvr[sym_table.st_offset].status_field = sym_table.pstvr[sym_table.st_offset].status_field & RESET_DATA_TYPE;
	/*Put the lexeme in the buffer*/
	for(i=0;i<(int)strlen(lexeme)+1;i++){
		if(!b_addc(sym_table.plsBD,lexeme[i]))
			return FAIL;
		/*If the buffer has been reallocated set r_flag to 1*/
		if (b_rflag(sym_table.plsBD))
            r_flag = 1;

	}
	/*In case The buffer has been reallocated reset plex*/
	if(r_flag){
		for (i = 0; i <= sym_table.st_offset; i++)
        {
			/*Use bufferBegin as an offset*/
			sym_table.pstvr[i].plex = b_setmark(sym_table.plsBD,bufferBegin);
			bufferBegin+= (short) strlen(sym_table.pstvr[i].plex) + 1;
        }
	}
	/*According to the type of the VID reset the status_field and set int_val to a default value*/
	switch(type){
	case 'I':
		sym_table.pstvr[sym_table.st_offset].status_field =sym_table.pstvr[sym_table.st_offset].status_field | INTEGER;
		sym_table.pstvr[sym_table.st_offset].i_value.int_val = 0;
		break;
	case 'F':
		sym_table.pstvr[sym_table.st_offset].status_field = sym_table.pstvr[sym_table.st_offset].status_field |FLOATING_POINT;
		sym_table.pstvr[sym_table.st_offset].i_value.fpl_val = 0.0f;
		break;
	case 'S':
		sym_table.pstvr[sym_table.st_offset].status_field = sym_table.pstvr[sym_table.st_offset].status_field | STRING;
		sym_table.pstvr[sym_table.st_offset].status_field = sym_table.pstvr[sym_table.st_offset].status_field | CHANGE;
		sym_table.pstvr[sym_table.st_offset].i_value.str_offset = -1;
		break;
	}
	/*Increment the offset in the symbol table before returning it*/
	st_incoffset();
	return sym_table.st_offset;
}
 /* ************************************************************************
 * Purpose:			This function is used to search for a VID record in the
					symbol table and return corresponding offset if the lexeme
					is found or -1 if lexeme doesn't exists
					
 * Author:			Long Tran
 * History/Version:	1.2; November 20, 2015
 * Called Function: none
 * Parameters:		Data Type: STD; name: sym_table; range: 0 to 100
					Data Type: char*; name: lexeme; range: 1 to 8
					
 * Return value:	an integer
 * Algorithm:		The purpose of this function is to search for a VID
					record in the symbol table by performing a backward
					searching from the last entry to the first entry and
					return corresponding value of offset or -1 if lexeme
					doesn't exist
 * ***********************************************************************/
int st_lookup(STD sym_table, char *lexeme){
	int i;
	int lastIndex;
	/*Check if the sym_table exists*/
	if(sym_table.st_size <= 0 || strlen(lexeme)<=0){
		return FAIL;
	}

	/*Find lastIndex of array of STVR object*/
	lastIndex = sym_table.st_offset - 1;
	
	/*Iterate through array of STVR object, check for lemexe*/
	for(i = lastIndex; i>= 0; i--){
		if(strcmp(sym_table.pstvr[i].plex, lexeme) == 0){
			return i;											/*Return immediate, if the lexeme is found in STD*/
		}
	}

	return FAIL;
}
 /* ************************************************************************
 * Purpose:			This function is used to update the data type indicator;
					status_field for different type of VID, including "Float",
					"Integer" and "String".
 * Author:			Amaury Diaz Diaz
 * History/Version:	1.2; November 20, 2015
 * Called Function: none
 * Parameters:		Data Type: STD; name: sym_table; range: 0 to 100
					Data Type: int; name: vid_offset; range: 0 to size of symbol table
					Data Type: char; name: v_type; range: 'A', 'I' or 'S'
 * Return value:	an integer
 * Algorithm:		The purpose of this function is to update the data type
					indicator; status_field using the bitwise operation based
					on the v_type value and setting a corresponding value
					for the status_field
 * ***********************************************************************/
int st_update_type(STD sym_table,int vid_offset, char v_type){
	/*Check that there is a symbol table and that the type has not been changed before*/
	if(sym_table.st_size==0||(sym_table.pstvr[vid_offset].status_field & CHANGE) == CHANGE||vid_offset<0){
		return FAIL;
	}
	/*Reset the data type*/
	sym_table.pstvr[vid_offset].status_field = sym_table.pstvr[vid_offset].status_field & RESET_DATA_TYPE;

	/*Reset the type according to the v_type*/
	switch(v_type){
	case 'I':
		sym_table.pstvr[vid_offset].status_field = sym_table.pstvr[vid_offset].status_field | INTEGER;
		break;
	case 'F':
		sym_table.pstvr[vid_offset].status_field =sym_table.pstvr[vid_offset].status_field | FLOATING_POINT;
		break;
	default:
		return FAIL;
		break;
	}
	/*Update the change flag*/
	sym_table.pstvr[vid_offset].status_field =sym_table.pstvr[vid_offset].status_field | CHANGE;

	return vid_offset;
}
/* ************************************************************************
 * Purpose:			This function is used to update the i_value of the variable
					specified by symbol table offset and return integer as an
					indication
 * Author:			Long Tran
 * History/Version:	1.2; November 20, 2015
 * Called Function: none
 * Parameters:		Data Type: STD; name: sym_table; range: 0 to 100
					Data Type: int; name: vid_offset; range: 0 to size of symbol table
					Data Type: InitialialValue; name: i_value; range: int to float
 * Return value:	an integer
 * Algorithm:		The purpose of this function is to update i_value of 
					lexeme object using the offset value from function argument
					and return an integer as indication; offset or -1
 * ***********************************************************************/
int st_update_value(STD sym_table,int vid_offset,InitialValue i_value){
	/*Check the function arguments; sym_table, vid_offset*/
	/* *********************************************************
	 * First condition, check if the symbol table exist 
	 * Second and third condtion, if vid_offset is valid argument
	 * *********************************************************/
	if(sym_table.st_size <= 0 || vid_offset < 0 || vid_offset > sym_table.st_offset){
		return FAIL;
	}

	/*Update the i_value of STVR object via pstvr and vid_offset*/
	sym_table.pstvr[vid_offset].i_value = i_value;
	return vid_offset;
}
/* ************************************************************************
 * Purpose:			This function is to return the type of the lexeme using
					the indexing value from the passing argument; vid_offset
 * Author:			Amaury Diaz Diaz
 * History/Version:	1.2; November 20, 2015
 * Called Function: none
 * Parameters:		Data Type: STD; name: sym_table; range: 0 to 100
					Data Type: int; name: vid_offset; range: 0 to size of symbol table
 * Return value:	a char, 'S', 'F' or 'I'
 * Algorithm:		The purpose of this function is to return the type of 
					VID record as a char using the offset from the function
					argument
 * ***********************************************************************/
char st_get_type(STD sym_table, int vid_offset){
	/*Check that there is an existing symbol table*/
	if(sym_table.st_size==0||vid_offset<0){
		return FAIL;
	}
	/*Bitwise AND with the INTEGER MASK and return int if INTEGER MASK is the result*/
	if((sym_table.pstvr[vid_offset].status_field & STRING) == STRING){
		return 'S';
	}
	else if((sym_table.pstvr[vid_offset].status_field & INTEGER) == INTEGER){
		return 'I';
	}
	/*Bitwise AND with the FLOAT MASK and return int if FLOAT MASK is the result*/
	else if((sym_table.pstvr[vid_offset].status_field & FLOATING_POINT) == FLOATING_POINT){
		return 'F';
	}
	/*Bitwise AND with the STRING MASK and return int if STRING MASK is the result*/
	
	return FAIL;
}
/* ************************************************************************
 * Purpose:			This function is used to free allocated memory of symbol
					table and reset the size of symbol table to 0
 * Author:			Long Tran
 * History/Version:	1.2; November 20, 2015
 * Called Function: free(), b_destroy();
 * Parameters:		Data Type: STD; name: sym_table; range: 0 to 100
 * Return value:	none
 * Algorithm:		The purpose of this function is to release all allocated
					memory, which is used for symbol table and reset the size
					of the symbol table to zero via its attribute st_size
 * ***********************************************************************/
void st_destroy(STD sym_table){
	if(sym_table.plsBD == NULL || sym_table.pstvr == NULL || sym_table.st_size <= 0){
		return;
	}

	/*Release allocate memory of array of STVR object*/
	free(sym_table.pstvr);
	/*Calling the b_destroy to release buffer memory*/
	b_destroy(sym_table.plsBD);
	st_setsize();
}
/* ************************************************************************
 * Purpose:			This function is used to print the contents of the symbol
					table to the standard output
 * Author:			Amaury Diaz Diaz
 * History/Version:	1.2; November 20, 2015
 * Called Function: None
 * Parameters:		Data Type: STD; name: sym_table; range: 0 to 100
 * Return value:	none
 * Algorithm:		The purpose of this function is print the contents
					of the symbol table to the consoler or file, via the
					the function argument; sym_table
 * ***********************************************************************/
int st_print(STD sym_table){
	int i;
	/*Check that there is an existing symbol table*/
	if(sym_table.st_size==0){
		return FAIL;
	}

	printf("\nSymbol Table\n");
	printf("____________\n");
	printf("\nLine Number Variable Identifier\n");

	/*Print every lexeme in the symbol table along with the line where it is found*/
	for(i=0;i<sym_table.st_offset;i++){
		printf("%2d          %s\n",sym_table.pstvr[i].o_line,sym_table.pstvr[i].plex);
	}
	return sym_table.st_offset;
}
 /* ************************************************************************
 * Purpose:			The purpose of this function is to allow other functions,
					which is not member function to call and reset the symbol
					table attribute; st_size
					to 0
 * Author:			Long Tran
 * History/Version:	1.2; November 20, 2015
 * Called Function: None
 * Parameters:		None
 * Return value:	None
 * Algorithm:		This function is declared as static to allow other 
					non-member function to call and reset the st_size 
 * ***********************************************************************/
static void st_setsize(void){
	/*set the st_size = 0, allow other functions to use*/
	sym_table.st_size = 0;
}
 /* ************************************************************************
 * Purpose:			The purpose of this function is to allow other functions,
					which is not member function to increment symbol table
					attribute; st_offset by 1
					to 0
 * Author:			Amaury Diaz Diaz
 * History/Version:	1.2; November 20, 2015
 * Called Function: None
 * Parameters:		None
 * Return value:	None
 * Algorithm:		This function is declared as static to allow other 
					non-member function to call incrementing the st_offset
					by 1
 * ***********************************************************************/
static void st_incoffset(void){
	++(sym_table.st_offset);
}
 /* ************************************************************************
 * Purpose:			This function is used to store the symbol table and its
					attribute values into a file, $stable.ste
 * Author:			Long Tran
 * History/Version:	1.2; November 20, 2015
 * Called Function: fopen(), fclose(), st_get_type();
 * Parameters:		Data Type: STD; name: sym_table; range: 0 to 100
 * Return value:	an integer
 * Algorithm:		The purpose of this function is to store the symbol
					table and its attribute values into a file, called 
					$stable.ste. Before storing the corrected value of VID
					i_value, the program validates the type of VID by calling
					st_get_type and store the i_value based on the return value
					from the called function
 * ***********************************************************************/
int st_store(STD sym_table){
	FILE * file;
	int recordCounter = 0;
	char type;
	/*Check if the sym_table exists*/
	if(sym_table.st_size <= 0){
		return FAIL;
	}

	file = fopen("$stable.ste", "w");

	if(file == NULL){
		return FAIL;
	}
	/*Print to the file; st_size of symbol table*/
	fprintf(file, "%d", sym_table.st_size);

	for(recordCounter = 0; recordCounter<sym_table.st_offset; recordCounter++){
		fprintf(file, " %X ", sym_table.pstvr[recordCounter].status_field);	/*Print hex value of status_field for each symbol*/
		fprintf(file, "%d ", strlen(sym_table.pstvr[recordCounter].plex));   /*Print the length of lexeme*/
		fprintf(file, "%s ", sym_table.pstvr[recordCounter].plex);
		fprintf(file, "%d ", sym_table.pstvr[recordCounter].o_line);
		
		/*Print corresponding int_value by checking type of i_value*/
		type = st_get_type(sym_table, recordCounter);
		switch (type){
		case 'I': case 'S':
			fprintf(file, "%d", sym_table.pstvr[recordCounter].i_value,type);
			break;

		case 'F':
			fprintf(file, "%.2f", sym_table.pstvr[recordCounter].i_value);
			break;	
		}
	}
	printf("\nSymbol Table stored.\n");
	fclose(file);
	return (recordCounter+1);
}

/* ************************************************************************
 * Purpose:			This function is used to sort the symbol table in different
					order based on argument of function; s_order
 * Author:			Amaury Diaz Diaz, Long Tran
 * History/Version:	1.2; November 20, 2015
 * Called Function: qsort(), ascending_compare(), descending_compare();
 * Parameters:		Data Type: STD; name: sym_table; range: 0 to 100
					Data Type: char; name: s_order; range: 'A' or 'D'
 * Return value:	an integer
 * Algorithm:		The purpose of this function is to perform a sorting
					mechanism of the symbol table/VID records using the
					qsort(). The function performs two different sorting 
					method based on the type of sort from function argument
 * ***********************************************************************/
int st_sort(STD sym_table, char s_order){
	/*Check that there is an existing symbol table and that no other character appart from A or D is passed*/
	if(sym_table.st_size <= 0||(s_order!='A' && s_order!='D')){
		return FAIL;
	}
	/*Sort the symbol table(ascending('A') or descending('D'))*/
	qsort(sym_table.pstvr, sym_table.st_offset, sizeof(STVR), (s_order=='A')?ascending_compare:descending_compare);
	return SUCCESS;
}
 /* ************************************************************************
 * Purpose:			This function is used to passed as argument into qsort()
					and perform the ascending sorting mechanism
 * Author:			Amaury Diaz Diaz, Long Tran
 * History/Version:	1.2; November 20, 2015
 * Called Function: strcmp();
 * Parameters:		Data Type: const*; name: first; range: 0 and up
					Data Type: const*; name: second; range: 0 and up
 * Return value:	an integer
 * Algorithm:		The purpose of this function is to passed into qsort()
					to perform sorting operations. This function accepts two
					argument, which is explicitly casted to STVR object, 
					which points to lexeme. Then calling the strcmp() method 
					to perform the sorting between two set of arrays/lexeme.
 * ***********************************************************************/
static int ascending_compare(const void* first,const void* second){
	/*Compare the strings pointed by plex*/
	return strcmp(((STVR *)first)->plex,((STVR *)second)->plex);
}
/* ************************************************************************
 * Purpose:			This function is used to passed as argument into qsort()
					and perform the ascending sorting mechanism 
 * Author:			Amaury Diaz Diaz, Long Tran
 * History/Version:	1.2; November 20, 2015
 * Called Function: strcmp();
 * Parameters:		Data Type: const*; name: first; range: 0 and up
					Data Type: const*; name: second; range: 0 and up
 * Return value:	an integer
 * Algorithm:		The purpose of this function is to passed into qsort()
					to perform sorting operations. This function accepts two
					argument, which is explicitly casted to STVR object, 
					which points to lexeme. Then calling the strcmp() method 
					to perform the sorting between two set of arrays/lexeme.
 * ***********************************************************************/
static int descending_compare(const void* first,const void* second){
	/*Compare the strings pointed by plex*/
	return strcmp(((STVR *)second)->plex,((STVR *)first)->plex);
}