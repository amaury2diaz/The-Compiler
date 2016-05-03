
/**********************************************************************************************
Filename:           buffer.c
Compiler:           Visual Studio Premium 2012
Author:             Amaury Diaz Diaz, 040-738-985
Course:             CST8152  Compilers, Lab Section:  012
Assignment:         Assignment 1 - The Buffer
Date:               September 29, 2015
Professor:          Svillen Ranev
Purpose:            Building a Buffer Data structure capable of storing and retrieving
characters.
Function list:      b_create(), b_addc(), b_reset(), b_destroy(), b_isfull(), b_size(), 
b_capacity(), b_setmark(), b_mark(), b_mode(), b_inc_factor(), b_load(),
b_isempty(), b_eob(), b_getc(), b_print(), b_pack(), b_rflag(),
b_retract(), b_retract_to_mark(), b_getc_offset()
*********************************************************************************************/
#include "buffer.h"

/********************************************************************************************
Purpose:            To create and to initialize the Buffer and its members respectively
Author:             Amaury Diaz Diaz
History/Versions:   1.0 - September 22, 2015
Called Functions:   calloc(), malloc(),free()
Parameters:         Data Type: short 
Name: init_capacity 
Values: 0 to BUFFER_MAX_SIZE

Data Type: char 
Name: inc_factor
Values: 1 to 255 (additive and fixed mode),1 to 100(multiplicative mode),0 (fixed mode)

Data Type: char 
Name: o_mode
Values: 'a', 'm' or 'f' 
Return Value(s):    Pointer to a Buffer structure, or NULL      
Algorithm:          Check the validity of the input. Then allocate memory for the Buffer
and the array of characters. If memory cannot be allocated NULL is
returned; otherwise it returns a pointer to the buffer.
*********************************************************************************************/
Buffer * b_create(short init_capacity, char inc_factor, char o_mode){
	/*Used to allocate memory for the buffer structure and the cb_head array*/
	pBuffer buffer;
	/*Check that the capacity parameter is valid*/
	if((init_capacity < 0)|| 
		/*Check that the o_mode being passed satisfies only additive, fixed or multiplicative*/
			(o_mode!='f'&& o_mode!='a'&& o_mode!='m')||
			/*Make sure that in fixed mode there is a capacity bigger than 0*/
			(init_capacity<=0 && o_mode=='f')){
				return NULL;
	}

	buffer = (pBuffer) calloc(1,sizeof(Buffer));
	/*Cannot allocate memory for the buffer return NULL*/
	if(buffer == NULL){
		free(buffer);
		return NULL;
	}

	buffer->cb_head = (char*)malloc(init_capacity * sizeof(char));
	/*Cannot allocate memory for the array of chars return NULL*/
	if(buffer->cb_head==NULL){
		free(buffer);
		free(buffer->cb_head);
		return NULL;
	}
	/*If you have either mode 'f' or increment factor 0 yo are in FIXED MODE*/
	if(o_mode == 'f' || (unsigned char)inc_factor == 0){
		buffer->inc_factor=0;
		buffer->mode=FIX_M;
	}
	/*If you have mode 'a' and an increment factor between 1 and 255 you are in ADDITIVE MODE*/
	else if(o_mode=='a'){
		buffer->inc_factor=inc_factor;
		buffer->mode = ADD_M;
	}
	/*If you have mode 'm' and an increment factor between 1 and 100 you are in MULTIPLICATIVE MODE*/
	else if(o_mode=='m' && ((unsigned char)inc_factor>=1 && (unsigned char)inc_factor<=100)){
		buffer->inc_factor=inc_factor;
		buffer->mode=MULT_M;
	}
	/*In any other case return NULL*/
	else{
		free(buffer);
		free(buffer->cb_head);
		return NULL;
	}
	buffer->capacity = init_capacity;

	return buffer;
}
/********************************************************************************************
Purpose:            To add a character on the buffer. Depending on the buffer mode it will
be resized.
Author:             Amaury Diaz Diaz
History/Versions:   1.0 - September 22, 2015
Called Functions:   b_isfull()
Parameters:         Data Type: pBuffer const 
Name: pBD 
Value: A pointer to a Buffer structure

Data Type: char
Name: symbol
Value: A character to be added to the buffer 
Return Value(s):    Pointer to a Buffer structure, or NULL
Algorithm:          Check the validity of the input. When buffer is full:
FIXED MODE: return Null
ADDITIVE MODE: Add the increment to the current capacity and resize
the buffer if you get a non-negative value from the operation.
MULTIPLICATIVE MODE: Use the formula and resize the buffer.
Then reallocate memory for the resized buffer and add the character
to the buffer.
*********************************************************************************************/
pBuffer b_addc(pBuffer const pBD, char symbol){
	/*Stores the value of the new capacity being calculated (AddItive and Multiplicative Modes only)*/
	short new_capacity=0;
	/*Available space between MAXIMUM_BUFFER_SIZE and capacity (Multiplicative Mode only)*/
	short available_space;
	/*New Increment to add to the current capacity (Multiplicative Mode only)*/
	double new_increment;
	/*Used to reallocate memory for thearray of chars in case the buffer is full*/
	char * temporary_pointer;

	/*Make sure that the buffer and the array containing the chars are valid*/
	if((pBD == NULL)|| 
		/*Check that the amount of elements in the buffer is not bigger than capacity*/
			(pBD->addc_offset>pBD->capacity)||
			/*Check that the number of elements in the array is represented by a positive number*/
			(pBD->addc_offset<0)||
			/*Check that the mode of the buffer is valid*/
			(pBD->mode != ADD_M && pBD->mode !=MULT_M && pBD->mode !=FIX_M)||
			/*Check that the buffer is not full in FIXED MODE*/
			(pBD->mode==FIX_M && pBD->capacity==pBD->addc_offset)||
			/*Check that in multiplicative mode the capacity is not equal to the MAXIMUM_BUFFER_SIZE
			and to the addc_offset(the array reached its limit)*/
			((pBD->mode==MULT_M) && (pBD->capacity==MAXIMUM_BUFFER_SIZE) && (pBD->capacity==pBD->addc_offset)))
	{
		return NULL;
	}
	/*Reset the reallocation flag*/
	pBD->r_flag=SET_DEFAULT;
	/*In case the buffer is full*/
	if(b_isfull(pBD)){
		/*the new_capacity is equal to to the current capacity plus the increment factor in bytes(Additive Mode)*/
		if(pBD->mode==ADD_M)
			new_capacity=pBD->capacity+((unsigned char)pBD->inc_factor*sizeof(char));
			if(new_capacity<0){
				return NULL;
			}
		/*expand the buffer in multiplicative mode using the provided formula*/
		else if(pBD->mode==MULT_M){
			available_space=MAXIMUM_BUFFER_SIZE - pBD->capacity;
			/*new_increment is a double datatype to take care of overflow a*b*/
			new_increment=(double)(available_space * pBD->inc_factor)/100;
			new_capacity=pBD->capacity+(short)new_increment;
			/*If the new capacity is not bigger than current capacity then the new capacity
			is equal to the MAXIMUM_BUFFER_SIZE*/
			(new_capacity<=pBD->capacity)?(new_capacity=MAXIMUM_BUFFER_SIZE):(new_capacity);
		}
		/*Try to reallocate memory for the new buffer*/
		temporary_pointer = (char*) realloc(pBD->cb_head,new_capacity*sizeof(char));
		/*If it cannot be reallocated return NULL*/
		if(temporary_pointer == NULL)
			return NULL;
		/*If the temporary pointer is different from cb_head addres then set the reallocation flag*/
		if(temporary_pointer!=pBD->cb_head){
			pBD->r_flag = SET_R_FLAG;
			pBD->cb_head=temporary_pointer;
		}
		pBD->capacity=new_capacity;
	}
	/*Assign the symbol passed to the addc_offset location in the array*/
	pBD->cb_head[pBD->addc_offset]=symbol;
	/*Increment the offset*/
	++(pBD->addc_offset);
	return pBD;
}

/********************************************************************************************
Purpose:            Reseting the buffer so it appears empty without freeing any memory
Author:             Amaury Diaz Diaz
History/Versions:   1.0 - September 23, 2015
Called Functions:   None
Parameters:         Data Type: Buffer * const 
Name: pBD 
Value: A pointer to a Buffer structure
Return Value(s):    An integer. 1 for success(reset successful) and -1 for failure
Algorithm:          Taking care of any run-time error and then resetting the needed
members in the buffer structure.
*********************************************************************************************/
int b_reset(Buffer *const pBD){
	/*Make sure that the buffer and the array containing the chars are valid*/
	if(pBD == NULL && pBD->cb_head==NULL){
		return R_FAIL_1;
	}
	/*Reset all the offsets to default*/
	pBD->addc_offset=SET_DEFAULT;
	pBD->mark_offset=SET_DEFAULT;
	pBD->getc_offset=SET_DEFAULT;
	pBD->eob=SET_DEFAULT;
	pBD->r_flag=SET_DEFAULT;
	return R_SUCCESS_1;
}
/********************************************************************************************
Purpose:            To free the memory previously allocated for the Buffer and the array of
chars.
Author:             Amaury Diaz Diaz
History/Versions:   1.0 - September 23, 2015
Called Functions:   free()
Parameters:         Data Type: Buffer * const 
Name: pBD 
Value: A pointer to a Buffer structure
Return Value(s):    None
Algorithm:          Free the pointers if needed.
*********************************************************************************************/
void b_destroy(Buffer * const pBD){
	if(pBD!=NULL||pBD->cb_head!=NULL){
		free(pBD->cb_head);
		free(pBD);
	}
}
/********************************************************************************************
Purpose:            To know if the buffer is full
Author:             Amaury Diaz Diaz
History/Versions:   1.0 - September 23, 2015
Called Functions:   None
Parameters:         Data Type: Buffer * const 
Name: pBD 
Value: A pointer to a Buffer structure
Return Value(s):    An integer. 1(full buffer); 0(non-full buffer); -1(run-time error)
*********************************************************************************************/
int b_isfull(Buffer * const pBD){
	return(pBD == NULL)?R_FAIL_1:
		((unsigned short)pBD->capacity==(pBD->addc_offset*sizeof(char)))?B_FULL:B_NON_FULL;
}
/********************************************************************************************
Purpose:            To return the size of the buffer
Author:             Amaury Diaz Diaz
History/Versions:   1.0 - September 23, 2015
Called Functions:   None
Parameters:         Data Type: Buffer * const 
Name: pBD 
Value: A pointer to a Buffer structure
Return Value(s):    A Short. Size of the buffer; -1(run-time error)
*********************************************************************************************/
short b_size(Buffer * const pBD){
	return(pBD == NULL)?R_FAIL_1:pBD->addc_offset;
}
/********************************************************************************************
Purpose:            To return the capacity of the buffer
Author:             Amaury Diaz Diaz
History/Versions:   1.0 - September 23, 2015
Called Functions:   None
Parameters:         Data Type: Buffer * const 
Name: pBD 
Value: A pointer to a Buffer structure
Return Value(s):    A Short. Capacity of the buffer; -1(run-time error)
*********************************************************************************************/
short b_capacity(Buffer*const pBD){
	return(pBD == NULL||pBD->cb_head==NULL)?R_FAIL_1:pBD->capacity;
}
/********************************************************************************************
Purpose:            To return the address of the character where the mark was done.
Author:             Amaury Diaz Diaz
History/Versions:   1.0 - September 24, 2015
Called Functions:   None
Parameters:         Data Type: Buffer * const 
Name: pBD 
Value: A pointer to a Buffer structure

Data Type: short
Name: mark
Value: a short that marks the position of the character in the buffer
array.
Return Value(s):    The address of the character in the array in the 'mark position'
Algorithm:			Check that the parameters being passed are valid. If not return NULL.
If they are assign mark to mark_offset and return the address of 
the character in the offset of the array
*********************************************************************************************/
char * b_setmark(Buffer*const pBD, short mark){
	/*Make sure that the buffer and the array containing the chars are valid*/
	if(pBD == NULL || pBD->cb_head==NULL||
		/*Check that the mark is within valid values*/
			mark<0 || mark>pBD->addc_offset){
				return NULL;
	}
	else{
		pBD->mark_offset=mark;
		/*Return the addres of the character where */
		return &(pBD->cb_head[pBD->mark_offset]);
	}
}
/********************************************************************************************
Purpose:            To return the mark offset
Author:             Amaury Diaz Diaz
History/Versions:   1.0 - September 24, 2015
Called Functions:   None
Parameters:         Data Type: Buffer * const 
Name: pBD 
Value: A pointer to a Buffer structure
Return Value(s):    A Short. Mark Offset; -1(run-time error)
*********************************************************************************************/
short b_mark(Buffer * const pBD){
	return(pBD == NULL||pBD->cb_head==NULL)?R_FAIL_1:pBD->mark_offset;
}
/********************************************************************************************
Purpose:            To return the mode of the buffer
Author:             Amaury Diaz Diaz
History/Versions:   1.0 - September 24, 2015
Called Functions:   None
Parameters:         Data Type: Buffer * const 
Name: pBD 
Value: A pointer to a Buffer structure
Return Value(s):    An integer.Buffer Mode; -1(run-time error)
*********************************************************************************************/
int b_mode(Buffer * const pBD){
	return (pBD == NULL || pBD->cb_head==NULL)?R_FAIL_2:pBD->mode;
}
/********************************************************************************************
Purpose:            To return the increment factor of the buffer
Author:             Amaury Diaz Diaz
History/Versions:   1.0 - September 24, 2015
Called Functions:   None
Parameters:         Data Type: Buffer * const 
Name: pBD 
Value: A pointer to a Buffer structure
Return Value(s):    size_t. Increment factor; 256(run-time error)
*********************************************************************************************/
size_t b_inc_factor(Buffer* const pBD){
	return(pBD == NULL || pBD->cb_head==NULL)?INC_FACTOR_FAIL:(size_t)(unsigned char)pBD->inc_factor;	
}
/********************************************************************************************
Purpose:            To load the characters from a file to the buffer
Author:             Amaury Diaz Diaz
History/Versions:   1.0 - September 25, 2015
Called Functions:   fgetc();feof();b_addc()
Parameters:         Data Type: Buffer * const 
Name: pBD 
Value: A pointer to a Buffer structure

Data Type: FILE * const
Name: fi
Value: Address to a file
Return Value(s):    An integer. Number of characters loaded; -1(run-time error);
-2 (loading error).
Algorithm:			Check that the parameters being passed are valid. If not return NULL.
If they are loop through the file and use b_addc to add each character
to the buffer until EOF is found.
*********************************************************************************************/
int b_load(FILE*const fi, Buffer*const pBD){
	/*Get the character before adding it to the buffer(avoid the addtion of EOF character)*/
	char c_character;
	/*Make sure that the buffer and the array containing the chars are valid, also check that 
	fi is pointing to a valid file*/
	if(fi==NULL || pBD==NULL||pBD->cb_head==NULL){
		return R_FAIL_1;
	}
	/*Get the character from the file*/
	c_character=(char)fgetc(fi);
	/*Loop until the end of file is reached*/
	while (!feof(fi)){
		/*Add the character and if there is a loading problem return -2*/
		if (b_addc(pBD,c_character) == NULL)//adding the termination character
			return LOAD_FAIL;
		c_character=(char)fgetc(fi);
	}
	/*Return the amount of characters in the buffer*/
	return pBD->addc_offset;

}
/********************************************************************************************
Purpose:            To know if the buffer is empty
Author:             Amaury Diaz Diaz
History/Versions:   1.0 - September 25, 2015
Called Functions:   None
Parameters:         Data Type: Buffer * const 
Name: pBD 
Value: A pointer to a Buffer structure
Return Value(s):    An integer. 1(empty buffer); 0(non-empty buffer); -1(run-time error)
*********************************************************************************************/
int b_isempty(Buffer*const pBD){
	return (pBD==NULL||pBD->cb_head==NULL)?R_FAIL_1:(pBD->addc_offset==0)? B_EMPTY : B_NON_EMPTY;	
}
/********************************************************************************************
Purpose:            To know if the buffer has reached its end.
Author:             Amaury Diaz Diaz
History/Versions:   1.0 - September 25, 2015
Called Functions:   None
Parameters:         Data Type: Buffer * const 
Name: pBD 
Value: A pointer to a Buffer structure
Return Value(s):    An integer. End of Buffer(1 reached, 0 non reached); -1(run-time error)
*********************************************************************************************/
int b_eob(Buffer*const pBD){
	return(pBD==NULL || pBD->cb_head==NULL)?R_FAIL_1:pBD->eob;
}
/********************************************************************************************
Purpose:            To return the current character in the array
Author:             Amaury Diaz Diaz
History/Versions:   1.0 - September 26, 2015
Called Functions:   None
Parameters:         Data Type: Buffer * const 
Name: pBD 
Value: A pointer to a Buffer structure
Return Value(s):    A char. The character itself; -2(run-time error); -1(if the end of buffer
has been reached).
Algorithm:			Check that the parameters being passed are valid. If not return -2.
If they are check if the buffer is full; if it is set eob to 1;if not
return the char at getc_offset(before increment getc_offset).
*********************************************************************************************/
char b_getc(Buffer * const pBD){
	/*Make sure that the buffer and the array containing the chars are valid*/
	if(pBD==NULL||pBD->cb_head==NULL){
		return R_FAIL_2;
	}
	/*Check if you have reached the maximum amount of characters in the buffer*/
	if(pBD->getc_offset==pBD->addc_offset){
		pBD->eob=EOB_REACHED;
		return R_FAIL_1;
	}
	else{
		pBD->eob=SET_DEFAULT;
		/*return the character and then increment the getc_offset by 1*/
		return pBD->cb_head[pBD->getc_offset++];
	}
}
/********************************************************************************************
Purpose:            To print the contents of the buffer
Author:             Amaury Diaz Diaz
History/Versions:   1.0 - September 26, 2015
Called Functions:   b_isempty();b_getc();b_eob()
Parameters:         Data Type: Buffer * const 
Name: pBD 
Value: A pointer to a Buffer structure
Return Value(s):    An integer. The amount of characters in the buffer.-1(run-time error)
Algorithm:			Check that the parameters being passed are valid. If not return -1.
If the buffer is empty print a prompt notifying the user. Otherwise
reset the get_c_offset and loop through the buffer printing each 
character until the end of buffer is reached.
*********************************************************************************************/
int b_print(Buffer *const pBD){
	/*Used to store the character returned by b_getc*/
	char c_character;
	/*Make sure that the buffer and the array containing the chars are valid*/
	if(pBD==NULL||pBD->cb_head==NULL){
		return R_FAIL_1;
	}
	/*Check if the buffer is empty and print a message if so*/
	if(b_isempty(pBD)){
		printf("The buffer is empty.");	
	}
	/*Reset the getc_offset*/
	pBD->getc_offset=SET_DEFAULT;
	/*Get the first character in the buffer(which resets eob)*/
	c_character = b_getc(pBD);
	/*Loop until end of buffer is reached*/
	while(!b_eob(pBD)){
		/*Print the character*/
		printf("%c",c_character);
		c_character=b_getc(pBD);
	}
	/*Reser the getc_offset*/
	pBD->getc_offset=SET_DEFAULT;
	printf("\n");
	/*Return the amount of characters in the buffer*/
	return pBD->addc_offset;
}
/********************************************************************************************
Purpose:            To pack the buffer and leave an extra space for the EOF
Author:             Amaury Diaz Diaz
History/Versions:   1.0 - September 27, 2015
Called Functions:   realloc()
Parameters:         Data Type: Buffer * const 
Name: pBD 
Value: A pointer to a Buffer structure
Return Value(s):    An pointer to a Buffer structure. NULL(run-time error).
Algorithm:			Check that the parameters being passed are valid. If not return NULL.
Try to reallocate memory to shrink or in some cases expand the buffer.
If successful set the capacity to the amount of characters in the
buffer plus one and return the pointer to the buffer.
*********************************************************************************************/
Buffer *b_pack(Buffer*const pBD){
	/*Temporary pointer used to reallocate memory for the current amount of characters in the
	buffer plus 1*/
	char * temporary_pointer;
	pBD->r_flag=SET_DEFAULT;
	/*Make sure that the buffer and the array containing the chars are valid*/
	if(pBD == NULL ||pBD->cb_head==NULL ||
		/*Check that the buffer is not completely full if its maximum size has been reached*/
			(pBD->addc_offset==MAXIMUM_BUFFER_SIZE)){
				return NULL;
	}
	/*Allocate memory to pack the buffer(amount of characters in plus 1)*/
	temporary_pointer=(char*)realloc(pBD->cb_head,pBD->capacity=((pBD->addc_offset+INC_ADDCOFFSET)*sizeof(char)));
	if(temporary_pointer==NULL){
		return NULL;
	}
	/*Set the reallocation flag to 1 if the addres of the array of characters has changed*/
	if(temporary_pointer!=pBD->cb_head){
		pBD->r_flag=SET_R_FLAG;
		pBD->cb_head=temporary_pointer;
	}

	return pBD;
}
/********************************************************************************************
Purpose:            To return the value of the rellocation flag.
Author:             Amaury Diaz Diaz
History/Versions:   1.0 - September 27, 2015
Called Functions:   None
Parameters:         Data Type: Buffer * const 
Name: pBD 
Value: A pointer to a Buffer structure
Return Value(s):    A char. Value of the rellocation flag.-1(Run-Time error)
*********************************************************************************************/
char b_rflag(Buffer * const pBD){
	return(pBD==NULL||pBD->cb_head==NULL)?R_FAIL_1:pBD->r_flag;
}
/********************************************************************************************
Purpose:            To retract the getc_offset by one
Author:             Amaury Diaz Diaz
History/Versions:   1.0 - September 27, 2015
Called Functions:   None
Parameters:         Data Type: Buffer * const 
Name: pBD 
Value: A pointer to a Buffer structure
Return Value(s):    A short. get_c_offset minus 1; -1(run-time error)
*********************************************************************************************/
short b_retract(Buffer* const pBD){
	return(pBD==NULL||pBD->cb_head==NULL||pBD->getc_offset<=0)?R_FAIL_1 : (pBD->getc_offset-=1);
}
/********************************************************************************************
Purpose:            To set getc_offset to the mark_offset
Author:             Amaury Diaz Diaz
History/Versions:   1.0 - September 27, 2015
Called Functions:   None
Parameters:         Data Type: Buffer * const 
Name: pBD 
Value: A pointer to a Buffer structure
Return Value(s):    A short. getc_offset; -1(run-time error)
*********************************************************************************************/
short b_retract_to_mark(Buffer* const pBD){
	return(pBD==NULL||pBD->cb_head==NULL)?R_FAIL_1:(pBD->getc_offset=pBD->mark_offset);
}
/********************************************************************************************
Purpose:            To return the current get_c_offset
Author:             Amaury Diaz Diaz
History/Versions:   1.0 - September 27, 2015
Called Functions:   None
Parameters:         Data Type: Buffer * const 
Name: pBD 
Value: A pointer to a Buffer structure
Return Value(s):    A short. getc_offset; -1(run-time error)
*********************************************************************************************/
short b_getc_offset(Buffer * const pBD){
	return(pBD==NULL||pBD->cb_head==NULL)?R_FAIL_1: pBD->getc_offset;
}
