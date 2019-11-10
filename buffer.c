/************************************************
Filename:                       buffer.c
Compiler:						MS Visual Studio Enterprise 2019
Version:                        1.0
Author:		                    Kai Ekdal
Student Number:                 040918802
Course Name/Number:             Compilers CST8152
Lab Section:					312
Assignment # :                  1
Assignment Name:                Programming and Using Buffers
Date:							October 2, 2019
Submission Date:                October 2, 2019
Professor:						Svillen Ranev
List of Source and Header Files:buffer.c, buffer.h
Purpose:						Programming and Using Dynamic Structures 
								(buffers) with C
Function list:					b_allocate();
								b_addc();
								b_clear();
								b_free();
								b_isfull();
								b_limit();
								b_capacity();
								b_mark();
								b_mode();
								b_incfactor();
								b_load();
								b_isempty();
								b_getc();
								b_eob();
								b_print();
								b_compact();
								b_rflag();
								b_retract();
								b_reset();
								b_getcoffset();
								b_rewind();
								b_location();
*************************************************/

#include "buffer.h"

/*****************************************
Function Name:		b_allocate
Purpose:			creates a new buffer in memory
Author:				Kai Ekdal
History/Versions:	1.0
Called functions:	calloc(), malloc(), sizeof(), free()
Parameters:  		short init_capacity - initial capacity
					char inc_factor - increment factor
					char o_mode - operational mode
Return value:		pointer to the buffer
					null, if unsuccessful
Algorithm:			Allocate memory for the buffer structure
					Check if the buffer is valid
					Check the validity of the initial capacity of the buffer
					Allocate memory for the character buffer
					Check the mode
					Assign values depending on mode
					Return a pointer to the buffer structure					
*****************************************/
Buffer* b_allocate(short init_capacity, char inc_factor, char o_mode)
{
	/* allocate memory for one buffer structure */
	Buffer* buffer = calloc(1, sizeof(Buffer));

	/* check to see if the buffer was successfully created */
	if (buffer == NULL) {
		/* returns null if the buffer creation was unsuccessful */
		return NULL;
	}

	/* check if valid input */
	if (init_capacity < 0 || init_capacity > MAX_CAP) {
		return NULL;	
	}

	if (init_capacity != 0) {
		/* pointer returned by malloc is assigned to cb_head */
		buffer->cb_head = malloc(init_capacity);

		/* returns null after freeing buffer if malloc was unsuccessful */
		if (buffer->cb_head == NULL) {
			free(buffer);
			return NULL;
		}

		/* set the mode and inc_factor to 0 if operational mode is f */
		if (o_mode == 'f' || inc_factor == 0 && init_capacity != 0) {
			buffer->mode = 0;
			inc_factor = 0;
		}

		/* checks if o_mode is a and the inc_factor is in range of 1-255, inclusive */
		else if (o_mode == 'a' && (unsigned char)inc_factor <= UCHAR_MAX && (unsigned char)inc_factor >= 1) {
			/* sets mode to 1 and buffer's inc_factor to inc_factor */
			buffer->mode = 1;
			buffer->inc_factor = inc_factor;
		}

		/* checks if o_mode is m and inc_factor is in range of 1-100, inclusive */
		else if (o_mode == 'm' && inc_factor >= 1 && inc_factor <= MAX_MUL_INC) {
			/* sets mode to -1 and inc_factor is assigned to buffer inc_factor */
			buffer->mode = -1;
			buffer->inc_factor = inc_factor;
		}
		else {
			free(buffer->cb_head);
			free(buffer);
			return NULL;
		}

		buffer->capacity = init_capacity;
	} 
	/* creates a buffer with default size 200 when init_capacity is 0 */
	else {
		/* pointer returned by malloc is assigned to cb_head */
		buffer->cb_head = malloc(DEFAULT_INIT_CAPACITY);

		/* returns null after freeing buffer if malloc was unsuccessful */
		if (buffer->cb_head == NULL) {
			free(buffer);
			return NULL;
		}

		/* sets capacity to default */
		buffer->capacity = DEFAULT_INIT_CAPACITY;

		/* set the inc_factor based on mode and set the mode */
		if (o_mode == 'a') {
			/* sets inc_factor to default 15 if mode is a */
			buffer->mode = 1;
			buffer->inc_factor = DEFAULT_INC_FACTOR;
		}
		else if (o_mode == 'm') {
			/* sets inc_factor to default 15 if mode is m */
			buffer->mode = -1;
			buffer->inc_factor = DEFAULT_INC_FACTOR;
		}
		/* sets inc_factor to 0 if mode is f */
		else if (o_mode == 'f') {
			inc_factor = 0;
		}
	}

	/* sets the flags field to its default value FFFC hexadecimal. */
	buffer->flags = DEFAULT_FLAGS;

	/* returns a pointer to the Buffer structure, on success */
	return buffer;
}

/*****************************************
Function Name:		b_addc
Purpose:			adds a symbol to the character array of the buffer
Author:				Kai Ekdal
History/Versions:	1.0
Called functions:	b_isfull(), realloc()
Parameters:			Buffer* const pBD - character buffer
					char symbol
Return value:		Buffer* const pBD - character buffer
					null, if unsuccessful
Algorithm:			Check if the buffer exists
					Reset the r_flag, using a bitwise & operation
					Add characters until the buffer is full
					Dynamically allocate space, depending on mode
					Add the remaining characters
*****************************************/
pBuffer b_addc(pBuffer const pBD, char symbol)
{
	short new_capacity = 0;
	short avail_space = 0;
	short new_incr = 0;
	/* temporary storage for the list */
	char* temp_buffer = 0;

	/* returns null if a runtime error is possible */
	if (!pBD) {
		return NULL;
	}

	/* reset the flags field f_flag bit to 0 */
	pBD->flags = pBD->flags & RESET_R_FLAG;

	/* adds the symbol if the buffer is not full */
	if (!b_isfull(pBD)) {
		pBD->cb_head[pBD->addc_offset] = symbol;
		/* increments addc_offset by 1 */
		pBD->addc_offset++;
		return pBD;
	}

	/* returns null if mode is fixed and buffer is full */
	if (pBD->mode == 0) {
		return NULL;
	}

	/* returns null if there is an overflow */
	if (pBD->addc_offset >= MAX_CAP) {
		return NULL;
	}

	/* increases current capacity when  mode is additive and buffer is full */
	if (pBD->mode == 1) {
		/* stores a new capacity by adding inc_factor and current capacity */
		new_capacity = pBD->capacity + (unsigned char)pBD->inc_factor;
		/* assigns max to new capacity if it is positive and exceeds max */
		if (new_capacity > MAX_CAP) {
			new_capacity = MAX_CAP;
		}
		/* returns null if the new capacity is a negative value */
		else if (new_capacity < 0) {
			return NULL;
		}
		/* proceeds if the result is positive and within range of the allowed max */
	}

	/* increases current capacity when mode is -1 and buffer is full, i.e. (pBD->mode == -1) */
	else {
		/* returns null if the current capacity is reached */
		if (pBD->capacity == MAX_CAP) {
			return NULL;
		}
		/* available space */
		avail_space = MAX_CAP - pBD->capacity;
		/* new increment */
		new_incr = (short)(avail_space * (((double)pBD->inc_factor) / 100));
		/* new capacity */
		new_capacity = pBD->capacity + new_incr;

		/* checks if new_incr was incremented successfully */
		if (new_incr == 0) {
			new_capacity = MAX_CAP;
		}

		/* checks if current capacity is less than max */
		if (pBD->capacity < MAX_CAP && new_capacity == pBD->capacity) {
			/* assigns the max value - 1 to new capacity */
			new_capacity = MAX_CAP;
		}
	}

	/* expand the character buffer with the new capacity */
	/* temp_buffer = (char *)realloc(pBD->cb_head, sizeof(char) * (unsigned short)new_capacity);*/
	temp_buffer = (char *)realloc(pBD->cb_head, (size_t)new_capacity);

	/* returns null if the reallocation fails */
	if (temp_buffer == NULL) {
		return NULL;
	}
	
	/* set r_flag bit to 1 if buffer location has changed */
	if (pBD->cb_head != temp_buffer) {
		pBD->flags |= SET_R_FLAG;
		/* set the head to the reallocated address */
		pBD->cb_head = temp_buffer;
	}

	/* assigns the new capacity into capacity */
	pBD->capacity = new_capacity;
	/* adds symbol to the end of the character buffer */
	/*my line of code below resulted in a buffer overrun warning on VS, yet my outputs for all test cases were 100% accurate */
	pBD->cb_head[pBD->addc_offset] = symbol;
	/* the buffer overrun warning disappeared after using the code below. */
	/**(pBD->cb_head + pBD->addc_offset) = symbol;*/

	/* increment addc_offset by 1 */
	pBD->addc_offset++;

	/* returns a pointer to Buffer */
	return pBD;
}

/*****************************************
Function Name:		b_clear
Purpose:			re-initializes data members and retains memory space currently
					allocated to the buffer
Author:				Kai Ekdal
History/Versions:   1.0
Called functions:	none
Parameters:			Buffer* const pBD - character buffer
Return value:		0, if successful
					-1, if unsuccessful
Algorithm:			Check if the buffer exists
					Reset the buffer offsets to 0
					Reset the eob and r flag using bitwise & 
					Return 0
*****************************************/
int b_clear(Buffer* const pBD)
{
	/* returns -1 if a runtime error is possible */
	if (!pBD) {
		return RT_FAIL_1;
	}

	/* reset the buffer offsets */
	pBD->addc_offset = 0;
	pBD->getc_offset = 0;
	pBD->markc_offset = 0;

	/* reset the flags */
	pBD->flags = pBD->flags & RESET_EOB;
	pBD->flags = pBD->flags & RESET_R_FLAG;

	return 0;
}

/*****************************************
Function Name:		b_free
Purpose:			deallocates memory of the buffer and buffer structure
Author:				Kai Ekdal
History/Versions:	1.0
Called functions:	free()
Parameters:			Buffer* const pBD - character buffer
Return value:		none
Algorithm:			Check if the buffer exists
					De-allocate the memory of the character buffer
					De-allocate the memory of the buffer descriptor
*****************************************/
void b_free(Buffer* const pBD)
{
	/* returns if a runtime error is possible */
	if (!pBD) {
		return;
	}

	/* frees the character buffer */
	free(pBD->cb_head);
	/* frees the buffer descriptor*/
	free(pBD);

}

/*****************************************
Function Name:		b_isfull
Purpose:			reports if character buffer is full
Author:				Kai Ekdal
History/Versions:	1.0
Called functions:	none
Parameters:			Buffer* const pBD - character buffer
Return value:		0 for success
					1 if the character buffer is full
					-1 if not successful
Algorithm:			Check if the buffer exists
					Return 1 if the character buffer is full
					Return 0 if the character buffer is not full
*****************************************/
int b_isfull(Buffer* const pBD)
{
	/* returns -1 if a runtime error is possible */
	if (!pBD) {
		return RT_FAIL_1;
	}

	if (pBD->addc_offset == pBD->capacity) {
		return 1;
	}

	return 0;
}

/*****************************************
Function Name:		b_limit
Purpose:			return the current limit of the character buffer
Author:				Kai Ekdal
History/Versions:	1.0
Called functions:	none
Parameters:			Buffer* const pBD - character buffer
Return value:		current limit of the character buffer
					-1 if not successful 
Algorithm:			Check if the buffer exists
					Return the current limit of the character buffer
*****************************************/
short b_limit(Buffer* const pBD)
{
	/* returns -1 if a runtime error is possible */
	if (!pBD) {
		return RT_FAIL_1;
	}

	return pBD->addc_offset;
}

/*****************************************
Function Name:		b_capacity
Purpose:			returns the current capacity of the character buffer
Author:				Kai Ekdal
History/Versions:	1.0
Called functions:	none
Parameters:			Buffer* const pBD - character buffer
Return value:		current capacity of buffer
					-1 if not successful
Algorithm:			Check if buffer exists
					Return the capacity of the character buffer
*****************************************/
short b_capacity(Buffer* const pBD)
{
	/* returns -1 if a runtime error is possible */
	if (!pBD) {
		return RT_FAIL_1;
	}

	/* returns the current capacity of the character buffer */
	return pBD->capacity;
}

/*****************************************
Function Name:		b_mark
Purpose:			sets markc_offset to mark
Author:				Kai Ekdal
History/Versions:	1.0
Called functions:	none
Parameters:			Buffer* const pBD - character buffer
					short mark
Return value:		currently set markc_offset
					-1, if unsuccessful
Algorithm:			Check if the buffer exists
					Set markc_offset to the value of mark
					Return the markc_offset 
					Return -1 if a runtime error is possible
*****************************************/
short b_mark(pBuffer const pBD, short mark)
{
	/* returns -1 if a runtime error is possible */
	if (!pBD) {
		return RT_FAIL_1;
	}

	/* sets markc_offset to mark if mark is valid */
	if (mark >= 0 && mark <= pBD->addc_offset) {
		pBD->markc_offset = mark;
		return pBD->markc_offset;
	}

	/* returns -1 if a runtime error is possible */
	return RT_FAIL_1;
}

/*****************************************
Function Name:		b_mode
Purpose:			returns the value of mode to the calling function
Author:				Kai Ekdal
History/Versions:   1.0
Called functions:	none
Parameters:			Buffer* const pBD - character buffer
Return value:		value of mode
					-2, if unsuccessful
Algorithm:			Check if the buffer exists
					Return the value of mode
*****************************************/
int b_mode(Buffer* const pBD)
{
	/* returns -2 if a runtime error is possible */
	if (!pBD) {
		return RT_FAIL_2;
	}

	/* returns the value of mode */
	return pBD->mode;
}

/*****************************************
Function Name:		b_incfactor
Purpose:			returns the non-negative value of inc_factor
Author:				Kai Ekdal
History/Versions:	1.0
Called functions:	none
Parameters:			Buffer* const pBD - character buffer
Return value:		non-negative value of inc_factor
					0x100, if unsuccessful  
Algorithm:			Return 0x100 if the buffer does not exist
					Return the non-negative value of increment factor
*****************************************/
size_t b_incfactor(Buffer* const pBD)
{
	if (!pBD) {
		return 0x100;
	}

	/* cast to unsigned char to return non-negative values */
	return (unsigned char)pBD->inc_factor;
}

/*****************************************
Function Name:		b_load
Purpose:			reads a file into a buffer
Author:				Kai Ekdal
History/Versions:	1.0
Called functions:	fgetc(), feof(), b_addc(), ungetc()
Parameters:			fi - open input file
					Buffer* const pBD - character buffer
Return value:		number of characters added to the buffer
					-1 if not successful
Algorithm:			Check if the buffer and the file exist
					Read each character into a buffer
					Return a character to the file stream if it cannot be added
					Increment a counter by 1 for each character added
					Terminate loop upon reaching the end of file
					Return the number of characters added
*****************************************/
int b_load(FILE* const fi, Buffer* const pBD)
{
	/* set a counter variable to 0 */
	int counter = 0;

	/* variable to store a character */
	char to_add = 0;

	/* returns -1 if a runtime error is possible */
	if (!pBD || !fi) {
		return RT_FAIL_1;
	}

	/* reads each character into a buffer until the end of the file */
	while (1) {
		/* store the variable to add and cast it to char */
		to_add = (char)fgetc(fi);

		/* exits loop when it reaches the end of file */
		if (feof(fi)) {
			break;
		}

		/* returns character to the file stream if it cannot be added */
		if (!b_addc(pBD, to_add)) {
			/* returns the character to the file stream*/
			ungetc(to_add, fi);
			/* returns -2 upon failure */
			return LOAD_FAIL;
		}
		/* increments the counter */
		counter++;
	}

	/* returns the number of characters added to the buffer */
	return counter;
}

/*****************************************
Function Name:		b_isempty
Purpose:			checks if buffer is empty
Author:				Kai Ekdal
History/Versions:	1.0
Called functions:	none
Parameters:			Buffer* const pBD - character buffer
Return value:		1 if addc_offset is 0
					0 if addc_offset not 0
					-1, if unsuccessful 
Algorithm:			Check if the buffer exists
					Return 1 if addc_offset is 0
					Return 0 if addc_offset is not 0 and nothing was added
*****************************************/
int b_isempty(Buffer* const pBD)
{
	/* returns -1 if a runtime error is possible */
	if (!pBD) {
		return RT_FAIL_1;
	}

	/* returns 1 if addc_offset is 0 */
	if (pBD->addc_offset == 0) {
		return 1;
	}

	/* returns 0 if addc_offset is not 0 */
	return 0;
}

/*****************************************
Function Name:		b_getc
Purpose:			read the buffer
Author:				Kai Ekdal
History/Versions:	1.0
Called functions:	none
Parameters:			Buffer* const pBD - character buffer
Return value:		character located at getc_offset
					-2, if unsuccessful or invalid argument
					0 if getc_offset and addc_offset are equal
Algorithm:			Check if the buffer exists
					Check if getc_offset and addc_offset are equal in value
					If equal, set the flags field eob bit to 1 using bitwise OR
					Return 0 after setting the flag
					If not equal, set the eob to 0 and return 0
					Increment getc_offset by 1
					Return the character at getc_offset
*****************************************/
char b_getc(Buffer* const pBD)
{
	/* variable to store the current character at getc_offset */
	char curr_temp = 0;

	/* returns -2 if a runtime error is possible */
	if (!pBD) {
		return RT_FAIL_2;
	}

	/* checks if getc_offset and addc_offset are equal */
	if (pBD->getc_offset == pBD->addc_offset) {
		/* sets the flags field eob bit to 1 */
		pBD->flags |= SET_EOB;
		/* returns 0 after setting flags field eob bit to 1 */
		return 0;
	}

	/* sets eob to 0 */
	pBD->flags &= RESET_EOB;

	/* stores the current character of getc_offset */
	curr_temp = pBD->cb_head[pBD->getc_offset];

	/* increments getc_offset by 1 */
	pBD->getc_offset++;

	/* returns the character located at getc_offset */
	return curr_temp;
}

/*****************************************
Function Name:		b_eob
Purpose:			returns the value of the flags field as per eob bit
Author:				Kai Ekdal
History/Versions:	1.0
Called functions:	none
Parameters:			Buffer* const pBD - character buffer
Return value:		value of flags in the eob bit
					-1, if unsuccessful
Algorithm:			Check if the buffer exists
					Return the value of the flags field of the eob bit
					using a bitwise & operator
*****************************************/
int b_eob(Buffer* const pBD)
{
	/* returns -1 if a runtime error is possible */
	if (!pBD) {
		return RT_FAIL_1;
	}

	/* return the value of the flags field based on the eob bit */
	return pBD->flags & CHECK_EOB;
}

/*****************************************
Function Name:		b_print
Purpose:			prints characters in the buffer
Author:				Kai Ekdal
History/Versions:	1.0
Called functions:	b_getc(), b_eob()
Parameters:			Buffer* const pBD - character buffer
					char nl - new line
Return value:		contents of the character buffer
					new line character, if nl is not 0
					number of characters printed
					-1, if unsuccessful
Algorithm:			Check if the buffer exists
					Perform a loop to print each character in the buffer
					Increment a counter by 1 upon each iteration of the loop
					Terminate loop upon reaching end of file
					Print a newline if nl is not zero
					Return the total number of characters displayed
*****************************************/
int b_print(Buffer* const pBD, char nl)
{
	/* set a counter variable to 0 */
	int counter = 0;

	/* variable to hold the current character */
	char curr = 0;

	/* returns -1 if unsuccessful */
	if (!pBD) {
		return RT_FAIL_1;
	}

	/* prints the contents of the character buffer */
	while (1) {
		/* stores the character in the buffer */
		curr = b_getc(pBD);
		/* exits loop if buffer is full */
		if (b_eob(pBD) > 0) {
			break;
		}
		/* print the current character */
		printf("%c", curr);
		/* increment the counter */
		counter++;
	}

	/* prints new line character if nl is not 0 */
	if (nl != 0) {
		printf("\n");
	}

	/* returns the number of characters printed */
	return counter;
}

/*****************************************
Function Name:		b_compact
Purpose:			shrinks or expands buffer to a new capacity
Author:				Kai Ekdal
History/Versions:	1.0
Called functions:	realloc()
Parameters:			Buffer* const pBD - character buffer
					char symbol 
Return value:		pointer to the buffer
					null, if operation unsuccessful
Algorithm:			Checks the validity of the buffer, head, and addc_offset
					Reset the r_flag using a bitwise & operation
					Allocate and set space for the new capacity
					Set the realloc flag if the location of the buffer changed
					Add the characters
					Return a pointer to the buffer
*****************************************/
Buffer* b_compact(Buffer* const pBD, char symbol)
{
	/* variable to store the new capacity initialized to 0 */
	short new_capacity = 0;

	/* returns null if operation unsuccessful  */
	if (!pBD || !pBD->cb_head || pBD->addc_offset > MAX_CAP) {
		return NULL;
	}

	/* reset the flags field f_flag bit to 0 */
	pBD->flags = pBD->flags & RESET_R_FLAG;

	/* calculate the new capacity */
	new_capacity = pBD->addc_offset + 1;

	/* returns null if there is underflow */
	if (new_capacity <= 0) {
		return NULL;
	}

	/* create a temporary buffer to hold the head after reallocating */
	char* temp_buffer = realloc(pBD->cb_head, new_capacity);

	/* return null if realloc was unsuccessful */
	if (temp_buffer == NULL) {
		return NULL;
	}

	/* set r_flag bit if buffer location has changed */
	if (pBD->cb_head != temp_buffer) {
		pBD->flags |= SET_R_FLAG;
		/* set the head to the reallocated address */
		pBD->cb_head = temp_buffer;
	}

	/* update the necessary members of the buffer descriptor */
	pBD->capacity = new_capacity;

	/* adds symbol to the end of the character buffer */
	pBD->cb_head[pBD->addc_offset++] = symbol;

	/* returns a pointer to Buffer */
	return pBD;
}

/*****************************************
Function Name:		b_rflag
Purpose:			returns the value of the flags determined by the r_flag bit
Author:				Kai Ekdal
History/Versions:	1.0
Called functions:	none
Parameters:			Buffer* const pBD - character buffer
Return value:		flags field value
					-1, if unsuccessful
Algorithm:			Checks if the buffer exists
					Perform a bitwise & operation with the buffer's flag
					on CHECK_R_FLAG and return the result
*****************************************/
char b_rflag(Buffer* const pBD)
{
	/* returns -1 if a runtime error is possible */
	if (!pBD) {
		return RT_FAIL_1;
	}

	/* returns the value of the flags field determined by the r_flag bit */
	return pBD->flags & CHECK_R_FLAG;
}

/*****************************************
Function Name:		b_retract
Purpose:			decrements getc_offset by 1
Author:				Kai Ekdal
History/Versions:	1.0
Called functions:	none
Parameters:			Buffer* const pBD - character buffer
Return value:		getc_offset
					-1, if unsuccessful
Algorithm:			Checks that the buffer exists
					Subtracts getc_offset by 1
					Returns the value of getc_offset
*****************************************/
short b_retract(Buffer* const pBD)
{
	/* returns -1 if a runtime error is possible */
	if (!pBD) {
		return RT_FAIL_1;
	}

	/* decrement getc_offset by 1 */
	if (pBD->getc_offset > 0) {
		pBD->getc_offset--;
	}

	/* returns getc_offset if successful */
	return pBD->getc_offset;
}

/*****************************************
Function Name:		b_reset
Purpose:			sets getc_offset to the value of the current markc_offset
Author:				Kai Ekdal
History/Versions:	1.0
Called functions:	none
Parameters:			Buffer* const pBD - character buffer
Return value:		getc_offset
					-1, if unsuccessful
Algorithm:			Checks if buffer exists
					Checks validity of mark
					Sets getc_offset to the value of the current mark
					Returns the value of getc_offset
*****************************************/
short b_reset(Buffer* const pBD)
{
	/* returns -1 if a runtime error is possible */
	if (!pBD || pBD->markc_offset < 0 || pBD->markc_offset > pBD->capacity) {
		return RT_FAIL_1;
	}

	/* The function sets getc_offset to the value of the current markc_offset */
	pBD->getc_offset = pBD->markc_offset;

	/* returns getc_offset if successful */
	return pBD->getc_offset;
}

/*****************************************
Function Name:		b_getcoffset
Purpose:			returns getc_offset to the calling function
Author:				Kai Ekdal
History/Versions:   1.0
Called functions:	none
Parameters:			Buffer* const pBD - character buffer
Return value:		getc_offset
					-1 if unsuccessful
Algorithm:			n/a
*****************************************/
short b_getcoffset(Buffer* const pBD)
{
	/* returns -1 if a runtime error is possible */
	if (!pBD) {
		return RT_FAIL_1;
	}

	return pBD->getc_offset;
}

/*****************************************
Function Name:		b_rewind
Purpose:			sets the getc_offset and markc_offset to 0
Author:				Kai Ekdal
History/Versions:	1.0
Called functions:	none
Parameters:			Buffer* const pBD - character buffer
Return value:		0 for success
					-1, if unsuccessful
Algorithm:			Checks if buffer exists
					Sets the distance from head to the location of a character
					and the distance from head to a mark to zero
*****************************************/
int b_rewind(Buffer* const pBD)
{
	/* returns -1 if a runtime error is possible */
	if (!pBD) {
		return RT_FAIL_1;
	}

	/* set getc_offset and markc_offset to 0 */
	pBD->getc_offset = 0;
	pBD->markc_offset = 0;

	return 0;
}

/*****************************************
Function Name:		b_location
Purpose:			Returns a pointer to a location of the character buffer
Author:				Kai Ekdal
History/Versions:	1.0
Called functions:	none
Parameters:			Buffer* const pBD - character buffer
Return value:		pointer to a location of the character buffer
					null, if unsuccessful
Algorithm:			Check if buffer exists
					Check if the mark is valid
					Return a pointer to a specific location of the character buffer
*****************************************/
char* b_location(Buffer* const pBD)
{
	/* returns null if run-time error is possible */
	if (!pBD) {
		return NULL;
	}

	if (pBD->markc_offset < 0 || pBD->markc_offset >= pBD->addc_offset) {
		return NULL;
	}

	/* returns pointer to the character buffer location */
	return pBD->cb_head + pBD->markc_offset;
}
