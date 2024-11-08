/************************************************
Filename:                       buffer.h
Compiler:						MS Visual Studio Enterprise 2019
Version:                        1.19.3
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
#ifndef BUFFER_H_
#define BUFFER_H_

/*#pragma warning(1:4001) *//*to enforce C89 type comments  - to make //comments an warning */

/*#pragma warning(error:4001)*//* to enforce C89 comments - to make // comments an error */

/* standard header files */
#include <stdio.h>  /* standard input/output */
#include <malloc.h> /* for dynamic memory allocation*/
#include <limits.h> /* implementation-defined data type ranges and limits */

/* constant definitions */
#define RT_FAIL_1 (-1)         /* operation failure return value 1 */
#define RT_FAIL_2 (-2)         /* operation failure return value 2 */
#define LOAD_FAIL (-2)         /* load fail return value */

#define DEFAULT_INIT_CAPACITY 200   /* default initial buffer capacity */
#define DEFAULT_INC_FACTOR 15       /* default increment factor */


/* You should add your own constant definitions here */
#define MAX_MUL_INC 100 /* max 100 value for multiplicative mode */
#define MAX_CAP (SHRT_MAX-1)

/* Add your bit-masks constant definitions here */
#define DEFAULT_FLAGS 0xFFFC /* 1111 1111 1111 1100*/
#define SET_EOB 0x0002 /* 0000 0000 0000 0010 */
#define RESET_EOB 0xFFFD /* 1111 1111 1111 1101 */
#define CHECK_EOB 0x0002 /* 0000 0000 0000 0010 */
#define SET_R_FLAG 0x0001 /* 0000 0000 0000 0001 */
#define RESET_R_FLAG 0xFFFE /* 1111 1111 1111 1110 */
#define CHECK_R_FLAG 0x0001 /* 0000 0000 0000 0001 */

/* user data type declarations */
typedef struct BufferDescriptor {
    char *cb_head;   /* pointer to the beginning of character array (character buffer) */
    short capacity;    /* current dynamic memory size (in bytes) allocated to character buffer */
    short addc_offset;  /* the offset (in chars) to the add-character location */
    short getc_offset;  /* the offset (in chars) to the get-character location */
    short markc_offset; /* the offset (in chars) to the mark location */
    char  inc_factor; /* character array increment factor */
    char  mode;       /* operational mode indicator*/
    unsigned short flags;     /* contains character array reallocation flag and end-of-buffer flag */
} Buffer, *pBuffer;
/*typedef Buffer *pBuffer;*/

/* function declarations */
Buffer* b_allocate(short init_capacity, char inc_factor, char o_mode);
pBuffer b_addc(pBuffer const pBD, char symbol);
int b_clear(Buffer* const pBD);
void b_free(Buffer* const pBD);
int b_isfull(Buffer* const pBD);
short b_limit(Buffer* const pBD);
short b_capacity(Buffer* const pBD);
short b_mark(pBuffer const pBD, short mark);
int b_mode(Buffer* const pBD);
size_t b_incfactor(Buffer* const pBD);
int b_load(FILE* const fi, Buffer* const pBD);
int b_isempty(Buffer* const pBD);
char b_getc(Buffer* const pBD);
int b_eob(Buffer* const pBD);
int b_print(Buffer* const pBD, char nl);
Buffer* b_compact(Buffer* const pBD, char symbol);
char b_rflag(Buffer* const pBD);
short b_retract(Buffer* const pBD);
short b_reset(Buffer* const pBD);
short b_getcoffset(Buffer* const pBD);
int b_rewind(Buffer* const pBD);
char* b_location(Buffer* const pBD);

#endif

