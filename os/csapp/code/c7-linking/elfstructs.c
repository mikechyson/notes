typedef struct {
    int name;			/* string table offset */
    char type:4,			/* function or data (4 bits) */
	bingding:4;			/* local or global (4 bits) */
    char reserved;		/* unused */
    short section;		/* section header index */
    long value;			/* section offset or absolute address */
    long size			/* object size in bytes */
} Elf64_Symbol;


typedef struct {
  long offset;			/* offset of the reference to relocate */
  long type:32,			/* relocation type */
    symbol:32;			/* symbol table index */
  long addend;			/* constant part of relocation expression */
} Elf64_Rela;
