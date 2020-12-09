#include <dlfcn.h>		/* dynamic linking function */


/* 
The dlopen function loads and links the shared library filename. 
The external symbols in filename are resolved using libraries previously opened with the RTLD_ GLOBAL flag. 
If the current executable was compiled with the -rdynamic flag, then its global symbols are also available for symbol resolution.
The flag argument must include either RTLD_NOW, which tells the linker to resolve references to external symbols immediately, 
or the RTLD_LAZY flag, which instructs the linker to defer symbol resolution until code from the library is executed. 
 */
void *dlopen(const char *filename, int flag); /* returns : pointer to handle if OK, NULL on error */



/* 
The dlsym function takes a handle to a previously opened shared library and 
a symbol name and returns the address of the symbol, if it exists, or NULL otherwise.
 */
void *dlsym(void *handle, char *symbol); /* returns: pointer to symbol if OK, NULL on error */



/* 
The dlclose function unloads the shared library if no other shared libraries are still using it.
 */
int dlclose(void *handle);	/* returns: 0 if OK, -1 on error */



/* 
The dlerror function returns a string describing the most recent error 
that occurred as a result of calling dlopen, dlsym, or dlclose, or NULL if no error occurred.
 */
const char *dlerror(void);	/* returns: error message if previous call to dlopen, dysym,
				   or dlclose failed; NULL if previous call was OK */
