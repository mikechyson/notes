#include <stdio.h>
int main()
{
    int testInteger = 5;
    printf("Number = %d", testInteger);

    const int a = 12345;	/* signed constant */
    const int b = 12345U;	/* unsigned constant */
    const int c = 12345u;	/* unsigned constant */
    
    return 0;
}
