/* 
   Given a string containing only three types of characters: '(', ')' and '*', write a function to check whether this string is valid. We define the validity of a string by these rules:

   Any left parenthesis '(' must have a corresponding right parenthesis ')'.
   Any right parenthesis ')' must have a corresponding left parenthesis '('.
   Left parenthesis '(' must go before the corresponding right parenthesis ')'.
   '*' could be treated as a single right parenthesis ')' or a single left parenthesis '(' or an empty string.
   An empty string is also valid.
   Example 1:

   Input: "()"
   Output: True
   Example 2:

   Input: "(*)"
   Output: True
   Example 3:

   Input: "(*))"
   Output: True
   Note:

   The string size will be in the range [1, 100].
*/
#include <string.h>
#include <stdio.h>

/* 
   (***) all possible cases: (extrac open left brackes)
   [0,1,2,3]
*/

int max(int a, int b)
{
    return a > b ? a : b;
}


int checkValidString(char *s)
{
    int low = 0;		/* smallest possible number of left brackets */
    int high = 0;		/* largest possible number of right brackets */


    char c;
    while(c = *(s++)) {
	low += c == '(' ? 1 : -1;
	high += c != ')' ? 1 : -1;
	if (high < 0)
	    break;		/* ) must go before ( */
	low = max(low, 0);	/* * can be empty also */
    }
    return low == 0;
}

int main(void)
{
    char s[] = ")(";
    int bool = checkValidString(s);
    printf("%d\n", bool);
}
