/* 
   Given two strings text1 and text2, return the length of their longest common subsequence.

   A subsequence of a string is a new string generated from the original string with some characters(can be none) deleted without changing the relative order of the remaining characters. (eg, "ace" is a subsequence of "abcde" while "aec" is not). A common subsequence of two strings is a subsequence that is common to both strings.

 

   If there is no common subsequence, return 0.

 

   Example 1:

   Input: text1 = "abcde", text2 = "ace" 
   Output: 3  
   Explanation: The longest common subsequence is "ace" and its length is 3.
   Example 2:

   Input: text1 = "abc", text2 = "abc"
   Output: 3
   Explanation: The longest common subsequence is "abc" and its length is 3.
   Example 3:

   Input: text1 = "abc", text2 = "def"
   Output: 0
   Explanation: There is no such common subsequence, so the result is 0.
 

   Constraints:

   1 <= text1.length <= 1000
   1 <= text2.length <= 1000
   The input strings consist of lowercase English characters only.
*/


#include <string.h>
#include <stdio.h>

int longestCommonSubsequence(char *text1, char *text2)
{
    int len1 = strlen(text1);
    int len2 = strlen(text2);
    printf("len1: %d, len2: %d\n", len1, len2);

    int c[len1+1][len2+1];

    for (int i = 0; i <= len1; i++)
	c[i][0] = 0;

    for (int j = 0; j <= len2; j++)
	c[0][j] = 0;

    for (int i = 0; i < len1; i++)
	for (int j = 0; j < len2; j++) {
	    if (text1[i] == text2[j])
		c[i+1][j+1] = c[i][j] + 1;
	    else if (c[i][j+1] >= c[i+1][j])
		c[i+1][j+1] = c[i][j+1];
	    else
		c[i+1][j+1] = c[i+1][j];
	}
    for (int i = 0; i <= len1; i++) {
	for (int j = 0; j <= len2; j++) 
	  printf("%d ",c[i][j]);
	printf("\n");
    }

    
    return c[len1][len2];
}


int main(void)
{
    /* char *text1 = "abcde"; */
    /* char *text2 = "ace"; */

    char *text1 = "abc";
    char *text2 = "def";

    int comm = longestCommonSubsequence(text1, text2);
    printf("%d\n", comm);
}
