/* 
   Return the root node of a binary search tree that matches the given preorder traversal.

   (Recall that a binary search tree is a binary tree where for every node, 
   any descendant of node.left has a value < node.val, and any descendant of node.right has a value > node.val.  
   Also recall that a preorder traversal displays the value of the node first, 
   then traverses node.left, then traverses node.right.)

   It's guaranteed that for the given test cases there is always possible to find a binary search tree 
   with the given requirements.

   Example 1:

   Input: [8,5,1,7,10,12]
   Output: [8,5,10,1,7,null,12]

 

   Constraints:

   1 <= preorder.length <= 100
   1 <= preorder[i] <= 10^8
   The values of preorder are distinct.
*/

/**
 * Definition for a binary tree node.
 * struct TreeNode {
 *     int val;
 *     struct TreeNode *left;
 *     struct TreeNode *right;
 * };
 */


#include <stdio.h>
#include <stdlib.h>


struct TreeNode {
    int val;
    struct TreeNode *left;
    struct TreeNode *right;
};


struct TreeNode *extractRoot(struct TreeNode *root, int *preorder, int start, int end)
{
    int lstart, lend;
    int rootval = preorder[start];
    root->val = rootval;

    printf("start: %d, end: %d\n", start, end);
    
    if (start == end) {
	root->left = NULL;
	root->right = NULL;
    } else {
	if (preorder[start +1] > preorder[start]) {
	    lstart = start;
	    lend = start;
	    root->left = NULL;
	    printf("lstart: %d, lend: %d\n", lstart, lend);
		    
	} else {
	    lstart = start + 1;
	    for (lend = start + 1; lend <= end && preorder[lend] < rootval; lend++)
		;
	    lend--;
	    struct TreeNode *lroot = (struct TreeNode *) malloc(sizeof(struct TreeNode));
	    printf("lstart: %d, lend: %d\n", lstart, lend);
	    root->left = extractRoot(lroot, preorder, lstart, lend);
	}

	if (lend == end) 
	    root->right = NULL;
	else {
	    struct TreeNode *rroot = (struct TreeNode *) malloc(sizeof(struct TreeNode));
	    root->right = extractRoot(rroot, preorder, lend + 1, end);
	}
    }
    return root;
}


struct TreeNode* bstFromPreorder(int* preorder, int preorderSize){
    if (preorderSize == 0)
	return NULL;

    struct TreeNode *root = (struct TreeNode *) malloc(sizeof(struct TreeNode));
    int start = 0;
    int end = preorderSize - 1;
    extractRoot(root, preorder, start, end);

    return root;
}

int main(void)
{
    int input[] = { 4, 2 };
    int preorderSize = sizeof(input) / sizeof(int);
    bstFromPreorder(input, preorderSize);
}
