/* 
   Design a stack that supports push, pop, top, and retrieving the minimum element in constant time.

   push(x) -- Push element x onto stack.
   pop() -- Removes the element on top of the stack.
   top() -- Get the top element.
   getMin() -- Retrieve the minimum element in the stack.
 

   Example 1:

   Input
   ["MinStack","push","push","push","getMin","pop","top","getMin"]
   [[],[-2],[0],[-3],[],[],[],[]]

   Output
   [null,null,null,null,-3,null,0,-2]

   Explanation
   MinStack minStack = new MinStack();
   minStack.push(-2);
   minStack.push(0);
   minStack.push(-3);
   minStack.getMin(); // return -3
   minStack.pop();
   minStack.top();    // return 0
   minStack.getMin(); // return -2
 

   Constraints:

   Methods pop, top and getMin operations will always be called on non-empty stacks.
*/

#include <limits.h>
#include <stdio.h>
#include <stdlib.h>


typedef struct minstack{
    int min;
    int value;
    struct minstack *next;    
} MinStack;

/** initialize your data structure here. */

MinStack* minStackCreate() {
    MinStack *obj = (MinStack *) malloc(sizeof(MinStack));
    obj->min = INT_MAX;
    obj->value = INT_MAX;
    obj->next = NULL;
    return obj;
}

void minStackPush(MinStack* obj, int x) { /* call by value */

    int omin = obj->min;
    int ovalue = obj->value;
    MinStack *onext = obj->next;

    MinStack *msptr = (MinStack *) malloc(sizeof(MinStack));
    msptr->value = ovalue;
    msptr->next = onext;
    msptr->min = omin;

    obj->value = x;
    obj->next = msptr;
    if (x > omin)
	obj->min = omin;
    else
	obj->min = x;

}

void minStackPop(MinStack* obj) { /* call by value */
    MinStack *next = obj->next;
    obj->value = next->value;
    obj->min = next->min;
    obj->next = next->next;

}

int minStackTop(MinStack* obj) {
    return obj->value;  
}

int minStackGetMin(MinStack* obj) {
    return obj->min;
}

void minStackFree(MinStack* obj) {
    obj->min = INT_MAX;
    obj->value = INT_MAX;
    obj->next = NULL;
}

/**
 * Your MinStack struct will be instantiated and called as such:
 * MinStack* obj = minStackCreate();
 * minStackPush(obj, x);
 
 * minStackPop(obj);
 
 * int param_3 = minStackTop(obj);
 
 * int param_4 = minStackGetMin(obj);
 
 * minStackFree(obj);
 */
