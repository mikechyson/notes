/* 
In reverse Polish notation, each operator follows its operands; an infix expression like
(1 - 2) * (4 + 5)
is entered as
1 2 - 4 5 + *
Parentheses are not needed; the notation is unambiguous as long as we know how many operands each operator expects.
*/

/*
The implementation is simple. 
Each operand is pushed onto a stack; 
when an operator arrives, 
the proper number of operands (two for binary operators) is popped, 
the operator is applied to them, and 
the result is pushed back onto the stack. 
*/
			    
/*
while (next operator or operand is not end-of-file indicator) 
    if (number)
        push it
    else if (operator)
        pop operands
        do operation
        push result
    else if (newline)
        pop and print top of stack
    else
        error
*/

#include <stdio.h>
#include <stdlib.h>		/* for atof() */


#define MAXOP 100		/* max size of operand or operator */
#define NUMBER '0'		/* signal that a number was found */

int getop(char []);
void push(double);
double pop(void);

int main(void) {
  int type;
  double op2;
  char s[MAXOP];

  while((type = getop(s)) != EOF) {
    switch (type) {
    case NUMBER:
      push(atof(s));
      break;
    case '+':
      push(pop() + pop());
      break;
    case '*':
      push(pop() * pop());
      break;
    case '-':
      op2 = pop();		/* order executed is uncertain in the argument list */
      push(pop() - op2);
      break;
    case '/':
      op2 = pop();
      if (op2 != 0.0)
	push(pop() / op2);
      else
	printf("error: zero divisor\n");
      break;
    case '\n':
      printf("\t%.8g\n", pop()); /* use the shortest representation: %e or %f */
      break;
    }
  }
  return 0;
}


#define MAXVAL 100		/* maximum depth of val stack */

int sp = 0;			/* next free stack position */
double val[MAXVAL];		/* value stack */

/* push: push f onto value stack */
void push(double f) {
  if (sp < MAXVAL)
    val[sp++] = f;
  else
    printf("error: stack full, can't push %g\n", f);
}

/* pop: pop and return top value from stack */
double pop(void) {
  if (sp > 0)
    return val[--sp];
  else {
    printf("error: stack empty\n");
    return 0.0;
  }
}
    
  

#include <ctype.h>

int getch(void);
void ungetch(int);

/* getop: get next character or numeric operand */
int getop(char s[]) {
  int i, c;

  while ((s[0] = c = getch()) == ' ' || c == '\t') /* skip blank and tab */
    ;

  s[1] = '\0';
  if (!isdigit(c) && c != '.')
    return c;			/* not a number */

  i = 0;
  if (isdigit(c))		/* collect integer part */
    while(isdigit(s[++i] = c = getch()))
      ;
  if (c == '.')			/* collect integer part */
    while (isdigit(s[++i] = c = getch()))
      ;
  s[i] = '\0';

  if (c != EOF)
    ungetch(c);			/* ? */

  return NUMBER;
    
}


#define BUFSIZE 100

char buf[BUFSIZE];		/* buffer for ungetch */
int bufp = 0;			/* next free position in buf */

int getch(void)	{		/* get a (possible pushed-back) character */
  return (bufp > 0) ? buf[--bufp] : getchar();
}

void ungetch(int c) {		/* push character back on input */
  if (bufp >= BUFSIZE)
    printf("ungetch: too many characters\n");
  else
    buf[bufp++] = c;
}
  
  


/* result: 
1 2 - 4 5 + *
	-9
*/
