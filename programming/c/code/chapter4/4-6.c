/* 
You can store values in variables [a-z] with =[a-z]
For example: 1 2 + =t
You can get the store value with '&'
For example: &t 2 +
======================
1 2 + =a
	3
last
the most recently printed value is: 	3
&a 2 + =b
	5
last
the most recently printed value is: 	5
 */



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
#include <math.h>
#include <string.h>
#include <ctype.h>

#define MAXOP 100		/* max size of operand or operator */
#define NUMBER '0'		/* signal that a number was found */
#define MATHLIB '1'		/* functions in math library */
#define VARIABLE '2'		/* computed value store in variable */
#define GET '3'			/* get value store in variable */

int getop(char []);
void push(double);
double pop(void);

double vars[26];
double last;

int main(void) {
  int type, v;
  double op2;
  char s[MAXOP];

  printf("You can store values in variables [a-z] with =[a-z]\n");
  printf("For example: 1 2 + =t\n");
  printf("You can get the store value with '&'\n");
  printf("For example: &t 2 +\n");
  printf("======================\n");

  while((type = getop(s)) != EOF) {
    switch (type) {
    case NUMBER:
      push(atof(s));
      break;
    case MATHLIB:
      if (strcmp(s, "sin") == 0)
	push(sin(pop()));
      else if (strcmp(s, "exp") == 0) 
	push(exp(pop()));
      else if (strcmp(s, "pow") == 0) {
	op2 = pop();
	push(pow(pop(), op2));
      } else if(strcmp(s, "last") == 0) {
	printf("the most recently printed value is: ");
	push(last);
      }
      else
	printf("error: unknown command %s\n", s);
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
    case '%':
      op2 = pop();
      if (op2 != 0.0)
	push(fmod(pop(), op2));
      else
	printf("error: zero modulus\n");
      break;
    case VARIABLE:
      v = tolower(s[0]);
      last = vars[v - 'a'] = pop();
      push(last);
      break;
    case GET:
      v = tolower(s[0]);
      push(vars[v - 'a']);
      break;
    case '\n':
      printf("\t%.8g\n", pop()); /* use the shortest representation: %e or %f */
      break;
    default:
      printf("error: unknown command %s\n", s);
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

/* top: print the top value without pop */
double gettop(void) {
  if (sp > 0)
    return val[sp-1];
  else {
    printf("error: stack empty\n");
    return 0.0;
  }
}

/* dupliate: duplicate the top value in the stack */
void duplicate() {
  double top = pop();
  push(top);
  push(top);
}

/* swap: swap the top two elements */
void swap(void) {
  double top1 = pop();
  double top2 = pop();
  push(top1);
  push(top2);
}

/* clear: clear the stack */
void clear(void) {
  sp = 0;
}

  

#include <ctype.h>

int getch(void);
void ungetch(int);

/* getop: get next character or numeric operand */
int getop(char s[])
{
  int i, c, c2;

  while ((s[0] = c = getch()) == ' ' || c == '\t') 
    ;
  s[1] = '\0';
  

  i = 0;
  if (isalpha(c))
    {
      while (isalpha(s[++i] = c = getch()))
	;
      s[i] = '\0';
      if (c != EOF)
	ungetch(c);
      return MATHLIB;
    }

  if (!isdigit(c) && c != '.' && c != '-')
    {
      if (c == '=')
	{
	  if (isalpha(c2 = getch()))
	    {
	      s[0] = c2;
	      return VARIABLE;
	    }
	  else if (c2 != EOF)
	    {
	      ungetch(c2);
	    }
	}
      else if (c == '&')
	{
	  if (isalpha(c2 = getch()))
	    {
	      s[0] = c2;
	      return GET;
	    }
	  else if (c2 != EOF)
	    {
	      ungetch(c2);
	    }
	}
      return c;			/* not a number */
    }
    
    
  /*  a unary minus will have no intervening space between it and its operand */

  if (c == '-') {
    c2 = getch();
    if (c2 != EOF)
      ungetch(c2);
    if (!isdigit(c2) && c2 != '.')
      return c;
  }
      
    
  if (isdigit(c) || c == '-')	/* collect integer part */
    while(isdigit(s[++i] = c = getch()))
      ;
  if (c == '.')			/* collect integer part */
    while (isdigit(s[++i] = c = getch()))
      ;
  s[i] = '\0';

  if (c != EOF)
    ungetch(c);			

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
  
  

