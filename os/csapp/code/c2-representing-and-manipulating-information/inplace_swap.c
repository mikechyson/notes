void inplace_swap(int *x, int *y) {
  *y = *x ^ *y;
  *x = *x ^ *y;
  *y = *x ^ *y;
}


/* There no performance advantage to this way of swapping;
   it is merely an intellectual amusement.*/

/* a^(a^b)=a^a^b=0^b=b */
/* a^(a^b)^(a^b)=b^(a^b)=b^a^b=b^b^a=0^a=a */

