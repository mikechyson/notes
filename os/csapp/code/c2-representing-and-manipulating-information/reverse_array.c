void inplace_swap(int *x, int *y) {
  *y = *x ^ *y;
  *x = *x ^ *y;
  *y = *x ^ *y;
}


/* There no performance advantage to this way of swapping;
   it is merely an intellectual amusement.*/

/* a^(a^b)=a^a^b=0^b=b */
/* a^(a^b)^(a^b)=b^(a^b)=b^a^b=b^b^a=0^a=a */




void reverse_array(int a[], int cnt) {
  int first, last;
  /* for (first = 0, last = cnt -1; first <= last; first++, last--) */
  for (first = 0, last = cnt -1; first < last; first++, last--) /* There no need to swap the middle element with itself */
    inplace_swap(&a[first], &a[last]);
}


  
