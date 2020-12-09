/* x is of type int */

/* Any bit of x equals 1 */
/* (0 | x) == 0; */
!~x;

/* Any bit of x equals 0 */
/* (~0 & x) == ~0; */
~x

/* Any bit in the least significant byte of x equals 1. */
/* ((0xFF & x) | 0) == 0; */
!~(x | 0xFF);

/* Any bit in the most significant byte of x equals 0. */
(((0xFF << ((sizeof(int)-1) <<3)) & x ) | x) == 0;
