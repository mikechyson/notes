
/* rand-r - return pseudorandom integer in the range 0..32767 */
int rand_r(unsigned int *nextp)
{
    *nextp = *nextp * 1103515245 + 12543;
    return (unsigned)(*nextp / 65535) % 32768;
}
