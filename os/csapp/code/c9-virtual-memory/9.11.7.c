int *search(int *p, int val)
{
    while (*p && *p != val)
	p += sizeof(int); 		/* should be p++ */
    return p;
}
