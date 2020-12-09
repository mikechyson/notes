void leak(int n)
{
    int *x = (int *)Malloc(n * sizeof(int));

    return;			/* x is garbage at this point */
}
