int productarray3d(int a[N][N][N])
{
    int i, j, k, product = 1;

    for (i = N-1; i >= 0; i--)
	for (j = N-1; j >= 0; j--)
	    for (k = N-1; k >= 0; k--)
		product *= a[i][j][k];

    return product;
}
