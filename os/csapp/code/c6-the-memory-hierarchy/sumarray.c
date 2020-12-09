int sumarrayrows(int a[M][N])
{
    int i, j, sum = 0;

    for (i = 0; i < M; i++)
	for (j = 0; j < N; j++)
	    sum += a[i][j];

    return sum;
}

int sumarraycols(int a[M][N])
{
    int i, j, sum = 0;

    for (j = 0; j < N; j++)
	for (i = 0; i < M; i++)
	    sum += a[i][j];

    return sum;
}
