void fft(complex *a, complex *y, int n, complex wn)
{
    if (n > 1)
    {
        int m = n >> 1;
        for (int i=0; i < m; ++i)
        {
            y[i] = a[i+i];
            y[i+m] = a[i+i+1];
        }
        
        fft(y, a, m, wn * wn);
        fft(y + m, a + m, m, wn * wn);
        
        complex w(1, 0);
        for (int i=0; i < m; ++i)
        {
            complex tmp = w * a[i+m];
            y[i] = a[i] + tmp;
            y[i+m] = a[i] - tmp;
            w = w * wn;
        }
    }
    else y[0] = a[0];
}