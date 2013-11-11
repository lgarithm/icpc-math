void eratosthenes(int n)
{
	for (int i=0; i <= n; ++i) p[i] = 1;
	for (int i=2; i <= n; ++i)
		if (p[i])
			for (int j=i * i; j <= n; j += i)
				p[j] = 0;
}