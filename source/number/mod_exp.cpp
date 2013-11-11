int mod_exp(int a, int n, int m)
{
	if (n == 1) return a;
	int tmp = mod_exp(a, n >> 1, m);
	tmp = (long long) tmp * tmp % m;
	if (n & 1) tmp = (long long) tmp * a % m;
	return tmp;
}