#include <algorithm>

struct SuffixArray{
  int n;
  const int* S;
  int* Rank;
  int* Tmp;

  struct idx_cmp{
    const int* S;
    idx_cmp(const int* S): S(S) {}

    bool operator()(int i, int j) const{
      return S[i] < S[j];
    }

    bool eq(int i, int j) const{
      return S[i] == S[j];
    }
  };

  struct rank_cmp{
    const int* R;
    const int n;
    const int d;

    rank_cmp(const int* R, int n, int d): R(R), n(n), d(d) {}

    bool operator()(int i, int j) const{
        return R[i] < R[j]
            || (R[i] == R[j] && i + d < n && j + d < n && R[i + d] < R[j + d]);
    }

    bool eq(int i, int j) const{
        return R[i] == R[j] 
            && ((i + d < n && j + d < n && R[i + d] == R[j + d])
                || (i + d >= n && j + d >= n));
    }
  };

  SuffixArray(int n): n(n) {
    Rank = new int[n];
    Tmp = new int[n];
  }

  ~SuffixArray() {
    delete[] Rank;
    delete[] Tmp;
  }

  template<typename Op>
  void rank(int* r, int* SA, const Op& op) const {
    std::sort(SA, SA + n, op);
    r[SA[0]] = 0;
    for (int i=1; i < n; ++i) {
      r[SA[i]] = r[SA[i-1]] +
    (op.eq(SA[i], SA[i-1]) ? 0 : 1);
    }
  }

  void operator()(const int* S, int* SA) {
    for (int i=0; i < n; ++i) {
      SA[i] = i;
    }
    rank(Rank, SA, idx_cmp(S));
    for (int d=1; d < n; d <<= 1) {
      rank(Tmp, SA, rank_cmp(Rank, n, d));
      std::swap(Rank, Tmp);
    }
  }
};
