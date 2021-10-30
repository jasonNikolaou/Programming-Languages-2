/* Run with:
 *
 * frama-c -wp -wp-prover alt-ergo -wp-rte -wp-timeout 60 -wp-verbose 0 samenum.c -then -report
 *
 * Tested with Frama-C Phosphorus-20170501.
 */

#define MAXN 1000000
#define MAXV 2000000
/*@ predicate isPair{L}(integer N, int* x, integer i, integer j) =
  @   0 <= i < j < N && \at(x[i], L) == \at(x[j], L);
  @*/

/*@ predicate existsPair{L}(integer N, int* x) =
  @   \exists integer i, j; 0 <= i < j < N && \at(x[i], L) == \at(x[j], L);
  @*/

/*@ predicate isValidSolution{L}(integer N, int* x, integer r) =
  @   existsPair{L}(N, x) ?
  @     (\exists integer i, j; 0 <= i < j < N && \at(x[i], L) == \at(x[j], L) ==> r == j - i) &&
  @     (\forall integer n, m; 0 <= n < m < N && \at(x[n], L) == \at(x[m], L) ==> r >= m - n)
  @   : r == 0;
  @*/

/*@ requires 0 <= N <= MAXN;
  @ requires \valid(x + (0..N-1));
  @ requires \forall integer j; 0 <= j < N ==> 0 <= x[j] <= MAXV;
  @ ensures  isValidSolution(N, x, \result);
  @*/
int samenum(int N, int *x) {
  int p[MAXV+1];
  /*@ loop invariant 0 <= i <= MAXV+1;
	  @ loop invariant \forall integer j; 0 <= j < i ==> p[j] == -1;
	  @ loop assigns i, p[0 .. MAXV];
	  @ loop variant MAXV - i;
	  @*/
  for (int i = 0; i <= MAXV; ++i) p[i] = -1;
  int best = 0;
  /*@ loop invariant 0 <= i <= N;
    @ loop invariant \forall integer j; i <= j < N ==> p[x[j]] >= -1;
		@ loop invariant \forall integer j; 0 <= j < i ==>
				(best >= j - p[x[j]] ||	p[x[j]] == j);
		@ loop invariant best >= 0;
    @ loop invariant \forall integer j; 0 <= j <= MAXV && p[j] >= 0 <==>
				(\exists integer k; 0 <= k < i && x[k] == j);
		@ loop invariant \forall integer j, k; 0 <= k <= j < i && x[j] == x[k] ==>
				p[x[j]] <= k;
		@ loop invariant isValidSolution(i, x, best);
	  @ loop assigns i, best, p[0 .. MAXV];
	  @ loop variant N - i;
	  @*/
  for (int i = 0; i < N; ++i) {
    if (p[x[i]] == -1) p[x[i]] = i;
    else if (i-p[x[i]] > best) best = i-p[x[i]];
  }
  return best;
}
