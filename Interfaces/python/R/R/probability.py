import scipy.stats as sp
import numpy as np

# expose R's equivalent of pnorm, qnorm, etc.


def as_numeric(x):
    return np.array(x)


def dnorm(x, mean=0, sd=1, log=False):
    """Normal density function.

    :param x: number, list or numpy array
        The argument of the density function.

    :param x: number, list or numpy array
        The argument of the density function.

    """
    x = as_numeric(x)
    if log:
        return sp.norm.logpdf(x, mean, sd)
    else:
        return sp.norm.pdf(x, mean, sd)


def pnorm(x, mean=0, sd=1, lower=True, log=False):
    """
    Normal cumulative distribuion function.

    """
    if log:
        if lower:
            return sp.norm.logcdf(x, loc=mean, scale=sd)
        else:
            return sp.norm.logsf(x, loc=mean, scale=sd)
    else:
        if lower:
            return sp.norm.cdf(x, loc=mean, scale=sd)
        else:
            return sp.norm.sf(x, loc=mean, scale=sd)


def qnorm(x, mean=0, sd=1, lower=True, log=False):
    """
    Quantiles of the normal distribuion.

    """
    if log:
        x = np.exp(x)

    if lower:
        return sp.norm.ppf(x, mean, sd)
    else:
        return sp.norm.ppf(1 - x, mean, sd)


def rnorm(n, mean=0, sd=1):
    """
    Random deviates from the normal distribution.

    """

    return np.random.randn(n) * sd + mean


def rmarkov(n: int, P: np.ndarray, pi0=None):
    """
    Simulation a Markov chain of length n from transition probability matrix P,
    with initial distribution pi0.  The state space is the set of integers in
    {0, ..., S-1} where S is the number of rows in P.

    Args:
      n:  The length of the Markov chain to simulate.
      P: A transition probability matrix.  A square matrix of non-negative
         elements, where each row sums to 1.
      pi0: The distribution of the state at time 0.  A vector of non-negative
        numbers summing to 1.

    Returns:
      A numpy array of integer dtype containing the simulated Markov chain..
    """
    assert(isinstance(P, np.ndarray))
    assert(len(P.shape) == 2)
    S = P.shape[0]
    assert(P.shape[1] == S)
    assert(S > 0)
    if pi0 is None:
        pi0 = np.ones(S) / S
    assert(len(pi0) == S)

    ans = np.full(n, -1)
    ans[0] = np.random.choice(range(S), p=pi0)
    for i in range(1, n):
        ans[i] = np.random.choice(range(S), p=P[ans[i-1], :])
    return(ans)
