import BayesBoom as boom
import numpy as np
import patsy
import R


class SparseDynamicRegression:
    """
    A dynamic regression model in discrete time.  Let beta_t denote the
    regression coefficients at time t.  The model

    A DynamicRegressionModel is a time series regression model where the
    coefficients obey a classic state space model.  The number of observations
    at each time point might differ, and observation i at time t can refer to a
    different subject than observation i at time t+1.  The model is implemented
    as a multivariate state space model.  Through data augmentation one could
    extend this model to most GLM's.

    Define the set of responses at time t as Y'_t = [y_1t, y_2t, ... y_n_tt],
    where Y_t = X[t] * beta[t] + error[t], with temporally IID error term
    error[t] ~ N(0, sigma^2 * I).

    Each coefficient beta[j, t] is zero with probability determined by a Markov
    chain Pr(beta[j, t] = s | beta[j, t-1] = r) = q[r, s], for r, s \\in {0,
    1}.  Let gamma[j, t] be an indicator for beta[j, t] being nonzero.  The
    model is that the sequences gamma[j, .] are IID across j.

    The conditional distribution of beta[j, t] given beta[j, t-1], and given
    that both are nonzero, is normal with mean b_jt = b_jt-1, and variance
    tau^2_j.  In other words, conditional on inclusion, model coefficients obey
    independent local level models (Gaussian random walks).
    """

    def __init__(self):
        self._residual_sd_prior = None
        self._innovation_sd_prior = None
        self._prior_inclusion_probabilities = None
        self._model = None
        self._sdy = None
        self._sdx = None

    @property
    def xdim(self):
        """
        The number of predictor variables.  The dimension of the time-vayring
        coefficient vector.
        """
        if self._model is None:
            return 0
        else:
            return self._model.xdim

    @property
    def time_dimension(self):
        """
        The number of distinct time periods in the training data.
        """
        if self._model is None:
            return 0
        else:
            return self._model.time_dimension

    def set_residual_sd_prior(self, prior: R.SdPrior):
        """
        Set the prior distribution for the residual standard deviation
        parameter.
        """
        self._residual_sd_prior = prior

    def set_innovation_sd_prior(self, prior):
        """
        Set the (independent) prior distributions on the standard deviations

        Args:
          prior: Either a single R.SdPrior object, or a list of such objects.
            If a list, then the number of elements must match the dimension of
            the predictor variable.  A single object will be repeated for each
            predictor.
        """
        self._innovation_sd_prior = prior

    def set_transition_probability_prior(self, probs, sample_size):
        pass

    def train_model(self, data, formula, timestamps, niter, ping=None,
                    seed=None):
        if seed is not None:
            boom.GlobalRng.rng.seed(int(seed))
        response, predictors = patsy.dmatrices(formula, data, eval_env=1)
        xdim = predictors.shape[1]
        self._model = boom.DynamicRegressionModel(xdim)
        self._assign_data(response, predictors, timestamps)
        self._assign_posterior_sampler()
        self._allocate_space(niter)
        if ping is None:
            ping = int(np.round(niter / 10))

        for i in range(niter):
            R.print_timestamp(i, ping)
            self._model.sample_posterior()
            self._record_draw(i)

    def restore_state(self, iteration: int):
        """
        Restore the state of the model to the specified MCMC iteration.
        """
        residual_sd = self.residual_sd[iteration]
        self._model.set_residual_sd(residual_sd)
        tmp = self.innovation_sd[iteration, :] / residual_sd
        self._model.set_unscaled_innovation_sds(boom.Vector(tmp))

        for i in range(self.xdim):
            self._model.set_transition_probabilities(
                i,
                boom.Matrix(self.transition_probabilities[
                    iteration, i, :, :]))
        self._model.set_coefficients(
            boom.Matrix(self.coefficients[iteration, :, :].astype("float")))

    def _assign_data(self, response, predictors, timestamps):
        """
        Helper function for train_model.

        Args:
          response: A 1-d numpy array of response values.
          predictors:  A 2-d numpy array of predictor values.
          timestamps: A numpy array or pandas series indicating the time period
            to which each observation belongs.

        Effects:
          self._unique_timestamps is populated with the sorted, unique values
            in timestamps.
          Data are assigned to self._model.

          Relevant data summaries are stored so they can be used to set default
          priors.
        """
        self._unique_timestamps = sorted(set(timestamps))
        self._sdy = R.sd(response)
        self._sdx = R.sd(predictors)
        self._sdx[self._sdx <= 0] = 1.0

        for timestamp in self._unique_timestamps:
            index = timestamps == timestamp
            time_point = boom.RegressionDataTimePoint(
                boom.Matrix(predictors[index, :].astype("float")),
                boom.Vector(response[index].astype("float")))
            self._model.add_data(time_point)

    def _assign_posterior_sampler(self):
        """
        Helper function for 'train_model'.

        Create a posterior sampler from self._model and either default priors or
        priors assigned using the set_X_prior member functions.

        Effects:
          On exit self._model will have a posterior sampler assigned.
          Some prior distributions may be modified
        """

        if self._residual_sd_prior is None:
            self._residual_sd_prior = R.SdPrior(self._sdy)
        if not isinstance(self._residual_sd_prior, R.SdPrior):
            raise Exception("Expected an SdPrior.")

        if self._innovation_sd_prior is None:
            # What if sdx is 0 (because x is constant)?
            innovation_sd_prior_guess = np.array(
                .01 * self._sdy / self._sdx, dtype="float")
            innovation_sd_prior_sample_size = np.ones_like(self._sdx)
        elif isinstance(self._innovation_sd_prior, R.SdPrior):
            innovation_sd_prior_guess = self._innovation_sd_prior

        sampler = boom.DynamicRegressionDirectGibbsSampler(
            self._model,
            self._residual_sd_prior.sigma_guess,
            self._residual_sd_prior.sample_size,
            boom.Vector(innovation_sd_prior_guess),
            boom.Vector(innovation_sd_prior_sample_size),
            boom.Vector(prior_inclusion_probabilities),
            boom.Vector(expected_inclusion_duration),
            boom.Vector(transition_probability_prior_sample_size),
            boom.GlobalRng.rng)

        self._model.set_method(sampler)

    def _allocate_space(self, niter: int):
        """
        Create space to store 'niter' MCMC draws.
        """
        self.residual_sd = np.empty(niter)
        self.innovation_sd = np.empty((niter, self.xdim))
        self.transition_probabilities = np.empty((niter, self.xdim, 2, 2))
        self.coefficients = np.empty((niter, self.xdim, self.time_dimension))

    def _record_draw(self, iteration: int):
        """
        Args:
          iteration:  The current MCMC iteration number.

        Effects:
          The current state of the model is recorded at the specified iteration
          number.
        """
        self.residual_sd[iteration] = self._model.residual_sd
        self.innovation_sd[iteration, :] = (
            self._model.residual_sd *
            self._model.unscaled_innovation_sds.to_numpy()
        )
        for i in range(self.xdim):
            self.transition_probabilities[iteration, i, :, :] = (
                self._model.transition_probabilities(i).to_numpy()
            )
        self.coefficients[iteration, :, :] = self._model.coefficients.to_numpy()
