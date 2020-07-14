*   outer model (blocks):<br><br>
    **X**<sub>p × n</sub> = **α**<sub>p × k</sub>**λ**<sub>k × n</sub> + **ε**<sub>p × n</sub><br>
    where *p* is the number of variables (features),  *k* is the number of latent factors and *n* is the sample size.<br>
    **X** is the data matrix with variables in the rows and sample
    elements in the columns, **α**<sub> • × j</sub> is the column
    vector of loadings for the j<sup>*t**h*</sup> latent variable and
    **λ**<sub>j × • </sub> is the row vector of scores for the
    j<sup>*t**h*</sup> unobserved variable, j = 1, …, k.<br>

    Normality and independence are assumed for the errors as
    ε<sub>ij</sub> ∼ N(0, σ<sub>i</sub><sup>2</sup>), for
    i = 1, …, p.<br>

*  inner model:<br>
  *   paths: <br>

        **λ**<sub>j × • </sub> = **β**<sup>⊤</sup>**λ**<sup>( − j)</sup> + ν <br>
        where **β** is a vector of constant coefficients and
        **λ**<sup>( − j)</sup><sub>(k − 1) × n</sub> represent a subset of the matrix of scores, i.e. at least excluding the j<sup>*t**h*</sup> row scores.<br>
        The error assumes standard normal distribution.

  *  exogenous: <br>
        **Y**<sub>l × • </sub> = **γ**<sub>0</sub> + **γ**<sup>⊤</sup>**λ** + **ξ** <br>

        where l is the number of exogenous variables, **γ** is a vector of constant coefficients and
        **γ**<sub>**0**</sub> is the intercept.
	<br> **λ**<sub>k × n</sub>
        is the matrix of scores and the error assumes
        ξ<sub>lj</sub> ∼ N(0, τ<sub>l</sub><sup>2</sup>), for
    j = 1, …, n.<br>
