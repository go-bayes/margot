# Policy Value Explainer (shared text)

Returns a concise, reusable explanation of the policy value quantities
used across Margot's policy tree reporting functions. This helper is
intended to be a single source of truth for how we describe these
estimands to end users and policymakers.

## Usage

``` r
margot_policy_value_explainer(
  audience = c("policy", "research"),
  include_acronyms = FALSE
)
```

## Arguments

- audience:

  Character; one of "policy" or "research". The wording is identical for
  now, but the parameter allows future tailoring.

- include_acronyms:

  Logical; if TRUE, append a list of common acronyms used in NZAVS
  research (RWA, SDO, PWI, NZSEI). Default FALSE.

## Value

A single character string with the explanation paragraph(s).
