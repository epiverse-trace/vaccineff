# Create Status Column

This function generates a binary status column using the set of columns
passed through the variable `col_names`. This column must contain the
information of the outcomes or the vaccine dates. It generates a logical
condition using the operators `&` or `|`, and evaluates if the registers
in the columns contain or not information. If the logical operator is
`&`, the function returns a success only if all the columns contain
information. On the other hand, if the logical operator is `|`, it is
enough to find one column with information indicating success. It is
recommended to use this method when working with several outcomes or
several vaccine doses. By default, it returns a binary column where `0`
means no outcome or no vaccine and `1` means the opposite. However, it
can also receive custom options, e.g., `c("v", "u")` for vaccinated and
unvaccinated.

## Usage

``` r
set_status(data_set, col_names, operator = c("&", "|"), status = c(1, 0))
```

## Arguments

- data_set:

  `data.frame` with at least one column from which to generate the
  status specified in `status`.

- col_names:

  Name(s) of the column(s) as a string or a character vector containing
  the information from which the status is calculated.

- operator:

  A single logical operator to evaluate the condition.

- status:

  A two-element vector specifying the values to be assigned that
  indicate whether the individual is vaccinated or not, e.g.,
  `c("v","u")`. The first element of the vector must be the status when
  the condition is satisfied, i.e., vaccinated, while the second element
  is the value indicating that the individual is not vaccinated.

## Value

Status
