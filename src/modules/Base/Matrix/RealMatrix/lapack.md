# Introdcution to lapack

## Naming convention

- `p-mm-aaa`
- `p` can be 
	- `S` Single precision real
	- `D` double precision real
	- `C` single precision complex
	- `Z` double precision complex
- `mm` matrix type
	- `BD` bi-diagonal
	- `DI` diagonal
	- `GB` general band matrix
	- `GE` general matrix
	- `GG` general matrices, generalized problem
	- `GT` general tridiagonal
	- `HB` hermitian band matrix
	- `HE`
	- `HG`
	- `HP`
	- `HS`
	- `OP`
	- `OR`
	- `PB`
	- `PO`
	- `PP`
	- `PT`
	- `SB`
	- `SP`
	- `ST`
	- `SY`
	- `TB`
	- `TG`
	- `TP`
	- `TR`
	- `TZ`
	- `UN`
	- `UP`


The routines are classified as
- driver
- computational
- auxiliary


## Introduction to `lapack95`

- `lapack95` provides interfaces to all LAPACK driver and computational routines.

** Naming convention

- GB general band
- GE general
- GG general matrices generalized problem
- GT general tridiagonal
- HB hermitian band
- HE hermitian
...

- All array arguments to LAPACK95 routines are assumed shape arrays.
- Zeros dimensional arrays can be send as an empty array
- workspace arguments and arguments to specify their dimensions are not used
- optional arguments are given
- generic name facilities

