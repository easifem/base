# Space-Time Convective Matrix

## ToDO

## Stucture

```fortran
 TYPE, PUBLIC, EXTENDS( STElemShapeData_ ) :: STConvectiveMatrix_
 END TYPE
```

## Getting Started

### Making the object

```fortran
Obj = STConvectiveMatrix( Row, Col, NIPS, NIPT )
Obj = STConvectiveMatrix( I1, I2, I3, I4, NIPS, NIPT )
Obj = STConvectiveMatrix( )
```

```fortran
Obj => STConvectiveMatrix_Pointer( Row, Col, NIPS, NIPT )
Obj => STConvectiveMatrix_Pointer( I1, I2, I3, I4, NIPS, NIPT )
Obj => STConvectiveMatrix_Pointer( )
```

### Getting Convective Matrix

- Currently, there are 39 interfaces for space-time convective matrix.

- The generic subroutine for getting the convective matrix is `getConvectiveMatrix()`. A summary of different interfaces are given below.

```fortran
CALL Obj % getConvectiveMatrix( C, Term1, Term2 )
CALL Obj % getConvectiveMatrix( C, Term1, Term2, nCopy )
CALL Obj % getConvectiveMatrix( C, Term1, Term2, CType )
CALL Obj % getConvectiveMatrix( C, Term1, Term2, CType, nCopy )
CALL Obj % getConvectiveMatrix( U, dCdU, dCdU_Type )
CALL Obj % getConvectiveMatrix( U )
CALL Obj % getConvectiveMatrix( U, C, dCdU, dCdU_Type )
```

The description of each interface is provided below.

#### Type-1

$$
M\left( {I,J,a,b} \right) = \left[ {\int_{{I_n}}^{} {\int_\Omega ^{} {{c_j}\frac{{\partial {N^I}{T_a}}}{{\partial {x_j}}} \cdot {N^J}{T_b}d\Omega dt} } } \right]
$$

$$
M\left( {I,J,a,b} \right) = \int_{{I_n}}^{} {\int_\Omega ^{} {\frac{{\partial {N^I}{T_a}}}{{\partial t}} \cdot {c_j}\frac{{\partial {N^J}{T_b}}}{{\partial {x_j}}}d\Omega dt} }
$$

The above two matrix can be computed using the following subroutine.

```fortran
CALL Obj % getConvectiveMatrix( C, Term1, Term2 )
```

- In the above call `Term1` and `Term2` can be 0 or 1. If it is 1 then it denotes the spatial gradient. Both `Term1` and `Term2` cannot be 0 or 1.
- `C` denotes the convective velocity. It can be Rank-1, Rank-2, Rank-3 array. 
  - If `C` is rank-1, `C(:)`,  then it denotes the convective velocity constant in both space and time.
  - If `C` is rank-2, `C(:,:)`, then it denotes the space-nodal values of convective velocity. In this case velocity is constant in time domain.
  - If `C` is rank-3, `C(:,:,:)`, then it denotes the space-time nodal values of convective velocity. In this case velocity changes with both space and time.

#### Type-2

$$
{M^{pq}}\left( {I,J,a,b} \right) = \left[ {\int_{{I_n}}^{} {\int_\Omega ^{} {{c_j}\frac{{\partial {N^I}{T_a}}}{{\partial {x_j}}} \cdot {N^J}{T_b}d\Omega dt} } } \right]{\delta _{pq}}
$$

$$
M\left( {I,J,a,b} \right) = \int_{{I_n}}^{} {\int_\Omega ^{} {\frac{{\partial {N^I}{T_a}}}{{\partial t}} \cdot {c_j}\frac{{\partial {N^J}{T_b}}}{{\partial {x_j}}}d\Omega dt} } {\delta _{pq}}
$$

The above two matrix can be computed using the following subroutine.

```fortran
CALL Obj % getConvectiveMatrix( C, Term1, Term2, nCopy )
```

- In the above call `Term1` and `Term2` can be 0 or 1. If it is 1 then it denotes the spatial gradient. Both `Term1` and `Term2` cannot be 0 or 1.
- `C` denotes the convective velocity. It can be Rank-1, Rank-2, Rank-3 array. 
  - If `C` is rank-1, `C(:)`,  then it denotes the convective velocity constant in both space and time.
  - If `C` is rank-2, `C(:,:)`, then it denotes the space-nodal values of convective velocity. In this case velocity is constant in time domain.
  - If `C` is rank-3, `C(:,:,:)`, then it denotes the space-time nodal values of convective velocity. In this case velocity changes with both space and time.

#### Type-3

$$
M\left( {I,J,a,b} \right) = \left[ {\int_{{I_n}}^{} {\int_\Omega ^{} {{c_j}\frac{{\partial {N^I}{T_a}}}{{\partial {x_j}}} \cdot {N^J}{T_b}d\Omega dt} } } \right]
$$

$$
M\left( {I,J,a,b} \right) = \int_{{I_n}}^{} {\int_\Omega ^{} {\frac{{\partial {N^I}{T_a}}}{{\partial t}} \cdot {c_j}\frac{{\partial {N^J}{T_b}}}{{\partial {x_j}}}d\Omega dt} }
$$

In the above case if `C` is defined at integration points then we can use the following interface. 

```fortran
CALL Obj % getConvectiveMatrix( C, Term1, Term2, CType )
```

- In the above call `Term1` and `Term2` can be 0 or 1. If it is 1 then it denotes the spatial gradient. Both `Term1` and `Term2` cannot be 0 or 1.
- `C` denotes the convective velocity. It can be Rank-1, Rank-2, Rank-3 array. 
- `CType` can be `NodalValues` or `QuadPoints`.

#### Type-4

$$
{M^{pq}}\left( {I,J,a,b} \right) = \left[ {\int_{{I_n}}^{} {\int_\Omega ^{} {{c_j}\frac{{\partial {N^I}{T_a}}}{{\partial {x_j}}} \cdot {N^J}{T_b}d\Omega dt} } } \right]{\delta _{pq}}
$$

$$
M\left( {I,J,a,b} \right) = \int_{{I_n}}^{} {\int_\Omega ^{} {\frac{{\partial {N^I}{T_a}}}{{\partial t}} \cdot {c_j}\frac{{\partial {N^J}{T_b}}}{{\partial {x_j}}}d\Omega dt} } {\delta _{pq}}
$$

In the above case if `C` is defined at integration points then we can use the following interface. 

```fortran
CALL Obj % getConvectiveMatrix( C, Term1, Term2, CType, nCopy )
```

- In the above call `Term1` and `Term2` can be 0 or 1. If it is 1 then it denotes the spatial gradient. Both `Term1` and `Term2` cannot be 0 or 1.
- `C` denotes the convective velocity. It can be Rank-1, Rank-2, Rank-3 array. 
- `CType` can be `NodalValues` or `QuadPoints`.

#### Type-5

$$
M\left( {I,J,a,b} \right) = \left[ {\int_{{I_n}}^{} {\int_\Omega ^{} {\frac{{\partial {N^I}{T_a}}}{{\partial t}} \cdot {c_j}\frac{{\partial {N^J}{T_b}}}{{\partial {x_j}}}d\Omega dt} } } \right]
$$

$$
M\left( {I,J,a,b} \right) = \left[ {\int_{{I_n}}^{} {\int_\Omega ^{} {{c_j}\frac{{\partial {N^I}{T_a}}}{{\partial {x_j}}}\frac{{\partial {N^J}{T_b}}}{{\partial t}}d\Omega dt} } } \right]
$$

The above two matrices can be computed using the following subroutine.

```fortran
CALL Obj % getConvectiveMatrix( C, Term1, Term2 )
```

- In the above call `Term1` and `Term2` are characters. They can be `dt`, `dx, dy, dz`.
- `C` denotes the convective velocity. It can be Rank-1, Rank-2, Rank-3 array. 
  - If `C` is rank-1, `C(:)`,  then it denotes the convective velocity constant in both space and time.
  - If `C` is rank-2, `C(:,:)`, then it denotes the space-nodal values of convective velocity. In this case velocity is constant in time domain.
  - If `C` is rank-3, `C(:,:,:)`, then it denotes the space-time nodal values of convective velocity. In this case velocity changes with both space and time.

#### Type-6

$$
{M^{pq}}\left( {I,J,a,b} \right) = \left[ {\int_{{I_n}}^{} {\int_\Omega ^{} {\frac{{\partial {N^I}{T_a}}}{{\partial t}} \cdot {c_j}\frac{{\partial {N^J}{T_b}}}{{\partial {x_j}}}d\Omega dt} } } \right]{\delta _{pq}}
$$

$$
{M^{pq}}\left( {I,J,a,b} \right) = \left[ {\int_{{I_n}}^{} {\int_\Omega ^{} {{c_j}\frac{{\partial {N^I}{T_a}}}{{\partial {x_j}}}\frac{{\partial {N^J}{T_b}}}{{\partial t}}d\Omega dt} } } \right] {\delta_{pq}}
$$

The above two matrices can be computed using the following subroutine.

```fortran
CALL Obj % getConvectiveMatrix( C, Term1, Term2, nCopy )
```

- In the above call `Term1` and `Term2` are characters. They can be `dt`, `dx, dy, dz`.
- `C` denotes the convective velocity. It can be Rank-1, Rank-2, Rank-3 array. 
  - If `C` is rank-1, `C(:)`,  then it denotes the convective velocity constant in both space and time.
  - If `C` is rank-2, `C(:,:)`, then it denotes the space-nodal values of convective velocity. In this case velocity is constant in time domain.
  - If `C` is rank-3, `C(:,:,:)`, then it denotes the space-time nodal values of convective velocity. In this case velocity changes with both space and time.

#### Type-7

$$
M\left( {I,J,a,b} \right) = \left[ {\int_{{I_n}}^{} {\int_\Omega ^{} {\frac{{\partial {N^I}{T_a}}}{{\partial t}} \cdot {c_j}\frac{{\partial {N^J}{T_b}}}{{\partial {x_j}}}d\Omega dt} } } \right]
$$

$$
M\left( {I,J,a,b} \right) = \left[ {\int_{{I_n}}^{} {\int_\Omega ^{} {{c_j}\frac{{\partial {N^I}{T_a}}}{{\partial {x_j}}}\frac{{\partial {N^J}{T_b}}}{{\partial t}}d\Omega dt} } } \right]
$$

In the above two matrices if `C` matrix is defined at integration points then we can use the following subroutine.

```fortran
CALL Obj % getConvectiveMatrix( C, Term1, Term2, CType )
```

- In the above call `Term1` and `Term2` are characters. They can be `dt`, `dx, dy, dz`.
- `C` denotes the convective velocity. It can be Rank-1, Rank-2, Rank-3 array. 

#### Type-8

$$
{M^{pq}}\left( {I,J,a,b} \right) = \left[ {\int_{{I_n}}^{} {\int_\Omega ^{} {\frac{{\partial {N^I}{T_a}}}{{\partial t}} \cdot {c_j}\frac{{\partial {N^J}{T_b}}}{{\partial {x_j}}}d\Omega dt} } } \right] {\delta_{pq}}
$$

$$
{M^{pq}}\left( {I,J,a,b} \right) = \left[ {\int_{{I_n}}^{} {\int_\Omega ^{} {{c_j}\frac{{\partial {N^I}{T_a}}}{{\partial {x_j}}}\frac{{\partial {N^J}{T_b}}}{{\partial t}}d\Omega dt} } } \right] {\delta_{pq}}
$$

In the above two matrices if `C` matrix is defined at integration points then we can use the following subroutine.

```fortran
CALL Obj % getConvectiveMatrix( C, Term1, Term2, CType, nCopy )
```

- In the above call `Term1` and `Term2` are characters. They can be `dt`, `dx, dy, dz`.
- `C` denotes the convective velocity. It can be Rank-1, Rank-2, Rank-3 array. 
- `CType` can be `NodalValues` or `QuadPoints`.

---

In case of the following terms in partial differntial equation

$$
\frac{{\partial {\bf{U}}}}{{\partial t}} + \frac{{\partial {\bf{f}}({\bf{U}})}}{{\partial x}} + \frac{{\partial {\bf{g}}({\bf{U}})}}{{\partial y}} + \frac{{\partial {\bf{h}}({\bf{U}})}}{{\partial z}} +  \cdots
$$

following matrices may appear.The next few interfaces deals with these terms.

#### Type-9

$$
M\left( {I,J,a,b} \right) = \left[ {\int_{{I_n}}^{} {\int_\Omega ^{} {\frac{{\partial {N^I}{T_a}}}{{\partial x}} \cdot {N^J}{T_b}d\Omega dt} } } \right]
$$

$$
M\left( {I,J,a,b} \right) = \left[ {\int_{{I_n}}^{} {\int_\Omega ^{} {\frac{{\partial {N^I}{T_a}}}{{\partial y}} \cdot {N^J}{T_b}d\Omega dt} } } \right]
$$

$$
M\left( {I,J,a,b} \right) = \left[ {\int_{{I_n}}^{} {\int_\Omega ^{} {\frac{{\partial {N^I}{T_a}}}{{\partial z}} \cdot {N^J}{T_b}d\Omega dt} } } \right]
$$

$$
M\left( {I,J,a,b} \right) = \left[ {\int_{{I_n}}^{} {\int_\Omega ^{} {{N^J}{T_b}\frac{{\partial {N^J}{T_b}}}{{\partial x}}d\Omega dt} } } \right]
$$

$$
M\left( {I,J,a,b} \right) = \left[ {\int_{{I_n}}^{} {\int_\Omega ^{} {{N^J}{T_b}\frac{{\partial {N^J}{T_b}}}{{\partial y}}d\Omega dt} } } \right]
$$

$$
M\left( {I,J,a,b} \right) = \left[ {\int_{{I_n}}^{} {\int_\Omega ^{} {{N^J}{T_b}\frac{{\partial {N^J}{T_b}}}{{\partial z}}d\Omega dt} } } \right]
$$

The above six matrices can be computed using the following subroutine.

```fortran
CALL Obj % getConvectiveMatrix( Term1, Term2, XType )
```

- In the above call `Term1` and `Term2` can be 0 or 1. If it is 1 then it denotes the spatial gradient. Both `Term1` and `Term2` cannot be 0 or 1 at the same time.
- `XType` can be `dx`, `dy`, and `dz`.

#### Type-10

$$
{M^{pq}}\left( {I,J,a,b} \right) = \left[ {\int_{{I_n}}^{} {\int_\Omega ^{} {\frac{{\partial {N^I}{T_a}}}{{\partial x}} \cdot {N^J}{T_b}d\Omega dt} } } \right]{\delta _{pq}}
$$

$$
{M^{pq}}\left( {I,J,a,b} \right) = \left[ {\int_{{I_n}}^{} {\int_\Omega ^{} {\frac{{\partial {N^I}{T_a}}}{{\partial y}} \cdot {N^J}{T_b}d\Omega dt} } } \right] {\delta_{pq}}
$$

$$
{M^{pq}}\left( {I,J,a,b} \right) = \left[ {\int_{{I_n}}^{} {\int_\Omega ^{} {\frac{{\partial {N^I}{T_a}}}{{\partial z}} \cdot {N^J}{T_b}d\Omega dt} } } \right] {\delta_{pq}}
$$

$$
{M^{pq}}\left( {I,J,a,b} \right) = \left[ {\int_{{I_n}}^{} {\int_\Omega ^{} {{N^J}{T_b}\frac{{\partial {N^J}{T_b}}}{{\partial x}}d\Omega dt} } } \right] {\delta_{pq}}
$$

$$
{M^{pq}}\left( {I,J,a,b} \right) = \left[ {\int_{{I_n}}^{} {\int_\Omega ^{} {{N^J}{T_b}\frac{{\partial {N^J}{T_b}}}{{\partial y}}d\Omega dt} } } \right] {\delta_{pq}}
$$

$$
{M^{pq}}\left( {I,J,a,b} \right) = \left[ {\int_{{I_n}}^{} {\int_\Omega ^{} {{N^J}{T_b}\frac{{\partial {N^J}{T_b}}}{{\partial z}}d\Omega dt} } } \right] {\delta_{pq}}
$$

The above six matrices can be computed using the following subroutine.

```fortran
CALL Obj % getConvectiveMatrix( Term1, Term2, XType, nCopy )
```

- In the above call `Term1` and `Term2` can be 0 or 1. If it is 1 then it denotes the spatial gradient. Both `Term1` and `Term2` cannot be 0 or 1 at the same time.
- `XType` can be `dx`, `dy`, and `dz`.

#### Type-11

$$
M(I,J,a,b) = \left[ {\int_{{I_n}}^{} {\int_\Omega ^{} {\frac{{\partial {N^I}{T_a}}}{{\partial t}}\frac{{\partial {N^J}{T_b}}}{{\partial x}}d\Omega dt} } } \right]
$$

$$
M(I,J,a,b) = \left[ {\int_{{I_n}}^{} {\int_\Omega ^{} {\frac{{\partial {N^I}{T_a}}}{{\partial t}}\frac{{\partial {N^J}{T_b}}}{{\partial y}}d\Omega dt} } } \right]
$$

$$
M(I,J,a,b) = \left[ {\int_{{I_n}}^{} {\int_\Omega ^{} {\frac{{\partial {N^I}{T_a}}}{{\partial t}}\frac{{\partial {N^J}{T_b}}}{{\partial z}}d\Omega dt} } } \right]
$$

$$
M(I,J,a,b) = \left[ {\int_{{I_n}}^{} {\int_\Omega ^{} {\frac{{\partial {N^I}{T_a}}}{{\partial x}}\frac{{\partial {N^J}{T_b}}}{{\partial t}}d\Omega dt} } } \right]
$$

$$
M(I,J,a,b) = \left[ {\int_{{I_n}}^{} {\int_\Omega ^{} {\frac{{\partial {N^I}{T_a}}}{{\partial y}}\frac{{\partial {N^J}{T_b}}}{{\partial t}}d\Omega dt} } } \right]
$$

$$
M(I,J,a,b) = \left[ {\int_{{I_n}}^{} {\int_\Omega ^{} {\frac{{\partial {N^I}{T_a}}}{{\partial z}}\frac{{\partial {N^J}{T_b}}}{{\partial t}}d\Omega dt} } } \right]
$$

The above six matrices can be computed using the following subroutine.

```fortran
CALL Obj % getConvectiveMatrix( Term1, Term2 )
```

#### Type-12

$$
{M^{pq}}(I,J,a,b) = \left[ {\int_{{I_n}}^{} {\int_\Omega ^{} {\frac{{\partial {N^I}{T_a}}}{{\partial t}}\frac{{\partial {N^J}{T_b}}}{{\partial x}}d\Omega dt} } } \right]{\delta _{pq}}
$$

$$
{M^{pq}}(I,J,a,b) = \left[ {\int_{{I_n}}^{} {\int_\Omega ^{} {\frac{{\partial {N^I}{T_a}}}{{\partial t}}\frac{{\partial {N^J}{T_b}}}{{\partial y}}d\Omega dt} } } \right]{\delta _{pq}}
$$

$$
{M^{pq}}(I,J,a,b) = \left[ {\int_{{I_n}}^{} {\int_\Omega ^{} {\frac{{\partial {N^I}{T_a}}}{{\partial t}}\frac{{\partial {N^J}{T_b}}}{{\partial z}}d\Omega dt} } } \right]{\delta _{pq}}
$$

$$
{M^{pq}}(I,J,a,b) = \left[ {\int_{{I_n}}^{} {\int_\Omega ^{} {\frac{{\partial {N^I}{T_a}}}{{\partial x}}\frac{{\partial {N^J}{T_b}}}{{\partial t}}d\Omega dt} } } \right]{\delta _{pq}}
$$

$$
{M^{pq}}(I,J,a,b) = \left[ {\int_{{I_n}}^{} {\int_\Omega ^{} {\frac{{\partial {N^I}{T_a}}}{{\partial y}}\frac{{\partial {N^J}{T_b}}}{{\partial t}}d\Omega dt} } } \right]{\delta _{pq}}
$$

$$
{M^{pq}}(I,J,a,b) = \left[ {\int_{{I_n}}^{} {\int_\Omega ^{} {\frac{{\partial {N^I}{T_a}}}{{\partial z}}\frac{{\partial {N^J}{T_b}}}{{\partial t}}d\Omega dt} } } \right]{\delta _{pq}}
$$

The above six matrices can be computed using the following subroutine.

```fortran
CALL Obj % getConvectiveMatrix( Term1, Term2, nCopy )
```

---

Now we want to compute the space-time convective finite element matrix for following PDE.

$$
\frac{\partial \textbf{U}}{\partial t} + \mathbf{A_1} \frac{\partial \textbf{U}}{\partial x} + \mathbf{A_2} \frac{\partial \textbf{U}}{\partial y} + \mathbf{A_3} \frac{\partial \textbf{U}}{\partial z} + \cdots
$$

#### Type-13

$$
{M^{pq}}(I,J,a,b) = \left[ {\int_{{I_n}}^{} {\int_\Omega ^{} {{N^I}{T_a}{{[{{\bf{A}}_{\bf{1}}}]}_{pq}}\frac{{\partial {N^J}{T_b}}}{{\partial x}}d\Omega dt} } } \right]
$$

$$
{M^{pq}}(I,J,a,b) = \left[ {\int_{{I_n}}^{} {\int_\Omega ^{} {{{[{{\bf{A}}_{\bf{1}}}]}_{qp}}\frac{{\partial {N^I}{T_a}}}{{\partial x}}{N^J}{T_b}d\Omega dt} } } \right]
$$

$$
{M^{pq}}(I,J,a,b) = \left[ {\int_{{I_n}}^{} {\int_\Omega ^{} {{N^I}{T_a}{{[{{\bf{A}}_2}]}_{pq}}\frac{{\partial {N^J}{T_b}}}{{\partial y}}d\Omega dt} } } \right]
$$

$$
{M^{pq}}(I,J,a,b) = \left[ {\int_{{I_n}}^{} {\int_\Omega ^{} {{{[{{\bf{A}}_{\bf{1}}}]}_{qp}}\frac{{\partial {N^I}{T_a}}}{{\partial y}}{N^J}{T_b}d\Omega dt} } } \right]
$$

$$
{M^{pq}}(I,J,a,b) = \left[ {\int_{{I_n}}^{} {\int_\Omega ^{} {{N^I}{T_a}{{[{{\bf{A}}_2}]}_{pq}}\frac{{\partial {N^J}{T_b}}}{{\partial z}}d\Omega dt} } } \right]
$$

$$
{M^{pq}}(I,J,a,b) = \left[ {\int_{{I_n}}^{} {\int_\Omega ^{} {{{[{{\bf{A}}_{\bf{1}}}]}_{qp}}\frac{{\partial {N^I}{T_a}}}{{\partial z}}{N^J}{T_b}d\Omega dt} } } \right]
$$

The above matrices can be computed using the following subroutine.

```fortran
CALL Obj % getConvectiveMatrix( A, Term1, Term2, Xtype, MultiVar )
```

- In the above call `A`  can be Rank-4 `A(:,:,:,:)`,  Rank-3 `A(:,:,:)` or Rank-2 `A(:,:)` fortran array.
- `Term-1` and `Term-2` are integers which can be 1 or 0.
- `XType` is character and it can be `dx, dy, dz`.
- `MultiVar` has no effect it is just for interface uniqness.

#### Type-14

$$
{M^{pq}}(I,J,a,b) = \left[ {\int_{{I_n}}^{} {\int_\Omega ^{} {\frac{{\partial {N^I}{T_a}}}{{\partial t}}{{[{{\bf{A}}_{\bf{1}}}]}_{pq}}\frac{{\partial {N^J}{T_b}}}{{\partial x}}d\Omega dt} } } \right]
$$

$$
{M^{pq}}(I,J,a,b) = \left[ {\int_{{I_n}}^{} {\int_\Omega ^{} {{{[{{\bf{A}}_{\bf{1}}}]}_{qp}}\frac{{\partial {N^I}{T_a}}}{{\partial x}}\frac{{\partial {N^J}{T_b}}}{{\partial t}}d\Omega dt} } } \right]
$$

$$
{M^{pq}}(I,J,a,b) = \left[ {\int_{{I_n}}^{} {\int_\Omega ^{} {\frac{{\partial {N^I}{T_a}}}{{\partial t}}{{[{{\bf{A}}_2}]}_{pq}}\frac{{\partial {N^J}{T_b}}}{{\partial y}}d\Omega dt} } } \right]
$$

$$
{M^{pq}}(I,J,a,b) = \left[ {\int_{{I_n}}^{} {\int_\Omega ^{} {{{[{{\bf{A}}_{\bf{1}}}]}_{qp}}\frac{{\partial {N^I}{T_a}}}{{\partial y}}\frac{{\partial {N^J}{T_b}}}{{\partial t}}d\Omega dt} } } \right]
$$

$$
{M^{pq}}(I,J,a,b) = \left[ {\int_{{I_n}}^{} {\int_\Omega ^{} {\frac{{\partial {N^I}{T_a}}}{{\partial t}}{{[{{\bf{A}}_2}]}_{pq}}\frac{{\partial {N^J}{T_b}}}{{\partial z}}d\Omega dt} } } \right]
$$

$$
{M^{pq}}(I,J,a,b) = \left[ {\int_{{I_n}}^{} {\int_\Omega ^{} {{{[{{\bf{A}}_{\bf{1}}}]}_{qp}}\frac{{\partial {N^I}{T_a}}}{{\partial z}}\frac{{\partial {N^J}{T_b}}}{{\partial t}}d\Omega dt} } } \right]
$$

The above matrices can be computed using the following subrouine.

```fortran
CALL Obj % getConvectiveMatrix( A, Term1, Term2, MultiVar )
```

- In above case `A` can be rank-4 `A(:,:,:,:)`, rank-3 `A( :, :, : )` or rank-2 `A(:,:)`. 
- In the above case `Term1` and `Term2` are characters, and it can be `dt, dx, dy, dz`.

---

```fortran
CALL Obj % getConvectiveMatrix( A, A0, Term1, Term2, MultiVar ) 
```

In the above case `A` and `A0` can be Rank-4, Rank-3, Rank-2. Term1 and Term2 are `dt, dx, dy, dz`.








































## Code used for testing the methods

```fortran
PROGRAM MAIN
USE GlobalData
USE IO
USE ElementData_Class
USE STElement_Class
USE STSlab_Classx
USE STElemShapeData_Class
USE STConvectiveMatrix_Class

CLASS( STElement_ ), POINTER :: Elem
CLASS( ElementData_ ), POINTER :: Data
CLASS( STSlab_ ), POINTER :: STSlabs
CLASS( STElemShapeData_ ), POINTER :: STElemSD
INTEGER( I4B ) :: Int1, NIPS, NIPT, IPS, IPT, I, a, b, J, &
                  NNS, NSD, NNT, XiDimension

INTEGER( I4B ), ALLOCATABLE :: Nptrs( : ), Mat4Shape( : )

REAL ( DFP ), ALLOCATABLE :: DummyMat2(:,:), TimeVec( : ), &
DummyVec( : ), DummyMat3( :, :, : ), DummyMat4( :, :, :, :)

! MAKING SPACE-TIME ELEMENT

NSD = 2; XiDimension = 2; NNS = 4; NNT = 2
WRITE( *, * ) "Making Space-Time Element"
CALL BlankLines(  )

Data => ElementData( )
CALL Data % setNSD( NSD )
CALL Data % setNNE( NNS )
CALL Data % setNNS( NNS )
CALL Data % setNNT( NNT )
CALL Data % setMatType( 1 )
CALL Data % setElemTopology( "Quad4")
CALL Data % setSpaceElemTopology( "Quad4" )
CALL Data % setTimeElemTopology( "Line2" )
CALL Data % setElemType( "SpaceTimeContinuum" )
CALL Data % setXiDimension( 2 )

Nptrs = (/1,2,3,4/)

Elem => STElement( Nptrs, Data )
CALL BlankLines( )
CALL Elem % Display( )


! MAKING SPACE-TIME SLAB

STSlabs => STSLAB( tSTSlabs = 2, tNodes = (/4,4/))

IF( ALLOCATED( DummyMat2 ) ) DEALLOCATE( DummyMat2 )
ALLOCATE( DummyMat2( 2, 4 ) )

DummyMat2 = ReSHAPE( [-1.0, -1.0, 1.0, &
                        -1.0, 1.0, 1.0, -1.0, 1.0],[2,4])

CALL STSlabs % addNodes( Val = DummyMat2, STSlabNum = 1, &
NodeNum =(/1,2,3,4/) )

CALL STSlabs % addNodes( Val = DummyMat2, STSlabNum = 2, &
NodeNum = [1,2,3,4] )

! MAKING ST-CONVECTIVE-MATRIX

ALLOCATE( TimeVec( 2 ) )
TimeVec = [-1.0, 1.0]

ALLOCATE( STConvectiveMatrix_ :: STElemSD )

! Ask for NIPS and NIPT

WRITE( *, "(A)" ) "TESTING SPACE-TIME CONVECTIVE MATRIX :: "
CALL BlankLines( )

WRITE( *, "(A)") "ENTER NIPS :: "
READ( *, * ) NIPS

WRITE( *, "(A)") "ENTER NIPT :: "
READ( *, * ) NIPT

!NIPS = 4; NIPT = 2

CALL Elem % getSTElemShapeData( STElemSD_Obj = STElemSD,&
TimeVec = TimeVec, NIPS = NIPS, NIPT = NIPT, STSlab_Obj = STSlabs )
```

## Structure

## Theory

Now we want to compute the space-time convective finite element matrix for following PDE. 

$$\frac{\partial u_i}{\partial t} + c_k \frac{\partial u_i}{\partial x_k} + \cdots $$

We would like to compute the following matrices.

$${}^{4}M(I,J,a,b) = {}^{a}\delta u_{iI} \quad \int_{Q_n} N^I T_a c_k \frac{\partial N^J T_b}{\partial x_k} {dQ} \quad {}^{b}u_{iJ}$$

$${}^{4}M(I,J,a,b) = {}^{a}\delta u_{iI} \quad \int_{Q_n} c_k \frac{\partial N^I T_a}{\partial x_k} N^J T_b {dQ} \quad {}^{b}u_{iJ}$$

> These tasks are performed by following methods; `getConvectiveMatrix_1`, `getConvectiveMatrix_2`, `getConvectiveMatrix_3`, `getConvectiveMatrix_4`, `getConvectiveMatrix_5`, `getConvectiveMatrix_6`, `getConvectiveMatrix_7`, `getConvectiveMatrix_8`, and `getConvectiveMatrix_28`.

Now we want to compute the space-time convective finite element matrix for following PDE. 

$$\frac{\partial \textbf{U}}{\partial t} + \frac{\partial \textbf{f(U)}}{\partial x} + \frac{\partial \textbf{g(U)}}{\partial y} + \frac{\partial \textbf{h(U)}}{\partial z} + \cdots $$

where $\textbf{U}, \textbf{f}, \textbf{g}, \textbf{h}  \in R^m$. In this case we wish to compute the following matrices.

$${}^{4}M(I,J,a,b) = \delta {}^{a} U_{iI} \quad \int_{Q_n} N^I T_a \frac{\partial N^J T_b}{\partial x} {dQ} \quad {}^{b}f_{iJ}$$

$${}^{4}M(I,J,a,b) = \delta {}^{a} U_{iI} \quad \int_{Q_n} \frac{\partial N^I T_a}{\partial x} N^J T_b {dQ}  \quad {}^{b}f_{iJ}$$

$${}^{4}M(I,J,a,b) = \delta {}^{a} U_{iI} \quad \int_{Q_n} N^I T_a \frac{\partial N^J T_b}{\partial x} {dQ} \quad {}^{b}g_{iJ}$$

$${}^{4}M(I,J,a,b) = \delta {}^{a} U_{iI} \quad \int_{Q_n} \frac{\partial N^I T_a}{\partial x} N^J T_b {dQ}  \quad {}^{b}g_{iJ}$$


$${}^{4}M(I,J,a,b) = \delta {}^{a} U_{iI} \quad \int_{Q_n} N^I T_a \frac{\partial N^J T_b}{\partial x} {dQ} \quad {}^{b}h_{iJ}$$

$${}^{4}M(I,J,a,b) = \delta {}^{a} U_{iI} \quad \int_{Q_n} \frac{\partial N^I T_a}{\partial x} N^J T_b {dQ}  \quad {}^{b}h_{iJ}$$

> These tasks are performed by following methods; `getConvectiveMatrix_9`, `getConvectiveMatrix_10`.

Now we want to compute the space-time convective finite element matrix for following PDE. 

$$\frac{\partial u_i}{\partial t} + c_k \frac{\partial u_i}{\partial x_k} + \cdots $$

We would like to compute the following matrices.

$${}^{4}M(I,J,a,b) = \delta {}^{a}u_{iI} \int_{Q_n} \frac{ \partial N^I T_a}{\partial t} c_{k}^{h} \frac{\partial N^J T_b}{\partial x_k} {dQ} \quad {}^{b}u_{iJ}$$

$${}^{4}M(I,J,a,b) = \delta {}^{a}u_{iI} \int_{Q_n} c_{k}^{h} \frac{\partial N^I T_a}{\partial x_k} \frac{\partial N^J T_b}{\partial t} {dQ} \quad {}^{b}u_{iJ}$$

> These tasks are performed by methods `getConvectiveMatrix_11()`, `getConvectiveMatrix_12()`, `getConvectiveMatrix_13()`, `getConvectiveMatrix_14()`, `getConvectiveMatrix_15()`, `getConvectiveMatrix_16()`, `getConvectiveMatrix_17()`, `getConvectiveMatrix_18()`,  `getConvectiveMatrix_27()`, and `getConvectiveMatrix_28()`.

Now we want to compute the space-time convective finite element matrix for following PDE. 

$$\frac{\partial \textbf{U}}{\partial t} + \frac{\partial \textbf{f(U)}}{\partial x} + \frac{\partial \textbf{g(U)}}{\partial y} + \frac{\partial \textbf{h(U)}}{\partial z} + \cdots $$

where $\textbf{U}, \textbf{f}, \textbf{g}, \textbf{h}  \in R^m$. In this case we wish to compute the following matrices.

$${}^{4}M(I,J,a,b) = {}^{a}\delta U_{iI} \quad \int_{Q_n} \frac{\partial N^I T_a}{\partial t} \frac{\partial N^J T_b}{\partial x} {dQ} \quad {}^{b}f_{iJ}$$

$${}^{4}M(I,J,a,b) = {}^{a}\delta U_{iI} \quad \int_{Q_n} \frac{\partial N^I T_a}{\partial x} \frac{\partial N^J T_b}{\partial t} {dQ}  \quad {}^{b}f_{iJ}$$

$${}^{4}M(I,J,a,b) = {}^{a}\delta U_{iI} \quad \int_{Q_n} \frac{\partial N^I T_a}{\partial t} \frac{\partial N^J T_b}{\partial y} {dQ} \quad {}^{b}g_{iJ}$$

$${}^{4}M(I,J,a,b) = {}^{a}\delta U_{iI} \quad \int_{Q_n} \frac{\partial N^I T_a}{\partial y} \frac{\partial N^J T_b}{\partial t} {dQ}  \quad {}^{b}g_{iJ}$$


$${}^{4}M(I,J,a,b) = {}^{a}\delta U_{iI} \quad \int_{Q_n} \frac{\partial N^I T_a}{\partial t} \frac{\partial N^J T_b}{\partial z} {dQ} \quad {}^{b}h_{iJ}$$

$${}^{4}M(I,J,a,b) = {}^{a}\delta U_{iI} \quad \int_{Q_n} \frac{\partial N^I T_a}{\partial z} \frac{\partial N^J T_b}{\partial t} {dQ}  \quad {}^{b}h_{iJ}$$

> These tasks are performed by `getConvectiveMatrix_22()` and `getConvectiveMatrix_23()`

Now we want to compute the space-time convective finite element matrix for following PDE. 

$$\frac{\partial \textbf{U}}{\partial t} + \mathbf{A_1} \frac{\partial \textbf{U}}{\partial x} + \mathbf{A_2} \frac{\partial \textbf{U}}{\partial y} + \mathbf{A_3} \frac{\partial \textbf{U}}{\partial z} + \cdots$$

where $\textbf{U} \in R^m$, $\mathbf{A_i} \in R^{m \times m}$. In this case we wish to compute the following matrices.

$${}^{4}M(I,J,a,b) = {}^{a}\delta U_{iI} \quad \int_{Q_n} N^I T_a [ \mathbf{A_1} ]_{ij} \frac{\partial N^J T_b}{\partial x} {dQ} \quad {}^{b}f_{jJ}$$

$${}^{4}M(I,J,a,b) = {}^{a}\delta U_{iI} \quad \int_{Q_n} [ \mathbf{A_1} ]_{ji} \frac{\partial N^I T_a}{\partial x} N^J T_b {dQ}  \quad {}^{b}f_{jJ}$$

$${}^{4}M(I,J,a,b) = {}^{a}\delta U_{iI} \quad \int_{Q_n} N^I T_a [\mathbf{A_2}]_{ij} \frac{\partial N^J T_b}{\partial y} {dQ} \quad {}^{b}g_{jJ}$$

$${}^{4}M(I,J,a,b) = {}^{a}\delta U_{iI} \quad \int_{Q_n} [\mathbf{A_2}]_{ji} \frac{\partial N^I T_a}{\partial y} N^J T_b {dQ}  \quad {}^{b}g_{jJ}$$


$${}^{4}M(I,J,a,b) = {}^{a}\delta U_{iI} \quad \int_{Q_n} N^I T_a [\mathbf{A_3}]_{ij} \frac{\partial N^J T_b}{\partial z} {dQ} \quad {}^{b}h_{jJ}$$

$${}^{4}M(I,J,a,b) = {}^{a}\delta U_{iI} \quad \int_{Q_n} [\mathbf{A_3}]_{ji} \frac{\partial N^I T_a}{\partial z} N^J T_b {dQ}  \quad {}^{b}h_{jJ}$$

The shape of each ${}^{4}M(:,:,a,b)$ is $(N_{NS} \times m, N_{NS} \times m)$. In this case there will be coupling between different components of $\mathbf{U}$. This coupling is due to $\mathbf{A_i}$. The structure of any of the above ${}^{4}\mathbf{M}$ is given as

$${}^{4}\mathbf{M}(:,:,a,b) =
\begin{bmatrix}
\mathbf{M_{11}} & \cdots & \mathbf{M_{1m}} \\  
\vdots          & \ddots & \vdots \\  
\mathbf{M_{m1}} & \cdots & \mathbf{M_{mm}} \\  
\end{bmatrix}$$

Each $\mathbf{M_{ij}}$ has shape $(N_{ns} \times N_{ns})$.

> These tasks are performed by methods `getConvectiveMatrix_19` to `getConvectiveMatrix_21`

Now we want to compute the space-time convective finite element matrix for following PDE. 

$$\frac{\partial \textbf{U}}{\partial t} + \mathbf{A_1} \frac{\partial \textbf{U}}{\partial x} + \mathbf{A_2} \frac{\partial \textbf{U}}{\partial y} + \mathbf{A_3} \frac{\partial \textbf{U}}{\partial z} + \cdots $$

where $\textbf{U} \in R^m$, $\mathbf{A_i} \in R^{m \times m}$. In this case we wish to compute the following matrices.

$${}^{4}M(I,J,a,b) = {}^{a}\delta U_{iI} \quad \int_{Q_n} \frac{\partial N^I T_a}{\partial t} [ \mathbf{A_1} ]_{ij} \frac{\partial N^J T_b}{\partial x} {dQ} \quad {}^{b}f_{jJ}$$

$${}^{4}M(I,J,a,b) = {}^{a}\delta U_{iI} \quad \int_{Q_n} [ \mathbf{A_1} ]_{ji} \frac{\partial N^I T_a}{\partial x} \frac{\partial N^J T_b}{\partial t}  {dQ}  \quad {}^{b}f_{jJ}$$

$${}^{4}M(I,J,a,b) = {}^{a}\delta U_{iI} \quad \int_{Q_n} \frac{\partial N^I T_a}{\partial t}  [\mathbf{A_2}]_{ij} \frac{\partial N^J T_b}{\partial y} {dQ} \quad {}^{b}g_{jJ}$$

$${}^{4}M(I,J,a,b) = {}^{a}\delta U_{iI} \quad \int_{Q_n} [\mathbf{A_2}]_{ji} \frac{\partial N^I T_a}{\partial y} \frac{\partial N^J T_b}{\partial t}  {dQ}  \quad {}^{b}g_{jJ}$$


$${}^{4}M(I,J,a,b) = {}^{a}\delta U_{iI} \quad \int_{Q_n} \frac{\partial N^I T_a}{\partial t}  [\mathbf{A_3}]_{ij} \frac{\partial N^J T_b}{\partial z} {dQ} \quad {}^{b}h_{jJ}$$

$${}^{4}M(I,J,a,b) = {}^{a}\delta U_{iI} \quad \int_{Q_n} [\mathbf{A_3}]_{ji} \frac{\partial N^I T_a}{\partial z} \frac{\partial N^J T_b}{\partial t}  {dQ}  \quad {}^{b}h_{jJ}$$

The shape of each ${}^{4}M(:,:,a,b)$ is $(N_{NS} \times m, N_{NS} \times m)$. In this case there will be coupling between different components of $\mathbf{U}$. This coupling is due to $\mathbf{A_i}$. The structure of any of the above ${}^{4}\mathbf{M}$ is given as

$${}^{4}\mathbf{M}(:,:,a,b) =
\begin{bmatrix}
\mathbf{M_{11}} & \cdots & \mathbf{M_{1m}} \\
\vdots          & \ddots & \vdots \\
\mathbf{M_{m1}} & \cdots & \mathbf{M_{mm}} \\
\end{bmatrix}$$

Each $\mathbf{M_{ij}}$ has shape $(N_{ns} \times N_{ns})$.

> These tasks are performed by methods `getConvectiveMatrix_24()`, `getConvectiveMatrix_25()`, `getConvectiveMatrix_26()`.

Now consider the following terms in a pde.

$$\mathbf{A_0} \frac{\partial U}{\partial t} + \mathbf{A_1} \frac{\partial U}{\partial x} + \mathbf{A_2} \frac{\partial \mathbf{U}}{\partial y} + \mathbf{A_3} \frac{\partial U}{\partial t} + \cdots$$

$${}^{4}M(I,J,a,b) = {}^{a}\delta U_{iI} \quad \int_{Q_n} [ \mathbf{A_0}]_{ki} \frac{\partial N^I T_a}{\partial t} [ \mathbf{A_1} ]_{kj} \frac{\partial N^J T_b}{\partial x} {dQ} \quad {}^{b}f_{jJ}$$

$${}^{4}M(I,J,a,b) = {}^{a}\delta U_{iI} \quad \int_{Q_n} [ \mathbf{A_1} ]_{ki} \frac{\partial N^I T_a}{\partial x} [\mathbf{A_0}]_{kj} \frac{\partial N^J T_b}{\partial t}  {dQ}  \quad {}^{b}f_{jJ}$$

$${}^{4}M(I,J,a,b) = {}^{a}\delta U_{iI} \quad \int_{Q_n} [ \mathbf{A_0}]_{ki}\frac{\partial N^I T_a}{\partial t}  [\mathbf{A_2}]_{kj} \frac{\partial N^J T_b}{\partial y} {dQ} \quad {}^{b}g_{jJ}$$

$${}^{4}M(I,J,a,b) = {}^{a}\delta U_{iI} \quad \int_{Q_n} [\mathbf{A_2}]_{ki} \frac{\partial N^I T_a}{\partial y} [ \mathbf{A_0}]_{kj} \frac{\partial N^J T_b}{\partial t}  {dQ}  \quad {}^{b}g_{jJ}$$


$${}^{4}M(I,J,a,b) = {}^{a}\delta U_{iI} \quad \int_{Q_n} [ \mathbf{A_0}]_{ki}\frac{\partial N^I T_a}{\partial t}  [\mathbf{A_3}]_{kj} \frac{\partial N^J T_b}{\partial z} {dQ} \quad {}^{b}h_{jJ}$$

$${}^{4}M(I,J,a,b) = {}^{a}\delta U_{iI} \quad \int_{Q_n} [\mathbf{A_3}]_{ki} \frac{\partial N^I T_a}{\partial z} [ \mathbf{A_0}]_{kj} \frac{\partial N^J T_b}{\partial t}  {dQ}  \quad {}^{b}h_{jJ}$$



## Methods

### getConvectiveMatrix_1()

INTERFACE

```fortran
 SUBROUTINE getConvectiveMatrix_1( Obj, C, Term1, Term2 )

    USE Utility, ONLY : OUTERPROD

    CLASS( STConvectiveMatrix_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), DIMENSION( :, :, : ), INTENT( IN ) :: C
    INTEGER( I4B ), INTENT( IN ) :: Term1, Term2
```

DESCRIPTION

- `C(:,:,:)` is a three dimension array. It represents the space-time nodal values of convective velocity $c(x,t)$. The shape of `C` is `(NSD, NNS, NNT)`. The first index of `C` denotes the spatial coordinate. The second index of `C` denotes the spatial-node. The third index of `C` denotes the temporal node. In this case `C` varies with both space-time.

- `Term1` and `Term2` are integers that can take values 0 and 1. They represents the spatial derivative. If it is zero then this means no spatial derivative. If it is 1 then it means first order spatial derivative.

SYMBOLIC CALCULATION

$${}^{4}M(I,J,a,b) = {}^{a}\delta u_{iI} \quad \int_{Q_n} N^I T_a c_k \frac{\partial N^J T_b}{\partial x_k} {dQ} \quad {}^{b}u_{iJ}$$

$${}^{4}M(I,J,a,b) = {}^{a}\delta u_{iI} \quad \int_{Q_n} c_k \frac{\partial N^I T_a}{\partial x_k} N^J T_b {dQ} \quad {}^{b}u_{iJ}$$

TESTING

```fortran
IF( ALLOCATED( DummyMat3 ) ) DEALLOCATE( DummyMat3 ) 
ALLOCATE( DummyMat3( NSD, NNS, NNT ) )
DummyMat3 = 1.0_DFP
    
CALL STElemSD % getConvectiveMatrix( C = DummyMat3, Term1 = 0, Term2 = 1 )
CALL STElemSD % DISPLAYMATRIX4( )
```

**`NIPS = 4`, `NIPT = 1`**

```fortran

CALL STElemSD % getConvectiveMatrix_1( C, Term1 = 0, Term2 = 1 )

MATRIX STORED IN ST-ELEMENT-SHAPEDATA

NIPS ::   4  NIPT ::   1

-------------------------------------------------

4D MATRIX, MAT4(:,:,:,:) ::

Mat4( :, :,  1, 1 )

 -0.3333333      0.8333333E-01  0.1666667      0.8333333E-01
 -0.2500000       0.000000      0.2500000       0.000000
 -0.1666667     -0.8333333E-01  0.3333333     -0.8333333E-01
 -0.2500000       0.000000      0.2500000       0.000000

Mat4( :, :,  1, 2 )

 -0.3333333      0.8333333E-01  0.1666667      0.8333333E-01
 -0.2500000       0.000000      0.2500000       0.000000
 -0.1666667     -0.8333333E-01  0.3333333     -0.8333333E-01
 -0.2500000       0.000000      0.2500000       0.000000

Mat4( :, :,  2, 1 )

 -0.3333333      0.8333333E-01  0.1666667      0.8333333E-01
 -0.2500000       0.000000      0.2500000       0.000000
 -0.1666667     -0.8333333E-01  0.3333333     -0.8333333E-01
 -0.2500000       0.000000      0.2500000       0.000000

Mat4( :, :,  2, 2 )

 -0.3333333      0.8333333E-01  0.1666667      0.8333333E-01
 -0.2500000       0.000000      0.2500000       0.000000
 -0.1666667     -0.8333333E-01  0.3333333     -0.8333333E-01
 -0.2500000       0.000000      0.2500000       0.000000
```

**`NIPS = 4`, `NIPT = 2`**

```fortran

CALL STElemSD % getConvectiveMatrix_1( C, Term1 = 0, Term2 = 1 )

MATRIX STORED IN ST-ELEMENT-SHAPEDATA

NIPS ::   4  NIPT ::   2

-------------------------------------------------

4D MATRIX, MAT4(:,:,:,:) ::

Mat4( :, :,  1, 1 )

 -0.4444444      0.1111111      0.2222222      0.1111111
 -0.3333333       0.000000      0.3333333       0.000000
 -0.2222222     -0.1111111      0.4444444     -0.1111111
 -0.3333333       0.000000      0.3333333       0.000000

Mat4( :, :,  1, 2 )

 -0.2222222      0.5555556E-01  0.1111111      0.5555556E-01
 -0.1666667       0.000000      0.1666667       0.000000
 -0.1111111     -0.5555556E-01  0.2222222     -0.5555556E-01
 -0.1666667       0.000000      0.1666667       0.000000

Mat4( :, :,  2, 1 )

 -0.2222222      0.5555556E-01  0.1111111      0.5555556E-01
 -0.1666667       0.000000      0.1666667       0.000000
 -0.1111111     -0.5555556E-01  0.2222222     -0.5555556E-01
 -0.1666667       0.000000      0.1666667       0.000000

Mat4( :, :,  2, 2 )

 -0.4444444      0.1111111      0.2222222      0.1111111
 -0.3333333       0.000000      0.3333333       0.000000
 -0.2222222     -0.1111111      0.4444444     -0.1111111
 -0.3333333       0.000000      0.3333333       0.000000
```

> As expected `NIPS = 4, NIPT = 1` is not sufficient for integration as the integrand is quadratic in time. Therefore we need atleast `NIPT = 2`. Note that this may be different incase mesh is moving, then additional time dependent terms may appear in the integrand.

### getConvectiveMatrix_2()

INTERFACE

```fortran
 SUBROUTINE getConvectiveMatrix_2( Obj, C, Term1, Term2 )

    USE Utility, ONLY : OUTERPROD

    CLASS( STConvectiveMatrix_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), DIMENSION( :, : ), INTENT( IN ) :: C
    INTEGER( I4B ), INTENT( IN ) :: Term1, Term2
```

DESCRIPTION

- `C(:,:)` is a two dimension array. It represents the spatial nodal values of convective velocity $c(x,t)$. The shape of `C` is `(NSD, NNS)`. The first index of `C` denotes the spatial coordinate. The second index of `C` denotes the spatial-node. In this case, `C` varies in space but remains constant in time.

- `Term1` and `Term2` are integers that can take values 0 and 1. They represents the spatial derivative. If it is zero then this means no spatial derivative. If it is 1 then it means first order spatial derivative.

SYMBOLIC CALCULATION

$${}^{4}M(I,J,a,b) = {}^{a}\delta u_{iI} \quad \int_{Q_n} N^I T_a c_k \frac{\partial N^J T_b}{\partial x_k} {dQ} \quad {}^{b}u_{iJ}$$

$${}^{4}M(I,J,a,b) = {}^{a}\delta u_{iI} \quad \int_{Q_n} c_k \frac{\partial N^I T_a}{\partial x_k} N^J T_b {dQ} \quad {}^{b}u_{iJ}$$


TESTING

```fortran
IF( ALLOCATED( DummyMat2 ) ) DEALLOCATE( DummyMat2 ) 
ALLOCATE( DummyMat2( NSD, NNS ) )  
DummyMat2 = 1.0_DFP
    
CALL STElemSD % getConvectiveMatrix( C = DummyMat2, Term1 = 0, Term2 = 1 )
CALL STElemSD % DISPLAYMATRIX4( )
```

**`NIPS = 4`, `NIPT = 2`**

```fortran
CALL STElemSD % getConvectiveMatrix_1( C = DummyMat2, Term1 = 0, Term2 = 1 )

MATRIX STORED IN ST-ELEMENT-SHAPEDATA

NIPS ::   4  NIPT ::   2

-------------------------------------------------

4D MATRIX, MAT4(:,:,:,:) ::

Mat4( :, :,  1, 1 )

 -0.4444444      0.1111111      0.2222222      0.1111111
 -0.3333333       0.000000      0.3333333       0.000000
 -0.2222222     -0.1111111      0.4444444     -0.1111111
 -0.3333333       0.000000      0.3333333       0.000000

Mat4( :, :,  1, 2 )

 -0.2222222      0.5555556E-01  0.1111111      0.5555556E-01
 -0.1666667       0.000000      0.1666667       0.000000
 -0.1111111     -0.5555556E-01  0.2222222     -0.5555556E-01
 -0.1666667       0.000000      0.1666667       0.000000

Mat4( :, :,  2, 1 )

 -0.2222222      0.5555556E-01  0.1111111      0.5555556E-01
 -0.1666667       0.000000      0.1666667       0.000000
 -0.1111111     -0.5555556E-01  0.2222222     -0.5555556E-01
 -0.1666667       0.000000      0.1666667       0.000000

Mat4( :, :,  2, 2 )

 -0.4444444      0.1111111      0.2222222      0.1111111
 -0.3333333       0.000000      0.3333333       0.000000
 -0.2222222     -0.1111111      0.4444444     -0.1111111
 -0.3333333       0.000000      0.3333333       0.000000
```

### getConvectiveMatrix_29()

INTERFACE

```fortran
 SUBROUTINE getConvectiveMatrix_29( Obj, C, Term1, Term2 )

    USE Utility, ONLY : OUTERPROD

    CLASS( STConvectiveMatrix_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), DIMENSION( : ), INTENT( IN ) :: C
    INTEGER( I4B ), INTENT( IN ) :: Term1, Term2
```

DESCRIPTION

- `C(:)` is a vector array. It represents the spatial components of the convective velocity $c(x,t)$. The shape of `C` is `(NSD)`. The first index of `C` denotes the spatial coordinate. In this case, `C` remains constant in both space and time domain.

- `Term1` and `Term2` are integers that can take values 0 and 1. They represents the spatial derivative. If it is zero then this means no spatial derivative. If it is 1 then it means first order spatial derivative.

SYMBOLIC CALCULATION

$${}^{4}M(I,J,a,b) = {}^{a}\delta u_{iI} \quad \int_{Q_n} N^I T_a c_k \frac{\partial N^J T_b}{\partial x_k} {dQ} \quad {}^{b}u_{iJ}$$

$${}^{4}M(I,J,a,b) = {}^{a}\delta u_{iI} \quad \int_{Q_n} c_k \frac{\partial N^I T_a}{\partial x_k} N^J T_b {dQ} \quad {}^{b}u_{iJ}$$


TESTING

```fortran
IF( ALLOCATED( DummyVec ) ) DEALLOCATE( DummyVec ) 
ALLOCATE( DummyVec( NSD ) )
    
DummyVec = 1.0_DFP
    
CALL STElemSD % getConvectiveMatrix( C = DummyVec, Term1 = 0, Term2 = 1 )

CALL STElemSD % DISPLAYMATRIX4( )
```

**`NIPS = 4`, `NIPT = 2`**

```fortran

CALL STElemSD % getConvectiveMatrix_28( C = DummyMat2, Term1 = 0, Term2 = 1 )

MATRIX STORED IN ST-ELEMENT-SHAPEDATA

NIPS ::   4  NIPT ::   2

-------------------------------------------------

4D MATRIX, MAT4(:,:,:,:) ::

Mat4( :, :,  1, 1 )

 -0.4444444      0.1111111      0.2222222      0.1111111
 -0.3333333       0.000000      0.3333333       0.000000
 -0.2222222     -0.1111111      0.4444444     -0.1111111
 -0.3333333       0.000000      0.3333333       0.000000

Mat4( :, :,  1, 2 )

 -0.2222222      0.5555556E-01  0.1111111      0.5555556E-01
 -0.1666667       0.000000      0.1666667       0.000000
 -0.1111111     -0.5555556E-01  0.2222222     -0.5555556E-01
 -0.1666667       0.000000      0.1666667       0.000000

Mat4( :, :,  2, 1 )

 -0.2222222      0.5555556E-01  0.1111111      0.5555556E-01
 -0.1666667       0.000000      0.1666667       0.000000
 -0.1111111     -0.5555556E-01  0.2222222     -0.5555556E-01
 -0.1666667       0.000000      0.1666667       0.000000

Mat4( :, :,  2, 2 )

 -0.4444444      0.1111111      0.2222222      0.1111111
 -0.3333333       0.000000      0.3333333       0.000000
 -0.2222222     -0.1111111      0.4444444     -0.1111111
 -0.3333333       0.000000      0.3333333       0.000000
 ```
 
### getConvectiveMatrix_3()

INTERFACE

```fortran
 SUBROUTINE getConvectiveMatrix_3( Obj, C, Term1, Term2, Ctype )

    USE Utility, ONLY : OUTERPROD

    CLASS( STConvectiveMatrix_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), DIMENSION( :, : ), INTENT( IN ) :: C
    INTEGER( I4B ), INTENT( IN ) :: Term1, Term2
    CHARACTER( LEN = * ), INTENT( IN ) :: CType
```

DESCRIPTION

- `C(:,:)` is a two dimension array. 
    - If `Ctype` is in the set `[Nodal, NodalValues, Nodal Values, Space Nodal Values, SpaceNodalValues]` then `C` denotes the spatial nodal values, and the shape of `C` will be `(NSD, NNS)`. In this case  first index of `C` denotes the spatial-coordinate. The second index denotes the spatial-node. 
    - If `Ctype` is in the set `[Integration, IntegrationPoints, Integration Points, Quad, QuadPoints, Quad Points]` then `C` denotes the value of _convective velocity_ at spatial-integration points(quadrature points). In this case, the shape of `C` is `(NSD, NIPS)`. The first index of `C` denotes the spatial coordinate. The second index of `C` denotes the spatial-integration point. 
    - In this method, `C` varies only in spatial dimension and remains constant in the temporal domain.
- `Term1` and `Term2` are integers that can take values 0 and 1. They represents the spatial derivative. If it is zero then this means no spatial derivative. If it is 1 then it means first order spatial derivative.

SYMBOLIC CALCULATION

$${}^{4}M(I,J,a,b) = {}^{a}\delta u_{iI} \quad \int_{Q_n} N^I T_a c_k \frac{\partial N^J T_b}{\partial x_k} {dQ} \quad {}^{b}u_{iJ}$$

$${}^{4}M(I,J,a,b) = {}^{a}\delta u_{iI} \quad \int_{Q_n} c_k \frac{\partial N^I T_a}{\partial x_k} N^J T_b {dQ} \quad {}^{b}u_{iJ}$$

TESTING

```fortran
IF( ALLOCATED( DummyMat2 ) ) DEALLOCATE( DummyMat2 ) 
ALLOCATE( DummyMat2( NSD, NIPS ) )
    
DummyMat2 = 1.0_DFP
    
CALL STElemSD % getConvectiveMatrix( C = DummyMat2, Term1 = 0, Term2 = 1, CType = "Quad" )
CALL STElemSD % DISPLAYMATRIX4( )
```

**`NIPS = 4`, `NIPT = 2`**

```fortran

CALL STElemSD % getConvectiveMatrix_3( C = DummyMat2, Term1 = 0, Term2 = 1, Ctype = 'Quad' )

MATRIX STORED IN ST-ELEMENT-SHAPEDATA

NIPS ::   4  NIPT ::   2

-------------------------------------------------

4D MATRIX, MAT4(:,:,:,:) ::

Mat4( :, :,  1, 1 )

 -0.4444444      0.1111111      0.2222222      0.1111111
 -0.3333333       0.000000      0.3333333       0.000000
 -0.2222222     -0.1111111      0.4444444     -0.1111111
 -0.3333333       0.000000      0.3333333       0.000000

Mat4( :, :,  1, 2 )

 -0.2222222      0.5555556E-01  0.1111111      0.5555556E-01
 -0.1666667       0.000000      0.1666667       0.000000
 -0.1111111     -0.5555556E-01  0.2222222     -0.5555556E-01
 -0.1666667       0.000000      0.1666667       0.000000

Mat4( :, :,  2, 1 )

 -0.2222222      0.5555556E-01  0.1111111      0.5555556E-01
 -0.1666667       0.000000      0.1666667       0.000000
 -0.1111111     -0.5555556E-01  0.2222222     -0.5555556E-01
 -0.1666667       0.000000      0.1666667       0.000000

Mat4( :, :,  2, 2 )

 -0.4444444      0.1111111      0.2222222      0.1111111
 -0.3333333       0.000000      0.3333333       0.000000
 -0.2222222     -0.1111111      0.4444444     -0.1111111
 -0.3333333       0.000000      0.3333333       0.000000
```

### getConvectiveMatrix_4()

INTERFACE

```fortran
 SUBROUTINE getConvectiveMatrix_4( Obj, C, Term1, Term2, Ctype )

    USE Utility, ONLY : OUTERPROD

    CLASS( STConvectiveMatrix_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), DIMENSION( :, :, : ), INTENT( IN ) :: C
    INTEGER( I4B ), INTENT( IN ) :: Term1, Term2
    CHARACTER( LEN = * ), INTENT( IN ) :: CType
```

DESCRIPTION

- `C(:,:)` is a two dimension array. 
    - If `Ctype` is in the set `[Nodal, NodalValues, Nodal Values, Space Nodal Values, SpaceNodalValues]` then `C` denotes the space-time nodal values, and the shape of `C` will be `(NSD, NNS, NNT)`. In this case, first index of `C` denotes the spatial-coordinate. The second index denotes the spatial-node. The third index denotes the temporal-node. 
    - If `Ctype` is in the set `[Integration, IntegrationPoints, Integration Points, Quad, QuadPoints, Quad Points]` then `C` denotes the value of _convective velocity_ at space-time integration points(quadrature points). In this case, the shape of `C` is `(NSD, NIPS, NIPT)`. The first index of `C` denotes the spatial coordinate. The second index of `C` denotes the spatial-integration point. The third index of `C` denotes the temporal-integration point.
    - In this method, `C` varies both in spatial and temporal dimension.
- `Term1` and `Term2` are integers that can take values 0 and 1. They represents the spatial derivative. If it is zero then this means no spatial derivative. If it is 1 then it means first order spatial derivative.

SYMBOLIC CALCULATION

$${}^{4}M(I,J,a,b) = {}^{a}\delta u_{iI} \quad \int_{Q_n} N^I T_a c_k \frac{\partial N^J T_b}{\partial x_k} {dQ} \quad {}^{b}u_{iJ}$$

$${}^{4}M(I,J,a,b) = {}^{a}\delta u_{iI} \quad \int_{Q_n} c_k \frac{\partial N^I T_a}{\partial x_k} N^J T_b {dQ} \quad {}^{b}u_{iJ}$$

TESTING

```fortran
IF( ALLOCATED( DummyMat2 ) ) DEALLOCATE( DummyMat2 ) 
ALLOCATE( DummyMat2( NSD, NIPS ) )
    
DummyMat2 = 1.0_DFP
    
CALL STElemSD % getConvectiveMatrix( C = DummyMat2, Term1 = 0, Term2 = 1, CType = "Quad" )
CALL STElemSD % DISPLAYMATRIX4( )
```

**`NIPS = 4`, `NIPT = 2`**

```fortran

CALL STElemSD % getConvectiveMatrix_3( C = DummyMat2, Term1 = 0, Term2 = 1, Ctype = 'Quad' )

MATRIX STORED IN ST-ELEMENT-SHAPEDATA

NIPS ::   4  NIPT ::   2

-------------------------------------------------

4D MATRIX, MAT4(:,:,:,:) ::

Mat4( :, :,  1, 1 )

 -0.4444444      0.1111111      0.2222222      0.1111111
 -0.3333333       0.000000      0.3333333       0.000000
 -0.2222222     -0.1111111      0.4444444     -0.1111111
 -0.3333333       0.000000      0.3333333       0.000000

Mat4( :, :,  1, 2 )

 -0.2222222      0.5555556E-01  0.1111111      0.5555556E-01
 -0.1666667       0.000000      0.1666667       0.000000
 -0.1111111     -0.5555556E-01  0.2222222     -0.5555556E-01
 -0.1666667       0.000000      0.1666667       0.000000

Mat4( :, :,  2, 1 )

 -0.2222222      0.5555556E-01  0.1111111      0.5555556E-01
 -0.1666667       0.000000      0.1666667       0.000000
 -0.1111111     -0.5555556E-01  0.2222222     -0.5555556E-01
 -0.1666667       0.000000      0.1666667       0.000000

Mat4( :, :,  2, 2 )

 -0.4444444      0.1111111      0.2222222      0.1111111
 -0.3333333       0.000000      0.3333333       0.000000
 -0.2222222     -0.1111111      0.4444444     -0.1111111
 -0.3333333       0.000000      0.3333333       0.000000
```

### getConvectiveMatrix_5()

INTERFACE

```fortran
 SUBROUTINE getConvectiveMatrix_5( Obj, C, Term1, Term2, nCopy )

    USE Utility, ONLY : OUTERPROD

    CLASS( STConvectiveMatrix_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), DIMENSION( :, :, : ), INTENT( IN ) :: C
    INTEGER( I4B ), INTENT( IN ) :: Term1, Term2, nCopy
```

DESCRIPTION

- `C(:,:,:)` is a three dimension array. `C` denotes the space-time nodal values of _convective velocity_, and the shape of `C` will be `(NSD, NNS, NNT)`. In this case, first index of `C` denotes the spatial-coordinate. The second index denotes the spatial-node. The third index denotes the temporal nodes. In this method, `C` varies in both spatial and temporal dimension.
- `Term1` and `Term2` are integers that can take values 0 and 1. They represents the spatial derivative. If it is zero then this means no spatial derivative. If it is 1 then it means first order spatial derivative.
- `nCopy` is the number of copies that should be placed on diagonals.

> For more details see the notes.

SYMBOLIC CALCULATION

$${}^{4}M(I,J,a,b) = {}^{a}\delta u_{iI} \quad \int_{Q_n} N^I T_a c_k \frac{\partial N^J T_b}{\partial x_k} {dQ} \quad {}^{b}u_{iJ}$$

$${}^{4}M(I,J,a,b) = {}^{a}\delta u_{iI} \quad \int_{Q_n} c_k \frac{\partial N^I T_a}{\partial x_k} N^J T_b {dQ} \quad {}^{b}u_{iJ}$$

TESTING

```fortran
IF( ALLOCATED( DummyMat3 ) ) DEALLOCATE( DummyMat3 ) 
ALLOCATE( DummyMat3( NSD, NNS, NNT ) )
    
DummyMat3 = 1.0_DFP
    
CALL STElemSD % getConvectiveMatrix( C = DummyMat3, Term1 = 0, Term2 = 1, nCopy = 2 )
CALL STElemSD % DISPLAYMATRIX4( )
```

**`NIPS = 4`, `NIPT = 2`**

```fortran

CALL STElemSD % getConvectiveMatrix_5( C = DummyMat3, Term1 = 0, Term2 = 1, nCopy = 2 )

MATRIX STORED IN ST-ELEMENT-SHAPEDATA

NIPS ::   4  NIPT ::   2

-------------------------------------------------

4D MATRIX, MAT4(:,:,:,:) ::

Mat4( :, :,  1, 1 )

 -0.4444444      0.1111111      0.2222222      0.1111111       0.000000       0.000000       0.000000       0.000000
 -0.3333333       0.000000      0.3333333       0.000000       0.000000       0.000000       0.000000       0.000000
 -0.2222222     -0.1111111      0.4444444     -0.1111111       0.000000       0.000000       0.000000       0.000000
 -0.3333333       0.000000      0.3333333       0.000000       0.000000       0.000000       0.000000       0.000000
   0.000000       0.000000       0.000000       0.000000     -0.4444444      0.1111111      0.2222222      0.1111111
   0.000000       0.000000       0.000000       0.000000     -0.3333333       0.000000      0.3333333       0.000000
   0.000000       0.000000       0.000000       0.000000     -0.2222222     -0.1111111      0.4444444     -0.1111111
   0.000000       0.000000       0.000000       0.000000     -0.3333333       0.000000      0.3333333       0.000000

Mat4( :, :,  1, 2 )

 -0.2222222      0.5555556E-01  0.1111111      0.5555556E-01   0.000000       0.000000       0.000000       0.000000
 -0.1666667       0.000000      0.1666667       0.000000       0.000000       0.000000       0.000000       0.000000
 -0.1111111     -0.5555556E-01  0.2222222     -0.5555556E-01   0.000000       0.000000       0.000000       0.000000
 -0.1666667       0.000000      0.1666667       0.000000       0.000000       0.000000       0.000000       0.000000
   0.000000       0.000000       0.000000       0.000000     -0.2222222      0.5555556E-01  0.1111111      0.5555556E-01
   0.000000       0.000000       0.000000       0.000000     -0.1666667       0.000000      0.1666667       0.000000
   0.000000       0.000000       0.000000       0.000000     -0.1111111     -0.5555556E-01  0.2222222     -0.5555556E-01
   0.000000       0.000000       0.000000       0.000000     -0.1666667       0.000000      0.1666667       0.000000

Mat4( :, :,  2, 1 )

 -0.2222222      0.5555556E-01  0.1111111      0.5555556E-01   0.000000       0.000000       0.000000       0.000000
 -0.1666667       0.000000      0.1666667       0.000000       0.000000       0.000000       0.000000       0.000000
 -0.1111111     -0.5555556E-01  0.2222222     -0.5555556E-01   0.000000       0.000000       0.000000       0.000000
 -0.1666667       0.000000      0.1666667       0.000000       0.000000       0.000000       0.000000       0.000000
   0.000000       0.000000       0.000000       0.000000     -0.2222222      0.5555556E-01  0.1111111      0.5555556E-01
   0.000000       0.000000       0.000000       0.000000     -0.1666667       0.000000      0.1666667       0.000000
   0.000000       0.000000       0.000000       0.000000     -0.1111111     -0.5555556E-01  0.2222222     -0.5555556E-01
   0.000000       0.000000       0.000000       0.000000     -0.1666667       0.000000      0.1666667       0.000000

Mat4( :, :,  2, 2 )

 -0.4444444      0.1111111      0.2222222      0.1111111       0.000000       0.000000       0.000000       0.000000
 -0.3333333       0.000000      0.3333333       0.000000       0.000000       0.000000       0.000000       0.000000
 -0.2222222     -0.1111111      0.4444444     -0.1111111       0.000000       0.000000       0.000000       0.000000
 -0.3333333       0.000000      0.3333333       0.000000       0.000000       0.000000       0.000000       0.000000
   0.000000       0.000000       0.000000       0.000000     -0.4444444      0.1111111      0.2222222      0.1111111
   0.000000       0.000000       0.000000       0.000000     -0.3333333       0.000000      0.3333333       0.000000
   0.000000       0.000000       0.000000       0.000000     -0.2222222     -0.1111111      0.4444444     -0.1111111
   0.000000       0.000000       0.000000       0.000000     -0.3333333       0.000000      0.3333333       0.000000
```

### getConvectiveMatrix_6()

INTERFACE

```fortran
 SUBROUTINE getConvectiveMatrix_6( Obj, C, Term1, Term2, nCopy )

    USE Utility, ONLY : OUTERPROD

    CLASS( STConvectiveMatrix_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), DIMENSION( :, : ), INTENT( IN ) :: C
    INTEGER( I4B ), INTENT( IN ) :: Term1, Term2, nCopy
```

DESCRIPTION

- `C(:,:,:)` is a two dimension array. `C` denotes the space-time nodal values of _convective velocity_, and the shape of `C` will be `(NSD, NNS)`. In this case, first index of `C` denotes the spatial-coordinate. The second index denotes the spatial-node. In this method, `C` varies in only in spatial dimension, and  remains constant in temporal dimension.
- `Term1` and `Term2` are integers that can take values 0 and 1. They represents the spatial derivative. If it is zero then this means no spatial derivative. If it is 1 then it means first order spatial derivative.
- `nCopy` is the number of copies that should be placed on diagonals.

> For more details see the notes.

SYMBOLIC CALCULATION

$${}^{4}M(I,J,a,b) = {}^{a}\delta u_{iI} \quad \int_{Q_n} N^I T_a c_k \frac{\partial N^J T_b}{\partial x_k} {dQ} \quad {}^{b}u_{iJ}$$

$${}^{4}M(I,J,a,b) = {}^{a}\delta u_{iI} \quad \int_{Q_n} c_k \frac{\partial N^I T_a}{\partial x_k} N^J T_b {dQ} \quad {}^{b}u_{iJ}$$

TESTING

```fortran
IF( ALLOCATED( DummyMat2 ) ) DEALLOCATE( DummyMat2 ) 
ALLOCATE( DummyMat2( NSD, NNS ) )
    
DummyMat2 = 1.0_DFP
    
CALL STElemSD % getConvectiveMatrix( C = DummyMat2, Term1 = 0, Term2 = 1, nCopy = 2 )
CALL STElemSD % DISPLAYMATRIX4( )
```

**`NIPS = 4`, `NIPT = 2`**

```fortran

CALL STElemSD % getConvectiveMatrix_6( C = DummyMat2, Term1 = 0, Term2 = 1, nCopy = 2 )

MATRIX STORED IN ST-ELEMENT-SHAPEDATA

NIPS ::   4  NIPT ::   2

-------------------------------------------------

4D MATRIX, MAT4(:,:,:,:) ::

Mat4( :, :,  1, 1 )

 -0.4444444      0.1111111      0.2222222      0.1111111       0.000000       0.000000       0.000000       0.000000
 -0.3333333       0.000000      0.3333333       0.000000       0.000000       0.000000       0.000000       0.000000
 -0.2222222     -0.1111111      0.4444444     -0.1111111       0.000000       0.000000       0.000000       0.000000
 -0.3333333       0.000000      0.3333333       0.000000       0.000000       0.000000       0.000000       0.000000
   0.000000       0.000000       0.000000       0.000000     -0.4444444      0.1111111      0.2222222      0.1111111
   0.000000       0.000000       0.000000       0.000000     -0.3333333       0.000000      0.3333333       0.000000
   0.000000       0.000000       0.000000       0.000000     -0.2222222     -0.1111111      0.4444444     -0.1111111
   0.000000       0.000000       0.000000       0.000000     -0.3333333       0.000000      0.3333333       0.000000

Mat4( :, :,  1, 2 )

 -0.2222222      0.5555556E-01  0.1111111      0.5555556E-01   0.000000       0.000000       0.000000       0.000000
 -0.1666667       0.000000      0.1666667       0.000000       0.000000       0.000000       0.000000       0.000000
 -0.1111111     -0.5555556E-01  0.2222222     -0.5555556E-01   0.000000       0.000000       0.000000       0.000000
 -0.1666667       0.000000      0.1666667       0.000000       0.000000       0.000000       0.000000       0.000000
   0.000000       0.000000       0.000000       0.000000     -0.2222222      0.5555556E-01  0.1111111      0.5555556E-01
   0.000000       0.000000       0.000000       0.000000     -0.1666667       0.000000      0.1666667       0.000000
   0.000000       0.000000       0.000000       0.000000     -0.1111111     -0.5555556E-01  0.2222222     -0.5555556E-01
   0.000000       0.000000       0.000000       0.000000     -0.1666667       0.000000      0.1666667       0.000000

Mat4( :, :,  2, 1 )

 -0.2222222      0.5555556E-01  0.1111111      0.5555556E-01   0.000000       0.000000       0.000000       0.000000
 -0.1666667       0.000000      0.1666667       0.000000       0.000000       0.000000       0.000000       0.000000
 -0.1111111     -0.5555556E-01  0.2222222     -0.5555556E-01   0.000000       0.000000       0.000000       0.000000
 -0.1666667       0.000000      0.1666667       0.000000       0.000000       0.000000       0.000000       0.000000
   0.000000       0.000000       0.000000       0.000000     -0.2222222      0.5555556E-01  0.1111111      0.5555556E-01
   0.000000       0.000000       0.000000       0.000000     -0.1666667       0.000000      0.1666667       0.000000
   0.000000       0.000000       0.000000       0.000000     -0.1111111     -0.5555556E-01  0.2222222     -0.5555556E-01
   0.000000       0.000000       0.000000       0.000000     -0.1666667       0.000000      0.1666667       0.000000

Mat4( :, :,  2, 2 )

 -0.4444444      0.1111111      0.2222222      0.1111111       0.000000       0.000000       0.000000       0.000000
 -0.3333333       0.000000      0.3333333       0.000000       0.000000       0.000000       0.000000       0.000000
 -0.2222222     -0.1111111      0.4444444     -0.1111111       0.000000       0.000000       0.000000       0.000000
 -0.3333333       0.000000      0.3333333       0.000000       0.000000       0.000000       0.000000       0.000000
   0.000000       0.000000       0.000000       0.000000     -0.4444444      0.1111111      0.2222222      0.1111111
   0.000000       0.000000       0.000000       0.000000     -0.3333333       0.000000      0.3333333       0.000000
   0.000000       0.000000       0.000000       0.000000     -0.2222222     -0.1111111      0.4444444     -0.1111111
   0.000000       0.000000       0.000000       0.000000     -0.3333333       0.000000      0.3333333
```

### getConvectiveMatrix_7()

INTERFACE

```fortran
 SUBROUTINE getConvectiveMatrix_7( Obj, C, Term1, Term2, CType, nCopy )

    USE Utility, ONLY : OUTERPROD

    CLASS( STConvectiveMatrix_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), DIMENSION( :, : ), INTENT( IN ) :: C
    INTEGER( I4B ), INTENT( IN ) :: Term1, Term2, nCopy
    CHARACTER( LEN = * ), INTENT( IN ) :: CType
```

DESCRIPTION

- `C(:,:)` is a two dimension array. 
    - If `Ctype` is in the set `[Nodal, NodalValues, Nodal Values, Space Nodal Values, SpaceNodalValues]` then `C` denotes the spatial nodal values, and the shape of `C` will be `(NSD, NNS)`. In this case  first index of `C` denotes the spatial-coordinate. The second index denotes the spatial-node. 
    - If `Ctype` is in the set `[Integration, IntegrationPoints, Integration Points, Quad, QuadPoints, Quad Points]` then `C` denotes the value of _convective velocity_ at spatial-integration points(quadrature points). In this case, the shape of `C` is `(NSD, NIPS)`. The first index of `C` denotes the spatial coordinate. The second index of `C` denotes the spatial-integration point. 
    - In this method, `C` varies only in spatial dimension and remains constant in the temporal domain.
- `Term1` and `Term2` are integers that can take values 0 and 1. They represents the spatial derivative. If it is zero then this means no spatial derivative. If it is 1 then it means first order spatial derivative.
- `nCopy` is the number of copies that should be placed on diagonals.

> For more details see the notes.

SYMBOLIC CALCULATION

$${}^{4}M(I,J,a,b) = {}^{a}\delta u_{iI} \quad \int_{Q_n} N^I T_a c_k \frac{\partial N^J T_b}{\partial x_k} {dQ} \quad {}^{b}u_{iJ}$$

$${}^{4}M(I,J,a,b) = {}^{a}\delta u_{iI} \quad \int_{Q_n} c_k \frac{\partial N^I T_a}{\partial x_k} N^J T_b {dQ} \quad {}^{b}u_{iJ}$$

TESTING

```fortran
IF( ALLOCATED( DummyMat2 ) ) DEALLOCATE( DummyMat2 ) 
ALLOCATE( DummyMat2( NSD, NIPS ) )
    
DummyMat2 = 1.0_DFP
    
CALL STElemSD % getConvectiveMatrix( C = DummyMat2, Term1 = 0, Term2 = 1, nCopy = 2 )
CALL STElemSD % DISPLAYMATRIX4( )
```

**`NIPS = 4`, `NIPT = 2`**

```fortran

CALL STElemSD % getConvectiveMatrix_7( C = DummyMat2, Term1 = 0,  Term2 = 1, nCopy = 2, CType = 'Quad' )

MATRIX STORED IN ST-ELEMENT-SHAPEDATA

NIPS ::   4  NIPT ::   2

-------------------------------------------------

4D MATRIX, MAT4(:,:,:,:) ::

Mat4( :, :,  1, 1 )

 -0.4444444      0.1111111      0.2222222      0.1111111       0.000000       0.000000       0.000000       0.000000
 -0.3333333       0.000000      0.3333333       0.000000       0.000000       0.000000       0.000000       0.000000
 -0.2222222     -0.1111111      0.4444444     -0.1111111       0.000000       0.000000       0.000000       0.000000
 -0.3333333       0.000000      0.3333333       0.000000       0.000000       0.000000       0.000000       0.000000
   0.000000       0.000000       0.000000       0.000000     -0.4444444      0.1111111      0.2222222      0.1111111
   0.000000       0.000000       0.000000       0.000000     -0.3333333       0.000000      0.3333333       0.000000
   0.000000       0.000000       0.000000       0.000000     -0.2222222     -0.1111111      0.4444444     -0.1111111
   0.000000       0.000000       0.000000       0.000000     -0.3333333       0.000000      0.3333333       0.000000

Mat4( :, :,  1, 2 )

 -0.2222222      0.5555556E-01  0.1111111      0.5555556E-01   0.000000       0.000000       0.000000       0.000000
 -0.1666667       0.000000      0.1666667       0.000000       0.000000       0.000000       0.000000       0.000000
 -0.1111111     -0.5555556E-01  0.2222222     -0.5555556E-01   0.000000       0.000000       0.000000       0.000000
 -0.1666667       0.000000      0.1666667       0.000000       0.000000       0.000000       0.000000       0.000000
   0.000000       0.000000       0.000000       0.000000     -0.2222222      0.5555556E-01  0.1111111      0.5555556E-01
   0.000000       0.000000       0.000000       0.000000     -0.1666667       0.000000      0.1666667       0.000000
   0.000000       0.000000       0.000000       0.000000     -0.1111111     -0.5555556E-01  0.2222222     -0.5555556E-01
   0.000000       0.000000       0.000000       0.000000     -0.1666667       0.000000      0.1666667       0.000000

Mat4( :, :,  2, 1 )

 -0.2222222      0.5555556E-01  0.1111111      0.5555556E-01   0.000000       0.000000       0.000000       0.000000
 -0.1666667       0.000000      0.1666667       0.000000       0.000000       0.000000       0.000000       0.000000
 -0.1111111     -0.5555556E-01  0.2222222     -0.5555556E-01   0.000000       0.000000       0.000000       0.000000
 -0.1666667       0.000000      0.1666667       0.000000       0.000000       0.000000       0.000000       0.000000
   0.000000       0.000000       0.000000       0.000000     -0.2222222      0.5555556E-01  0.1111111      0.5555556E-01
   0.000000       0.000000       0.000000       0.000000     -0.1666667       0.000000      0.1666667       0.000000
   0.000000       0.000000       0.000000       0.000000     -0.1111111     -0.5555556E-01  0.2222222     -0.5555556E-01
   0.000000       0.000000       0.000000       0.000000     -0.1666667       0.000000      0.1666667       0.000000

Mat4( :, :,  2, 2 )

 -0.4444444      0.1111111      0.2222222      0.1111111       0.000000       0.000000       0.000000       0.000000
 -0.3333333       0.000000      0.3333333       0.000000       0.000000       0.000000       0.000000       0.000000
 -0.2222222     -0.1111111      0.4444444     -0.1111111       0.000000       0.000000       0.000000       0.000000
 -0.3333333       0.000000      0.3333333       0.000000       0.000000       0.000000       0.000000       0.000000
   0.000000       0.000000       0.000000       0.000000     -0.4444444      0.1111111      0.2222222      0.1111111
   0.000000       0.000000       0.000000       0.000000     -0.3333333       0.000000      0.3333333       0.000000
   0.000000       0.000000       0.000000       0.000000     -0.2222222     -0.1111111      0.4444444     -0.1111111
   0.000000       0.000000       0.000000       0.000000     -0.3333333       0.000000      0.3333333       0.000000
```

### getConvectiveMatrix_8()

INTERFACE

```fortran
 SUBROUTINE getConvectiveMatrix_8( Obj, C, Term1, Term2, CType, nCopy )

    USE Utility, ONLY : OUTERPROD

    CLASS( STConvectiveMatrix_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), DIMENSION( :, :, : ), INTENT( IN ) :: C
    INTEGER( I4B ), INTENT( IN ) :: Term1, Term2, nCopy
    CHARACTER( LEN = * ), INTENT( IN ) :: CType
```

DESCRIPTION

- `C(:,:)` is a two dimension array. 
    - If `Ctype` is in the set `[Nodal, NodalValues, Nodal Values, Space Nodal Values, SpaceNodalValues]` then `C` denotes the space-time nodal values, and the shape of `C` will be `(NSD, NNS, NNT)`. In this case, first index of `C` denotes the spatial-coordinate. The second index denotes the spatial-node. The third index denotes the temporal-node. 
    - If `Ctype` is in the set `[Integration, IntegrationPoints, Integration Points, Quad, QuadPoints, Quad Points]` then `C` denotes the value of _convective velocity_ at space-time integration points(quadrature points). In this case, the shape of `C` is `(NSD, NIPS, NIPT)`. The first index of `C` denotes the spatial coordinate. The second index of `C` denotes the spatial-integration point. The third index of `C` denotes the temporal-integration point.
    - In this method, `C` varies both in spatial and temporal dimension.
- `Term1` and `Term2` are integers that can take values 0 and 1. They represents the spatial derivative. If it is zero then this means no spatial derivative. If it is 1 then it means first order spatial derivative.
- `nCopy` is the number of copies that should be placed on diagonals.

> For more details see the notes.

SYMBOLIC CALCULATION

$${}^{4}M(I,J,a,b) = {}^{a}\delta u_{iI} \quad \int_{Q_n} N^I T_a c_k \frac{\partial N^J T_b}{\partial x_k} {dQ} \quad {}^{b}u_{iJ}$$

$${}^{4}M(I,J,a,b) = {}^{a}\delta u_{iI} \quad \int_{Q_n} c_k \frac{\partial N^I T_a}{\partial x_k} N^J T_b {dQ} \quad {}^{b}u_{iJ}$$

TESTING

```fortran
IF( ALLOCATED( DummyMat3 ) ) DEALLOCATE( DummyMat3 ) 
ALLOCATE( DummyMat3( NSD, NIPS, NIPT ) )
    
DummyMat3 = 1.0_DFP
    
CALL STElemSD % getConvectiveMatrix( C = DummyMat3, Term1 = 0, Term2 = 1, nCopy = 2, CType = "Quad" )
CALL STElemSD % DISPLAYMATRIX4( )
```

**`NIPS = 4`, `NIPT = 2`**

```fortran

CALL STElemSD % getConvectiveMatrix_8( C = DummyMat3, Term1 = 0,  Term2 = 1, nCopy = 2, CType = 'Quad' )

MATRIX STORED IN ST-ELEMENT-SHAPEDATA

NIPS ::   4  NIPT ::   2

-------------------------------------------------

4D MATRIX, MAT4(:,:,:,:) ::

Mat4( :, :,  1, 1 )

 -0.4444444      0.1111111      0.2222222      0.1111111       0.000000       0.000000       0.000000       0.000000
 -0.3333333       0.000000      0.3333333       0.000000       0.000000       0.000000       0.000000       0.000000
 -0.2222222     -0.1111111      0.4444444     -0.1111111       0.000000       0.000000       0.000000       0.000000
 -0.3333333       0.000000      0.3333333       0.000000       0.000000       0.000000       0.000000       0.000000
   0.000000       0.000000       0.000000       0.000000     -0.4444444      0.1111111      0.2222222      0.1111111
   0.000000       0.000000       0.000000       0.000000     -0.3333333       0.000000      0.3333333       0.000000
   0.000000       0.000000       0.000000       0.000000     -0.2222222     -0.1111111      0.4444444     -0.1111111
   0.000000       0.000000       0.000000       0.000000     -0.3333333       0.000000      0.3333333       0.000000

Mat4( :, :,  1, 2 )

 -0.2222222      0.5555556E-01  0.1111111      0.5555556E-01   0.000000       0.000000       0.000000       0.000000
 -0.1666667       0.000000      0.1666667       0.000000       0.000000       0.000000       0.000000       0.000000
 -0.1111111     -0.5555556E-01  0.2222222     -0.5555556E-01   0.000000       0.000000       0.000000       0.000000
 -0.1666667       0.000000      0.1666667       0.000000       0.000000       0.000000       0.000000       0.000000
   0.000000       0.000000       0.000000       0.000000     -0.2222222      0.5555556E-01  0.1111111      0.5555556E-01
   0.000000       0.000000       0.000000       0.000000     -0.1666667       0.000000      0.1666667       0.000000
   0.000000       0.000000       0.000000       0.000000     -0.1111111     -0.5555556E-01  0.2222222     -0.5555556E-01
   0.000000       0.000000       0.000000       0.000000     -0.1666667       0.000000      0.1666667       0.000000

Mat4( :, :,  2, 1 )

 -0.2222222      0.5555556E-01  0.1111111      0.5555556E-01   0.000000       0.000000       0.000000       0.000000
 -0.1666667       0.000000      0.1666667       0.000000       0.000000       0.000000       0.000000       0.000000
 -0.1111111     -0.5555556E-01  0.2222222     -0.5555556E-01   0.000000       0.000000       0.000000       0.000000
 -0.1666667       0.000000      0.1666667       0.000000       0.000000       0.000000       0.000000       0.000000
   0.000000       0.000000       0.000000       0.000000     -0.2222222      0.5555556E-01  0.1111111      0.5555556E-01
   0.000000       0.000000       0.000000       0.000000     -0.1666667       0.000000      0.1666667       0.000000
   0.000000       0.000000       0.000000       0.000000     -0.1111111     -0.5555556E-01  0.2222222     -0.5555556E-01
   0.000000       0.000000       0.000000       0.000000     -0.1666667       0.000000      0.1666667       0.000000

Mat4( :, :,  2, 2 )

 -0.4444444      0.1111111      0.2222222      0.1111111       0.000000       0.000000       0.000000       0.000000
 -0.3333333       0.000000      0.3333333       0.000000       0.000000       0.000000       0.000000       0.000000
 -0.2222222     -0.1111111      0.4444444     -0.1111111       0.000000       0.000000       0.000000       0.000000
 -0.3333333       0.000000      0.3333333       0.000000       0.000000       0.000000       0.000000       0.000000
   0.000000       0.000000       0.000000       0.000000     -0.4444444      0.1111111      0.2222222      0.1111111
   0.000000       0.000000       0.000000       0.000000     -0.3333333       0.000000      0.3333333       0.000000
   0.000000       0.000000       0.000000       0.000000     -0.2222222     -0.1111111      0.4444444     -0.1111111
   0.000000       0.000000       0.000000       0.000000     -0.3333333       0.000000      0.3333333       0.000000
```

### getConvectiveMatrix_9()

INTERFACE

```fortran
 SUBROUTINE getConvectiveMatrix_9( Obj, Term1, Term2, Xtype )

    USE Utility, ONLY : OUTERPROD

    CLASS( STConvectiveMatrix_ ), INTENT( INOUT ) ::  Obj
    INTEGER( I4B ), INTENT( IN ) :: Term1, Term2
    CHARACTER( LEN = * ), INTENT( IN ) :: XType
```

DESCRIPTION

- `Term1` and `Term2` are integers that can take values 0 and 1. They represents the spatial derivative. If it is zero then this means no spatial derivative. If it is 1 then it means first order spatial derivative.
- `XType` is a string which denotes the spatial gradient type. 
    - If `Xtype` is in the set `[dx, dX, dx1, dX1, x, X, x1, X1]` then it denotes the spatial gradient with respect to `x` coordinate.
    - If `Xtype` is in the set `[dy, dY, dx2, dX2, y, Y, x2, X2]` then it denotes the spatial gradient with respect to `y` coordinate.
    - If `Xtype` is in the set `[dz, dZ, dx3, dX3, z, Z, x3, X3]` then it denotes the spatial gradient with respect to `z` coordinate.
    
SYMBOLIC CALCULATION

$${}^{4}M(I,J,a,b) = \delta {}^{a} U_{iI} \quad \int_{Q_n} N^I T_a \frac{\partial N^J T_b}{\partial x} {dQ} \quad {}^{b}f_{iJ}$$

$${}^{4}M(I,J,a,b) = \delta {}^{a} U_{iI} \quad \int_{Q_n} \frac{\partial N^I T_a}{\partial x} N^J T_b {dQ}  \quad {}^{b}f_{iJ}$$

$${}^{4}M(I,J,a,b) = \delta {}^{a} U_{iI} \quad \int_{Q_n} N^I T_a \frac{\partial N^J T_b}{\partial x} {dQ} \quad {}^{b}g_{iJ}$$

$${}^{4}M(I,J,a,b) = \delta {}^{a} U_{iI} \quad \int_{Q_n} \frac{\partial N^I T_a}{\partial x} N^J T_b {dQ}  \quad {}^{b}g_{iJ}$$


$${}^{4}M(I,J,a,b) = \delta {}^{a} U_{iI} \quad \int_{Q_n} N^I T_a \frac{\partial N^J T_b}{\partial x} {dQ} \quad {}^{b}h_{iJ}$$

$${}^{4}M(I,J,a,b) = \delta {}^{a} U_{iI} \quad \int_{Q_n} \frac{\partial N^I T_a}{\partial x} N^J T_b {dQ}  \quad {}^{b}h_{iJ}$$

    
TESTING

```fortran
CALL STElemSD % getConvectiveMatrix( Term1 = 0, Term2 = 1, XType = "dx" )
CALL STElemSD % DISPLAYMATRIX4( )
```

**`NIPS = 4`, `NIPT = 2`**

```fortran

CALL STElemSD % getConvectiveMatrix_9( Term1 = 0,  Term2 = 1, XType = 'dx' )

MATRIX STORED IN ST-ELEMENT-SHAPEDATA

NIPS ::   4  NIPT ::   2

-------------------------------------------------

4D MATRIX, MAT4(:,:,:,:) ::

Mat4( :, :,  1, 1 )

 -0.2222222      0.2222222      0.1111111     -0.1111111
 -0.2222222      0.2222222      0.1111111     -0.1111111
 -0.1111111      0.1111111      0.2222222     -0.2222222
 -0.1111111      0.1111111      0.2222222     -0.2222222

Mat4( :, :,  1, 2 )

 -0.1111111      0.1111111      0.5555556E-01 -0.5555556E-01
 -0.1111111      0.1111111      0.5555556E-01 -0.5555556E-01
 -0.5555556E-01  0.5555556E-01  0.1111111     -0.1111111
 -0.5555556E-01  0.5555556E-01  0.1111111     -0.1111111

Mat4( :, :,  2, 1 )

 -0.1111111      0.1111111      0.5555556E-01 -0.5555556E-01
 -0.1111111      0.1111111      0.5555556E-01 -0.5555556E-01
 -0.5555556E-01  0.5555556E-01  0.1111111     -0.1111111
 -0.5555556E-01  0.5555556E-01  0.1111111     -0.1111111

Mat4( :, :,  2, 2 )

 -0.2222222      0.2222222      0.1111111     -0.1111111
 -0.2222222      0.2222222      0.1111111     -0.1111111
 -0.1111111      0.1111111      0.2222222     -0.2222222
 -0.1111111      0.1111111      0.2222222     -0.2222222
```

### getConvectiveMatrix_10()

INTERFACE

```fortran
 SUBROUTINE getConvectiveMatrix_10( Obj, Term1, Term2, Xtype, nCopy )

    USE Utility, ONLY : OUTERPROD

    CLASS( STConvectiveMatrix_ ), INTENT( INOUT ) ::  Obj
    INTEGER( I4B ), INTENT( IN ) :: Term1, Term2, nCopy
    CHARACTER( LEN = * ), INTENT( IN ) :: XType
```

DESCRIPTION

- `Term1` and `Term2` are integers that can take values 0 and 1. They represents the spatial derivative. If it is zero then this means no spatial derivative. If it is 1 then it means first order spatial derivative.
- `XType` is a string which denotes the spatial gradient type. 
    - If `Xtype` is in the set `[dx, dX, dx1, dX1, x, X, x1, X1]` then it denotes the spatial gradient with respect to `x` coordinate.
    - If `Xtype` is in the set `[dy, dY, dx2, dX2, y, Y, x2, X2]` then it denotes the spatial gradient with respect to `y` coordinate.
    - If `Xtype` is in the set `[dz, dZ, dx3, dX3, z, Z, x3, X3]` then it denotes the spatial gradient with respect to `z` coordinate.
- `nCopy` is the number of copies that should be placed on diagonals.

SYMBOLIC CALCULATION

$${}^{4}M(I,J,a,b) = \delta {}^{a} U_{iI} \quad \int_{Q_n} N^I T_a \frac{\partial N^J T_b}{\partial x} {dQ} \quad {}^{b}f_{iJ}$$

$${}^{4}M(I,J,a,b) = \delta {}^{a} U_{iI} \quad \int_{Q_n} \frac{\partial N^I T_a}{\partial x} N^J T_b {dQ}  \quad {}^{b}f_{iJ}$$

$${}^{4}M(I,J,a,b) = \delta {}^{a} U_{iI} \quad \int_{Q_n} N^I T_a \frac{\partial N^J T_b}{\partial x} {dQ} \quad {}^{b}g_{iJ}$$

$${}^{4}M(I,J,a,b) = \delta {}^{a} U_{iI} \quad \int_{Q_n} \frac{\partial N^I T_a}{\partial x} N^J T_b {dQ}  \quad {}^{b}g_{iJ}$$


$${}^{4}M(I,J,a,b) = \delta {}^{a} U_{iI} \quad \int_{Q_n} N^I T_a \frac{\partial N^J T_b}{\partial x} {dQ} \quad {}^{b}h_{iJ}$$

$${}^{4}M(I,J,a,b) = \delta {}^{a} U_{iI} \quad \int_{Q_n} \frac{\partial N^I T_a}{\partial x} N^J T_b {dQ}  \quad {}^{b}h_{iJ}$$
    
TESTING

```fortran
CALL STElemSD % getConvectiveMatrix( Term1 = 0, Term2 = 1, XType = "X", nCopy = 2 )
CALL STElemSD % DISPLAYMATRIX4( )
```

**`NIPS = 4`, `NIPT = 2`**

```fortran
CALL STElemSD % getConvectiveMatrix_10( Term1 = 0,  Term2 = 1, XType = 'X', nCopy = 2 )

MATRIX STORED IN ST-ELEMENT-SHAPEDATA

NIPS ::   4  NIPT ::   2

-------------------------------------------------

4D MATRIX, MAT4(:,:,:,:) ::

Mat4( :, :,  1, 1 )

 -0.2222222      0.2222222      0.1111111     -0.1111111       0.000000       0.000000       0.000000       0.000000
 -0.2222222      0.2222222      0.1111111     -0.1111111       0.000000       0.000000       0.000000       0.000000
 -0.1111111      0.1111111      0.2222222     -0.2222222       0.000000       0.000000       0.000000       0.000000
 -0.1111111      0.1111111      0.2222222     -0.2222222       0.000000       0.000000       0.000000       0.000000
   0.000000       0.000000       0.000000       0.000000     -0.2222222      0.2222222      0.1111111     -0.1111111
   0.000000       0.000000       0.000000       0.000000     -0.2222222      0.2222222      0.1111111     -0.1111111
   0.000000       0.000000       0.000000       0.000000     -0.1111111      0.1111111      0.2222222     -0.2222222
   0.000000       0.000000       0.000000       0.000000     -0.1111111      0.1111111      0.2222222     -0.2222222

Mat4( :, :,  1, 2 )

 -0.1111111      0.1111111      0.5555556E-01 -0.5555556E-01   0.000000       0.000000       0.000000       0.000000
 -0.1111111      0.1111111      0.5555556E-01 -0.5555556E-01   0.000000       0.000000       0.000000       0.000000
 -0.5555556E-01  0.5555556E-01  0.1111111     -0.1111111       0.000000       0.000000       0.000000       0.000000
 -0.5555556E-01  0.5555556E-01  0.1111111     -0.1111111       0.000000       0.000000       0.000000       0.000000
   0.000000       0.000000       0.000000       0.000000     -0.1111111      0.1111111      0.5555556E-01 -0.5555556E-01
   0.000000       0.000000       0.000000       0.000000     -0.1111111      0.1111111      0.5555556E-01 -0.5555556E-01
   0.000000       0.000000       0.000000       0.000000     -0.5555556E-01  0.5555556E-01  0.1111111     -0.1111111
   0.000000       0.000000       0.000000       0.000000     -0.5555556E-01  0.5555556E-01  0.1111111     -0.1111111

Mat4( :, :,  2, 1 )

 -0.1111111      0.1111111      0.5555556E-01 -0.5555556E-01   0.000000       0.000000       0.000000       0.000000
 -0.1111111      0.1111111      0.5555556E-01 -0.5555556E-01   0.000000       0.000000       0.000000       0.000000
 -0.5555556E-01  0.5555556E-01  0.1111111     -0.1111111       0.000000       0.000000       0.000000       0.000000
 -0.5555556E-01  0.5555556E-01  0.1111111     -0.1111111       0.000000       0.000000       0.000000       0.000000
   0.000000       0.000000       0.000000       0.000000     -0.1111111      0.1111111      0.5555556E-01 -0.5555556E-01
   0.000000       0.000000       0.000000       0.000000     -0.1111111      0.1111111      0.5555556E-01 -0.5555556E-01
   0.000000       0.000000       0.000000       0.000000     -0.5555556E-01  0.5555556E-01  0.1111111     -0.1111111
   0.000000       0.000000       0.000000       0.000000     -0.5555556E-01  0.5555556E-01  0.1111111     -0.1111111

Mat4( :, :,  2, 2 )

 -0.2222222      0.2222222      0.1111111     -0.1111111       0.000000       0.000000       0.000000       0.000000
 -0.2222222      0.2222222      0.1111111     -0.1111111       0.000000       0.000000       0.000000       0.000000
 -0.1111111      0.1111111      0.2222222     -0.2222222       0.000000       0.000000       0.000000       0.000000
 -0.1111111      0.1111111      0.2222222     -0.2222222       0.000000       0.000000       0.000000       0.000000
   0.000000       0.000000       0.000000       0.000000     -0.2222222      0.2222222      0.1111111     -0.1111111
   0.000000       0.000000       0.000000       0.000000     -0.2222222      0.2222222      0.1111111     -0.1111111
   0.000000       0.000000       0.000000       0.000000     -0.1111111      0.1111111      0.2222222     -0.2222222
   0.000000       0.000000       0.000000       0.000000     -0.1111111      0.1111111      0.2222222     -0.2222222
```

### getConvectiveMatrix_11()

INTERFACE

```fortran
 SUBROUTINE getConvectiveMatrix_11( Obj, C, Term1, Term2 )

    USE Utility, ONLY : OUTERPROD

    CLASS( STConvectiveMatrix_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), DIMENSION( :, :, : ), INTENT( IN ) :: C
    CHARACTER( LEN = * ), INTENT( IN ) :: Term1, Term2
```

DESCRIPTION

- Here `C(:,:,:)` is the 3D array which denotes the **space-time** nodal values of *convection velocity* $c(x,t)$. The shape of `C` must be `C(NSD, NNS, NNT)`. Where `NSD` is number of spatial dimentsion. `NNS` number of nodes in spatial-element. `NNT` is number of nodes in time-element. 
- `Term1` and `Term2` are `string`, and should not be identical. They can take following values `[dx, dy, dz, dx1, dx2, dx3, x1, x2, x3, x, y, z] [dt]`. Thesevalues represent the time derivative or spatial derivatives.
- The first set denotes the gradient. `dt` denotes the time derivative. Symbolically following matrix is computed.


SYMBOLIC CALCULATION 

$${}^{4}M(I,J,a,b) = \delta {}^{a}u_{iI} \int_{Q_n} \frac{ \partial N^I T_a}{\partial t} c_{k}^{h} \frac{\partial N^J T_b}{\partial x_k} {dQ} \quad {}^{b}u_{iJ}$$

$${}^{4}M(I,J,a,b) = \delta {}^{a}u_{iI} \int_{Q_n} c_{k}^{h} \frac{\partial N^I T_a}{\partial x_k} \frac{\partial N^J T_b}{\partial t} {dQ} \quad {}^{b}u_{iJ}$$


TESTING

For all the tests `C(:,:,:) = 1.0`. The following code is used for testing.

```fortran
IF( ALLOCATED( DummyMat3 ) ) DEALLOCATE( DummyMat3 )
ALLOCATE( DummyMat3( NSD, NNS, NNT ) )

DummyMat3 = 1.0_DFP

CALL STElemSD % getConvectiveMatrix( Term1 = "dt", Term2 = "dx", C = DummyMat3 )

CALL STElemSD % getConvectiveMatrix( Term1 = "dx", Term2 = "dt", C = DummyMat3 )
```

**`NIPS = 1`, `NIPT = 1`**

```fortran
CALL STElemSD % getConvectiveMatrix_11(  Term1 = 'dt', Term2 = 'dx', C = DummyMat3 )

MATRIX STORED IN ST-ELEMENT-SHAPEDATA

NIPS ::   1  NIPT ::   1

-------------------------------------------------

4D MATRIX, MAT4(:,:,:,:) ::

Mat4( :, :,  1, 1 )

  0.2500000       0.000000     -0.2500000       0.000000
  0.2500000       0.000000     -0.2500000       0.000000
  0.2500000       0.000000     -0.2500000       0.000000
  0.2500000       0.000000     -0.2500000       0.000000

Mat4( :, :,  1, 2 )

  0.2500000       0.000000     -0.2500000       0.000000
  0.2500000       0.000000     -0.2500000       0.000000
  0.2500000       0.000000     -0.2500000       0.000000
  0.2500000       0.000000     -0.2500000       0.000000

Mat4( :, :,  2, 1 )

 -0.2500000       0.000000      0.2500000       0.000000
 -0.2500000       0.000000      0.2500000       0.000000
 -0.2500000       0.000000      0.2500000       0.000000
 -0.2500000       0.000000      0.2500000       0.000000

Mat4( :, :,  2, 2 )

 -0.2500000       0.000000      0.2500000       0.000000
 -0.2500000       0.000000      0.2500000       0.000000
 -0.2500000       0.000000      0.2500000       0.000000
 -0.2500000       0.000000      0.2500000       0.000000
```

```fortran
CALL STElemSD % getConvectiveMatrix_11(  Term1 = 'dx', Term2 = 'dt', C = DummyMat3 )

MATRIX STORED IN ST-ELEMENT-SHAPEDATA

NIPS ::   1  NIPT ::   1

-------------------------------------------------

4D MATRIX, MAT4(:,:,:,:) ::

Mat4( :, :,  1, 1 )

  0.2500000      0.2500000      0.2500000      0.2500000
   0.000000       0.000000       0.000000       0.000000
 -0.2500000     -0.2500000     -0.2500000     -0.2500000
   0.000000       0.000000       0.000000       0.000000

Mat4( :, :,  1, 2 )

 -0.2500000     -0.2500000     -0.2500000     -0.2500000
   0.000000       0.000000       0.000000       0.000000
  0.2500000      0.2500000      0.2500000      0.2500000
   0.000000       0.000000       0.000000       0.000000

Mat4( :, :,  2, 1 )

  0.2500000      0.2500000      0.2500000      0.2500000
   0.000000       0.000000       0.000000       0.000000
 -0.2500000     -0.2500000     -0.2500000     -0.2500000
   0.000000       0.000000       0.000000       0.000000

Mat4( :, :,  2, 2 )

 -0.2500000     -0.2500000     -0.2500000     -0.2500000
   0.000000       0.000000       0.000000       0.000000
  0.2500000      0.2500000      0.2500000      0.2500000
   0.000000       0.000000       0.000000       0.000000

```

> As expected the matrices are transpose of each other, i.e. ${}^{4}M(I,J,a,b) = {}^{4}M(J,I,b,a)$. Therefore, we will consider only the first one.

**`NIPS = 1`, `NIPT = 2`**

```fortran
CALL STElemSD % getConvectiveMatrix_11(  Term1 = 'dt', Term2 = 'dx', C = DummyMat3 )

MATRIX STORED IN ST-ELEMENT-SHAPEDATA

NIPS ::   1  NIPT ::   2

-------------------------------------------------

4D MATRIX, MAT4(:,:,:,:) ::

Mat4( :, :,  1, 1 )

  0.2500000       0.000000     -0.2500000       0.000000
  0.2500000       0.000000     -0.2500000       0.000000
  0.2500000       0.000000     -0.2500000       0.000000
  0.2500000       0.000000     -0.2500000       0.000000

Mat4( :, :,  1, 2 )

  0.2500000       0.000000     -0.2500000       0.000000
  0.2500000       0.000000     -0.2500000       0.000000
  0.2500000       0.000000     -0.2500000       0.000000
  0.2500000       0.000000     -0.2500000       0.000000

Mat4( :, :,  2, 1 )

 -0.2500000       0.000000      0.2500000       0.000000
 -0.2500000       0.000000      0.2500000       0.000000
 -0.2500000       0.000000      0.2500000       0.000000
 -0.2500000       0.000000      0.2500000       0.000000

Mat4( :, :,  2, 2 )

 -0.2500000       0.000000      0.2500000       0.000000
 -0.2500000       0.000000      0.2500000       0.000000
 -0.2500000       0.000000      0.2500000       0.000000
 -0.2500000       0.000000      0.2500000       0.000000
```

> The matrix is rank deficient, and there is no effect of increasing the `NIPT`. Therefore, we must increase the `NIPS`

**`NIPS = 4, NIPT = 1`**

```fortran
CALL STElemSD % getConvectiveMatrix_11(  Term1 = 'dt', Term2 = 'dx', C = DummyMat3 )

MATRIX STORED IN ST-ELEMENT-SHAPEDATA

NIPS ::   4  NIPT ::   1

-------------------------------------------------

4D MATRIX, MAT4(:,:,:,:) ::

Mat4( :, :,  1, 1 )

  0.3333333     -0.8333333E-01 -0.1666667     -0.8333333E-01
  0.2500000       0.000000     -0.2500000       0.000000
  0.1666667      0.8333333E-01 -0.3333333      0.8333333E-01
  0.2500000       0.000000     -0.2500000       0.000000

Mat4( :, :,  1, 2 )

  0.3333333     -0.8333333E-01 -0.1666667     -0.8333333E-01
  0.2500000       0.000000     -0.2500000       0.000000
  0.1666667      0.8333333E-01 -0.3333333      0.8333333E-01
  0.2500000       0.000000     -0.2500000       0.000000

Mat4( :, :,  2, 1 )

 -0.3333333      0.8333333E-01  0.1666667      0.8333333E-01
 -0.2500000       0.000000      0.2500000       0.000000
 -0.1666667     -0.8333333E-01  0.3333333     -0.8333333E-01
 -0.2500000       0.000000      0.2500000       0.000000

Mat4( :, :,  2, 2 )

 -0.3333333      0.8333333E-01  0.1666667      0.8333333E-01
 -0.2500000       0.000000      0.2500000       0.000000
 -0.1666667     -0.8333333E-01  0.3333333     -0.8333333E-01
 -0.2500000       0.000000      0.2500000       0.000000
```

**`NIPS = 4, NIPT = 2`**

```fortran
CALL STElemSD % getConvectiveMatrix_11(  Term1 = 'dt', Term2 = 'dx', C = DummyMat3 )

MATRIX STORED IN ST-ELEMENT-SHAPEDATA

NIPS ::   4  NIPT ::   2

-------------------------------------------------

4D MATRIX, MAT4(:,:,:,:) ::

Mat4( :, :,  1, 1 )

  0.3333333     -0.8333333E-01 -0.1666667     -0.8333333E-01
  0.2500000       0.000000     -0.2500000       0.000000
  0.1666667      0.8333333E-01 -0.3333333      0.8333333E-01
  0.2500000       0.000000     -0.2500000       0.000000

Mat4( :, :,  1, 2 )

  0.3333333     -0.8333333E-01 -0.1666667     -0.8333333E-01
  0.2500000       0.000000     -0.2500000       0.000000
  0.1666667      0.8333333E-01 -0.3333333      0.8333333E-01
  0.2500000       0.000000     -0.2500000       0.000000

Mat4( :, :,  2, 1 )

 -0.3333333      0.8333333E-01  0.1666667      0.8333333E-01
 -0.2500000       0.000000      0.2500000       0.000000
 -0.1666667     -0.8333333E-01  0.3333333     -0.8333333E-01
 -0.2500000       0.000000      0.2500000       0.000000

Mat4( :, :,  2, 2 )

 -0.3333333      0.8333333E-01  0.1666667      0.8333333E-01
 -0.2500000       0.000000      0.2500000       0.000000
 -0.1666667     -0.8333333E-01  0.3333333     -0.8333333E-01
 -0.2500000       0.000000      0.2500000       0.000000
```

> `NIPS = 4, NIPT = 1` results same as `NIPS = 4, NIPT = 2`, because in the present case mesh is not moving so the integrand is linear in time therefore we need only one integration point for exact integration. Note that this may vary when mesh is also moving.

**`NIPS = 9, NIPT = 1`**

```fortran
CALL STElemSD % getConvectiveMatrix_11(  Term1 = 'dt', Term2 = 'dx', C = DummyMat3 )

MATRIX STORED IN ST-ELEMENT-SHAPEDATA

NIPS ::   9  NIPT ::   1

-------------------------------------------------

4D MATRIX, MAT4(:,:,:,:) ::

Mat4( :, :,  1, 1 )

  0.3333333     -0.8333333E-01 -0.1666667     -0.8333333E-01
  0.2500000      0.8673617E-18 -0.2500000      0.8673617E-18
  0.1666667      0.8333333E-01 -0.3333333      0.8333333E-01
  0.2500000      0.8673617E-18 -0.2500000      0.8673617E-18

Mat4( :, :,  1, 2 )

  0.3333333     -0.8333333E-01 -0.1666667     -0.8333333E-01
  0.2500000      0.8673617E-18 -0.2500000      0.8673617E-18
  0.1666667      0.8333333E-01 -0.3333333      0.8333333E-01
  0.2500000      0.8673617E-18 -0.2500000      0.8673617E-18

Mat4( :, :,  2, 1 )

 -0.3333333      0.8333333E-01  0.1666667      0.8333333E-01
 -0.2500000     -0.8673617E-18  0.2500000     -0.8673617E-18
 -0.1666667     -0.8333333E-01  0.3333333     -0.8333333E-01
 -0.2500000     -0.8673617E-18  0.2500000     -0.8673617E-18

Mat4( :, :,  2, 2 )

 -0.3333333      0.8333333E-01  0.1666667      0.8333333E-01
 -0.2500000     -0.8673617E-18  0.2500000     -0.8673617E-18
 -0.1666667     -0.8333333E-01  0.3333333     -0.8333333E-01
 -0.2500000     -0.8673617E-18  0.2500000     -0.8673617E-18
```

Once again the results are the same. Therefore, lets stop here and move to next method.

### getConvectiveMatrix_12()

INTERFACE

```fortran
 SUBROUTINE getConvectiveMatrix_12( Obj, C, Term1, Term2 )

    USE Utility, ONLY : OUTERPROD

    CLASS( STConvectiveMatrix_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), DIMENSION( :, : ), INTENT( IN ) :: C
    CHARACTER( LEN = * ), INTENT( IN ) :: Term1, Term2
```

DESCRIPTION

- This subroutine perform the same task as `getConvectiveMatrix_11()`
- In this case, `C(:,:)` is a two-dimensional array. `C` denotes the spatial-nodal values of *convective velocity*. This case means that `convective velocity` does not change with time, however it changes with spatial coordiantes. The shape of `C` should be `(NSD, NNS)`. The first index denotes the spatial coordinate, and the second index denotes the spatial-node.

The following code snippet can be used to perform this task.

```fortran
CALL Obj % getConvectiveMatrix( C = C, Term1 = "dt", Term2 = "dx")
CALL Obj % getConvectiveMatrix( C = C, Term1 = "dx", Term2 = "dt")
```
SYMBOLIC CALCULATION 

$${}^{4}M(I,J,a,b) = \delta {}^{a}u_{iI} \int_{Q_n} \frac{ \partial N^I T_a}{\partial t} c_{k}^{h} \frac{\partial N^J T_b}{\partial x_k} {dQ} \quad {}^{b}u_{iJ}$$

$${}^{4}M(I,J,a,b) = \delta {}^{a}u_{iI} \int_{Q_n} c_{k}^{h} \frac{\partial N^I T_a}{\partial x_k} \frac{\partial N^J T_b}{\partial t} {dQ} \quad {}^{b}u_{iJ}$$

TESTING

For all the tests `C(:,:) = 1.0`. The following code is used for testing.

```fortran
IF( ALLOCATED( DummyMat2 ) ) DEALLOCATE( DummyMat2 )
ALLOCATE( DummyMat2( NSD, NNS ) )

DummyMat2 = 1.0_DFP

CALL STElemSD % getConvectiveMatrix( Term1 = "dt", Term2 = "dx", C = DummyMat2 )

```

**`NIPS = 4`, `NIPT = 1`**

```fortran
CALL STElemSD % getConvectiveMatrix_12(  Term1 = 'dt', Term2 = 'dx', C = DummyMat2 )

MATRIX STORED IN ST-ELEMENT-SHAPEDATA

NIPS ::   4  NIPT ::   1

-------------------------------------------------

 _4D MATRIX, MAT4(:,:,:,:) ::

Mat4( :, :,  1, 1 )

  0.3333333     -0.8333333E-01 -0.1666667     -0.8333333E-01
  0.2500000       0.000000     -0.2500000       0.000000
  0.1666667      0.8333333E-01 -0.3333333      0.8333333E-01
  0.2500000       0.000000     -0.2500000       0.000000

Mat4( :, :,  1, 2 )

  0.3333333     -0.8333333E-01 -0.1666667     -0.8333333E-01
  0.2500000       0.000000     -0.2500000       0.000000
  0.1666667      0.8333333E-01 -0.3333333      0.8333333E-01
  0.2500000       0.000000     -0.2500000       0.000000

Mat4( :, :,  2, 1 )

 -0.3333333      0.8333333E-01  0.1666667      0.8333333E-01
 -0.2500000       0.000000      0.2500000       0.000000
 -0.1666667     -0.8333333E-01  0.3333333     -0.8333333E-01
 -0.2500000       0.000000      0.2500000       0.000000

Mat4( :, :,  2, 2 )

 -0.3333333      0.8333333E-01  0.1666667      0.8333333E-01
 -0.2500000       0.000000      0.2500000       0.000000
 -0.1666667     -0.8333333E-01  0.3333333     -0.8333333E-01
 -0.2500000       0.000000      0.2500000       0.000000
```

### getConvectiveMatrix_27()

INTERFACE

```fortran
 SUBROUTINE getConvectiveMatrix_27( Obj, C, Term1, Term2 )

    USE Utility, ONLY : OUTERPROD

    CLASS( STConvectiveMatrix_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), DIMENSION( : ), INTENT( IN ) :: C
    CHARACTER( LEN = * ), INTENT( IN ) :: Term1, Term2
```

DESCRIPTION

- This subroutine perform the same task as `getConvectiveMatrix_11()`.
- In this case `C(:)` is a one-dimensional array. `C` denotes the components of *convective velocity*. This case means that `convective velocity` is constant inside the element and does not change in space-time domain. The shape of `C` should be `(NSD)`

The following code snippet can be used to perform this task.

```fortran
CALL Obj % getConvectiveMatrix( C = C, Term1 = "dt", Term2 = "dx")
CALL Obj % getConvectiveMatrix( C = C, Term1 = "dx", Term2 = "dt")
```
SYMBOLIC CALCULATION 

$${}^{4}M(I,J,a,b) = \delta {}^{a}u_{iI} \int_{Q_n} \frac{ \partial N^I T_a}{\partial t} c_{k}^{h} \frac{\partial N^J T_b}{\partial x_k} {dQ} \quad {}^{b}u_{iJ}$$

$${}^{4}M(I,J,a,b) = \delta {}^{a}u_{iI} \int_{Q_n} c_{k}^{h} \frac{\partial N^I T_a}{\partial x_k} \frac{\partial N^J T_b}{\partial t} {dQ} \quad {}^{b}u_{iJ}$$

TESTING

For all the tests `C(:) = 1.0`. The following code is used for testing.

```fortran
IF( ALLOCATED( DummyVec ) ) DEALLOCATE( DummyVec )
ALLOCATE( DummyVec( NSD ) )

DummyVec = 1.0_DFP

CALL STElemSD % getConvectiveMatrix( Term1 = "dt", Term2 = "dx", C = DummyVec )

CALL STElemSD % DISPLAYMATRIX4( )
```

**`NIPS = 4`, `NIPT = 1`**

```fortran
CALL STElemSD % getConvectiveMatrix_27(  Term1 = 'dt', Term2 = 'dx', C = DummyVec )

MATRIX STORED IN ST-ELEMENT-SHAPEDATA

NIPS ::   4  NIPT ::   1

-------------------------------------------------

4D MATRIX, MAT4(:,:,:,:) ::

Mat4( :, :,  1, 1 )

  0.3333333     -0.8333333E-01 -0.1666667     -0.8333333E-01
  0.2500000       0.000000     -0.2500000       0.000000
  0.1666667      0.8333333E-01 -0.3333333      0.8333333E-01
  0.2500000       0.000000     -0.2500000       0.000000

Mat4( :, :,  1, 2 )

  0.3333333     -0.8333333E-01 -0.1666667     -0.8333333E-01
  0.2500000       0.000000     -0.2500000       0.000000
  0.1666667      0.8333333E-01 -0.3333333      0.8333333E-01
  0.2500000       0.000000     -0.2500000       0.000000

Mat4( :, :,  2, 1 )

 -0.3333333      0.8333333E-01  0.1666667      0.8333333E-01
 -0.2500000       0.000000      0.2500000       0.000000
 -0.1666667     -0.8333333E-01  0.3333333     -0.8333333E-01
 -0.2500000       0.000000      0.2500000       0.000000

Mat4( :, :,  2, 2 )

 -0.3333333      0.8333333E-01  0.1666667      0.8333333E-01
 -0.2500000       0.000000      0.2500000       0.000000
 -0.1666667     -0.8333333E-01  0.3333333     -0.8333333E-01
 -0.2500000       0.000000      0.2500000       0.000000
```

### getConvectiveMatrix_13()

INTERFACE

```fortran
 SUBROUTINE getConvectiveMatrix_13( Obj, C, Term1, Term2, CType )

    USE Utility, ONLY : OUTERPROD

    CLASS( STConvectiveMatrix_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), DIMENSION( :, : ), INTENT( IN ) :: C
    CHARACTER( LEN = * ), INTENT( IN ) :: Term1, Term2
    CHARACTER( LEN = * ), INTENT( IN ) :: CType
```

DESCRIPTION

- This subroutine perform the same task as `getConvectiveMatrix_11()`.
- In this case, `C(:, :)` is a 2-dimensional array. 
- `Ctype` variable is an important parameter. If the value of `Ctype` is in the set `[Nodal, NodalValues, Nodal Values, Space Nodal Values, SpaeNodalValues]` then this subroutine calls the `getConvectiveMatrix_12()`. 
- If the value of `Ctype` is in the set `[Integration, IntegrationPoints, Integration Points, Quad, QuadPoints, Quad Points]` then `C` denotes the value of *convective velocity* at spatial-integration (quadrature) points. 
- This case means that `convective velocity` is constant in time domain but varying in the space domain. The shape of `C` should be `(NSD, NIPS)`

The following code snippet can be used to perform this task.

```fortran
CALL Obj % getConvectiveMatrix( C = C, Term1 = "dt", Term2 = "dx", CType = "Quad")
CALL Obj % getConvectiveMatrix( C = C, Term1 = "dx", Term2 = "dt", Ctype = "Quad")
```
SYMBOLIC CALCULATION 

$${}^{4}M(I,J,a,b) = \delta {}^{a}u_{iI} \int_{Q_n} \frac{ \partial N^I T_a}{\partial t} c_{k}^{h} \frac{\partial N^J T_b}{\partial x_k} {dQ} \quad {}^{b}u_{iJ}$$

$${}^{4}M(I,J,a,b) = \delta {}^{a}u_{iI} \int_{Q_n} c_{k}^{h} \frac{\partial N^I T_a}{\partial x_k} \frac{\partial N^J T_b}{\partial t} {dQ} \quad {}^{b}u_{iJ}$$

TESTING

For all the tests `C(:, :) = 1.0`. The following code is used for testing.

```fortran
IF( ALLOCATED( DummyMat2 ) ) DEALLOCATE( DummyMat2 )
ALLOCATE( DummyMat2( NSD, NIPS ) )

DummyMat2 = 1.0_DFP

CALL STElemSD % getConvectiveMatrix( Term1 = "dt", Term2 = "dx", C = DummyMat2, CType = "Quad" )
CALL STElemSD % DISPLAYMATRIX4( )
```

**`NIPS = 4`, `NIPT = 1`**

```fortran
CALL STElemSD % getConvectiveMatrix_13(  Term1 = 'dt', Term2 = 'dx', C = DummyMat2 )

MATRIX STORED IN ST-ELEMENT-SHAPEDATA

NIPS ::   4  NIPT ::   1

-------------------------------------------------

4D MATRIX, MAT4(:,:,:,:) ::

Mat4( :, :,  1, 1 )

  0.3333333     -0.8333333E-01 -0.1666667     -0.8333333E-01
  0.2500000       0.000000     -0.2500000       0.000000
  0.1666667      0.8333333E-01 -0.3333333      0.8333333E-01
  0.2500000       0.000000     -0.2500000       0.000000

Mat4( :, :,  1, 2 )

  0.3333333     -0.8333333E-01 -0.1666667     -0.8333333E-01
  0.2500000       0.000000     -0.2500000       0.000000
  0.1666667      0.8333333E-01 -0.3333333      0.8333333E-01
  0.2500000       0.000000     -0.2500000       0.000000

Mat4( :, :,  2, 1 )

 -0.3333333      0.8333333E-01  0.1666667      0.8333333E-01
 -0.2500000       0.000000      0.2500000       0.000000
 -0.1666667     -0.8333333E-01  0.3333333     -0.8333333E-01
 -0.2500000       0.000000      0.2500000       0.000000

Mat4( :, :,  2, 2 )

 -0.3333333      0.8333333E-01  0.1666667      0.8333333E-01
 -0.2500000       0.000000      0.2500000       0.000000
 -0.1666667     -0.8333333E-01  0.3333333     -0.8333333E-01
 -0.2500000       0.000000      0.2500000       0.000000
```

### getConvectiveMatrix_14()

INTERFACE

```fortran
 SUBROUTINE getConvectiveMatrix_14( Obj, C, Term1, Term2, CType)

    USE Utility, ONLY : OUTERPROD

    CLASS( STConvectiveMatrix_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), DIMENSION( :, :, : ), INTENT( IN ) :: C
    CHARACTER( LEN = * ), INTENT( IN ) :: Term1, Term2
    CHARACTER( LEN = * ), INTENT( IN ) :: CType
```

DESCRIPTION

This subroutien performs the same task as `getConvectiveMatrix_11()`, but in this case `C(:, :, :)` is a 3-dimensional array. `Ctype` variable is an important parameter. If the value of `Ctype` is in the set `[Nodal, NodalValues, Nodal Values, STNodalValues, ST Nodal Values]` then this subroutine calls the `getConvectiveMatrix_11()`. If the value of `Ctype` is in the set `[Integration, IntegrationPoints, Integration Points, Quad, QuadPoints, Quad Points]` then `C` denotes the value of *convective velocity* at space-time integration (quadrature) points. This case means that `convective velocity` is changes in both space and time domain. The shape of `C` should be `(NSD, NIPS, NIPT)`

The following code snippet can be used to perform this task.

```fortran
CALL Obj % getConvectiveMatrix( C = C, Term1 = "dt", Term2 = "dx", CType = "Quad")
CALL Obj % getConvectiveMatrix( C = C, Term1 = "dx", Term2 = "dt", Ctype = "Quad")
```

SYMBOLIC CALCULATION 

$${}^{4}M(I,J,a,b) = \delta {}^{a}u_{iI} \int_{Q_n} \frac{ \partial N^I T_a}{\partial t} c_{k}^{h} \frac{\partial N^J T_b}{\partial x_k} {dQ} \quad {}^{b}u_{iJ}$$

$${}^{4}M(I,J,a,b) = \delta {}^{a}u_{iI} \int_{Q_n} c_{k}^{h} \frac{\partial N^I T_a}{\partial x_k} \frac{\partial N^J T_b}{\partial t} {dQ} \quad {}^{b}u_{iJ}$$

TESTING

For all the tests `C(:, :, :) = 1.0`. The following code is used for testing.

```fortran
IF( ALLOCATED( DummyMat3 ) ) DEALLOCATE( DummyMat3 )
ALLOCATE( DummyMat3( NSD, NIPS, NIPT ) )

DummyMat3 = 1.0_DFP

CALL STElemSD % getConvectiveMatrix( Term1 = "dt", Term2 = "dx", C = DummyMat3, CType = "Quad" )
CALL STElemSD % DISPLAYMATRIX4( )
```

**`NIPS = 4`, `NIPT = 1`**

```fortran
CALL STElemSD % getConvectiveMatrix_14(  Term1 = 'dt', Term2 = 'dx', C = DummyMat3 )

MATRIX STORED IN ST-ELEMENT-SHAPEDATA

NIPS ::   4  NIPT ::   1

-------------------------------------------------

4D MATRIX, MAT4(:,:,:,:) ::

Mat4( :, :,  1, 1 )

  0.3333333     -0.8333333E-01 -0.1666667     -0.8333333E-01
  0.2500000       0.000000     -0.2500000       0.000000
  0.1666667      0.8333333E-01 -0.3333333      0.8333333E-01
  0.2500000       0.000000     -0.2500000       0.000000

Mat4( :, :,  1, 2 )

  0.3333333     -0.8333333E-01 -0.1666667     -0.8333333E-01
  0.2500000       0.000000     -0.2500000       0.000000
  0.1666667      0.8333333E-01 -0.3333333      0.8333333E-01
  0.2500000       0.000000     -0.2500000       0.000000

Mat4( :, :,  2, 1 )

 -0.3333333      0.8333333E-01  0.1666667      0.8333333E-01
 -0.2500000       0.000000      0.2500000       0.000000
 -0.1666667     -0.8333333E-01  0.3333333     -0.8333333E-01
 -0.2500000       0.000000      0.2500000       0.000000

Mat4( :, :,  2, 2 )

 -0.3333333      0.8333333E-01  0.1666667      0.8333333E-01
 -0.2500000       0.000000      0.2500000       0.000000
 -0.1666667     -0.8333333E-01  0.3333333     -0.8333333E-01
 -0.2500000       0.000000      0.2500000       0.000000
```

### getConvectiveMatrix_15()

INTERFACE

```fortran
 SUBROUTINE getConvectiveMatrix_15( Obj, C, Term1, Term2, nCopy )

    USE Utility, ONLY : OUTERPROD

    CLASS( STConvectiveMatrix_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), DIMENSION( :, :, : ), INTENT( IN ) :: C
    INTEGER( I4B ), INTENT( IN ) :: nCopy
    CHARACTER( LEN = * ), INTENT( IN ) :: Term1, Term2
```

DESCRIPTION

Symbolically, this subroutine does the following,

**`Term1 = dt, Term2 = dx`**

$${}^{4}M(I,J,a,b) = {}^{a}\delta u_{iI} \int_{Q_n} \frac{\partial N^I T_a}{\partial t} c_{k}^{h} \frac{\partial N^J T_b}{\partial x_k}{dQ} ({}^{b}u_{iJ})$$

**`Term1 = dx, Term2 = dt`**

$${}^{4}M(I,J,a,b) = {}^{a}\delta u_{iI} \int_{Q_n} c_{k}^{h} \frac{\partial N^I T_a}{\partial x_k} \frac{\partial N^J T_b}{\partial t} {dQ} ({}^{b}u_{iJ})$$

Here `C(:, :, :)` is a 3-dimensional array that represents the space-time nodal values of *convective velocity* $c(x,t)$. The shape of `C` array should be `(NSD, NNS, NNT)`. `Term1` and `Term2` has same meaning as defined in above methods. `nCopy` here defines the number of unknowns i.e. $u_i, i=1 \cdots nCopy$. In this case, the shape of ${}^{4}M(:,:,:,:)$ will be $(nCopy \times NNS, nCopy \times NNS, NNT, NNT)$. The structure of ${}^{4}M(:,:,a,b)$ is shown below.

$$
{}^{4}M(:,:,a,b)=\begin{bmatrix}
    \textbf{M} & \textbf{0} & \cdots & \textbf{0} \\
    \textbf{0} & \textbf{M} & \cdots & \textbf{0} \\
    \vdots     & \vdots     & \ddots     & \vdots \\
    \textbf{0} & \textbf{0} & \cdots & \textbf{M} \\
\end{bmatrix}$$

> Here all $\textbf{M}$ are identical, and has shape `(NNS, NNS)`. For more details see the notes.

The following code snippet can be used to perform this task.

```fortran
CALL Obj % getConvectiveMatrix( C = C, Term1 = "dt", Term2 = "dx", nCopy = 2 )
CALL Obj % getConvectiveMatrix( C = C, Term1 = "dx", Term2 = "dt", nCopy = 2 )
```

TESTING

For all the tests `C(:, :, :) = 1.0`. The following code is used for testing.

```fortran

IF( ALLOCATED( DummyMat3 ) ) DEALLOCATE( DummyMat3 )
ALLOCATE( DummyMat3( NSD, NNS, NNT ) )

DummyMat3 = 1.0_DFP

CALL STElemSD % getConvectiveMatrix( Term1 = "dt", Term2 = "dx", C = DummyMat3, nCopy = 2 )
CALL STElemSD % DISPLAYMATRIX4( )
```

**`NIPS = 4`, `NIPT = 1`**

```fortran
CALL STElemSD % getConvectiveMatrix_15(  Term1 = 'dt', Term2 = 'dx', C = DummyMat3, nCopy = 2 )

MATRIX STORED IN ST-ELEMENT-SHAPEDATA

NIPS ::   4  NIPT ::   1

-------------------------------------------------

4D MATRIX, MAT4(:,:,:,:) ::

Mat4( :, :,  1, 1 )

  0.3333333     -0.8333333E-01 -0.1666667     -0.8333333E-01   0.000000       0.000000       0.000000       0.000000
  0.2500000       0.000000     -0.2500000       0.000000       0.000000       0.000000       0.000000       0.000000
  0.1666667      0.8333333E-01 -0.3333333      0.8333333E-01   0.000000       0.000000       0.000000       0.000000
  0.2500000       0.000000     -0.2500000       0.000000       0.000000       0.000000       0.000000       0.000000
   0.000000       0.000000       0.000000       0.000000      0.3333333     -0.8333333E-01 -0.1666667     -0.8333333E-01
   0.000000       0.000000       0.000000       0.000000      0.2500000       0.000000     -0.2500000       0.000000
   0.000000       0.000000       0.000000       0.000000      0.1666667      0.8333333E-01 -0.3333333      0.8333333E-01
   0.000000       0.000000       0.000000       0.000000      0.2500000       0.000000     -0.2500000       0.000000

Mat4( :, :,  1, 2 )

  0.3333333     -0.8333333E-01 -0.1666667     -0.8333333E-01   0.000000       0.000000       0.000000       0.000000
  0.2500000       0.000000     -0.2500000       0.000000       0.000000       0.000000       0.000000       0.000000
  0.1666667      0.8333333E-01 -0.3333333      0.8333333E-01   0.000000       0.000000       0.000000       0.000000
  0.2500000       0.000000     -0.2500000       0.000000       0.000000       0.000000       0.000000       0.000000
   0.000000       0.000000       0.000000       0.000000      0.3333333     -0.8333333E-01 -0.1666667     -0.8333333E-01
   0.000000       0.000000       0.000000       0.000000      0.2500000       0.000000     -0.2500000       0.000000
   0.000000       0.000000       0.000000       0.000000      0.1666667      0.8333333E-01 -0.3333333      0.8333333E-01
   0.000000       0.000000       0.000000       0.000000      0.2500000       0.000000     -0.2500000       0.000000

Mat4( :, :,  2, 1 )

 -0.3333333      0.8333333E-01  0.1666667      0.8333333E-01   0.000000       0.000000       0.000000       0.000000
 -0.2500000       0.000000      0.2500000       0.000000       0.000000       0.000000       0.000000       0.000000
 -0.1666667     -0.8333333E-01  0.3333333     -0.8333333E-01   0.000000       0.000000       0.000000       0.000000
 -0.2500000       0.000000      0.2500000       0.000000       0.000000       0.000000       0.000000       0.000000
   0.000000       0.000000       0.000000       0.000000     -0.3333333      0.8333333E-01  0.1666667      0.8333333E-01
   0.000000       0.000000       0.000000       0.000000     -0.2500000       0.000000      0.2500000       0.000000
   0.000000       0.000000       0.000000       0.000000     -0.1666667     -0.8333333E-01  0.3333333     -0.8333333E-01
   0.000000       0.000000       0.000000       0.000000     -0.2500000       0.000000      0.2500000       0.000000

Mat4( :, :,  2, 2 )

 -0.3333333      0.8333333E-01  0.1666667      0.8333333E-01   0.000000       0.000000       0.000000       0.000000
 -0.2500000       0.000000      0.2500000       0.000000       0.000000       0.000000       0.000000       0.000000
 -0.1666667     -0.8333333E-01  0.3333333     -0.8333333E-01   0.000000       0.000000       0.000000       0.000000
 -0.2500000       0.000000      0.2500000       0.000000       0.000000       0.000000       0.000000       0.000000
   0.000000       0.000000       0.000000       0.000000     -0.3333333      0.8333333E-01  0.1666667      0.8333333E-01
   0.000000       0.000000       0.000000       0.000000     -0.2500000       0.000000      0.2500000       0.000000
   0.000000       0.000000       0.000000       0.000000     -0.1666667     -0.8333333E-01  0.3333333     -0.8333333E-01
   0.000000       0.000000       0.000000       0.000000     -0.2500000       0.000000      0.2500000       0.000000
```

### getConvectiveMatrix_16()

INTERFACE

```fortran
 SUBROUTINE getConvectiveMatrix_16( Obj, C, Term1, Term2, nCopy )

    USE Utility, ONLY : OUTERPROD

    CLASS( STConvectiveMatrix_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), DIMENSION( :, : ), INTENT( IN ) :: C
    INTEGER( I4B ), INTENT( IN ) :: nCopy
    CHARACTER( LEN = * ), INTENT( IN ) :: Term1, Term2
```

DESCRIPTION

The subroutine performs the same task as `getConvectiveMatrix_15()`. However here `C(:,:)` is a two dimensional array, and represents the spatial-nodal values. In this case the *convective velocity* $c(x,t)$ does not chage with time, and change only in space domain. For details see the notes or `getConvectiveMatrix_15()`. The shape of `C(:,:)` should be `(NSD, NNS)`

The following code snippet can be used to perform this task.

```fortran
CALL Obj % getConvectiveMatrix( C = C, Term1 = "dt", Term2 = "dx", nCopy = 2 )
CALL Obj % getConvectiveMatrix( C = C, Term1 = "dx", Term2 = "dt", nCopy = 2 )
```

SYMBOLIC CALCULATION 

$${}^{4}M(I,J,a,b) = \delta {}^{a}u_{iI} \int_{Q_n} \frac{ \partial N^I T_a}{\partial t} c_{k}^{h} \frac{\partial N^J T_b}{\partial x_k} {dQ} \quad {}^{b}u_{iJ}$$

$${}^{4}M(I,J,a,b) = \delta {}^{a}u_{iI} \int_{Q_n} c_{k}^{h} \frac{\partial N^I T_a}{\partial x_k} \frac{\partial N^J T_b}{\partial t} {dQ} \quad {}^{b}u_{iJ}$$

TESTING

For all the tests `C(:, :) = 1.0`. The following code is used for testing.

```fortran
IF( ALLOCATED( DummyMat2 ) ) DEALLOCATE( DummyMat2 )
ALLOCATE( DummyMat2( NSD, NNS ) )

DummyMat2 = 1.0_DFP

CALL STElemSD % getConvectiveMatrix( Term1 = "dt", Term2 = "dx", C = DummyMat2, nCopy = 2 )
CALL STElemSD % DISPLAYMATRIX4( )
```

**`NIPS = 4`, `NIPT = 1`**

```fortran
CALL STElemSD % getConvectiveMatrix_16(  Term1 = 'dt', Term2 = 'dx', C = DummyMat2, nCopy = 2 )

MATRIX STORED IN ST-ELEMENT-SHAPEDATA

NIPS ::   4  NIPT ::   1

-------------------------------------------------

4D MATRIX, MAT4(:,:,:,:) ::

Mat4( :, :,  1, 1 )

  0.3333333     -0.8333333E-01 -0.1666667     -0.8333333E-01   0.000000       0.000000       0.000000       0.000000
  0.2500000       0.000000     -0.2500000       0.000000       0.000000       0.000000       0.000000       0.000000
  0.1666667      0.8333333E-01 -0.3333333      0.8333333E-01   0.000000       0.000000       0.000000       0.000000
  0.2500000       0.000000     -0.2500000       0.000000       0.000000       0.000000       0.000000       0.000000
   0.000000       0.000000       0.000000       0.000000      0.3333333     -0.8333333E-01 -0.1666667     -0.8333333E-01
   0.000000       0.000000       0.000000       0.000000      0.2500000       0.000000     -0.2500000       0.000000
   0.000000       0.000000       0.000000       0.000000      0.1666667      0.8333333E-01 -0.3333333      0.8333333E-01
   0.000000       0.000000       0.000000       0.000000      0.2500000       0.000000     -0.2500000       0.000000

Mat4( :, :,  1, 2 )

  0.3333333     -0.8333333E-01 -0.1666667     -0.8333333E-01   0.000000       0.000000       0.000000       0.000000
  0.2500000       0.000000     -0.2500000       0.000000       0.000000       0.000000       0.000000       0.000000
  0.1666667      0.8333333E-01 -0.3333333      0.8333333E-01   0.000000       0.000000       0.000000       0.000000
  0.2500000       0.000000     -0.2500000       0.000000       0.000000       0.000000       0.000000       0.000000
   0.000000       0.000000       0.000000       0.000000      0.3333333     -0.8333333E-01 -0.1666667     -0.8333333E-01
   0.000000       0.000000       0.000000       0.000000      0.2500000       0.000000     -0.2500000       0.000000
   0.000000       0.000000       0.000000       0.000000      0.1666667      0.8333333E-01 -0.3333333      0.8333333E-01
   0.000000       0.000000       0.000000       0.000000      0.2500000       0.000000     -0.2500000       0.000000

Mat4( :, :,  2, 1 )

 -0.3333333      0.8333333E-01  0.1666667      0.8333333E-01   0.000000       0.000000       0.000000       0.000000
 -0.2500000       0.000000      0.2500000       0.000000       0.000000       0.000000       0.000000       0.000000
 -0.1666667     -0.8333333E-01  0.3333333     -0.8333333E-01   0.000000       0.000000       0.000000       0.000000
 -0.2500000       0.000000      0.2500000       0.000000       0.000000       0.000000       0.000000       0.000000
   0.000000       0.000000       0.000000       0.000000     -0.3333333      0.8333333E-01  0.1666667      0.8333333E-01
   0.000000       0.000000       0.000000       0.000000     -0.2500000       0.000000      0.2500000       0.000000
   0.000000       0.000000       0.000000       0.000000     -0.1666667     -0.8333333E-01  0.3333333     -0.8333333E-01
   0.000000       0.000000       0.000000       0.000000     -0.2500000       0.000000      0.2500000       0.000000

Mat4( :, :,  2, 2 )

 -0.3333333      0.8333333E-01  0.1666667      0.8333333E-01   0.000000       0.000000       0.000000       0.000000
 -0.2500000       0.000000      0.2500000       0.000000       0.000000       0.000000       0.000000       0.000000
 -0.1666667     -0.8333333E-01  0.3333333     -0.8333333E-01   0.000000       0.000000       0.000000       0.000000
 -0.2500000       0.000000      0.2500000       0.000000       0.000000       0.000000       0.000000       0.000000
   0.000000       0.000000       0.000000       0.000000     -0.3333333      0.8333333E-01  0.1666667      0.8333333E-01
   0.000000       0.000000       0.000000       0.000000     -0.2500000       0.000000      0.2500000       0.000000
   0.000000       0.000000       0.000000       0.000000     -0.1666667     -0.8333333E-01  0.3333333     -0.8333333E-01
   0.000000       0.000000       0.000000       0.000000     -0.2500000       0.000000      0.2500000       0.000000
```

### getConvectiveMatrix_28()

INTERFACE

```fortran
 SUBROUTINE getConvectiveMatrix_28( Obj, C, Term1, Term2, nCopy )

    USE Utility, ONLY : OUTERPROD

    CLASS( STConvectiveMatrix_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), DIMENSION( : ), INTENT( IN ) :: C
    INTEGER( I4B ), INTENT( IN ) :: nCopy
    CHARACTER( LEN = * ), INTENT( IN ) :: Term1, Term2
```

DESCRIPTION

The subroutine performs the same task as `getConvectiveMatrix_15()`. However here `C(:)` is a vector in this case, and represents the component of convective velocity. In this case the *convective velocity* $c(x,t)$ does not chage in space and time domain. For details see the notes or `getConvectiveMatrix_15()`. The shape of `C(:,:)` should be `(NSD, NNS)`

The following code snippet can be used to perform this task.

```fortran
CALL Obj % getConvectiveMatrix( C = C, Term1 = "dt", Term2 = "dx", nCopy = 2 )
CALL Obj % getConvectiveMatrix( C = C, Term1 = "dx", Term2 = "dt", nCopy = 2 )
```
SYMBOLIC CALCULATION 

$${}^{4}M(I,J,a,b) = \delta {}^{a}u_{iI} \int_{Q_n} \frac{ \partial N^I T_a}{\partial t} c_{k}^{h} \frac{\partial N^J T_b}{\partial x_k} {dQ} \quad {}^{b}u_{iJ}$$

$${}^{4}M(I,J,a,b) = \delta {}^{a}u_{iI} \int_{Q_n} c_{k}^{h} \frac{\partial N^I T_a}{\partial x_k} \frac{\partial N^J T_b}{\partial t} {dQ} \quad {}^{b}u_{iJ}$$

TESTING

For all the tests `C(:, :) = 1.0`. The following code is used for testing.

```fortran
IF( ALLOCATED( DummyVec ) ) DEALLOCATE( DummyVec )
ALLOCATE( DummyVec( NSD ) )

DummyVec = 1.0_DFP

CALL STElemSD % getConvectiveMatrix( Term1 = "dt", Term2 = "dx", C = DummyVec, nCopy = 2 )
CALL STElemSD % DISPLAYMATRIX4( )
```

**`NIPS = 4`, `NIPT = 1`**

```fortran
CALL STElemSD % getConvectiveMatrix_28(  Term1 = 'dt', Term2 = 'dx', C = DummyVec, nCopy = 2 )

MATRIX STORED IN ST-ELEMENT-SHAPEDATA

NIPS ::   4  NIPT ::   1

-------------------------------------------------

4D MATRIX, MAT4(:,:,:,:) ::

Mat4( :, :,  1, 1 )

  0.3333333     -0.8333333E-01 -0.1666667     -0.8333333E-01   0.000000       0.000000       0.000000       0.000000
  0.2500000       0.000000     -0.2500000       0.000000       0.000000       0.000000       0.000000       0.000000
  0.1666667      0.8333333E-01 -0.3333333      0.8333333E-01   0.000000       0.000000       0.000000       0.000000
  0.2500000       0.000000     -0.2500000       0.000000       0.000000       0.000000       0.000000       0.000000
   0.000000       0.000000       0.000000       0.000000      0.3333333     -0.8333333E-01 -0.1666667     -0.8333333E-01
   0.000000       0.000000       0.000000       0.000000      0.2500000       0.000000     -0.2500000       0.000000
   0.000000       0.000000       0.000000       0.000000      0.1666667      0.8333333E-01 -0.3333333      0.8333333E-01
   0.000000       0.000000       0.000000       0.000000      0.2500000       0.000000     -0.2500000       0.000000

Mat4( :, :,  1, 2 )

  0.3333333     -0.8333333E-01 -0.1666667     -0.8333333E-01   0.000000       0.000000       0.000000       0.000000
  0.2500000       0.000000     -0.2500000       0.000000       0.000000       0.000000       0.000000       0.000000
  0.1666667      0.8333333E-01 -0.3333333      0.8333333E-01   0.000000       0.000000       0.000000       0.000000
  0.2500000       0.000000     -0.2500000       0.000000       0.000000       0.000000       0.000000       0.000000
   0.000000       0.000000       0.000000       0.000000      0.3333333     -0.8333333E-01 -0.1666667     -0.8333333E-01
   0.000000       0.000000       0.000000       0.000000      0.2500000       0.000000     -0.2500000       0.000000
   0.000000       0.000000       0.000000       0.000000      0.1666667      0.8333333E-01 -0.3333333      0.8333333E-01
   0.000000       0.000000       0.000000       0.000000      0.2500000       0.000000     -0.2500000       0.000000

Mat4( :, :,  2, 1 )

 -0.3333333      0.8333333E-01  0.1666667      0.8333333E-01   0.000000       0.000000       0.000000       0.000000
 -0.2500000       0.000000      0.2500000       0.000000       0.000000       0.000000       0.000000       0.000000
 -0.1666667     -0.8333333E-01  0.3333333     -0.8333333E-01   0.000000       0.000000       0.000000       0.000000
 -0.2500000       0.000000      0.2500000       0.000000       0.000000       0.000000       0.000000       0.000000
   0.000000       0.000000       0.000000       0.000000     -0.3333333      0.8333333E-01  0.1666667      0.8333333E-01
   0.000000       0.000000       0.000000       0.000000     -0.2500000       0.000000      0.2500000       0.000000
   0.000000       0.000000       0.000000       0.000000     -0.1666667     -0.8333333E-01  0.3333333     -0.8333333E-01
   0.000000       0.000000       0.000000       0.000000     -0.2500000       0.000000      0.2500000       0.000000

Mat4( :, :,  2, 2 )

 -0.3333333      0.8333333E-01  0.1666667      0.8333333E-01   0.000000       0.000000       0.000000       0.000000
 -0.2500000       0.000000      0.2500000       0.000000       0.000000       0.000000       0.000000       0.000000
 -0.1666667     -0.8333333E-01  0.3333333     -0.8333333E-01   0.000000       0.000000       0.000000       0.000000
 -0.2500000       0.000000      0.2500000       0.000000       0.000000       0.000000       0.000000       0.000000
   0.000000       0.000000       0.000000       0.000000     -0.3333333      0.8333333E-01  0.1666667      0.8333333E-01
   0.000000       0.000000       0.000000       0.000000     -0.2500000       0.000000      0.2500000       0.000000
   0.000000       0.000000       0.000000       0.000000     -0.1666667     -0.8333333E-01  0.3333333     -0.8333333E-01
   0.000000       0.000000       0.000000       0.000000     -0.2500000       0.000000      0.2500000       0.000000
```

### getConvectiveMatrix_17()

INTERFACE

```fortran
 SUBROUTINE getConvectiveMatrix_17( Obj, C, Term1, Term2, CType, nCopy )

    USE Utility, ONLY : OUTERPROD

    CLASS( STConvectiveMatrix_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), DIMENSION( :, : ), INTENT( IN ) :: C
    CHARACTER( LEN = * ), INTENT( IN ) :: Term1, Term2, CType
    INTEGER( I4B ), INTENT( IN ) :: nCopy
```

DESCRIPTION

The subroutine performs the same task as `getConvectiveMatrix_15()`. However, in this case, `C(:, :)` is a 2D array. If `Ctype` is `Nodal` then `C` denotes the spatial-nodal values and shape of `C` must be `(NSD, NNS)`. If `Ctype` is defined as `Quad` then `C(:,:)` represents the convective velocities at *spatial-integration* points. In this method, the *convective velocity* $c(x,t)$ does not chage in time, but varies only in space domain. The shape of `C` array should be `(NSD, NIPS)`. the second index of `C` denotes the spatial-integration points. For details see the notes or `getConvectiveMatrix_15()`.

The following code snippet can be used to perform this task.

```fortran
CALL Obj % getConvectiveMatrix( C = C, Term1 = "dt", Term2 = "dx", nCopy = 2, Ctype = 'Quad' )
CALL Obj % getConvectiveMatrix( C = C, Term1 = "dx", Term2 = "dt", nCopy = 2, Ctype = 'Quad' )
```
SYMBOLIC CALCULATION 

$${}^{4}M(I,J,a,b) = \delta {}^{a}u_{iI} \int_{Q_n} \frac{ \partial N^I T_a}{\partial t} c_{k}^{h} \frac{\partial N^J T_b}{\partial x_k} {dQ} \quad {}^{b}u_{iJ}$$

$${}^{4}M(I,J,a,b) = \delta {}^{a}u_{iI} \int_{Q_n} c_{k}^{h} \frac{\partial N^I T_a}{\partial x_k} \frac{\partial N^J T_b}{\partial t} {dQ} \quad {}^{b}u_{iJ}$$

TESTING

For all the tests `C(:, :) = 1.0`. The following code is used for testing.

```fortran
IF( ALLOCATED( DummyMat2 ) ) DEALLOCATE( DummyMat2 )
ALLOCATE( DummyMat2( NSD, NIPS ) )

DummyMat2 = 1.0_DFP

CALL STElemSD % getConvectiveMatrix( Term1 = "dt", Term2 = "dx", &
C = DummyMat2, CType = "Quad", nCopy = 2 )
CALL STElemSD % DISPLAYMATRIX4( )
```

**`NIPS = 4`, `NIPT = 1`**

```fortran
CALL STElemSD % getConvectiveMatrix_17(  Term1 = 'dt', Term2 = 'dx', C = DummyMat2, Ctype = 'Quad', nCopy = 2 )

MATRIX STORED IN ST-ELEMENT-SHAPEDATA

NIPS ::   4  NIPT ::   1

-------------------------------------------------

4D MATRIX, MAT4(:,:,:,:) ::

Mat4( :, :,  1, 1 )

  0.3333333     -0.8333333E-01 -0.1666667     -0.8333333E-01   0.000000       0.000000       0.000000       0.000000
  0.2500000       0.000000     -0.2500000       0.000000       0.000000       0.000000       0.000000       0.000000
  0.1666667      0.8333333E-01 -0.3333333      0.8333333E-01   0.000000       0.000000       0.000000       0.000000
  0.2500000       0.000000     -0.2500000       0.000000       0.000000       0.000000       0.000000       0.000000
   0.000000       0.000000       0.000000       0.000000      0.3333333     -0.8333333E-01 -0.1666667     -0.8333333E-01
   0.000000       0.000000       0.000000       0.000000      0.2500000       0.000000     -0.2500000       0.000000
   0.000000       0.000000       0.000000       0.000000      0.1666667      0.8333333E-01 -0.3333333      0.8333333E-01
   0.000000       0.000000       0.000000       0.000000      0.2500000       0.000000     -0.2500000       0.000000

Mat4( :, :,  1, 2 )

  0.3333333     -0.8333333E-01 -0.1666667     -0.8333333E-01   0.000000       0.000000       0.000000       0.000000
  0.2500000       0.000000     -0.2500000       0.000000       0.000000       0.000000       0.000000       0.000000
  0.1666667      0.8333333E-01 -0.3333333      0.8333333E-01   0.000000       0.000000       0.000000       0.000000
  0.2500000       0.000000     -0.2500000       0.000000       0.000000       0.000000       0.000000       0.000000
   0.000000       0.000000       0.000000       0.000000      0.3333333     -0.8333333E-01 -0.1666667     -0.8333333E-01
   0.000000       0.000000       0.000000       0.000000      0.2500000       0.000000     -0.2500000       0.000000
   0.000000       0.000000       0.000000       0.000000      0.1666667      0.8333333E-01 -0.3333333      0.8333333E-01
   0.000000       0.000000       0.000000       0.000000      0.2500000       0.000000     -0.2500000       0.000000

Mat4( :, :,  2, 1 )

 -0.3333333      0.8333333E-01  0.1666667      0.8333333E-01   0.000000       0.000000       0.000000       0.000000
 -0.2500000       0.000000      0.2500000       0.000000       0.000000       0.000000       0.000000       0.000000
 -0.1666667     -0.8333333E-01  0.3333333     -0.8333333E-01   0.000000       0.000000       0.000000       0.000000
 -0.2500000       0.000000      0.2500000       0.000000       0.000000       0.000000       0.000000       0.000000
   0.000000       0.000000       0.000000       0.000000     -0.3333333      0.8333333E-01  0.1666667      0.8333333E-01
   0.000000       0.000000       0.000000       0.000000     -0.2500000       0.000000      0.2500000       0.000000
   0.000000       0.000000       0.000000       0.000000     -0.1666667     -0.8333333E-01  0.3333333     -0.8333333E-01
   0.000000       0.000000       0.000000       0.000000     -0.2500000       0.000000      0.2500000       0.000000

Mat4( :, :,  2, 2 )

 -0.3333333      0.8333333E-01  0.1666667      0.8333333E-01   0.000000       0.000000       0.000000       0.000000
 -0.2500000       0.000000      0.2500000       0.000000       0.000000       0.000000       0.000000       0.000000
 -0.1666667     -0.8333333E-01  0.3333333     -0.8333333E-01   0.000000       0.000000       0.000000       0.000000
 -0.2500000       0.000000      0.2500000       0.000000       0.000000       0.000000       0.000000       0.000000
   0.000000       0.000000       0.000000       0.000000     -0.3333333      0.8333333E-01  0.1666667      0.8333333E-01
   0.000000       0.000000       0.000000       0.000000     -0.2500000       0.000000      0.2500000       0.000000
   0.000000       0.000000       0.000000       0.000000     -0.1666667     -0.8333333E-01  0.3333333     -0.8333333E-01
   0.000000       0.000000       0.000000       0.000000     -0.2500000       0.000000      0.2500000       0.000000
```

### getConvectiveMatrix_18()

INTERFACE

```fortran
 SUBROUTINE getConvectiveMatrix_18( Obj, C, Term1, Term2, CType, nCopy )

    USE Utility, ONLY : OUTERPROD

    CLASS( STConvectiveMatrix_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), DIMENSION( :, :, : ), INTENT( IN ) :: C
    CHARACTER( LEN = * ), INTENT( IN ) :: Term1, Term2, CType
    INTEGER( I4B ), INTENT( IN ) :: nCopy
```

DESCRIPTION

The subroutine performs the same task as `getConvectiveMatrix_15()`. However, in this case, `C(:, :, :)` is a 3D array. If `Ctype` is `Nodal` then `C` denotes the space-time nodal values and shape of `C` must be `(NSD, NNS, NNT)`. If `Ctype` is defined as `Quad` then `C(:,:,:)` represents the convective velocities at *space-time integration* points. In this method, the *convective velocity* $c(x,t)$ chages in both space and time domain. The shape of `C` array should be `(NSD, NIPS, NIPT)`. the second index of `C` denotes the spatial-integration points, and third index denotes the temporal integration points. For details see the notes or `getConvectiveMatrix_15()`.

The following code snippet can be used to perform this task.

```fortran
CALL Obj % getConvectiveMatrix( C = C, Term1 = "dt", Term2 = "dx", nCopy = 2, Ctype = 'Quad' )
CALL Obj % getConvectiveMatrix( C = C, Term1 = "dx", Term2 = "dt", nCopy = 2, Ctype = 'Quad' )
```

SYMBOLIC CALCULATION 

$${}^{4}M(I,J,a,b) = \delta {}^{a}u_{iI} \int_{Q_n} \frac{ \partial N^I T_a}{\partial t} c_{k}^{h} \frac{\partial N^J T_b}{\partial x_k} {dQ} \quad {}^{b}u_{iJ}$$

$${}^{4}M(I,J,a,b) = \delta {}^{a}u_{iI} \int_{Q_n} c_{k}^{h} \frac{\partial N^I T_a}{\partial x_k} \frac{\partial N^J T_b}{\partial t} {dQ} \quad {}^{b}u_{iJ}$$

TESTING

For all the tests `C(:, :, :) = 1.0`. The following code is used for testing.

```fortran
IF( ALLOCATED( DummyMat3 ) ) DEALLOCATE( DummyMat3 )
ALLOCATE( DummyMat3( NSD, NIPS, NIPT ) )

DummyMat3 = 1.0_DFP

CALL STElemSD % getConvectiveMatrix( Term1 = "dt", Term2 = "dx", &
C = DummyMat3, CType = "Quad", nCopy = 2 )
CALL STElemSD % DISPLAYMATRIX4( )
```

**`NIPS = 4`, `NIPT = 1`**

```fortran
CALL STElemSD % getConvectiveMatrix_18(  Term1 = 'dt', Term2 = 'dx', C = DummyMat3, Ctype = 'Quad', nCopy = 2 )

MATRIX STORED IN ST-ELEMENT-SHAPEDATA

NIPS ::   4  NIPT ::   1

-------------------------------------------------

4D MATRIX, MAT4(:,:,:,:) ::

Mat4( :, :,  1, 1 )

  0.3333333     -0.8333333E-01 -0.1666667     -0.8333333E-01   0.000000       0.000000       0.000000       0.000000
  0.2500000       0.000000     -0.2500000       0.000000       0.000000       0.000000       0.000000       0.000000
  0.1666667      0.8333333E-01 -0.3333333      0.8333333E-01   0.000000       0.000000       0.000000       0.000000
  0.2500000       0.000000     -0.2500000       0.000000       0.000000       0.000000       0.000000       0.000000
   0.000000       0.000000       0.000000       0.000000      0.3333333     -0.8333333E-01 -0.1666667     -0.8333333E-01
   0.000000       0.000000       0.000000       0.000000      0.2500000       0.000000     -0.2500000       0.000000
   0.000000       0.000000       0.000000       0.000000      0.1666667      0.8333333E-01 -0.3333333      0.8333333E-01
   0.000000       0.000000       0.000000       0.000000      0.2500000       0.000000     -0.2500000       0.000000

Mat4( :, :,  1, 2 )

  0.3333333     -0.8333333E-01 -0.1666667     -0.8333333E-01   0.000000       0.000000       0.000000       0.000000
  0.2500000       0.000000     -0.2500000       0.000000       0.000000       0.000000       0.000000       0.000000
  0.1666667      0.8333333E-01 -0.3333333      0.8333333E-01   0.000000       0.000000       0.000000       0.000000
  0.2500000       0.000000     -0.2500000       0.000000       0.000000       0.000000       0.000000       0.000000
   0.000000       0.000000       0.000000       0.000000      0.3333333     -0.8333333E-01 -0.1666667     -0.8333333E-01
   0.000000       0.000000       0.000000       0.000000      0.2500000       0.000000     -0.2500000       0.000000
   0.000000       0.000000       0.000000       0.000000      0.1666667      0.8333333E-01 -0.3333333      0.8333333E-01
   0.000000       0.000000       0.000000       0.000000      0.2500000       0.000000     -0.2500000       0.000000

Mat4( :, :,  2, 1 )

 -0.3333333      0.8333333E-01  0.1666667      0.8333333E-01   0.000000       0.000000       0.000000       0.000000
 -0.2500000       0.000000      0.2500000       0.000000       0.000000       0.000000       0.000000       0.000000
 -0.1666667     -0.8333333E-01  0.3333333     -0.8333333E-01   0.000000       0.000000       0.000000       0.000000
 -0.2500000       0.000000      0.2500000       0.000000       0.000000       0.000000       0.000000       0.000000
   0.000000       0.000000       0.000000       0.000000     -0.3333333      0.8333333E-01  0.1666667      0.8333333E-01
   0.000000       0.000000       0.000000       0.000000     -0.2500000       0.000000      0.2500000       0.000000
   0.000000       0.000000       0.000000       0.000000     -0.1666667     -0.8333333E-01  0.3333333     -0.8333333E-01
   0.000000       0.000000       0.000000       0.000000     -0.2500000       0.000000      0.2500000       0.000000

Mat4( :, :,  2, 2 )

 -0.3333333      0.8333333E-01  0.1666667      0.8333333E-01   0.000000       0.000000       0.000000       0.000000
 -0.2500000       0.000000      0.2500000       0.000000       0.000000       0.000000       0.000000       0.000000
 -0.1666667     -0.8333333E-01  0.3333333     -0.8333333E-01   0.000000       0.000000       0.000000       0.000000
 -0.2500000       0.000000      0.2500000       0.000000       0.000000       0.000000       0.000000       0.000000
   0.000000       0.000000       0.000000       0.000000     -0.3333333      0.8333333E-01  0.1666667      0.8333333E-01
   0.000000       0.000000       0.000000       0.000000     -0.2500000       0.000000      0.2500000       0.000000
   0.000000       0.000000       0.000000       0.000000     -0.1666667     -0.8333333E-01  0.3333333     -0.8333333E-01
   0.000000       0.000000       0.000000       0.000000     -0.2500000       0.000000      0.2500000       0.000000
```

### getConvectiveMatrix_22()

INTERFACE

```fortran
 SUBROUTINE getConvectiveMatrix_22( Obj, Term1, Term2 )

    USE Utility, ONLY : OUTERPROD

    CLASS( STConvectiveMatrix_ ), INTENT( INOUT ) ::  Obj
    CHARACTER( LEN = * ), INTENT( IN ) :: Term1, Term2
```

DESCRIPTION

- `Term1` and `Term2` are strings. They represent the time derivative and/or space-derivative.
- If `Term1` or `Term2` is in the set`[dt, Dt, dT, DT]` then it means the time derivative.
- If `Term1` or `Term2` is in the set `[dx, dx1, dX, x, X, x1, X1]` then it means deriavative with respect to `x` coordinate.
- If `Term1` or `Term2` is in the set `[dy, dx2, dY, y, Y, x2, X2]` then it means deriavative with respect to `y` coordinate.
- If `Term1` or `Term2` is in the set `[dz, dx3, dZ, z, Z, x3, X3]` then it means deriavative with respect to `z` coordinate.

The following code snippet can be used to perform this task.

```fortran
CALL Obj % getConvectiveMatrix( Term1 = "dt", Term2 = "dx") 
CALL Obj % getConvectiveMatrix( Term1 = "dt", Term2 = "dy") 
CALL Obj % getConvectiveMatrix( Term1 = "dt", Term2 = "dz") 
CALL Obj % getConvectiveMatrix( Term1 = "dx", Term2 = "dt") 
CALL Obj % getConvectiveMatrix( Term1 = "dy", Term2 = "dt") 
CALL Obj % getConvectiveMatrix( Term1 = "dz", Term2 = "dt") 
```

> `Term1` and `Term2` should not be the same. Why?, because it does not make sense in case of convective terms.

SYMBOLIC CALCULATION

$${}^{4}M(I,J,a,b) = {}^{a}\delta U_{iI} \quad \int_{Q_n} \frac{\partial N^I T_a}{\partial t} \frac{\partial N^J T_b}{\partial x} {dQ} \quad {}^{b}f_{iJ}$$

$${}^{4}M(I,J,a,b) = {}^{a}\delta U_{iI} \quad \int_{Q_n} \frac{\partial N^I T_a}{\partial x} \frac{\partial N^J T_b}{\partial t} {dQ}  \quad {}^{b}f_{iJ}$$

$${}^{4}M(I,J,a,b) = {}^{a}\delta U_{iI} \quad \int_{Q_n} \frac{\partial N^I T_a}{\partial t} \frac{\partial N^J T_b}{\partial y} {dQ} \quad {}^{b}g_{iJ}$$

$${}^{4}M(I,J,a,b) = {}^{a}\delta U_{iI} \quad \int_{Q_n} \frac{\partial N^I T_a}{\partial y} \frac{\partial N^J T_b}{\partial t} {dQ}  \quad {}^{b}g_{iJ}$$


$${}^{4}M(I,J,a,b) = {}^{a}\delta U_{iI} \quad \int_{Q_n} \frac{\partial N^I T_a}{\partial t} \frac{\partial N^J T_b}{\partial z} {dQ} \quad {}^{b}h_{iJ}$$

$${}^{4}M(I,J,a,b) = {}^{a}\delta U_{iI} \quad \int_{Q_n} \frac{\partial N^I T_a}{\partial z} \frac{\partial N^J T_b}{\partial t} {dQ}  \quad {}^{b}h_{iJ}$$


TESTING

```fortran
CALL STElemSD % getConvectiveMatrix( Term1 = "dt", Term2 = "dx")
CALL STElemSD % DISPLAYMATRIX4( )

CALL STElemSD % getConvectiveMatrix( Term1 = "dx", Term2 = "dt")
CALL STElemSD % DISPLAYMATRIX4( )
```

**`NIPS = 1`, `NIPT = 1`**

```fortran
CALL STElemSD % getConvectiveMatrix_22( Term1 = 'dt', Term2 = 'dx')

MATRIX STORED IN ST-ELEMENT-SHAPEDATA

NIPS ::   1  NIPT ::   1

-------------------------------------------------

4D MATRIX, MAT4(:,:,:,:) ::

Mat4( :, :,  1, 1 )

  0.1250000     -0.1250000     -0.1250000      0.1250000
  0.1250000     -0.1250000     -0.1250000      0.1250000
  0.1250000     -0.1250000     -0.1250000      0.1250000
  0.1250000     -0.1250000     -0.1250000      0.1250000

Mat4( :, :,  1, 2 )

  0.1250000     -0.1250000     -0.1250000      0.1250000
  0.1250000     -0.1250000     -0.1250000      0.1250000
  0.1250000     -0.1250000     -0.1250000      0.1250000
  0.1250000     -0.1250000     -0.1250000      0.1250000

Mat4( :, :,  2, 1 )

 -0.1250000      0.1250000      0.1250000     -0.1250000
 -0.1250000      0.1250000      0.1250000     -0.1250000
 -0.1250000      0.1250000      0.1250000     -0.1250000
 -0.1250000      0.1250000      0.1250000     -0.1250000

Mat4( :, :,  2, 2 )

 -0.1250000      0.1250000      0.1250000     -0.1250000
 -0.1250000      0.1250000      0.1250000     -0.1250000
 -0.1250000      0.1250000      0.1250000     -0.1250000
 -0.1250000      0.1250000      0.1250000     -0.1250000
```

```fortran
CALL STElemSD % getConvectiveMatrix_22( Term1 = 'dx', Term2 = 'dt')

MATRIX STORED IN ST-ELEMENT-SHAPEDATA

NIPS ::   1  NIPT ::   1

-------------------------------------------------

4D MATRIX, MAT4(:,:,:,:) ::

Mat4( :, :,  1, 1 )

  0.1250000      0.1250000      0.1250000      0.1250000
 -0.1250000     -0.1250000     -0.1250000     -0.1250000
 -0.1250000     -0.1250000     -0.1250000     -0.1250000
  0.1250000      0.1250000      0.1250000      0.1250000

Mat4( :, :,  1, 2 )

 -0.1250000     -0.1250000     -0.1250000     -0.1250000
  0.1250000      0.1250000      0.1250000      0.1250000
  0.1250000      0.1250000      0.1250000      0.1250000
 -0.1250000     -0.1250000     -0.1250000     -0.1250000

Mat4( :, :,  2, 1 )

  0.1250000      0.1250000      0.1250000      0.1250000
 -0.1250000     -0.1250000     -0.1250000     -0.1250000
 -0.1250000     -0.1250000     -0.1250000     -0.1250000
  0.1250000      0.1250000      0.1250000      0.1250000

Mat4( :, :,  2, 2 )

 -0.1250000     -0.1250000     -0.1250000     -0.1250000
  0.1250000      0.1250000      0.1250000      0.1250000
  0.1250000      0.1250000      0.1250000      0.1250000
 -0.1250000     -0.1250000     -0.1250000     -0.1250000
```

> As expected the matrices are transpose of each other, i.e. ${}^{4}M(I,J,a,b) = {}^{4}M(J,I,b,a)$. Therefore, we will consider only the first one.

**`NIPS = 1`, `NIPT = 2`**

```fortran
CALL STElemSD % getConvectiveMatrix_22( Term1 = 'dt', Term2 = 'dx')

MATRIX STORED IN ST-ELEMENT-SHAPEDATA

NIPS ::   1  NIPT ::   2

-------------------------------------------------

4D MATRIX, MAT4(:,:,:,:) ::

Mat4( :, :,  1, 1 )

  0.1250000     -0.1250000     -0.1250000      0.1250000
  0.1250000     -0.1250000     -0.1250000      0.1250000
  0.1250000     -0.1250000     -0.1250000      0.1250000
  0.1250000     -0.1250000     -0.1250000      0.1250000

Mat4( :, :,  1, 2 )

  0.1250000     -0.1250000     -0.1250000      0.1250000
  0.1250000     -0.1250000     -0.1250000      0.1250000
  0.1250000     -0.1250000     -0.1250000      0.1250000
  0.1250000     -0.1250000     -0.1250000      0.1250000

Mat4( :, :,  2, 1 )

 -0.1250000      0.1250000      0.1250000     -0.1250000
 -0.1250000      0.1250000      0.1250000     -0.1250000
 -0.1250000      0.1250000      0.1250000     -0.1250000
 -0.1250000      0.1250000      0.1250000     -0.1250000

Mat4( :, :,  2, 2 )

 -0.1250000      0.1250000      0.1250000     -0.1250000
 -0.1250000      0.1250000      0.1250000     -0.1250000
 -0.1250000      0.1250000      0.1250000     -0.1250000
 -0.1250000      0.1250000      0.1250000     -0.1250000
```

> The matrix is rank deficient, and there is no effect of increasing the `NIPT`. Therefore, we must increase the `NIPS`

**`NIPS = 4, NIPT = 1`**

```fortran
CALL STElemSD % getConvectiveMatrix_22( Term1 = 'dt', Term2 = 'dx')

MATRIX STORED IN ST-ELEMENT-SHAPEDATA

NIPS ::   4  NIPT ::   1

-------------------------------------------------

4D MATRIX, MAT4(:,:,:,:) ::

Mat4( :, :,  1, 1 )

  0.1666667     -0.1666667     -0.8333333E-01  0.8333333E-01
  0.1666667     -0.1666667     -0.8333333E-01  0.8333333E-01
  0.8333333E-01 -0.8333333E-01 -0.1666667      0.1666667
  0.8333333E-01 -0.8333333E-01 -0.1666667      0.1666667

Mat4( :, :,  1, 2 )

  0.1666667     -0.1666667     -0.8333333E-01  0.8333333E-01
  0.1666667     -0.1666667     -0.8333333E-01  0.8333333E-01
  0.8333333E-01 -0.8333333E-01 -0.1666667      0.1666667
  0.8333333E-01 -0.8333333E-01 -0.1666667      0.1666667

Mat4( :, :,  2, 1 )

 -0.1666667      0.1666667      0.8333333E-01 -0.8333333E-01
 -0.1666667      0.1666667      0.8333333E-01 -0.8333333E-01
 -0.8333333E-01  0.8333333E-01  0.1666667     -0.1666667
 -0.8333333E-01  0.8333333E-01  0.1666667     -0.1666667

Mat4( :, :,  2, 2 )

 -0.1666667      0.1666667      0.8333333E-01 -0.8333333E-01
 -0.1666667      0.1666667      0.8333333E-01 -0.8333333E-01
 -0.8333333E-01  0.8333333E-01  0.1666667     -0.1666667
 -0.8333333E-01  0.8333333E-01  0.1666667     -0.1666667
```

**`NIPS = 4, NIPT = 2`**

```fortran
CALL STElemSD % getConvectiveMatrix_22( Term1 = 'dt', Term2 = 'dx')

MATRIX STORED IN ST-ELEMENT-SHAPEDATA

NIPS ::   4  NIPT ::   2

-------------------------------------------------

4D MATRIX, MAT4(:,:,:,:) ::

Mat4( :, :,  1, 1 )

  0.1666667     -0.1666667     -0.8333333E-01  0.8333333E-01
  0.1666667     -0.1666667     -0.8333333E-01  0.8333333E-01
  0.8333333E-01 -0.8333333E-01 -0.1666667      0.1666667
  0.8333333E-01 -0.8333333E-01 -0.1666667      0.1666667

Mat4( :, :,  1, 2 )

  0.1666667     -0.1666667     -0.8333333E-01  0.8333333E-01
  0.1666667     -0.1666667     -0.8333333E-01  0.8333333E-01
  0.8333333E-01 -0.8333333E-01 -0.1666667      0.1666667
  0.8333333E-01 -0.8333333E-01 -0.1666667      0.1666667

Mat4( :, :,  2, 1 )

 -0.1666667      0.1666667      0.8333333E-01 -0.8333333E-01
 -0.1666667      0.1666667      0.8333333E-01 -0.8333333E-01
 -0.8333333E-01  0.8333333E-01  0.1666667     -0.1666667
 -0.8333333E-01  0.8333333E-01  0.1666667     -0.1666667

Mat4( :, :,  2, 2 )

 -0.1666667      0.1666667      0.8333333E-01 -0.8333333E-01
 -0.1666667      0.1666667      0.8333333E-01 -0.8333333E-01
 -0.8333333E-01  0.8333333E-01  0.1666667     -0.1666667
 -0.8333333E-01  0.8333333E-01  0.1666667     -0.1666667
```

> `NIPS = 4, NIPT = 1` results same as `NIPS = 4, NIPT = 2`, because in the present case mesh is not moving so the integrand is linear in time therefore we need only one integration point for exact integration. Note that this may vary when mesh is also moving.

**`NIPS = 9, NIPT = 1`**

```fortran
CALL STElemSD % getConvectiveMatrix_22( Term1 = 'dt', Term2 = 'dx')

MATRIX STORED IN ST-ELEMENT-SHAPEDATA

NIPS ::   9  NIPT ::   1

-------------------------------------------------

4D MATRIX, MAT4(:,:,:,:) ::

Mat4( :, :,  1, 1 )

  0.1666667     -0.1666667     -0.8333333E-01  0.8333333E-01
  0.1666667     -0.1666667     -0.8333333E-01  0.8333333E-01
  0.8333333E-01 -0.8333333E-01 -0.1666667      0.1666667
  0.8333333E-01 -0.8333333E-01 -0.1666667      0.1666667

Mat4( :, :,  1, 2 )

  0.1666667     -0.1666667     -0.8333333E-01  0.8333333E-01
  0.1666667     -0.1666667     -0.8333333E-01  0.8333333E-01
  0.8333333E-01 -0.8333333E-01 -0.1666667      0.1666667
  0.8333333E-01 -0.8333333E-01 -0.1666667      0.1666667

Mat4( :, :,  2, 1 )

 -0.1666667      0.1666667      0.8333333E-01 -0.8333333E-01
 -0.1666667      0.1666667      0.8333333E-01 -0.8333333E-01
 -0.8333333E-01  0.8333333E-01  0.1666667     -0.1666667
 -0.8333333E-01  0.8333333E-01  0.1666667     -0.1666667

Mat4( :, :,  2, 2 )

 -0.1666667      0.1666667      0.8333333E-01 -0.8333333E-01
 -0.1666667      0.1666667      0.8333333E-01 -0.8333333E-01
 -0.8333333E-01  0.8333333E-01  0.1666667     -0.1666667
 -0.8333333E-01  0.8333333E-01  0.1666667     -0.1666667
```

Once again the results are the same. Therefore, lets stop here and move to next method.

### getConvectiveMatrix_23()

INTERFACE

```fortran
 SUBROUTINE getConvectiveMatrix_23( Obj, Term1, Term2, nCopy )

    USE Utility, ONLY : OUTERPROD

    CLASS( STConvectiveMatrix_ ), INTENT( INOUT ) ::  Obj
    CHARACTER( LEN = * ), INTENT( IN ) :: Term1, Term2
    INTEGER( I4B ), INTENT( IN ) :: nCopy
```

DESCRIPTION

- This method will create the `nCopy` of the convective matrix defined in the previous routine.

The following code snippet can be used to perform this task.

```fortran
CALL Obj % getConvectiveMatrix( Term1 = "dt", Term2 = "dx", nCopy = 2 )
CALL Obj % getConvectiveMatrix( Term1 = "dt", Term2 = "dy", nCopy = 2 )
CALL Obj % getConvectiveMatrix( Term1 = "dt", Term2 = "dz", nCopy = 2 )
CALL Obj % getConvectiveMatrix( Term1 = "dx", Term2 = "dt", nCopy = 2 )
CALL Obj % getConvectiveMatrix( Term1 = "dy", Term2 = "dt", nCopy = 2 )
CALL Obj % getConvectiveMatrix( Term1 = "dz", Term2 = "dt", nCopy = 2 )
```

SYMBOLIC CALCULATION

$${}^{4}M(I,J,a,b) = {}^{a}\delta U_{iI} \quad \int_{Q_n} \frac{\partial N^I T_a}{\partial t} \frac{\partial N^J T_b}{\partial x} {dQ} \quad {}^{b}f_{iJ}$$

$${}^{4}M(I,J,a,b) = {}^{a}\delta U_{iI} \quad \int_{Q_n} \frac{\partial N^I T_a}{\partial x} \frac{\partial N^J T_b}{\partial t} {dQ}  \quad {}^{b}f_{iJ}$$

$${}^{4}M(I,J,a,b) = {}^{a}\delta U_{iI} \quad \int_{Q_n} \frac{\partial N^I T_a}{\partial t} \frac{\partial N^J T_b}{\partial y} {dQ} \quad {}^{b}g_{iJ}$$

$${}^{4}M(I,J,a,b) = {}^{a}\delta U_{iI} \quad \int_{Q_n} \frac{\partial N^I T_a}{\partial y} \frac{\partial N^J T_b}{\partial t} {dQ}  \quad {}^{b}g_{iJ}$$


$${}^{4}M(I,J,a,b) = {}^{a}\delta U_{iI} \quad \int_{Q_n} \frac{\partial N^I T_a}{\partial t} \frac{\partial N^J T_b}{\partial z} {dQ} \quad {}^{b}h_{iJ}$$

$${}^{4}M(I,J,a,b) = {}^{a}\delta U_{iI} \quad \int_{Q_n} \frac{\partial N^I T_a}{\partial z} \frac{\partial N^J T_b}{\partial t} {dQ}  \quad {}^{b}h_{iJ}$$


TESTING

```fortran
CALL STElemSD % getConvectiveMatrix( Term1 = "dt", Term2 = "dx", nCopy = 2 )
CALL STElemSD % DISPLAYMATRIX4( )
```

**`NIPS = 1`, `NIPT = 1`**

```fortran
CALL STElemSD % getConvectiveMatrix_23( Term1 = 'dt', Term2 = 'dx', nCopy = 2 )

MATRIX STORED IN ST-ELEMENT-SHAPEDATA

NIPS ::   4  NIPT ::   1

-------------------------------------------------

4D MATRIX, MAT4(:,:,:,:) ::

Mat4( :, :,  1, 1 )

  0.1666667     -0.1666667     -0.8333333E-01  0.8333333E-01   0.000000       0.000000       0.000000       0.000000
  0.1666667     -0.1666667     -0.8333333E-01  0.8333333E-01   0.000000       0.000000       0.000000       0.000000
  0.8333333E-01 -0.8333333E-01 -0.1666667      0.1666667       0.000000       0.000000       0.000000       0.000000
  0.8333333E-01 -0.8333333E-01 -0.1666667      0.1666667       0.000000       0.000000       0.000000       0.000000
   0.000000       0.000000       0.000000       0.000000      0.1666667     -0.1666667     -0.8333333E-01  0.8333333E-01
   0.000000       0.000000       0.000000       0.000000      0.1666667     -0.1666667     -0.8333333E-01  0.8333333E-01
   0.000000       0.000000       0.000000       0.000000      0.8333333E-01 -0.8333333E-01 -0.1666667      0.1666667
   0.000000       0.000000       0.000000       0.000000      0.8333333E-01 -0.8333333E-01 -0.1666667      0.1666667

Mat4( :, :,  1, 2 )

  0.1666667     -0.1666667     -0.8333333E-01  0.8333333E-01   0.000000       0.000000       0.000000       0.000000
  0.1666667     -0.1666667     -0.8333333E-01  0.8333333E-01   0.000000       0.000000       0.000000       0.000000
  0.8333333E-01 -0.8333333E-01 -0.1666667      0.1666667       0.000000       0.000000       0.000000       0.000000
  0.8333333E-01 -0.8333333E-01 -0.1666667      0.1666667       0.000000       0.000000       0.000000       0.000000
   0.000000       0.000000       0.000000       0.000000      0.1666667     -0.1666667     -0.8333333E-01  0.8333333E-01
   0.000000       0.000000       0.000000       0.000000      0.1666667     -0.1666667     -0.8333333E-01  0.8333333E-01
   0.000000       0.000000       0.000000       0.000000      0.8333333E-01 -0.8333333E-01 -0.1666667      0.1666667
   0.000000       0.000000       0.000000       0.000000      0.8333333E-01 -0.8333333E-01 -0.1666667      0.1666667

Mat4( :, :,  2, 1 )

 -0.1666667      0.1666667      0.8333333E-01 -0.8333333E-01   0.000000       0.000000       0.000000       0.000000
 -0.1666667      0.1666667      0.8333333E-01 -0.8333333E-01   0.000000       0.000000       0.000000       0.000000
 -0.8333333E-01  0.8333333E-01  0.1666667     -0.1666667       0.000000       0.000000       0.000000       0.000000
 -0.8333333E-01  0.8333333E-01  0.1666667     -0.1666667       0.000000       0.000000       0.000000       0.000000
   0.000000       0.000000       0.000000       0.000000     -0.1666667      0.1666667      0.8333333E-01 -0.8333333E-01
   0.000000       0.000000       0.000000       0.000000     -0.1666667      0.1666667      0.8333333E-01 -0.8333333E-01
   0.000000       0.000000       0.000000       0.000000     -0.8333333E-01  0.8333333E-01  0.1666667     -0.1666667
   0.000000       0.000000       0.000000       0.000000     -0.8333333E-01  0.8333333E-01  0.1666667     -0.1666667

Mat4( :, :,  2, 2 )

 -0.1666667      0.1666667      0.8333333E-01 -0.8333333E-01   0.000000       0.000000       0.000000       0.000000
 -0.1666667      0.1666667      0.8333333E-01 -0.8333333E-01   0.000000       0.000000       0.000000       0.000000
 -0.8333333E-01  0.8333333E-01  0.1666667     -0.1666667       0.000000       0.000000       0.000000       0.000000
 -0.8333333E-01  0.8333333E-01  0.1666667     -0.1666667       0.000000       0.000000       0.000000       0.000000
   0.000000       0.000000       0.000000       0.000000     -0.1666667      0.1666667      0.8333333E-01 -0.8333333E-01
   0.000000       0.000000       0.000000       0.000000     -0.1666667      0.1666667      0.8333333E-01 -0.8333333E-01
   0.000000       0.000000       0.000000       0.000000     -0.8333333E-01  0.8333333E-01  0.1666667     -0.1666667
   0.000000       0.000000       0.000000       0.000000     -0.8333333E-01  0.8333333E-01  0.1666667     -0.1666667
```

### getConvectiveMatrix_19()

INTERFACE

```fortran
 SUBROUTINE getConvectiveMatrix_19( Obj, A, Term1, Term2, Xtype, MultiVar )

    USE Utility, ONLY : OUTERPROD

    CLASS( STConvectiveMatrix_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), DIMENSION( :, :, :, : ) :: A
    INTEGER( I4B ), INTENT( IN ) :: Term1, Term2
    CHARACTER( LEN = * ), INTENT( IN ) :: XType
    LOGICAL( LGT ), INTENT( IN ) :: MultiVar
```

DESCRIPTION

- `A(:,:,:,:)` is a four dimensional array. The shape of `A` is `(M,M,NIPS, NIPT)`. The thrid index denotes the spatial-integation point. The fourth index represent the temporal integration point. In this case, `A` changes with both space and time.
- `Term1` and `Term2` are integers, and can take values 0 or 1. If They are 1 then it means first order spatial derivative. If they are 0 then it means no spatial-derivative.
- `Xtype` is a string, which stands for the type of spatial gradient. If `Xtype` is in the set `[dx, dX, dx1, dX1, x, X, x1, X1]` then it means the spatial derivative is with respect to the `x`. If it belongs to the set `[dy, dY, dx2, dX2, y, Y, x2, X2]` then it means the spatial derivative is with respect to the `y`. If it belongs to the set `[dz, dZ, dx3, dX3, z, Z, x3, X3]` then it means the spatial derivative is with respect to the `z`. 
- `MultiVar` is a logical parameter, which has no effect on the functionality of this method. However, this dummy argument is required to tell the compiler that this routine is different then other routine which involves _convection velocity_.

SYMBOLIC CALCULATION

$${}^{4}M(I,J,a,b) = {}^{a}\delta U_{iI} \quad \int_{Q_n} N^I T_a [ \mathbf{A_1} ]_{ij} \frac{\partial N^J T_b}{\partial x} {dQ} \quad {}^{b}f_{jJ}$$

$${}^{4}M(I,J,a,b) = {}^{a}\delta U_{iI} \quad \int_{Q_n} [ \mathbf{A_1} ]_{ji} \frac{\partial N^I T_a}{\partial x} N^J T_b {dQ}  \quad {}^{b}f_{jJ}$$

$${}^{4}M(I,J,a,b) = {}^{a}\delta U_{iI} \quad \int_{Q_n} N^I T_a [\mathbf{A_2}]_{ij} \frac{\partial N^J T_b}{\partial y} {dQ} \quad {}^{b}g_{jJ}$$

$${}^{4}M(I,J,a,b) = {}^{a}\delta U_{iI} \quad \int_{Q_n} [\mathbf{A_2}]_{ji} \frac{\partial N^I T_a}{\partial y} N^J T_b {dQ}  \quad {}^{b}g_{jJ}$$


$${}^{4}M(I,J,a,b) = {}^{a}\delta U_{iI} \quad \int_{Q_n} N^I T_a [\mathbf{A_3}]_{ij} \frac{\partial N^J T_b}{\partial z} {dQ} \quad {}^{b}h_{jJ}$$

$${}^{4}M(I,J,a,b) = {}^{a}\delta U_{iI} \quad \int_{Q_n} [\mathbf{A_3}]_{ji} \frac{\partial N^I T_a}{\partial z} N^J T_b {dQ}  \quad {}^{b}h_{jJ}$$

TESTING

```fortran
IF( ALLOCATED( DummyMat4 ) ) DEALLOCATE( DummyMat4 )
ALLOCATE( DummyMat4( 1, 1, NIPS, NIPT ) )
DummyMat4 = 0.0_DFP; DummyMat4( 1, 1, :, : ) = 1.0_DFP; !DummyMat4( 2, 2, :, : ) = 1.0_DFP

CALL STElemSD % getConvectiveMatrix( A = DummyMat4, Term1 = 0, Term2 = 1, XType = "dx", MultiVar = .TRUE. )
CALL STElemSD % DISPLAYMATRIX4( )
```

**`NIPS = 4`, `NIPT = 1`**

```fortran
CALL STElemSD % getConvectiveMatrix_19( A = DummyMat4, Term1 = 0, Term2 = 1, Xtype = 'dx', MultiVar = .TRUE. )

MATRIX STORED IN ST-ELEMENT-SHAPEDATA

NIPS ::   4  NIPT ::   1

-------------------------------------------------

4D MATRIX, MAT4(:,:,:,:) ::

Mat4( :, :,  1, 1 )

 -0.1666667      0.1666667      0.8333333E-01 -0.8333333E-01
 -0.1666667      0.1666667      0.8333333E-01 -0.8333333E-01
 -0.8333333E-01  0.8333333E-01  0.1666667     -0.1666667
 -0.8333333E-01  0.8333333E-01  0.1666667     -0.1666667

Mat4( :, :,  1, 2 )

 -0.1666667      0.1666667      0.8333333E-01 -0.8333333E-01
 -0.1666667      0.1666667      0.8333333E-01 -0.8333333E-01
 -0.8333333E-01  0.8333333E-01  0.1666667     -0.1666667
 -0.8333333E-01  0.8333333E-01  0.1666667     -0.1666667

Mat4( :, :,  2, 1 )

 -0.1666667      0.1666667      0.8333333E-01 -0.8333333E-01
 -0.1666667      0.1666667      0.8333333E-01 -0.8333333E-01
 -0.8333333E-01  0.8333333E-01  0.1666667     -0.1666667
 -0.8333333E-01  0.8333333E-01  0.1666667     -0.1666667

Mat4( :, :,  2, 2 )

 -0.1666667      0.1666667      0.8333333E-01 -0.8333333E-01
 -0.1666667      0.1666667      0.8333333E-01 -0.8333333E-01
 -0.8333333E-01  0.8333333E-01  0.1666667     -0.1666667
 -0.8333333E-01  0.8333333E-01  0.1666667     -0.1666667
```

**`NIPS = 4`, `NIPT = 2`**

```fortran
CALL STElemSD % getConvectiveMatrix_19( A = DummyMat4, Term1 = 0, Term2 = 1, Xtype = 'dx', MultiVar = .TRUE. )

MATRIX STORED IN ST-ELEMENT-SHAPEDATA

NIPS ::   4  NIPT ::   2

-------------------------------------------------

4D MATRIX, MAT4(:,:,:,:) ::

Mat4( :, :,  1, 1 )

 -0.2222222      0.2222222      0.1111111     -0.1111111
 -0.2222222      0.2222222      0.1111111     -0.1111111
 -0.1111111      0.1111111      0.2222222     -0.2222222
 -0.1111111      0.1111111      0.2222222     -0.2222222

Mat4( :, :,  1, 2 )

 -0.1111111      0.1111111      0.5555556E-01 -0.5555556E-01
 -0.1111111      0.1111111      0.5555556E-01 -0.5555556E-01
 -0.5555556E-01  0.5555556E-01  0.1111111     -0.1111111
 -0.5555556E-01  0.5555556E-01  0.1111111     -0.1111111

Mat4( :, :,  2, 1 )

 -0.1111111      0.1111111      0.5555556E-01 -0.5555556E-01
 -0.1111111      0.1111111      0.5555556E-01 -0.5555556E-01
 -0.5555556E-01  0.5555556E-01  0.1111111     -0.1111111
 -0.5555556E-01  0.5555556E-01  0.1111111     -0.1111111

Mat4( :, :,  2, 2 )

 -0.2222222      0.2222222      0.1111111     -0.1111111
 -0.2222222      0.2222222      0.1111111     -0.1111111
 -0.1111111      0.1111111      0.2222222     -0.2222222
 -0.1111111      0.1111111      0.2222222     -0.2222222
```

**`NIPS = 9`, `NIPT = 2`**

```fortran
CALL STElemSD % getConvectiveMatrix_19( A = DummyMat4, Term1 = 0, Term2 = 1, Xtype = 'dx', MultiVar = .TRUE. )

MATRIX STORED IN ST-ELEMENT-SHAPEDATA

NIPS ::   9  NIPT ::   2

-------------------------------------------------

4D MATRIX, MAT4(:,:,:,:) ::

Mat4( :, :,  1, 1 )

 -0.2222222      0.2222222      0.1111111     -0.1111111
 -0.2222222      0.2222222      0.1111111     -0.1111111
 -0.1111111      0.1111111      0.2222222     -0.2222222
 -0.1111111      0.1111111      0.2222222     -0.2222222

Mat4( :, :,  1, 2 )

 -0.1111111      0.1111111      0.5555556E-01 -0.5555556E-01
 -0.1111111      0.1111111      0.5555556E-01 -0.5555556E-01
 -0.5555556E-01  0.5555556E-01  0.1111111     -0.1111111
 -0.5555556E-01  0.5555556E-01  0.1111111     -0.1111111

Mat4( :, :,  2, 1 )

 -0.1111111      0.1111111      0.5555556E-01 -0.5555556E-01
 -0.1111111      0.1111111      0.5555556E-01 -0.5555556E-01
 -0.5555556E-01  0.5555556E-01  0.1111111     -0.1111111
 -0.5555556E-01  0.5555556E-01  0.1111111     -0.1111111

Mat4( :, :,  2, 2 )

 -0.2222222      0.2222222      0.1111111     -0.1111111
 -0.2222222      0.2222222      0.1111111     -0.1111111
 -0.1111111      0.1111111      0.2222222     -0.2222222
 -0.1111111      0.1111111      0.2222222     -0.2222222
 ```
 
 > `NIPS = 4, NIPT = 1` is not sufficient, because the integrand is quadratic in time therefore, we need atleast `2` integration points in time domain. `NIPS = 4, NIPT = 2` will compute the integration accurately. Note that this case may change when mesh is moving. Because then additional time dependent terms may appear in the integrand.
 
### getConvectiveMatrix_20()

INTERFACE

```fortran
 SUBROUTINE getConvectiveMatrix_20( Obj, A, Term1, Term2, Xtype, MultiVar )

    USE Utility, ONLY : OUTERPROD

    CLASS( STConvectiveMatrix_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), DIMENSION( :, :, : ) :: A
    INTEGER( I4B ), INTENT( IN ) :: Term1, Term2
    CHARACTER( LEN = * ), INTENT( IN ) :: XType
    LOGICAL( LGT ), INTENT( IN ) :: MultiVar
```

DESCRIPTION

- `A(:,:,:)` is a three dimensional array. The shape of `A` is `(M,M,NIPS)`. The thrid index denotes the spatial-integation point. In this case, `A` changes with space, but it is constant in time.
- `Term1` and `Term2` are integers, and can take values 0 or 1. If They are 1 then it means first order spatial derivative. If they are 0 then it means no spatial-derivative.
- `Xtype` is a string, which stands for the type of spatial gradient. If `Xtype` is in the set `[dx, dX, dx1, dX1, x, X, x1, X1]` then it means the spatial derivative is with respect to the `x`. If it belongs to the set `[dy, dY, dx2, dX2, y, Y, x2, X2]` then it means the spatial derivative is with respect to the `y`. If it belongs to the set `[dz, dZ, dx3, dX3, z, Z, x3, X3]` then it means the spatial derivative is with respect to the `z`. 
- `MultiVar` is a logical parameter, which has no effect on the functionality of this method. However, this dummy argument is required to tell the compiler that this routine is different then other routine which involves _convection velocity_.

SYMBOLIC CALCULATION

$${}^{4}M(I,J,a,b) = {}^{a}\delta U_{iI} \quad \int_{Q_n} N^I T_a [ \mathbf{A_1} ]_{ij} \frac{\partial N^J T_b}{\partial x} {dQ} \quad {}^{b}f_{jJ}$$

$${}^{4}M(I,J,a,b) = {}^{a}\delta U_{iI} \quad \int_{Q_n} [ \mathbf{A_1} ]_{ji} \frac{\partial N^I T_a}{\partial x} N^J T_b {dQ}  \quad {}^{b}f_{jJ}$$

$${}^{4}M(I,J,a,b) = {}^{a}\delta U_{iI} \quad \int_{Q_n} N^I T_a [\mathbf{A_2}]_{ij} \frac{\partial N^J T_b}{\partial y} {dQ} \quad {}^{b}g_{jJ}$$

$${}^{4}M(I,J,a,b) = {}^{a}\delta U_{iI} \quad \int_{Q_n} [\mathbf{A_2}]_{ji} \frac{\partial N^I T_a}{\partial y} N^J T_b {dQ}  \quad {}^{b}g_{jJ}$$


$${}^{4}M(I,J,a,b) = {}^{a}\delta U_{iI} \quad \int_{Q_n} N^I T_a [\mathbf{A_3}]_{ij} \frac{\partial N^J T_b}{\partial z} {dQ} \quad {}^{b}h_{jJ}$$

$${}^{4}M(I,J,a,b) = {}^{a}\delta U_{iI} \quad \int_{Q_n} [\mathbf{A_3}]_{ji} \frac{\partial N^I T_a}{\partial z} N^J T_b {dQ}  \quad {}^{b}h_{jJ}$$

TESTING

```fortran
IF( ALLOCATED( DummyMat3 ) ) DEALLOCATE( DummyMat3 )
ALLOCATE( DummyMat3( 1, 1, NIPS ) )
DummyMat3 = 0.0_DFP; DummyMat3( 1, 1, : ) = 1.0_DFP

CALL STElemSD % getConvectiveMatrix( A = DummyMat3, Term1 = 0, Term2 = 1, XType = "dx", MultiVar = .TRUE. )    
CALL STElemSD % DISPLAYMATRIX4( )
```

**`NIPS = 4`, `NIPT = 2`**

```fortran
CALL STElemSD % getConvectiveMatrix_20( A = DummyMat3, Term1 = 0, Term2 = 1, Xtype = 'dx', MultiVar = .TRUE. )

MATRIX STORED IN ST-ELEMENT-SHAPEDATA

NIPS ::   4  NIPT ::   2

-------------------------------------------------

4D MATRIX, MAT4(:,:,:,:) ::

Mat4( :, :,  1, 1 )

 -0.2222222      0.2222222      0.1111111     -0.1111111
 -0.2222222      0.2222222      0.1111111     -0.1111111
 -0.1111111      0.1111111      0.2222222     -0.2222222
 -0.1111111      0.1111111      0.2222222     -0.2222222

Mat4( :, :,  1, 2 )

 -0.1111111      0.1111111      0.5555556E-01 -0.5555556E-01
 -0.1111111      0.1111111      0.5555556E-01 -0.5555556E-01
 -0.5555556E-01  0.5555556E-01  0.1111111     -0.1111111
 -0.5555556E-01  0.5555556E-01  0.1111111     -0.1111111

Mat4( :, :,  2, 1 )

 -0.1111111      0.1111111      0.5555556E-01 -0.5555556E-01
 -0.1111111      0.1111111      0.5555556E-01 -0.5555556E-01
 -0.5555556E-01  0.5555556E-01  0.1111111     -0.1111111
 -0.5555556E-01  0.5555556E-01  0.1111111     -0.1111111

Mat4( :, :,  2, 2 )

 -0.2222222      0.2222222      0.1111111     -0.1111111
 -0.2222222      0.2222222      0.1111111     -0.1111111
 -0.1111111      0.1111111      0.2222222     -0.2222222
 -0.1111111      0.1111111      0.2222222     -0.2222222
 ```
 
### getConvectiveMatrix_21()

INTERFACE

```fortran
 SUBROUTINE getConvectiveMatrix_21( Obj, A, Term1, Term2, Xtype, MultiVar )

    USE Utility, ONLY : OUTERPROD

    CLASS( STConvectiveMatrix_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), DIMENSION( :, : ) :: A
    INTEGER( I4B ), INTENT( IN ) :: Term1, Term2
    CHARACTER( LEN = * ), INTENT( IN ) :: XType
    LOGICAL( LGT ), INTENT( IN ) :: MultiVar
```

DESCRIPTION

- `A(:,:)` is a two dimensional array. The shape of `A` is `(M,M)`. In this case, `A` does not change in space and time domain.
- `Term1` and `Term2` are integers, and can take values 0 or 1. If they are 1 then it means first order spatial derivative. If they are 0 then it means no spatial-derivative.
- `Xtype` is a string, which stands for the type of spatial gradient. If `Xtype` is in the set `[dx, dX, dx1, dX1, x, X, x1, X1]` then it means the spatial derivative is with respect to the `x`. If it belongs to the set `[dy, dY, dx2, dX2, y, Y, x2, X2]` then it means the spatial derivative is with respect to the `y`. If it belongs to the set `[dz, dZ, dx3, dX3, z, Z, x3, X3]` then it means the spatial derivative is with respect to the `z`. 
- `MultiVar` is a logical parameter, which has no effect on the functionality of this method. However, this dummy argument is required to tell the compiler that this routine is different then other routine which involves _convection velocity_.

SYMBOLIC CALCULATION

$${}^{4}M(I,J,a,b) = {}^{a}\delta U_{iI} \quad \int_{Q_n} N^I T_a [ \mathbf{A_1} ]_{ij} \frac{\partial N^J T_b}{\partial x} {dQ} \quad {}^{b}f_{jJ}$$

$${}^{4}M(I,J,a,b) = {}^{a}\delta U_{iI} \quad \int_{Q_n} [ \mathbf{A_1} ]_{ji} \frac{\partial N^I T_a}{\partial x} N^J T_b {dQ}  \quad {}^{b}f_{jJ}$$

$${}^{4}M(I,J,a,b) = {}^{a}\delta U_{iI} \quad \int_{Q_n} N^I T_a [\mathbf{A_2}]_{ij} \frac{\partial N^J T_b}{\partial y} {dQ} \quad {}^{b}g_{jJ}$$

$${}^{4}M(I,J,a,b) = {}^{a}\delta U_{iI} \quad \int_{Q_n} [\mathbf{A_2}]_{ji} \frac{\partial N^I T_a}{\partial y} N^J T_b {dQ}  \quad {}^{b}g_{jJ}$$


$${}^{4}M(I,J,a,b) = {}^{a}\delta U_{iI} \quad \int_{Q_n} N^I T_a [\mathbf{A_3}]_{ij} \frac{\partial N^J T_b}{\partial z} {dQ} \quad {}^{b}h_{jJ}$$

$${}^{4}M(I,J,a,b) = {}^{a}\delta U_{iI} \quad \int_{Q_n} [\mathbf{A_3}]_{ji} \frac{\partial N^I T_a}{\partial z} N^J T_b {dQ}  \quad {}^{b}h_{jJ}$$

TESTING

```fortran
IF( ALLOCATED( DummyMat2 ) ) DEALLOCATE( DummyMat2 )
ALLOCATE( DummyMat2( 1, 1 ) )
DummyMat2 = 0.0_DFP; DummyMat2( 1, 1 ) = 1.0_DFP

CALL STElemSD % getConvectiveMatrix( A = DummyMat2, Term1 = 0, Term2 = 1, XType = "dx", MultiVar = .TRUE. )
CALL STElemSD % DISPLAYMATRIX4( )
```

**`NIPS = 4`, `NIPT = 2`**

```fortran
CALL STElemSD % getConvectiveMatrix_21( A = DummyMat2, Term1 = 0, Term2 = 1, Xtype = 'dx', MultiVar = .TRUE. )

MATRIX STORED IN ST-ELEMENT-SHAPEDATA

NIPS ::   4  NIPT ::   2

-------------------------------------------------

4D MATRIX, MAT4(:,:,:,:) ::

Mat4( :, :,  1, 1 )

 -0.2222222      0.2222222      0.1111111     -0.1111111
 -0.2222222      0.2222222      0.1111111     -0.1111111
 -0.1111111      0.1111111      0.2222222     -0.2222222
 -0.1111111      0.1111111      0.2222222     -0.2222222

Mat4( :, :,  1, 2 )

 -0.1111111      0.1111111      0.5555556E-01 -0.5555556E-01
 -0.1111111      0.1111111      0.5555556E-01 -0.5555556E-01
 -0.5555556E-01  0.5555556E-01  0.1111111     -0.1111111
 -0.5555556E-01  0.5555556E-01  0.1111111     -0.1111111

Mat4( :, :,  2, 1 )

 -0.1111111      0.1111111      0.5555556E-01 -0.5555556E-01
 -0.1111111      0.1111111      0.5555556E-01 -0.5555556E-01
 -0.5555556E-01  0.5555556E-01  0.1111111     -0.1111111
 -0.5555556E-01  0.5555556E-01  0.1111111     -0.1111111

Mat4( :, :,  2, 2 )

 -0.2222222      0.2222222      0.1111111     -0.1111111
 -0.2222222      0.2222222      0.1111111     -0.1111111
 -0.1111111      0.1111111      0.2222222     -0.2222222
 -0.1111111      0.1111111      0.2222222     -0.2222222
```

### getConvectiveMatrix_24()

INTERFACE

```fortran
 SUBROUTINE getConvectiveMatrix_24( Obj, A, Term1, Term2, MultiVar )

    USE Utility, ONLY : OUTERPROD

    CLASS( STConvectiveMatrix_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), DIMENSION( :, :, :, : ) :: A
    CHARACTER( LEN = * ), INTENT( IN ) :: Term1, Term2
    LOGICAL( LGT ), INTENT( IN ) :: MultiVar
```

DESCRIPTION

- `A(:,:, :, :)` is a four-dimensional array. The shape of `A` is `(M,M, NIPS, NIPT)`. The third index of `A` denotes the spatial-integration point. The fourth index of A denotes the temporal integration point. In this case, `A` changes in both space and time domain.
- `Term1` and `Term2` are strings, and denotes the either the temporal derivative or spatial derivative.
	- If `Term1` or `Term2` is in the set `[dx, dX, dx1, dX1, x, X, x1, X1]` then it means the spatial derivative is with respect to the `x`.
	- If it belongs to the set `[dy, dY, dx2, dX2, y, Y, x2, X2]` then it means the spatial derivative is with respect to the `y`.
	- If it belongs to the set `[dz, dZ, dx3, dX3, z, Z, x3, X3]` then it means the spatial derivative is with respect to the `z`.
	- If it belongs to the set `[dt, Dt, DT, dT]` then it means temporal derivative. 
- `MultiVar` is a logical parameter, which has no effect on the functionality of this method. However, this dummy argument is required to tell the compiler that this routine is different then other routine which involves _convection velocity_.

> One of the `Term1` and `Term2` must be `dt`, Why? because it is convective matrix ;).

SYMBOLIC CALCULATION

$${}^{4}M(I,J,a,b) = {}^{a}\delta U_{iI} \quad \int_{Q_n} \frac{\partial N^I T_a}{\partial t} [ \mathbf{A_1} ]_{ij} \frac{\partial N^J T_b}{\partial x} {dQ} \quad {}^{b}f_{jJ}$$

$${}^{4}M(I,J,a,b) = {}^{a}\delta U_{iI} \quad \int_{Q_n} [ \mathbf{A_1} ]_{ji} \frac{\partial N^I T_a}{\partial x} \frac{\partial N^J T_b}{\partial t}  {dQ}  \quad {}^{b}f_{jJ}$$

$${}^{4}M(I,J,a,b) = {}^{a}\delta U_{iI} \quad \int_{Q_n} \frac{\partial N^I T_a}{\partial t}  [\mathbf{A_2}]_{ij} \frac{\partial N^J T_b}{\partial y} {dQ} \quad {}^{b}g_{jJ}$$

$${}^{4}M(I,J,a,b) = {}^{a}\delta U_{iI} \quad \int_{Q_n} [\mathbf{A_2}]_{ji} \frac{\partial N^I T_a}{\partial y} \frac{\partial N^J T_b}{\partial t}  {dQ}  \quad {}^{b}g_{jJ}$$


$${}^{4}M(I,J,a,b) = {}^{a}\delta U_{iI} \quad \int_{Q_n} \frac{\partial N^I T_a}{\partial t}  [\mathbf{A_3}]_{ij} \frac{\partial N^J T_b}{\partial z} {dQ} \quad {}^{b}h_{jJ}$$

$${}^{4}M(I,J,a,b) = {}^{a}\delta U_{iI} \quad \int_{Q_n} [\mathbf{A_3}]_{ji} \frac{\partial N^I T_a}{\partial z} \frac{\partial N^J T_b}{\partial t}  {dQ}  \quad {}^{b}h_{jJ}$$

TESTING

```fortran
IF( ALLOCATED( DummyMat4 ) ) DEALLOCATE( DummyMat4 )
ALLOCATE( DummyMat4( 1, 1, NIPS, NIPT ) )
DummyMat4 = 0.0_DFP; DummyMat4( 1, 1, :, : ) = 1.0_DFP

CALL STElemSD % getConvectiveMatrix( A = DummyMat4, Term1 = 'dt', Term2 = 'dx', MultiVar = .TRUE. )
CALL STElemSD % DISPLAYMATRIX4( )
```

**`NIPS = 4`, `NIPT = 1`**

```fortran
CALL STElemSD % getConvectiveMatrix_24( A = DummyMat4, Term1 = 'dt', Term2 = 'dx', MultiVar = .TRUE. )

MATRIX STORED IN ST-ELEMENT-SHAPEDATA

NIPS ::   4  NIPT ::   2

-------------------------------------------------

4D MATRIX, MAT4(:,:,:,:) ::

Mat4( :, :,  1, 1 )

  0.1666667     -0.1666667     -0.8333333E-01  0.8333333E-01
  0.1666667     -0.1666667     -0.8333333E-01  0.8333333E-01
  0.8333333E-01 -0.8333333E-01 -0.1666667      0.1666667
  0.8333333E-01 -0.8333333E-01 -0.1666667      0.1666667

Mat4( :, :,  1, 2 )

  0.1666667     -0.1666667     -0.8333333E-01  0.8333333E-01
  0.1666667     -0.1666667     -0.8333333E-01  0.8333333E-01
  0.8333333E-01 -0.8333333E-01 -0.1666667      0.1666667
  0.8333333E-01 -0.8333333E-01 -0.1666667      0.1666667

Mat4( :, :,  2, 1 )

 -0.1666667      0.1666667      0.8333333E-01 -0.8333333E-01
 -0.1666667      0.1666667      0.8333333E-01 -0.8333333E-01
 -0.8333333E-01  0.8333333E-01  0.1666667     -0.1666667
 -0.8333333E-01  0.8333333E-01  0.1666667     -0.1666667

Mat4( :, :,  2, 2 )

 -0.1666667      0.1666667      0.8333333E-01 -0.8333333E-01
 -0.1666667      0.1666667      0.8333333E-01 -0.8333333E-01
 -0.8333333E-01  0.8333333E-01  0.1666667     -0.1666667
 -0.8333333E-01  0.8333333E-01  0.1666667     -0.1666667
```

**`NIPS = 4`, `NIPT = 2`**

```fortran

CALL STElemSD % getConvectiveMatrix_24( A = DummyMat4, Term1 = 'dt', Term2 = 'dx', MultiVar = .TRUE. )

MATRIX STORED IN ST-ELEMENT-SHAPEDATA

NIPS ::   4  NIPT ::   2

-------------------------------------------------

4D MATRIX, MAT4(:,:,:,:) ::

Mat4( :, :,  1, 1 )

  0.1666667     -0.1666667     -0.8333333E-01  0.8333333E-01
  0.1666667     -0.1666667     -0.8333333E-01  0.8333333E-01
  0.8333333E-01 -0.8333333E-01 -0.1666667      0.1666667
  0.8333333E-01 -0.8333333E-01 -0.1666667      0.1666667

Mat4( :, :,  1, 2 )

  0.1666667     -0.1666667     -0.8333333E-01  0.8333333E-01
  0.1666667     -0.1666667     -0.8333333E-01  0.8333333E-01
  0.8333333E-01 -0.8333333E-01 -0.1666667      0.1666667
  0.8333333E-01 -0.8333333E-01 -0.1666667      0.1666667

Mat4( :, :,  2, 1 )

 -0.1666667      0.1666667      0.8333333E-01 -0.8333333E-01
 -0.1666667      0.1666667      0.8333333E-01 -0.8333333E-01
 -0.8333333E-01  0.8333333E-01  0.1666667     -0.1666667
 -0.8333333E-01  0.8333333E-01  0.1666667     -0.1666667

Mat4( :, :,  2, 2 )

 -0.1666667      0.1666667      0.8333333E-01 -0.8333333E-01
 -0.1666667      0.1666667      0.8333333E-01 -0.8333333E-01
 -0.8333333E-01  0.8333333E-01  0.1666667     -0.1666667
 -0.8333333E-01  0.8333333E-01  0.1666667     -0.1666667
```
 
 > As expected `NIPS = 4, NIPT = 1` is enough for the present case, as the integrand is linear in time. However, this situation may change when the mesh is moving and new time dependent terms may appear.

### getConvectiveMatrix_25()

INTERFACE

```fortran
 SUBROUTINE getConvectiveMatrix_25( Obj, A, Term1, Term2, MultiVar )

    USE Utility, ONLY : OUTERPROD

    CLASS( STConvectiveMatrix_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), DIMENSION( :, :, : ) :: A
    CHARACTER( LEN = * ), INTENT( IN ) :: Term1, Term2
    LOGICAL( LGT ), INTENT( IN ) :: MultiVar
```

DESCRIPTION

- `A(:,:, :)` is a three-dimensional array. The shape of `A` is `(M,M, NIPS)`. The third index of `A` denotes the spatial-integration point. In this case, `A` changes in spatial domain but does not change with time.
- `Term1` and `Term2` are strings, and denotes the either the temporal derivative or spatial derivative.
	- If `Term1` or `Term2` is in the set `[dx, dX, dx1, dX1, x, X, x1, X1]` then it means the spatial derivative is with respect to the `x`.
	- If it belongs to the set `[dy, dY, dx2, dX2, y, Y, x2, X2]` then it means the spatial derivative is with respect to the `y`.
	- If it belongs to the set `[dz, dZ, dx3, dX3, z, Z, x3, X3]` then it means the spatial derivative is with respect to the `z`.
	- If it belongs to the set `[dt, Dt, DT, dT]` then it means temporal derivative. 
- `MultiVar` is a logical parameter, which has no effect on the functionality of this method. However, this dummy argument is required to tell the compiler that this routine is different then other routine which involves _convection velocity_.

> One of the `Term1` and `Term2` must be `dt`, Why? because it is convective matrix ;).

SYMBOLIC CALCULATION

$${}^{4}M(I,J,a,b) = {}^{a}\delta U_{iI} \quad \int_{Q_n} \frac{\partial N^I T_a}{\partial t} [ \mathbf{A_1} ]_{ij} \frac{\partial N^J T_b}{\partial x} {dQ} \quad {}^{b}f_{jJ}$$

$${}^{4}M(I,J,a,b) = {}^{a}\delta U_{iI} \quad \int_{Q_n} [ \mathbf{A_1} ]_{ji} \frac{\partial N^I T_a}{\partial x} \frac{\partial N^J T_b}{\partial t}  {dQ}  \quad {}^{b}f_{jJ}$$

$${}^{4}M(I,J,a,b) = {}^{a}\delta U_{iI} \quad \int_{Q_n} \frac{\partial N^I T_a}{\partial t}  [\mathbf{A_2}]_{ij} \frac{\partial N^J T_b}{\partial y} {dQ} \quad {}^{b}g_{jJ}$$

$${}^{4}M(I,J,a,b) = {}^{a}\delta U_{iI} \quad \int_{Q_n} [\mathbf{A_2}]_{ji} \frac{\partial N^I T_a}{\partial y} \frac{\partial N^J T_b}{\partial t}  {dQ}  \quad {}^{b}g_{jJ}$$


$${}^{4}M(I,J,a,b) = {}^{a}\delta U_{iI} \quad \int_{Q_n} \frac{\partial N^I T_a}{\partial t}  [\mathbf{A_3}]_{ij} \frac{\partial N^J T_b}{\partial z} {dQ} \quad {}^{b}h_{jJ}$$

$${}^{4}M(I,J,a,b) = {}^{a}\delta U_{iI} \quad \int_{Q_n} [\mathbf{A_3}]_{ji} \frac{\partial N^I T_a}{\partial z} \frac{\partial N^J T_b}{\partial t}  {dQ}  \quad {}^{b}h_{jJ}$$

TESTING

```fortran
IF( ALLOCATED( DummyMat3 ) ) DEALLOCATE( DummyMat3 )
ALLOCATE( DummyMat3( 1, 1, NIPS ) )
DummyMat3 = 0.0_DFP; DummyMat3( 1, 1, : ) = 1.0_DFP

CALL STElemSD % getConvectiveMatrix( A = DummyMat3, Term1 = 'dt', Term2 = 'dx', MultiVar = .TRUE. )
CALL STElemSD % DISPLAYMATRIX4( )
```

**`NIPS = 4`, `NIPT = 1`**

```fortran

CALL STElemSD % getConvectiveMatrix_25( A = DummyMat3, Term1 = 'dt', Term2 = 'dx', MultiVar = .TRUE. )

MATRIX STORED IN ST-ELEMENT-SHAPEDATA

NIPS ::   4  NIPT ::   1

-------------------------------------------------

4D MATRIX, MAT4(:,:,:,:) ::

Mat4( :, :,  1, 1 )

  0.1666667     -0.1666667     -0.8333333E-01  0.8333333E-01
  0.1666667     -0.1666667     -0.8333333E-01  0.8333333E-01
  0.8333333E-01 -0.8333333E-01 -0.1666667      0.1666667
  0.8333333E-01 -0.8333333E-01 -0.1666667      0.1666667

Mat4( :, :,  1, 2 )

  0.1666667     -0.1666667     -0.8333333E-01  0.8333333E-01
  0.1666667     -0.1666667     -0.8333333E-01  0.8333333E-01
  0.8333333E-01 -0.8333333E-01 -0.1666667      0.1666667
  0.8333333E-01 -0.8333333E-01 -0.1666667      0.1666667

Mat4( :, :,  2, 1 )

 -0.1666667      0.1666667      0.8333333E-01 -0.8333333E-01
 -0.1666667      0.1666667      0.8333333E-01 -0.8333333E-01
 -0.8333333E-01  0.8333333E-01  0.1666667     -0.1666667
 -0.8333333E-01  0.8333333E-01  0.1666667     -0.1666667

Mat4( :, :,  2, 2 )

 -0.1666667      0.1666667      0.8333333E-01 -0.8333333E-01
 -0.1666667      0.1666667      0.8333333E-01 -0.8333333E-01
 -0.8333333E-01  0.8333333E-01  0.1666667     -0.1666667
 -0.8333333E-01  0.8333333E-01  0.1666667     -0.1666667
```

### getConvectiveMatrix_26()

INTERFACE

```fortran
 SUBROUTINE getConvectiveMatrix_26( Obj, A, Term1, Term2, MultiVar )

    USE Utility, ONLY : OUTERPROD

    CLASS( STConvectiveMatrix_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), DIMENSION( :, : ) :: A
    CHARACTER( LEN = * ), INTENT( IN ) :: Term1, Term2
    LOGICAL( LGT ), INTENT( IN ) :: MultiVar
```

DESCRIPTION

- `A(:,:)` is a two-dimensional array. The shape of `A` is `(M,M)`. In this case, `A` does not changes in space-time domain.
- `Term1` and `Term2` are strings, and denotes the either the temporal derivative or spatial derivative.
	- If `Term1` or `Term2` is in the set `[dx, dX, dx1, dX1, x, X, x1, X1]` then it means the spatial derivative is with respect to the `x`.
	- If it belongs to the set `[dy, dY, dx2, dX2, y, Y, x2, X2]` then it means the spatial derivative is with respect to the `y`.
	- If it belongs to the set `[dz, dZ, dx3, dX3, z, Z, x3, X3]` then it means the spatial derivative is with respect to the `z`.
	- If it belongs to the set `[dt, Dt, DT, dT]` then it means temporal derivative. 
- `MultiVar` is a logical parameter, which has no effect on the functionality of this method. However, this dummy argument is required to tell the compiler that this routine is different then other routine which involves _convection velocity_.

> One of the `Term1` and `Term2` must be `dt`, Why? because it is convective matrix ;).

SYMBOLIC CALCULATION

$${}^{4}M(I,J,a,b) = {}^{a}\delta U_{iI} \quad \int_{Q_n} \frac{\partial N^I T_a}{\partial t} [ \mathbf{A_1} ]_{ij} \frac{\partial N^J T_b}{\partial x} {dQ} \quad {}^{b}f_{jJ}$$

$${}^{4}M(I,J,a,b) = {}^{a}\delta U_{iI} \quad \int_{Q_n} [ \mathbf{A_1} ]_{ji} \frac{\partial N^I T_a}{\partial x} \frac{\partial N^J T_b}{\partial t}  {dQ}  \quad {}^{b}f_{jJ}$$

$${}^{4}M(I,J,a,b) = {}^{a}\delta U_{iI} \quad \int_{Q_n} \frac{\partial N^I T_a}{\partial t}  [\mathbf{A_2}]_{ij} \frac{\partial N^J T_b}{\partial y} {dQ} \quad {}^{b}g_{jJ}$$

$${}^{4}M(I,J,a,b) = {}^{a}\delta U_{iI} \quad \int_{Q_n} [\mathbf{A_2}]_{ji} \frac{\partial N^I T_a}{\partial y} \frac{\partial N^J T_b}{\partial t}  {dQ}  \quad {}^{b}g_{jJ}$$


$${}^{4}M(I,J,a,b) = {}^{a}\delta U_{iI} \quad \int_{Q_n} \frac{\partial N^I T_a}{\partial t}  [\mathbf{A_3}]_{ij} \frac{\partial N^J T_b}{\partial z} {dQ} \quad {}^{b}h_{jJ}$$

$${}^{4}M(I,J,a,b) = {}^{a}\delta U_{iI} \quad \int_{Q_n} [\mathbf{A_3}]_{ji} \frac{\partial N^I T_a}{\partial z} \frac{\partial N^J T_b}{\partial t}  {dQ}  \quad {}^{b}h_{jJ}$$

TESTING

```fortran
IF( ALLOCATED( DummyMat2 ) ) DEALLOCATE( DummyMat2 )
ALLOCATE( DummyMat2( 1, 1 ) )
DummyMat2 = 0.0_DFP; DummyMat2( 1, 1 ) = 1.0_DFP

CALL STElemSD % getConvectiveMatrix( A = DummyMat2, Term1 = 'dt', Term2 = 'dx', MultiVar = .TRUE. )
CALL STElemSD % DISPLAYMATRIX4( )
```

**`NIPS = 4`, `NIPT = 1`**

```fortran

CALL STElemSD % getConvectiveMatrix_25( A = DummyMat2, Term1 = 'dt', Term2 = 'dx', MultiVar = .TRUE. )

MATRIX STORED IN ST-ELEMENT-SHAPEDATA

NIPS ::   4  NIPT ::   1

-------------------------------------------------

4D MATRIX, MAT4(:,:,:,:) ::

Mat4( :, :,  1, 1 )

  0.1666667     -0.1666667     -0.8333333E-01  0.8333333E-01
  0.1666667     -0.1666667     -0.8333333E-01  0.8333333E-01
  0.8333333E-01 -0.8333333E-01 -0.1666667      0.1666667
  0.8333333E-01 -0.8333333E-01 -0.1666667      0.1666667

Mat4( :, :,  1, 2 )

  0.1666667     -0.1666667     -0.8333333E-01  0.8333333E-01
  0.1666667     -0.1666667     -0.8333333E-01  0.8333333E-01
  0.8333333E-01 -0.8333333E-01 -0.1666667      0.1666667
  0.8333333E-01 -0.8333333E-01 -0.1666667      0.1666667

Mat4( :, :,  2, 1 )

 -0.1666667      0.1666667      0.8333333E-01 -0.8333333E-01
 -0.1666667      0.1666667      0.8333333E-01 -0.8333333E-01
 -0.8333333E-01  0.8333333E-01  0.1666667     -0.1666667
 -0.8333333E-01  0.8333333E-01  0.1666667     -0.1666667

Mat4( :, :,  2, 2 )

 -0.1666667      0.1666667      0.8333333E-01 -0.8333333E-01
 -0.1666667      0.1666667      0.8333333E-01 -0.8333333E-01
 -0.8333333E-01  0.8333333E-01  0.1666667     -0.1666667
 -0.8333333E-01  0.8333333E-01  0.1666667     -0.1666667
```

### getConvectiveMatrix_30()

INTERFACE

```fortran
 SUBROUTINE getConvectiveMatrix_30( Obj, A, A0, Term1, Term2, MultiVar )

    USE Utility, ONLY : OUTERPROD

    CLASS( STConvectiveMatrix_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), DIMENSION( :, :, :, : ) :: A, A0
    CHARACTER( LEN = * ), INTENT( IN ) :: Term1, Term2
    LOGICAL( LGT ), INTENT( IN ) :: MultiVar
```

DESCRIPTION

- `A(:,:,:,:)` and `A0(:,:,:,:)` is a four-dimensional array. The shape of `A` and `A0` is `(M, M, NIPS, NIPT)`. The third index denotes the spatial-integration point and the fourth index denotes the temporal integration point. In this case, `A` and `A0` changes in space-time domain.
- `Term1` and `Term2` are strings, and denotes the either the temporal derivative or spatial derivative.
	- If `Term1` or `Term2` is in the set `[dx, dX, dx1, dX1, x, X, x1, X1]` then it means the spatial derivative is with respect to the `x`.
	- If it belongs to the set `[dy, dY, dx2, dX2, y, Y, x2, X2]` then it means the spatial derivative is with respect to the `y`.
	- If it belongs to the set `[dz, dZ, dx3, dX3, z, Z, x3, X3]` then it means the spatial derivative is with respect to the `z`.
	- If it belongs to the set `[dt, Dt, DT, dT]` then it means temporal derivative. 
- `MultiVar` is a logical parameter, which has no effect on the functionality of this method. However, this dummy argument is required to tell the compiler that this routine is different then other routine which involves _convection velocity_.

> One of the `Term1` and `Term2` must be `dt`, Why? because it is convective matrix ;).

SYMBOLIC CALCULATION


$${}^{4}M(I,J,a,b) = {}^{a}\delta U_{iI} \quad \int_{Q_n} [ \mathbf{A_0}]_{ki} \frac{\partial N^I T_a}{\partial t} [ \mathbf{A_1} ]_{kj} \frac{\partial N^J T_b}{\partial x} {dQ} \quad {}^{b}f_{jJ}$$

$${}^{4}M(I,J,a,b) = {}^{a}\delta U_{iI} \quad \int_{Q_n} [ \mathbf{A_1} ]_{ki} \frac{\partial N^I T_a}{\partial x} [\mathbf{A_0}]_{kj} \frac{\partial N^J T_b}{\partial t}  {dQ}  \quad {}^{b}f_{jJ}$$

$${}^{4}M(I,J,a,b) = {}^{a}\delta U_{iI} \quad \int_{Q_n} [ \mathbf{A_0}]_{ki}\frac{\partial N^I T_a}{\partial t}  [\mathbf{A_2}]_{kj} \frac{\partial N^J T_b}{\partial y} {dQ} \quad {}^{b}g_{jJ}$$

$${}^{4}M(I,J,a,b) = {}^{a}\delta U_{iI} \quad \int_{Q_n} [\mathbf{A_2}]_{ki} \frac{\partial N^I T_a}{\partial y} [ \mathbf{A_0}]_{kj} \frac{\partial N^J T_b}{\partial t}  {dQ}  \quad {}^{b}g_{jJ}$$


$${}^{4}M(I,J,a,b) = {}^{a}\delta U_{iI} \quad \int_{Q_n} [ \mathbf{A_0}]_{ki}\frac{\partial N^I T_a}{\partial t}  [\mathbf{A_3}]_{kj} \frac{\partial N^J T_b}{\partial z} {dQ} \quad {}^{b}h_{jJ}$$

$${}^{4}M(I,J,a,b) = {}^{a}\delta U_{iI} \quad \int_{Q_n} [\mathbf{A_3}]_{ki} \frac{\partial N^I T_a}{\partial z} [ \mathbf{A_0}]_{kj} \frac{\partial N^J T_b}{\partial t}  {dQ}  \quad {}^{b}h_{jJ}$$

TESTING

```fortran
IF( ALLOCATED( DummyMat4 )) DEALLOCATE( DummyMat4 )
ALLOCATE( DummyMat4( 1, 1, NIPS, NIPT ) )
DummyMat4 = 1.0_DFP

CALL STElemSD % getConvectiveMatrix( term1 = 'dt', term2 = 'dx', A = DummyMat4, &
A0 = DummyMat4, Multivar = .TRUE. )

CALL BlankLines( )
WRITE( *, "(A)") "CALL STElemSD % getConvectiveMatrix( term1 = 'dt', term2 = 'dx', A = DummyMat4, &
A0 = DummyMat4, Multivar = .TRUE. )"

CALL STElemSD % DisplayMatrix4( )
```

**`NIPS = 4`, `NIPT = 1`**

```fortran
CALL STElemSD % getConvectiveMatrix( term1 = 'dt', term2 = 'dx', A = DummyMat4, A0 = DummyMat4, Multivar = .TRUE. )

MATRIX STORED IN ST-ELEMENT-SHAPEDATA

NIPS ::   4  NIPT ::   1

-------------------------------------------------

4D MATRIX, MAT4(:,:,:,:) ::

Mat4( :, :,  1, 1 )

  0.1666667     -0.1666667     -0.8333333E-01  0.8333333E-01
  0.1666667     -0.1666667     -0.8333333E-01  0.8333333E-01
  0.8333333E-01 -0.8333333E-01 -0.1666667      0.1666667
  0.8333333E-01 -0.8333333E-01 -0.1666667      0.1666667

Mat4( :, :,  1, 2 )

  0.1666667     -0.1666667     -0.8333333E-01  0.8333333E-01
  0.1666667     -0.1666667     -0.8333333E-01  0.8333333E-01
  0.8333333E-01 -0.8333333E-01 -0.1666667      0.1666667
  0.8333333E-01 -0.8333333E-01 -0.1666667      0.1666667

Mat4( :, :,  2, 1 )

 -0.1666667      0.1666667      0.8333333E-01 -0.8333333E-01
 -0.1666667      0.1666667      0.8333333E-01 -0.8333333E-01
 -0.8333333E-01  0.8333333E-01  0.1666667     -0.1666667
 -0.8333333E-01  0.8333333E-01  0.1666667     -0.1666667

Mat4( :, :,  2, 2 )

 -0.1666667      0.1666667      0.8333333E-01 -0.8333333E-01
 -0.1666667      0.1666667      0.8333333E-01 -0.8333333E-01
 -0.8333333E-01  0.8333333E-01  0.1666667     -0.1666667
 -0.8333333E-01  0.8333333E-01  0.1666667     -0.1666667
```

### getConvectiveMatrix_31()

INTERFACE

```fortran
 SUBROUTINE getConvectiveMatrix_31( Obj, A, A0, Term1, Term2, MultiVar )

    USE Utility, ONLY : OUTERPROD

    CLASS( STConvectiveMatrix_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), DIMENSION( :, :, : ) :: A, A0
    CHARACTER( LEN = * ), INTENT( IN ) :: Term1, Term2
    LOGICAL( LGT ), INTENT( IN ) :: MultiVar
```

DESCRIPTION

- `A(:,:,:)` and `A0(:,:,:)` is a three-dimensional array. The shape of `A` and `A0` is `(M, M, NIPS)`. The third index denotes the spatial-integration point. In this case, `A` and `A0` changes in spatial dimension but remain constant in time domain.
- `Term1` and `Term2` are strings, and denote either the temporal derivative or spatial derivative.
	- If `Term1` or `Term2` is in the set `[dx, dX, dx1, dX1, x, X, x1, X1]` then it means the spatial derivative is with respect to the `x`.
	- If it belongs to the set `[dy, dY, dx2, dX2, y, Y, x2, X2]` then it means the spatial derivative is with respect to the `y`.
	- If it belongs to the set `[dz, dZ, dx3, dX3, z, Z, x3, X3]` then it means the spatial derivative is with respect to the `z`.
	- If it belongs to the set `[dt, Dt, DT, dT]` then it means temporal derivative. 
- `MultiVar` is a logical parameter, which has no effect on the functionality of this method. However, this dummy argument is required to tell the compiler that this routine is different then other routine which involves _convection velocity_.

> One of the `Term1` and `Term2` must be `dt`, Why? because it is convective matrix ;).

SYMBOLIC CALCULATION


$${}^{4}M(I,J,a,b) = {}^{a}\delta U_{iI} \quad \int_{Q_n} [ \mathbf{A_0}]_{ki} \frac{\partial N^I T_a}{\partial t} [ \mathbf{A_1} ]_{kj} \frac{\partial N^J T_b}{\partial x} {dQ} \quad {}^{b}f_{jJ}$$

$${}^{4}M(I,J,a,b) = {}^{a}\delta U_{iI} \quad \int_{Q_n} [ \mathbf{A_1} ]_{ki} \frac{\partial N^I T_a}{\partial x} [\mathbf{A_0}]_{kj} \frac{\partial N^J T_b}{\partial t}  {dQ}  \quad {}^{b}f_{jJ}$$

$${}^{4}M(I,J,a,b) = {}^{a}\delta U_{iI} \quad \int_{Q_n} [ \mathbf{A_0}]_{ki}\frac{\partial N^I T_a}{\partial t}  [\mathbf{A_2}]_{kj} \frac{\partial N^J T_b}{\partial y} {dQ} \quad {}^{b}g_{jJ}$$

$${}^{4}M(I,J,a,b) = {}^{a}\delta U_{iI} \quad \int_{Q_n} [\mathbf{A_2}]_{ki} \frac{\partial N^I T_a}{\partial y} [ \mathbf{A_0}]_{kj} \frac{\partial N^J T_b}{\partial t}  {dQ}  \quad {}^{b}g_{jJ}$$


$${}^{4}M(I,J,a,b) = {}^{a}\delta U_{iI} \quad \int_{Q_n} [ \mathbf{A_0}]_{ki}\frac{\partial N^I T_a}{\partial t}  [\mathbf{A_3}]_{kj} \frac{\partial N^J T_b}{\partial z} {dQ} \quad {}^{b}h_{jJ}$$

$${}^{4}M(I,J,a,b) = {}^{a}\delta U_{iI} \quad \int_{Q_n} [\mathbf{A_3}]_{ki} \frac{\partial N^I T_a}{\partial z} [ \mathbf{A_0}]_{kj} \frac{\partial N^J T_b}{\partial t}  {dQ}  \quad {}^{b}h_{jJ}$$

TESTING

```fortran
IF( ALLOCATED( DummyMat3 )) DEALLOCATE( DummyMat3 )
ALLOCATE( DummyMat3( 1, 1, NIPS) )
DummyMat3 = 1.0_DFP

CALL STElemSD % getConvectiveMatrix( term1 = 'dt', term2 = 'dx', A = DummyMat3, &
A0 = DummyMat3, Multivar = .TRUE. )

CALL BlankLines( )
WRITE( *, "(A)") "CALL STElemSD % getConvectiveMatrix( term1 = 'dt', term2 = 'dx', A = DummyMat3, &
A0 = DummyMat3, Multivar = .TRUE. )"

CALL STElemSD % DisplayMatrix4( )
```

**`NIPS = 4`, `NIPT = 1`**

```fortran
CALL STElemSD % getConvectiveMatrix( term1 = 'dt', term2 = 'dx', A = DummyMat3, A0 = DummyMat3, Multivar = .TRUE. )

MATRIX STORED IN ST-ELEMENT-SHAPEDATA

NIPS ::   4  NIPT ::   1

-------------------------------------------------

4D MATRIX, MAT4(:,:,:,:) ::

Mat4( :, :,  1, 1 )

  0.1666667     -0.1666667     -0.8333333E-01  0.8333333E-01
  0.1666667     -0.1666667     -0.8333333E-01  0.8333333E-01
  0.8333333E-01 -0.8333333E-01 -0.1666667      0.1666667
  0.8333333E-01 -0.8333333E-01 -0.1666667      0.1666667

Mat4( :, :,  1, 2 )

  0.1666667     -0.1666667     -0.8333333E-01  0.8333333E-01
  0.1666667     -0.1666667     -0.8333333E-01  0.8333333E-01
  0.8333333E-01 -0.8333333E-01 -0.1666667      0.1666667
  0.8333333E-01 -0.8333333E-01 -0.1666667      0.1666667

Mat4( :, :,  2, 1 )

 -0.1666667      0.1666667      0.8333333E-01 -0.8333333E-01
 -0.1666667      0.1666667      0.8333333E-01 -0.8333333E-01
 -0.8333333E-01  0.8333333E-01  0.1666667     -0.1666667
 -0.8333333E-01  0.8333333E-01  0.1666667     -0.1666667

Mat4( :, :,  2, 2 )

 -0.1666667      0.1666667      0.8333333E-01 -0.8333333E-01
 -0.1666667      0.1666667      0.8333333E-01 -0.8333333E-01
 -0.8333333E-01  0.8333333E-01  0.1666667     -0.1666667
 -0.8333333E-01  0.8333333E-01  0.1666667     -0.1666667
```

### getConvectiveMatrix_32()

INTERFACE

```fortran
 SUBROUTINE getConvectiveMatrix_31( Obj, A, A0, Term1, Term2, MultiVar )

    USE Utility, ONLY : OUTERPROD

    CLASS( STConvectiveMatrix_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), DIMENSION( :, : ) :: A, A0
    CHARACTER( LEN = * ), INTENT( IN ) :: Term1, Term2
    LOGICAL( LGT ), INTENT( IN ) :: MultiVar
```

DESCRIPTION

- `A(:,:)` and `A0(:,:)` is a two-dimensional array. The shape of `A` and `A0` is `(M, M)`. In this case, `A` and `A0` does not change in spatial and temporal domain.
- `Term1` and `Term2` are strings, and denote either the temporal derivative or spatial derivative.
	- If `Term1` or `Term2` is in the set `[dx, dX, dx1, dX1, x, X, x1, X1]` then it means the spatial derivative is with respect to the `x`.
	- If it belongs to the set `[dy, dY, dx2, dX2, y, Y, x2, X2]` then it means the spatial derivative is with respect to the `y`.
	- If it belongs to the set `[dz, dZ, dx3, dX3, z, Z, x3, X3]` then it means the spatial derivative is with respect to the `z`.
	- If it belongs to the set `[dt, Dt, DT, dT]` then it means temporal derivative. 
- `MultiVar` is a logical parameter, which has no effect on the functionality of this method. However, this dummy argument is required to tell the compiler that this routine is different then other routine which involves _convection velocity_.

> One of the `Term1` and `Term2` must be `dt`, Why? because it is convective matrix ;).

SYMBOLIC CALCULATION


$${}^{4}M(I,J,a,b) = {}^{a}\delta U_{iI} \quad \int_{Q_n} [ \mathbf{A_0}]_{ki} \frac{\partial N^I T_a}{\partial t} [ \mathbf{A_1} ]_{kj} \frac{\partial N^J T_b}{\partial x} {dQ} \quad {}^{b}f_{jJ}$$

$${}^{4}M(I,J,a,b) = {}^{a}\delta U_{iI} \quad \int_{Q_n} [ \mathbf{A_1} ]_{ki} \frac{\partial N^I T_a}{\partial x} [\mathbf{A_0}]_{kj} \frac{\partial N^J T_b}{\partial t}  {dQ}  \quad {}^{b}f_{jJ}$$

$${}^{4}M(I,J,a,b) = {}^{a}\delta U_{iI} \quad \int_{Q_n} [ \mathbf{A_0}]_{ki}\frac{\partial N^I T_a}{\partial t}  [\mathbf{A_2}]_{kj} \frac{\partial N^J T_b}{\partial y} {dQ} \quad {}^{b}g_{jJ}$$

$${}^{4}M(I,J,a,b) = {}^{a}\delta U_{iI} \quad \int_{Q_n} [\mathbf{A_2}]_{ki} \frac{\partial N^I T_a}{\partial y} [ \mathbf{A_0}]_{kj} \frac{\partial N^J T_b}{\partial t}  {dQ}  \quad {}^{b}g_{jJ}$$


$${}^{4}M(I,J,a,b) = {}^{a}\delta U_{iI} \quad \int_{Q_n} [ \mathbf{A_0}]_{ki}\frac{\partial N^I T_a}{\partial t}  [\mathbf{A_3}]_{kj} \frac{\partial N^J T_b}{\partial z} {dQ} \quad {}^{b}h_{jJ}$$

$${}^{4}M(I,J,a,b) = {}^{a}\delta U_{iI} \quad \int_{Q_n} [\mathbf{A_3}]_{ki} \frac{\partial N^I T_a}{\partial z} [ \mathbf{A_0}]_{kj} \frac{\partial N^J T_b}{\partial t}  {dQ}  \quad {}^{b}h_{jJ}$$

TESTING

```fortran
IF( ALLOCATED( DummyMat2 )) DEALLOCATE( DummyMat2 )
ALLOCATE( DummyMat2( 1, 1) )
DummyMat2 = 1.0_DFP

CALL STElemSD % getConvectiveMatrix( term1 = 'dt', term2 = 'dx', A = DummyMat2, &
A0 = DummyMat2, Multivar = .TRUE. )

CALL BlankLines( )
WRITE( *, "(A)") "CALL STElemSD % getConvectiveMatrix( term1 = 'dt', term2 = 'dx', A = DummyMat2, &
A0 = DummyMat2, Multivar = .TRUE. )"

CALL STElemSD % DisplayMatrix4( )
```

**`NIPS = 4`, `NIPT = 1`**

```fortran
CALL STElemSD % getConvectiveMatrix( term1 = 'dt', term2 = 'dx', A = DummyMat2, A0 = DummyMat2, Multivar = .TRUE. )

MATRIX STORED IN ST-ELEMENT-SHAPEDATA

NIPS ::   4  NIPT ::   1

-------------------------------------------------

4D MATRIX, MAT4(:,:,:,:) ::

Mat4( :, :,  1, 1 )

  0.1666667     -0.1666667     -0.8333333E-01  0.8333333E-01
  0.1666667     -0.1666667     -0.8333333E-01  0.8333333E-01
  0.8333333E-01 -0.8333333E-01 -0.1666667      0.1666667
  0.8333333E-01 -0.8333333E-01 -0.1666667      0.1666667

Mat4( :, :,  1, 2 )

  0.1666667     -0.1666667     -0.8333333E-01  0.8333333E-01
  0.1666667     -0.1666667     -0.8333333E-01  0.8333333E-01
  0.8333333E-01 -0.8333333E-01 -0.1666667      0.1666667
  0.8333333E-01 -0.8333333E-01 -0.1666667      0.1666667

Mat4( :, :,  2, 1 )

 -0.1666667      0.1666667      0.8333333E-01 -0.8333333E-01
 -0.1666667      0.1666667      0.8333333E-01 -0.8333333E-01
 -0.8333333E-01  0.8333333E-01  0.1666667     -0.1666667
 -0.8333333E-01  0.8333333E-01  0.1666667     -0.1666667

Mat4( :, :,  2, 2 )

 -0.1666667      0.1666667      0.8333333E-01 -0.8333333E-01
 -0.1666667      0.1666667      0.8333333E-01 -0.8333333E-01
 -0.8333333E-01  0.8333333E-01  0.1666667     -0.1666667
 -0.8333333E-01  0.8333333E-01  0.1666667     -0.1666667
```
