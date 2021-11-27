# Space Time Mass Matrix

## ToDo

## Description

The mass matrix in case of space-time finite element involves time derivative. The following four forms are possible. 

## Structure

1. Term1 = 0, and Term2 = 0 implies following form

$$\int_{Q_n}{N^I T_a N^J T_b}\ d\Omega dt$$

2. Term1 = 1, and Term2 = 0 implies following form

$$\int_{Q_n} \frac{\partial N^I T_a}{\partial t} {N^J T_b}\ d\Omega dt$$

3. Term1 = 0, and Term2 = 1 implies following form

$$\int_{Q_n} {N^I T_a} \frac{\partial N^J T_b}{\partial t} \ d\Omega dt$$

4. Term1 = 1, and Term2 = 1 implies following form

$$\int_{Q_n} \frac{\partial N^I T_a}{\partial t} \frac{\partial N^J T_b}{\partial t} \ d\Omega dt$$

## Getting Started

### Making the object

Using the `STMassMatrix()`

```fortran
Obj = STMassMatrix( )
Obj = STMassMatrix( Row, Col, NIPS, NIPT )
Obj = STMassMatrix( I1, I2, I3, I4, NIPS, NIPT )
```

Using the `STMassMatrix_Pointer()`

```fortran
Obj => STMassMatrix_Pointer( )
Obj => STMassMatrix_Pointer( Row, Col, NIPS, NIPT )
Obj => STMassMatrix_Pointer( I1, I2, I3, I4, NIPS, NIPT )
```

### Getting the mass matrix

We can compute the following matrices

$$
\mathop {\bf{M}}\limits_{ST} \left( {I,J,a,b} \right): = {}^a\delta {u_I}\left[ {\int_{{I_n}}^{} {\int_\Omega ^{} {{N^I}{T_a}} {N^J}{T_b}} d\Omega dt} \right]{}^b{u_J}
$$

$$
\mathop {\bf{M}}\limits_{ST} \left( {I,J,a,b} \right) = {}^a\delta {u_I}\left[ {\int_{{I_n}}^{} {\int_\Omega ^{} {\frac{{\partial {N^I}{T_a}}}{{\partial t}}} {N^J}{T_b}} d\Omega dt} \right]{}^b{u_J}
$$

$$
\mathop {\bf{M}}\limits_{ST} \left( {I,J,a,b} \right) = {}^a\delta {u_I}\left[ {\int_{{I_n}}^{} {\int_\Omega ^{} {\frac{{\partial {N^I}{T_a}}}{{\partial t}}} \frac{{\partial {N^J}{T_b}}}{{\partial t}}} d\Omega dt} \right]{}^b{u_J}
$$

$$
\mathop {\bf{M}}\limits_{ST} \left( {I,J,a,b} \right) = {}^a\delta {u_I}\left[ {\int_{{I_n}}^{} {\int_\Omega ^{} {{N^I}{T_a}} \frac{{\partial {N^J}{T_b}}}{{\partial t}}} d\Omega dt} \right]{}^b{u_J}
$$

using the following fortran command

```fortran
CALL Obj % getMassMatrix( Term1, Term2 )
```

The following loop has been implemented to compute these matrices

- For **(1,1)** following loop have been implemented

```fortran
DO IPT = 1, NIPT 

    DO IPS = 1, NIPS

        SD => Obj % SD( IPS, IPT )
        RealVal = SD % Ws * SD % Wt * SD % Js_Xi2Xt * SD % Jt * SD % Thickness * rho

        DO b = 1, NNT

            DO a = 1, NNT 

                Obj % Mat4( :, :, a, b ) = Obj % Mat4( :, :, a, b ) + &
                OUTERPROD( a = SD % dNTdt( :, a ), b = SD % dNTdt( :, b ) ) * RealVal

            END DO

        END DO

    END DO

END DO
```

- For **(1,0)** following loop have been implemented

```fortran
DO IPT = 1, NIPT

    DO IPS = 1, NIPS

        SD => Obj % SD( IPS, IPT )
        RealVal1 = SD % Ws * SD % Wt * SD % Js_Xi2Xt * SD % Jt * SD % Thickness * rho

        DO a = 1, NNT

            Mat2 = OUTERPROD( a = SD % dNTdt( :, a ), b = SD % N )

            DO b = 1, NNT

                RealVal = SD % T( b ) * RealVal1

                Obj % Mat4( :, :, a, b ) = Obj % Mat4( :, :, a, b ) &
                                            + Mat2 * RealVal

            END DO

        END DO

    END DO

END DO
```

- For **(0,1)** following loop have been implemented

```fortran
DO IPT = 1, NIPT

    DO IPS = 1, NIPS

        SD => Obj % SD( IPS, IPT )
        RealVal1 = SD % Ws * SD % Wt * SD % Js_Xi2Xt * SD % Jt * SD % Thickness * rho

        DO b = 1, NNT

            Mat2 = OUTERPROD( a = SD % N, b = SD % dNTdt( :, b ) )

            DO a = 1, NNT

                RealVal = SD % T( a ) * RealVal1
                Obj % Mat4( :, :, a, b ) = Obj % Mat4( :, :, a, b ) &
                                            + Mat2 * RealVal

            END DO

        END DO

    END DO

END DO
```

- For **(0,0)** following loop have been implmented

```fortran
DO IPT = 1, NIPT

    DO IPS = 1, NIPS

        SD => Obj % SD( IPS, IPT )
        RealVal1 = SD % Ws * SD % Wt * SD % Js_Xi2Xt * SD % Jt * SD % Thickness * rho
        Mat2 = OUTERPROD( a = SD % N, b = SD % N )

        DO b = 1, NNT

            DO a = 1, NNT

                RealVal = SD % T( a ) * SD % T( b ) * RealVal1
                Obj % Mat4( :, :, a, b ) = Obj % Mat4( :, :, a, b ) &
                                            + Mat2 * RealVal

            END DO

        END DO

    END DO

END DO
```

---

The space-time mass matrices

$$
{\mathop {\bf{M}}\limits_{ST} ^{ii}}\left( {I,J,a,b} \right): = {}^a\delta {u_{iI}}\left[ {\int_{{I_n}}^{} {\int_\Omega ^{} {{N^I}{T_a}} {N^J}{T_b}} d\Omega dt} \right]{}^b{u_{iJ}}
$$

$$
{\mathop {\bf{M}}\limits_{ST} ^{ii}}\left( {I,J,a,b} \right) = {}^a\delta {u_{iI}}\left[ {\int_{{I_n}}^{} {\int_\Omega ^{} {\frac{{\partial {N^I}{T_a}}}{{\partial t}}} {N^J}{T_b}} d\Omega dt} \right]{}^b{u_{iJ}}
$$

$$
\mathop {\bf{M}}\limits_{ST} ^{ii} \left( {I,J,a,b} \right) = {}^a\delta {u_{iI}}\left[ {\int_{{I_n}}^{} {\int_\Omega ^{} {\frac{{\partial {N^I}{T_a}}}{{\partial t}}} \frac{{\partial {N^J}{T_b}}}{{\partial t}}} d\Omega dt} \right]{}^b{u_{iJ}}
$$

$$
\mathop {\bf{M}}\limits_{ST}^{ii} \left( {I,J,a,b} \right) = {}^a\delta {u_{iI}}\left[ {\int_{{I_n}}^{} {\int_\Omega ^{} {{N^I}{T_a}} \frac{{\partial {N^J}{T_b}}}{{\partial t}}} d\Omega dt} \right]{}^b{u_{iJ}}
$$

can be computed using the following fortran command.

```fortran
CALL Obj % getMassMatrix( Term1, Term2, nCopy )
```

We can compute the following space-time mass matrices

$$
\mathop {\bf{M}}\limits_{ST} \left( {I,J,a,b} \right): = {}^a\delta {u_I}\left[ {\int_{{I_n}}^{} {\int_\Omega ^{} {{N^I}{T_a}} \rho(\mathbf{x},t) {N^J}{T_b}} d\Omega dt} \right]{}^b{u_J}
$$

$$
\mathop {\bf{M}}\limits_{ST} \left( {I,J,a,b} \right) = {}^a\delta {u_I}\left[ {\int_{{I_n}}^{} {\int_\Omega ^{} {\frac{{\partial {N^I}{T_a}}}{{\partial t}}} \rho(\mathbf{x},t) {N^J}{T_b}} d\Omega dt} \right]{}^b{u_J}
$$

$$
\mathop {\bf{M}}\limits_{ST} \left( {I,J,a,b} \right) = {}^a\delta {u_I}\left[ {\int_{{I_n}}^{} {\int_\Omega ^{} {\frac{{\partial {N^I}{T_a}}}{{\partial t}}} \rho(\mathbf{x},t) \frac{{\partial {N^J}{T_b}}}{{\partial t}}} d\Omega dt} \right]{}^b{u_J}
$$

$$
\mathop {\bf{M}}\limits_{ST} \left( {I,J,a,b} \right) = {}^a\delta {u_I}\left[ {\int_{{I_n}}^{} {\int_\Omega ^{} {{N^I}{T_a}} \rho(\mathbf{x},t) \frac{{\partial {N^J}{T_b}}}{{\partial t}}} d\Omega dt} \right]{}^b{u_J}
$$


```fortran
CALL Obj % getMassMatrix( rho, Term1, Term2 )
```

In above case `rho` can be Rank-0, Rank-1, Rank-2.

- `rho`, Rank-0, denotes the constant value of $\rho$ in both space and time
- `rho(:)`, Rank-1, denotes the space nodal values; $\rho := \rho(\mathbf{x})$ changes only in space
- `rho(:,:)`, Rank-2, denotes the space-time nodal values; $\rho := \rho(\mathbf{x}, t)$ changes in both space and time.

If the $\rho$ is defined on the integration points then we can use the following command.

```fortran
CALL Obj % getMassMatrix( rho, Term1, Term2, rhoType )
```

In the above case, `rhoType` can be `"NodalValues"` or `"QuadPoints"`. In this case, we `rho` can be Rank-0, Rank-1, Rank-2.

We can compute the following matrices

$$
\mathop {{{\bf{M}}^{ii}}}\limits_{ST} \left( {I,J,a,b} \right): = {}^a\delta {u_{iI}}\left[ {\int_{{I_n}}^{} {\int_\Omega ^{} {{N^I}{T_a}} \rho ({\bf{x}},t){N^J}{T_b}} d\Omega dt} \right]{}^b{u_{iJ}}
$$

$$
{\mathop {\bf{M}}\limits_{ST} ^{ii}}\left( {I,J,a,b} \right) = {}^a\delta {u_{iI}}\left[ {\int_{{I_n}}^{} {\int_\Omega ^{} {\frac{{\partial {N^I}{T_a}}}{{\partial t}}} \rho ({\bf{x}},t){N^J}{T_b}} d\Omega dt} \right]{}^b{u_{iJ}}
$$

$$
{\mathop {\bf{M}}\limits_{ST} ^{ii}}\left( {I,J,a,b} \right) = {}^a\delta {u_{iI}}\left[ {\int_{{I_n}}^{} {\int_\Omega ^{} {\frac{{\partial {N^I}{T_a}}}{{\partial t}}} \rho ({\bf{x}},t)\frac{{\partial {N^J}{T_b}}}{{\partial t}}} d\Omega dt} \right]{}^b{u_{iJ}}
$$

$$
{\mathop {\bf{M}}\limits_{ST} ^{ii}}\left( {I,J,a,b} \right) = {}^a\delta {u_{iI}}\left[ {\int_{{I_n}}^{} {\int_\Omega ^{} {{N^I}{T_a}} \rho ({\bf{x}},t)\frac{{\partial {N^J}{T_b}}}{{\partial t}}} d\Omega dt} \right]{}^b{u_{iJ}}
$$

the above intergrals can be computed using the following fortran command.

```fortran
CALL Obj % getMassMatrix( rho, Term1, Term2, nCopy )
```

If $\rho$ is defined on integral points then we can use following fortran command.

In the above case, `rho` can be Rank-0, Rank-1, Rank-2.

```fortran
CALL Obj % getMassMatrix( rho, Term1, Term2, RhoType, nCopy )
```

In the above case, `rho` can be Rank-0, Rank-1, Rank-2.

We can compute the following matrices

$$
{\mathop {\bf{M}}\limits_{ST} ^{pq}}\left( {I,J,a,b} \right) = {}^a\delta {u_{iI}}\left[ {\int_{{I_n}}^{} {\int_\Omega ^{} {{N^I}{T_a}} {N^J}{T_b}\frac{{\partial {u_p}}}{{\partial {x_r}}}\frac{{\partial {c_r}}}{{\partial {u_q}}}} d\Omega dt} \right]\Delta {}^b{u_{qJ}}
$$

using the foloowing fortran command

```fortran
CALL Obj % getMassMatrix( U, dCdU, 0, 0, dCdU_Type)
```

- In the above call `dCdU` denotes the jacobian matrix for the mapping from $u \rightarrow c$ and it can be given by Rank-2, Rank-3, Rank-4.
- `dCdU_Type` can be `NodalValues` or `QuadPoints`.
- If $\frac{\partial c}{\partial u}$ changes in space and time then we must represent it using Rank-4 array.
- If $\frac{\partial c}{\partial u}$ changes only in space, and remains contant in time then we must represent it using Rank-3 array.
- If $\frac{\partial c}{\partial u}$ does not change in space and time then we must represent it using Rank-2 array.
- `U` is a space-time nodal values of velocity, and it is represented by Rank-3 array.

We can compute the following matrices

$$
{\mathop {\bf{M}}\limits_{ST} ^{pq}}\left( {I,J,a,b} \right) = {}^a\delta {u_{iI}}\left[ {\int_{{I_n}}^{} {\int_\Omega ^{} {\frac{{\partial {N^I}{T_a}}}{{\partial t}}} {N^J}{T_b}\frac{{\partial {u_p}}}{{\partial {x_r}}}\frac{{\partial {c_r}}}{{\partial {u_q}}}} d\Omega dt} \right]\Delta {}^b{u_{qJ}}
$$

using the foloowing fortran command

```fortran
CALL Obj % getMassMatrix( U, dCdU, 1, 0, dCdU_Type)
```

- In the above call `dCdU` denotes the jacobian matrix for the mapping from $u \rightarrow c$ and it can be given by Rank-2, Rank-3, Rank-4.
- `dCdU_Type` can be `NodalValues` or `QuadPoints`.
- If $\frac{\partial c}{\partial u}$ changes in space and time then we must represent it using Rank-4 array.
- If $\frac{\partial c}{\partial u}$ changes only in space, and remains contant in time then we must represent it using Rank-3 array.
- If $\frac{\partial c}{\partial u}$ does not change in space and time then we must represent it using Rank-2 array.
- `U` is a space-time nodal values of velocity, and it is represented by Rank-3 array.

In case of _Naviar-Stokes_ equation or _Burgers_ Equation the jacobian $\frac{\partial c_p}{\partial u_q}$ is identity (i.e. $c(\mathbf{x},t) = u(\mathbf{x},t)$) in such case we get following matrices

$$
{\mathop {\bf{M}}\limits_{ST} ^{pq}}\left( {I,J,a,b} \right) = {}^a\delta {u_{iI}}\left[ {\int_{{I_n}}^{} {\int_\Omega ^{} {{N^I}{T_a}} {N^J}{T_b}\frac{{\partial {u_p}}}{{\partial {x_q}}}} d\Omega dt} \right]\Delta {}^b{u_{qJ}}
$$

the above matrix can be computed using following fortran command.

```fortran
CALL Obj % getMassMatrix( U, 0, 0 )
```

In case of _Naviar-Stokes_ equation or _Burgers_ Equation the jacobian $\frac{\partial c_p}{\partial u_q}$ is identity (i.e. $c(\mathbf{x},t) = u(\mathbf{x},t)$) in such case we get following matrices

$$
\mathop {\bf{M}}\limits_{ST}^{pq} \left( {I,J,a,b} \right) = {}^a\delta {u_{iI}}\left[ {\int_{{I_n}}^{} {\int_\Omega ^{} {\frac{{\partial {N^I}{T_a}}}{{\partial t}}} {N^J}{T_b}\frac{{\partial {u_p}}}{{\partial {x_q}}}} d\Omega dt} \right]{\Delta ^b}{u_{qJ}}
$$

The above matrix can be computed using following fortran command.

```fortran
CALL Obj % getMassMatrix( U, 1, 0 )
```

## Methods

### _Constructor1()_

INTERFACE

```fortran
 FUNCTION Constructor1( Row, Col, NIPS, NIPT )

    CLASS( STMassMatrix_ ), POINTER ::  Constructor1
    INTEGER( I4B ), INTENT( IN ) :: row, col, NIPS, NIPT
    ALLOCATE( Constructor1 )
    ALLOCATE( Constructor1 % Mat2( row, col ) )
    Constructor1 % Mat2 = 0.0_DFP
    CALL Constructor1 % Initiate( NIPS = NIPS, NIPT = NIPT )
END FUNCTION Constructor1
```

DESCRIPTION
    This function allocated `Obj % Mat2`, and also allocated `Obj % SD`. 

### _Constructor2()_

INTERFACE

```fortran
 FUNCTION Constructor2( I1, I2, I3, I4, NIPS, NIPT )

    CLASS( STMassMatrix_ ), POINTER ::  Constructor2
    INTEGER( I4B ), INTENT( IN ) :: I1, I2, I3, I4, NIPS, NIPT
    ALLOCATE( Constructor2 )
    ALLOCATE( Constructor2 % Mat4( I1, I2, I3, I4 ) )
    Constructor2 % Mat4 = 0.0_DFP
    CALL Constructor2 % Initiate( NIPS = NIPS, NIPT = NIPT )
 END FUNCTION Constructor2
```

DESCRIPTION
    This function allocated `Obj % Mat4`, and also allocated `Obj % SD`. 
    

### _getMassMatrix\_1()_

INTERFACE

```fortran
SUBROUTINE getMassMatrix_1( Obj, rho, Term1, Term2 )
    CLASS( STMassMatrix_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), INTENT( IN ) :: rho
    INTEGER( I4B ), INTENT( IN ) :: Term1, Term2
```

DESCRIPTION

This subroutine computes the mass matrix. `rho` is constant in this case which can be used as a scale.

### _getMassMatrix\_2()_

INTERFACE 

```fortran
 SUBROUTINE getMassMatrix_2( Obj, Term1, Term2 )
   CLASS( STMassMatrix_ ), INTENT( INOUT ) ::  Obj
    INTEGER( I4B ), INTENT( IN ) :: Term1, Term2
```

DESCRIPTION

In this case `rho` $\rho$ is absent. 

### _getMAssMatrix\_3()_

INTERFACE

```fortran
 SUBROUTINE getMassMatrix_3( Obj, rho, Term1, Term2 )

    USE Utility, ONLY : OUTERPROD

    CLASS( STMassMatrix_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), DIMENSION( : ), INTENT( IN ) :: rho
    INTEGER( I4B ), INTENT( IN ) :: Term1, Term2
```

DESCRIPTION

In this case `rho` $\rho$ is a vector, i.e. it is the spatial nodal values of scalar variable `rho`. In this case, `rho` $rho$ does not change with time, but only varies in space.

### _getMAssMatrix\_4()_

INTERFACE

```fortran
SUBROUTINE getMassMatrix_4( Obj, rho, Term1, Term2 )

    USE Utility, ONLY : OUTERPROD

    CLASS( STMassMatrix_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), DIMENSION( :, : ), INTENT( IN ) :: rho
    INTEGER( I4B ), INTENT( IN ) :: Term1, Term2
```

DESCRIPTION

In this case `rho` $\rho$ is a two dimensional array, i.e. it is the space-time nodal values of scalar variable `rho`. The first index of `rho` denotes the spatial node, and second index of `rho` denotes the temporal node. In this case, `rho` $rho$ changes in both space and time domain.

### _getMAssMatrix\_5()_

INTERFACE

```fortran
 SUBROUTINE getMassMatrix_5( Obj, rho, Term1, Term2, rhoType )

    USE Utility, ONLY : OUTERPROD
    
    CLASS( STMassMatrix_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), DIMENSION( :, : ), INTENT( IN ) :: rho
    INTEGER( I4B ), INTENT( IN ) :: Term1, Term2
    CHARACTER( LEN = * ), INTENT( IN ) :: RhoType
```

DESCRIPTION

In this case `rho` $\rho$ is a two dimensional array. If `rhotype` is `[Integration Points, IntegrationPoints, Quad Points, QuadPoints, Quad]` then it is defined at space-time integation points. In this case the number of rows in `rho` must be equal to `NIPS` and number of columns in `rho` must be equal to the `NIPT`. The first index of `rho` denotes the spatial integation point, and second index of `rho` denotes the temporal integration points. In this case, `rho` $rho$ changes in both space and time domain. This method will be useful in forming the *stabilized* matrices. If `rhotype` is `[Nodal, Nodal Values, NodalValues, STNodalValues]` then `rho` is defined at space-time nodal values and the methods call the `getMassMatrix_4()` for computation.

### _getMassMatrix\_6()_

INTERFACE

```fortran
 SUBROUTINE getMassMatrix_6( Obj, rho, Term1, Term2, rhoType )

    USE Utility, ONLY : OUTERPROD

    CLASS( STMassMatrix_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), DIMENSION( : ), INTENT( IN ) :: rho
    INTEGER( I4B ), INTENT( IN ) :: Term1, Term2
    CHARACTER( LEN = * ), INTENT( IN ) :: RhoType
```

DESCRIPTION

In this case `rho` $\rho$ is a one dimensional array. If `rhotype` is `[Integration Points, IntegrationPoints, Quad Points, QuadPoints, Quad]` then it is defined at space integation points. In this case the size of `rho` must be equal to `NIPS`. The elements of `rho` denotes the spatial integation pointIn this case, `rho` $rho$ changes in only in space, and not in time domain. This method will be useful in forming the *stabilized* matrices. If `rhotype` is `[Nodal, Nodal Values, NodalValues, STNodalValues]` then `rho` is defined at space nodal values and the methods call the `getMassMatrix_3()` for computation.

### _getMassMatrix\_7()_

INTERFACE

```fortran
 SUBROUTINE getMassMatrix_7( Obj, rho, Term1, Term2, nCopy )

    USE Utility, ONLY : OUTERPROD

    CLASS( STMassMatrix_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), INTENT( IN ) :: rho
    INTEGER( I4B ), INTENT( IN ) :: nCopy, Term1, Term2
```

DESCRIPTION

In this case `rho` $\rho$ is a real scalar. This subroutine computes the `nCopy` of the mass matrix. **This Need More Explaination**

### _getMassMatrix\_8()_

INTERFACE

```fortran
 SUBROUTINE getMassMatrix_8( Obj, Term1, Term2, nCopy )

    CLASS( STMassMatrix_ ), INTENT( INOUT ) ::  Obj
    INTEGER( I4B ), INTENT( IN ) :: nCopy, Term1, Term2
```

DESCRIPTION

In this case `rho` is absent. This subroutine computes the `nCopy` of the mass matrix. **This Need More Explaination**

### _getMassMatrix\_9()_

INTERFACE

```fortran
 SUBROUTINE getMassMatrix_9( Obj, rho, Term1, Term2, nCopy )

    USE Utility, ONLY : OUTERPROD

    CLASS( STMassMatrix_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), DIMENSION( : ), INTENT( IN ) :: rho
    INTEGER( I4B ), INTENT( IN ) :: Term1, Term2, nCopy
```

DESCRIPTION

In this case `rho` is a vector, and represent the spatial nodal values. This subroutine computes the `nCopy` of the mass matrix. **This Need More Explaination**

### _getMassMatrix\_10()_

INTERFACE

```fortran
SUBROUTINE getMassMatrix_10( Obj, rho, Term1, Term2, nCopy )

    USE Utility, ONLY : OUTERPROD

    CLASS( STMassMatrix_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), DIMENSION( :, : ), INTENT( IN ) :: rho
    INTEGER( I4B ), INTENT( IN ) :: Term1, Term2, nCopy
```

DESCRIPTION

In this case `rho` is a 2D array, and represent the space-time nodal values. This subroutine computes the `nCopy` of the mass matrix. **This Need More Explaination**

### _getMassMatrix\_11()_

INTERFACE

```fortran
SUBROUTINE getMassMatrix_11( Obj, rho, Term1, Term2, rhoType, nCopy )

    USE Utility, ONLY : OUTERPROD

    CLASS( STMassMatrix_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), DIMENSION( :, : ), INTENT( IN ) :: rho
    INTEGER( I4B ), INTENT( IN ) :: Term1, Term2, nCopy
    CHARACTER( LEN = * ), INTENT( IN ) :: RhoType
```

DESCRIPTION

In this case `rho` is a 2D array, and can represent the space-time nodal values, or space-time integation point values. This subroutine computes the `nCopy` of the mass matrix. **This Need More Explaination**

### _getMassMatrix\_12()_

INTERFACE

```fortran
SUBROUTINE getMassMatrix_12( Obj, rho, Term1, Term2, rhoType, nCopy )

    USE Utility, ONLY : OUTERPROD

    CLASS( STMassMatrix_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), DIMENSION( : ), INTENT( IN ) :: rho
    INTEGER( I4B ), INTENT( IN ) :: Term1, Term2, nCopy
    CHARACTER( LEN = * ), INTENT( IN ) :: RhoType
```

DESCRIPTION

In this case `rho` is a vector, and can represent the space-nodal values or space-integration value. This subroutine computes the `nCopy` of the mass matrix. **This Need More Explaination**
