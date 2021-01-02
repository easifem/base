---
title: Degrees of freedom in easifem
author: Vikas Sharma
language: fortran
---

# `DOF_` type documentation

## Module and Submodules

- `DOF_Method.f90`
- `DOF_Method@Constructor.f90`
- `DOF_Method@setMethod.f90`
- `DOF_Method@getMethod.f90`

## Structure

```fortran
INTEGER( I4B ), PARAMETER, PUBLIC :: DOF_FMT = 0
INTEGER( I4B ), PARAMETER, PUBLIC :: FMT_DOF = 0
INTEGER( I4B ), PARAMETER, PUBLIC :: NODES_FMT = 1
INTEGER( I4B ), PARAMETER, PUBLIC :: FMT_Nodes = 1

TYPE :: DOF_
  INTEGER( I4B ), ALLOCATABLE :: MAP( :, : ), ValMap( : )
  INTEGER( I4B ) :: StorageFMT = Nodes_FMT
END TYPE DOF_

PUBLIC :: DOF_

TYPE( DOF_ ), PUBLIC, PARAMETER :: TypeDOF = DOF_( MAP=NULL(), &
  ValMap=NULL(),  StorageFMT = Nodes_FMT )

TYPE :: DOFPointer_
  CLASS( DOF_ ), POINTER :: Ptr => NULL( )
END TYPE DOFPointer_

PUBLIC :: DOFPointer_
```

There are two types of storage formats:

- `DOF_FMT` or `FMT_DOF` which is called *degree of freedom format*, in this case spatial-temporal components are also considered as nodal DOFs.
- `Nodes_FMT` or `FMT_Nodes` which is called *nodes format*, in this case, nodal dof is a vector of all other degrees of freedom.

Further, to understand this concept consider the example of NSE, in which we have pressure and velocity as the physical variables. Following code has been used to initiate the `DOF_` object

```fortran
obj = dof( tNodes = [20, 10], Names = ['V', 'P'], SpaceCompo = [3, -1], &
  & Timecompo = [2, 2], StorageFMT = dof_FMT )
```

- In this case of `DOF_FMT` the degrees of freedom will be represented as shown below:

![Illustration of `FMT_DOF`](assets/dof_fmt.svg)

Here each $\left \{ {\cdot}_a^b\right \}$ denotes a vector of nodal values of dof.

In this case $V$ and $P$ can have different order of spatial interpolation. This type of arrangement leads to a block-structure.

Now consider the following case to understand `Nodes_FMT`:

```fortran
obj = dof( tNodes = [10], Names = ['V'], SpaceCompo = [3], &
  & Timecompo = [2], StorageFMT = Nodes_FMT )
```

In this case degrees of freedom are stored as shown in above figure.

> It is noteworthy that in the case of `Nodes_FMT` all physical variables should have same order of interpolation in space and time domain. However, in the case of `DOF_FMT`, they can have different order of interpolation.

To understand the structure of `Map` lets generate a `dof_` object using following code.

```fortran
obj = dof( tNodes = [20, 10], Names = ['V', 'P'], SpaceCompo = [3, -1], &
  & Timecompo = [2, 2], StorageFMT = dof_FMT )
```

In this case `MAP` is given by following table:

|   ICHAR      | Space Compo  | Time Compo  | tDOF  | Indx | tNodes |
|   ---        | ---          | ---         | ---   | ---  | ---    |
|   ICHAR("V") |   3          | 2           | 6     | 1    | 20     |
|   ICHAR("P") |   -1         | 2           | 2     | 7    | 10     |
|   0          |   0          | 0           | 8     | 9    | 140    |

- First col of `MAP` denotes the name of physical variable; only unit length characters are allowed in physical names
- Second col of `MAP` denotes space component in the physical variable
- Third col denotes time component
- Fourth col denotes total number of degrees of freedom
- Fifth col denotes the starting position of physical name; in this way components of a physical name, `V` are stored from 1 to 6
- Sixth col denotes the total number of spatial nodes in the components of a physical name