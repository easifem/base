title: DOF
author: Vikas Sharma
category: DOF
documentation: partially-done
license: GNU-GPL3
metdata: concrete, high-level
status: stable
version: v1.0.0

## Introduction

DOF_ object type is defined to encapsulate the properties of nodal degrees of freedom. There are different ways to understand the concept of DOF. For example, a vector $\textbf{v}$ in three dimension space defines three degrees of freedom at each spatial nodes of the mesh (this corresponds to $v_x, v_y, v_z$.

Now, there arises two ways to store the nodal values of the physical variable (here, $\textbf{v}$).

- `FMT_DOF` : In this case each components of $v$  will be treated as an independent DOF. So there are actually three nodal degrees of freedom, which corresponds to the spatial components $V_x, V_y, V_z$. In abstract sense, each DOF is a scalar as they represent the individual component of a physical variable. This definition is very flexible because we are treating each nodal DOF as a component. Therefore, these nodal DOFs can have different length. Moreover, in space-time FEM, they can easily represent spatial-temporal components of a physical variable.
- `FMT_Nodes`: In this case all the components of $v$ are clubbed together, i.e., at each node degrees of freedom is represented by a small vector of these components. So we are defining a nodal vector of these small vectors. In this case, nodal degrees of freedom is a vector of all  degrees of freedom defined at a node.

`DOF_` object has following properties:

## Storage format

Further, to understand this concept consider an example in which we have pressure and velocity as the physical variables. Note that velocity is a vector and pressure is scalar physical variable. Then, following code can be used to define the `DOF_` object. Note that each physical variable contains two components in time domain.

```fortran
obj = dof( tNodes = [20, 10], Names = ['V', 'P'], SpaceCompo = [3, -1], &
  & Timecompo = [2, 2], StorageFMT = FMT_DOF )
```

![DOF storage format](../media/dof_fmt.svg)

In the case of `FMT_DOF` , the degrees of freedom will be represented as shown in above figure. Each term in  `{}` denotes a nodal vector of DOF (i.e., v11, v12, p1, p2, etc.). Note that, in this representation both V and P can have different order of interpolation. For example, it velocity can be quadratic and pressure can be linear. Needless to state, this yields a block-structured tangent matrix.

Now consider the following case to understand `FMT_NODES`:

```fortran
obj = dof( tNodes = [10], Names = ['V'], SpaceCompo = [3], &
  & Timecompo = [2], StorageFMT = FMT_NODES )
```

In this case degrees of freedom are stored as shown in above figure.

> It is noteworthy that in the case of `FMT_NODES` all physical variables should have same order of interpolation in space and time domain. However, in the case of `FMT_DOF`, they can have different order of interpolation.

## Encapsulation of properties in MAP

To understand the structure of `Map` lets generate a `dof_` object using following code.

```fortran
obj = dof( tNodes = [20, 10], Names = ['V', 'P'], SpaceCompo = [3, -1], &
  & Timecompo = [2, 2], StorageFMT = dof_FMT )
```

In this case `MAP` is given by following table:

[Untitled](https://www.notion.so/0421256c2a194dc3b2787096d8048ba6)

- Column 1 of `MAP` denotes the name of physical variable; only unit length characters are allowed in physical names
- Column 2 of `MAP` denotes space component in the physical variable
- Column 3 denotes time component
- Column 4 denotes total number of degrees of freedom
- Column 5 denotes the starting position of physical name; in this way components of a physical name, `V` are stored from 1 to 6
- Column 6 denotes the total number of spatial nodes in the components of a physical name