# About EASIFEM

EASIFEM stands for Expandable And Scalable infrastructure for Finite Element Method. EASIFEM can be though of as a framework for implementing finite element method. This framework has been developed in [Modern-Fortran](https://fortran-lang.org/) programming language. The library is still in the development stage. EASIFEM is written both in OOP and Multiple dispatch approach.

The aim of EASIFEM is to ease the coding of FEM by using fortran language, especially for problems related to multiphysics and multiphase problems. The vision is also to implement new type of finite element method and develop new algorithms. The author of this library conduct his research in the area of computational geomechanics. So easifem has been used to develop several application in this area.

EASIFEM describes all types of datatype required for coding FEM, such as string, file, vector, array, degrees of freedom, indices, mesh, nodes, finite elements, domain, shape functions, quadrature rules, linear solver, sparse and dense matrix, iterative and non iterative solvers, among others. EASIFEM also have a strong interface with the [GMSH](https://gmsh.info/) for pre- and post- processing. In addition, it provides great interface with [PARAVIEW](https://www.paraview.org/) for visualization of results.

In future, EASIFEM, will also act as a platform to use exisiting finite element libraries in effortless manner. The vision of EASIFEM will always be  to keep  FEM programming easy and ellegant.