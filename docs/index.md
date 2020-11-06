# EASIFEM

EASIFEM stands for Expandable And Scalable infrastructure for Finite Element Method. EASIFEM can be though of as a framework for implementing finite element method. This framework has been developed in [Modern-Fortran](https://fortran-lang.org/) programming language. The library is still in the development stage. EASIFEM is written both in OOP and Multiple dispatch approach.

The aim of EASIFEM is to ease the coding of FEM by using fortran language, especially for problems related to multiphysics and multiphase problems. The vision is also to implement new type of finite element method and develop new algorithms. The author of this library conduct his research in the area of computational geomechanics. So easifem has been used to develop several application in this area.

EASIFEM describes all types of datatype required for coding FEM, such as string, file, vector, array, degrees of freedom, indices, mesh, nodes, finite elements, domain, shape functions, quadrature rules, linear solver, sparse and dense matrix, iterative and non iterative solvers, among others. EASIFEM also have a strong interface with the [GMSH](https://gmsh.info/) for pre- and post- processing. In addition, it provides great interface with [PARAVIEW](https://www.paraview.org/) for visualization of results.

In future, EASIFEM, will also act as a platform to use exisiting finite element libraries in effortless manner. The vision of EASIFEM will always be  to keep  FEM programming easy and ellegant.

## Structure of EASIFEM

The structor of easifem is broadly classified into

- Base; The user data type defined inside the `Base` follow muli-dispatch paradigm. All these datatypes have been defined in module called `BaseType.f90` and the methods are included in module called `BaseMethod.f90`
- Classes; the data-type defined inside `Classes` follow OOP.
- Extpkgs; lib developed by other users

## Base

- [boundingBox](./Base/BoundingBox.md)
- [buffer](#)
- [DOF](#)
- [ElemShapeData](#)
- [FEMatrix](#)
- [FEVariable](#)
- [File](#)
- [IndexValue](#)
- [IterationData](#)
- [KeyValue](#)
- [QuadraturePoint](#)
- [Random](#)
- [Tensor](#)
- [ReferenceElement](#)
- [RealMatrix](#)
- [SparseMatrix](#)
- [IntVector](#)
- [RealVector](#)
- [AbstractArray](#)
- [AbstractMatrix](#)
- [AbstractVector](#)
- [BaseMethod](#)
- [BaseType](#)
- [BlasInterface](#)
- [ErrorHandling](#)
- [GlobalData](#)
- [IO](#)
- [Utility](#)


<!-- ## Welcome to GitHub Pages

You can use the [editor on GitHub](https://github.com/vickysharma0812/easifem/edit/master/docs/index.md) to maintain and preview the content for your website in Markdown files.

Whenever you commit to this repository, GitHub Pages will run [Jekyll](https://jekyllrb.com/) to rebuild the pages in your site, from the content in your Markdown files.

### Markdown

Markdown is a lightweight and easy-to-use syntax for styling your writing. It includes conventions for

```markdown
Syntax highlighted code block

# Header 1
## Header 2
### Header 3

- Bulleted
- List

1. Numbered
2. List

**Bold** and _Italic_ and `Code` text

[Link](url) and ![Image](src)
```

For more details see [GitHub Flavored Markdown](https://guides.github.com/features/mastering-markdown/).

### Jekyll Themes

Your Pages site will use the layout and styles from the Jekyll theme you have selected in your [repository settings](https://github.com/vickysharma0812/easifem/settings). The name of this theme is saved in the Jekyll `_config.yml` configuration file.

### Support or Contact

Having trouble with Pages? Check out our [documentation](https://docs.github.com/categories/github-pages-basics/) or [contact support](https://github.com/contact) and we’ll help you sort it out. -->
