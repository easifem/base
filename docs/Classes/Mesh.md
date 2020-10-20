---
title: Mesh type in easifem
author: Vikas Sharma
language: fortran
---

# Mesh in `EASIFEM`

## Status

- [ ] Incomplete task
  - [ ] Add `getFacetElement()` which returns the mesh of facet element
  - [ ] Add method to get interfacial mesh between two mesh object
  - [ ] Add method to get bounding box of a mesh
  - [ ] Add method to get facet element/boundary elements inside a bounding box
  - [ ] Add method to get submesh located inside the bounding box
  - [ ] Add method to merge the mesh

---

## Structure

In `Mesh_Class.f90` following three classes have been defined.
- `Mesh_Class`
- `MeshData_Class`
- `MeshConnectivity_Class`

Corresponding to these three Classes there are three submodules.
- `Mesh_Class@MeshMethods.f90`
- `Mesh_Class@MeshDataMethods.f90`
- `Mesh_Class@MeshConnectivityMethods.f90`

---

## Getting started

---

## Description of Methods
