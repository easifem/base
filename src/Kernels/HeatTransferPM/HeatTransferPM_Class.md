# Documentation

## Getting started

## User supplied functions

```fortran
ABSTRACT INTERFACE
PURE FUNCTION temp_func( matType, temp ) RESULT( Ans )
  IMPORT :: DFP, I4B
  REAL( DFP ), INTENT( IN ) :: temp
  INTEGER( I4B ), INTENT( IN ) :: matType
  REAL( DFP ) :: Ans
END FUNCTION temp_func
END INTERFACE

ABSTRACT INTERFACE
PURE FUNCTION mix_thermcond( matType, k_s, k_i, k_l, theta_s, theta_i, &
  & theta_l) RESULT(Ans)
  IMPORT :: DFP, I4B
  INTEGER( I4B ), INTENT( IN ) :: matType
  REAL( DFP ), INTENT( IN ) :: k_s
  REAL( DFP ), INTENT( IN ) :: k_i
  REAL( DFP ), INTENT( IN ) :: k_l
  REAL( DFP ), INTENT( IN ) :: theta_s
  REAL( DFP ), INTENT( IN ) :: theta_i
  REAL( DFP ), INTENT( IN ) :: theta_l
  REAL( DFP ) :: Ans
END FUNCTION mix_thermcond
END INTERFACE

ABSTRACT INTERFACE
PURE FUNCTION mix_volHeatCap( matType, C_s, C_i, C_l, theta_s, theta_i, &
  & theta_l) RESULT(Ans)
  IMPORT :: DFP, I4B
  INTEGER( I4B ), INTENT( IN ) :: matType
  REAL( DFP ), INTENT( IN ) :: C_s
  REAL( DFP ), INTENT( IN ) :: C_i
  REAL( DFP ), INTENT( IN ) :: C_l
  REAL( DFP ), INTENT( IN ) :: theta_s
  REAL( DFP ), INTENT( IN ) :: theta_i
  REAL( DFP ), INTENT( IN ) :: theta_l
  REAL( DFP ) :: Ans
END FUNCTION mix_volHeatCap
END INTERFACE
```

### `obj%initiate(nsd, dt)`

This is a simple routine which initiate the kernel. It will specify the number of spatial dimension and time step for the problem.

### `obj%setDomain(dom, omegaNo)`

This routine has been described in `Kernel_` datatype.

### `obj%setMaterialProperties()`

- We can also use `obj%setThermCond()` and `obj%setvolHeatCap()`
- Currently, these functions are used only to describe the constant material properties
- In near future, functionality to include any random thermal properties will be added
- Innear future, functionality to describe the temperature dependent thermal properties will be added

### `setAlgorithm()`

- `mainOption(1)`; `STATIC`, `SEMIDISCRETE`
- `extraOption(1)`; `MAT_PROPS_CONST` `MAT_PROPS_SPACE_TIME` `MAT_PROPS_SPACE`, `MAT_PROPS_TEMP`, `MAT_PROPS_PHASE_CHANGE`
-

## Extended procedures

- setAlgorithm
- setMaterialProperties

## Original procedure

## Procedure pointers

Original

- applyInitialCondition
- applyDBC
- applyNBC

extension

- setDOF
- assemble
- isConverged
- update
- writeData
- solve
- saveState
