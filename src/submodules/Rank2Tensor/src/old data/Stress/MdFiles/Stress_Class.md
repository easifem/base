# Stress Class

## ToDo

* Extend Assignment operator for `Obj = Mat`. This should not change the stress type
* Extend Assignment operator for `Obj = Vec`. This should not change the stress type

## Structure

```fortran
 TYPE, PUBLIC :: Stress_
    REAL( DFP ), ALLOCATABLE :: V( : )
    INTEGER( I4B ) :: NSD
    CHARACTER( LEN = 50 ) :: StressType = ""
```

## Description

## Getting Started

### Constructing the Object

**Initiate** subroutine

```fortran
CALL Obj%Initiate( Vec, StresType )
CALL Obj%Initiate( Mat, VoigtLen, StressType )
CALL Obj%Initiate( Mat, StressType )
CALL Obj%Initiate( Obj2 )
CALL Obj%Initiate( TensorObj, VoigtLen, StressType )
CALL Obj%Initiate( TensorObj, StressType )
```

**Stress** function

```fortran
Obj = Stress( Vec, StresType )
Obj = Stress( Mat, VoigtLen, StressType )
Obj = Stress( Mat, StressType )
Obj = Stress( Obj2 )
Obj = Stress( TensorObj, VoigtLen, StressType )
Obj = Stress( TensorObj, StressType )
```

**Stress_Pointer** function

```fortran
Obj => Stress_Pointer( Vec, StresType )
Obj => Stress_Pointer( Mat, VoigtLen, StressType )
Obj => Stress_Pointer( Mat, StressType )
Obj => Stress_Pointer( Obj2 )
Obj => Stress_Pointer( TensorObj, VoigtLen, StressType )
Obj => Stress_Pointer( TensorObj, StressType )
```

### Getting the length of `Obj%V`

```fortran
tSize = .SIZE. Obj
```

The program stops if the `Obj%V` is not allocated.

### Getting the Stress Tensor in Voigt Form

Many times we need to get the stress tensor in voigt vector form with appropriate length. When we use `Obj = Mat` then the voigt vector length will be 6 even if the Mat retpresent the 2D, Rank-2 tensor. Therefore, it is very important to get voigt vector of correct length. For this we have designed the operator called `.Shape.`. `Obj .Shape. M` will return voigt vector of length M.

```fortran
Vec = Obj .Shape. M
```

Note that M should belong to the list {1,2,3,4,6}.


### Assignment Operator

```fortran
Obj = Obj2
TensorObj = Obj
Mat = Obj
Vec = Obj
Obj = Mat
Obj = TensorObj
Obj = Vec
```

```fortran
Obj = Obj2
```

The above call we copy `Obj2` into `Obj`


```fortran
TensorObj = Obj
```

The above call copies the content of `Obj` into  Rank2Tensor_ class object `TensorObj`

```fortran
Mat = Obj
```

The above call copies the content of `Obj` into the 3 by 3 array.

```fortran
Vec = Obj
```

The above call copies the content of the `Obj` into the vector. The length of the returned vector is same as the length of `Obj%V`

```fortran
Obj = Mat
```

The above call copies the content of `Mat` into the stress object `Obj`. The `StressType` remains unchanged.

```fortran
Obj = Vec
```

The above call copies the content of the `Vec` into the stress object `Obj`. The `StressType` remains unchanged.

```fortran
Obj = TensorObj
```

The above call copies the content of the `Rank2Tensor_` object into the stress Object `Obj`. The `StressType` remains unchanged.