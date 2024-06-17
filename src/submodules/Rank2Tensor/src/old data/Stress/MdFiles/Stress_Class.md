# Stress Class

## ToDo

* Extend Assignment operator for `obj = Mat`. This should not change the stress type
* Extend Assignment operator for `obj = Vec`. This should not change the stress type

## Structure

```fortran
 TYPE, PUBLIC :: Stress_
    REAL( DFP ), ALLOCATABLE :: V( : )
    INTEGER( I4B ) :: NSD
    CHARACTER( LEN = 50 ) :: StressType = ""
```

## Description

## Getting Started

### Constructing the object

**Initiate** subroutine

```fortran
CALL obj%Initiate( Vec, StresType )
CALL obj%Initiate( Mat, VoigtLen, StressType )
CALL obj%Initiate( Mat, StressType )
CALL obj%Initiate( obj2 )
CALL obj%Initiate( Tensorobj, VoigtLen, StressType )
CALL obj%Initiate( Tensorobj, StressType )
```

**Stress** function

```fortran
obj = Stress( Vec, StresType )
obj = Stress( Mat, VoigtLen, StressType )
obj = Stress( Mat, StressType )
obj = Stress( obj2 )
obj = Stress( Tensorobj, VoigtLen, StressType )
obj = Stress( Tensorobj, StressType )
```

**Stress_Pointer** function

```fortran
obj => Stress_Pointer( Vec, StresType )
obj => Stress_Pointer( Mat, VoigtLen, StressType )
obj => Stress_Pointer( Mat, StressType )
obj => Stress_Pointer( obj2 )
obj => Stress_Pointer( Tensorobj, VoigtLen, StressType )
obj => Stress_Pointer( Tensorobj, StressType )
```

### Getting the length of `obj%V`

```fortran
tSize = .SIZE. obj
```

The program stops if the `obj%V` is not allocated.

### Getting the Stress Tensor in Voigt Form

Many times we need to get the stress tensor in voigt vector form with appropriate length. When we use `obj = Mat` then the voigt vector length will be 6 even if the Mat retpresent the 2D, Rank-2 tensor. Therefore, it is very important to get voigt vector of correct length. For this we have designed the operator called `.Shape.`. `obj .Shape. M` will return voigt vector of length M.

```fortran
Vec = obj .Shape. M
```

Note that M should belong to the list {1,2,3,4,6}.


### Assignment Operator

```fortran
obj = obj2
Tensorobj = obj
Mat = obj
Vec = obj
obj = Mat
obj = Tensorobj
obj = Vec
```

```fortran
obj = obj2
```

The above call we copy `obj2` into `obj`


```fortran
Tensorobj = obj
```

The above call copies the content of `obj` into  Rank2Tensor_ class object `Tensorobj`

```fortran
Mat = obj
```

The above call copies the content of `obj` into the 3 by 3 array.

```fortran
Vec = obj
```

The above call copies the content of the `obj` into the vector. The length of the returned vector is same as the length of `obj%V`

```fortran
obj = Mat
```

The above call copies the content of `Mat` into the stress object `obj`. The `StressType` remains unchanged.

```fortran
obj = Vec
```

The above call copies the content of the `Vec` into the stress object `obj`. The `StressType` remains unchanged.

```fortran
obj = Tensorobj
```

The above call copies the content of the `Rank2Tensor_` object into the stress object `obj`. The `StressType` remains unchanged.