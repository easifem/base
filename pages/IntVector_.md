---
title: IntVector
author: Vikas Sharma, Ph.D.
date: 24 Feb 2021
---

- [ ] TODO Finish documentation of IntVector#Set-Method and IntVector#Get-Method  documentation.
    
# IntVector

!!! example ""
    Intvector contains a dynamic array of rank 1 of integer type. It can be used to construct ragged vectors. Or vector or arrays of intvector. 
    
## Structure

The structure of [[IntVector_]] is given below.

```fortran
TYPE :: IntVector_
  INTEGER(I4B) :: tDimension = 1_I4B
  INTEGER(I4B), ALLOCATABLE :: Val(:)
END TYPE IntVector_
```

### tDimension

Total dimension of the array, it is always one

### Val

Vectors of integers.

## Constructor methods

### Shape

!!! note ""
    Return the shape of IntVector in a fortran vector of size 1. See example [[IntVector_test_1]]

### Size

!!! note ""
    Return the size of IntVector, If the instance of intvector is not allocated then it will return 0. See example [[IntVector_test_1]]
        
### GetTotalDimension

!!! note ""
    Return a integer scalar, total dimension of IntVector. It will return 1.
    See example [[IntVector_test_1]]
    
### Allocate
    
!!! note ""
    Allocate the size of IntVector. See example [[IntVector_test_1]]
    
### Deallocate

!!! note ""
    Deallocate the data stored inside IntVector. See example [[IntVector_test_1]]

### Initiate

!!! note ""
    Initiate an instance of IntVector. See example [[IntVector_test_2]] for more details.
    
### IntVector

!!! note ""
    This is a function, which returns an intance of [[IntVector_]]. You can find more details about this function here 👉⚡  [[IntVector_test_3]].

### IntVector_Pointer 

!!! note ""
    This is a function, which returns a pointer to a newly created instance of [[IntVector_]]. You can find more details about this function here 👉⚡  [[IntVector_test_4]].

### isAllocated

!!! note ""
    This function returns true if the instance of intvector is allocated. See [[IntVector_test_1]] for usage.
    
### isInitiated

!!! note ""
    Alias to isAllocated method.

## IO methods

### Display

!!! note ""
    This function displays the content of intvector. You can find more details about this function here 👉⚡  [[IntVector_test_1]] [[IntVector_test_2]] [[IntVector_test_3]] [[IntVector_test_4]].

## Get methods
  
### Operator(.in.)

!!! note ""
    The operator (.in.) returns true if a integer set is subset of another integer set. You can find the usage and more details about this method 👉🔥 [[IntVector_test_5]] 

### Get

!!! note ""
    Returns the values stored inside intvector. See, 👉🔥 [[IntVector_test_6]] for more details. This routine has all the features of fortran for native integer vectors.

## Set methods

