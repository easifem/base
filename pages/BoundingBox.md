Category: Mesh
Documentation: partially-done
Metdata: concrete, high-level
Status: stable
Title: BoundingBox
Author: Vikas Sharma, Ph. D.

## Introduction

A `BoundingBox_` datatype contains `[x_min, y_min, z_min, x_max, y_max, z_max` and `NSD` to represents a bounding box.

## Getting started

To create an instance use the subroutine `initiate()` or the function `BoundingBox()`, `BoundingBox_Pointer()`.

Example 1

```fortran
	type(BoundingBox_) :: obj
  call initiate( obj, nsd = 2, lim=[0.0_DFP, 1.0_DFP, 0.0_DFP, 1.0_DFP, 0.0_DFP, 0.0_DFP] )
  call display( obj, msg="test1" )
```

Example 2

```fortran
subroutine test2
  type(BoundingBox_) :: obj, obj2
  call initiate( obj, 2, [0.0_DFP, 1.0_DFP, 0.0_DFP, 1.0_DFP, 0.0_DFP, 0.0_DFP] )
  call initiate(obj2, obj)
  call display( obj2, msg="test2")
end subroutine test2
```

Example 3

```fortran
subroutine test3
  type(BoundingBox_) :: obj
  obj = BoundingBox( nsd = 2, lim=[0.0_DFP, 1.0_DFP, 0.0_DFP, 1.0_DFP, 0.0_DFP, 0.0_DFP] )
  call display( obj, msg="test1" )
end subroutine test3
```

Example 4

```fortran
subroutine test4
  type(BoundingBox_) :: obj, obj2
  call initiate( obj, 2, [0.0_DFP, 1.0_DFP, 0.0_DFP, 1.0_DFP, 0.0_DFP, 0.0_DFP] )
  obj2 = BoundingBox(obj)
  call display( obj2, msg="test2")
end subroutine test
```

Example 5

```fortran
subroutine test5
  type(BoundingBox_) :: obj
  obj = boundingBox(RESHAPE([0.0_DFP, 1.0_DFP, 0.0_DFP, 1.0_DFP, 0.0_DFP, 0.0_DFP], [2,3]))
  call display(obj, "test5")
end subroutine test5
```

Example 6

```fortran
subroutine test6
  type(BoundingBox_) :: obj
  type(BoundingBox_), pointer :: obj2

  call initiate( obj, 2, [0.0_DFP, 1.0_DFP, 0.0_DFP, 1.0_DFP, 0.0_DFP, 0.0_DFP] )
  obj2 => BoundingBox_Pointer(obj)
  call display( obj2, msg="test6")
end subroutine test6
```

Example 7

```fortran
subroutine test7
  type(BoundingBox_), pointer :: obj
  obj => BoundingBox_Pointer(nsd=3, lim=[0.0_DFP, 1.0_DFP, 0.0_DFP, 1.0_DFP, 0.0_DFP, 0.0_DFP])
  call display(obj, "test7")
end subroutine test7
```