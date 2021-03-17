! This program is a part of EASIFEM library
! Copyright (C) 2020-2021  Vikas Sharma, Ph.D
!
! This program is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <https: //www.gnu.org/licenses/>
!

!> authors: Vikas Sharma, Ph. D.
! date: 	10 March 2021
! summary: 	This module contains method for [[Rank2Tensor_]]

MODULE Rank2Tensor_Method
USE GlobalData
USE BaseType
IMPLICIT NONE
PRIVATE

!----------------------------------------------------------------------------
!                                                      initiate@constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 13 March 2021
! summary: Initiates [[Rank2Tensor_]] from a matrix
!
!### Usage
!
!```fortran
! type( Rank2Tensor_ ) :: obj
! real( dfp ) :: mat(3,3)
! call random_number( mat )
! call display( mat, "mat=")
! call initiate( obj, mat )
! call display( obj, "test1: ")
! call initiate( obj, sym(mat), .true.)
! call display( obj, "test2: ")
!```
INTERFACE
MODULE PURE SUBROUTINE init_by_mat( Obj, Mat, isSym )
  CLASS( Rank2Tensor_ ), INTENT( INOUT ) :: Obj
  REAL( DFP ), INTENT( IN ) :: Mat( 3, 3 )
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: isSym
END SUBROUTINE init_by_mat
END INTERFACE

!----------------------------------------------------------------------------
!                                                      initiate@constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 13 March 2021
! summary: Initiates [[Rank2Tensor_]] from a [[VoigtRank2Tensor_]].
!
!@note
! This subroutine is part of Assignment(=) operator.
!@endnote
!
!### Usage
!
!```fortran
! type( Rank2Tensor_ ) :: obj
! real( dfp ) :: v( 6 )
! call random_number(v)
! call display( v, "v: ", orient="row" )
! call Initiate( obj, VoigtRank2Tensor( v, VoigtType=StressTypeVoigt ) )
! call display( obj, "obj: ")
!```

INTERFACE
MODULE PURE SUBROUTINE init_by_voigt( Obj, V )
  CLASS( Rank2Tensor_ ), INTENT( INOUT ) :: Obj
  CLASS( VoigtRank2Tensor_ ), INTENT( IN ) :: V
END SUBROUTINE init_by_voigt
END INTERFACE

!----------------------------------------------------------------------------
!                                                      Initiate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 13 March 2021
! summary: Initiates [[VoigtRank2Tensor_]] from a [[Rank2Tensor_]]

INTERFACE
MODULE PURE SUBROUTINE init_voigt_from_r2tensor( Obj, T, VoigtType )
  CLASS( VoigtRank2Tensor_ ), INTENT( INOUT ) :: Obj
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: T
  INTEGER( I4B ), INTENT( IN ) :: VoigtType
END SUBROUTINE init_voigt_from_r2tensor
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE init_by_mat, init_by_voigt, init_voigt_from_r2tensor
END INTERFACE Initiate

PUBLIC :: Initiate

!----------------------------------------------------------------------------
!                                                    Rank2Tensor@constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 March 2021
! summary: This function returns an instance of [[Rank2Tensor_]]
!
!### Usage
!
!```fortran
! type( Rank2Tensor_ ) :: obj
! real( dfp ) :: mat(3,3)
! call random_number( mat )
! call display( mat, "test3: mat=")
! obj = Rank2Tensor( mat )
! call display( obj, "test3: obj=")
! obj = Rank2Tensor( sym(mat), .true.)
! call display( obj, "test3: obj=")
!```

INTERFACE
MODULE PURE FUNCTION r2t_by_mat( Mat, isSym ) RESULT( Ans )
  REAL( DFP ), INTENT( IN ) :: Mat( 3, 3 )
  TYPE( Rank2Tensor_ ) :: Ans
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: isSym
END FUNCTION r2t_by_mat
END INTERFACE

!----------------------------------------------------------------------------
!                                                    Rank2Tensor@constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 March 2021
! summary: This function returns an instance of [[Rank2Tensor_]]
!
!### Usage
!
!```fortran
! type( Rank2Tensor_ ) :: obj
! real( dfp ) :: v(6)
! call random_number( v )
! call display( v, "test4 mat=")
! obj = Rank2Tensor( VoigtRank2Tensor(v, VoigtType=StressTypeVoigt) )
! call display( obj, "test4 obj=")
!```

INTERFACE
MODULE PURE FUNCTION r2t_by_voigt( V ) RESULT( Ans )
  CLASS( VoigtRank2Tensor_ ), INTENT( IN ) :: V
  TYPE( Rank2Tensor_ ) :: Ans
END FUNCTION r2t_by_voigt
END INTERFACE

INTERFACE Rank2Tensor
  MODULE PROCEDURE r2t_by_mat, r2t_by_voigt
END INTERFACE Rank2Tensor

PUBLIC :: Rank2Tensor

!----------------------------------------------------------------------------
!                                            Rank2Tensor_Pointer@constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 March 2021
! summary: This function returns the pointer to an newly created instance of [[Rank2Tensor_]].
!
!### Usage
!
!```fortran
! class( Rank2Tensor_ ), pointer :: obj
! real( dfp ) :: mat(3,3)
! call random_number( mat )
! call display( mat, "test5: mat=")
! obj => Rank2Tensor_Pointer( mat )
! call display( obj, "test5: obj=")
! obj => Rank2Tensor_Pointer( sym(mat), .true.)
! call display( obj, "test5: obj=")
!```

INTERFACE
MODULE PURE FUNCTION ptr_r2t_by_mat( Mat, isSym ) RESULT( Ans )
  REAL( DFP ), INTENT( IN ) :: Mat( 3, 3 )
  CLASS( Rank2Tensor_ ), POINTER :: Ans
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: isSym
END FUNCTION ptr_r2t_by_mat
END INTERFACE

!----------------------------------------------------------------------------
!                                                    Rank2Tensor@constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 March 2021
! summary: This function returns a pointer to a newly created instance of [[Rank2Tensor_]]
!
!### Usage
!
!```fortran
! class( Rank2Tensor_ ), pointer :: obj
! real( dfp ) :: v(6)
! call random_number( v )
! call display( v, "test6: mat=")
! obj => Rank2Tensor_Pointer( VoigtRank2Tensor(v, VoigtType=StressTypeVoigt))
! call display( obj, "test6: obj=")
!```

INTERFACE
MODULE PURE FUNCTION ptr_r2t_by_voigt( V ) RESULT( Ans )
  CLASS( VoigtRank2Tensor_ ), INTENT( IN ) :: V
  CLASS( Rank2Tensor_ ), POINTER :: Ans
END FUNCTION ptr_r2t_by_voigt
END INTERFACE

INTERFACE Rank2Tensor_Pointer
  MODULE PROCEDURE ptr_r2t_by_mat, ptr_r2t_by_voigt
END INTERFACE Rank2Tensor_Pointer

PUBLIC :: Rank2Tensor_Pointer

!----------------------------------------------------------------------------
!                                                     Assignment@constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 13 March 2021
! summary: [[Rank2Tensor_]] = Matrix(3,3)
!
!@note
! This SUBROUTINE will create an unsymmetric tensor
!@endnote
!
!### Usage
!
!```fortran
! type( Rank2Tensor_ ) :: obj
! real( dfp ) :: mat( 3, 3 )
! call random_number(mat)
! obj = mat
! call display( obj, "test7: obj=")
!```

INTERFACE
MODULE PURE SUBROUTINE r2tensor_eq_mat( Obj, Mat )
  CLASS( Rank2Tensor_ ), INTENT( INOUT ) :: Obj
  REAL( DFP ), INTENT( IN ) :: Mat( 3, 3 )
END SUBROUTINE r2tensor_eq_mat
END INTERFACE

!----------------------------------------------------------------------------
!                                                     Assignment@constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 13 March 2021
! summary: Matrix(3,3) = [[Rank2Tensor_]]
!
!
!### Usage
!
!```fortran
! type( Rank2Tensor_ ) :: obj
! real( dfp ) :: mat( 3, 3 )
! call random_number(mat)
! obj = mat
! call display( obj, "test7: obj=")
! mat = 0.0; mat = obj
! call display( mat, "test7: mat=")
!```


INTERFACE
MODULE PURE SUBROUTINE mat_eq_r2tensor( Mat, Obj )
  REAL( DFP ), INTENT( INOUT ) :: Mat( 3, 3 )
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: Obj
END SUBROUTINE mat_eq_r2tensor
END INTERFACE

!----------------------------------------------------------------------------
!                                                     Assignment@constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 March 2021
! summary: This routine returns a [[VoigtRank2Tensor_]] from [[Rank2Tensor_2]]
!
!@note
! The `VoigtType` will be `StressTypeVoigt`.
!@endnote
!
!### Usage
!
!```fortran
! type( Rank2Tensor_ ) :: obj
! type( VoigtRank2Tensor_ ) :: v
! real( dfp ) :: mat( 3, 3 )
! call random_number(mat)
! obj = mat
! call display( obj, "test8: obj=")
! v = obj
! call display( v, "test8: v=")
!```

INTERFACE
MODULE PURE SUBROUTINE voigt_eq_r2tensor( V, Obj )
  CLASS( VoigtRank2Tensor_ ), INTENT( INOUT ) :: V
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: Obj
END SUBROUTINE voigt_eq_r2tensor
END INTERFACE

INTERFACE ASSIGNMENT( = )
  MODULE PROCEDURE r2tensor_eq_mat, mat_eq_r2tensor, init_by_voigt, &
    & voigt_eq_r2tensor
END INTERFACE

PUBLIC :: ASSIGNMENT( = )

!----------------------------------------------------------------------------
!                                                 IdentityTensor@constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 March 2021
! summary: Returns the rank2 identity tensor
!
!### Usage
!
!```fortran
! type( Rank2Tensor_ ) :: obj
! call IdentityTensor(obj)
! call display( obj, "test9: IdentityTensor=")
! call Ones(obj)
! call display( obj, "test9: Ones=")
! call Zeros(obj)
! call display( obj, "test9: Zeros=")
! call IsotropicTensor(obj, 2.0_DFP)
! call display( obj, "test9: Isotropic=")
!```
INTERFACE
MODULE PURE SUBROUTINE identity_rank2( Obj )
  CLASS( Rank2Tensor_ ), INTENT( INOUT ) :: Obj
END SUBROUTINE identity_rank2
END INTERFACE

INTERFACE IdentityTensor
  MODULE PROCEDURE identity_rank2
END INTERFACE IdentityTensor

PUBLIC :: IdentityTensor

!----------------------------------------------------------------------------
!                                                     OnesTensor@constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 March 2021
! summary: Returns a second order tensor with all entry one
!
!### Usage
!
!```fortran
! type( Rank2Tensor_ ) :: obj
! call IdentityTensor(obj)
! call display( obj, "test9: IdentityTensor=")
! call Ones(obj)
! call display( obj, "test9: Ones=")
! call Zeros(obj)
! call display( obj, "test9: Zeros=")
! call IsotropicTensor(obj, 2.0_DFP)
! call display( obj, "test9: Isotropic=")
!```
INTERFACE
MODULE PURE SUBROUTINE Ones_rank2( Obj )
  CLASS( Rank2Tensor_ ), INTENT( INOUT ) :: Obj
END SUBROUTINE Ones_rank2
END INTERFACE

INTERFACE Ones
  MODULE PROCEDURE Ones_rank2
END INTERFACE Ones

PUBLIC :: Ones

!----------------------------------------------------------------------------
!                                                   ZerosTensor@constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 March 2021
! summary: Returns a zero second order tensor
!
!### Usage
!
!```fortran
! type( Rank2Tensor_ ) :: obj
! call IdentityTensor(obj)
! call display( obj, "test9: IdentityTensor=")
! call Ones(obj)
! call display( obj, "test9: Ones=")
! call Zeros(obj)
! call display( obj, "test9: Zeros=")
! call IsotropicTensor(obj, 2.0_DFP)
! call display( obj, "test9: Isotropic=")
!```

INTERFACE
MODULE PURE SUBROUTINE Zeros_rank2( Obj )
  CLASS( Rank2Tensor_ ), INTENT( INOUT ) :: Obj
END SUBROUTINE Zeros_rank2
END INTERFACE

INTERFACE Zeros
  MODULE PROCEDURE Zeros_rank2
END INTERFACE Zeros

PUBLIC :: Zeros

!----------------------------------------------------------------------------
!                                                IsotropicTensor@constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 March 2021
! summary: returns a second order isotropic tensor
!
!### Usage
!
!```fortran
! type( Rank2Tensor_ ) :: obj
! call IdentityTensor(obj)
! call display( obj, "test9: IdentityTensor=")
! call Ones(obj)
! call display( obj, "test9: Ones=")
! call Zeros(obj)
! call display( obj, "test9: Zeros=")
! call IsotropicTensor(obj, 2.0_DFP)
! call display( obj, "test9: Isotropic=")
!```

INTERFACE
MODULE PURE SUBROUTINE isotropic_rank2( Obj, Lambda )
  CLASS( Rank2Tensor_ ), INTENT( INOUT ) :: Obj
  REAL( DFP ), INTENT( IN ) :: Lambda
END SUBROUTINE isotropic_rank2
END INTERFACE

INTERFACE IsotropicTensor
  MODULE PROCEDURE isotropic_rank2
END INTERFACE IsotropicTensor

PUBLIC :: IsotropicTensor

!----------------------------------------------------------------------------
!                                                               Display@IO
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 March 2021
! summary: Display the content of [[Rank2Tensor_]]

INTERFACE
MODULE SUBROUTINE display_obj( Obj, Msg, UnitNo )
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: Obj
  CHARACTER( LEN = * ), INTENT( IN ) :: Msg
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: UnitNo
END SUBROUTINE display_obj
END INTERFACE

INTERFACE Display
  MODULE PROCEDURE display_obj
END INTERFACE Display

PUBLIC :: Display

!----------------------------------------------------------------------------
!                                                           Trace@Operation
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 March 2021
! summary: Returns trace of a tensor
!
!### Introduction
! Trace of a tensor is given by
! $$Tr(A) = A_{ii}$$
! Trace of $A^2$ is given by
! $$Tr(A^2) = A:A^T$$
! Trace of A^3 is given by
! $$Tr(A^3) = A^2 : A^T$$
!
!### Usage
!
!```fortran
! type( Rank2Tensor_ ) :: obj
! real( dfp ) :: mat( 3, 3 )
! call random_number( mat )
! obj = sym(mat)
! call display( trace(obj), "test10: trace(obj)=" )
!```

INTERFACE
MODULE PURE FUNCTION trace_obj( Obj, Power ) RESULT( Ans )
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: Obj
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: Power
  REAL( DFP ) :: Ans
END FUNCTION trace_obj
END INTERFACE

INTERFACE Trace
  MODULE PROCEDURE trace_obj
END INTERFACE Trace

PUBLIC :: Trace

!----------------------------------------------------------------------------
!                                                                J2@Operation
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 March 2021
! summary: Returns J2 invariant of tensor
!
!### Introduction
! $J_2$ is given by
! $$J_{2}\left( A\right)  =\frac{1}{2} tr\left( dev^{2}\left( A\right)  \right)$$
!
!@note
! if `isDeviatoric` logical flag is false then the function calculates the $J_2$ using the components of the tensor $A$.
!@endnote
!
!### Usage
!
!```fortran
! type( Rank2Tensor_ ) :: obj
! real( dfp ) :: mat( 3, 3 )
! call random_number( mat )
! obj = sym(mat)
! call display( J2(obj), "test10: trace(obj)=" )
!```

INTERFACE
MODULE PURE FUNCTION j2_obj( Obj, isDeviatoric ) RESULT( Ans )
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: Obj
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: isDeviatoric
  REAL( DFP ) :: Ans
END FUNCTION j2_obj
END INTERFACE

INTERFACE J2
  MODULE PROCEDURE j2_obj
END INTERFACE J2

PUBLIC :: J2

!----------------------------------------------------------------------------
!                                                                J3@Operation
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 March 2021
! summary: Returns J3 invarinat of a tensor
!
!### Introduction
! $J_3$ is an invariant of a tensor, which is given by
!
! $$J_{3}\left( A\right)  =Det\left( Dev\left( A\right)  \right)$$
!
!@note
! If the tensor is not a Deviatoric tensor the this function calculates the Deviatoric part of the tensor to determine $J_3$
!@endnote
!
!### Usage
!
!```fortran
! type( Rank2Tensor_ ) :: obj
! real( dfp ) :: mat( 3, 3 )
! call random_number( mat )
! obj = sym(mat)
! call display( J3(obj), "test10: trace(obj)=" )
!```

INTERFACE
MODULE PURE FUNCTION j3_obj( Obj, isDeviatoric ) RESULT( Ans )
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: Obj
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: isDeviatoric
  REAL( DFP ) :: Ans
END FUNCTION j3_obj
END INTERFACE

INTERFACE J3
  MODULE PROCEDURE j3_obj
END INTERFACE J3

PUBLIC :: J3

!----------------------------------------------------------------------------
!                                                               Det@Operation
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 March 2021
! summary: Returns the Determinant of a tensor
!
!### Introduction
! This function returns the Determinant of a tensor.
!
!### Usage
!
!```fortran
! type( Rank2Tensor_ ) :: obj
! real( dfp ) :: mat( 3, 3 )
! call random_number( mat )
! obj = sym(mat)
! call display( Det(obj), "test10: trace(obj)=" )
!```

INTERFACE
MODULE PURE FUNCTION det_obj( Obj ) RESULT( Ans )
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: Obj
  REAL( DFP ) :: Ans
END FUNCTION det_obj
END INTERFACE

INTERFACE Det
  MODULE PROCEDURE Det_obj
END INTERFACE Det

PUBLIC :: Det

!----------------------------------------------------------------------------
!                                                       LodeAngle@Operation
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 March 2021
! summary: Returns lode angle
!
!### Introduction
! This function calculates the Lode angle $\theta$ from the invariants $J_2$ and $J_3$, which is given by:
!
!$$ cos3\theta =\frac{3\sqrt{3} J_{3}}{2J_{2}\sqrt{J_{2}}} $$
!$$ sin3\theta =-\frac{3\sqrt{3} J_{3}}{2J_{2}\sqrt{J_{2}}} $$
!
! As mentioned above, Lode angle can be described in two ways; Sine and Cosine. This can be specified by defining the input parameter `LodeType` which can be `SineLode` and `CosineLode`
!
!@note
! This subroutine is called by [[theta_obj]].
!@endnote
!

INTERFACE
MODULE PURE FUNCTION theta_obj_j2j3( LodeType, J2, J3 ) RESULT( Ans )
  INTEGER( I4B ), INTENT( IN ) :: LodeType
  REAL( DFP ), INTENT( IN ) :: J2, J3
  REAL( DFP ) :: Ans
END FUNCTION theta_obj_j2j3
END INTERFACE

!----------------------------------------------------------------------------
!                                                       LodeAngle@Operation
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 March 2021
! summary: Returns the Lode angle
!
!### Introduction
!
! This function returns the lode angle.
! Lode angle can be described using two ways. Sin and Cosine. This can be selected by using the input parameter `LodeType` which can be `SineLode` and `CosineLode`
!
!$$ cos3\theta =\frac{3\sqrt{3} J_{3}}{2J_{2}\sqrt{J_{2}}} $$
!$$ sin3\theta =-\frac{3\sqrt{3} J_{3}}{2J_{2}\sqrt{J_{2}}} $$
!
!### Usage
!
!```fortran
! type( Rank2Tensor_ ) :: obj
! real( dfp ) :: mat( 3, 3 )
! call random_number( mat )
! obj = sym(mat)
! call display( LodeAngle(obj, LodeType=CosineLode, isDeviatoric=.FALSE.), "test10: trace(obj)=" )
!```

INTERFACE
MODULE PURE FUNCTION theta_obj( Obj, LodeType, isDeviatoric ) RESULT( Ans )
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: LodeType
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: isDeviatoric
  REAL( DFP ) :: Ans
END FUNCTION theta_obj
END INTERFACE

INTERFACE LodeAngle
  MODULE PROCEDURE theta_obj, theta_obj_j2j3
END INTERFACE LodeAngle

PUBLIC :: LodeAngle

!----------------------------------------------------------------------------
!                                                             Sym@Operation
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 March 2021
! summary: Returns the symmetric part of a rank2 tensor
!
!### Introduction
! Returns the symmetric part of the tensor
!
!
!### Usage
!
!```fortran
! type( Rank2Tensor_ ) :: obj
! real( dfp ) :: mat( 3, 3 )
! call random_number( mat )
! obj = mat
! obj = sym(obj)
!```

INTERFACE
MODULE PURE FUNCTION sym_r2t( Obj ) RESULT( Ans )
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: Obj
  TYPE( Rank2Tensor_ ) :: Ans
END FUNCTION sym_r2t
END INTERFACE

INTERFACE Sym
  MODULE PROCEDURE sym_r2t
END INTERFACE Sym

PUBLIC :: Sym

!----------------------------------------------------------------------------
!                                                         SkewSym@Operation
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 March 2021
! summary: Returns the skew symmetric part of the tensor
!
!### Introduction
! Returns the skew symmetric part of the tensor.
!
!### Usage
!
!```fortran
! type( Rank2Tensor_ ) :: obj
! real( dfp ) :: mat( 3, 3 )
! call random_number( mat )
! obj = mat
! obj = SkewSym(obj)
!```

INTERFACE
MODULE PURE FUNCTION Skewsym_r2t( Obj ) RESULT( Ans )
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: Obj
  TYPE( Rank2Tensor_ ) :: Ans
END FUNCTION Skewsym_r2t
END INTERFACE

INTERFACE SkewSym
  MODULE PROCEDURE Skewsym_r2t
END INTERFACE SkewSym

PUBLIC :: SkewSym

!----------------------------------------------------------------------------
!                                                    IsotropicPart@Operation
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 March 2021
! summary: Returns the isotropic part of the tensor
!
!### Introduction
! This function returns the isotropic part of a tensor, which is given by
! $$Isotropic(Obj) = \frac{1}{3} Trace(Obj)$$
!
!### Usage
!
!```fortran
! type( Rank2Tensor_ ) :: obj
! real( dfp ) :: mat( 3, 3 )
! call random_number( mat )
! obj = mat
! obj = Isotropic(obj)
!```

INTERFACE
MODULE PURE FUNCTION iso_part_obj( Obj ) RESULT( Ans )
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: Obj
  TYPE( Rank2Tensor_ ) :: Ans
END FUNCTION iso_part_obj
END INTERFACE

INTERFACE Isotropic
  MODULE PROCEDURE iso_part_obj
END INTERFACE Isotropic

PUBLIC :: Isotropic

INTERFACE Iso
  MODULE PROCEDURE iso_part_obj
END INTERFACE Iso

PUBLIC :: Iso

!----------------------------------------------------------------------------
!                                                    DeviatoricPart@Operation
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 March 2021
! summary: Returns the Deviatoric part of the tensor
!
!### Introduction
!This function returns the Deviatoric part of the tensor, which is given by
!
! $$Dev(T) = T - Iso(T)$$

INTERFACE
MODULE PURE FUNCTION dev_part_obj( Obj ) RESULT( Ans )
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: Obj
  TYPE( Rank2Tensor_ ) :: Ans
END FUNCTION dev_part_obj
END INTERFACE

INTERFACE Deviatoric
  MODULE PROCEDURE dev_part_obj
END INTERFACE Deviatoric

PUBLIC :: Deviatoric

INTERFACE Dev
  MODULE PROCEDURE dev_part_obj
END INTERFACE Dev

PUBLIC :: Dev

!----------------------------------------------------------------------------
!                                                      Contraction@Operation
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 March 2021
! summary: Returns the contraction of tensor

INTERFACE
MODULE PURE FUNCTION r2_contract_r2( Obj1, Obj2 ) RESULT( Ans )
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: Obj1, Obj2
  REAL( DFP ) :: Ans
END FUNCTION r2_contract_r2
END INTERFACE

!----------------------------------------------------------------------------
!                                                      Contraction@Operation
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 March 2021
! summary: Returns the contraction of a rank2 tensor and voigt rank2 tensor

INTERFACE
MODULE PURE FUNCTION r2_contract_voigt_r2( Obj1, Obj2 ) RESULT( Ans )
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: Obj1
  CLASS( VoigtRank2Tensor_ ), INTENT( IN ) :: Obj2
  REAL( DFP ) :: Ans
END FUNCTION r2_contract_voigt_r2
END INTERFACE

!----------------------------------------------------------------------------
!                                                      Contraction@Operation
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 March 2021
! summary: Returns contraction of aa voigt rank2 tensor and rank2 tensor

INTERFACE
MODULE PURE FUNCTION voigt_r2_contract_r2( Obj1, Obj2 ) RESULT( Ans )
  CLASS( VoigtRank2Tensor_ ), INTENT( IN ) :: Obj1
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: Obj2
  REAL( DFP ) :: Ans
END FUNCTION voigt_r2_contract_r2
END INTERFACE

!----------------------------------------------------------------------------
!                                                      Contraction@Operation
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 March 2021
! summary: Returns contraction of two voigt rank tensor

INTERFACE
MODULE PURE FUNCTION voigt_r2_contract_voigt_r2( Obj1, Obj2 ) RESULT( Ans )
  CLASS( VoigtRank2Tensor_ ), INTENT( IN ) :: Obj1
  CLASS( VoigtRank2Tensor_ ), INTENT( IN ) :: Obj2
  REAL( DFP ) :: Ans
END FUNCTION voigt_r2_contract_voigt_r2
END INTERFACE

!----------------------------------------------------------------------------
!                                                      Contraction@Operation
!----------------------------------------------------------------------------

INTERFACE Contraction
  MODULE PROCEDURE &
    & r2_contract_r2, &
    & r2_contract_voigt_r2, &
    & voigt_r2_contract_r2, &
    & voigt_r2_contract_voigt_r2
END INTERFACE Contraction

PUBLIC :: Contraction

!----------------------------------------------------------------------------
!                                                      Invariants@Operation
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 March 2021
! summary: Returns invariant of [[Rank2Tensor_]]
!
!### Introduction
!
! This function returns the invariant of [[Rank2Tensor_]].
! If the tensor is not a Deviatoric tensor then following invariants are returned:
!
! $$I_1 = Tr(T)$$
! $$I_2 = \frac{1}{2}(Tr^2(T) - Tr(A^2))$$
! $$I_3 = det(T)$$
!
! If the tensor is a Deviatoric tensor then following invariants are returned
! $$I_1 = 0.0$$
! $$I_2 = \frac{1}{2} Tr(A^2)$$
! $$I_3 = det(T)$$
!
!### Usage
!
!```fortran
!
!```

INTERFACE
MODULE PURE FUNCTION invariants_rank2( Obj, isDeviatoric ) RESULT( Ans )
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: Obj
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: isDeviatoric
  REAL( DFP ) :: Ans( 3 )
END FUNCTION invariants_rank2
END INTERFACE

INTERFACE Invariants
  MODULE PROCEDURE invariants_rank2
END INTERFACE Invariants

PUBLIC :: Invariants

!----------------------------------------------------------------------------
!                                                          Eigen@Operation
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 March 2021
! summary: Returns the eigen vector and eigen value of the tensor.

INTERFACE
MODULE SUBROUTINE eigen_r2t( Obj, QR, WR, QI, WI )
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: Obj
  REAL( DFP ), INTENT( INOUT ) :: QR( 3, 3 ), WR( 3 )
  REAL( DFP ), OPTIONAL, INTENT( INOUT ) :: QI( 3, 3 ), WI( 3 )
END SUBROUTINE eigen_r2t
END INTERFACE

INTERFACE Eigen
  MODULE PROCEDURE eigen_r2t
END INTERFACE Eigen

PUBLIC :: Eigen

!----------------------------------------------------------------------------
!                                                     PolarDecomp@Operation
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE right_pd_r2t( Obj, R, U )
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: Obj
  CLASS( Rank2Tensor_ ), INTENT( INOUT ) :: R, U
END SUBROUTINE right_pd_r2t
END INTERFACE

INTERFACE
MODULE SUBROUTINE left_pd_r2t( Obj, V, R )
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: Obj
  CLASS( Rank2Tensor_ ), INTENT( INOUT ) :: R, V
END SUBROUTINE left_pd_r2t
END INTERFACE

INTERFACE RightPolarDecomp
  MODULE PROCEDURE right_pd_r2t
END INTERFACE RightPolarDecomp

INTERFACE LeftPolarDecomp
  MODULE PROCEDURE right_pd_r2t
END INTERFACE LeftPolarDecomp

PUBLIC :: RightPolarDecomp, LeftPolarDecomp

!----------------------------------------------------------------------------
!                                                        Transpose@Operation
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION obj_transpose( Obj ) RESULT( Ans )
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: Obj
  TYPE( Rank2Tensor_ ) :: Ans
END FUNCTION obj_transpose
END INTERFACE

INTERFACE TRANSPOSE
  MODULE PROCEDURE obj_transpose
END INTERFACE TRANSPOSE

PUBLIC :: TRANSPOSE

!----------------------------------------------------------------------------
!                                                                +@Operation
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION obj_add_obj( Obj1, Obj2 ) RESULT( Ans )
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: Obj1
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: Obj2
  TYPE( Rank2Tensor_ ) :: Ans
END FUNCTION obj_add_obj
END INTERFACE

INTERFACE
MODULE PURE FUNCTION obj_add_mat( Obj1, Obj2 ) RESULT( Ans )
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: Obj1
  REAL( DFP ), INTENT( IN ) :: Obj2( 3, 3 )
  TYPE( Rank2Tensor_ ) :: Ans
END FUNCTION obj_add_mat
END INTERFACE

INTERFACE
MODULE PURE FUNCTION mat_add_obj( Obj1, Obj2 ) RESULT( Ans )
  REAL( DFP ), INTENT( IN ) :: Obj1( 3, 3 )
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: Obj2
  TYPE( Rank2Tensor_ ) :: Ans
END FUNCTION mat_add_obj
END INTERFACE

INTERFACE
MODULE PURE FUNCTION obj_add_scalar( Obj1, Obj2 ) RESULT( Ans )
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: Obj1
  REAL( DFP ), INTENT( IN ) :: Obj2
  TYPE( Rank2Tensor_ ) :: Ans
END FUNCTION obj_add_scalar
END INTERFACE

INTERFACE
MODULE PURE FUNCTION scalar_add_obj( Obj1, Obj2 ) RESULT( Ans )
  REAL( DFP ), INTENT( IN ) :: Obj1
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: Obj2
  TYPE( Rank2Tensor_ ) :: Ans
END FUNCTION scalar_add_obj
END INTERFACE

INTERFACE OPERATOR( + )
  MODULE PROCEDURE obj_add_obj, obj_add_mat, mat_add_obj, obj_add_scalar, &
    & scalar_add_obj
END INTERFACE

PUBLIC :: OPERATOR( + )

!----------------------------------------------------------------------------
!                                                                -@Operation
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION obj_minus_obj( Obj1, Obj2 ) RESULT( Ans )
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: Obj1
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: Obj2
  TYPE( Rank2Tensor_ ) :: Ans
END FUNCTION obj_minus_obj
END INTERFACE

INTERFACE
MODULE PURE FUNCTION obj_minus_mat( Obj1, Obj2 ) RESULT( Ans )
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: Obj1
  REAL( DFP ), INTENT( IN ) :: Obj2( 3, 3 )
  TYPE( Rank2Tensor_ ) :: Ans
END FUNCTION obj_minus_mat
END INTERFACE

INTERFACE
MODULE PURE FUNCTION mat_minus_obj( Obj1, Obj2 ) RESULT( Ans )
  REAL( DFP ), INTENT( IN ) :: Obj1( 3, 3 )
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: Obj2
  TYPE( Rank2Tensor_ ) :: Ans
END FUNCTION mat_minus_obj
END INTERFACE

INTERFACE
MODULE PURE FUNCTION obj_minus_scalar( Obj1, Obj2 ) RESULT( Ans )
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: Obj1
  REAL( DFP ), INTENT( IN ) :: Obj2
  TYPE( Rank2Tensor_ ) :: Ans
END FUNCTION obj_minus_scalar
END INTERFACE

INTERFACE
MODULE PURE FUNCTION scalar_minus_obj( Obj1, Obj2 ) RESULT( Ans )
  REAL( DFP ), INTENT( IN ) :: Obj1
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: Obj2
  TYPE( Rank2Tensor_ ) :: Ans
END FUNCTION scalar_minus_obj
END INTERFACE

INTERFACE OPERATOR( - )
  MODULE PROCEDURE obj_minus_obj, obj_minus_mat, mat_minus_obj, &
    & obj_minus_scalar, scalar_minus_obj
END INTERFACE

PUBLIC :: OPERATOR( - )

!----------------------------------------------------------------------------
!                                                             Exp@Operation
!----------------------------------------------------------------------------

! INTERFACE

! MODULE FUNCTION exp_r2t( Obj ) RESULT( Ans )
! 	CLASS( Rank2Tensor_ ), INTENT( IN ) ::Obj
! 	TYPE( Rank2Tensor_ ) :: Ans
! END FUNCTION exp_r2t

! END INTERFACE

! INTERFACE EXP
! 	MODULE PROCEDURE exp_r2t
! END INTERFACE

! PUBLIC :: EXP

!----------------------------------------------------------------------------
!                                                                    Contains
!----------------------------------------------------------------------------

END MODULE Rank2Tensor_Method
