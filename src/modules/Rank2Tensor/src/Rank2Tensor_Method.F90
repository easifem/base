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
! summary: Initiates [[Rank2Tensor_]] from another [[Rank2Tensor_]]
!
!# Introduction
! 	Initiates [[Rank2Tensor_]] from another [[Rank2Tensor_]]
!
!@note
! 	This routine also used in assignment(=) operator
!@endnote

INTERFACE
MODULE PURE SUBROUTINE init_by_rank2( obj, obj2 )
  CLASS( Rank2Tensor_ ), INTENT( INOUT ) :: obj
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: obj2
END SUBROUTINE init_by_rank2
END INTERFACE

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
MODULE PURE SUBROUTINE init_by_mat( obj, Mat, isSym )
  CLASS( Rank2Tensor_ ), INTENT( INOUT ) :: obj
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
MODULE PURE SUBROUTINE init_by_voigt( obj, V )
  CLASS( Rank2Tensor_ ), INTENT( INOUT ) :: obj
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
MODULE PURE SUBROUTINE init_voigt_from_r2tensor( obj, T, VoigtType )
  CLASS( VoigtRank2Tensor_ ), INTENT( INOUT ) :: obj
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: T
  INTEGER( I4B ), INTENT( IN ) :: VoigtType
END SUBROUTINE init_voigt_from_r2tensor
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE init_by_rank2, init_by_mat, init_by_voigt, &
    & init_voigt_from_r2tensor
END INTERFACE Initiate

PUBLIC :: Initiate

INTERFACE ASSIGNMENT(=)
  MODULE PROCEDURE init_by_rank2
END INTERFACE ASSIGNMENT(=)

PUBLIC :: ASSIGNMENT(=)

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
! summary: returns the pointer to an newly created instance of [[Rank2Tensor_]].
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
! summary: returns a pointer to a newly created instance of [[Rank2Tensor_]]
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
MODULE PURE SUBROUTINE r2tensor_eq_mat( obj, Mat )
  CLASS( Rank2Tensor_ ), INTENT( INOUT ) :: obj
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
MODULE PURE SUBROUTINE mat_eq_r2tensor( Mat, obj )
  REAL( DFP ), INTENT( INOUT ) :: Mat( 3, 3 )
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: obj
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
MODULE PURE SUBROUTINE voigt_eq_r2tensor( V, obj )
  CLASS( VoigtRank2Tensor_ ), INTENT( INOUT ) :: V
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: obj
END SUBROUTINE voigt_eq_r2tensor
END INTERFACE

INTERFACE ASSIGNMENT( = )
  MODULE PROCEDURE r2tensor_eq_mat, mat_eq_r2tensor, init_by_voigt, &
    & voigt_eq_r2tensor
END INTERFACE

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
MODULE PURE SUBROUTINE identity_rank2( obj )
  CLASS( Rank2Tensor_ ), INTENT( INOUT ) :: obj
END SUBROUTINE identity_rank2
END INTERFACE

INTERFACE IdentityTensor
  MODULE PROCEDURE identity_rank2
END INTERFACE IdentityTensor

PUBLIC :: IdentityTensor

!----------------------------------------------------------------------------
!                                                 getOnesTensor@constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 March 2021
! summary: Returns a second order tensor with all entry one

INTERFACE
MODULE PURE SUBROUTINE rank2_getOnes( obj )
  CLASS( Rank2Tensor_ ), INTENT( INOUT ) :: obj
END SUBROUTINE rank2_getOnes
END INTERFACE

INTERFACE getOnes
  MODULE PROCEDURE rank2_getOnes
END INTERFACE getOnes

PUBLIC :: getOnes

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
MODULE PURE SUBROUTINE rank2_getZeros( obj )
  CLASS( Rank2Tensor_ ), INTENT( INOUT ) :: obj
END SUBROUTINE rank2_getZeros
END INTERFACE

INTERFACE getZeros
  MODULE PROCEDURE rank2_getZeros
END INTERFACE getZeros

PUBLIC :: getZeros

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
MODULE PURE SUBROUTINE isotropic_rank2( obj, Lambda )
  CLASS( Rank2Tensor_ ), INTENT( INOUT ) :: obj
  REAL( DFP ), INTENT( IN ) :: Lambda
END SUBROUTINE isotropic_rank2
END INTERFACE

INTERFACE IsotropicTensor
  MODULE PROCEDURE isotropic_rank2
END INTERFACE IsotropicTensor

PUBLIC :: IsotropicTensor

!----------------------------------------------------------------------------
!                                                          isSym@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	17 March 2021
! summary: 	Returns true if symmetric

INTERFACE
MODULE PURE FUNCTION isSym_rank2( obj ) RESULT( Ans )
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: obj
  LOGICAL( LGT ) :: Ans
END FUNCTION isSym_rank2
END INTERFACE

INTERFACE isSym
  MODULE PROCEDURE isSym_rank2
END INTERFACE isSym

PUBLIC :: isSym

!----------------------------------------------------------------------------
!                                                   isDeviatoric@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	17 March 2021
! summary: 	Returns true of deviatoric tensor

INTERFACE
MODULE PURE FUNCTION isDeviatoric_rank2( obj ) RESULT( Ans )
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: obj
  LOGICAL( LGT ) :: Ans
END FUNCTION isDeviatoric_rank2
END INTERFACE

INTERFACE isDeviatoric
  MODULE PROCEDURE isDeviatoric_rank2
END INTERFACE isDeviatoric

PUBLIC :: isDeviatoric

!----------------------------------------------------------------------------
!                                           DeformationGradient@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: 	Returns pointer to a newly created instance of [[DeformationGradient_]]

INTERFACE
MODULE PURE FUNCTION F_constructor1( obj ) RESULT( Ans )
  CLASS( Rank2Tensor_ ), OPTIONAL, INTENT( IN ) :: obj
  TYPE( DeformationGradient_ ) :: Ans
END FUNCTION F_constructor1
END INTERFACE

INTERFACE DeformationGradient
  MODULE PROCEDURE F_constructor1
END INTERFACE DeformationGradient

PUBLIC :: DeformationGradient

!----------------------------------------------------------------------------
!                                           DeformationGradient@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: 	Returns pointer to a newly created instance of [[DeformationGradient_]]

INTERFACE
MODULE PURE FUNCTION F_constructor_1( obj ) RESULT( Ans )
  CLASS( Rank2Tensor_ ), OPTIONAL, INTENT( IN ) :: obj
  CLASS( DeformationGradient_ ), POINTER :: Ans
END FUNCTION F_constructor_1
END INTERFACE

INTERFACE DeformationGradient_Pointer
  MODULE PROCEDURE F_constructor_1
END INTERFACE DeformationGradient_Pointer

PUBLIC :: DeformationGradient_Pointer

!----------------------------------------------------------------------------
!                                                           LeftCauchyGreen
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: 	Returns pointer to a newly created instance of [[LeftCauchyGreen_]]

INTERFACE
MODULE PURE FUNCTION b_constructor1( F, V ) RESULT( Ans )
  CLASS( Rank2Tensor_ ), OPTIONAL, INTENT( IN ) :: F
  CLASS( Rank2Tensor_ ), OPTIONAL, INTENT( IN ) :: V
  TYPE( LeftCauchyGreen_ ) :: Ans
END FUNCTION b_constructor1
END INTERFACE

INTERFACE LeftCauchyGreen
  MODULE PROCEDURE b_constructor1
END INTERFACE LeftCauchyGreen

PUBLIC :: LeftCauchyGreen

!----------------------------------------------------------------------------
!                                                           LeftCauchyGreen
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: 	Returns pointer to a newly created instance of [[LeftCauchyGreen_]]

INTERFACE
MODULE PURE FUNCTION b_constructor_1( F, V ) RESULT( Ans )
  CLASS( Rank2Tensor_ ), OPTIONAL, INTENT( IN ) :: F
  CLASS( Rank2Tensor_ ), OPTIONAL, INTENT( IN ) :: V
  CLASS( LeftCauchyGreen_ ), POINTER :: Ans
END FUNCTION b_constructor_1
END INTERFACE

INTERFACE LeftCauchyGreen_Pointer
  MODULE PROCEDURE b_constructor_1
END INTERFACE LeftCauchyGreen_Pointer

PUBLIC :: LeftCauchyGreen_Pointer

!----------------------------------------------------------------------------
!                                                           RightCauchyGreen
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: 	Returns pointer to a newly created instance of [[RightCauchyGreen_]]

INTERFACE
MODULE PURE FUNCTION C_constructor1( F, U ) RESULT( Ans )
  CLASS( Rank2Tensor_ ), OPTIONAL, INTENT( IN ) :: F
  CLASS( Rank2Tensor_ ), OPTIONAL, INTENT( IN ) :: U
  TYPE( RightCauchyGreen_ ) :: Ans
END FUNCTION C_constructor1
END INTERFACE

INTERFACE RightCauchyGreen
  MODULE PROCEDURE C_constructor1
END INTERFACE RightCauchyGreen

PUBLIC :: RightCauchyGreen

!----------------------------------------------------------------------------
!                                                           RightCauchyGreen
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: 	Returns pointer to a newly created instance of [[RightCauchyGreen_]]

INTERFACE
MODULE PURE FUNCTION C_constructor_1( F, U ) RESULT( Ans )
  CLASS( Rank2Tensor_ ), OPTIONAL, INTENT( IN ) :: F
  CLASS( Rank2Tensor_ ), OPTIONAL, INTENT( IN ) :: U
  CLASS( RightCauchyGreen_ ), POINTER :: Ans
END FUNCTION C_constructor_1
END INTERFACE

INTERFACE RightCauchyGreen_Pointer
  MODULE PROCEDURE C_constructor_1
END INTERFACE RightCauchyGreen_Pointer

PUBLIC :: RightCauchyGreen_Pointer

!----------------------------------------------------------------------------
!                                                              INV@Operation
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE inv_rank2( obj, Invobj )
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: obj
  CLASS( Rank2Tensor_ ), INTENT( INOUT ) :: Invobj
END SUBROUTINE inv_rank2
END INTERFACE

INTERFACE INV
  MODULE PROCEDURE inv_rank2
END INTERFACE INV

PUBLIC :: INV

!----------------------------------------------------------------------------
!                                                        Transpose@Operation
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	17 March 2021
! summary: 	Transpose of a tensor

INTERFACE
MODULE PURE FUNCTION obj_transpose( obj ) RESULT( Ans )
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: obj
  TYPE( Rank2Tensor_ ) :: Ans
END FUNCTION obj_transpose
END INTERFACE

INTERFACE TRANSPOSE
  MODULE PROCEDURE obj_transpose
END INTERFACE TRANSPOSE

PUBLIC :: TRANSPOSE

!----------------------------------------------------------------------------
!                                                           Sym@InvarMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 March 2021
! summary: Returns the symmetric part of a rank2 tensor
!
!# Introduction
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
MODULE PURE FUNCTION sym_r2t( obj ) RESULT( Ans )
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: obj
  TYPE( Rank2Tensor_ ) :: Ans
END FUNCTION sym_r2t
END INTERFACE

INTERFACE Sym
  MODULE PROCEDURE sym_r2t
END INTERFACE Sym

PUBLIC :: Sym

!----------------------------------------------------------------------------
!                                                       SkewSym@InvarMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 March 2021
! summary: Returns the skew symmetric part of the tensor
!
!# Introduction
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
MODULE PURE FUNCTION Skewsym_r2t( obj ) RESULT( Ans )
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: obj
  TYPE( Rank2Tensor_ ) :: Ans
END FUNCTION Skewsym_r2t
END INTERFACE

INTERFACE SkewSym
  MODULE PROCEDURE Skewsym_r2t
END INTERFACE SkewSym

PUBLIC :: SkewSym

!----------------------------------------------------------------------------
!                                                               Display@IO
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 March 2021
! summary: Display the content of [[Rank2Tensor_]]

INTERFACE
MODULE SUBROUTINE display_obj( obj, Msg, UnitNo )
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: Msg
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: UnitNo
END SUBROUTINE display_obj
END INTERFACE

INTERFACE Display
  MODULE PROCEDURE display_obj
END INTERFACE Display

PUBLIC :: Display

!----------------------------------------------------------------------------
!                                                         Trace@InvarMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 March 2021
! summary: Returns trace of a tensor
!
!# Introduction
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
MODULE PURE FUNCTION trace_obj( obj, Power ) RESULT( Ans )
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: Power
  REAL( DFP ) :: Ans
END FUNCTION trace_obj
END INTERFACE

INTERFACE Trace
  MODULE PROCEDURE trace_obj
END INTERFACE Trace

PUBLIC :: Trace

!----------------------------------------------------------------------------
!                                                            J2@InvarMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 March 2021
! summary: Returns J2 invariant of tensor
!
!# Introduction
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
MODULE PURE FUNCTION j2_obj( obj, isDeviatoric ) RESULT( Ans )
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: obj
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: isDeviatoric
  REAL( DFP ) :: Ans
END FUNCTION j2_obj
END INTERFACE

INTERFACE J2
  MODULE PROCEDURE j2_obj
END INTERFACE J2

PUBLIC :: J2

!----------------------------------------------------------------------------
!                                                           J3@InvarMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 March 2021
! summary: Returns J3 invarinat of a tensor
!
!# Introduction
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
MODULE PURE FUNCTION j3_obj( obj, isDeviatoric ) RESULT( Ans )
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: obj
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: isDeviatoric
  REAL( DFP ) :: Ans
END FUNCTION j3_obj
END INTERFACE

INTERFACE J3
  MODULE PROCEDURE j3_obj
END INTERFACE J3

PUBLIC :: J3

!----------------------------------------------------------------------------
!                                                           Det@InvarMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 March 2021
! summary: Returns the Determinant of a tensor
!
!# Introduction
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
MODULE PURE FUNCTION det_obj( obj ) RESULT( Ans )
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: obj
  REAL( DFP ) :: Ans
END FUNCTION det_obj
END INTERFACE

INTERFACE Det
  MODULE PROCEDURE Det_obj
END INTERFACE Det

PUBLIC :: Det

!----------------------------------------------------------------------------
!                                                     LodeAngle@InvarMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 March 2021
! summary: Returns lode angle
!
!# Introduction
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
!                                                     LodeAngle@InvarMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 March 2021
! summary: Returns the Lode angle
!
!# Introduction
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
MODULE PURE FUNCTION theta_obj( obj, LodeType, isDeviatoric ) RESULT( Ans )
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: obj
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
!                                                IsotropicPart@InvarMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 March 2021
! summary: Returns the isotropic part of the tensor
!
!# Introduction
! This function returns the isotropic part of a tensor, which is given by
! $$Isotropic(obj) = \frac{1}{3} Trace(obj)$$
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
MODULE PURE FUNCTION iso_part_obj( obj ) RESULT( Ans )
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: obj
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
!                                                DeviatoricPart@InvarMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 March 2021
! summary: Returns the Deviatoric part of the tensor
!
!# Introduction
!This function returns the Deviatoric part of the tensor, which is given by
!
! $$Dev(T) = T - Iso(T)$$

INTERFACE
MODULE PURE FUNCTION dev_part_obj( obj ) RESULT( Ans )
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: obj
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
!                                                    Invariants@InvarMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 March 2021
! summary: Returns invariant of [[Rank2Tensor_]]
!
!# Introduction
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
MODULE PURE FUNCTION invariants_rank2( obj, isDeviatoric ) RESULT( Ans )
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: obj
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: isDeviatoric
  REAL( DFP ) :: Ans( 3 )
END FUNCTION invariants_rank2
END INTERFACE

INTERFACE Invariants
  MODULE PROCEDURE invariants_rank2
END INTERFACE Invariants

PUBLIC :: Invariants

!----------------------------------------------------------------------------
!                                                        Eigen@InvarMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 March 2021
! summary: Returns the eigen vector and eigen value of the tensor.
!
!# Introduction
! 	This subroutine returns the eigen values and eigen vectors of a tensor.
! If the tensor is symmetric then the eigenvalues and eigenvectors are real
! and `QI` and `WI` are not required. However, if the tensor is not symmetric
! then `QI` and `WI` contain the imaginary part of the eigenvalues and
! eigenvectors.
!
!### Usage
!
!```fortran
! type( Rank2Tensor_ ) :: obj
! real( dfp ) :: mat( 3, 3 ), QR( 3, 3 ), WR( 3 ), QI( 3, 3 ), WI( 3 )
! mat = 0.0
! mat(1,1) = 5.0
! mat(2:3, 2) = [-6, -12]
! mat(2:3, 3) = [-12, 1]
! call initiate( obj, mat, isSym=.true.)
! call Eigen( obj, QR, WR )
! call BlankLines(unitNo=stdout, NOL=2)
! call display( obj, 'test12: obj=')
! call display( Invariants(obj), "test12: Invariants=" )
! call display( QR, "test12: QR=")
! call display( WR, "test12: WR=")
!```

INTERFACE
MODULE SUBROUTINE eigen_r2t( obj, QR, WR, QI, WI )
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: obj
  REAL( DFP ), INTENT( INOUT ) :: QR( 3, 3 ), WR( 3 )
  REAL( DFP ), OPTIONAL, INTENT( INOUT ) :: QI( 3, 3 ), WI( 3 )
END SUBROUTINE eigen_r2t
END INTERFACE

INTERFACE Eigen
  MODULE PROCEDURE eigen_r2t
END INTERFACE Eigen

PUBLIC :: Eigen

!----------------------------------------------------------------------------
!                                                   PolarDecomp@InvarMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	17 March 2021
! summary: 	This subroutine provides polar decomposition of a tensor.
!
!# Introduction
! 	This subroutine provides right polar decomposition of a tensor, which is
! given by
!
! $$T=RU$$
!
!### Usage
!
!```fortran
! type( Rank2Tensor_ ) :: obj, R, U, V
! real( dfp ) :: mat( 3, 3 ) = reshape( [1.0, -0.333, 0.959, 0.495, 1.0, 0.0, 0.5, -0.247, 1.5], [3,3] )
! call initiate( obj, mat, isSym=.false. )
! call PolarDecomp( obj, R, U, V )
!```

INTERFACE
MODULE SUBROUTINE pd_r2t( obj, R, U, V )
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: obj
  CLASS( Rank2Tensor_ ), INTENT( INOUT ) :: R
  CLASS( Rank2Tensor_ ), INTENT( INOUT ) :: U
  CLASS( Rank2Tensor_ ), INTENT( INOUT ) :: V
END SUBROUTINE pd_r2t
END INTERFACE

INTERFACE PolarDecomp
  MODULE PROCEDURE pd_r2t
END INTERFACE PolarDecomp

PUBLIC :: PolarDecomp

!----------------------------------------------------------------------------
!                                                    Contraction@Contraction
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 March 2021
! summary: Returns the contraction of tensor

INTERFACE
MODULE PURE FUNCTION r2_contract_r2( obj1, obj2 ) RESULT( Ans )
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: obj1, obj2
  REAL( DFP ) :: Ans
END FUNCTION r2_contract_r2
END INTERFACE

!----------------------------------------------------------------------------
!                                                    Contraction@Contraction
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 March 2021
! summary: Returns the contraction of a rank2 tensor and voigt rank2 tensor

INTERFACE
MODULE PURE FUNCTION r2_contract_voigt_r2( obj1, obj2 ) RESULT( Ans )
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: obj1
  CLASS( VoigtRank2Tensor_ ), INTENT( IN ) :: obj2
  REAL( DFP ) :: Ans
END FUNCTION r2_contract_voigt_r2
END INTERFACE

!----------------------------------------------------------------------------
!                                                    Contraction@Contraction
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 March 2021
! summary: Returns contraction of aa voigt rank2 tensor and rank2 tensor

INTERFACE
MODULE PURE FUNCTION voigt_r2_contract_r2( obj1, obj2 ) RESULT( Ans )
  CLASS( VoigtRank2Tensor_ ), INTENT( IN ) :: obj1
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: obj2
  REAL( DFP ) :: Ans
END FUNCTION voigt_r2_contract_r2
END INTERFACE

!----------------------------------------------------------------------------
!                                                    Contraction@Contraction
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 March 2021
! summary: Returns contraction of two voigt rank tensor

INTERFACE
MODULE PURE FUNCTION voigt_r2_contract_voigt_r2( obj1, obj2 ) RESULT( Ans )
  CLASS( VoigtRank2Tensor_ ), INTENT( IN ) :: obj1
  CLASS( VoigtRank2Tensor_ ), INTENT( IN ) :: obj2
  REAL( DFP ) :: Ans
END FUNCTION voigt_r2_contract_voigt_r2
END INTERFACE

!----------------------------------------------------------------------------
!                                                    Contraction@Contraction
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
!                                                                +@Operator
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	17 March 2021
! summary: 	Addition of two tensor
!
!### Usage
!
!```fortran
  ! type( Rank2Tensor_ ) :: a, b
  ! real( dfp ) :: mat( 3, 3 )
  ! call random_number( mat )
  ! a = mat
  ! call random_number( mat )
  ! b = mat
  ! call display( a+b, "test14: a+b=")
  ! call display( a+1.0_DFP, "test14: a+1=")
  ! call display( 1.0_DFP + a, "test14: 1+a=")
!```

INTERFACE
MODULE PURE FUNCTION obj_add_obj( obj1, obj2 ) RESULT( Ans )
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: obj1
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: obj2
  TYPE( Rank2Tensor_ ) :: Ans
END FUNCTION obj_add_obj
END INTERFACE

!----------------------------------------------------------------------------
!                                                                +@Operator
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	17 March 2021
! summary: 	Addition of tensor and scalar
!
!### Usage
!
!```fortran
  ! type( Rank2Tensor_ ) :: a, b
  ! real( dfp ) :: mat( 3, 3 )
  ! call random_number( mat )
  ! a = mat
  ! call random_number( mat )
  ! b = mat
  ! call display( a+b, "test14: a+b=")
  ! call display( a+1.0_DFP, "test14: a+1=")
  ! call display( 1.0_DFP + a, "test14: 1+a=")
!```

INTERFACE
MODULE PURE FUNCTION obj_add_scalar( obj1, obj2 ) RESULT( Ans )
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: obj1
  REAL( DFP ), INTENT( IN ) :: obj2
  TYPE( Rank2Tensor_ ) :: Ans
END FUNCTION obj_add_scalar
END INTERFACE

!----------------------------------------------------------------------------
!                                                                +@Operator
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	17 March 2021
! summary: 	Addition of tensor and scalar
!
!### Usage
!
!```fortran
  ! type( Rank2Tensor_ ) :: a, b
  ! real( dfp ) :: mat( 3, 3 )
  ! call random_number( mat )
  ! a = mat
  ! call random_number( mat )
  ! b = mat
  ! call display( a+b, "test14: a+b=")
  ! call display( a+1.0_DFP, "test14: a+1=")
  ! call display( 1.0_DFP + a, "test14: 1+a=")
!```

INTERFACE
MODULE PURE FUNCTION scalar_add_obj( obj1, obj2 ) RESULT( Ans )
  REAL( DFP ), INTENT( IN ) :: obj1
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: obj2
  TYPE( Rank2Tensor_ ) :: Ans
END FUNCTION scalar_add_obj
END INTERFACE

INTERFACE OPERATOR( + )
  MODULE PROCEDURE obj_add_obj, obj_add_scalar, scalar_add_obj
END INTERFACE

PUBLIC :: OPERATOR( + )

!----------------------------------------------------------------------------
!                                                                -@Operator
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	17 March 2021
! summary: 	Subtraction of tensor and tensor
!
!### Usage
!
!```fortran
  ! type( Rank2Tensor_ ) :: a, b
  ! real( dfp ) :: mat( 3, 3 )
  ! call random_number( mat )
  ! a = mat
  ! call random_number( mat )
  ! b = mat
  ! call display( a-b, "test14: a+b=")
  ! call display( a-1.0_DFP, "test14: a+1=")
  ! call display( 1.0_DFP - a, "test14: 1+a=")
!```

INTERFACE
MODULE PURE FUNCTION obj_minus_obj( obj1, obj2 ) RESULT( Ans )
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: obj1
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: obj2
  TYPE( Rank2Tensor_ ) :: Ans
END FUNCTION obj_minus_obj
END INTERFACE

!----------------------------------------------------------------------------
!                                                                -@Operator
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	17 March 2021
! summary: 	Subtraction of tensor and scalar
!
!### Usage
!
!```fortran
  ! type( Rank2Tensor_ ) :: a, b
  ! real( dfp ) :: mat( 3, 3 )
  ! call random_number( mat )
  ! a = mat
  ! call random_number( mat )
  ! b = mat
  ! call display( a-b, "test14: a-b=")
  ! call display( a-1.0_DFP, "test14: a-1=")
  ! call display( 1.0_DFP - a, "test14: 1-a=")
!```

INTERFACE
MODULE PURE FUNCTION obj_minus_scalar( obj1, obj2 ) RESULT( Ans )
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: obj1
  REAL( DFP ), INTENT( IN ) :: obj2
  TYPE( Rank2Tensor_ ) :: Ans
END FUNCTION obj_minus_scalar
END INTERFACE

!----------------------------------------------------------------------------
!                                                                -@Operator
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	17 March 2021
! summary: 	Subtraction of tensor and scalar
!
!### Usage
!
!```fortran
  ! type( Rank2Tensor_ ) :: a, b
  ! real( dfp ) :: mat( 3, 3 )
  ! call random_number( mat )
  ! a = mat
  ! call random_number( mat )
  ! b = mat
  ! call display( a-b, "test14: a-b=")
  ! call display( a-1.0_DFP, "test14: a-1=")
  ! call display( 1.0_DFP - a, "test14: 1-a=")
!```

INTERFACE
MODULE PURE FUNCTION scalar_minus_obj( obj1, obj2 ) RESULT( Ans )
  REAL( DFP ), INTENT( IN ) :: obj1
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: obj2
  TYPE( Rank2Tensor_ ) :: Ans
END FUNCTION scalar_minus_obj
END INTERFACE

INTERFACE OPERATOR( - )
  MODULE PROCEDURE obj_minus_obj, &
    & obj_minus_scalar, scalar_minus_obj
END INTERFACE

PUBLIC :: OPERATOR( - )

!----------------------------------------------------------------------------
!                                                                 *@Operator
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	17 March 2021
! summary: 	rank2 tensor times rank 2 tensor
!
!### Usage
!
!```fortran
  ! type( Rank2Tensor_ ) :: a, b
  ! real( dfp ) :: mat( 3, 3 )
  ! call random_number( mat )
  ! a = mat
  ! call random_number( mat )
  ! b = mat
  ! call display( a*b, "test14: a*b=")
  ! call display( a*1.0_DFP, "test14: a*1=")
  ! call display( 1.0_DFP * a, "test14: 1*a=")
!```

INTERFACE
MODULE PURE FUNCTION obj_times_obj( obj1, obj2 ) RESULT( Ans )
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: obj1
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: obj2
  TYPE( Rank2Tensor_ ) :: Ans
END FUNCTION obj_times_obj
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 *@Operator
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	17 March 2021
! summary: 	rank2 tensor times scalar
!
!### Usage
!
!```fortran
  ! type( Rank2Tensor_ ) :: a, b
  ! real( dfp ) :: mat( 3, 3 )
  ! call random_number( mat )
  ! a = mat
  ! call random_number( mat )
  ! b = mat
  ! call display( a*b, "test14: a*b=")
  ! call display( a*1.0_DFP, "test14: a*1=")
  ! call display( 1.0_DFP * a, "test14: 1*a=")
!```

INTERFACE
MODULE PURE FUNCTION obj_times_scalar( obj1, obj2 ) RESULT( Ans )
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: obj1
  REAL( DFP ), INTENT( IN ) :: obj2
  TYPE( Rank2Tensor_ ) :: Ans
END FUNCTION obj_times_scalar
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 *@Operator
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	17 March 2021
! summary: 	rank2 tensor times scalar
!
!### Usage
!
!```fortran
  ! type( Rank2Tensor_ ) :: a, b
  ! real( dfp ) :: mat( 3, 3 )
  ! call random_number( mat )
  ! a = mat
  ! call random_number( mat )
  ! b = mat
  ! call display( a*b, "test14: a*b=")
  ! call display( a*1.0_DFP, "test14: a*1=")
  ! call display( 1.0_DFP * a, "test14: 1*a=")
!```

INTERFACE
MODULE PURE FUNCTION scalar_times_obj( obj1, obj2 ) RESULT( Ans )
  REAL( DFP ), INTENT( IN ) :: obj1
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: obj2
  TYPE( Rank2Tensor_ ) :: Ans
END FUNCTION scalar_times_obj
END INTERFACE

INTERFACE OPERATOR( * )
  MODULE PROCEDURE obj_times_obj, obj_times_scalar, scalar_times_obj
END INTERFACE OPERATOR( * )

PUBLIC :: OPERATOR( * )

!----------------------------------------------------------------------------
!                                                                /@Operator
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION obj_div_obj( obj1, obj2 ) RESULT( Ans )
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: obj1
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: obj2
  TYPE( Rank2Tensor_ ) :: Ans
END FUNCTION obj_div_obj
END INTERFACE

!----------------------------------------------------------------------------
!                                                                /@Operator
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION obj_div_scalar( obj1, obj2 ) RESULT( Ans )
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: obj1
  REAL( DFP ), INTENT( IN ) :: obj2
  TYPE( Rank2Tensor_ ) :: Ans
END FUNCTION obj_div_scalar
END INTERFACE

!----------------------------------------------------------------------------
!                                                                /@Operator
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION scalar_div_obj( obj1, obj2 ) RESULT( Ans )
  REAL( DFP ), INTENT( IN ) :: obj1
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: obj2
  TYPE( Rank2Tensor_ ) :: Ans
END FUNCTION scalar_div_obj
END INTERFACE

INTERFACE OPERATOR( / )
  MODULE PROCEDURE obj_div_obj, obj_div_scalar, scalar_div_obj
END INTERFACE OPERATOR( / )

PUBLIC :: OPERATOR( / )

!----------------------------------------------------------------------------
!                                                             MATMUL@Operator
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION obj_matmul_obj( obj1, obj2 ) RESULT( Ans )
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: obj1
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: obj2
  TYPE( Rank2Tensor_ ) :: Ans
END FUNCTION obj_matmul_obj
END INTERFACE

!----------------------------------------------------------------------------
!                                                            MATMUL@Operator
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION obj_matmul_vec( obj1, obj2 ) RESULT( Ans )
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: obj1
  REAL( DFP ), INTENT( IN ) :: obj2( 3 )
  REAL( DFP ) :: Ans( 3 )
END FUNCTION obj_matmul_vec
END INTERFACE

!----------------------------------------------------------------------------
!                                                            MATMUL@Operator
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION vec_matmul_obj( obj1, obj2 ) RESULT( Ans )
  REAL( DFP ), INTENT( IN ) :: obj1( 3 )
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: obj2
  REAL( DFP ) :: Ans( 3 )
END FUNCTION vec_matmul_obj
END INTERFACE

INTERFACE MATMUL
  MODULE PROCEDURE obj_matmul_obj, obj_matmul_vec, vec_matmul_obj
END INTERFACE MATMUL

PUBLIC :: MATMUL

!----------------------------------------------------------------------------
!                                                          Pullback@Pullback
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION pullback_rank2( T, F, indx1, indx2 ) RESULT( Ans )
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: T
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: F
  CHARACTER( LEN = * ),  INTENT( IN ) :: indx1, indx2
  TYPE( Rank2Tensor_ ) :: Ans
END FUNCTION pullback_rank2
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Pullback@Pullback
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION pullback_vec( Vec, F, indx1 ) RESULT( Ans )
  REAL( DFP ), INTENT( IN ) :: Vec( 3 )
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: F
  CHARACTER( LEN = * ), INTENT( IN ) :: indx1
  REAL( DFP ) :: Ans( 3 )
END FUNCTION pullback_vec
END INTERFACE

INTERFACE PULLBACK
  MODULE PROCEDURE pullback_rank2, pullback_vec
END INTERFACE PULLBACK

PUBLIC :: PULLBACK

!----------------------------------------------------------------------------
!                                                    PushForward@Pushforward
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION pushforward_rank2( T, F, indx1, indx2 ) RESULT( Ans )
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: T
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: F
  CHARACTER( LEN = * ), INTENT( IN ) :: indx1, indx2
  TYPE( Rank2Tensor_ ) :: Ans
END FUNCTION pushforward_rank2
END INTERFACE

!----------------------------------------------------------------------------
!                                                    Pushforward@Pushforward
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION pushforward_vec( Vec, F, indx1 ) RESULT( Ans )
  REAL( DFP ), INTENT( IN ) :: Vec( 3 )
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: F
  CHARACTER( LEN = * ), INTENT( IN ) :: indx1
  REAL( DFP ) :: Ans( 3 )
END FUNCTION pushforward_vec
END INTERFACE

INTERFACE PushForward
  MODULE PROCEDURE pushforward_rank2, pushforward_vec
END INTERFACE PushForward

PUBLIC :: PushForward

!----------------------------------------------------------------------------
!                                                                 D
!----------------------------------------------------------------------------
END MODULE Rank2Tensor_Method
