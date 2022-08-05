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

!> author: Vikas Sharma, Ph. D.
! date: 	10 March 2021
! summary: 	This module contains methods for [[VoigtRank2Tensor_]]

MODULE VoigtRank2Tensor_Method
USE GlobalData
USE BaseType
IMPLICIT NONE
PRIVATE

!----------------------------------------------------------------------------
!                                                       Initiate@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 13 March 2021
! summary: Initiate [[VoigtRank2Tensor_]] using Vector
!
!# Introduction
!
! Initiate [[VoigtRank2Tensor_]] from a given vector.
!
!
!### Usage
!
!```fortran
! type( VoigtRank2Tensor_ ) :: obj
! real( dfp ) :: vec( 6 )
! call random_number( vec )
! call display( vec, "vec:", orient="row" )
! obj = VoigtRank2Tensor(vec, VoigtType=StressTypeVoigt)
! call display( obj, "stress type voigt")
! obj = VoigtRank2Tensor(vec, VoigtType=StrainTypeVoigt)
! call display( obj, "stress type voigt")
!```

INTERFACE
MODULE PURE SUBROUTINE init_from_vec( obj, Vec, VoigtType )
  CLASS( VoigtRank2Tensor_ ), INTENT( INOUT ) :: obj
  REAL( DFP ), INTENT( IN ) :: Vec( 6 )
  INTEGER( I4B ), INTENT( IN ) :: VoigtType
END SUBROUTINE init_from_vec
END INTERFACE

!----------------------------------------------------------------------------
!                                                       Initiate@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 13 March 2021
! summary: Initiate [[VoigtRank2Tensor_]] from a rank2 matrix
!
!
!### Usage
!
!```fortran
! type( VoigtRank2Tensor_ ) :: obj
! real( dfp ) :: vec( 6 )
! call random_number( vec )
! call display( vec, "vec:", orient="row" )
! obj = VoigtRank2Tensor(vec, VoigtType=StressTypeVoigt)
! call display( obj, "stress type voigt")
! obj = VoigtRank2Tensor(vec, VoigtType=StrainTypeVoigt)
! call display( obj, "stress type voigt")
!```

INTERFACE
MODULE PURE SUBROUTINE init_from_mat( obj, T, VoigtType )
  CLASS( VoigtRank2Tensor_ ), INTENT( INOUT ) :: obj
  REAL( DFP ), INTENT( IN ) :: T( 3, 3 )
  INTEGER( I4B ), INTENT( IN ) :: VoigtType
END SUBROUTINE init_from_mat
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE init_from_vec, init_from_mat
END INTERFACE Initiate

PUBLIC :: Initiate

!----------------------------------------------------------------------------
!                                               VoigtRank2Tensor@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 13 March 2021
! summary: This function returns an instance of [[VoigtRank2Tensor_]].
!
!### Usage
!
!```fortran
! type( VoigtRank2Tensor_ ) :: obj
! real( dfp ) :: vec( 6 )
! call random_number( vec )
! call display( vec, "vec:", orient="row" )
! obj = VoigtRank2Tensor(vec, VoigtType=StressTypeVoigt)
! call display( obj, "stress type voigt")
! obj = VoigtRank2Tensor(vec, VoigtType=StrainTypeVoigt)
! call display( obj, "stress type voigt")
!```
INTERFACE
MODULE PURE FUNCTION constructor1( Vec, VoigtType ) RESULT( Ans )
  REAL( DFP ), INTENT( IN ) :: Vec( : )
  INTEGER( I4B ), INTENT( IN ) :: VoigtType
  TYPE( VoigtRank2Tensor_ ) :: Ans
END FUNCTION constructor1
END INTERFACE

!----------------------------------------------------------------------------
!                                               VoigtRank2Tensor@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 13 March 2021
! summary: This function returns an instance of [[VoigtRank2Tensor_]].
!
!### Usage
!
!```fortran
! type( VoigtRank2Tensor_ ) :: obj
! real( dfp ) :: vec( 6 )
! call random_number( vec )
! call display( vec, "vec:", orient="row" )
! obj = VoigtRank2Tensor(vec, VoigtType=StressTypeVoigt)
! call display( obj, "stress type voigt")
! obj = VoigtRank2Tensor(vec, VoigtType=StrainTypeVoigt)
! call display( obj, "stress type voigt")
!```

INTERFACE
MODULE PURE FUNCTION constructor2( T, VoigtType ) RESULT( Ans )
  REAL( DFP ), INTENT( IN ) :: T( 3, 3 )
  INTEGER( I4B ), INTENT( IN ) :: VoigtType
  TYPE( VoigtRank2Tensor_ ) :: Ans
END FUNCTION constructor2
END INTERFACE

INTERFACE VoigtRank2Tensor
  MODULE PROCEDURE constructor1, constructor2
END INTERFACE VoigtRank2Tensor

PUBLIC :: VoigtRank2Tensor

!----------------------------------------------------------------------------
!                                       VoigtRank2Tensor_Pointer@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 13 March 2021
! summary: This function returns an instance of [[VoigtRank2Tensor_]].
!
!### Usage
!
!```fortran
! type( VoigtRank2Tensor_ ) :: obj
! real( dfp ) :: vec( 6 )
! call random_number( vec )
! call display( vec, "vec:", orient="row" )
! obj = VoigtRank2Tensor(vec, VoigtType=StressTypeVoigt)
! call display( obj, "stress type voigt")
! obj = VoigtRank2Tensor(vec, VoigtType=StrainTypeVoigt)
! call display( obj, "stress type voigt")
!```

INTERFACE
MODULE PURE FUNCTION constructor_1( Vec, VoigtType ) RESULT( Ans )
  REAL( DFP ), INTENT( IN ) :: Vec( : )
  INTEGER( I4B ), INTENT( IN ) :: VoigtType
  CLASS( VoigtRank2Tensor_ ), POINTER :: Ans
END FUNCTION constructor_1
END INTERFACE

!----------------------------------------------------------------------------
!                                       VoigtRank2Tensor_Pointer@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 13 March 2021
! summary: This function returns a pointer to an instance of [[VoigtRank2Tensor_]]
!
!### Usage
!
!```fortran
! type( VoigtRank2Tensor_ ) :: obj
! real( dfp ) :: vec( 6 )
! call random_number( vec )
! call display( vec, "vec:", orient="row" )
! obj = VoigtRank2Tensor(vec, VoigtType=StressTypeVoigt)
! call display( obj, "stress type voigt")
! obj = VoigtRank2Tensor(vec, VoigtType=StrainTypeVoigt)
! call display( obj, "stress type voigt")
!```

INTERFACE
MODULE PURE FUNCTION constructor_2( T, VoigtType ) RESULT( Ans )
  REAL( DFP ), INTENT( IN ) :: T( 3, 3 )
  INTEGER( I4B ), INTENT( IN ) :: VoigtType
  CLASS( VoigtRank2Tensor_ ), POINTER :: Ans
END FUNCTION constructor_2
END INTERFACE

INTERFACE VoigtRank2Tensor_Pointer
  MODULE PROCEDURE constructor_1, constructor_2
END INTERFACE VoigtRank2Tensor_Pointer

PUBLIC :: VoigtRank2Tensor_Pointer

!----------------------------------------------------------------------------
!                                                      Assignment@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 13 March 2021
! summary: This subroutine converts [[VoigtRank2Tensor_]] into Mat(3,3)
!
!### Usage
!
!```fortran
! type( VoigtRank2Tensor_ ) :: obj
! real( dfp ) :: T( 3, 3 ), W( 3, 3 )
! call random_number( T )
! T = 0.5*(T + transpose(T))
! call display( T, "T = " )
! obj = VoigtRank2Tensor(T, VoigtType=StressTypeVoigt)
! call display( obj, "stress type voigt")
! W = obj
! call display( W, "W=obj (stress type): ")
! obj = VoigtRank2Tensor(T, VoigtType=StrainTypeVoigt)
! call display( obj, "strain type voigt")
! W = obj
! call display( W, "W=obj (strain type): ")
!```

INTERFACE
MODULE PURE SUBROUTINE mat_eq_obj( T, obj )
  REAL( DFP ), INTENT( INOUT ) :: T( 3, 3 )
  CLASS( VoigtRank2Tensor_ ), INTENT( IN ) :: obj
END SUBROUTINE mat_eq_obj
END INTERFACE

!----------------------------------------------------------------------------
!                                                      Assignment@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE vec_eq_obj( vec, obj )
  REAL( DFP ), INTENT( INOUT ) :: vec( 6 )
  CLASS( VoigtRank2Tensor_ ), INTENT( IN ) :: obj
END SUBROUTINE vec_eq_obj
END INTERFACE

INTERFACE ASSIGNMENT( = )
  MODULE PROCEDURE mat_eq_obj, vec_eq_obj
END INTERFACE ASSIGNMENT( = )

PUBLIC :: ASSIGNMENT( = )

!----------------------------------------------------------------------------
!                                                                Display@IO
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 13 March 2021
! summary: This routine displays the content of [[VoigtRank2Tensor_]]

INTERFACE
MODULE SUBROUTINE display_obj( obj, Msg, UnitNo )
  CLASS( VoigtRank2Tensor_ ), INTENT( IN ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: Msg
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: UnitNo
END SUBROUTINE display_obj
END INTERFACE

INTERFACE Display
  MODULE PROCEDURE display_obj
END INTERFACE Display

PUBLIC :: Display

END MODULE VoigtRank2Tensor_Method
