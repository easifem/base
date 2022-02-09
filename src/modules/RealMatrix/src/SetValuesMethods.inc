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

PUBLIC :: set
PUBLIC :: add

!----------------------------------------------------------------------------
!                                                        setValues@setValues
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	6 March 2021
! summary: Add values in [[realmatrix_]]
!
!# Introduction
!
! This subroutine set `obj%val` to `Val`
!
!### Usage
!
!```fortran
!
!```

INTERFACE
MODULE PURE SUBROUTINE realmat_set_1( obj, Val )
  TYPE( RealMatrix_ ), INTENT( INOUT ) :: obj
  REAL( DFP ), INTENT( IN ) :: Val( :, : )
END SUBROUTINE realmat_set_1
END INTERFACE

INTERFACE set
  MODULE PROCEDURE realmat_set_1
END INTERFACE set

!----------------------------------------------------------------------------
!                                                        setValues@setValues
!----------------------------------------------------------------------------

INTERFACE
!> authors: Vikas Sharma, Ph. D.
! date: 6 March 2021
! summary: Set values in [[realmatrix_]]

!> authors: Dr. Vikas Sharma
!
! This subroutine set values in `obj%l`
! `obj%l( i, j ) = Val`
!
!### Usage
!
!```fortran
!
!```

MODULE PURE SUBROUTINE realmat_set_2( obj, Val, Row, Col )
  TYPE( RealMatrix_ ), INTENT( INOUT ) :: obj
  REAL( DFP ), INTENT( IN ) :: Val
  INTEGER( I4B ), INTENT( IN ) :: Col, Row
END SUBROUTINE realmat_set_2
END INTERFACE

INTERFACE set
  MODULE PROCEDURE realmat_set_2
END INTERFACE set

!----------------------------------------------------------------------------
!                                                        setValues@setValues
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	6 March 2021
! summary: 	 Set values in [[realmatrix_]]
!
!# Introduction
!
! This subroutine set values in `obj%l`
!

INTERFACE
MODULE PURE SUBROUTINE realmat_set_3( obj, Val, Row, Col )
  TYPE( RealMatrix_ ), INTENT( INOUT ) :: obj
  REAL( DFP ), INTENT( IN ) :: Val( :, : )
  INTEGER( I4B ), INTENT( IN ) :: Col( : ), Row( : )
END SUBROUTINE realmat_set_3
END INTERFACE

INTERFACE set
  MODULE PROCEDURE realmat_set_3
END INTERFACE set

!----------------------------------------------------------------------------
!                                                        setValues@setValues
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 6 March 2021
! summary: Set values in [[realmatrix_]]
!
!# Introduction
!
! This subroutine set values in `obj%l`
! - If `ExtraOption=MATRIX_DIAGONAL` then diagonal values are set; and `Indx`
! denotes diagonal number with `0` being the main diagonal
! - If `Extraoption=MATRIX_ROW` then row values are set; `Indx` then denotes
! row number
! - If `Extraoption=MATRIX_COLUMN` then col values are set; `Indx` then
! denotes col number
!
!### Usage
!
!```fortran
!
!```

INTERFACE
MODULE PURE SUBROUTINE realmat_set_4( obj, Val, Indx, ExtraOption )
  TYPE( RealMatrix_ ), INTENT( INOUT ) :: obj
  REAL( DFP ), INTENT( IN ) :: Val( : )
  INTEGER( I4B ), INTENT( IN ) :: Indx
  INTEGER( I4B ), INTENT( IN ) :: ExtraOption
END SUBROUTINE realmat_set_4
END INTERFACE

INTERFACE set
  MODULE PROCEDURE realmat_set_4
END INTERFACE set

!----------------------------------------------------------------------------
!                                                        setValues@setValues
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	6 March 2021
! summary: Set values in [[realmatrix_]]
!
!# Introduction
!
! This subroutine set values in `obj%l`
! - If `ExtraOption=MATRIX_DIAGONAL` then diagonal values are set; and `Indx`
! denotes the diagonal number with `0` being the main diagonal
! - If `Extraoption=ROW` then row values are set; `Indx` then denotes row
! number
! - If `Extraoption=COLUMN` then col values are set; `Indx` then denotes col
! number
!
!### Usage
!
!```fortran
!
!```

INTERFACE
MODULE PURE SUBROUTINE realmat_set_5( obj, Val, Indx, ExtraOption )
  TYPE( RealMatrix_ ), INTENT( INOUT ) :: obj
  REAL( DFP ), INTENT( IN ) :: Val( :, : )
  INTEGER( I4B ), INTENT( IN ) :: Indx ( : ), ExtraOption
END SUBROUTINE realmat_set_5
END INTERFACE

INTERFACE set
  MODULE PROCEDURE realmat_set_5
END INTERFACE set

!----------------------------------------------------------------------------
!                                                        setValues@setValues
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	6 March 2021
! summary: Add values in [[realmatrix_]]
!
!# Introduction
!
! This subroutine set `obj%l` to `Val`
!
!### Usage
!
!```fortran
!
!```

INTERFACE
MODULE PURE SUBROUTINE realmat_set_6( obj, Val )
  TYPE( RealMatrix_ ), INTENT( INOUT ) :: obj
  REAL( DFP ), INTENT( IN ) :: Val
END SUBROUTINE realmat_set_6
END INTERFACE

INTERFACE set
  MODULE PROCEDURE realmat_set_6
END INTERFACE set

INTERFACE ASSIGNMENT(=)
  MODULE PROCEDURE realmat_set_6
END INTERFACE ASSIGNMENT(=)

!----------------------------------------------------------------------------
!                                                  addContribution@setValues
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	6 March 2021
! summary: 	 Add contribution in values of [[realmatrix_]]
!
!# Introduction
!
! This subroutine adds contribution in values of `obj%l`. This subroutine
! performs following task:
!
! $$obj = obj <op> scale * val $$
!
! Here `op` can be `+, -, *, /`.
!
!@todo
! 	Use Blas routines or OpenMP support?
!@endtodo

INTERFACE
MODULE PURE SUBROUTINE realmat_add_1( obj, Val, Scale, Op )
  TYPE( RealMatrix_ ), INTENT( INOUT ) :: obj
  REAL( DFP ), INTENT( IN ) :: Val( :, : )
  REAL( DFP ), INTENT( IN ) :: Scale
    !! Scaling for `Val`
  CHARACTER( LEN = 1 ), INTENT( IN ) :: Op
    !! operator symbol; `+, -, *, /`
END SUBROUTINE realmat_add_1
END INTERFACE

INTERFACE add
  MODULE PROCEDURE realmat_add_1
END INTERFACE add

!----------------------------------------------------------------------------
!                                                  addContribution@setValues
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	6 March 2021
! summary: Add contribution in values of [[Realmatrix_]]
!
!# Introduction
!
! This subroutine adds contribution in values of `obj%l`
!
!```fortran
! obj%l = obj%v%*Op* scale * val
!```

INTERFACE
MODULE PURE SUBROUTINE realmat_add_2( obj, Val, Row, Col, Scale, Op )
  TYPE( RealMatrix_ ), INTENT( INOUT ) :: obj
  REAL( DFP ), INTENT( IN ) :: Val
  REAL( DFP ), INTENT( IN ) :: Scale
  INTEGER( I4B ), INTENT( IN ) :: Row
  INTEGER( I4B ), INTENT( IN ) :: Col
  CHARACTER( LEN = 1 ), INTENT( IN ) :: Op
END SUBROUTINE realmat_add_2
END INTERFACE

INTERFACE add
  MODULE PROCEDURE realmat_add_2
END INTERFACE add

!----------------------------------------------------------------------------
!                                                  addContribution@setValues
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 6 March 2021
! summary: Add contribution in values of [[realmatrix_]]
!
!# Introduction
!
! This subroutine adds contribution in values of `obj%l`
!
!```fortran
! obj%l = obj%v%*Op* scale * val
!```

INTERFACE
MODULE PURE SUBROUTINE realmat_add_3( obj, Val, Row, Col, Scale, Op )
  TYPE( RealMatrix_ ), INTENT( INOUT ) :: obj
  REAL( DFP ), INTENT( IN ) :: Val( :, : )
  REAL( DFP ), INTENT( IN ) :: Scale
  INTEGER( I4B ), INTENT( IN ) :: Row( : )
  INTEGER( I4B ), INTENT( IN ) :: Col( : )
  CHARACTER( LEN = 1 ), INTENT( IN ) :: Op
END SUBROUTINE realmat_add_3
END INTERFACE

INTERFACE add
  MODULE PROCEDURE realmat_add_3
END INTERFACE add

!----------------------------------------------------------------------------
!                                                  addContribution@setValues
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	6 March 2021
! summary: Add contribution in values of [[Realmatrix_]]
!
!# Introduction
!
! This subroutine adds contribution in values of `obj%l`
!
! ```fortran
! obj%l = obj%v%*Op* scale * val
! ```

INTERFACE
MODULE PURE SUBROUTINE realmat_add_4( obj, Val, Indx, ExtraOption, Scale, Op )
  TYPE( RealMatrix_ ), INTENT( INOUT ) :: obj
  REAL( DFP ), INTENT( IN ) :: Val( : ), Scale
  INTEGER( I4B ), INTENT( IN ) :: Indx
  INTEGER( I4B ), INTENT( IN ) :: ExtraOption
  CHARACTER( LEN = 1 ), INTENT( IN ) :: Op
END SUBROUTINE realmat_add_4
END INTERFACE

INTERFACE add
  MODULE PROCEDURE realmat_add_4
END INTERFACE add

!----------------------------------------------------------------------------
!                                                  addContribution@setValues
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	6 March 2021
! summary: Add contribution in values of [[realmatrix_]]
!
!# Introduction
!
! This subroutine adds contribution in values of `obj%l`
!
!```fortran
! obj%l = obj%v%*Op* scale * val
!```

INTERFACE
MODULE PURE SUBROUTINE realmat_add_5( obj, Val, Indx, ExtraOption, &
  & Scale, Op )
  TYPE( RealMatrix_ ), INTENT( INOUT ) :: obj
  REAL( DFP ), INTENT( IN ) :: Val( :, : ), Scale
  INTEGER( I4B ), INTENT( IN ) :: Indx( : )
  INTEGER( I4B ), INTENT( IN ) :: ExtraOption
  CHARACTER( LEN = 1 ), INTENT( IN ) :: Op
END SUBROUTINE realmat_add_5
END INTERFACE

INTERFACE add
  MODULE PROCEDURE realmat_add_5
END INTERFACE add
