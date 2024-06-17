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

SUBMODULE(SymUtility) Methods
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Sym_Int8
#include "./Sym/Sym.inc"
END PROCEDURE Sym_Int8

MODULE PROCEDURE Sym_Int16
#include "./Sym/Sym.inc"
END PROCEDURE Sym_Int16

MODULE PROCEDURE Sym_Int32
#include "./Sym/Sym.inc"
END PROCEDURE Sym_Int32

MODULE PROCEDURE Sym_Int64
#include "./Sym/Sym.inc"
END PROCEDURE Sym_Int64

MODULE PROCEDURE Sym_Real32
#include "./Sym/Sym.inc"
END PROCEDURE Sym_Real32

MODULE PROCEDURE Sym_Real64
#include "./Sym/Sym.inc"
END PROCEDURE Sym_Real64

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE GetSym_Int8
#include "./Sym/GetSym.inc"
END PROCEDURE GetSym_Int8

MODULE PROCEDURE GetSym_Int16
#include "./Sym/GetSym.inc"
END PROCEDURE GetSym_Int16

MODULE PROCEDURE GetSym_Int32
#include "./Sym/GetSym.inc"
END PROCEDURE GetSym_Int32

MODULE PROCEDURE GetSym_Int64
#include "./Sym/GetSym.inc"
END PROCEDURE GetSym_Int64

MODULE PROCEDURE GetSym_Real32
#include "./Sym/GetSym.inc"
END PROCEDURE GetSym_Real32

MODULE PROCEDURE GetSym_Real64
#include "./Sym/GetSym.inc"
END PROCEDURE GetSym_Real64

END SUBMODULE Methods
