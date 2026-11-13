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
! date: 2021-03-21
! summary: This module contains methods for [[CSRSparsity_]]

MODULE CSRSparsity_Method
USE BaseType, ONLY: CSRSparsity_
USE BaseType, ONLY: DOF_
USE BaseType, ONLY: IntVector_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
IMPLICIT NONE
PRIVATE

PUBLIC :: GetSym
PUBLIC :: Initiate
PUBLIC :: ASSIGNMENT(=)
PUBLIC :: SetSparsity
PUBLIC :: CSRSparsity
PUBLIC :: CSRSparsityPointer
PUBLIC :: DEALLOCATE
PUBLIC :: GetDiagonal
PUBLIC :: Display
PUBLIC :: Shape
PUBLIC :: Size
PUBLIC :: GetNNZ
PUBLIC :: GetColIndex
PUBLIC :: GetColNumber
PUBLIC :: OPERATOR(.startColumn.)
PUBLIC :: OPERATOR(.endColumn.)
PUBLIC :: SetIA
PUBLIC :: SetJA
PUBLIC :: GetIA
PUBLIC :: GetJA

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 22 March 2021
! summary: Initiate an empty sparsity pattern of a given shape
!
! This routine deallocates `obj` and prepares it for the `SetSparsity`
! calls. `obj%IA` and `obj%idiag` are allocated, with sizes `nrow + 1`
! and `nrow`, and `obj%nnz` is set to `nnz` (default 0). `obj%JA` is
! allocated only when `nnz` is positive. The object is not locked.
!
! `idof` and `jdof` are copied into the object, and they must be given
! together. In debug builds the routine stops when only one of them is
! present, or when `.tNodes. idof` differs from `nrow` or `.tNodes. jdof`
! differs from `ncol`.
!
! @note
! If `idof` and `jdof` are not present, then this routine initiates
! `obj%idof` and `obj%jdof` internally with the following options.
!
! - tNodes = [nrow] for `idof`, [ncol] for `jdof`
! - names = ["K"]
! - spacecompo = [1]
! - timecompo = [1]
! - storageFMT = NODES_FMT
! @endnote
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: CSRSparsity_
! USE BaseType, ONLY: DOF_
! USE CSRSparsity_Method, ONLY: ASSIGNMENT(=)
! USE CSRSparsity_Method, ONLY: CSRSparsity
! USE CSRSparsity_Method, ONLY: CSRSparsityPointer
! USE CSRSparsity_Method, ONLY: DEALLOCATE
! USE CSRSparsity_Method, ONLY: Initiate
! USE DOF_Method, ONLY: DEALLOCATE
! USE DOF_Method, ONLY: Initiate
! USE GlobalData, ONLY: FMT_DOF
! USE GlobalData, ONLY: FMT_NODES
! USE GlobalData, ONLY: I4B
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! ! Test 1: internal scalar DOF, rectangular shape
! BLOCK
!   TYPE(CSRSparsity_) :: csr
!   CALL Initiate(obj=csr, ncol=4_I4B, nrow=3_I4B)
!   CALL OK(csr%isInitiated, "default: isInitiated")
!   CALL OK(.NOT. csr%isSparsityLock, "default: not locked")
!   CALL OK(csr%nrow .EQ. 3_I4B, "default: nrow")
!   CALL OK(csr%ncol .EQ. 4_I4B, "default: ncol")
!   CALL OK(csr%nnz .EQ. 0_I4B, "default: nnz")
!   CALL OK(SIZE(csr%IA) .EQ. 4, "default: SIZE(IA)")
!   CALL OK(SIZE(csr%idiag) .EQ. 3, "default: SIZE(idiag)")
!   CALL OK(.NOT. ALLOCATED(csr%JA), "default: JA is not allocated")
!   CALL OK(csr%idof%storageFMT .EQ. FMT_NODES, "default: idof format")
!   CALL OK(csr%jdof%storageFMT .EQ. FMT_NODES, "default: jdof format")
!   CALL DEALLOCATE (csr)
! END BLOCK
!
! ! Test 2: nnz allocates JA
! BLOCK
!   TYPE(CSRSparsity_) :: csr
!   CALL Initiate(obj=csr, ncol=3_I4B, nrow=3_I4B, nnz=5_I4B)
!   CALL OK(csr%nnz .EQ. 5_I4B, "nnz: nnz")
!   CALL OK(SIZE(csr%JA) .EQ. 5, "nnz: SIZE(JA)")
!   CALL DEALLOCATE (csr)
! END BLOCK
!
! ! Test 3: given DOF objects are copied
! BLOCK
!   TYPE(CSRSparsity_) :: csr
!   TYPE(DOF_) :: dof
!   CALL Initiate(obj=dof, tNodes=[3_I4B], names=["U"], &
!                 spacecompo=[2_I4B], timecompo=[1_I4B], &
!                 storagefmt=FMT_DOF)
!   CALL Initiate(obj=csr, ncol=6_I4B, nrow=6_I4B, idof=dof, jdof=dof)
!   CALL DEALLOCATE (dof)
!   CALL OK(csr%idof%storageFMT .EQ. FMT_DOF, "dof: idof format")
!   CALL OK(csr%jdof%storageFMT .EQ. FMT_DOF, "dof: jdof format")
!   CALL DEALLOCATE (csr)
! END BLOCK
!
! ! Test 4: a second Initiate replaces the first one
! BLOCK
!   TYPE(CSRSparsity_) :: csr
!   CALL Initiate(obj=csr, ncol=3_I4B, nrow=3_I4B, nnz=5_I4B)
!   CALL Initiate(obj=csr, ncol=2_I4B, nrow=2_I4B)
!   CALL OK(csr%nrow .EQ. 2_I4B, "again: nrow")
!   CALL OK(csr%nnz .EQ. 0_I4B, "again: nnz")
!   CALL OK(SIZE(csr%IA) .EQ. 3, "again: SIZE(IA)")
!   CALL OK(.NOT. ALLOCATED(csr%JA), "again: JA is not allocated")
!   CALL DEALLOCATE (csr)
! END BLOCK
!
! ! Test 5: constructors
! BLOCK
!   TYPE(CSRSparsity_) :: csr
!   TYPE(CSRSparsity_), POINTER :: ptr
!   csr = CSRSparsity(nrow=3_I4B, ncol=4_I4B)
!   CALL OK(csr%isInitiated, "CSRSparsity: isInitiated")
!   CALL OK(csr%nrow .EQ. 3_I4B, "CSRSparsity: nrow")
!   CALL OK(csr%ncol .EQ. 4_I4B, "CSRSparsity: ncol")
!   ptr => CSRSparsityPointer(nrow=3_I4B, ncol=4_I4B)
!   CALL OK(ptr%isInitiated, "CSRSparsityPointer: isInitiated")
!   CALL OK(ptr%nrow .EQ. 3_I4B, "CSRSparsityPointer: nrow")
!   CALL OK(ptr%ncol .EQ. 4_I4B, "CSRSparsityPointer: ncol")
!   CALL DEALLOCATE (csr)
!   CALL DEALLOCATE (ptr)
!   DEALLOCATE (ptr)
! END BLOCK
!
! END PROGRAM main
!```
!
! See also: [[CSRSparsity_Method:obj_Initiate3]],
! [[CSRSparsity_Method:obj_SetSparsity1]]

INTERFACE Initiate
  MODULE SUBROUTINE obj_Initiate1(obj, ncol, nrow, idof, jdof, nnz)
    TYPE(CSRSparsity_), INTENT(INOUT) :: obj
    !! sparsity pattern, its old content is deallocated
    INTEGER(I4B), INTENT(IN) :: ncol, nrow
    !! number of columns and rows of the matrix
    TYPE(DOF_), OPTIONAL, INTENT(IN) :: idof
    !! DOF for row
    TYPE(DOF_), OPTIONAL, INTENT(IN) :: jdof
    !! DOF for column
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: nnz
    !! number of nonzeros
  END SUBROUTINE obj_Initiate1
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 22 March 2021
! summary: Copy a `CSRSparsity_` object
!
! This routine deallocates `obj` and copies `obj2` into it: the shape,
! the flags, `IA`, `JA`, `idiag`, the row storage `row`, `idof` and
! `jdof`. The copy does not share data with `obj2`. It also defines the
! assignment operator.
!
! An object can be copied at any time, also while the pattern is being
! built. The final `SetSparsity(obj)` call on the copy then gives the
! same pattern as on `obj2`.
!
! Changes:
!   - 2026-11-05: The row storage of obj2 is now copied.
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: CSRSparsity_
! USE CSRSparsity_Method, ONLY: ASSIGNMENT(=)
! USE CSRSparsity_Method, ONLY: DEALLOCATE
! USE CSRSparsity_Method, ONLY: Initiate
! USE CSRSparsity_Method, ONLY: SetSparsity
! USE GlobalData, ONLY: I4B
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! ! Test 1: copy of a finished pattern with Initiate
! BLOCK
!   TYPE(CSRSparsity_) :: csr1, csr2
!   CALL Initiate(obj=csr1, ncol=3_I4B, nrow=3_I4B)
!   CALL SetSparsity(obj=csr1, row=1_I4B, col=[2_I4B, 1_I4B])
!   CALL SetSparsity(csr1)
!   CALL Initiate(obj=csr2, obj2=csr1)
!   CALL OK(csr2%isSparsityLock, "Initiate: locked")
!   CALL OK(csr2%nnz .EQ. 2_I4B, "Initiate: nnz")
!   CALL OK(ALL(csr2%IA .EQ. [1, 3, 3, 3]), "Initiate: IA")
!   CALL OK(ALL(csr2%JA .EQ. [1, 2]), "Initiate: JA")
!   CALL DEALLOCATE (csr1)
!   CALL OK(ALL(csr2%JA .EQ. [1, 2]), "Initiate: independent of source")
!   CALL DEALLOCATE (csr2)
! END BLOCK
!
! ! Test 2: assignment replaces the content of the left side
! BLOCK
!   TYPE(CSRSparsity_) :: csr1, csr2
!   CALL Initiate(obj=csr1, ncol=2_I4B, nrow=2_I4B)
!   CALL SetSparsity(obj=csr1, row=2_I4B, col=[1_I4B])
!   CALL SetSparsity(csr1)
!   CALL Initiate(obj=csr2, ncol=5_I4B, nrow=5_I4B, nnz=9_I4B)
!   csr2 = csr1
!   CALL OK(csr2%nrow .EQ. 2_I4B, "assignment: nrow")
!   CALL OK(ALL(csr2%IA .EQ. [1, 1, 2]), "assignment: IA")
!   CALL OK(ALL(csr2%JA .EQ. [1]), "assignment: JA")
!   CALL DEALLOCATE (csr1)
!   CALL DEALLOCATE (csr2)
! END BLOCK
!
! ! Test 3: copy of a pattern that is still being built
! BLOCK
!   TYPE(CSRSparsity_) :: csr1, csr2
!   CALL Initiate(obj=csr1, ncol=3_I4B, nrow=3_I4B)
!   CALL SetSparsity(obj=csr1, row=1_I4B, col=[2_I4B, 1_I4B])
!   csr2 = csr1
!   CALL OK(ALLOCATED(csr2%row), "partial copy: row storage is copied")
!   IF (ALLOCATED(csr2%row)) THEN
!     CALL SetSparsity(csr2)
!     CALL OK(csr2%nnz .EQ. 2_I4B, "partial copy: nnz")
!     CALL OK(ALL(csr2%IA .EQ. [1, 3, 3, 3]), "partial copy: IA")
!     CALL OK(ALL(csr2%JA .EQ. [1, 2]), "partial copy: JA")
!   END IF
!   CALL DEALLOCATE (csr1)
!   CALL DEALLOCATE (csr2)
! END BLOCK
!
! ! Test 4: DEALLOCATE resets every component
! BLOCK
!   TYPE(CSRSparsity_) :: csr
!   CALL Initiate(obj=csr, ncol=3_I4B, nrow=3_I4B)
!   CALL SetSparsity(obj=csr, row=1_I4B, col=[2_I4B])
!   CALL DEALLOCATE (csr)
!   CALL OK(.NOT. csr%isInitiated, "DEALLOCATE: isInitiated")
!   CALL OK(.NOT. csr%isSparsityLock, "DEALLOCATE: isSparsityLock")
!   CALL OK(csr%nnz .EQ. 0_I4B, "DEALLOCATE: nnz")
!   CALL OK(csr%nrow .EQ. 0_I4B, "DEALLOCATE: nrow")
!   CALL OK(csr%ncol .EQ. 0_I4B, "DEALLOCATE: ncol")
!   CALL OK(.NOT. ALLOCATED(csr%IA), "DEALLOCATE: IA")
!   CALL OK(.NOT. ALLOCATED(csr%JA), "DEALLOCATE: JA")
!   CALL OK(.NOT. ALLOCATED(csr%idiag), "DEALLOCATE: idiag")
!   CALL OK(.NOT. ALLOCATED(csr%row), "DEALLOCATE: row")
! END BLOCK
!
! ! Test 5: DEALLOCATE of an object that was never initiated
! BLOCK
!   TYPE(CSRSparsity_) :: csr
!   CALL DEALLOCATE (csr)
!   CALL OK(.NOT. csr%isInitiated, "DEALLOCATE: empty object")
! END BLOCK
!
! END PROGRAM main
!```
!
! See also: [[CSRSparsity_Method:obj_Deallocate]]

INTERFACE Initiate
  MODULE SUBROUTINE obj_Initiate2(obj, obj2)
    TYPE(CSRSparsity_), INTENT(INOUT) :: obj
    !! the copy, its old content is deallocated
    TYPE(CSRSparsity_), INTENT(IN) :: obj2
    !! object to copy
  END SUBROUTINE obj_Initiate2
END INTERFACE Initiate

INTERFACE ASSIGNMENT(=)
  MODULE PROCEDURE obj_Initiate2
END INTERFACE ASSIGNMENT(=)

!----------------------------------------------------------------------------
!                                                 Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 22 March 2021
! summary: Initiate a `CSRSparsity_` object from IA and JA
!
! This routine constructs the object from existing CSR indices, which is
! helpful in reading data from files. The number of rows is
! `SIZE(IA) - 1` and `obj%nnz` is `SIZE(JA)`. The indices are copied
! without any check.
!
! This routine calls [[CSRSparsity_Method:obj_Initiate1]] without `idof`
! and `jdof`, so the object has the internal scalar DOF and no useful
! information about the degrees of freedom.
!
! The object is not locked. `SetSparsity(obj)` locks it and keeps `IA`
! and `JA` unchanged.
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: CSRSparsity_
! USE CSRSparsity_Method, ONLY: ASSIGNMENT(=)
! USE CSRSparsity_Method, ONLY: CSRSparsity
! USE CSRSparsity_Method, ONLY: CSRSparsityPointer
! USE CSRSparsity_Method, ONLY: DEALLOCATE
! USE CSRSparsity_Method, ONLY: Initiate
! USE GlobalData, ONLY: I4B
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! ! Test 1: ncol defaults to the number of rows
! BLOCK
!   TYPE(CSRSparsity_) :: csr
!   CALL Initiate(obj=csr, IA=[1_I4B, 3_I4B, 3_I4B, 4_I4B], &
!                 JA=[1_I4B, 3_I4B, 2_I4B])
!   CALL OK(csr%nrow .EQ. 3_I4B, "default ncol: nrow")
!   CALL OK(csr%ncol .EQ. 3_I4B, "default ncol: ncol")
!   CALL OK(csr%nnz .EQ. 3_I4B, "default ncol: nnz")
!   CALL OK(ALL(csr%IA .EQ. [1, 3, 3, 4]), "default ncol: IA")
!   CALL OK(ALL(csr%JA .EQ. [1, 3, 2]), "default ncol: JA")
!   CALL OK(csr%isInitiated, "default ncol: isInitiated")
!   CALL DEALLOCATE (csr)
! END BLOCK
!
! ! Test 2: rectangular pattern with ncol
! BLOCK
!   TYPE(CSRSparsity_) :: csr
!   CALL Initiate(obj=csr, IA=[1_I4B, 2_I4B, 3_I4B], &
!                 JA=[5_I4B, 1_I4B], ncol=5_I4B)
!   CALL OK(csr%nrow .EQ. 2_I4B, "ncol: nrow")
!   CALL OK(csr%ncol .EQ. 5_I4B, "ncol: ncol")
!   CALL DEALLOCATE (csr)
! END BLOCK
!
! ! Test 3: no entries
! BLOCK
!   TYPE(CSRSparsity_) :: csr
!   INTEGER(I4B) :: empty(0)
!   CALL Initiate(obj=csr, IA=[1_I4B, 1_I4B, 1_I4B], JA=empty)
!   CALL OK(csr%nrow .EQ. 2_I4B, "empty: nrow")
!   CALL OK(csr%nnz .EQ. 0_I4B, "empty: nnz")
!   CALL OK(SIZE(csr%JA) .EQ. 0, "empty: SIZE(JA)")
!   CALL DEALLOCATE (csr)
! END BLOCK
!
! ! Test 4: constructors
! BLOCK
!   TYPE(CSRSparsity_) :: csr
!   TYPE(CSRSparsity_), POINTER :: ptr
!   csr = CSRSparsity(IA=[1_I4B, 2_I4B, 3_I4B], JA=[2_I4B, 1_I4B])
!   CALL OK(csr%nnz .EQ. 2_I4B, "CSRSparsity: nnz")
!   CALL OK(ALL(csr%JA .EQ. [2, 1]), "CSRSparsity: JA")
!   ptr => CSRSparsityPointer(IA=[1_I4B, 2_I4B, 3_I4B], JA=[2_I4B, 1_I4B])
!   CALL OK(ptr%nnz .EQ. 2_I4B, "CSRSparsityPointer: nnz")
!   CALL OK(ALL(ptr%JA .EQ. [2, 1]), "CSRSparsityPointer: JA")
!   CALL DEALLOCATE (csr)
!   CALL DEALLOCATE (ptr)
!   DEALLOCATE (ptr)
! END BLOCK
!
! END PROGRAM main
!```
!
! See also: [[CSRSparsity_Method:obj_Initiate1]]

INTERFACE Initiate
  MODULE SUBROUTINE obj_Initiate3(obj, IA, JA, ncol)
    TYPE(CSRSparsity_), INTENT(INOUT) :: obj
    !! sparsity pattern, its old content is deallocated
    INTEGER(I4B), INTENT(IN) :: IA(:), JA(:)
    !! CSR indices, SIZE(IA) = nrow + 1, SIZE(JA) = nnz
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ncol
    !! number of columns, default is number of rows
  END SUBROUTINE obj_Initiate3
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                            CSRSparsity@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 10 Oct, 2021
! summary: Returns an empty sparsity pattern of a given shape
!
! This function calls [[CSRSparsity_Method:obj_Initiate1]] and returns
! the result. `idof` and `jdof` must be given together.
!
! Returns:
!   ans : TYPE(CSRSparsity_) - initiated object
!
! Examples
!
!```fortran
! TYPE(CSRSparsity_) :: csr
!
! csr = CSRSparsity(nrow=3_I4B, ncol=4_I4B)
!```
!
! Full example: src/modules/CSRSparsity/examples/Initiate_test_1.F90
!
! See also: [[CSRSparsity_Method:obj_Initiate1]]

INTERFACE CSRSparsity
  MODULE FUNCTION obj_Constructor1(nrow, ncol, idof, jdof) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: nrow
    !! number of rows
    INTEGER(I4B), INTENT(IN) :: ncol
    !! number of columns
    TYPE(DOF_), OPTIONAL, INTENT(IN) :: idof
    !! dof for row
    TYPE(DOF_), OPTIONAL, INTENT(IN) :: jdof
    !! dof for column
    TYPE(CSRSparsity_) :: ans
    !! initiated object
  END FUNCTION obj_Constructor1
END INTERFACE CSRSparsity

!----------------------------------------------------------------------------
!                                            CSRSparsity@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 10 Oct, 2021
! summary: Returns a sparsity pattern made from IA and JA
!
! This function calls [[CSRSparsity_Method:obj_Initiate3]] and returns
! the result. The matrix is square, since `ncol` cannot be given here.
!
! Returns:
!   ans : TYPE(CSRSparsity_) - initiated object
!
! Examples
!
!```fortran
! TYPE(CSRSparsity_) :: csr
!
! csr = CSRSparsity(IA=[1_I4B, 2_I4B, 3_I4B], JA=[2_I4B, 1_I4B])
!```
!
! Full example: src/modules/CSRSparsity/examples/Initiate_test_3.F90
!
! See also: [[CSRSparsity_Method:obj_Initiate3]]

INTERFACE CSRSparsity
  MODULE FUNCTION obj_Constructor2(IA, JA) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: IA(:)
    !! row pointers, SIZE(IA) = nrow + 1
    INTEGER(I4B), INTENT(IN) :: JA(:)
    !! column numbers, SIZE(JA) = nnz
    TYPE(CSRSparsity_) :: ans
    !! initiated object
  END FUNCTION obj_Constructor2
END INTERFACE CSRSparsity

!----------------------------------------------------------------------------
!                                       CSRSparsityPointer@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 10 Oct, 2021
! summary: Returns a pointer to an empty sparsity pattern
!
! This function allocates the object and calls
! [[CSRSparsity_Method:obj_Initiate1]]. The caller owns the target: call
! `DEALLOCATE(ptr)` of this module for its content, then the Fortran
! `DEALLOCATE` statement for the pointer.
!
! Returns:
!   ans : TYPE(CSRSparsity_), POINTER - new object
!
! Examples
!
!```fortran
! TYPE(CSRSparsity_), POINTER :: ptr
!
! ptr => CSRSparsityPointer(nrow=3_I4B, ncol=4_I4B)
! CALL DEALLOCATE (ptr)
! DEALLOCATE (ptr)
!```
!
! Full example: src/modules/CSRSparsity/examples/Initiate_test_1.F90
!
! See also: [[CSRSparsity_Method:obj_Constructor1]]

INTERFACE CSRSparsityPointer
  MODULE FUNCTION obj_Constructor_1(nrow, ncol, idof, jdof) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: nrow
    !! number of rows
    INTEGER(I4B), INTENT(IN) :: ncol
    !! number of columns
    TYPE(DOF_), OPTIONAL, INTENT(IN) :: idof
    !! dof for row
    TYPE(DOF_), OPTIONAL, INTENT(IN) :: jdof
    !! dof for column
    TYPE(CSRSparsity_), POINTER :: ans
    !! newly allocated and initiated object
  END FUNCTION obj_Constructor_1
END INTERFACE CSRSparsityPointer

!----------------------------------------------------------------------------
!                                      CSRSparsityPointer@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 10 Oct, 2021
! summary: Returns a pointer to a sparsity pattern made from IA and JA
!
! This function allocates the object and calls
! [[CSRSparsity_Method:obj_Initiate3]]. The caller owns the target, see
! [[CSRSparsity_Method:obj_Constructor_1]].
!
! Returns:
!   ans : TYPE(CSRSparsity_), POINTER - new object
!
! Examples
!
!```fortran
! TYPE(CSRSparsity_), POINTER :: ptr
!
! ptr => CSRSparsityPointer(IA=[1_I4B, 2_I4B, 3_I4B], JA=[2_I4B, 1_I4B])
!```
!
! Full example: src/modules/CSRSparsity/examples/Initiate_test_3.F90
!
! See also: [[CSRSparsity_Method:obj_Constructor2]]

INTERFACE CSRSparsityPointer
  MODULE FUNCTION obj_Constructor_2(IA, JA) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: IA(:)
    !! row pointers, SIZE(IA) = nrow + 1
    INTEGER(I4B), INTENT(IN) :: JA(:)
    !! column numbers, SIZE(JA) = nnz
    TYPE(CSRSparsity_), POINTER :: ans
    !! newly allocated and initiated object
  END FUNCTION obj_Constructor_2
END INTERFACE CSRSparsityPointer

!----------------------------------------------------------------------------
!                                             Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 22 March 2021
! summary: Deallocate the data of a `CSRSparsity_` object
!
! This routine frees the data of `obj`, resets `nnz`, `nrow` and `ncol`
! to 0 and all the flags to `.FALSE.`. The object can be initiated again
! with any shape. Calling it on an object that was never initiated is
! allowed.
!
! Changes:
!   - 2026-11-05: The row storage obj%row is now deallocated.
!
! Examples
!
!```fortran
! CALL DEALLOCATE (csr)
!```
!
! Full example: src/modules/CSRSparsity/examples/Initiate_test_2.F90
!
! See also: [[CSRSparsity_Method:obj_Initiate1]]

INTERFACE DEALLOCATE
  MODULE PURE SUBROUTINE obj_Deallocate(obj)
    TYPE(CSRSparsity_), INTENT(INOUT) :: obj
    !! object to deallocate
  END SUBROUTINE obj_Deallocate
END INTERFACE DEALLOCATE

!----------------------------------------------------------------------------
!                                                         Display@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 22 March 2021
! summary: Display the content of a `CSRSparsity_` object
!
! This routine writes `Msg`, `nnz`, `ncol`, `nrow`, `idof`, `jdof`, `IA`,
! `JA` and `idiag`, and `row` while it is allocated. For `IA`, `JA` and
! `idiag`, a line with the text "is not allocated" replaces an array that
! is not allocated.
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: CSRSparsity_
! USE CSRSparsity_Method, ONLY: DEALLOCATE
! USE CSRSparsity_Method, ONLY: Display
! USE CSRSparsity_Method, ONLY: Initiate
! USE CSRSparsity_Method, ONLY: SetSparsity
! USE GlobalData, ONLY: I4B
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! ! Test 1: finished pattern
! BLOCK
!   TYPE(CSRSparsity_) :: csr
!   CALL Initiate(obj=csr, ncol=2_I4B, nrow=2_I4B)
!   CALL SetSparsity(obj=csr, row=1_I4B, col=[1_I4B])
!   CALL SetSparsity(csr)
!   CALL OK(Contains_(csr, "my title"), "finished: Msg")
!   CALL OK(Contains_(csr, "# NNZ"), "finished: nnz")
!   CALL OK(Contains_(csr, "# NROW"), "finished: nrow")
!   CALL OK(Contains_(csr, "# NCOL"), "finished: ncol")
!   CALL OK(Contains_(csr, "# IA"), "finished: IA")
!   CALL OK(Contains_(csr, "# JA"), "finished: JA")
!   CALL DEALLOCATE (csr)
! END BLOCK
!
! ! Test 2: JA is not allocated
! BLOCK
!   TYPE(CSRSparsity_) :: csr
!   CALL Initiate(obj=csr, ncol=2_I4B, nrow=2_I4B)
!   CALL OK(Contains_(csr, "# JA is not allocated"), "no JA: message")
!   CALL DEALLOCATE (csr)
! END BLOCK
!
! CONTAINS
!
! ! Displays obj on a scratch unit and looks for text in the lines
! FUNCTION Contains_(obj, text) RESULT(ans)
!   TYPE(CSRSparsity_), INTENT(IN) :: obj
!   CHARACTER(*), INTENT(IN) :: text
!   LOGICAL :: ans
!
!   CHARACTER(256) :: line, errmsg
!   INTEGER(I4B) :: unitNo, ios
!
!   OPEN (newunit=unitNo, status="SCRATCH", action="READWRITE")
!   CALL Display(obj=obj, Msg="my title", UnitNo=unitNo)
!   REWIND (unitNo)
!
!   ans = .FALSE.
!   scan: DO
!     READ (unitNo, "(A)", iostat=ios, iomsg=errmsg) line
!     IF (ios .NE. 0) EXIT scan
!     ans = ans .OR. (INDEX(line, text) .GT. 0)
!   END DO scan
!
!   CLOSE (unitNo)
! END FUNCTION Contains_
!
! END PROGRAM main
!```

INTERFACE Display
  MODULE SUBROUTINE obj_Display(obj, Msg, UnitNo)
    TYPE(CSRSparsity_), INTENT(IN) :: obj
    !! object to display
    CHARACTER(*), INTENT(IN) :: Msg
    !! message written first
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: UnitNo
    !! output unit, default is stdout
  END SUBROUTINE obj_Display
END INTERFACE Display

!----------------------------------------------------------------------------
!                                                          Shape@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 22 March 2021
! summary: Return the shape of the sparse matrix
!
! Returns:
!   ans : INTEGER(I4B)(2) - [nrow, ncol]
!
! Examples
!
!```fortran
! INTEGER(I4B) :: s(2)
!
! s = Shape(csr)
!```
!
! Full example: src/modules/CSRSparsity/examples/GetColIndex_test_1.F90
!
! See also: [[CSRSparsity_Method:obj_Size]]

INTERFACE Shape
  MODULE PURE FUNCTION obj_Shape(obj) RESULT(ans)
    TYPE(CSRSparsity_), INTENT(IN) :: obj
    !! sparsity pattern
    INTEGER(I4B) :: ans(2)
    !! [nrow, ncol]
  END FUNCTION obj_Shape
END INTERFACE Shape

!----------------------------------------------------------------------------
!                                                           Size@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 22 March 2021
! summary: Return the size of the sparse matrix
!
! If `Dims` is equal to 1, then the number of rows is returned. Any other
! value of `Dims` returns the number of columns. If `Dims` is absent, then
! `nrow * ncol` is returned.
!
! Returns:
!   ans : INTEGER(I4B) - requested size
!
! Examples
!
!```fortran
! INTEGER(I4B) :: nrow, ncol, total
!
! nrow = Size(csr, 1_I4B)
! ncol = Size(csr, 2_I4B)
! total = Size(csr)
!```
!
! Full example: src/modules/CSRSparsity/examples/GetColIndex_test_1.F90
!
! See also: [[CSRSparsity_Method:obj_Shape]]

INTERFACE Size
  MODULE PURE FUNCTION obj_Size(obj, Dims) RESULT(ans)
    TYPE(CSRSparsity_), INTENT(IN) :: obj
    !! sparsity pattern
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: Dims
    !! 1 for rows, otherwise columns
    INTEGER(I4B) :: ans
    !! requested size
  END FUNCTION obj_Size
END INTERFACE Size

!----------------------------------------------------------------------------
!                                                          GetNNZ@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 22 March 2021
! summary: Return the total number of nonzero entries
!
! This function returns `obj%nnz`. It is exact only after the final
! `SetSparsity(obj)` call; before that call it is an upper bound, see
! [[CSRSparsity_Method:obj_SetSparsity1]].
!
! Returns:
!   ans : INTEGER(I4B) - obj%nnz
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: CSRSparsity_
! USE CSRSparsity_Method, ONLY: DEALLOCATE
! USE CSRSparsity_Method, ONLY: GetNNZ
! USE CSRSparsity_Method, ONLY: Initiate
! USE GlobalData, ONLY: I4B
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! TYPE(CSRSparsity_) :: csr
!
! CALL Initiate(obj=csr, IA=[1_I4B, 4_I4B, 5_I4B, 7_I4B], &
!               JA=[1_I4B, 2_I4B, 3_I4B, 2_I4B, 1_I4B, 3_I4B])
!
! ! Test 1: total number of entries
! BLOCK
!   CALL OK(GetNNZ(csr) .EQ. 6_I4B, "total")
! END BLOCK
!
! ! Test 2: one part, upper and lower case
! BLOCK
!   CALL OK(GetNNZ(obj=csr, from="U") .EQ. 2_I4B, "from: U")
!   CALL OK(GetNNZ(obj=csr, from="u") .EQ. 2_I4B, "from: u")
!   CALL OK(GetNNZ(obj=csr, from="L") .EQ. 1_I4B, "from: L")
!   CALL OK(GetNNZ(obj=csr, from="l") .EQ. 1_I4B, "from: l")
!   CALL OK(GetNNZ(obj=csr, from="D") .EQ. 3_I4B, "from: D")
!   CALL OK(GetNNZ(obj=csr, from="d") .EQ. 3_I4B, "from: d")
!   CALL OK(GetNNZ(obj=csr, from="A") .EQ. 6_I4B, "from: A")
!   CALL OK(GetNNZ(obj=csr, from="?") .EQ. 6_I4B, "from: other value")
! END BLOCK
!
! ! Test 3: all the parts in one call
! BLOCK
!   CALL OK(ALL(GetNNZ(obj=csr, from=[" "]) .EQ. [2, 1, 3]), "parts")
! END BLOCK
!
! ! Test 4: an object that is not initiated
! BLOCK
!   TYPE(CSRSparsity_) :: empty
!   CALL OK(GetNNZ(empty) .EQ. 0_I4B, "not initiated: total")
!   CALL OK(GetNNZ(obj=empty, from="U") .EQ. 0_I4B, "not initiated: from")
!   CALL OK(ALL(GetNNZ(obj=empty, from=[" "]) .EQ. 0), "not initiated: parts")
! END BLOCK
!
! CALL DEALLOCATE (csr)
!
! END PROGRAM main
!```
!
! See also: [[CSRSparsity_Method:obj_GetNNZ1]],
! [[CSRSparsity_Method:obj_GetNNZ2]]

INTERFACE GetNNZ
  MODULE PURE FUNCTION obj_GetNNZ(obj) RESULT(ans)
    TYPE(CSRSparsity_), INTENT(IN) :: obj
    !! sparsity pattern
    INTEGER(I4B) :: ans
    !! obj%nnz
  END FUNCTION obj_GetNNZ
END INTERFACE GetNNZ

!----------------------------------------------------------------------------
!                                                          GetNNZ@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 22 March 2021
! summary: Return the number of nonzeros of the sum of two patterns
!
! For `op` equal to "+" or "-" this function returns the size of the union
! of the patterns of `obj1` and `obj2`, row by row. An entry present in
! both is counted once. Numerical cancellation is not considered. Both
! objects must have the same shape, and it is not checked.
!
! With `isSorted` true, the routine merges the sorted columns of each row
! without a work array. Unsorted columns give a wrong count in this case.
!
! `op` equal to "*" is listed but not implemented. The result is undefined
! for it and for any other value of `op`.
!
! Returns:
!   ans : INTEGER(I4B) - nonzeros of the sum
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: CSRSparsity_
! USE CSRSparsity_Method, ONLY: DEALLOCATE
! USE CSRSparsity_Method, ONLY: GetNNZ
! USE CSRSparsity_Method, ONLY: Initiate
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! LOGICAL(LGT), PARAMETER :: yes = .TRUE.
!
! ! Test 1: sorted columns, one shared entry
! BLOCK
!   TYPE(CSRSparsity_) :: a, b
!   CALL Initiate(obj=a, IA=[1_I4B, 3_I4B, 4_I4B], &
!                 JA=[1_I4B, 2_I4B, 2_I4B])
!   CALL Initiate(obj=b, IA=[1_I4B, 2_I4B, 4_I4B], &
!                 JA=[2_I4B, 1_I4B, 2_I4B])
!   CALL OK(GetNNZ(obj1=a, obj2=b, op="+") .EQ. 4_I4B, "sorted: +")
!   CALL OK(GetNNZ(obj1=a, obj2=b, op="-") .EQ. 4_I4B, "sorted: -")
!   CALL OK(GetNNZ(obj1=a, obj2=b, op="+", isSorted=yes) .EQ. 4_I4B, &
!           "sorted: + with isSorted")
!   CALL OK(GetNNZ(obj1=a, obj2=b, op="-", isSorted=yes) .EQ. 4_I4B, &
!           "sorted: - with isSorted")
!   CALL DEALLOCATE (a)
!   CALL DEALLOCATE (b)
! END BLOCK
!
! ! Test 2: unsorted columns need the default isSorted
! BLOCK
!   TYPE(CSRSparsity_) :: a, b
!   CALL Initiate(obj=a, IA=[1_I4B, 4_I4B], &
!                 JA=[3_I4B, 1_I4B, 2_I4B])
!   CALL Initiate(obj=b, IA=[1_I4B, 3_I4B], JA=[2_I4B, 3_I4B])
!   CALL OK(GetNNZ(obj1=a, obj2=b, op="+") .EQ. 3_I4B, "unsorted: +")
!   CALL DEALLOCATE (a)
!   CALL DEALLOCATE (b)
! END BLOCK
!
! ! Test 3: disjoint patterns with empty rows
! BLOCK
!   TYPE(CSRSparsity_) :: a, b
!   CALL Initiate(obj=a, IA=[1_I4B, 1_I4B, 3_I4B], JA=[1_I4B, 2_I4B])
!   CALL Initiate(obj=b, IA=[1_I4B, 2_I4B, 2_I4B], JA=[2_I4B])
!   CALL OK(GetNNZ(obj1=a, obj2=b, op="+") .EQ. 3_I4B, "disjoint")
!   CALL OK(GetNNZ(obj1=a, obj2=b, op="+", isSorted=yes) .EQ. 3_I4B, &
!           "disjoint with isSorted")
!   CALL DEALLOCATE (a)
!   CALL DEALLOCATE (b)
! END BLOCK
!
! END PROGRAM main
!```
!
! See also: [[CSRSparsity_Method:GetNNZ_Add_Subtract]],
! [[CSRSparsity_Method:GetNNZ_Add_Subtract_sorted]]

INTERFACE GetNNZ
  MODULE PURE FUNCTION obj_GetNNZ_from_operation(obj1, obj2, op, isSorted) &
    RESULT(ans)
    TYPE(CSRSparsity_), INTENT(IN) :: obj1
    !! CSRSparsity object
    TYPE(CSRSparsity_), INTENT(IN) :: obj2
    !! CSRSparsity object
    CHARACTER(1), INTENT(IN) :: op
    !! "*", "+", "-"
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isSorted
    !! Set it to true if the columns are sorted in obj1 and obj2
    !! Default is .false.
    INTEGER(I4B) :: ans
    !! total number of non zero entries
  END FUNCTION obj_GetNNZ_from_operation
END INTERFACE GetNNZ

!----------------------------------------------------------------------------
!                                                          GetNNZ@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-12-16
! summary: Number of nonzeros in the sum of two CSR patterns
!
! This private helper counts the union of the two patterns row by row with
! a logical work array of size `ncol`. The columns may be unsorted.
!
! Returns:
!   ans : INTEGER(I4B) - nonzeros of the sum
!
! See also: [[CSRSparsity_Method:obj_GetNNZ_from_operation]]

INTERFACE
  MODULE PURE FUNCTION GetNNZ_Add_Subtract(nrow, ncol, ja, ia, jb, ib) &
    RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: nrow, ncol
    !! number of rows in a and b matrix
    INTEGER(I4B), INTENT(IN) :: ja(:)
    !! sparsity of ja
    INTEGER(I4B), INTENT(IN) :: ia(:)
    !! nrow + 1
    INTEGER(I4B), INTENT(IN) :: jb(:)
    !! sparsity of jb
    INTEGER(I4B), INTENT(IN) :: ib(:)
    !! nrow + 1
    INTEGER(I4B) :: ans
    !! number of nonzeros of the sum
  END FUNCTION GetNNZ_Add_Subtract
END INTERFACE

!----------------------------------------------------------------------------
!                                                          GetNNZ@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-12-16
! summary: Number of nonzeros in the sum of two sorted CSR patterns
!
! This private helper counts the union of the two patterns by merging the
! columns of each row. The columns of each row must be sorted.
!
! Returns:
!   ans : INTEGER(I4B) - nonzeros of the sum
!
! See also: [[CSRSparsity_Method:obj_GetNNZ_from_operation]]

INTERFACE
  MODULE PURE FUNCTION GetNNZ_Add_Subtract_sorted(nrow, ncol, ja, ia, jb, &
                                                  ib) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: nrow, ncol
    !! number of rows in a and b matrix
    INTEGER(I4B), INTENT(IN) :: ja(:)
    !! sparsity of ja
    INTEGER(I4B), INTENT(IN) :: ia(:)
    !! nrow + 1
    INTEGER(I4B), INTENT(IN) :: jb(:)
    !! sparsity of jb
    INTEGER(I4B), INTENT(IN) :: ib(:)
    !! nrow + 1
    INTEGER(I4B) :: ans
    !! number of nonzeros of the sum
  END FUNCTION GetNNZ_Add_Subtract_sorted
END INTERFACE

!----------------------------------------------------------------------------
!                                                          GetNNZ@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-01-28
! summary: Return the number of nonzeros in one part of the matrix
!
! `from` is not case sensitive. Any value other than U, L and D returns
! `obj%nnz`. The result is 0 when `obj` is not initiated.
!
! Returns:
!   ans : INTEGER(I4B) - entries in the requested part
!
! Examples
!
!```fortran
! INTEGER(I4B) :: nnzU
!
! nnzU = GetNNZ(obj=csr, from="U")
!```
!
! Full example: src/modules/CSRSparsity/examples/GetNNZ_test_1.F90
!
! See also: [[CSRSparsity_Method:obj_GetNNZ2]]

INTERFACE GetNNZ
  MODULE PURE FUNCTION obj_GetNNZ1(obj, from) RESULT(ans)
    TYPE(CSRSparsity_), INTENT(IN) :: obj
    !! sparsity pattern
    CHARACTER(1), INTENT(IN) :: from
    !! "U" nnz in upper triangular part, j > i
    !! "L" nnz in lower triangular part, i > j
    !! "D" nnz in diagonal part, i=j
    !! "A" nnz in whole matrix, L+U+D
    INTEGER(I4B) :: ans
    !! number of entries in the requested part
  END FUNCTION obj_GetNNZ1
END INTERFACE GetNNZ

!----------------------------------------------------------------------------
!                                                          GetNNZ@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-01-28
! summary: Return the number of nonzeros in U, L and D
!
! The result is 0 for all parts when `obj` is not initiated.
!
! Returns:
!   ans : INTEGER(I4B)(3) - [nnzU, nnzL, nnzD]
!
! Examples
!
!```fortran
! INTEGER(I4B) :: parts(3)
!
! parts = GetNNZ(obj=csr, from=[" "])
!```
!
! Full example: src/modules/CSRSparsity/examples/GetNNZ_test_1.F90
!
! See also: [[CSRSparsity_Method:obj_GetNNZ1]]

INTERFACE GetNNZ
  MODULE PURE FUNCTION obj_GetNNZ2(obj, from) RESULT(ans)
    TYPE(CSRSparsity_), INTENT(IN) :: obj
    !! sparsity pattern
    CHARACTER(1), INTENT(IN) :: from(1)
    !! this argument is not referred, it is here
    !! to create a unique interface only
    INTEGER(I4B) :: ans(3)
    !! [nnzU, nnzL, nnzD]
  END FUNCTION obj_GetNNZ2
END INTERFACE GetNNZ

!----------------------------------------------------------------------------
!                                                      GetDiagonal@GeMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 15 July 2021
! summary: Returns a diagonal of a sparse matrix and its positions
!
! This subroutine returns the entries `a(i, j)` with `j - i = offSet`,
! using `GetDIA` of Saad's sparse library. So `offSet = 0` means the main
! diagonal. The object is not changed.
!
! `diag` and `idiag` are reallocated to size `nrow`. If `idiag(i)` is 0,
! then row `i` has no entry on this diagonal and `diag(i)` is zero.
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: CSRSparsity_
! USE CSRSparsity_Method, ONLY: DEALLOCATE
! USE CSRSparsity_Method, ONLY: GetDiagonal
! USE CSRSparsity_Method, ONLY: Initiate
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP) :: A(6)
! INTEGER(I4B), PARAMETER :: IA(4) = [1, 3, 5, 7]
! INTEGER(I4B), PARAMETER :: JA(6) = [1, 3, 1, 3, 2, 3]
!
! A = [10.0_DFP, 20.0_DFP, 30.0_DFP, 40.0_DFP, 50.0_DFP, 60.0_DFP]
!
! ! Test 1: diag and idiag
! BLOCK
!   TYPE(CSRSparsity_) :: csr
!   REAL(DFP), ALLOCATABLE :: diag(:)
!   INTEGER(I4B), ALLOCATABLE :: idiag(:)
!   CALL Initiate(obj=csr, IA=IA, JA=JA)
!
!   CALL GetDiagonal(obj=csr, A=A, diag=diag, idiag=idiag)
!   CALL OK(ALL(ABS(diag - [10.0_DFP, 0.0_DFP, 60.0_DFP]) .LT. tol), &
!           "main: diag")
!   CALL OK(ALL(idiag .EQ. [1, 0, 6]), "main: idiag")
!
!   CALL GetDiagonal(obj=csr, A=A, diag=diag, idiag=idiag, offSet=1_I4B)
!   CALL OK(ALL(ABS(diag - [0.0_DFP, 40.0_DFP, 0.0_DFP]) .LT. tol), &
!           "offSet 1: diag")
!   CALL OK(ALL(idiag .EQ. [0, 4, 0]), "offSet 1: idiag")
!
!   CALL GetDiagonal(obj=csr, A=A, diag=diag, idiag=idiag, offSet=-1_I4B)
!   CALL OK(ALL(ABS(diag - [0.0_DFP, 30.0_DFP, 50.0_DFP]) .LT. tol), &
!           "offSet -1: diag")
!   CALL OK(ALL(idiag .EQ. [0, 3, 5]), "offSet -1: idiag")
!   CALL DEALLOCATE (csr)
! END BLOCK
!
! ! Test 2: the first call stores the positions in the object
! BLOCK
!   TYPE(CSRSparsity_) :: csr
!   REAL(DFP), ALLOCATABLE :: diag(:)
!   INTEGER(I4B), PARAMETER :: IA1(4) = [1, 3, 4, 6]
!   INTEGER(I4B), PARAMETER :: JA1(5) = [1, 3, 2, 1, 3]
!   REAL(DFP) :: A1(5)
!   A1 = [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP]
!   CALL Initiate(obj=csr, IA=IA1, JA=JA1)
!   CALL OK(.NOT. csr%isDiagStored, "stored: not before first call")
!
!   CALL GetDiagonal(obj=csr, A=A1, diag=diag)
!   CALL OK(csr%isDiagStored, "stored: after first call")
!   CALL OK(ALL(csr%idiag .EQ. [1, 3, 5]), "stored: idiag")
!   CALL OK(ALL(ABS(diag - [1.0_DFP, 3.0_DFP, 5.0_DFP]) .LT. tol), &
!           "stored: diag, first call")
!
!   A1 = 2.0_DFP * A1
!   CALL GetDiagonal(obj=csr, A=A1, diag=diag)
!   CALL OK(ALL(ABS(diag - [2.0_DFP, 6.0_DFP, 10.0_DFP]) .LT. tol), &
!           "stored: diag, second call")
!   CALL DEALLOCATE (csr)
! END BLOCK
!
! ! Test 3: another diagonal, and a row with no entry
! BLOCK
!   TYPE(CSRSparsity_) :: csr
!   REAL(DFP), ALLOCATABLE :: diag(:)
!   CALL Initiate(obj=csr, IA=IA, JA=JA)
!   CALL GetDiagonal(obj=csr, A=A, diag=diag, offSet=-1_I4B)
!   CALL GetDiagonal(obj=csr, A=A, diag=diag, offSet=1_I4B)
!   CALL OK(ALL(ABS(diag - [0.0_DFP, 40.0_DFP, 0.0_DFP]) .LT. tol), &
!           "second offSet: diag")
!   CALL OK(.NOT. csr%isDiagStored, "second offSet: nothing is stored")
!   CALL GetDiagonal(obj=csr, A=A, diag=diag)
!   CALL GetDiagonal(obj=csr, A=A, diag=diag)
!   CALL OK(ALL(ABS(diag - [10.0_DFP, 0.0_DFP, 60.0_DFP]) .LT. tol), &
!           "stored: row with no entry")
!   CALL DEALLOCATE (csr)
! END BLOCK
!
! END PROGRAM main
!```
!
! See also: [[CSRSparsity_Method:obj_GetDiagonal2]]

INTERFACE GetDiagonal
  MODULE SUBROUTINE obj_GetDiagonal1(obj, A, diag, idiag, offSet)
    TYPE(CSRSparsity_), INTENT(IN) :: obj
    !! sparsity pattern of the matrix
    REAL(DFP), INTENT(IN) :: A(:)
    !! sparse matrix values, SIZE(A) = obj%nnz
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: diag(:)
    !! Diagonal entries
    INTEGER(I4B), ALLOCATABLE, INTENT(INOUT) :: idiag(:)
    !! Position of diagonal entries in `A(:)`
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: offSet
    !! offSet of the wanted diagonal
  END SUBROUTINE obj_GetDiagonal1
END INTERFACE GetDiagonal

!----------------------------------------------------------------------------
!                                                    GetDiagonal@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 15 July 2021
! summary: Returns a diagonal of a sparse matrix, storing its positions
!
! This routine is similar to [[CSRSparsity_Method:obj_GetDiagonal1]], but
! it does not return the positions. For the main diagonal it keeps them
! in `obj%idiag`: the first call finds them and sets `obj%isDiagStored`,
! and later calls only read `A(obj%idiag(i))`, which is faster for
! matrices with the same pattern.
!
! Any other `offSet` is searched on every call and leaves `obj%idiag` and
! `obj%isDiagStored` unchanged. `diag(i)` is zero when row `i` has no
! entry on the diagonal.
!
! Changes:
!   - 2026-11-05: Only the main diagonal is stored, so offSet is honored
!     on every call. Rows with no entry give zero on later calls.
!
! Examples
!
!```fortran
! REAL(DFP), ALLOCATABLE :: diag(:)
!
! CALL GetDiagonal(obj=csr, A=A, diag=diag)
!```
!
! Full example: src/modules/CSRSparsity/examples/GetDiagonal_test_1.F90
!
! See also: [[CSRSparsity_Method:obj_GetDiagonal1]]

INTERFACE GetDiagonal
  MODULE SUBROUTINE obj_GetDiagonal2(obj, A, diag, offSet)
    TYPE(CSRSparsity_), INTENT(INOUT) :: obj
    !! sparsity pattern, the main diagonal is stored in obj%idiag
    REAL(DFP), INTENT(IN) :: A(:)
    !! Sparse matrix values
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: diag(:)
    !! Diagonal entries
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: offSet
    !! offSet of diagonal
  END SUBROUTINE obj_GetDiagonal2
END INTERFACE GetDiagonal

!----------------------------------------------------------------------------
!                                                   GetColNumber@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-12-14
! summary: Get the column number from JA
!
! This function returns `obj%JA(indx)`, the same as
! [[CSRSparsity_Method:obj_GetJA]].
!
! Returns:
!   ans : INTEGER(I4B) - column number
!
! Examples
!
!```fortran
! INTEGER(I4B) :: jcol
!
! jcol = GetColNumber(obj=csr, indx=2_I4B)
!```
!
! Full example: src/modules/CSRSparsity/examples/GetColIndex_test_1.F90
!
! See also: [[CSRSparsity_Method:obj_GetColIndex1]]

INTERFACE GetColNumber
  MODULE PURE FUNCTION obj_GetColNumber1(obj, indx) RESULT(ans)
    TYPE(CSRSparsity_), INTENT(IN) :: obj
    !! sparsity pattern
    INTEGER(I4B), INTENT(IN) :: indx
    !! position in JA, from 1 to obj%nnz
    INTEGER(I4B) :: ans
    !! column number
  END FUNCTION obj_GetColNumber1
END INTERFACE GetColNumber

!----------------------------------------------------------------------------
!                                                     GetColIndex@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-12-14
! summary: Get the first and last position of a row in JA
!
! This function returns `[IA(irow), IA(irow + 1) - 1]`. The second entry
! is smaller than the first one when the row is empty, so a `DO` loop
! over this range is safe.
!
! Returns:
!   ans : INTEGER(I4B)(2) - first and last position in JA
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: CSRSparsity_
! USE CSRSparsity_Method, ONLY: CsrShape => Shape
! USE CSRSparsity_Method, ONLY: CsrSize => Size
! USE CSRSparsity_Method, ONLY: DEALLOCATE
! USE CSRSparsity_Method, ONLY: GetColIndex
! USE CSRSparsity_Method, ONLY: GetColNumber
! USE CSRSparsity_Method, ONLY: GetIA
! USE CSRSparsity_Method, ONLY: GetJA
! USE CSRSparsity_Method, ONLY: Initiate
! USE CSRSparsity_Method, ONLY: OPERATOR(.endColumn.)
! USE CSRSparsity_Method, ONLY: OPERATOR(.startColumn.)
! USE CSRSparsity_Method, ONLY: SetIA
! USE CSRSparsity_Method, ONLY: SetJA
! USE GlobalData, ONLY: I4B
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! TYPE(CSRSparsity_) :: csr
!
! CALL Initiate(obj=csr, IA=[1_I4B, 3_I4B, 3_I4B, 4_I4B], &
!               JA=[2_I4B, 3_I4B, 1_I4B], ncol=4_I4B)
!
! ! Test 1: Shape and Size
! BLOCK
!   CALL OK(ALL(CsrShape(csr) .EQ. [3, 4]), "Shape")
!   CALL OK(CsrSize(csr, 1_I4B) .EQ. 3_I4B, "Size: Dims=1")
!   CALL OK(CsrSize(csr, 2_I4B) .EQ. 4_I4B, "Size: Dims=2")
!   CALL OK(CsrSize(csr) .EQ. 12_I4B, "Size: no Dims")
! END BLOCK
!
! ! Test 2: range of a row with columns
! BLOCK
!   CALL OK(ALL(GetColIndex(obj=csr, irow=1_I4B) .EQ. [1, 2]), &
!           "GetColIndex: row 1")
!   CALL OK((csr.startColumn.1_I4B) .EQ. 1_I4B, ".startColumn.: row 1")
!   CALL OK((csr.endColumn.1_I4B) .EQ. 2_I4B, ".endColumn.: row 1")
! END BLOCK
!
! ! Test 3: the end of an empty row comes before its start
! BLOCK
!   CALL OK(ALL(GetColIndex(obj=csr, irow=2_I4B) .EQ. [3, 2]), &
!           "GetColIndex: empty row")
!   CALL OK((csr.startColumn.2_I4B) .EQ. 3_I4B, ".startColumn.: empty row")
!   CALL OK((csr.endColumn.2_I4B) .EQ. 2_I4B, ".endColumn.: empty row")
! END BLOCK
!
! ! Test 4: entries of IA and JA
! BLOCK
!   CALL OK(GetIA(obj=csr, irow=4_I4B) .EQ. 4_I4B, "GetIA: last entry")
!   CALL OK(GetJA(obj=csr, indx=3_I4B) .EQ. 1_I4B, "GetJA")
!   CALL OK(GetColNumber(obj=csr, indx=2_I4B) .EQ. 3_I4B, "GetColNumber")
! END BLOCK
!
! ! Test 5: SetIA and SetJA
! BLOCK
!   CALL SetIA(obj=csr, irow=2_I4B, VALUE=2_I4B)
!   CALL OK(GetIA(obj=csr, irow=2_I4B) .EQ. 2_I4B, "SetIA")
!   CALL SetJA(obj=csr, indx=1_I4B, VALUE=4_I4B)
!   CALL OK(GetJA(obj=csr, indx=1_I4B) .EQ. 4_I4B, "SetJA")
! END BLOCK
!
! CALL DEALLOCATE (csr)
!
! END PROGRAM main
!```
!
! See also: [[CSRSparsity_Method:obj_startColumn1]],
! [[CSRSparsity_Method:obj_endColumn1]]

INTERFACE GetColIndex
  MODULE PURE FUNCTION obj_GetColIndex1(obj, irow) RESULT(ans)
    TYPE(CSRSparsity_), INTENT(IN) :: obj
    !! sparsity pattern
    INTEGER(I4B), INTENT(IN) :: irow
    !! row number, from 1 to obj%nrow
    INTEGER(I4B) :: ans(2)
    !! first and last position in JA
  END FUNCTION obj_GetColIndex1
END INTERFACE GetColIndex

!----------------------------------------------------------------------------
!                                                     startColumn@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-12-14
! summary: Get the first position of a row in JA
!
! This operator returns `obj%IA(irow)`.
!
! Returns:
!   ans : INTEGER(I4B) - first position in JA
!
! Examples
!
!```fortran
! INTEGER(I4B) :: ii
!
! ii = csr.startColumn.1_I4B
!```
!
! Full example: src/modules/CSRSparsity/examples/GetColIndex_test_1.F90
!
! See also: [[CSRSparsity_Method:obj_GetColIndex1]]

INTERFACE OPERATOR(.startColumn.)
  MODULE PURE FUNCTION obj_startColumn1(obj, irow) RESULT(ans)
    TYPE(CSRSparsity_), INTENT(IN) :: obj
    !! sparsity pattern
    INTEGER(I4B), INTENT(IN) :: irow
    !! row number, from 1 to obj%nrow
    INTEGER(I4B) :: ans
    !! first position in JA
  END FUNCTION obj_startColumn1
END INTERFACE OPERATOR(.startColumn.)

!----------------------------------------------------------------------------
!                                                        endColumn@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-12-14
! summary: Get the last position of a row in JA
!
! This operator returns `obj%IA(irow + 1) - 1`. For an empty row it is
! one less than `.startColumn.` of that row.
!
! Returns:
!   ans : INTEGER(I4B) - last position in JA
!
! Examples
!
!```fortran
! INTEGER(I4B) :: ii
!
! ii = csr.endColumn.1_I4B
!```
!
! Full example: src/modules/CSRSparsity/examples/GetColIndex_test_1.F90
!
! See also: [[CSRSparsity_Method:obj_GetColIndex1]]

INTERFACE OPERATOR(.endColumn.)
  MODULE PURE FUNCTION obj_endColumn1(obj, irow) RESULT(ans)
    TYPE(CSRSparsity_), INTENT(IN) :: obj
    !! sparsity pattern
    INTEGER(I4B), INTENT(IN) :: irow
    !! row number, from 1 to obj%nrow
    INTEGER(I4B) :: ans
    !! last position in JA
  END FUNCTION obj_endColumn1
END INTERFACE OPERATOR(.endColumn.)

!----------------------------------------------------------------------------
!                                                     SetSparsity@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 22 March 2021
! summary: Add the column nodes of one row node to the sparsity pattern
!
! This routine adds the coupling between the row node `row` and the
! column nodes `col` to the sparsity pattern that is being built.
!
! The node numbers are first converted to matrix indices. Every degree of
! freedom of `row` in `obj%idof` gives one matrix row, and every degree of
! freedom of each entry of `col` in `obj%jdof` gives one matrix column.
! Both conversions follow the storage format (`FMT_NODES` or `FMT_DOF`)
! of the DOF objects. The list of column indices is then appended to
! `obj%row(i)` for each of these matrix rows.
!
! The appended indices are neither sorted nor made unique, and repeated
! calls for the same row only append more entries. `obj%nnz` grows by
! the number of appended entries, so it is an upper bound at this
! stage. Call `SetSparsity(obj)` after the last call. It sorts each row,
! removes the duplicates, and builds `obj%IA`, `obj%JA` and the exact
! `obj%nnz`.
!
! If `col` is empty, only the row storage is allocated. In debug builds
! the routine stops when `obj` is not Initiated or the sparsity pattern
! is locked.
!
! Changes:
!   - 2026-11-03: Row and column indices are no longer sorted here, since
!     SetSparsity(obj) sorts each row anyway.
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: CSRSparsity_
! USE BaseType, ONLY: DOF_
! USE CSRSparsity_Method, ONLY: Initiate
! USE CSRSparsity_Method, ONLY: SetSparsity
! USE CSRSparsity_Method, ONLY: DEALLOCATE
! USE DOF_Method, ONLY: Initiate
! USE DOF_Method, ONLY: DEALLOCATE
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: FMT_NODES
! USE GlobalData, ONLY: FMT_DOF
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B) :: empty(0)
!
! ! Test 1: scalar DOF, unsorted and repeated col, one empty row
! BLOCK
!   TYPE(CSRSparsity_) :: csr
!   CALL Initiate(obj=csr, ncol=4_I4B, nrow=4_I4B)
!   CALL SetSparsity(obj=csr, row=1_I4B, col=[3_I4B, 1_I4B, 1_I4B])
!   CALL SetSparsity(obj=csr, row=2_I4B, col=[2_I4B, 4_I4B])
!   CALL SetSparsity(obj=csr, row=1_I4B, col=[2_I4B, 3_I4B])
!   CALL SetSparsity(obj=csr, row=3_I4B, col=empty)
!   CALL SetSparsity(obj=csr, row=4_I4B, col=[4_I4B, 1_I4B, 4_I4B])
!   CALL SetSparsity(csr)
!   CALL OK(csr%nnz .EQ. 7_I4B, "scalar: nnz")
!   CALL OK(ALL(csr%IA .EQ. [1, 4, 6, 6, 8]), "scalar: IA")
!   CALL OK(ALL(csr%JA .EQ. [1, 2, 3, 2, 4, 1, 4]), "scalar: JA")
!   CALL DEALLOCATE (csr)
! END BLOCK
!
! ! Test 2: 3 nodes, 2 components, FMT_NODES
! BLOCK
!   TYPE(CSRSparsity_) :: csr
!   TYPE(DOF_) :: dof
!   CALL Initiate(obj=dof, tNodes=[3_I4B], names=["U"], &
!                 spacecompo=[2_I4B], timecompo=[1_I4B], &
!                 storagefmt=FMT_NODES)
!   CALL Initiate(obj=csr, ncol=6_I4B, nrow=6_I4B, idof=dof, jdof=dof)
!   CALL SetSparsity(obj=csr, row=1_I4B, col=[2_I4B, 1_I4B])
!   CALL SetSparsity(obj=csr, row=3_I4B, col=[3_I4B])
!   CALL SetSparsity(obj=csr, row=1_I4B, col=[1_I4B, 3_I4B])
!   CALL SetSparsity(csr)
!   CALL OK(csr%nnz .EQ. 16_I4B, "FMT_NODES: nnz")
!   CALL OK(ALL(csr%IA .EQ. [1, 7, 13, 13, 13, 15, 17]), "FMT_NODES: IA")
!   CALL OK(ALL(csr%JA .EQ. [1, 2, 3, 4, 5, 6, 1, 2, 3, 4, 5, 6, &
!                            5, 6, 5, 6]), "FMT_NODES: JA")
!   CALL DEALLOCATE (csr)
!   CALL DEALLOCATE (dof)
! END BLOCK
!
! ! Test 3: 3 nodes, 2 components, FMT_DOF
! BLOCK
!   TYPE(CSRSparsity_) :: csr
!   TYPE(DOF_) :: dof
!   CALL Initiate(obj=dof, tNodes=[3_I4B], names=["U"], &
!                 spacecompo=[2_I4B], timecompo=[1_I4B], &
!                 storagefmt=FMT_DOF)
!   CALL Initiate(obj=csr, ncol=6_I4B, nrow=6_I4B, idof=dof, jdof=dof)
!   CALL SetSparsity(obj=csr, row=1_I4B, col=[2_I4B, 1_I4B])
!   CALL SetSparsity(obj=csr, row=3_I4B, col=[3_I4B])
!   CALL SetSparsity(obj=csr, row=1_I4B, col=[1_I4B, 3_I4B])
!   CALL SetSparsity(csr)
!   CALL OK(csr%nnz .EQ. 16_I4B, "FMT_DOF: nnz")
!   CALL OK(ALL(csr%IA .EQ. [1, 7, 7, 9, 15, 15, 17]), "FMT_DOF: IA")
!   CALL OK(ALL(csr%JA .EQ. [1, 2, 3, 4, 5, 6, 3, 6, &
!                            1, 2, 3, 4, 5, 6, 3, 6]), "FMT_DOF: JA")
!   CALL DEALLOCATE (csr)
!   CALL DEALLOCATE (dof)
! END BLOCK
!
! END PROGRAM main
!```
!
! See also: [[CSRSparsity_Method:obj_SetSparsity2]],
! [[CSRSparsity_Method:obj_SetSparsity_final]]

INTERFACE SetSparsity
  MODULE SUBROUTINE obj_SetSparsity1(obj, row, col)
    TYPE(CSRSparsity_), INTENT(INOUT) :: obj
    !! sparsity pattern, Initiated and not locked
    INTEGER(I4B), INTENT(IN) :: row
    !! row node number, in the node numbering of obj%idof
    INTEGER(I4B), INTENT(IN) :: col(:)
    !! column node numbers connected to row, in the node numbering of
    !! obj%jdof; may be unsorted, contain repeats, or be empty
  END SUBROUTINE obj_SetSparsity1
END INTERFACE SetSparsity

!----------------------------------------------------------------------------
!                                                     SetSparsity@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 22 March 2021
! summary: Add the column nodes of several row nodes to the sparsity
!   pattern
!
! This routine adds the coupling between each row node `row(i)` and the
! column nodes `col(i)%val` to the sparsity pattern that is being built.
! It calls [[CSRSparsity_Method:obj_SetSparsity1]] once for every `i`, so
! the conversion of node numbers to matrix indices through `obj%idof` and
! `obj%jdof` is the same as in that routine.
!
! Rows may appear more than once in `row` and in any order, and each
! `col(i)` may be unsorted, contain repeats, or have Size zero. Call
! `SetSparsity(obj)` after the last call to sort the rows, remove the
! duplicates, and build `obj%IA`, `obj%JA` and `obj%nnz`.
!
! Only `col(i)%val(1:col(i)%tsize)` is read. Set each `col(i)` with the
! `IntVector_` assignment, as in the example, since filling `col(i)%val`
! directly leaves `tsize` at 0 and the entry adds nothing. In debug
! builds the routine stops when the Sizes of `row` and `col` differ.
!
! Examples
!
!```fortran
! TYPE(CSRSparsity_) :: csr
! TYPE(IntVector_) :: col(3)
!
! col(1) = [2_I4B, 1_I4B]
! col(2) = [3_I4B]
! col(3) = [1_I4B, 3_I4B]
! CALL Initiate(obj=csr, ncol=3_I4B, nrow=3_I4B)
! CALL SetSparsity(obj=csr, row=[1_I4B, 3_I4B, 1_I4B], col=col)
! CALL SetSparsity(csr)
! ! csr%IA = [1, 4, 4, 5], csr%JA = [1, 2, 3, 3]
!```
!
! Full example: src/modules/CSRSparsity/examples/SetSparsity_test_2.F90
!
! See also: [[CSRSparsity_Method:obj_SetSparsity1]],
! [[CSRSparsity_Method:obj_SetSparsity_final]]

INTERFACE SetSparsity
  MODULE SUBROUTINE obj_SetSparsity2(obj, row, col)
    TYPE(CSRSparsity_), INTENT(INOUT) :: obj
    !! sparsity pattern, Initiated and not locked
    INTEGER(I4B), INTENT(IN) :: row(:)
    !! row node numbers, in the node numbering of obj%idof
    TYPE(IntVector_), INTENT(IN) :: col(:)
    !! col(i)%val(1:col(i)%tsize) are the column node numbers connected
    !! to row(i), in the node numbering of obj%jdof;
    !! SIZE(col) = SIZE(row)
  END SUBROUTINE obj_SetSparsity2
END INTERFACE SetSparsity

!----------------------------------------------------------------------------
!                                                     SetSparsity@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 10 Oct 2021
! summary: Add the column nodes of one row node to a block of the sparsity
!   pattern
!
! This routine works like [[CSRSparsity_Method:obj_SetSparsity1]], but
! only for one block of a DOF with several physical variables. The row
! node `row` belongs to variable `ivar` of `obj%idof`, and the column
! nodes `col` belong to variable `jvar` of `obj%jdof`.
!
! Both DOF objects must use the storage format `FMT_DOF`. In debug builds
! the routine stops when they do not, and when `obj` is not Initiated or
! the sparsity pattern is locked.
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: CSRSparsity_
! USE BaseType, ONLY: DOF_
! USE CSRSparsity_Method, ONLY: Initiate
! USE CSRSparsity_Method, ONLY: SetSparsity
! USE CSRSparsity_Method, ONLY: DEALLOCATE
! USE DOF_Method, ONLY: Initiate
! USE DOF_Method, ONLY: DEALLOCATE
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: FMT_DOF
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B) :: empty(0)
!
! ! Test 1: two variables, all four blocks
! BLOCK
!   TYPE(CSRSparsity_) :: csr
!   TYPE(DOF_) :: dof
!   CALL Initiate(obj=dof, tNodes=[3_I4B, 2_I4B], names=["V", "P"], &
!                 spacecompo=[2_I4B, 1_I4B], timecompo=[1_I4B, 1_I4B], &
!                 storagefmt=FMT_DOF)
!   CALL Initiate(obj=csr, ncol=8_I4B, nrow=8_I4B, idof=dof, jdof=dof)
!   ! V node 1 with P nodes 2, 1: rows 1, 4 and columns 8, 7
!   CALL SetSparsity(obj=csr, row=1_I4B, col=[2_I4B, 1_I4B], &
!                    ivar=1_I4B, jvar=2_I4B)
!   ! P node 2 with V node 3: row 8 and columns 3, 6
!   CALL SetSparsity(obj=csr, row=2_I4B, col=[3_I4B], &
!                    ivar=2_I4B, jvar=1_I4B)
!   ! V node 1 with V node 1 (repeated): rows 1, 4 and columns 1, 4
!   CALL SetSparsity(obj=csr, row=1_I4B, col=[1_I4B, 1_I4B], &
!                    ivar=1_I4B, jvar=1_I4B)
!   ! P node 1 with no columns: nothing added
!   CALL SetSparsity(obj=csr, row=1_I4B, col=empty, &
!                    ivar=2_I4B, jvar=2_I4B)
!   CALL SetSparsity(csr)
!   CALL OK(csr%nnz .EQ. 10_I4B, "two variables: nnz")
!   CALL OK(ALL(csr%IA .EQ. [1, 5, 5, 5, 9, 9, 9, 9, 11]), &
!           "two variables: IA")
!   CALL OK(ALL(csr%JA .EQ. [1, 4, 7, 8, 1, 4, 7, 8, 3, 6]), &
!           "two variables: JA")
!   CALL DEALLOCATE (csr)
!   CALL DEALLOCATE (dof)
! END BLOCK
!
! ! Test 2: one variable, ivar = jvar = 1 equals SetSparsity(obj, row, col)
! BLOCK
!   TYPE(CSRSparsity_) :: csr1, csr2
!   TYPE(DOF_) :: dof
!   CALL Initiate(obj=dof, tNodes=[3_I4B], names=["U"], &
!                 spacecompo=[2_I4B], timecompo=[1_I4B], &
!                 storagefmt=FMT_DOF)
!   CALL Initiate(obj=csr1, ncol=6_I4B, nrow=6_I4B, idof=dof, jdof=dof)
!   CALL Initiate(obj=csr2, ncol=6_I4B, nrow=6_I4B, idof=dof, jdof=dof)
!   CALL SetSparsity(obj=csr1, row=1_I4B, col=[2_I4B, 1_I4B], &
!                    ivar=1_I4B, jvar=1_I4B)
!   CALL SetSparsity(obj=csr1, row=3_I4B, col=[3_I4B], &
!                    ivar=1_I4B, jvar=1_I4B)
!   CALL SetSparsity(obj=csr2, row=1_I4B, col=[2_I4B, 1_I4B])
!   CALL SetSparsity(obj=csr2, row=3_I4B, col=[3_I4B])
!   CALL SetSparsity(csr1)
!   CALL SetSparsity(csr2)
!   CALL OK(csr1%nnz .EQ. csr2%nnz, "one variable: nnz equals row-wise")
!   CALL OK(ALL(csr1%IA .EQ. csr2%IA), "one variable: IA equals row-wise")
!   CALL OK(ALL(csr1%JA .EQ. csr2%JA), "one variable: JA equals row-wise")
!   CALL DEALLOCATE (csr1)
!   CALL DEALLOCATE (csr2)
!   CALL DEALLOCATE (dof)
! END BLOCK
!
! END PROGRAM main
! ! ok 1 - two variables: nnz
! ! ok 2 - two variables: IA
! ! ok 3 - two variables: JA
! ! ok 4 - one variable: nnz equals row-wise
! ! ok 5 - one variable: IA equals row-wise
! ! ok 6 - one variable: JA equals row-wise
!```
!
! See also: [[CSRSparsity_Method:obj_SetSparsity4]],
! [[CSRSparsity_Method:obj_SetSparsity_final]]

INTERFACE SetSparsity
  MODULE SUBROUTINE obj_SetSparsity3(obj, row, col, ivar, jvar)
    TYPE(CSRSparsity_), INTENT(INOUT) :: obj
    !! sparsity pattern, Initiated and not locked
    INTEGER(I4B), INTENT(IN) :: row
    !! row number
    INTEGER(I4B), INTENT(IN) :: col(:)
    !! sparsity of row, column numbers
    INTEGER(I4B), INTENT(IN) :: ivar
    !! block address (row index)
    INTEGER(I4B), INTENT(IN) :: jvar
    !! block address (col index)
  END SUBROUTINE obj_SetSparsity3
END INTERFACE SetSparsity

!----------------------------------------------------------------------------
!                                                     SetSparsity@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 22 March 2021
! summary: Add the column nodes of several row nodes to a block of the
!   sparsity pattern
!
! This routine works like [[CSRSparsity_Method:obj_SetSparsity2]], but
! only for one block of a DOF with several physical variables, see
! [[CSRSparsity_Method:obj_SetSparsity3]]. Both DOF objects must use the
! storage format `FMT_DOF`.
!
! Rows may appear more than once in `row` and in any order, and each
! `col(i)%val` may be unsorted or contain repeats. In debug builds the
! routine also stops when the sizes of `row` and `col` differ.
!
! Examples
!
!```fortran
! TYPE(IntVector_) :: col(2)
!
! col(1) = [2_I4B, 1_I4B]
! col(2) = [3_I4B]
! CALL SetSparsity(obj=csr, row=[1_I4B, 2_I4B], col=col, &
!                  ivar=1_I4B, jvar=2_I4B)
! CALL SetSparsity(csr)
!```
!
! Only `col(i)%val(1:col(i)%tsize)` is read, so set each `col(i)` with
! the `IntVector_` assignment.
!
! Full example: src/modules/CSRSparsity/examples/SetSparsity_test_9.F90
!
! See also: [[CSRSparsity_Method:obj_SetSparsity3]]

INTERFACE SetSparsity
  MODULE SUBROUTINE obj_SetSparsity4(obj, row, col, ivar, jvar)
    TYPE(CSRSparsity_), INTENT(INOUT) :: obj
    !! sparsity pattern, Initiated and not locked
    INTEGER(I4B), INTENT(IN) :: row(:)
    !! several row numbers
    TYPE(IntVector_), INTENT(IN) :: col(:)
    !! col(i)%val(1:col(i)%tsize) are the column node numbers of row(i)
    INTEGER(I4B), INTENT(IN) :: ivar
    !! block address (row index)
    INTEGER(I4B), INTENT(IN) :: jvar
    !! block address (col index)
  END SUBROUTINE obj_SetSparsity4
END INTERFACE SetSparsity

!----------------------------------------------------------------------------
!                                                     SetSparsity@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 22 March 2021
! summary: Set the sparsity pattern from an integer graph
!
! This routine adds the coupling between row node `i` and column node `j`
! for every nonzero `graph(i, j)`. It builds the column list of each row
! and calls [[CSRSparsity_Method:obj_SetSparsity2]], so the graph is in
! terms of nodes and the DOF objects expand it to matrix indices.
!
! Call `SetSparsity(obj)` after the last call.
!
! Examples
!
!```fortran
! INTEGER(I4B) :: graph(3, 3)
!
! graph = 0
! graph(1, 2) = 1
! CALL SetSparsity(obj=csr, graph=graph)
! CALL SetSparsity(csr)
!```
!
! Full example: src/modules/CSRSparsity/examples/SetSparsity_test_10.F90
!
! See also: [[CSRSparsity_Method:obj_SetSparsity6]]

INTERFACE SetSparsity
  MODULE SUBROUTINE obj_SetSparsity5(obj, graph)
    TYPE(CSRSparsity_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: graph(:, :)
  !! graph of sparsity
  !! If graph( i, j ) .EQ. 0, then i and j are not connected
  !! else they are connected.
  END SUBROUTINE obj_SetSparsity5
END INTERFACE SetSparsity

!----------------------------------------------------------------------------
!                                                     SetSparsity@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 22 March 2021
! summary: Set the sparsity pattern from a logical graph
!
! This routine works like [[CSRSparsity_Method:obj_SetSparsity5]]. Here
! row node `i` and column node `j` are connected when `graph(i, j)` is
! true.
!
! Examples
!
!```fortran
! LOGICAL(LGT) :: graph(3, 3)
!
! graph = .FALSE.
! graph(1, 2) = .TRUE.
! CALL SetSparsity(obj=csr, graph=graph)
! CALL SetSparsity(csr)
!```
!
! Full example: src/modules/CSRSparsity/examples/SetSparsity_test_11.F90
!
! See also: [[CSRSparsity_Method:obj_SetSparsity5]]

INTERFACE SetSparsity
  MODULE SUBROUTINE obj_SetSparsity6(obj, graph)
    TYPE(CSRSparsity_), INTENT(INOUT) :: obj
    LOGICAL(LGT), INTENT(IN) :: graph(:, :)
  !! graph of sparsity
  !! If graph( i, j ) .EQ. FALSE, then i and j are not connected
  !! else they are connected.
  END SUBROUTINE obj_SetSparsity6
END INTERFACE SetSparsity

!----------------------------------------------------------------------------
!                                                     SetSparsity@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 22 March 2021
! summary: Finish the sparsity pattern and build IA and JA
!
! Call this routine once, after the last `SetSparsity` call that adds
! entries. It sorts each row, removes the duplicates, sets the exact
! `obj%nnz`, builds `obj%IA` and `obj%JA`, frees the row storage
! `obj%row`, and sets `obj%isSparsityLock`. It does not set `obj%idiag`
! or `obj%isSorted`.
!
! A locked object is left unchanged, with a warning in debug builds. An
! object with no row storage, such as one initiated from `IA` and `JA`,
! keeps `IA` and `JA` and only gets locked. In debug builds the routine
! stops when `obj` is not Initiated.
!
! Examples
!
!```fortran
! CALL Initiate(obj=csr, ncol=3_I4B, nrow=3_I4B)
! CALL SetSparsity(obj=csr, row=1_I4B, col=[3_I4B, 1_I4B])
! CALL SetSparsity(csr)
!```
!
! Full example: src/modules/CSRSparsity/examples/SetSparsity_test_3.F90
!
! See also: [[CSRSparsity_Method:obj_SetSparsity1]]

INTERFACE SetSparsity
  MODULE SUBROUTINE obj_SetSparsity_final(obj)
    TYPE(CSRSparsity_), INTENT(INOUT) :: obj
    !! sparsity pattern, Initiated
  END SUBROUTINE obj_SetSparsity_final
END INTERFACE SetSparsity

!----------------------------------------------------------------------------
!                                                        SetIA@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-12-14
! summary: Set an entry of IA
!
! This routine sets `obj%IA(irow)` with no check. The caller must keep `IA`
! and `JA` consistent with each other.
!
! Examples
!
!```fortran
! CALL SetIA(obj=csr, irow=1_I4B, VALUE=2_I4B)
!```
!
! Full example: src/modules/CSRSparsity/examples/GetColIndex_test_1.F90
!
! See also: [[CSRSparsity_Method:obj_GetIA]]

INTERFACE SetIA
  MODULE PURE SUBROUTINE obj_SetIA(obj, irow, VALUE)
    TYPE(CSRSparsity_), INTENT(INOUT) :: obj
    !! sparsity pattern
    INTEGER(I4B), INTENT(IN) :: irow
    !! position in IA, from 1 to obj%nrow + 1
    INTEGER(I4B), INTENT(IN) :: VALUE
    !! new value
  END SUBROUTINE obj_SetIA
END INTERFACE SetIA

!----------------------------------------------------------------------------
!                                                        SetJA@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-12-14
! summary: Set an entry of JA
!
! This routine sets `obj%JA(indx)` with no check. The caller must keep `IA`
! and `JA` consistent with each other.
!
! Examples
!
!```fortran
! CALL SetJA(obj=csr, indx=1_I4B, VALUE=2_I4B)
!```
!
! Full example: src/modules/CSRSparsity/examples/GetColIndex_test_1.F90
!
! See also: [[CSRSparsity_Method:obj_GetJA]]

INTERFACE SetJA
  MODULE PURE SUBROUTINE obj_SetJA(obj, indx, VALUE)
    TYPE(CSRSparsity_), INTENT(INOUT) :: obj
    !! sparsity pattern
    INTEGER(I4B), INTENT(IN) :: indx
    !! position in JA, from 1 to obj%nnz
    INTEGER(I4B), INTENT(IN) :: VALUE
    !! new value
  END SUBROUTINE obj_SetJA
END INTERFACE SetJA

!----------------------------------------------------------------------------
!                                                          GetIA@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-12-23
! summary: Get an entry of IA
!
! Returns:
!   ans : INTEGER(I4B) - entry of IA
!
! Examples
!
!```fortran
! INTEGER(I4B) :: ii
!
! ii = GetIA(obj=csr, irow=1_I4B)
!```
!
! Full example: src/modules/CSRSparsity/examples/GetColIndex_test_1.F90
!
! See also: [[CSRSparsity_Method:obj_SetIA]]

INTERFACE GetIA
  MODULE PURE FUNCTION obj_GetIA(obj, irow) RESULT(ans)
    TYPE(CSRSparsity_), INTENT(IN) :: obj
    !! sparsity pattern
    INTEGER(I4B), INTENT(IN) :: irow
    !! position in IA, from 1 to obj%nrow + 1
    INTEGER(I4B) :: ans
    !! entry of IA
  END FUNCTION obj_GetIA
END INTERFACE GetIA

!----------------------------------------------------------------------------
!                                                        GetJA@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-12-14
! summary: Get an entry of JA
!
! Returns:
!   ans : INTEGER(I4B) - entry of JA
!
! Examples
!
!```fortran
! INTEGER(I4B) :: ii
!
! ii = GetJA(obj=csr, indx=1_I4B)
!```
!
! Full example: src/modules/CSRSparsity/examples/GetColIndex_test_1.F90
!
! See also: [[CSRSparsity_Method:obj_SetJA]]

INTERFACE GetJA
  MODULE PURE FUNCTION obj_GetJA(obj, indx) RESULT(ans)
    TYPE(CSRSparsity_), INTENT(IN) :: obj
    !! sparsity pattern
    INTEGER(I4B), INTENT(IN) :: indx
    !! position in JA, from 1 to obj%nnz
    INTEGER(I4B) :: ans
    !! entry of JA
  END FUNCTION obj_GetJA
END INTERFACE GetJA

!----------------------------------------------------------------------------
!                                                         GetSym@SymMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-01-28
! summary: Get the symmetric pattern of one triangle
!
! This routine builds in `symObj` the pattern of the symmetric matrix
! whose upper (`from` = "U") or lower (`from` = "L") triangle is the one
! stored in `obj`. Entries of the other triangle of `obj` are ignored.
! `from` is not case sensitive. Each row of `symObj` holds its lower
! part, its diagonal and its upper part, in this order, so it is sorted
! when `obj` is. The matrix must be square.
!
! `symObj%nnz` is twice the size of the strict triangle plus the number
! of diagonal entries. `symObj%idiag(i)` is the position of the diagonal
! of row `i` in `symObj%JA`, or 0 when `obj` has no diagonal entry in
! this row, and `symObj%isDiagStored` is set. The other flags, `idof` and
! `jdof` are taken from `obj`.
!
! With any other value of `from`, `symObj` is left unchanged, and debug
! builds print an error message.
!
! Changes:
!   - 2026-11-05: Fixed the last entry of symObj%IA for from = "U".
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: CSRSparsity_
! USE CSRSparsity_Method, ONLY: DEALLOCATE
! USE CSRSparsity_Method, ONLY: GetSym
! USE CSRSparsity_Method, ONLY: Initiate
! USE GlobalData, ONLY: I4B
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B), PARAMETER :: symIA(4) = [1, 4, 6, 8]
! INTEGER(I4B), PARAMETER :: symJA(7) = [1, 2, 3, 1, 2, 1, 3]
! INTEGER(I4B), PARAMETER :: symDiag(3) = [1, 5, 7]
!
! ! Test 1: from the upper triangle
! BLOCK
!   TYPE(CSRSparsity_) :: csr, sym
!   CALL Initiate(obj=csr, IA=[1_I4B, 4_I4B, 5_I4B, 6_I4B], &
!                 JA=[1_I4B, 2_I4B, 3_I4B, 2_I4B, 3_I4B])
!   CALL GetSym(obj=csr, symObj=sym, from="U")
!   CALL OK(sym%nnz .EQ. 7_I4B, "U: nnz")
!   CALL OK(sym%nrow .EQ. 3_I4B, "U: nrow")
!   CALL OK(sym%isDiagStored, "U: isDiagStored")
!   CALL OK(ALL(sym%IA .EQ. symIA), "U: IA")
!   CALL OK(ALL(sym%JA .EQ. symJA), "U: JA")
!   CALL OK(ALL(sym%idiag .EQ. symDiag), "U: idiag")
!   CALL DEALLOCATE (csr)
!   CALL DEALLOCATE (sym)
! END BLOCK
!
! ! Test 2: from the lower triangle
! BLOCK
!   TYPE(CSRSparsity_) :: csr, sym
!   CALL Initiate(obj=csr, IA=[1_I4B, 2_I4B, 4_I4B, 6_I4B], &
!                 JA=[1_I4B, 1_I4B, 2_I4B, 1_I4B, 3_I4B])
!   CALL GetSym(obj=csr, symObj=sym, from="L")
!   CALL OK(sym%nnz .EQ. 7_I4B, "L: nnz")
!   CALL OK(sym%isDiagStored, "L: isDiagStored")
!   CALL OK(ALL(sym%IA .EQ. symIA), "L: IA")
!   CALL OK(ALL(sym%JA .EQ. symJA), "L: JA")
!   CALL OK(ALL(sym%idiag .EQ. symDiag), "L: idiag")
!   CALL DEALLOCATE (csr)
!   CALL DEALLOCATE (sym)
! END BLOCK
!
! ! Test 3: from accepts lower case letters
! BLOCK
!   TYPE(CSRSparsity_) :: csr, sym
!   CALL Initiate(obj=csr, IA=[1_I4B, 3_I4B, 4_I4B], &
!                 JA=[1_I4B, 2_I4B, 2_I4B])
!   CALL GetSym(obj=csr, symObj=sym, from="u")
!   CALL OK(sym%nnz .EQ. 4_I4B, "lower case: nnz")
!   CALL OK(ALL(sym%JA .EQ. [1, 2, 1, 2]), "lower case: JA")
!   CALL DEALLOCATE (csr)
!   CALL DEALLOCATE (sym)
! END BLOCK
!
! ! Test 4: row 2 has no diagonal entry, idiag(2) is zero
! BLOCK
!   TYPE(CSRSparsity_) :: csr, sym
!   CALL Initiate(obj=csr, IA=[1_I4B, 3_I4B, 4_I4B, 5_I4B], &
!                 JA=[1_I4B, 2_I4B, 3_I4B, 3_I4B])
!   CALL GetSym(obj=csr, symObj=sym, from="U")
!   CALL OK(sym%nnz .EQ. 6_I4B, "no diagonal: nnz")
!   CALL OK(ALL(sym%IA .EQ. [1, 3, 5, 7]), "no diagonal: IA")
!   CALL OK(ALL(sym%JA .EQ. [1, 2, 1, 3, 2, 3]), "no diagonal: JA")
!   CALL OK(ALL(sym%idiag .EQ. [1, 0, 6]), "no diagonal: idiag")
!   CALL DEALLOCATE (csr)
!   CALL DEALLOCATE (sym)
! END BLOCK
!
! END PROGRAM main
!```
!
! See also: [[CSRSparsity_Method:obj_GetNNZ2]]

INTERFACE GetSym
  MODULE SUBROUTINE obj_GetSym1(obj, symObj, from)
    TYPE(CSRSparsity_), INTENT(IN) :: obj
    !! sparsity pattern with the triangle
    TYPE(CSRSparsity_), INTENT(INOUT) :: symObj
    !! symmetric pattern
    CHARACTER(1), INTENT(IN) :: from
    !! "U" or "L", the triangle of obj to use
  END SUBROUTINE obj_GetSym1
END INTERFACE GetSym

!----------------------------------------------------------------------------
!                                                         GetSym@SymMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-01-28
! summary: Replace a pattern by its symmetric pattern (not implemented)
!
! This routine is meant to be the in-place form of
! [[CSRSparsity_Method:obj_GetSym1]]. Its body is empty, so `obj` is left
! unchanged.
!
! See also: [[CSRSparsity_Method:obj_GetSym1]]

INTERFACE GetSym
  MODULE SUBROUTINE obj_GetSym2(obj, from)
    TYPE(CSRSparsity_), INTENT(INOUT) :: obj
    !! sparsity pattern
    CHARACTER(1), INTENT(IN) :: from
    !! "U" or "L", the triangle of obj to use
  END SUBROUTINE obj_GetSym2
END INTERFACE GetSym

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE CSRSparsity_Method
