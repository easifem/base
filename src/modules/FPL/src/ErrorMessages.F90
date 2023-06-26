!-----------------------------------------------------------------
! FPL (Fortran Parameter List)
! Copyright (c) 2015 Santiago Badia, Alberto F. Martín,
! Javier Principe and Víctor Sande.
! All rights reserved.
!
! This library is free software; you can redistribute it and/or
! modify it under the terms of the GNU Lesser General Public
! License as published by the Free Software Foundation; either
! version 3.0 of the License, or (at your option) any later version.
!
! This library is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! Lesser General Public License for more details.
!
! You should have received a copy of the GNU Lesser General Public
! License along with this library.
!-----------------------------------------------------------------

MODULE ErrorMessages
USE ISO_FORTRAN_ENV, ONLY: OUTPUT_UNIT, ERROR_UNIT
USE PENF, ONLY: I4P, str

IMPLICIT NONE
PRIVATE

INTEGER(I4P), PUBLIC, PARAMETER :: FPLSuccess = 0
INTEGER(I4P), PUBLIC, PARAMETER :: FPLWrapperFactoryError = -1
INTEGER(I4P), PUBLIC, PARAMETER :: FPLWrapperError = -2
INTEGER(I4P), PUBLIC, PARAMETER :: FPLSublistError = -3
INTEGER(I4P), PUBLIC, PARAMETER :: FPLParameterListIteratorError = -4

TYPE :: MessageHandler_t
  PRIVATE
  CHARACTER(5) :: prefix = '[FPL]'
CONTAINS
  PROCEDURE, NON_OVERRIDABLE :: PRINT => MessageHandler_Print
  PROCEDURE, NON_OVERRIDABLE :: Warn => MessageHandler_Warn
  PROCEDURE, NON_OVERRIDABLE :: Error => MessageHandler_Error
END TYPE

TYPE(MessageHandler_t), SAVE :: msg
!$OMP THREADPRIVATE(msg)

PUBLIC :: msg

CONTAINS

SUBROUTINE MessageHandler_Print(this, txt, unit, iostat, iomsg)
  !-----------------------------------------------------------------
  !< Print a txt message preceding for prefix
  !-----------------------------------------------------------------
  CLASS(MessageHandler_t), INTENT(IN) :: this !< Message handler
  CHARACTER(*), INTENT(IN) :: txt !< Text to print
  INTEGER(I4P), OPTIONAL, INTENT(IN) :: unit !< Unit where to print
  INTEGER(I4P), OPTIONAL, INTENT(OUT) :: iostat !< IO error.
  CHARACTER(*), OPTIONAL, INTENT(OUT) :: iomsg !< IO error message.
  INTEGER(I4P) :: iostatd !< Real IO error.
  INTEGER(I4P) :: u !< Real unit
  CHARACTER(500) :: iomsgd !< Real IO error message.
  !-----------------------------------------------------------------
  u = OUTPUT_UNIT; IF (PRESENT(unit)) u = unit; iostatd = 0;
  iomsgd = ''
  WRITE (unit=u, fmt='(A)', iostat=iostatd, iomsg=iomsgd) &
   & this%Prefix//' '//txt
  IF (PRESENT(iostat)) iostat = iostatd
  IF (PRESENT(iomsg)) iomsg = iomsgd
END SUBROUTINE

SUBROUTINE MessageHandler_Warn(this, txt, unit, file, line, iostat, iomsg)
  !-----------------------------------------------------------------
  !< Warn a with txt message preceding for WARNING!
  !-----------------------------------------------------------------
  CLASS(MessageHandler_t), INTENT(IN) :: this !< Message handler
  CHARACTER(*), INTENT(IN) :: txt !< Text to print
  INTEGER(I4P), OPTIONAL, INTENT(IN) :: unit !< Unit where to print
  CHARACTER(*), OPTIONAL, INTENT(IN) :: file !< Source file
  INTEGER(I4P), OPTIONAL, INTENT(IN) :: line !< Number of line in source file
  INTEGER(I4P), OPTIONAL, INTENT(OUT) :: iostat !< IO error.
  CHARACTER(*), OPTIONAL, INTENT(OUT) :: iomsg !< IO error message.
  CHARACTER(:), ALLOCATABLE :: loc !< Warning location string
  INTEGER(I4P) :: iostatd !< Real IO error.
  INTEGER(I4P) :: u !< Real unit
  CHARACTER(500) :: iomsgd !< Real IO error message.
  !-----------------------------------------------------------------
  u = ERROR_UNIT; IF (PRESENT(unit)) u = unit; iostatd = 0;
  iomsgd = ''; loc = ''
  IF (PRESENT(file) .AND. PRESENT(line)) &
    & loc = '('//file//':'//TRIM(str(no_sign=.TRUE., n=line))//') '
  call this%Print('WARNING! '//trim(adjustl(loc//txt)), &
    & unit=u, iostat=iostatd, iomsg=iomsgd)
  IF (PRESENT(iostat)) iostat = iostatd
  IF (PRESENT(iomsg)) iomsg = iomsgd
END SUBROUTINE

SUBROUTINE MessageHandler_Error(this, txt, unit, file, line, iostat, iomsg)
  !-----------------------------------------------------------------
  !< Print a txt message preceding for ERROR!
  !-----------------------------------------------------------------
  CLASS(MessageHandler_t), INTENT(IN) :: this !< Message handler
  CHARACTER(*), INTENT(IN) :: txt !< Text to print
  INTEGER(I4P), OPTIONAL, INTENT(IN) :: unit !< Unit where to print
  CHARACTER(*), OPTIONAL, INTENT(IN) :: file !< Source file
  INTEGER(I4P), OPTIONAL, INTENT(IN) :: line !< Number of line in source file
  INTEGER(I4P), OPTIONAL, INTENT(OUT) :: iostat !< IO error.
  CHARACTER(*), OPTIONAL, INTENT(OUT) :: iomsg !< IO error message.
  CHARACTER(:), ALLOCATABLE :: loc !< Error location string
  INTEGER(I4P) :: iostatd !< Real IO error.
  INTEGER(I4P) :: u !< Real unit
  CHARACTER(500) :: iomsgd !< Real IO error message.
  !-----------------------------------------------------------------
  u = ERROR_UNIT; IF (PRESENT(unit)) u = unit; iostatd = 0; iomsgd = ''
  loc = ''
  IF (PRESENT(file) .AND. PRESENT(line)) &
    & loc = '('//file//':'//TRIM(str(no_sign=.TRUE., n=line))//') '
  call this%Print('ERROR! '//trim(adjustl(loc//txt)), &
    & unit=u, iostat=iostatd, iomsg=iomsgd)
  IF (PRESENT(iostat)) iostat = iostatd
  IF (PRESENT(iomsg)) iomsg = iomsgd
END SUBROUTINE

END MODULE
