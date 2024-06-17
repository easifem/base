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

SUBMODULE(RealVector_Norm2ErrorMethods) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                Norm2Error
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_norm2error_1
INTEGER(I4B) :: p(3), s(3), ii, jj
s = GetNodeLoc(obj=dofobj, idof=getIDOF(obj=dofobj, ivar=ivarobj, &
                                        idof=idofobj))

p = GetNodeLoc(obj=dofobj2, idof=getIDOF(obj=dofobj2, ivar=ivarobj2, &
                                         idof=idofobj2))

jj = 0; ans = 0.0_DFP

DO ii = s(1), s(2), s(3)
  jj = jj + 1
  ans = ans + (obj2%val(p(1) + (jj - 1) * p(3)) - obj%val(ii))**2
END DO

ans = SQRT(ans)
END PROCEDURE obj_norm2error_1

!----------------------------------------------------------------------------
!                                                                Norm2Error
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_norm2error_2
INTEGER(I4B) :: p(3), s(3), ii, jj, kk
ans = 0.0_DFP
DO kk = 1, SIZE(idofobj)
  s = GetNodeLoc(obj=dofobj, idof=getIDOF(obj=dofobj, ivar=ivarobj, &
                                          idof=idofobj(kk)))

  p = GetNodeLoc(obj=dofobj2, idof=getIDOF(obj=dofobj2, ivar=ivarobj2, &
                                           idof=idofobj2(kk)))

  jj = 0

  DO ii = s(1), s(2), s(3)
    jj = jj + 1
    ans = ans + (obj2%val(p(1) + (jj - 1) * p(3)) - obj%val(ii))**2
  END DO

END DO

ans = SQRT(ans)

END PROCEDURE obj_norm2error_2

!----------------------------------------------------------------------------
!                                                                 Norm2Error
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_norm2error_3
INTEGER(I4B) :: p(3), s(3), ii, jj

s = GetNodeLoc(obj=dofobj, idof=idofobj)
p = GetNodeLoc(obj=dofobj2, idof=idofobj2)

jj = 0; ans = 0.0_DFP

DO ii = s(1), s(2), s(3)
  jj = jj + 1
  ans = ans + (obj2%val(p(1) + (jj - 1) * p(3)) - obj%val(ii))**2
END DO

ans = SQRT(ans)

END PROCEDURE obj_norm2error_3

!----------------------------------------------------------------------------
!                                                                 Norm2Error
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_norm2error_4
INTEGER(I4B) :: p(3), s(3), ii, jj, kk

ans = 0.0_DFP

DO kk = 1, SIZE(idofobj)

  s = GetNodeLoc(obj=dofobj, idof=idofobj(kk))
  p = GetNodeLoc(obj=dofobj2, idof=idofobj2(kk))

  jj = 0

  DO ii = s(1), s(2), s(3)
    jj = jj + 1
    ans = ans + (obj2%val(p(1) + (jj - 1) * p(3)) - obj%val(ii))**2
  END DO

END DO
ans = SQRT(ans)
END PROCEDURE obj_norm2error_4

!----------------------------------------------------------------------------
!                                                                Norm2Error
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_norm2error_5
INTEGER(I4B) :: p(3), s(3), ii, jj
s = GetNodeLoc(obj=dofobj, idof=getIDOF(obj=dofobj, ivar=ivarobj, &
                            spaceCompo=spaceCompoObj, timeCompo=timeCompoObj))

p = GetNodeLoc(obj=dofobj2, idof=getIDOF(obj=dofobj2, ivar=ivarobj2, &
                          spaceCompo=spaceCompoobj2, timeCompo=timeCompoobj2))

jj = 0; ans = 0.0_DFP
DO ii = s(1), s(2), s(3)
  jj = jj + 1
  ans = ans + (obj2%val(p(1) + (jj - 1) * p(3)) - obj%val(ii))**2
END DO
ans = SQRT(ans)
END PROCEDURE obj_norm2error_5

!----------------------------------------------------------------------------
!                                                                Norm2Error
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_norm2error_6
INTEGER(I4B) :: p(3), s(3), ii, jj, kk
ans = 0.0_DFP
DO kk = 1, SIZE(timeCompoObj)
  s = GetNodeLoc(obj=dofobj, idof=getIDOF(obj=dofobj, ivar=ivarobj, &
                        spaceCompo=spaceCompoObj, timeCompo=timeCompoObj(kk)))

  p = GetNodeLoc(obj=dofobj2, idof=getIDOF(obj=dofobj2, ivar=ivarobj2, &
                      spaceCompo=spaceCompoobj2, timeCompo=timeCompoobj2(kk)))

  jj = 0
  DO ii = s(1), s(2), s(3)
    jj = jj + 1
    ans = ans + (obj2%val(p(1) + (jj - 1) * p(3)) - obj%val(ii))**2
  END DO
END DO
ans = SQRT(ans)
END PROCEDURE obj_norm2error_6

!----------------------------------------------------------------------------
!                                                                Norm2Error
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_norm2error_7
INTEGER(I4B) :: p(3), s(3), ii, jj, kk
ans = 0.0_DFP
DO kk = 1, SIZE(spaceCompoObj)
  s = GetNodeLoc(obj=dofobj, idof=getIDOF(obj=dofobj, ivar=ivarobj, &
                        spaceCompo=spaceCompoObj(kk), timeCompo=timeCompoObj))
  p = GetNodeLoc(obj=dofobj2, idof=getIDOF(obj=dofobj2, &
       ivar=ivarobj2, spaceCompo=spaceCompoobj2(kk), timeCompo=timeCompoobj2))

  jj = 0
  DO ii = s(1), s(2), s(3)
    jj = jj + 1
    ans = ans + (obj2%val(p(1) + (jj - 1) * p(3)) - obj%val(ii))**2
  END DO
END DO
ans = SQRT(ans)
END PROCEDURE obj_norm2error_7

END SUBMODULE Methods
