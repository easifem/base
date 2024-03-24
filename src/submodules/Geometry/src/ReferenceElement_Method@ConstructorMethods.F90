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

SUBMODULE(ReferenceElement_Method) ConstructorMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                       RefTopoReallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE RefTopoReallocate
INTEGER(I4B) :: tsize, ii
LOGICAL(LGT) :: isok

isok = ALLOCATED(obj)

IF (isok) THEN

  tsize = SIZE(obj)

  DO ii = 1, tsize
    CALL DEALLOCATE (obj(ii))
  END DO

  IF (tsize .NE. n) THEN
    DEALLOCATE (obj)
    ALLOCATE (obj(n))
  END IF

ELSE

  ALLOCATE (obj(n))

END IF

END PROCEDURE RefTopoReallocate

!----------------------------------------------------------------------------
!                                                                Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE RefTopoDeallocate
LOGICAL(LGT) :: isok
INTEGER(I4B) :: ii, tsize

isok = ALLOCATED(obj)

IF (isok) THEN
  tsize = SIZE(obj)

  DO ii = 1, tsize
    CALL DEALLOCATE (obj(ii))
  END DO

  DEALLOCATE (obj)

END IF

END PROCEDURE RefTopoDeallocate

!----------------------------------------------------------------------------
!                                                         ReferenceTopology
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_ReferenceTopology
CALL Reallocate(obj%nptrs, SIZE(nptrs))
obj%nptrs = nptrs
obj%name = name
obj%xiDimension = XiDimension(name)
END PROCEDURE refelem_ReferenceTopology

!----------------------------------------------------------------------------
!                                                            Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_Deallocate1
IF (ALLOCATED(obj%nptrs)) DEALLOCATE (obj%nptrs)
obj%name = 0_I4B
obj%XiDimension = 0_I4B
END PROCEDURE refelem_Deallocate1

!----------------------------------------------------------------------------
!                                                            Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_Deallocate2
INTEGER(I4B) :: ii
obj%domainName = "GENERAL"
obj%entityCounts = 0
obj%xiDimension = 0
obj%name = 0
obj%order = 0
obj%nsd = 0
obj%interpolationPointType = Equidistance
IF (ALLOCATED(obj%topology)) THEN
  DO ii = 1, SIZE(obj%topology)
    CALL DEALLOCATE (obj%topology(ii))
  END DO
  DEALLOCATE (obj%topology)
END IF
IF (ALLOCATED(obj%xiJ)) DEALLOCATE (obj%xiJ)
obj%highOrderElement => NULL()
END PROCEDURE refelem_Deallocate2

!----------------------------------------------------------------------------
!                                                                        NNE
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_NNE1
IF (ALLOCATED(obj%nptrs)) THEN
  Ans = SIZE(obj%nptrs)
ELSE
  Ans = 0
END IF
END PROCEDURE refelem_NNE1

!----------------------------------------------------------------------------
!                                                                        NNE
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_NNE2
IF (ALLOCATED(obj%XiJ)) THEN
  Ans = SIZE(obj%XiJ, 2)
ELSE
  Ans = 0
END IF
END PROCEDURE refelem_NNE2

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_Initiate1
obj%domainName = anotherobj%domainName
IF (ALLOCATED(anotherobj%xiJ)) obj%xiJ = anotherobj%xiJ
obj%entityCounts = anotherobj%entityCounts
obj%xiDimension = anotherobj%xiDimension
obj%nsd = anotherobj%nsd
obj%order = anotherobj%order
obj%name = anotherobj%name
obj%interpolationPointType = anotherobj%interpolationPointType
IF (ALLOCATED(anotherobj%topology)) THEN
  obj%topology = anotherobj%topology
END IF
obj%highOrderElement => anotherobj%highOrderElement
END PROCEDURE refelem_Initiate1

!----------------------------------------------------------------------------
!                                                  ReferenceElement_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_constructor_1
CLASS(ReferenceElement_), POINTER :: refelem
INTEGER(I4B) :: elemOrder
refelem => NULL()
SELECT CASE (xidim)
CASE (0)
  ans => ReferencePoint_Pointer(nsd=nsd)
CASE (1)
  elemOrder = ElementOrder(elemType)
  IF (elemOrder .NE. 1) THEN
    refelem => ReferenceLine_Pointer(nsd=nsd)
    ALLOCATE (ReferenceLine_ :: ans)
    CALL refelem%highOrderElement( &
      & order=elemOrder, &
      & ipType=ipType, &
      & highOrderObj=ans)
    CALL DEALLOCATE (refelem)
    DEALLOCATE (refelem)
    refelem => NULL()
  ELSE
    ans => ReferenceLine_Pointer(nsd=nsd)
  END IF
CASE (2)
  elemOrder = ElementOrder(elemType)
  IF (isTriangle(elemType)) THEN
    IF (elemOrder .NE. 1) THEN
      refelem => ReferenceTriangle_Pointer(nsd=nsd)
      ALLOCATE (ReferenceTriangle_ :: ans)
      CALL refelem%highOrderElement( &
        & order=elemOrder, &
        & highOrderObj=ans, &
        & ipType=ipType)
      CALL DEALLOCATE (refelem)
      DEALLOCATE (refelem)
      refelem => NULL()
    ELSE
      ans => ReferenceTriangle_Pointer(nsd=nsd)
    END IF
  ELSE IF (isQuadrangle(elemType)) THEN
    IF (elemOrder .NE. 1) THEN
      refelem => ReferenceQuadrangle_Pointer(nsd=nsd)
      ALLOCATE (ReferenceQuadrangle_ :: ans)
      CALL refelem%highOrderElement( &
        & order=elemOrder, &
        & highOrderObj=ans,  &
        & ipType=ipType)
      CALL DEALLOCATE (refelem)
      DEALLOCATE (refelem)
      refelem => NULL()
    ELSE
      ans => ReferenceQuadrangle_Pointer(nsd=nsd)
    END IF
  END IF
CASE (3)
  elemOrder = ElementOrder(elemType)
  IF (isTetrahedron(elemType)) THEN
    IF (elemOrder .NE. 1) THEN
      refelem => ReferenceTetrahedron_Pointer(nsd=nsd)
      ALLOCATE (ReferenceTetrahedron_ :: ans)
      CALL refelem%highOrderElement( &
        & order=elemOrder, &
        & highOrderObj=ans, &
        & ipType=ipType)
      CALL DEALLOCATE (refelem)
      DEALLOCATE (refelem)
      refelem => NULL()
    ELSE
      ans => ReferenceTetrahedron_Pointer(nsd=nsd)
    END IF
  ELSE IF (isHexahedron(elemType)) THEN
    IF (elemOrder .NE. 1) THEN
      refelem => ReferenceHexahedron_Pointer(nsd=nsd)
      ALLOCATE (ReferenceHexahedron_ :: ans)
      CALL refelem%highOrderElement( &
        & order=elemOrder, &
        & highOrderObj=ans, &
        & ipType=ipType)
      CALL DEALLOCATE (refelem)
      DEALLOCATE (refelem)
      refelem => NULL()
    ELSE
      ans => ReferenceHexahedron_Pointer(nsd=nsd)
    END IF
  ELSE IF (isPrism(elemType)) THEN
    IF (elemOrder .NE. 1) THEN
      refelem => ReferencePrism_Pointer(nsd=nsd)
      ALLOCATE (ReferencePrism_ :: ans)
      CALL refelem%highOrderElement( &
        & order=elemOrder, &
        & highOrderObj=ans, &
        & ipType=ipType)
      CALL DEALLOCATE (refelem)
      DEALLOCATE (refelem)
      refelem => NULL()
    ELSE
      ans => ReferencePrism_Pointer(nsd=nsd)
    END IF
  ELSE IF (isPyramid(elemType)) THEN
    IF (elemOrder .NE. 1) THEN
      refelem => ReferencePyramid_Pointer(nsd=nsd)
      ALLOCATE (ReferencePyramid_ :: ans)
      CALL refelem%highOrderElement( &
        & order=elemOrder, &
        & highOrderObj=ans, &
        & ipType=ipType)
      CALL DEALLOCATE (refelem)
      DEALLOCATE (refelem)
      refelem => NULL()
    ELSE
      ans => ReferencePyramid_Pointer(nsd=nsd)
    END IF
  END IF
END SELECT
ans%interpolationPointType = ipType
END PROCEDURE refelem_constructor_1

!----------------------------------------------------------------------------
!                                                  ReferenceElement_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_constructor_2
SELECT TYPE (refelem)
TYPE IS (ReferenceLine_)
  ALLOCATE (ReferenceLine_ :: ans)
TYPE IS (ReferenceTriangle_)
  ALLOCATE (ReferenceTriangle_ :: ans)
TYPE IS (ReferenceQuadrangle_)
  ALLOCATE (ReferenceQuadrangle_ :: ans)
TYPE IS (ReferenceTetrahedron_)
  ALLOCATE (ReferenceTetrahedron_ :: ans)
TYPE IS (ReferenceHexahedron_)
  ALLOCATE (ReferenceHexahedron_ :: ans)
TYPE IS (ReferencePrism_)
  ALLOCATE (ReferencePrism_ :: ans)
TYPE IS (ReferencePyramid_)
  ALLOCATE (ReferencePyramid_ :: ans)
CLASS DEFAULT
  SELECT CASE (refelem%name)
  CASE (Line)
    ALLOCATE (ReferenceLine_ :: ans)
  CASE (Triangle)
    ALLOCATE (ReferenceTriangle_ :: ans)
  CASE (Quadrangle)
    ALLOCATE (ReferenceQuadrangle_ :: ans)
  CASE (Tetrahedron)
    ALLOCATE (ReferenceTetrahedron_ :: ans)
  CASE (Hexahedron)
    ALLOCATE (ReferenceHexahedron_ :: ans)
  CASE (Prism)
    ALLOCATE (ReferencePrism_ :: ans)
  CASE (Pyramid)
    ALLOCATE (ReferencePyramid_ :: ans)
  END SELECT
END SELECT
ans = refelem
END PROCEDURE refelem_constructor_2

!----------------------------------------------------------------------------
!                                                              getnptrs
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_getnptrs
INTEGER(I4B) :: ii, tsize
LOGICAL(LGT) :: isok

isok = ALLOCATED(obj%topology)
IF (.NOT. isok) THEN
  CALL Reallocate(ans, 0)
  RETURN
END IF

ii = SUM(obj%entityCounts)
tsize = SIZE(obj%topology)
isok = ii .LE. tsize

IF (.NOT. isok) THEN
  CALL Reallocate(ans, 0)
  RETURN
END IF

isok = ALLOCATED(obj%topology(ii)%nptrs)

IF (.NOT. isok) THEN
  CALL Reallocate(ans, 0)
  RETURN
END IF

ans = obj%topology(ii)%nptrs
END PROCEDURE refelem_getnptrs

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE ConstructorMethods
