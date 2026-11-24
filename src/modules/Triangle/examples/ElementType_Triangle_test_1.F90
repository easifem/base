!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: Testing ElementType_Triangle, ElementName_Triangle and
! ElementOrder_Triangle for all triangle element types

PROGRAM main
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: Triangle10
USE GlobalData, ONLY: Triangle12
USE GlobalData, ONLY: Triangle15
USE GlobalData, ONLY: Triangle15a
USE GlobalData, ONLY: Triangle15b
USE GlobalData, ONLY: Triangle21
USE GlobalData, ONLY: Triangle3
USE GlobalData, ONLY: Triangle6
USE GlobalData, ONLY: Triangle9
USE ReferenceTriangle_Method, ONLY: ElementName_Triangle
USE ReferenceTriangle_Method, ONLY: ElementOrder_Triangle
USE ReferenceTriangle_Method, ONLY: ElementType_Triangle
USE ReferenceTriangle_Method, ONLY: TotalNodesInElement_Triangle
USE Test_Method, ONLY: OK
IMPLICIT NONE

! Test 1: name to type for every name returned by ElementName_Triangle
BLOCK
  INTEGER(I4B), PARAMETER :: types(8) = [Triangle3, Triangle6, &
                     Triangle9, Triangle10, Triangle12, Triangle15a, &
                                         Triangle15b, Triangle21]
  INTEGER(I4B) :: ii
  LOGICAL :: isok

  isok = .TRUE.
  DO ii = 1, SIZE(types)
    isok = isok .AND. &
           (ElementType_Triangle(ElementName_Triangle(types(ii))) &
            .EQ. types(ii))
  END DO
  CALL OK(isok, "type -> name -> type round trip")
END BLOCK

! Test 2: "Triangle" is a synonym of "Triangle3"
BLOCK
  CALL OK(ElementType_Triangle("Triangle") .EQ. Triangle3, &
          "Triangle is Triangle3")
END BLOCK

! Test 3: "Triangle15" (complete quartic) is a valid name
! HighorderElement_Triangle builds this name for order 4
BLOCK
  CALL OK(ElementType_Triangle("Triangle15") .EQ. Triangle15, &
          "Triangle15 is the complete quartic triangle")
END BLOCK

! Test 4: unknown names give 0
BLOCK
  CALL OK(ElementType_Triangle("Quadrangle4") .EQ. 0, &
          "unknown name gives 0")
  CALL OK(ElementName_Triangle(-1_I4B) .EQ. "NONE", &
          "unknown type gives NONE")
END BLOCK

! Test 5: element order and number of nodes
BLOCK
  CALL OK(ElementOrder_Triangle(Triangle3) .EQ. 1, "order of Triangle3")
  CALL OK(ElementOrder_Triangle(Triangle6) .EQ. 2, "order of Triangle6")
  CALL OK(ElementOrder_Triangle(Triangle9) .EQ. 3, "order of Triangle9")
  CALL OK(ElementOrder_Triangle(Triangle10) .EQ. 3, "order of Triangle10")
  CALL OK(ElementOrder_Triangle(Triangle12) .EQ. 4, "order of Triangle12")
  CALL OK(ElementOrder_Triangle(Triangle15) .EQ. 4, "order of Triangle15")
  CALL OK(ElementOrder_Triangle(Triangle21) .EQ. 5, "order of Triangle21")

  CALL OK(TotalNodesInElement_Triangle(Triangle15) .EQ. 15, &
          "nodes of Triangle15")
  CALL OK(TotalNodesInElement_Triangle(Triangle21) .EQ. 21, &
          "nodes of Triangle21")
  CALL OK(TotalNodesInElement_Triangle(-1_I4B) .EQ. 0, &
          "nodes of unknown type is 0")
END BLOCK

END PROGRAM main
