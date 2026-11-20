!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: Testing GetValue, reading bounding boxes from a TOML table

PROGRAM main
USE BaseType, ONLY: BoundingBox_
USE BoundingBox_Method, ONLY: GetValue
USE BoundingBox_Method, ONLY: OPERATOR(.Xmax.)
USE BoundingBox_Method, ONLY: OPERATOR(.Xmin.)
USE BoundingBox_Method, ONLY: OPERATOR(.Ymax.)
USE BoundingBox_Method, ONLY: OPERATOR(.Ymin.)
USE BoundingBox_Method, ONLY: OPERATOR(.Zmax.)
USE BoundingBox_Method, ONLY: OPERATOR(.Zmin.)
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: LGT
USE Test_Method, ONLY: OK
USE tomlf, ONLY: toml_loads
USE tomlf, ONLY: toml_table
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
CHARACTER(*), PARAMETER :: nl = NEW_LINE("a")

! Test 1: read one box from the keys xmin, xmax, ..., zmax of a table
BLOCK
  TYPE(toml_table), ALLOCATABLE :: table
  TYPE(BoundingBox_) :: obj
  CALL toml_loads(table, &
                  "xmin = 0.0"//nl//"xmax = 1.0"//nl// &
                  "ymin = 2.0"//nl//"ymax = 3.0"//nl// &
                  "zmin = 4.0"//nl//"zmax = 5.0"//nl)
  CALL GetValue(table=table, key="", VALUE=obj)
  CALL OK(obj%nsd .EQ. 3, "GetValue r0: nsd")
  CALL OK(ABS((.Xmin.obj) - 0.0_DFP) .LT. tol .AND. &
          ABS((.Xmax.obj) - 1.0_DFP) .LT. tol, "GetValue r0: x range")
  CALL OK(ABS((.Ymin.obj) - 2.0_DFP) .LT. tol .AND. &
          ABS((.Ymax.obj) - 3.0_DFP) .LT. tol, "GetValue r0: y range")
  CALL OK(ABS((.Zmin.obj) - 4.0_DFP) .LT. tol .AND. &
          ABS((.Zmax.obj) - 5.0_DFP) .LT. tol, "GetValue r0: z range")
END BLOCK

! Test 2: read an array of boxes stored under a key
BLOCK
  TYPE(toml_table), ALLOCATABLE :: table
  TYPE(BoundingBox_), ALLOCATABLE :: obj(:)
  LOGICAL(LGT) :: isFound
  CALL toml_loads(table, &
                  "boxes = [" //nl// &
                  "{xmin=0.0, xmax=1.0, ymin=0.0, ymax=1.0, " // &
                  "zmin=0.0, zmax=1.0}," //nl// &
                  "{xmin=1.0, xmax=2.0, ymin=1.0, ymax=3.0, " // &
                  "zmin=1.0, zmax=4.0}" //nl// "]"//nl)
  CALL GetValue(table=table, key="boxes", VALUE=obj, isFound=isFound)
  CALL OK(isFound, "GetValue r1: isFound")
  CALL OK(SIZE(obj) .EQ. 2, "GetValue r1: number of boxes")
  CALL OK(ABS((.Xmax.obj(2)) - 2.0_DFP) .LT. tol .AND. &
          ABS((.Ymax.obj(2)) - 3.0_DFP) .LT. tol .AND. &
          ABS((.Zmax.obj(2)) - 4.0_DFP) .LT. tol, &
          "GetValue r1: second box")
END BLOCK

! Test 3: a missing key sets isFound to false
BLOCK
  TYPE(toml_table), ALLOCATABLE :: table
  TYPE(BoundingBox_), ALLOCATABLE :: obj(:)
  LOGICAL(LGT) :: isFound
  CALL toml_loads(table, "other = 1"//nl)
  CALL GetValue(table=table, key="boxes", VALUE=obj, isFound=isFound)
  CALL OK(.NOT. isFound, "GetValue r1: missing key, isFound")
  CALL OK(.NOT. ALLOCATED(obj), "GetValue r1: missing key, not allocated")
END BLOCK

END PROGRAM main
