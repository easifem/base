! Reference: https://github.com/dennisdjensen/fortran-testanything/blob/master/test_examples.f08

PROGRAM test_examples
USE test
USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32, REAL64, REAL128

CALL plan(61)

CALL note("")
CALL note("Simple tests:")
CALL note("")

CALL ok(.TRUE.)
CALL ok(.FALSE.)
CALL ok(.TRUE., "Test name 1")
CALL ok(.FALSE., "Test name 2")

CALL note("")
CALL note("Pass/fail tests:")
CALL note("")

CALL PASS
CALL fail
CALL PASS("Passing test")
CALL fail("Failing test")

CALL note("")
CALL note("To do tests:")
CALL note("")

CALL todo
CALL ok(.FALSE.)
CALL todo("Skipping?")
CALL ok(.FALSE.)

CALL todo(8)
CALL ok(.TRUE.)
CALL ok(.FALSE.)
CALL ok(.FALSE., "Test name 1")
CALL ok(.FALSE., "Test name 2")
CALL PASS
CALL fail
CALL PASS("Passing test")
CALL fail("Failing test")

CALL todo("Remember this!", 4)
CALL ok(.TRUE.)
CALL ok(.FALSE.)
CALL ok(.TRUE., "Test name 1")
CALL ok(.FALSE., "Test name 2")

CALL note("")
CALL note("Skipping tests")
CALL note("")

CALL skip
CALL skip("Skippely skip 1 before just skipping 2")
CALL skip(2)
CALL skip("Skipping many", 3)

CALL note("")
CALL note("Tests using is-comparisons on overloaded scalar types")
CALL note("")

CALL is(1, 1, "1 == 1")
CALL is(2 + 2, 5, "2 + 2 == 5")
CALL is(.TRUE., .TRUE., ".true. .eqv. .true.")
CALL is(.FALSE., .TRUE., ".false. .eqv. .true.")
CALL is("Fish", "Tuna", '"Fish" == "Tuna"')
CALL is("Fish", "Fish")
CALL is("D", "d", '"D" == "d"')
CALL is(" Lewis", "Lewis")

CALL note("")
CALL note("Tests using comparisons on real kinds (32, 64, and 128)")
CALL note("")

CALL note("")
CALL note("#### Tests using isabs")
CALL note("")
CALL isabs(0.1_REAL32, 0.1_REAL32)
CALL isabs(0.1_REAL32, 0.2_REAL32, 0.1_REAL32) ! not ok
CALL isabs(0.1_REAL64, 0.1_REAL64)
CALL isabs(0.1_REAL64, 0.2_REAL64, 0.1_REAL64) ! not ok
CALL isabs(0.1_REAL128, 0.1_REAL128)
CALL isabs(0.1_REAL128, 0.2_REAL128, 0.1_REAL128) ! not ok
CALL note("")
CALL note("#### Tests using isrel")
CALL note("")
CALL isrel(1008.0_REAL32, 1008.0_REAL32)
CALL isrel(1008.0_REAL32, 1009.0_REAL32, 0.5E-3_REAL32) ! ok
CALL isrel(1008.0_REAL32, 1009.0_REAL32, 0.5E-4_REAL32) ! not ok
CALL isrel(1008.0_REAL64, 1008.0_REAL64)
CALL isrel(1008.0_REAL64, 1009.0_REAL64, 0.5E-3_REAL64) ! ok
CALL isrel(1008.0_REAL64, 1009.0_REAL64, 0.5E-4_REAL64) ! not ok
CALL isrel(1008.0_REAL128, 1008.0_REAL128)
CALL isrel(1008.0_REAL128, 1009.0_REAL128, 0.5E-3_REAL128) ! ok
CALL isrel(1008.0_REAL128, 1009.0_REAL128, 0.5E-4_REAL128) ! not ok
CALL note("")
CALL note("#### Tests using isnear")
CALL note("")
CALL isnear(0.1_REAL32, 0.1_REAL32)
CALL isnear(0.1_REAL32, 0.2_REAL32, 0.1_REAL32) ! not ok
CALL isnear(0.1_REAL64, 0.1_REAL64)
CALL isnear(0.1_REAL64, 0.2_REAL64, 0.1_REAL64) ! not ok
CALL isnear(0.1_REAL128, 0.1_REAL128)
CALL isnear(0.1_REAL128, 0.2_REAL128, 0.1_REAL128) ! not ok
CALL isnear(1008.0_REAL128, 1008.0_REAL128)
CALL isnear(1008.0_REAL128, 1009.0_REAL128, 0.5E-3_REAL128) ! not ok
CALL isnear(1008.0_REAL128, 1009.0_REAL128, 0.5E-4_REAL128) ! not ok

CALL note("")
CALL note("Notes and diagnostic output")
CALL note("")

CALL diag("--> Visible in the test harness, unlike the note lines")
CALL note("--> A note line like this is invisible in the test harness")
CALL diag("--> Another diagnostic line")

CALL note("")
CALL note("DONE!")
CALL note("")

CALL done_testing
END PROGRAM
