program test_examples
   use test
   use, intrinsic :: iso_fortran_env, only: real32, real64, real128

   call plan(61)

   call note("")
   call note("Simple tests:")
   call note("")

   call ok(.true.)
   call ok(.false.)
   call ok(.true.,  "Test name 1")
   call ok(.false., "Test name 2")

   call note("")
   call note("Pass/fail tests:")
   call note("")

   call pass
   call fail
   call pass("Passing test")
   call fail("Failing test")

   call note("")
   call note("To do tests:")
   call note("")

   call todo
   call ok(.false.)
   call todo("Skipping?")
   call ok(.false.)

   call todo(8)
   call ok(.true.)
   call ok(.false.)
   call ok(.false.,  "Test name 1")
   call ok(.false., "Test name 2")
   call pass
   call fail
   call pass("Passing test")
   call fail("Failing test")

   call todo("Remember this!", 4)
   call ok(.true.)
   call ok(.false.)
   call ok(.true.,  "Test name 1")
   call ok(.false., "Test name 2")

   call note("")
   call note("Skipping tests")
   call note("")

   call skip
   call skip("Skippely skip 1 before just skipping 2")
   call skip(2)
   call skip("Skipping many", 3)

   call note("")
   call note("Tests using is-comparisons on overloaded scalar types")
   call note("")

   call is(1, 1, "1 == 1")
   call is(2 + 2, 5, "2 + 2 == 5")
   call is(.true., .true., ".true. .eqv. .true.")
   call is(.false., .true., ".false. .eqv. .true.")
   call is("Fish", "Tuna", '"Fish" == "Tuna"')
   call is("Fish", "Fish")
   call is("D", "d", '"D" == "d"')
   call is(" Lewis", "Lewis")

   call note("")
   call note("Tests using comparisons on real kinds (32, 64, and 128)")
   call note("")

   call note("")
   call note("#### Tests using isabs")
   call note("")
   call isabs(0.1_real32, 0.1_real32)
   call isabs(0.1_real32, 0.2_real32, 0.1_real32) ! not ok
   call isabs(0.1_real64, 0.1_real64)
   call isabs(0.1_real64, 0.2_real64, 0.1_real64) ! not ok
   call isabs(0.1_real128, 0.1_real128)
   call isabs(0.1_real128, 0.2_real128, 0.1_real128) ! not ok
   call note("")
   call note("#### Tests using isrel")
   call note("")
   call isrel(1008.0_real32, 1008.0_real32)
   call isrel(1008.0_real32, 1009.0_real32, 0.5e-3_real32) ! ok
   call isrel(1008.0_real32, 1009.0_real32, 0.5e-4_real32) ! not ok
   call isrel(1008.0_real64, 1008.0_real64)
   call isrel(1008.0_real64, 1009.0_real64, 0.5e-3_real64) ! ok
   call isrel(1008.0_real64, 1009.0_real64, 0.5e-4_real64) ! not ok
   call isrel(1008.0_real128, 1008.0_real128)
   call isrel(1008.0_real128, 1009.0_real128, 0.5e-3_real128) ! ok
   call isrel(1008.0_real128, 1009.0_real128, 0.5e-4_real128) ! not ok
   call note("")
   call note("#### Tests using isnear")
   call note("")
   call isnear(0.1_real32, 0.1_real32)
   call isnear(0.1_real32, 0.2_real32, 0.1_real32) ! not ok
   call isnear(0.1_real64, 0.1_real64)
   call isnear(0.1_real64, 0.2_real64, 0.1_real64) ! not ok
   call isnear(0.1_real128, 0.1_real128)
   call isnear(0.1_real128, 0.2_real128, 0.1_real128) ! not ok
   call isnear(1008.0_real128, 1008.0_real128)
   call isnear(1008.0_real128, 1009.0_real128, 0.5e-3_real128) ! not ok
   call isnear(1008.0_real128, 1009.0_real128, 0.5e-4_real128) ! not ok

   call note("")
   call note("Notes and diagnostic output")
   call note("")

   call diag("--> Visible in the test harness, unlike the note lines")
   call note("--> A note line like this is invisible in the test harness")
   call diag("--> Another diagnostic line")

   call note("")
   call note("DONE!")
   call note("")

   call done_testing
end program

