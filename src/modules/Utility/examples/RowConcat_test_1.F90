```fortran
PROGRAM main
    USE easifemBase
    IMPLICIT NONE
```

Concat two integer vectors

```fortran
call display( RowConcat(a=[1,2], b=[3,4]), "RowConcat=" )
call ok( all( RowConcat(a=[1,2], b=[3,4]) &
    & .eq. reshape([1,3,2,4], [2,2])), "RowConcat" )
```

<details>
<summary>See results</summary>
<div>

```txt title="results"
RowConcat=
----------
   1  2   
   3  4
```

</div>
</details>

Concat two integer vectors of different length

```fortran
call display( RowConcat(a=[1,2], b=[3,4,5]), "RowConcat=" )
call ok( all( RowConcat(a=[1,2], b=[3,4,5]) &
    & .eq. reshape([1,3,2,4,0,5], [2,3])), "RowConcat" )
```

<details>
<summary>See results</summary>
<div>

```txt title="results"
RowConcat=
----------
 1  2  0  
 3  4  5
```

</div>
</details>

```fortran
END PROGRAM main
```
