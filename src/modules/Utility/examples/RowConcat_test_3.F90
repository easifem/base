```fortran
PROGRAM main
    USE easifemBase
    IMPLICIT NONE
```

We can also concat the columns of two matrices.

```fortran
call display( RowConcat(a=reshape([1,2,3,4], [2,2]), &
    & b=reshape([5,6,7,8], [2,2]) ), "RowConcat=" )
```

<details>
<summary>See results</summary>
<div>

```txt title="results"
RowConcat=
----------
   1  3   
   2  4   
   5  7   
   6  8
```

</div>
</details>

```fortran
call display( RowConcat(a=reshape(arange(1,9), [3,3]), &
    & b=reshape(arange(10,13), [2,2]) ), "RowConcat=" )
```

<details>
<summary>See results</summary>
<div>

```txt title="results"
RowConcat=
----------
 1   4   7
 2   5   8
 3   6   9
10  12   0
11  13   0
```

</div>
</details>

```fortran
call display( RowConcat(a=reshape(arange(1,4), [2,2]), &
    & b=reshape(arange(5,13), [3,3]) ), "RowConcat=" )
```

<details>
<summary>See results</summary>
<div>

```txt title="results"
RowConcat=
----------
 1   3   0
 2   4   0
 5   8  11
 6   9  12
 7  10  13
```

</div>
</details>

```fortran
END PROGRAM main
```
