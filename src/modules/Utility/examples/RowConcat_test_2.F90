```fortran
PROGRAM main
    USE easifemBase
    IMPLICIT NONE
```

Concat columns of a rank2 array and rank 1 array.

```fortran
call display( RowConcat(a=reshape([1,2,4,5], [2,2]), &
    & b=[3,6]), "RowConcat=" )
```

<details>
<summary>See results</summary>
<div>

```txt title="results"
RowConcat=
----------
   1  4   
   2  5   
   3  6
```

</div>
</details>

RowConcat works fine even when number of columns in rank-2 array is not the same as the size of rank-1 array.

```fortran
call display( RowConcat(a=reshape([1,2,4,5], [2,2]), &
    & b=[3,6, 7]), "RowConcat=" )
```

<details>
<summary>See results</summary>
<div>

```txt title="results"
RowConcat=
----------
 1  4  0  
 2  5  0  
 3  6  7
```

</div>
</details>

We can also concat the columns of a vector and a matrix.

```fortran
call display( RowConcat(b=reshape([1,2,4,5], [2,2]), &
    & a=[3,6]), "RowConcat=" )
call display( RowConcat(b=reshape([1,2,4,5], [2,2]), &
    & a=[3,6,7]), "RowConcat=" )
```

<details>
<summary>See results</summary>
<div>

```txt title="results"
RowConcat=
----------
   3  6   
   1  4   
   2  5   

RowConcat=
----------
 3  6  7  
 1  4  0  
 2  5  0
```

</div>
</details>

```fortran
END PROGRAM main
```
