```fortran
PROGRAM main
    USE easifemBase
    IMPLICIT NONE
```

Concat columns of a rank2 array and rank 1 array.

```fortran
call display( ColConcat(a=reshape([1,2,3,4], [2,2]), &
    & b=[5,6]), "colconcat=" )
```

<details>
<summary>See results</summary>
<div>

```txt title="results"
colconcat=
----------
 1  3  5  
 2  4  6
```

</div>
</details>

ColConcat works fine even when number of rows in rank-2 array is not the same as the size of rank-1 array.

```fortran
call display( ColConcat(a=reshape([1,2,3,4], [2,2]), &
    & b=[5,6, 7]), "colconcat=" )
```

<details>
<summary>See results</summary>
<div>

```txt title="results"
colconcat=
----------
 1  3  5  
 2  4  6  
 0  0  7
```

</div>
</details>

Concat columns of a rank2 array and rank 1 array.

```fortran
call display( ColConcat(b=reshape([1,2,3,4], [2,2]), &
    & a=[5,6]), "colconcat=" )
```

<details>
<summary>See results</summary>
<div>

```txt title="results"
colconcat=
----------
 5  1  3  
 6  2  4
```

</div>
</details>

ColConcat works fine even when number of rows in rank-2 array is not the same as the size of rank-1 array.

```fortran
call display( ColConcat(b=reshape([1,2,3,4], [2,2]), &
    & a=[5,6, 7]), "colconcat=" )
```

<details>
<summary>See results</summary>
<div>

```txt title="results"
colconcat=
----------
 5  1  3  
 6  2  4  
 7  0  0
```

</div>
</details>

```fortran
END PROGRAM main
```
