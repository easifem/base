```fortran
PROGRAM main
    USE easifemBase
    IMPLICIT NONE
```

```fortran
call display( ColConcat(a=reshape([1,2,3,4], [2,2]), &
    & b=reshape([5,6,7,8], [2,2]) ), "colconcat=" )
```

<details>
<summary>See results</summary>
<div>

```txt title="results"
colconcat=
----------
1  3  5  7
2  4  6  8
```

</div>
</details>

```fortran
call display( ColConcat(a=reshape(arange(1,9), [3,3]), &
    & b=reshape(arange(10,13), [2,2]) ), "colconcat=" )
```

<details>
<summary>See results</summary>
<div>

```txt title="results"
    colconcat=    
------------------
 1   4   7  10  12
 2   5   8  11  13
 3   6   9   0   0
```

</div>
</details>

```fortran
call display( ColConcat(a=reshape(arange(1,4), [2,2]), &
    & b=reshape(arange(5,13), [3,3]) ), "colconcat=" )
```

<details>
<summary>See results</summary>
<div>

```txt title="results"
    colconcat=    
------------------
 1   3   5   8  11
 2   4   6   9  12
 0   0   7  10  13
```

</div>
</details>

```fortran
END PROGRAM main
```
