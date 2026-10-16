```fortran
PROGRAM main
    USE easifemBase
    IMPLICIT NONE
```

Concat two integer vectors

```fortran
call display( MdEnCode(ColConcat(a=[1,2], b=[3,4])), "colconcat="//CHAR_LF//CHAR_LF )
call ok( all( ColConcat(a=[1,2], b=[3,4]) &
    & .eq. reshape([1,2,3,4], [2,2])), "colconcat" )
```

<details>
<summary>See results</summary>
<div>

|   |   |
| - | - |
| 1 | 3 |
| 2 | 4 |

</div>
</details>

Concat two integer vectors of different length

```fortran
call display( MdEnCode(ColConcat(a=[1,2], b=[3,4,5])), "colconcat="//CHAR_LF//CHAR_LF )
call ok( all( ColConcat(a=[1,2], b=[3,4,5]) &
    & .eq. reshape([1,2,0,3,4,5], [3,2])), "colconcat" )
```

<details>
<summary>See results</summary>
<div>

|   |   |
| - | - |
| 1 | 3 |
| 2 | 4 |
| 0 | 5 |

</div>
</details>

Concat two integer vectors of different length.

```fortran
call display( MdEnCode(ColConcat(a=[3,4,5], b=[1,2])), "colconcat="//CHAR_LF//CHAR_LF )
```

<details>
<summary>See results</summary>
<div>

colconcat=

|   |   |
| - | - |
| 3 | 1 |
| 4 | 2 |
| 5 | 0 |

</div>
</details>

```fortran
END PROGRAM main
```
