program main
    use stringifor
    implicit none

    type( string ) :: obj1, obj2, obj3, obj
    character( len = 120 ) :: char

    obj1 = "hello"
    obj2 = "world"
    obj3 = "!"

    obj = obj % join( [obj1, obj2, obj3] )

    write( *, * ) trim( obj % raw )

    char = obj // "-try"

    write( *, * ) trim( char )

end program main