! This module is taken from
! https://github.com/interkosmos/fortran-lua54
!
! lua.f90
!
! A collection of ISO C binding interfaces to Lua 5.4 for Fortran 2008.
!
! Author:  Philipp Engel
! Licence: ISC
!
! I have modified it slighly according to the EASIFEM requirement.
!

MODULE LuaInterface
USE, INTRINSIC :: ISO_C_BINDING
USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: i8 => INT64
USE GlobalData, ONLY: I4B
USE Display_Method, ONLY: Display
IMPLICIT NONE
PRIVATE

! The integer and float types used by Lua are platform-specific.
! Select the types according to your local Lua library.
INTEGER, PARAMETER, PUBLIC :: lua_integer = C_LONG_LONG
! The other options for lua_integer are
! c_int, c_long, c_long_long, c_int64_t
INTEGER, PARAMETER, PUBLIC :: lua_number = C_DOUBLE
! The other options for lua_number are
! c_float, c_double, c_long_double
INTEGER, PARAMETER, PUBLIC :: lua_kcontext = C_INTPTR_T
! The other options for lua_kcontext are
! c_intptr_t, c_ptrdiff_t

INTEGER(kind=C_INT), PARAMETER, PUBLIC :: LUA_VERSION_NUM = 504

! Option for multiple returns in `lua_pcall()` and `lua_call()`.
INTEGER(kind=C_INT), PARAMETER, PUBLIC :: LUA_MULTRET = -1

! Basic types.
INTEGER(kind=C_INT), PARAMETER, PUBLIC :: LUA_TNONE = -1
INTEGER(kind=C_INT), PARAMETER, PUBLIC :: LUA_TNIL = 0
INTEGER(kind=C_INT), PARAMETER, PUBLIC :: LUA_TBOOLEAN = 1
INTEGER(kind=C_INT), PARAMETER, PUBLIC :: LUA_TLIGHTUSERDATA = 2
INTEGER(kind=C_INT), PARAMETER, PUBLIC :: LUA_TNUMBER = 3
INTEGER(kind=C_INT), PARAMETER, PUBLIC :: LUA_TSTRING = 4
INTEGER(kind=C_INT), PARAMETER, PUBLIC :: LUA_TTABLE = 5
INTEGER(kind=C_INT), PARAMETER, PUBLIC :: LUA_TFUNCTION = 6
INTEGER(kind=C_INT), PARAMETER, PUBLIC :: LUA_TUSERDATA = 7
INTEGER(kind=C_INT), PARAMETER, PUBLIC :: LUA_TTHREAD = 8

! Comparison and arithmetic options.
INTEGER(kind=C_INT), PARAMETER, PUBLIC :: LUA_OPADD = 0
INTEGER(kind=C_INT), PARAMETER, PUBLIC :: LUA_OPSUB = 1
INTEGER(kind=C_INT), PARAMETER, PUBLIC :: LUA_OPMUL = 2
INTEGER(kind=C_INT), PARAMETER, PUBLIC :: LUA_OPMOD = 3
INTEGER(kind=C_INT), PARAMETER, PUBLIC :: LUA_OPPOW = 4
INTEGER(kind=C_INT), PARAMETER, PUBLIC :: LUA_OPDIV = 5
INTEGER(kind=C_INT), PARAMETER, PUBLIC :: LUA_OPIDIV = 6
INTEGER(kind=C_INT), PARAMETER, PUBLIC :: LUA_OPBAND = 7
INTEGER(kind=C_INT), PARAMETER, PUBLIC :: LUA_OPBOR = 8
INTEGER(kind=C_INT), PARAMETER, PUBLIC :: LUA_OPBXOR = 9
INTEGER(kind=C_INT), PARAMETER, PUBLIC :: LUA_OPSHL = 10
INTEGER(kind=C_INT), PARAMETER, PUBLIC :: LUA_OPSHR = 11
INTEGER(kind=C_INT), PARAMETER, PUBLIC :: LUA_OPUNM = 12
INTEGER(kind=C_INT), PARAMETER, PUBLIC :: LUA_OPBNOT = 13

INTEGER(kind=C_INT), PARAMETER, PUBLIC :: LUA_OPEQ = 0
INTEGER(kind=C_INT), PARAMETER, PUBLIC :: LUA_OPLT = 1
INTEGER(kind=C_INT), PARAMETER, PUBLIC :: LUA_OPLE = 2

! Garbage-collection options.
INTEGER(kind=C_INT), PARAMETER, PUBLIC :: LUA_GCSTOP = 0
INTEGER(kind=C_INT), PARAMETER, PUBLIC :: LUA_GCRESTART = 1
INTEGER(kind=C_INT), PARAMETER, PUBLIC :: LUA_GCCOLLECT = 2
INTEGER(kind=C_INT), PARAMETER, PUBLIC :: LUA_GCCOUNT = 3
INTEGER(kind=C_INT), PARAMETER, PUBLIC :: LUA_GCCOUNTB = 4
INTEGER(kind=C_INT), PARAMETER, PUBLIC :: LUA_GCSTEP = 5
INTEGER(kind=C_INT), PARAMETER, PUBLIC :: LUA_GCSETPAUSE = 6
INTEGER(kind=C_INT), PARAMETER, PUBLIC :: LUA_GCSETSTEPMUL = 7
INTEGER(kind=C_INT), PARAMETER, PUBLIC :: LUA_GCISRUNNING = 9
INTEGER(kind=C_INT), PARAMETER, PUBLIC :: LUA_GCGEN = 10
INTEGER(kind=C_INT), PARAMETER, PUBLIC :: LUA_GCINC = 11

! Error codes.
INTEGER(kind=C_INT), PARAMETER, PUBLIC :: LUA_OK = 0
INTEGER(kind=C_INT), PARAMETER, PUBLIC :: LUA_YIELD = 1
INTEGER(kind=C_INT), PARAMETER, PUBLIC :: LUA_ERRRUN = 2
INTEGER(kind=C_INT), PARAMETER, PUBLIC :: LUA_ERRSYNTAX = 3
INTEGER(kind=C_INT), PARAMETER, PUBLIC :: LUA_ERRMEM = 4
INTEGER(kind=C_INT), PARAMETER, PUBLIC :: LUA_ERRERR = 5
INTEGER(kind=C_INT), PARAMETER, PUBLIC :: LUA_ERRFILE = LUA_ERRERR + 1

PUBLIC :: lua_checkerror
PUBLIC :: lua_arith
PUBLIC :: lua_call
PUBLIC :: lua_callk
PUBLIC :: lua_checkstack
PUBLIC :: lua_close
PUBLIC :: lua_compare
PUBLIC :: lua_concat
PUBLIC :: lua_copy
PUBLIC :: lua_createtable
PUBLIC :: lua_gc
PUBLIC :: lua_getfield
PUBLIC :: lua_getglobal
PUBLIC :: lua_gettable
PUBLIC :: lua_gettop
PUBLIC :: lua_isboolean
PUBLIC :: lua_iscfunction
PUBLIC :: lua_isfunction
PUBLIC :: lua_isinteger
PUBLIC :: lua_isnil
PUBLIC :: lua_isnone
PUBLIC :: lua_isnoneornil
PUBLIC :: lua_isnumber
PUBLIC :: lua_isstring
PUBLIC :: lua_istable
PUBLIC :: lua_isthread
PUBLIC :: lua_isuserdata
PUBLIC :: lua_isyieldable
PUBLIC :: lua_load
PUBLIC :: lua_newtable
PUBLIC :: lua_pcall
PUBLIC :: lua_pcallk
PUBLIC :: lua_pop
PUBLIC :: lua_pushboolean
PUBLIC :: lua_pushcclosure
PUBLIC :: lua_pushinteger
PUBLIC :: lua_pushlightuserdata
PUBLIC :: lua_pushlstring
PUBLIC :: lua_pushnil
PUBLIC :: lua_pushnumber
PUBLIC :: lua_pushstring
PUBLIC :: lua_pushthread
PUBLIC :: lua_pushvalue
PUBLIC :: lua_rawget
PUBLIC :: lua_rawgeti
PUBLIC :: lua_rawlen
PUBLIC :: lua_rawset
PUBLIC :: lua_rawseti
PUBLIC :: lua_register
PUBLIC :: lua_setfield
PUBLIC :: lua_setglobal
PUBLIC :: lua_seti
PUBLIC :: lua_settable
PUBLIC :: lua_settop
PUBLIC :: lua_status
PUBLIC :: lua_toboolean
PUBLIC :: lua_tointeger
PUBLIC :: lua_tointegerx
PUBLIC :: lua_tonumber
PUBLIC :: lua_tonumberx
PUBLIC :: lua_tostring
PUBLIC :: lua_type
PUBLIC :: lua_typename
PUBLIC :: lua_version
PUBLIC :: lual_checkversion_
PUBLIC :: lual_dofile
PUBLIC :: lual_dostring
PUBLIC :: lual_len
PUBLIC :: lual_loadfile
PUBLIC :: lual_loadfilex
PUBLIC :: lual_loadstring
PUBLIC :: lual_newstate
PUBLIC :: lual_openlibs

PRIVATE :: c_f_str_ptr

!----------------------------------------------------------------------------
!                                                                 Strlen
!----------------------------------------------------------------------------

! Interfaces to libc.
INTERFACE
  FUNCTION c_strlen(str) BIND(c, name='strlen')
    IMPORT :: C_PTR, C_SIZE_T
    IMPLICIT NONE
    TYPE(C_PTR), INTENT(in), VALUE :: str
    INTEGER(kind=C_SIZE_T) :: c_strlen
  END FUNCTION c_strlen
END INTERFACE

!----------------------------------------------------------------------------
!                                                             lua_checkstack
!----------------------------------------------------------------------------

! Interfaces to Lua 5.4.
INTERFACE
  ! int lua_checkstack(lua_State *L, int n)
  FUNCTION lua_checkstack(l, n) BIND(c, name='lua_checkstack')
    IMPORT :: C_INT, C_PTR
    IMPLICIT NONE
    TYPE(C_PTR), INTENT(in), VALUE :: l
    INTEGER(kind=C_INT), INTENT(in), VALUE :: n
    INTEGER(kind=C_INT) :: lua_checkstack
  END FUNCTION lua_checkstack
END INTERFACE

!----------------------------------------------------------------------------
!                                                             lua_compare
!----------------------------------------------------------------------------

INTERFACE
  ! int lua_compare(lua_State *L, int index1, int index2, int op)
  FUNCTION lua_compare(l, index1, index2, op) BIND(c, name='lua_compare')
    IMPORT :: C_INT, C_PTR
    IMPLICIT NONE
    TYPE(C_PTR), INTENT(in), VALUE :: l
    INTEGER(kind=C_INT), INTENT(in), VALUE :: index1
    INTEGER(kind=C_INT), INTENT(in), VALUE :: index2
    INTEGER(kind=C_INT), INTENT(in), VALUE :: op
    INTEGER(kind=C_INT) :: lua_compare
  END FUNCTION lua_compare
END INTERFACE

!----------------------------------------------------------------------------
!                                                                    lua_gc
!----------------------------------------------------------------------------

INTERFACE
  ! int lua_gc(lua_State *L, int what, int data)
  FUNCTION lua_gc(l, what, DATA) BIND(c, name='lua_gc')
    IMPORT :: C_INT, C_PTR
    IMPLICIT NONE
    TYPE(C_PTR), INTENT(in), VALUE :: l
    INTEGER(kind=C_INT), INTENT(in), VALUE :: what
    INTEGER(kind=C_INT), INTENT(in), VALUE :: DATA
    INTEGER(kind=C_INT) :: lua_gc
  END FUNCTION lua_gc
END INTERFACE

!----------------------------------------------------------------------------
!                                                              lua_getfield
!----------------------------------------------------------------------------

INTERFACE
  ! int lua_getfield(lua_State *L, int idx, const char *k)
  FUNCTION lua_getfield_(l, idx, k) BIND(c, name='lua_getfield')
    IMPORT :: C_CHAR, C_INT, C_PTR
    IMPLICIT NONE
    TYPE(C_PTR), INTENT(in), VALUE :: l
    INTEGER(kind=C_INT), INTENT(in), VALUE :: idx
    CHARACTER(kind=C_CHAR), INTENT(in) :: k
    INTEGER(kind=C_INT) :: lua_getfield_
  END FUNCTION lua_getfield_
END INTERFACE

!----------------------------------------------------------------------------
!                                                             lua_getglobal
!----------------------------------------------------------------------------

INTERFACE
  ! int lua_getglobal(lua_State *L, const char *name)
  FUNCTION lua_getglobal_(l, name) BIND(c, name='lua_getglobal')
    IMPORT :: C_CHAR, C_INT, C_PTR
    IMPLICIT NONE
    TYPE(C_PTR), INTENT(in), VALUE :: l
    CHARACTER(kind=C_CHAR), INTENT(in) :: name
    INTEGER(kind=C_INT) :: lua_getglobal_
  END FUNCTION lua_getglobal_
END INTERFACE

!----------------------------------------------------------------------------
!                                                              lua_gettable
!----------------------------------------------------------------------------

INTERFACE
  ! int lua_gettable (lua_State *L, int idx)
  FUNCTION lua_gettable(l, idx) BIND(c, name='lua_gettable')
    IMPORT :: C_INT, C_PTR
    IMPLICIT NONE
    TYPE(C_PTR), INTENT(in), VALUE :: l
    INTEGER(kind=C_INT), INTENT(in), VALUE :: idx
    INTEGER(kind=C_INT) :: lua_gettable
  END FUNCTION lua_gettable
END INTERFACE

!----------------------------------------------------------------------------
!                                                                lua_gettop
!----------------------------------------------------------------------------

INTERFACE
  ! int lua_gettop(lua_State *L)
  FUNCTION lua_gettop(l) BIND(c, name='lua_gettop')
    IMPORT :: C_INT, C_PTR
    IMPLICIT NONE
    TYPE(C_PTR), INTENT(in), VALUE :: l
    INTEGER(kind=C_INT) :: lua_gettop
  END FUNCTION lua_gettop
END INTERFACE

!----------------------------------------------------------------------------
!                                                         lua_iscfunction
!----------------------------------------------------------------------------

INTERFACE
  ! int lua_iscfunction(lua_State *L, int idx)
  FUNCTION lua_iscfunction(l, idx) BIND(c, name='lua_iscfunction')
    IMPORT :: C_INT, C_PTR
    IMPLICIT NONE
    TYPE(C_PTR), INTENT(in), VALUE :: l
    INTEGER(kind=C_INT), INTENT(in), VALUE :: idx
    INTEGER(kind=C_INT) :: lua_iscfunction
  END FUNCTION lua_iscfunction
END INTERFACE

!----------------------------------------------------------------------------
!                                                         lua_isinteger
!----------------------------------------------------------------------------

INTERFACE
  ! int lua_isinteger(lua_State *L, int idx)
  FUNCTION lua_isinteger(l, idx) BIND(c, name='lua_isinteger')
    IMPORT :: C_INT, C_PTR
    IMPLICIT NONE
    TYPE(C_PTR), INTENT(in), VALUE :: l
    INTEGER(kind=C_INT), INTENT(in), VALUE :: idx
    INTEGER(kind=C_INT) :: lua_isinteger
  END FUNCTION lua_isinteger
END INTERFACE

!----------------------------------------------------------------------------
!                                                              lua_isnumber
!----------------------------------------------------------------------------

INTERFACE
  ! int lua_isnumber(lua_State *L, int idx)
  FUNCTION lua_isnumber(l, idx) BIND(c, name='lua_isnumber')
    IMPORT :: C_INT, C_PTR
    IMPLICIT NONE
    TYPE(C_PTR), INTENT(in), VALUE :: l
    INTEGER(kind=C_INT), INTENT(in), VALUE :: idx
    INTEGER(kind=C_INT) :: lua_isnumber
  END FUNCTION lua_isnumber

END INTERFACE

!----------------------------------------------------------------------------
!                                                              lua_isstring
!----------------------------------------------------------------------------

INTERFACE
  ! int lua_isstring(lua_State *L, int idx)
  FUNCTION lua_isstring(l, idx) BIND(c, name='lua_isstring')
    IMPORT :: C_INT, C_PTR
    IMPLICIT NONE
    TYPE(C_PTR), INTENT(in), VALUE :: l
    INTEGER(kind=C_INT), INTENT(in), VALUE :: idx
    INTEGER(kind=C_INT) :: lua_isstring
  END FUNCTION lua_isstring
END INTERFACE

!----------------------------------------------------------------------------
!                                                            lua_isuserdata
!----------------------------------------------------------------------------

INTERFACE
  ! int lua_isuserdata(lua_State *L, int idx)
  FUNCTION lua_isuserdata(l, idx) BIND(c, name='lua_isuserdata')
    IMPORT :: C_INT, C_PTR
    IMPLICIT NONE
    TYPE(C_PTR), INTENT(in), VALUE :: l
    INTEGER(kind=C_INT), INTENT(in), VALUE :: idx
    INTEGER(kind=C_INT) :: lua_isuserdata
  END FUNCTION lua_isuserdata
END INTERFACE

!----------------------------------------------------------------------------
!                                                           lua_isyieldable
!----------------------------------------------------------------------------

INTERFACE
  ! int lua_isyieldable(lua_State *L)
  FUNCTION lua_isyieldable(l) BIND(c, name='lua_isyielable')
    IMPORT :: C_INT, C_PTR
    IMPLICIT NONE
    TYPE(C_PTR), INTENT(in), VALUE :: l
    INTEGER(kind=C_INT) :: lua_isyieldable
  END FUNCTION lua_isyieldable
END INTERFACE

!----------------------------------------------------------------------------
!                                                                  lua_load
!----------------------------------------------------------------------------

INTERFACE
  ! int lua_load(lua_State *L, lua_Reader reader, void *data, const char *chunkname, const char *mode)
  FUNCTION lua_load(l, reader, DATA, chunkname, mode) BIND(c, name='lua_load')
    IMPORT :: C_CHAR, C_FUNPTR, C_INT, C_PTR
    IMPLICIT NONE
    TYPE(C_PTR), INTENT(in), VALUE :: l
    TYPE(C_FUNPTR), INTENT(in), VALUE :: reader
    TYPE(C_PTR), INTENT(in), VALUE :: DATA
    CHARACTER(kind=C_CHAR), INTENT(in) :: chunkname
    CHARACTER(kind=C_CHAR), INTENT(in) :: mode
    INTEGER(kind=C_INT) :: lua_load
  END FUNCTION lua_load
END INTERFACE

!----------------------------------------------------------------------------
!                                                                lua_rawget
!----------------------------------------------------------------------------

INTERFACE
  ! int lua_rawget(lua_State *L, int idx)
  FUNCTION lua_rawget(l, idx) BIND(c, name='lua_rawget')
    IMPORT :: C_INT, C_PTR
    IMPLICIT NONE
    TYPE(C_PTR), INTENT(in), VALUE :: l
    INTEGER(kind=C_INT), INTENT(in), VALUE :: idx
    INTEGER(kind=C_INT) :: lua_rawget
  END FUNCTION lua_rawget
END INTERFACE

!----------------------------------------------------------------------------
!                                                               lua_rawgeti
!----------------------------------------------------------------------------

INTERFACE
  ! int lua_rawgeti(lua_State *L, int idx, lua_Integer n)
  FUNCTION lua_rawgeti(l, idx, n) BIND(c, name='lua_rawgeti')
    IMPORT :: C_INT, C_PTR, lua_integer
    IMPLICIT NONE
    TYPE(C_PTR), INTENT(in), VALUE :: l
    INTEGER(kind=C_INT), INTENT(in), VALUE :: idx
    INTEGER(kind=lua_integer), INTENT(in), VALUE :: n
    INTEGER(kind=C_INT) :: lua_rawgeti
  END FUNCTION lua_rawgeti
END INTERFACE

!----------------------------------------------------------------------------
!                                                                lua_rawlen
!----------------------------------------------------------------------------

INTERFACE
  ! size_t lua_rawlen(lua_State *L, int idx)
  FUNCTION lua_rawlen(l, idx) BIND(c, name='lua_rawlen')
    IMPORT :: C_INT, C_PTR, C_SIZE_T
    IMPLICIT NONE
    TYPE(C_PTR), INTENT(in), VALUE :: l
    INTEGER(kind=C_INT), INTENT(in), VALUE :: idx
    INTEGER(kind=C_SIZE_T) :: lua_rawlen
  END FUNCTION lua_rawlen
END INTERFACE

!----------------------------------------------------------------------------
!                                                                lua_status
!----------------------------------------------------------------------------

INTERFACE
  ! int lua_status(lua_State *L)
  FUNCTION lua_status(l) BIND(c, name='lua_status')
    IMPORT :: C_INT, C_PTR
    IMPLICIT NONE
    TYPE(C_PTR), INTENT(in), VALUE :: l
    INTEGER(kind=C_INT) :: lua_status
  END FUNCTION lua_status
END INTERFACE

!----------------------------------------------------------------------------
!                                                             lua_toboolean
!----------------------------------------------------------------------------

INTERFACE
  ! int lua_toboolean(lua_State *L, int idx)
  FUNCTION lua_toboolean_(l, idx) BIND(c, name='lua_toboolean')
    IMPORT :: C_INT, C_PTR
    IMPLICIT NONE
    TYPE(C_PTR), INTENT(in), VALUE :: l
    INTEGER(kind=C_INT), INTENT(in), VALUE :: idx
    INTEGER(kind=C_INT) :: lua_toboolean_
  END FUNCTION lua_toboolean_
END INTERFACE

!----------------------------------------------------------------------------
!                                                            lua_tonumberx
!----------------------------------------------------------------------------

INTERFACE
  ! float lua_tonumberx(lua_State *L, int idx, int *isnum)
  FUNCTION lua_tonumberx(l, idx, isnum) BIND(c, name='lua_tonumberx')
    IMPORT :: C_INT, C_PTR, lua_number
    IMPLICIT NONE
    TYPE(C_PTR), INTENT(in), VALUE :: l
    INTEGER(kind=C_INT), INTENT(in), VALUE :: idx
    TYPE(C_PTR), INTENT(in), VALUE :: isnum
    REAL(kind=lua_number) :: lua_tonumberx
  END FUNCTION lua_tonumberx
END INTERFACE

!----------------------------------------------------------------------------
!                                                         lua_tointegerx
!----------------------------------------------------------------------------

INTERFACE
  ! lua_Integer lua_tointegerx(lua_State *L, int idx, int *isnum)
  FUNCTION lua_tointegerx(l, idx, isnum) BIND(c, name='lua_tointegerx')
    IMPORT :: C_INT, C_PTR, lua_integer
    IMPLICIT NONE
    TYPE(C_PTR), INTENT(in), VALUE :: l
    INTEGER(kind=C_INT), INTENT(in), VALUE :: idx
    TYPE(C_PTR), INTENT(in), VALUE :: isnum
    INTEGER(kind=lua_integer) :: lua_tointegerx
  END FUNCTION lua_tointegerx

END INTERFACE

!----------------------------------------------------------------------------
!                                                           lua_tolstring
!----------------------------------------------------------------------------

INTERFACE
  ! const char *lua_tolstring(lua_State *L, int idx, size_t *len)
  FUNCTION lua_tolstring(l, idx, len) BIND(c, name='lua_tolstring')
    IMPORT :: C_INT, C_PTR
    IMPLICIT NONE
    TYPE(C_PTR), INTENT(in), VALUE :: l
    INTEGER(kind=C_INT), INTENT(in), VALUE :: idx
    TYPE(C_PTR), INTENT(in), VALUE :: len
    TYPE(C_PTR) :: lua_tolstring
  END FUNCTION lua_tolstring
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 lua_type
!----------------------------------------------------------------------------

INTERFACE
  ! int lua_type(lua_State *L, int idx)
  FUNCTION lua_type(l, idx) BIND(c, name='lua_type')
    IMPORT :: C_INT, C_PTR
    IMPLICIT NONE
    TYPE(C_PTR), INTENT(in), VALUE :: l
    INTEGER(kind=C_INT), INTENT(in), VALUE :: idx
    INTEGER(kind=C_INT) :: lua_type
  END FUNCTION lua_type
END INTERFACE

!----------------------------------------------------------------------------
!                                                              lua_typename
!----------------------------------------------------------------------------

INTERFACE
  ! const char *lua_typename(lua_State *L, int tp)
  FUNCTION lua_typename_(l, tp) BIND(c, name='lua_typename')
    IMPORT :: C_INT, C_PTR
    IMPLICIT NONE
    TYPE(C_PTR), INTENT(in), VALUE :: l
    INTEGER(kind=C_INT), INTENT(in), VALUE :: tp
    TYPE(C_PTR) :: lua_typename_
  END FUNCTION lua_typename_
END INTERFACE

!----------------------------------------------------------------------------
!                                                               lua_pcallk
!----------------------------------------------------------------------------

INTERFACE
  ! int lua_pcallk(lua_State *L, int nargs, int nresults, int errfunc, lua_KContext ctx, lua_KFunction k)
        function lua_pcallk(l, nargs, nresults, errfunc, ctx, k) bind(c, name='lua_pcallk')
    IMPORT :: C_FUNPTR, C_INT, C_PTR, lua_kcontext
    IMPLICIT NONE
    TYPE(C_PTR), INTENT(in), VALUE :: l
    INTEGER(kind=C_INT), INTENT(in), VALUE :: nargs
    INTEGER(kind=C_INT), INTENT(in), VALUE :: nresults
    INTEGER(kind=C_INT), INTENT(in), VALUE :: errfunc
    INTEGER(kind=lua_kcontext), INTENT(in), VALUE :: ctx
    TYPE(C_FUNPTR), INTENT(in), VALUE :: k
    INTEGER(kind=C_INT) :: lua_pcallk
  END FUNCTION lua_pcallk
END INTERFACE

!----------------------------------------------------------------------------
!                                                           lua_pushlstring
!----------------------------------------------------------------------------

INTERFACE
  ! const char *lua_pushlstring(lua_State *L, const char *s, size_t len)
  FUNCTION lua_pushlstring_(l, s, len) BIND(c, name='lua_pushlstring')
    IMPORT :: C_CHAR, C_PTR, C_SIZE_T
    IMPLICIT NONE
    TYPE(C_PTR), INTENT(in), VALUE :: l
    CHARACTER(kind=C_CHAR), INTENT(in) :: s
    INTEGER(kind=C_SIZE_T), INTENT(in), VALUE :: len
    TYPE(C_PTR) :: lua_pushlstring_
  END FUNCTION lua_pushlstring_
END INTERFACE

!----------------------------------------------------------------------------
!                                                           lua_pushstring
!----------------------------------------------------------------------------

INTERFACE
  ! const char *lua_pushstring(lua_State *L, const char *s)
  FUNCTION lua_pushstring_(l, s) BIND(c, name='lua_pushstring')
    IMPORT :: C_CHAR, C_PTR
    IMPLICIT NONE
    TYPE(C_PTR), INTENT(in), VALUE :: l
    CHARACTER(kind=C_CHAR), INTENT(in) :: s
    TYPE(C_PTR) :: lua_pushstring_
  END FUNCTION lua_pushstring_
END INTERFACE

!----------------------------------------------------------------------------
!                                                            lua_pushthread
!----------------------------------------------------------------------------

INTERFACE
  ! int lua_pushthread(lua_State *L)
  FUNCTION lua_pushthread(l) BIND(c, name='lua_pushthread')
    IMPORT :: C_INT, C_PTR
    IMPLICIT NONE
    TYPE(C_PTR), INTENT(in), VALUE :: l
    INTEGER(kind=C_INT) :: lua_pushthread
  END FUNCTION lua_pushthread
END INTERFACE

!----------------------------------------------------------------------------
!                                                               lua_version
!----------------------------------------------------------------------------

INTERFACE
  ! lua_Number lua_version(lua_State *L)
  FUNCTION lua_version(l) BIND(c, name='lua_version')
    IMPORT :: C_PTR, lua_number
    IMPLICIT NONE
    TYPE(C_PTR), INTENT(in), VALUE :: l
    REAL(kind=lua_number) :: lua_version
  END FUNCTION lua_version
END INTERFACE

!----------------------------------------------------------------------------
!                                                                  luaL_len
!----------------------------------------------------------------------------

INTERFACE
  ! int luaL_len(lua_State *L, int idx)
  FUNCTION lual_len(l, idx) BIND(c, name='luaL_len')
    IMPORT :: C_INT, C_PTR
    IMPLICIT NONE
    TYPE(C_PTR), INTENT(in), VALUE :: l
    INTEGER(kind=C_INT), INTENT(in), VALUE :: idx
    INTEGER(kind=C_INT) :: lual_len
  END FUNCTION lual_len
END INTERFACE

!----------------------------------------------------------------------------
!                                                         lual_loadfilex
!----------------------------------------------------------------------------

INTERFACE
  ! int luaL_loadfilex(lua_State *L, const char *filename, const char *mode)
  FUNCTION lual_loadfilex(l, filename, mode) BIND(c, name='luaL_loadfilex')
    IMPORT :: C_CHAR, C_INT, C_PTR
    IMPLICIT NONE
    TYPE(C_PTR), INTENT(in), VALUE :: l
    CHARACTER(kind=C_CHAR), INTENT(in) :: filename
    TYPE(C_PTR), INTENT(in), VALUE :: mode
    INTEGER(kind=C_INT) :: lual_loadfilex
  END FUNCTION lual_loadfilex
END INTERFACE

!----------------------------------------------------------------------------
!                                                         lual_loadstring
!----------------------------------------------------------------------------

INTERFACE
  ! int luaL_loadstring(lua_State *L, const char *s)
  FUNCTION lual_loadstring_(l, s) BIND(c, name='luaL_loadstring')
    IMPORT :: C_CHAR, C_INT, C_PTR
    IMPLICIT NONE
    TYPE(C_PTR), INTENT(in), VALUE :: l
    CHARACTER(kind=C_CHAR), INTENT(in) :: s
    INTEGER(kind=C_INT) :: lual_loadstring_
  END FUNCTION lual_loadstring_
END INTERFACE

!----------------------------------------------------------------------------
!                                                           lual_newstate
!----------------------------------------------------------------------------

INTERFACE
  ! lua_State *luaL_newstate(void)
  FUNCTION lual_newstate() BIND(c, name='luaL_newstate')
    IMPORT :: C_PTR
    IMPLICIT NONE
    TYPE(C_PTR) :: lual_newstate
  END FUNCTION lual_newstate
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 lua_arith
!----------------------------------------------------------------------------

INTERFACE
  ! void lua_arith(lua_State *L, int op)
  SUBROUTINE lua_arith(l, op) BIND(c, name='lua_arith')
    IMPORT :: C_INT, C_PTR
    IMPLICIT NONE
    TYPE(C_PTR), INTENT(in), VALUE :: l
    INTEGER(kind=C_INT), INTENT(in), VALUE :: op
  END SUBROUTINE lua_arith
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 lua_callk
!----------------------------------------------------------------------------

INTERFACE
  ! void lua_callk(lua_State *L, int nargs, int nresults, lua_KContext ctx, lua_CFunction k)
  SUBROUTINE lua_callk(l, nargs, nresults, ctx, k) BIND(c, name='lua_callk')
    IMPORT :: C_FUNPTR, C_INT, C_PTR, lua_kcontext
    IMPLICIT NONE
    TYPE(C_PTR), INTENT(in), VALUE :: l
    INTEGER(kind=C_INT), INTENT(in), VALUE :: nargs
    INTEGER(kind=C_INT), INTENT(in), VALUE :: nresults
    INTEGER(kind=lua_kcontext), INTENT(in), VALUE :: ctx
    TYPE(C_FUNPTR), INTENT(in), VALUE :: k
  END SUBROUTINE lua_callk
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 lua_close
!----------------------------------------------------------------------------

INTERFACE
  ! void lua_close(lua_State *L)
  SUBROUTINE lua_close(l) BIND(c, name='lua_close')
    IMPORT :: C_PTR
    IMPLICIT NONE
    TYPE(C_PTR), INTENT(in), VALUE :: l
  END SUBROUTINE lua_close

END INTERFACE

!----------------------------------------------------------------------------
!                                                             lua_concat
!----------------------------------------------------------------------------

INTERFACE
  ! void lua_concat(lua_State *L, int n)
  SUBROUTINE lua_concat(l, n) BIND(c, name='lua_concat')
    IMPORT :: C_INT, C_PTR
    IMPLICIT NONE
    TYPE(C_PTR), INTENT(in), VALUE :: l
    INTEGER(kind=C_INT), INTENT(in), VALUE :: n
  END SUBROUTINE lua_concat
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  ! void lua_copy(lua_State *L, int fromidx, int toidx)
  SUBROUTINE lua_copy(l, fromidx, toidx) BIND(c, name='lua_copy')
    IMPORT :: C_INT, C_PTR
    IMPLICIT NONE
    TYPE(C_PTR), INTENT(in), VALUE :: l
    INTEGER(kind=C_INT), INTENT(in), VALUE :: fromidx
    INTEGER(kind=C_INT), INTENT(in), VALUE :: toidx
  END SUBROUTINE lua_copy
END INTERFACE

!----------------------------------------------------------------------------
!                                                            lua_createtable
!----------------------------------------------------------------------------

INTERFACE
  ! void lua_createtable(lua_State *L, int narr, int nrec)
  SUBROUTINE lua_createtable(l, narr, nrec) BIND(c, name='lua_createtable')
    IMPORT :: C_INT, C_PTR
    IMPLICIT NONE
    TYPE(C_PTR), INTENT(in), VALUE :: l
    INTEGER(kind=C_INT), INTENT(in), VALUE :: narr
    INTEGER(kind=C_INT), INTENT(in), VALUE :: nrec
  END SUBROUTINE lua_createtable
END INTERFACE

!----------------------------------------------------------------------------
!                                                           lua_pushboolean
!----------------------------------------------------------------------------

INTERFACE
  ! void lua_pushboolean(lua_State *L, int b)
  SUBROUTINE lua_pushboolean(l, b) BIND(c, name='lua_pushboolean')
    IMPORT :: C_INT, C_PTR
    IMPLICIT NONE
    TYPE(C_PTR), INTENT(in), VALUE :: l
    INTEGER(kind=C_INT), INTENT(in), VALUE :: b
  END SUBROUTINE lua_pushboolean
END INTERFACE

!----------------------------------------------------------------------------
!                                                         lua_pushcclosure
!----------------------------------------------------------------------------

INTERFACE
  ! void lua_pushcclosure(lua_State *L, lua_CFunction fn, int n)
  SUBROUTINE lua_pushcclosure(l, fn, n) BIND(c, name='lua_pushcclosure')
    IMPORT :: C_FUNPTR, C_INT, C_PTR
    IMPLICIT NONE
    TYPE(C_PTR), INTENT(in), VALUE :: l
    TYPE(C_FUNPTR), INTENT(in), VALUE :: fn
    INTEGER(kind=C_INT), INTENT(in), VALUE :: n
  END SUBROUTINE lua_pushcclosure
END INTERFACE

!----------------------------------------------------------------------------
!                                                           lua_pushinteger
!----------------------------------------------------------------------------

INTERFACE
  ! void lua_pushinteger(lua_State *L, lua_Integer n)
  SUBROUTINE lua_pushinteger(l, n) BIND(c, name='lua_pushinteger')
    IMPORT :: C_PTR, lua_integer
    IMPLICIT NONE
    TYPE(C_PTR), INTENT(in), VALUE :: l
    INTEGER(kind=lua_integer), INTENT(in), VALUE :: n
  END SUBROUTINE lua_pushinteger

END INTERFACE

!----------------------------------------------------------------------------
!                                                     lua_pushlightuserdata
!----------------------------------------------------------------------------

INTERFACE
  ! void  lua_pushlightuserdata(lua_State *L, void *p)
  SUBROUTINE lua_pushlightuserdata(l, p) BIND(c, name='lua_pushlightuserdata')
    IMPORT :: C_PTR
    IMPLICIT NONE
    TYPE(C_PTR), INTENT(in), VALUE :: l
    TYPE(C_PTR), INTENT(in), VALUE :: p
  END SUBROUTINE lua_pushlightuserdata
END INTERFACE

!----------------------------------------------------------------------------
!                                                             lua_pushnil
!----------------------------------------------------------------------------

INTERFACE
  ! void lua_pushnil(lua_State *L)
  SUBROUTINE lua_pushnil(l) BIND(c, name='lua_pushnil')
    IMPORT :: C_PTR
    IMPLICIT NONE
    TYPE(C_PTR), INTENT(in), VALUE :: l
  END SUBROUTINE lua_pushnil
END INTERFACE

!----------------------------------------------------------------------------
!                                                         lua_pushnumber
!----------------------------------------------------------------------------

INTERFACE
  ! void lua_pushnumber(lua_State *L, lua_Number n)
  SUBROUTINE lua_pushnumber(l, n) BIND(c, name='lua_pushnumber')
    IMPORT :: C_PTR, lua_number
    IMPLICIT NONE
    TYPE(C_PTR), INTENT(in), VALUE :: l
    REAL(kind=lua_number), INTENT(in), VALUE :: n
  END SUBROUTINE lua_pushnumber
END INTERFACE

!----------------------------------------------------------------------------
!                                                             lua_pushvalue
!----------------------------------------------------------------------------

INTERFACE
  ! void lua_pushvalue(lua_State *L, int idx)
  SUBROUTINE lua_pushvalue(l, idx) BIND(c, name='lua_pushvalue')
    IMPORT :: C_INT, C_PTR
    IMPLICIT NONE
    TYPE(C_PTR), INTENT(in), VALUE :: l
    INTEGER(kind=C_INT), INTENT(in), VALUE :: idx
  END SUBROUTINE lua_pushvalue
END INTERFACE

!----------------------------------------------------------------------------
!                                                                lua_rawset
!----------------------------------------------------------------------------

INTERFACE
  ! void lua_rawset(lua_State *L, int idx)
  SUBROUTINE lua_rawset(l, idx) BIND(c, name='lua_rawset')
    IMPORT :: C_INT, C_PTR, lua_integer
    IMPLICIT NONE
    TYPE(C_PTR), INTENT(in), VALUE :: l
    INTEGER(kind=C_INT), INTENT(in), VALUE :: idx
  END SUBROUTINE lua_rawset
END INTERFACE

!----------------------------------------------------------------------------
!                                                               lua_rawseti
!----------------------------------------------------------------------------

INTERFACE
  ! void lua_rawseti(lua_State *L, int idx, lua_Integer n)
  SUBROUTINE lua_rawseti(l, idx, n) BIND(c, name='lua_rawseti')
    IMPORT :: C_INT, C_PTR, lua_integer
    IMPLICIT NONE
    TYPE(C_PTR), INTENT(in), VALUE :: l
    INTEGER(kind=C_INT), INTENT(in), VALUE :: idx
    INTEGER(kind=lua_integer), INTENT(in), VALUE :: n
  END SUBROUTINE lua_rawseti
END INTERFACE

!----------------------------------------------------------------------------
!                                                              lua_setfield
!----------------------------------------------------------------------------

INTERFACE
  ! void lua_setfield(lua_State *L, int idx, const char *k)
  SUBROUTINE lua_setfield_(l, idx, k) BIND(c, name='lua_setfield')
    IMPORT :: C_CHAR, C_INT, C_PTR
    IMPLICIT NONE
    TYPE(C_PTR), INTENT(in), VALUE :: l
    INTEGER(kind=C_INT), INTENT(in), VALUE :: idx
    CHARACTER(kind=C_CHAR), INTENT(in) :: k
  END SUBROUTINE lua_setfield_
END INTERFACE

!----------------------------------------------------------------------------
!                                                             lua_setglobal
!----------------------------------------------------------------------------

INTERFACE
  ! void lua_setglobal(lua_State *L, const char *name)
  SUBROUTINE lua_setglobal_(l, name) BIND(c, name='lua_setglobal')
    IMPORT :: C_CHAR, C_PTR
    IMPLICIT NONE
    TYPE(C_PTR), INTENT(in), VALUE :: l
    CHARACTER(kind=C_CHAR), INTENT(in) :: name
  END SUBROUTINE lua_setglobal_
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 lua_seti
!----------------------------------------------------------------------------
INTERFACE
  ! void lua_seti(lua_State *L, int idx, lua_Integer n)
  SUBROUTINE lua_seti(l, idx, n) BIND(c, name='lua_seti')
    IMPORT :: C_INT, C_PTR, lua_integer
    IMPLICIT NONE
    TYPE(C_PTR), INTENT(in), VALUE :: l
    INTEGER(kind=C_INT), INTENT(in), VALUE :: idx
    INTEGER(kind=lua_integer), INTENT(in), VALUE :: n
  END SUBROUTINE lua_seti
END INTERFACE

!----------------------------------------------------------------------------
!                                                              lua_settable
!----------------------------------------------------------------------------

INTERFACE
  ! void lua_settable(lua_State *L, int idx)
  SUBROUTINE lua_settable(l, idx) BIND(c, name='lua_settable')
    IMPORT :: C_INT, C_PTR
    IMPLICIT NONE
    TYPE(C_PTR), INTENT(in), VALUE :: l
    INTEGER(kind=C_INT), INTENT(in), VALUE :: idx
  END SUBROUTINE lua_settable
END INTERFACE

!----------------------------------------------------------------------------
!                                                                lua_settop
!----------------------------------------------------------------------------

INTERFACE
  ! void lua_settop(lua_State *L, int idx)
  SUBROUTINE lua_settop(l, idx) BIND(c, name='lua_settop')
    IMPORT :: C_INT, C_PTR
    IMPLICIT NONE
    TYPE(C_PTR), INTENT(in), VALUE :: l
    INTEGER(kind=C_INT), INTENT(in), VALUE :: idx
  END SUBROUTINE lua_settop
END INTERFACE

!----------------------------------------------------------------------------
!                                                         lual_checkversion_
!----------------------------------------------------------------------------

INTERFACE
  ! void luaL_checkversion_(lua_State *L, lua_Number ver, size_t sz)
  SUBROUTINE lual_checkversion_(l, ver, sz) BIND(c, name='luaL_checkversion_')
    IMPORT :: C_PTR, C_SIZE_T, lua_number
    IMPLICIT NONE
    TYPE(C_PTR), INTENT(in), VALUE :: l
    REAL(kind=lua_number), INTENT(in), VALUE :: ver
    INTEGER(kind=C_SIZE_T), INTENT(in), VALUE :: sz
  END SUBROUTINE lual_checkversion_
END INTERFACE

!----------------------------------------------------------------------------
!                                                             lual_openlibs
!----------------------------------------------------------------------------

INTERFACE
  ! void luaL_openlibs(lua_State *L)
  SUBROUTINE lual_openlibs(l) BIND(c, name='luaL_openlibs')
    IMPORT :: C_PTR
    IMPLICIT NONE
    TYPE(C_PTR), INTENT(in), VALUE :: l
  END SUBROUTINE lual_openlibs
END INTERFACE

!----------------------------------------------------------------------------
!                                                                  Constains
!----------------------------------------------------------------------------

CONTAINS

!----------------------------------------------------------------------------
!                                                           lua_checkerror
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-19
! summary:  Check error

SUBROUTINE lua_checkerror(ierr, file, routine, lineNo, unitNo)
  INTEGER(I4B), INTENT(IN) :: ierr
  CHARACTER(*), INTENT(IN) :: file
  CHARACTER(*), INTENT(IN) :: routine
  INTEGER(I4B), INTENT(IN) :: lineNo
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitNo

  IF (ierr .NE. LUA_OK) THEN
    CALL Display("ERROR while running lua code :", unitNo=unitNo)
    CALL Display(file, "file :", unitNo=unitNo)
    CALL Display(routine, "routine :", unitNo=unitNo)
    CALL Display(lineNo, "line :", unitNo=unitNo)
  END IF
  ! SELECT CASE (ierr)
  !   ! CASE (LUA_OK)
  !   ! case(LUA_YIELD)
  !   ! case(LUA_ERRRUN)
  !   ! case(LUA_ERRSYNTAX)
  !   ! case(LUA_ERRMEM)
  !   ! case(LUA_ERRERR)
  !   ! case(LUA_ERRFILE)
  ! END SELECT
END SUBROUTINE lua_checkerror

!----------------------------------------------------------------------------
!                                                              lua_getfield
!----------------------------------------------------------------------------

! int lua_getfield(lua_State *L, int idx, const char *k)
FUNCTION lua_getfield(l, idx, k)
        !! Wrapper for `lua_getfield_()` that null-terminates string `k`.
  TYPE(C_PTR), INTENT(in) :: l
  INTEGER, INTENT(in) :: idx
  CHARACTER(*), INTENT(in) :: k
  INTEGER :: lua_getfield

  lua_getfield = lua_getfield_(l, idx, k//C_NULL_CHAR)
END FUNCTION lua_getfield

!----------------------------------------------------------------------------
!                                                              lua_getglobal
!----------------------------------------------------------------------------

! int lua_getglobal(lua_State *L, const char *name)
FUNCTION lua_getglobal(l, name)
        !! Wrapper for `lua_getglobal_()` that null-terminates string `name`.
  TYPE(C_PTR), INTENT(in) :: l
  CHARACTER(*), INTENT(in) :: name
  INTEGER :: lua_getglobal

  lua_getglobal = lua_getglobal_(l, name//C_NULL_CHAR)
END FUNCTION lua_getglobal

!----------------------------------------------------------------------------
!                                                           lua_isboolean
!----------------------------------------------------------------------------

! int lua_isboolean(lua_State *L, int index)
FUNCTION lua_isboolean(l, idx)
        !! Macro replacement that returns whether the stack variable is
        !! boolean.
  TYPE(C_PTR), INTENT(in) :: l
  INTEGER, INTENT(in) :: idx
  INTEGER :: lua_isboolean

  lua_isboolean = 0
  IF (lua_type(l, idx) == LUA_TBOOLEAN) lua_isboolean = 1
END FUNCTION lua_isboolean

!----------------------------------------------------------------------------
!                                                             lua_isfunction
!----------------------------------------------------------------------------

! int lua_isfunction(lua_State *L, int index)
FUNCTION lua_isfunction(l, idx)
        !! Macro replacement that returns whether the stack variable is a
        !! function.
  TYPE(C_PTR), INTENT(in) :: l
  INTEGER, INTENT(in) :: idx
  INTEGER :: lua_isfunction

  lua_isfunction = 0
  IF (lua_type(l, idx) == LUA_TFUNCTION) lua_isfunction = 1
END FUNCTION lua_isfunction

!----------------------------------------------------------------------------
!                                                       lua_islightuserdata
!----------------------------------------------------------------------------

! int lua_islightuserdata(lua_State *L, int index)
FUNCTION lua_islightuserdata(l, idx)
        !! Macro replacement that returns whether the stack variable is
        !! light user data.
  TYPE(C_PTR), INTENT(in) :: l
  INTEGER, INTENT(in) :: idx
  INTEGER :: lua_islightuserdata

  lua_islightuserdata = 0
  IF (lua_type(l, idx) == LUA_TLIGHTUSERDATA) lua_islightuserdata = 1
END FUNCTION lua_islightuserdata

!----------------------------------------------------------------------------
!                                                       lua_islightuserdata
!----------------------------------------------------------------------------

! int lua_isnil(lua_State *L, int index)
FUNCTION lua_isnil(l, idx)
        !! Macro replacement that returns whether the stack variable is
        !! nil.
  TYPE(C_PTR), INTENT(in) :: l
  INTEGER, INTENT(in) :: idx
  INTEGER :: lua_isnil

  lua_isnil = 0
  IF (lua_type(l, idx) == LUA_TNIL) lua_isnil = 1
END FUNCTION lua_isnil

!----------------------------------------------------------------------------
!                                                                lua_isnone
!----------------------------------------------------------------------------

! int lua_isnone(lua_State *L, int index)
FUNCTION lua_isnone(l, idx)
        !! Macro replacement that returns whether the stack variable is
        !! none.
  TYPE(C_PTR), INTENT(in) :: l
  INTEGER, INTENT(in) :: idx
  INTEGER :: lua_isnone

  lua_isnone = 0
  IF (lua_type(l, idx) == LUA_TNONE) lua_isnone = 1
END FUNCTION lua_isnone

!----------------------------------------------------------------------------
!                                                           lua_isnoneornil
!----------------------------------------------------------------------------

! int lua_isnoneornil(lua_State *L, int index)
FUNCTION lua_isnoneornil(l, idx)
        !! Macro replacement that returns whether the stack variable is
        !! none or nil.
  TYPE(C_PTR), INTENT(in) :: l
  INTEGER, INTENT(in) :: idx
  INTEGER :: lua_isnoneornil

  lua_isnoneornil = 0
  IF (lua_type(l, idx) <= 0) lua_isnoneornil = 1
END FUNCTION lua_isnoneornil

!----------------------------------------------------------------------------
!                                                              lua_istable
!----------------------------------------------------------------------------

! int lua_istable(lua_State *L, int index)
FUNCTION lua_istable(l, idx)
        !! Macro replacement that returns whether the stack variable is a
        !! table.
  TYPE(C_PTR), INTENT(in) :: l
  INTEGER, INTENT(in) :: idx
  INTEGER :: lua_istable

  lua_istable = 0
  IF (lua_type(l, idx) == LUA_TTABLE) lua_istable = 1
END FUNCTION lua_istable

!----------------------------------------------------------------------------
!                                                             lua_isthread
!----------------------------------------------------------------------------

! int lua_isthread(lua_State *L, int index)
FUNCTION lua_isthread(l, idx)
        !! Macro replacement that returns whether the stack variable is a
        !! thread.
  TYPE(C_PTR), INTENT(in) :: l
  INTEGER, INTENT(in) :: idx
  INTEGER :: lua_isthread

  lua_isthread = 0
  IF (lua_type(l, idx) == LUA_TTHREAD) lua_isthread = 1
END FUNCTION lua_isthread

!----------------------------------------------------------------------------
!                                                                 lua_pcall
!----------------------------------------------------------------------------

! int lua_pcall(lua_State *L, int nargs, int nresults, int msgh)
FUNCTION lua_pcall(l, nargs, nresults, errfunc)
        !! Macro replacement that calls `lua_pcallk()`.
  TYPE(C_PTR), INTENT(in) :: l
  INTEGER, INTENT(in) :: nargs
  INTEGER, INTENT(in) :: nresults
  INTEGER, INTENT(in) :: errfunc
  INTEGER :: lua_pcall

  lua_pcall = lua_pcallk(l, nargs, nresults, errfunc, &
    &  INT(0, kind=lua_kcontext), C_NULL_FUNPTR)
END FUNCTION lua_pcall

!----------------------------------------------------------------------------
!                                                             lua_tointeger
!----------------------------------------------------------------------------

! lua_Integer lua_tointeger(lua_State *l, int idx)
FUNCTION lua_tointeger(l, idx)
  TYPE(C_PTR), INTENT(in) :: l
  INTEGER, INTENT(in) :: idx
  INTEGER(kind=lua_integer) :: lua_tointeger

  lua_tointeger = lua_tointegerx(l, idx, C_NULL_PTR)
END FUNCTION lua_tointeger

!----------------------------------------------------------------------------
!                                                             lua_toboolean
!----------------------------------------------------------------------------

! logical lua_toboolean(lua_State *L, int index)
FUNCTION lua_toboolean(l, idx)
  TYPE(C_PTR), INTENT(in) :: l
  INTEGER, INTENT(in) :: idx
  LOGICAL :: lua_toboolean

  lua_toboolean = (lua_toboolean_(l, idx) /= 0)
END FUNCTION lua_toboolean

!----------------------------------------------------------------------------
!                                                              lua_tonumber
!----------------------------------------------------------------------------

! lua_Number lua_tonumber(lua_State *l, int idx)
FUNCTION lua_tonumber(l, idx)
  TYPE(C_PTR), INTENT(in) :: l
  INTEGER, INTENT(in) :: idx
  REAL(kind=lua_number) :: lua_tonumber

  lua_tonumber = lua_tonumberx(l, idx, C_NULL_PTR)
END FUNCTION lua_tonumber

!----------------------------------------------------------------------------
!                                                              lua_tostring
!----------------------------------------------------------------------------

! const char *lua_tostring(lua_State *L, int index)
FUNCTION lua_tostring(l, i)
  !! Wrapper that calls `lua_tolstring()` and converts the returned C
  !! pointer to Fortran string. Returns an unallocated character on error.
  TYPE(C_PTR), INTENT(in) :: l
  INTEGER, INTENT(in) :: i
  CHARACTER(:), ALLOCATABLE :: lua_tostring
  TYPE(C_PTR) :: ptr

  ptr = lua_tolstring(l, i, C_NULL_PTR)
  IF (.NOT. C_ASSOCIATED(ptr)) RETURN
  CALL c_f_str_ptr(ptr, lua_tostring)
END FUNCTION lua_tostring

!----------------------------------------------------------------------------
!                                                             lua_typename
!----------------------------------------------------------------------------

! const char *lua_typename(lua_State *L, int tp)
FUNCTION lua_typename(l, tp)
  !! Wrapper that calls `lua_typename_()` and converts the returned C
  !! pointer to Fortran string. Returns an unallocated character on error.
  TYPE(C_PTR), INTENT(in) :: l
  INTEGER, INTENT(in) :: tp
  CHARACTER(:), ALLOCATABLE :: lua_typename
  TYPE(C_PTR) :: ptr

  ptr = lua_typename_(l, tp)
  IF (.NOT. C_ASSOCIATED(ptr)) RETURN
  CALL c_f_str_ptr(ptr, lua_typename)
END FUNCTION lua_typename

!----------------------------------------------------------------------------
!                                                               lual_dofile
!----------------------------------------------------------------------------

! int luaL_dofile(lua_State *L, const char *filename)
FUNCTION lual_dofile(l, fn)
        !! Macro replacement that calls `lual_loadfile()` and `lua_pcall()`.
  TYPE(C_PTR), INTENT(in) :: l
  CHARACTER(*), INTENT(in) :: fn
  INTEGER :: lual_dofile

  lual_dofile = lual_loadfile(l, fn)
  IF (lual_dofile == 0) lual_dofile = lua_pcall(l, 0, LUA_MULTRET, 0)
END FUNCTION lual_dofile

!----------------------------------------------------------------------------
!                                                             lual_dostring
!----------------------------------------------------------------------------

! int luaL_dostring(lua_State *L, const char *str)
FUNCTION lual_dostring(l, str)
        !! Macro replacement that calls `lual_loadstring()` and `lua_pcall()`.
  TYPE(C_PTR), INTENT(in) :: l
  CHARACTER(*), INTENT(in) :: str
  INTEGER :: lual_dostring

  lual_dostring = lual_loadstring(l, str)
  IF (lual_dostring == 0) lual_dostring = lua_pcall(l, 0, LUA_MULTRET, 0)
END FUNCTION lual_dostring

!----------------------------------------------------------------------------
!                                                              luaL_loadfile
!----------------------------------------------------------------------------

! int luaL_loadfile(lua_State *L, const char *filename)
FUNCTION lual_loadfile(l, fn)
        !! Macro replacement that calls `lual_loadfilex()`.
  TYPE(C_PTR), INTENT(in) :: l
  CHARACTER(*), INTENT(in) :: fn
  INTEGER :: lual_loadfile

  lual_loadfile = lual_loadfilex(l, fn//C_NULL_CHAR, C_NULL_PTR)
END FUNCTION lual_loadfile

!----------------------------------------------------------------------------
!                                                         luaL_loadstring
!----------------------------------------------------------------------------

! int luaL_loadstring(lua_State *L, const char *s)
FUNCTION lual_loadstring(l, s)
        !! Wrapper for `lual_loadstring()` that null-terminates the given
        !! string.
  TYPE(C_PTR), INTENT(in) :: l
  CHARACTER(*), INTENT(in) :: s
  INTEGER :: lual_loadstring

  lual_loadstring = lual_loadstring_(l, s//C_NULL_CHAR)
END FUNCTION lual_loadstring

!----------------------------------------------------------------------------
!                                                           lua_pushlstring
!----------------------------------------------------------------------------

! const char *lua_pushlstring(lua_State *L, const char *s, size_t len)
FUNCTION lua_pushlstring(l, s, len)
  TYPE(C_PTR), INTENT(in) :: l
  CHARACTER(*), INTENT(in) :: s
  INTEGER(kind=C_SIZE_T), INTENT(in) :: len
  TYPE(C_PTR) :: lua_pushlstring

  lua_pushlstring = lua_pushlstring_(l, s//C_NULL_CHAR, len)
END FUNCTION lua_pushlstring

!----------------------------------------------------------------------------
!                                                            lua_pushstring
!----------------------------------------------------------------------------

! const char *lua_pushstring(lua_State *L, const char *s)
FUNCTION lua_pushstring(l, s)
  TYPE(C_PTR), INTENT(in) :: l
  CHARACTER(*), INTENT(in) :: s
  TYPE(C_PTR) :: lua_pushstring

  lua_pushstring = lua_pushstring_(l, s//C_NULL_CHAR)
END FUNCTION lua_pushstring

!----------------------------------------------------------------------------
!                                                                  lua_call
!----------------------------------------------------------------------------

! void lua_call(lua_State *L, int nargs, int nresults)
SUBROUTINE lua_call(l, nargs, nresults)
  TYPE(C_PTR), INTENT(in) :: l
  INTEGER, INTENT(in) :: nargs
  INTEGER, INTENT(in) :: nresults

  CALL lua_callk(l, nargs, nresults, INT(0, kind=C_SIZE_T), C_NULL_FUNPTR)
END SUBROUTINE lua_call

!----------------------------------------------------------------------------
!                                                              lua_newtable
!----------------------------------------------------------------------------
! void lua_newtable(lua_State *L)
SUBROUTINE lua_newtable(l)
  TYPE(C_PTR), INTENT(in) :: l

  CALL lua_createtable(l, 0, 0)
END SUBROUTINE lua_newtable

!----------------------------------------------------------------------------
!                                                                  lua_pop
!----------------------------------------------------------------------------

! void lua_pop(lua_State *l, int n)
SUBROUTINE lua_pop(l, n)
  TYPE(C_PTR), INTENT(in) :: l
  INTEGER, INTENT(in) :: n

  CALL lua_settop(l, -n - 1)
END SUBROUTINE lua_pop

!----------------------------------------------------------------------------
!                                                          lua_pushcfunction
!----------------------------------------------------------------------------

! void lua_pushcfunction(lua_State *L, lua_CFunction f)
SUBROUTINE lua_pushcfunction(l, f)
  TYPE(C_PTR), INTENT(in) :: l
  TYPE(C_FUNPTR), INTENT(in) :: f

  CALL lua_pushcclosure(l, f, 0)
END SUBROUTINE lua_pushcfunction

!----------------------------------------------------------------------------
!                                                               lua_register
!----------------------------------------------------------------------------

! void lua_register(lua_State *L, const char *name, lua_CFunction f)
SUBROUTINE lua_register(l, n, f)
        !! Macro replacement.
  TYPE(C_PTR), INTENT(in) :: l
  CHARACTER(*), INTENT(in) :: n
  TYPE(C_FUNPTR), INTENT(in) :: f

  CALL lua_pushcfunction(l, f)
  CALL lua_setglobal_(l, n//C_NULL_CHAR)
END SUBROUTINE lua_register

!----------------------------------------------------------------------------
!                                                               lua_setfield
!----------------------------------------------------------------------------

! void lua_setfield(lua_State *L, int idx, const char *k)
SUBROUTINE lua_setfield(l, idx, k)
  TYPE(C_PTR), INTENT(in) :: l
  INTEGER, INTENT(in) :: idx
  CHARACTER(*), INTENT(in) :: k

  CALL lua_setfield_(l, idx, k//C_NULL_CHAR)
END SUBROUTINE lua_setfield

!----------------------------------------------------------------------------
!                                                             lua_setglobal
!----------------------------------------------------------------------------

! int lua_getglobal(lua_State *L, const char *name)
SUBROUTINE lua_setglobal(l, name)
  TYPE(C_PTR), INTENT(in) :: l
  CHARACTER(kind=C_CHAR), INTENT(in) :: name

  CALL lua_setglobal_(l, name//C_NULL_CHAR)
END SUBROUTINE lua_setglobal

!----------------------------------------------------------------------------
!                                                               c_f_str_ptr
!----------------------------------------------------------------------------

SUBROUTINE c_f_str_ptr(c_str, f_str)
  !! Copies a C string, passed as a C pointer, to a Fortran string.
  TYPE(C_PTR), INTENT(in) :: c_str
  CHARACTER(:), ALLOCATABLE, INTENT(out) :: f_str

  CHARACTER(kind=C_CHAR), POINTER :: ptrs(:)
  INTEGER(kind=C_SIZE_T) :: i, sz

  copy_block: BLOCK
    IF (.NOT. C_ASSOCIATED(c_str)) EXIT copy_block
    sz = c_strlen(c_str)
    IF (sz < 0) EXIT copy_block
    CALL C_F_POINTER(c_str, ptrs, [sz])
    ALLOCATE (CHARACTER(sz) :: f_str)

    DO i = 1, sz
      f_str(i:i) = ptrs(i)
    END DO

    RETURN
  END BLOCK copy_block

  IF (.NOT. ALLOCATED(f_str)) f_str = ''
END SUBROUTINE c_f_str_ptr

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------
END MODULE LuaInterface
