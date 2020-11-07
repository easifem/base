program 
use stringifor
implcit none

type(string) :: astring
type(string) :: strings(3)

astring = '0123456789'
print "(A)", astring%reverse()//'' ! print "9876543210"

astring = 'Hello World'
print "(A)", astring%replace(old='World', new='People')//'' ! print "Hello People"

astring = 'Hello World'
strings = astring%partition(sep='lo Wo')
print "(A)", 'Before sep: "'//strings(1)//'"' ! print "Hel"
print "(A)", 'Sep itself: "'//strings(2)//'"' ! print "lo Wo"
print "(A)", 'After sep:  "'//strings(3)//'"' ! print "rld"

strings(1) = 'one'
strings(2) = 'two'
strings(3) = 'three'
print "(A)", astring%join(strings)//''          ! print "onetwothree"
print "(A)", astring%join(strings, sep='-')//'' ! print "one-two-three"

astring = ' a StraNgE caSe var'
print "(A)", astring%camelcase()//'' ! print " AStrangeCaseVar"
print "(A)", astring%snakecase()//'' ! print " a_strange_case_var"
print "(A)", astring%startcase()//'' ! print " A Strange Case Var"
    implicit none
    
end program use stringifor
type(string) :: astring
type(string) :: strings(3)

astring = '0123456789'
print "(A)", astring%reverse()//'' ! print "9876543210"

astring = 'Hello World'
print "(A)", astring%replace(old='World', new='People')//'' ! print "Hello People"

astring = 'Hello World'
strings = astring%partition(sep='lo Wo')
print "(A)", 'Before sep: "'//strings(1)//'"' ! print "Hel"
print "(A)", 'Sep itself: "'//strings(2)//'"' ! print "lo Wo"
print "(A)", 'After sep:  "'//strings(3)//'"' ! print "rld"

strings(1) = 'one'
strings(2) = 'two'
strings(3) = 'three'
print "(A)", astring%join(strings)//''          ! print "onetwothree"
print "(A)", astring%join(strings, sep='-')//'' ! print "one-two-three"

astring = ' a StraNgE caSe var'
print "(A)", astring%camelcase()//'' ! print " AStrangeCaseVar"
print "(A)", astring%snakecase()//'' ! print " a_strange_case_var"
print "(A)", astring%startcase()//'' ! print " A Strange Case Var"