!< FACE, Fortran Ansi Colors Environment.
MODULE face
!< FACE, Fortran Ansi Colors Environment.
USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT32

IMPLICIT NONE
PRIVATE
PUBLIC :: colorize
PUBLIC :: colors_samples
PUBLIC :: styles_samples
PUBLIC :: ASCII
PUBLIC :: UCS4

INTERFACE colorize
#if defined ASCII_SUPPORTED && defined ASCII_NEQ_DEFAULT
  MODULE PROCEDURE colorize_ascii
  MODULE PROCEDURE colorize_default
#else
  MODULE PROCEDURE colorize_default
#endif
#ifdef UCS4_SUPPORTED
  MODULE PROCEDURE colorize_ucs4
#endif
end interface

! kind parameters
#ifdef ASCII_SUPPORTED
INTEGER, PARAMETER :: ASCII = SELECTED_CHAR_KIND('ascii') !< ASCII character set kind.
#else
INTEGER, PARAMETER :: ASCII = SELECTED_CHAR_KIND('default') !< ASCII character set kind.
#endif
#ifdef UCS4_SUPPORTED
INTEGER, PARAMETER :: UCS4 = SELECTED_CHAR_KIND('iso_10646') !< Unicode character set kind.
#else
INTEGER, PARAMETER :: UCS4 = SELECTED_CHAR_KIND('default') !< Unicode character set kind.
#endif
! parameters
CHARACTER(26), PARAMETER :: UPPER_ALPHABET = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' !< Upper case alphabet.
CHARACTER(26), PARAMETER :: LOWER_ALPHABET = 'abcdefghijklmnopqrstuvwxyz' !< Lower case alphabet.
CHARACTER(1), PARAMETER :: NL = NEW_LINE('a') !< New line character.
CHARACTER(1), PARAMETER :: ESCAPE = ACHAR(27) !< "\" character.
! codes
CHARACTER(2), PARAMETER :: CODE_START = ESCAPE//'[' !< Start ansi code, "\[".
CHARACTER(1), PARAMETER :: CODE_END = 'm' !< End ansi code, "m".
CHARACTER(4), PARAMETER :: CODE_CLEAR = CODE_START//'0'//CODE_END !< Clear all styles, "\[0m".
! styles codes
CHARACTER(17), PARAMETER :: STYLES(1:2, 1:16) = RESHAPE([ &
                       'BOLD_ON          ', '1                ', & !  Bold on.
                    'ITALICS_ON       ', '3                ', & !  Italics on.
                  'UNDERLINE_ON     ', '4                ', & !  Underline on.
'INVERSE_ON       ', '7                ', & !  Inverse on: reverse foreground and background colors.
              'STRIKETHROUGH_ON ', '9                ', & !  Strikethrough on.
                      'BOLD_OFF         ', '22               ', & !  Bold off.
                   'ITALICS_OFF      ', '23               ', & !  Italics off.
                 'UNDERLINE_OFF    ', '24               ', & !  Underline off.
'INVERSE_OFF      ', '27               ', & !  Inverse off: reverse foreground and background colors.
             'STRIKETHROUGH_OFF', '29               ', & !  Strikethrough off.
                     'FRAMED_ON        ', '51               ', & !  Framed on.
                  'ENCIRCLED_ON     ', '52               ', & !  Encircled on.
                  'OVERLINED_ON     ', '53               ', & !  Overlined on.
                    'FRAMED_OFF       ', '54               ', & !  Framed off.
                 'ENCIRCLED_OFF    ', '54               ', & !  Encircled off.
                  'OVERLINED_OFF    ', '55               ' & !  Overlined off.
                                                        ], [2, 16]) !< Styles.
! colors codes
CHARACTER(15), PARAMETER :: COLORS_FG(1:2, 1:17) = RESHAPE([ &
                             'BLACK          ', '30             ', & !  Black.
                               'RED            ', '31             ', & !  Red.
                             'GREEN          ', '32             ', & !  Green.
                            'YELLOW         ', '33             ', & !  Yellow.
                              'BLUE           ', '34             ', & !  Blue.
                           'MAGENTA        ', '35             ', & !  Magenta.
                              'CYAN           ', '36             ', & !  Cyan.
                             'WHITE          ', '37             ', & !  White.
                   'DEFAULT        ', '39             ', & !  Default (white).
                     'BLACK_INTENSE  ', '90             ', & !  Black intense.
                       'RED_INTENSE    ', '91             ', & !  Red intense.
                     'GREEN_INTENSE  ', '92             ', & !  Green intense.
                    'YELLOW_INTENSE ', '93             ', & !  Yellow intense.
                      'BLUE_INTENSE   ', '94             ', & !  Blue intense.
                   'MAGENTA_INTENSE', '95             ', & !  Magenta intense.
                      'CYAN_INTENSE   ', '96             ', & !  Cyan intense.
                      'WHITE_INTENSE  ', '97             ' & !  White intense.
                                                           ], [2, 17]) !< Foreground colors.
CHARACTER(15), PARAMETER :: COLORS_BG(1:2, 1:17) = RESHAPE([ &
                             'BLACK          ', '40             ', & !  Black.
                               'RED            ', '41             ', & !  Red.
                             'GREEN          ', '42             ', & !  Green.
                            'YELLOW         ', '43             ', & !  Yellow.
                              'BLUE           ', '44             ', & !  Blue.
                           'MAGENTA        ', '45             ', & !  Magenta.
                              'CYAN           ', '46             ', & !  Cyan.
                             'WHITE          ', '47             ', & !  White.
                   'DEFAULT        ', '49             ', & !  Default (black).
                     'BLACK_INTENSE  ', '100            ', & !  Black intense.
                       'RED_INTENSE    ', '101            ', & !  Red intense.
                     'GREEN_INTENSE  ', '102            ', & !  Green intense.
                    'YELLOW_INTENSE ', '103            ', & !  Yellow intense.
                      'BLUE_INTENSE   ', '104            ', & !  Blue intense.
                   'MAGENTA_INTENSE', '105            ', & !  Magenta intense.
                      'CYAN_INTENSE   ', '106            ', & !  Cyan intense.
                      'WHITE_INTENSE  ', '107            ' & !  White intense.
                                                           ], [2, 17]) !< Background colors.
CONTAINS
! public procedures
SUBROUTINE colors_samples()
  !< Print to standard output all colors samples.
  INTEGER(INT32) :: c !< Counter.

  PRINT '(A)', colorize('Foreground colors samples', color_fg='red_intense')
  DO c = 1, SIZE(COLORS_FG, dim=2)
      print '(A)', '  colorize("'//COLORS_FG(1, c)//'", color_fg="'//COLORS_FG(1, c)//'") => '//&
      colorize(COLORS_FG(1, c), color_fg=COLORS_FG(1, c))// &
         ' code: '//colorize(trim(COLORS_FG(2, c)), color_fg=COLORS_FG(1, c), style='inverse_on')
  END DO
  PRINT '(A)', colorize('Background colors samples', color_fg='red_intense')
  DO c = 1, SIZE(COLORS_BG, dim=2)
      print '(A)', '  colorize("'//COLORS_BG(1, c)//'", color_bg="'//COLORS_BG(1, c)//'") => '//&
      colorize(COLORS_BG(1, c), color_bg=COLORS_BG(1, c))// &
         ' code: '//colorize(trim(COLORS_BG(2, c)), color_bg=COLORS_BG(1, c), style='inverse_on')
  END DO
end subroutine colors_samples

SUBROUTINE styles_samples()
  !< Print to standard output all styles samples.
  INTEGER(INT32) :: s !< Counter.

  PRINT '(A)', colorize('Styles samples', color_fg='red_intense')
  DO s = 1, SIZE(STYLES, dim=2)
      print '(A)', '  colorize("'//STYLES(1, s)//'", style="'//STYLES(1, s)//'") => '//&
      colorize(STYLES(1, s), style=STYLES(1, s))// &
         ' code: '//colorize(trim(STYLES(2, s)), color_fg='magenta', style='inverse_on')
  END DO
end subroutine styles_samples

! private procedures
   pure function colorize_ascii(string, color_fg, color_bg, style) result(colorized)
  !< Colorize and stylize strings, ASCII kind.
  CHARACTER(len=*, kind=ASCII), INTENT(in) :: string !< Input string.
  CHARACTER(len=*), INTENT(in), OPTIONAL :: color_fg !< Foreground color definition.
  CHARACTER(len=*), INTENT(in), OPTIONAL :: color_bg !< Background color definition.
  CHARACTER(len=*), INTENT(in), OPTIONAL :: style !< Style definition.
  CHARACTER(len=:, kind=ASCII), ALLOCATABLE :: colorized !< Colorized string.
  CHARACTER(len=:, kind=ASCII), ALLOCATABLE :: buffer !< Temporary buffer.
  INTEGER(INT32) :: i !< Counter.

  colorized = string
  IF (PRESENT(color_fg)) THEN
    i = color_index(upper(color_fg))
    IF (i > 0) THEN
      buffer = CODE_START//TRIM(COLORS_FG(2, i))//CODE_END
      colorized = buffer//colorized
      buffer = CODE_CLEAR
      colorized = colorized//buffer
    END IF
  END IF
  IF (PRESENT(color_bg)) THEN
    i = color_index(upper(color_bg))
    IF (i > 0) THEN
      buffer = CODE_START//TRIM(COLORS_BG(2, i))//CODE_END
      colorized = buffer//colorized
      buffer = CODE_CLEAR
      colorized = colorized//buffer
    END IF
  END IF
  IF (PRESENT(style)) THEN
    i = style_index(upper(style))
    IF (i > 0) THEN
      buffer = CODE_START//TRIM(STYLES(2, i))//CODE_END
      colorized = buffer//colorized
      buffer = CODE_CLEAR
      colorized = colorized//buffer
    END IF
  END IF
end function colorize_ascii

   pure function colorize_default(string, color_fg, color_bg, style) result(colorized)
  !< Colorize and stylize strings, DEFAULT kind.
  CHARACTER(len=*), INTENT(in) :: string !< Input string.
  CHARACTER(len=*), INTENT(in), OPTIONAL :: color_fg !< Foreground color definition.
  CHARACTER(len=*), INTENT(in), OPTIONAL :: color_bg !< Background color definition.
  CHARACTER(len=*), INTENT(in), OPTIONAL :: style !< Style definition.
  CHARACTER(len=:), ALLOCATABLE :: colorized !< Colorized string.
  INTEGER(INT32) :: i !< Counter.

  colorized = string
  IF (PRESENT(color_fg)) THEN
    i = color_index(upper(color_fg))
      if (i>0) colorized = CODE_START//trim(COLORS_FG(2, i))//CODE_END//colorized//CODE_CLEAR
  END IF
  IF (PRESENT(color_bg)) THEN
    i = color_index(upper(color_bg))
      if (i>0) colorized = CODE_START//trim(COLORS_BG(2, i))//CODE_END//colorized//CODE_CLEAR
  END IF
  IF (PRESENT(style)) THEN
    i = style_index(upper(style))
      if (i>0) colorized = CODE_START//trim(STYLES(2, i))//CODE_END//colorized//CODE_CLEAR
  END IF
end function colorize_default

   pure function colorize_ucs4(string, color_fg, color_bg, style) result(colorized)
  !< Colorize and stylize strings, UCS4 kind.
  CHARACTER(len=*, kind=UCS4), INTENT(in) :: string !< Input string.
  CHARACTER(len=*), INTENT(in), OPTIONAL :: color_fg !< Foreground color definition.
  CHARACTER(len=*), INTENT(in), OPTIONAL :: color_bg !< Background color definition.
  CHARACTER(len=*), INTENT(in), OPTIONAL :: style !< Style definition.
  CHARACTER(len=:, kind=UCS4), ALLOCATABLE :: colorized !< Colorized string.
  CHARACTER(len=:, kind=UCS4), ALLOCATABLE :: buffer !< Temporary buffer.
  INTEGER(INT32) :: i !< Counter.

  colorized = string
  IF (PRESENT(color_fg)) THEN
    i = color_index(upper(color_fg))
    IF (i > 0) THEN
      buffer = CODE_START//TRIM(COLORS_FG(2, i))//CODE_END
      colorized = buffer//colorized
      buffer = CODE_CLEAR
      colorized = colorized//buffer
    END IF
  END IF
  IF (PRESENT(color_bg)) THEN
    i = color_index(upper(color_bg))
    IF (i > 0) THEN
      buffer = CODE_START//TRIM(COLORS_BG(2, i))//CODE_END
      colorized = buffer//colorized
      buffer = CODE_CLEAR
      colorized = colorized//buffer
    END IF
  END IF
  IF (PRESENT(style)) THEN
    i = style_index(upper(style))
    IF (i > 0) THEN
      buffer = CODE_START//TRIM(STYLES(2, i))//CODE_END
      colorized = buffer//colorized
      buffer = CODE_CLEAR
      colorized = colorized//buffer
    END IF
  END IF
end function colorize_ucs4

ELEMENTAL FUNCTION color_index(color)
  !< Return the array-index corresponding to the queried color.
  !<
  !< @note Because Foreground and backround colors lists share the same name, no matter what array is used to find the color index.
  !< Thus, the foreground array is used.
  CHARACTER(len=*), INTENT(in) :: color !< Color definition.
  INTEGER(INT32) :: color_index !< Index into the colors arrays.
  INTEGER(INT32) :: c !< Counter.

  color_index = 0
  DO c = 1, SIZE(COLORS_FG, dim=2)
    IF (TRIM(COLORS_FG(1, c)) == TRIM(ADJUSTL(color))) THEN
      color_index = c
      EXIT
    END IF
  END DO
end function color_index

ELEMENTAL FUNCTION style_index(style)
  !< Return the array-index corresponding to the queried style.
  CHARACTER(len=*), INTENT(in) :: style !< Style definition.
  INTEGER(INT32) :: style_index !< Index into the styles array.
  INTEGER(INT32) :: s !< Counter.

  style_index = 0
  DO s = 1, SIZE(STYLES, dim=2)
    IF (TRIM(STYLES(1, s)) == TRIM(ADJUSTL(style))) THEN
      style_index = s
      EXIT
    END IF
  END DO
end function style_index

ELEMENTAL FUNCTION upper(string)
  !< Return a string with all uppercase characters.
  CHARACTER(len=*), INTENT(in) :: string !< Input string.
  CHARACTER(len=LEN(string)) :: upper !< Upper case string.
  INTEGER :: n1 !< Characters counter.
  INTEGER :: n2 !< Characters counter.

  upper = string
  DO n1 = 1, LEN(string)
    n2 = INDEX(LOWER_ALPHABET, string(n1:n1))
    IF (n2 > 0) upper(n1:n1) = UPPER_ALPHABET(n2:n2)
  END DO
end function upper
endmodule face
