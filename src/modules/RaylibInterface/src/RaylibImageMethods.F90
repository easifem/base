! raylib.f90
!
! A collection of auto-generated Fortran 2018 interface bindings to
! raylib 5.1.
!
! Author:  Philipp Engel
! Licence: ISC

MODULE RaylibImageMethods
USE, INTRINSIC :: ISO_C_BINDING
USE RaylibTypes
USE RaylibEnums
IMPLICIT NONE
PRIVATE

PUBLIC :: image_alpha_clear
PUBLIC :: image_alpha_crop
PUBLIC :: image_alpha_mask
PUBLIC :: image_alpha_premultiply
PUBLIC :: image_blur_gaussian
PUBLIC :: image_clear_background
PUBLIC :: image_color_brightness
PUBLIC :: image_color_contrast
PUBLIC :: image_color_grayscale
PUBLIC :: image_color_invert
PUBLIC :: image_color_replace
PUBLIC :: image_color_tint
PUBLIC :: image_copy
PUBLIC :: image_crop
PUBLIC :: image_dither
PUBLIC :: image_draw
PUBLIC :: image_draw_circle
PUBLIC :: image_draw_circle_lines
PUBLIC :: image_draw_circle_lines_v
PUBLIC :: image_draw_circle_v
PUBLIC :: image_draw_line
PUBLIC :: image_draw_line_v
PUBLIC :: image_draw_pixel
PUBLIC :: image_draw_pixel_v
PUBLIC :: image_draw_rectangle
PUBLIC :: image_draw_rectangle_lines
PUBLIC :: image_draw_rectangle_rec
PUBLIC :: image_draw_rectangle_v
PUBLIC :: image_draw_text
PUBLIC :: image_draw_text_ex
PUBLIC :: image_flip_horizontal
PUBLIC :: image_flip_vertical
PUBLIC :: image_format
PUBLIC :: image_from_image
PUBLIC :: image_kernel_convolution
PUBLIC :: image_mipmaps
PUBLIC :: image_resize
PUBLIC :: image_resize_canvas
PUBLIC :: image_resize_nn
PUBLIC :: image_rotate
PUBLIC :: image_rotate_ccw
PUBLIC :: image_rotate_cw
PUBLIC :: image_text
PUBLIC :: image_text_ex
PUBLIC :: image_to_pot

INTERFACE

  ! void ImageBlurGaussian(Image *image, int blurSize)
  SUBROUTINE image_blur_gaussian(image, blur_size) BIND(c, name= &
                                                        'ImageBlurGaussian')
    IMPORT :: C_INT, image_type
    IMPLICIT NONE
    TYPE(image_type), INTENT(inout) :: image
    INTEGER(kind=C_INT), INTENT(in), VALUE :: blur_size
  END SUBROUTINE image_blur_gaussian

  ! void ImageAlphaClear(Image *image, Color color, float threshold)
        subroutine image_alpha_clear(image, color, threshold) bind(c, name='ImageAlphaClear')
    IMPORT :: C_FLOAT, color_type, image_type
    IMPLICIT NONE
    TYPE(image_type), INTENT(inout) :: image
    TYPE(color_type), INTENT(in), VALUE :: color
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: threshold
  END SUBROUTINE image_alpha_clear

  ! void ImageAlphaCrop(Image *image, float threshold)
  SUBROUTINE image_alpha_crop(image, threshold) BIND(c, name='ImageAlphaCrop')
    IMPORT :: C_FLOAT, image_type
    IMPLICIT NONE
    TYPE(image_type), INTENT(inout) :: image
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: threshold
  END SUBROUTINE image_alpha_crop

  ! void ImageAlphaMask(Image *image, Image alphaMask)
 SUBROUTINE image_alpha_mask(image, alpha_mask) BIND(c, name='ImageAlphaMask')
    IMPORT :: image_type
    IMPLICIT NONE
    TYPE(image_type), INTENT(inout) :: image
    TYPE(image_type), INTENT(in), VALUE :: alpha_mask
  END SUBROUTINE image_alpha_mask

  ! void ImageAlphaPremultiply(Image *image)
        subroutine image_alpha_premultiply(image) bind(c, name='ImageAlphaPremultiply')
    IMPORT :: image_type
    IMPLICIT NONE
    TYPE(image_type), INTENT(inout) :: image
  END SUBROUTINE image_alpha_premultiply

  ! void ImageClearBackground(Image *dst, Color color)
        subroutine image_clear_background(dst, color) bind(c, name='ImageClearBackground')
    IMPORT :: color_type, image_type
    IMPLICIT NONE
    TYPE(image_type), INTENT(inout) :: dst
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE image_clear_background

  ! void ImageColorBrightness(Image *image, int brightness)
        subroutine image_color_brightness(image, brightness) bind(c, name='ImageColorBrightness')
    IMPORT :: C_INT, image_type
    IMPLICIT NONE
    TYPE(image_type), INTENT(inout) :: image
    INTEGER(kind=C_INT), INTENT(in), VALUE :: brightness
  END SUBROUTINE image_color_brightness

  ! void ImageColorContrast(Image *image, float contrast)
        subroutine image_color_contrast(image, contrast) bind(c, name='ImageColorContrast')
    IMPORT :: C_FLOAT, image_type
    IMPLICIT NONE
    TYPE(image_type), INTENT(inout) :: image
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: contrast
  END SUBROUTINE image_color_contrast

  ! void ImageColorGrayscale(Image *image)
  SUBROUTINE image_color_grayscale(image) BIND(c, name='ImageColorGrayscale')
    IMPORT :: image_type
    IMPLICIT NONE
    TYPE(image_type), INTENT(inout) :: image
  END SUBROUTINE image_color_grayscale

  ! void ImageColorInvert(Image *image)
  SUBROUTINE image_color_invert(image) BIND(c, name='ImageColorInvert')
    IMPORT :: image_type
    IMPLICIT NONE
    TYPE(image_type), INTENT(inout) :: image
  END SUBROUTINE image_color_invert

  ! void ImageColorReplace(Image *image, Color color, Color replace)
        subroutine image_color_replace(image, color, replace) bind(c, name='ImageColorReplace')
    IMPORT :: color_type, image_type
    IMPLICIT NONE
    TYPE(image_type), INTENT(inout) :: image
    TYPE(color_type), INTENT(in), VALUE :: color
    TYPE(color_type), INTENT(in), VALUE :: replace
  END SUBROUTINE image_color_replace

  ! void ImageColorTint(Image *image, Color color)
  SUBROUTINE image_color_tint(image, color) BIND(c, name='ImageColorTint')
    IMPORT :: color_type, image_type
    IMPLICIT NONE
    TYPE(image_type), INTENT(inout) :: image
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE image_color_tint

  ! Image ImageCopy(Image image)
  FUNCTION image_copy(image) BIND(c, name='ImageCopy')
    IMPORT :: image_type
    IMPLICIT NONE
    TYPE(image_type), INTENT(in), VALUE :: image
    TYPE(image_type) :: image_copy
  END FUNCTION image_copy

  ! void ImageCrop(Image *image, Rectangle crop)
  SUBROUTINE image_crop(image, crop) BIND(c, name='ImageCrop')
    IMPORT :: image_type, rectangle_type
    IMPLICIT NONE
    TYPE(image_type), INTENT(inout) :: image
    TYPE(rectangle_type), INTENT(in), VALUE :: crop
  END SUBROUTINE image_crop

  ! void ImageDither(Image *image, int rBpp, int gBpp, int bBpp, int aBpp)
        subroutine image_dither(image, r_bpp, g_bpp, b_bpp, a_bpp) bind(c, name='ImageDither')
    IMPORT :: C_INT, image_type
    IMPLICIT NONE
    TYPE(image_type), INTENT(inout) :: image
    INTEGER(kind=C_INT), INTENT(in), VALUE :: r_bpp
    INTEGER(kind=C_INT), INTENT(in), VALUE :: g_bpp
    INTEGER(kind=C_INT), INTENT(in), VALUE :: b_bpp
    INTEGER(kind=C_INT), INTENT(in), VALUE :: a_bpp
  END SUBROUTINE image_dither

  ! void ImageDraw(Image *dst, Image src, Rectangle srcRec, Rectangle dstRec, Color tint)
        subroutine image_draw(dst, src, src_rec, dst_rec, tint) bind(c, name='ImageDraw')
    IMPORT :: color_type, image_type, rectangle_type
    IMPLICIT NONE
    TYPE(image_type), INTENT(inout) :: dst
    TYPE(image_type), INTENT(in), VALUE :: src
    TYPE(rectangle_type), INTENT(in), VALUE :: src_rec
    TYPE(rectangle_type), INTENT(in), VALUE :: dst_rec
    TYPE(color_type), INTENT(in), VALUE :: tint
  END SUBROUTINE image_draw

  ! void ImageDrawCircle(Image *dst, int centerX, int centerY, int radius, Color color)
        subroutine image_draw_circle(dst, center_x, center_y, radius, color) bind(c, name='ImageDrawCircle')
    IMPORT :: C_INT, color_type, image_type
    IMPLICIT NONE
    TYPE(image_type), INTENT(inout) :: dst
    INTEGER(kind=C_INT), INTENT(in), VALUE :: center_x
    INTEGER(kind=C_INT), INTENT(in), VALUE :: center_y
    INTEGER(kind=C_INT), INTENT(in), VALUE :: radius
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE image_draw_circle

  ! void ImageDrawCircleLines(Image *dst, int centerX, int centerY, int radius, Color color)
        subroutine image_draw_circle_lines(dst, center_x, center_y, radius, color) bind(c, name='ImageDrawCircleLines')
    IMPORT :: C_INT, color_type, image_type
    IMPLICIT NONE
    TYPE(image_type), INTENT(inout) :: dst
    INTEGER(kind=C_INT), INTENT(in), VALUE :: center_x
    INTEGER(kind=C_INT), INTENT(in), VALUE :: center_y
    INTEGER(kind=C_INT), INTENT(in), VALUE :: radius
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE image_draw_circle_lines

  ! void ImageDrawCircleLinesV(Image *dst, Vector2 center, int radius, Color color)
        subroutine image_draw_circle_lines_v(dst, center, radius, color) bind(c, name='ImageDrawCircleLinesV')
    IMPORT :: C_INT, color_type, image_type, vector2_type
    IMPLICIT NONE
    TYPE(image_type), INTENT(inout) :: dst
    TYPE(vector2_type), INTENT(in), VALUE :: center
    INTEGER(kind=C_INT), INTENT(in), VALUE :: radius
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE image_draw_circle_lines_v

  ! void ImageDrawCircleV(Image *dst, Vector2 center, int radius, Color color)
        subroutine image_draw_circle_v(dst, center, radius, color) bind(c, name='ImageDrawCircleV')
    IMPORT :: C_INT, color_type, image_type, vector2_type
    IMPLICIT NONE
    TYPE(image_type), INTENT(inout) :: dst
    TYPE(vector2_type), INTENT(in), VALUE :: center
    INTEGER(kind=C_INT), INTENT(in), VALUE :: radius
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE image_draw_circle_v

  ! void ImageDrawLine(Image *dst, int startPosX, int startPosY, int endPosX, int endPosY, Color color)
        subroutine image_draw_line(dst, start_pos_x, start_pos_y, end_pos_x, end_pos_y, color) &
    BIND(c, name='ImageDrawLine')
    IMPORT :: C_INT, color_type, image_type
    IMPLICIT NONE
    TYPE(image_type), INTENT(inout) :: dst
    INTEGER(kind=C_INT), INTENT(in), VALUE :: start_pos_x
    INTEGER(kind=C_INT), INTENT(in), VALUE :: start_pos_y
    INTEGER(kind=C_INT), INTENT(in), VALUE :: end_pos_x
    INTEGER(kind=C_INT), INTENT(in), VALUE :: end_pos_y
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE image_draw_line

  ! void ImageDrawLineV(Image *dst, Vector2 start, Vector2 end, Color color)
        subroutine image_draw_line_v(dst, start, end, color) bind(c, name='ImageDrawLineV')
    IMPORT :: color_type, image_type, vector2_type
    IMPLICIT NONE
    TYPE(image_type), INTENT(inout) :: dst
    TYPE(vector2_type), INTENT(in), VALUE :: start
    TYPE(vector2_type), INTENT(in), VALUE :: END
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE image_draw_line_v

  ! void ImageDrawPixel(Image *dst, int posX, int posY, Color color)
        subroutine image_draw_pixel(dst, pos_x, pos_y, color) bind(c, name='ImageDrawPixel')
    IMPORT :: C_INT, color_type, image_type
    IMPLICIT NONE
    TYPE(image_type), INTENT(inout) :: dst
    INTEGER(kind=C_INT), INTENT(in), VALUE :: pos_x
    INTEGER(kind=C_INT), INTENT(in), VALUE :: pos_y
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE image_draw_pixel

  ! void ImageDrawPixelV(Image *dst, Vector2 position, Color color)
        subroutine image_draw_pixel_v(dst, position, color) bind(c, name='ImageDrawPixelV')
    IMPORT :: color_type, image_type, vector2_type
    IMPLICIT NONE
    TYPE(image_type), INTENT(inout) :: dst
    TYPE(vector2_type), INTENT(in), VALUE :: position
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE image_draw_pixel_v

  ! void ImageDrawRectangle(Image *dst, int posX, int posY, int width, int height, Color color)
        subroutine image_draw_rectangle(dst, pos_x, pos_y, width, height, color) bind(c, name='ImageDrawRectangle')
    IMPORT :: C_INT, color_type, image_type
    IMPLICIT NONE
    TYPE(image_type), INTENT(inout) :: dst
    INTEGER(kind=C_INT), INTENT(in), VALUE :: pos_x
    INTEGER(kind=C_INT), INTENT(in), VALUE :: pos_y
    INTEGER(kind=C_INT), INTENT(in), VALUE :: width
    INTEGER(kind=C_INT), INTENT(in), VALUE :: height
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE image_draw_rectangle

  ! void ImageDrawRectangleLines(Image *dst, Rectangle rec, int thick, Color color)
        subroutine image_draw_rectangle_lines(dst, rec, thick, color) bind(c, name='ImageDrawRectangleLines')
    IMPORT :: C_INT, color_type, image_type, rectangle_type
    IMPLICIT NONE
    TYPE(image_type), INTENT(inout) :: dst
    TYPE(rectangle_type), INTENT(in), VALUE :: rec
    INTEGER(kind=C_INT), INTENT(in), VALUE :: thick
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE image_draw_rectangle_lines

  ! void ImageDrawRectangleRec(Image *dst, Rectangle rec, Color color)
        subroutine image_draw_rectangle_rec(dst, rec, color) bind(c, name='ImageDrawRectangleRec')
    IMPORT :: color_type, image_type, rectangle_type
    IMPLICIT NONE
    TYPE(image_type), INTENT(inout) :: dst
    TYPE(rectangle_type), INTENT(in), VALUE :: rec
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE image_draw_rectangle_rec

  ! void ImageDrawRectangleV(Image *dst, Vector2 position, Vector2 size, Color color)
        subroutine image_draw_rectangle_v(dst, position, size, color) bind(c, name='ImageDrawRectangleV')
    IMPORT :: color_type, image_type, vector2_type
    IMPLICIT NONE
    TYPE(image_type), INTENT(inout) :: dst
    TYPE(vector2_type), INTENT(in), VALUE :: position
    TYPE(vector2_type), INTENT(in), VALUE :: size
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE image_draw_rectangle_v

  ! void ImageDrawText(Image *dst, const char *text, int posX, int posY, int fontSize, Color color)
        subroutine image_draw_text(dst, text, pos_x, pos_y, font_size, color) bind(c, name='ImageDrawText')
    IMPORT :: C_CHAR, C_INT, color_type, image_type
    IMPLICIT NONE
    TYPE(image_type), INTENT(inout) :: dst
    CHARACTER(kind=C_CHAR), INTENT(in) :: text
    INTEGER(kind=C_INT), INTENT(in), VALUE :: pos_x
    INTEGER(kind=C_INT), INTENT(in), VALUE :: pos_y
    INTEGER(kind=C_INT), INTENT(in), VALUE :: font_size
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE image_draw_text

  ! void ImageDrawTextEx(Image *dst, Font font, const char *text, Vector2 position, float fontSize, float spacing, Color tint)
        subroutine image_draw_text_ex(dst, font, text, position, font_size, spacing, tint) bind(c, name='ImageDrawTextEx')
    IMPORT :: C_CHAR, C_FLOAT, color_type, font_type, image_type, vector2_type
    IMPLICIT NONE
    TYPE(image_type), INTENT(inout) :: dst
    TYPE(font_type), INTENT(in), VALUE :: font
    CHARACTER(kind=C_CHAR), INTENT(in) :: text
    TYPE(vector2_type), INTENT(in), VALUE :: position
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: font_size
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: spacing
    TYPE(color_type), INTENT(in), VALUE :: tint
  END SUBROUTINE image_draw_text_ex

  ! void ImageFlipHorizontal(Image *image)
  SUBROUTINE image_flip_horizontal(image) BIND(c, name='ImageFlipHorizontal')
    IMPORT :: image_type
    IMPLICIT NONE
    TYPE(image_type), INTENT(inout) :: image
  END SUBROUTINE image_flip_horizontal

  ! void ImageFlipVertical(Image *image)
  SUBROUTINE image_flip_vertical(image) BIND(c, name='ImageFlipVertical')
    IMPORT :: image_type
    IMPLICIT NONE
    TYPE(image_type), INTENT(inout) :: image
  END SUBROUTINE image_flip_vertical

  ! void ImageFormat(Image *image, int newFormat)
  SUBROUTINE image_format(image, new_format) BIND(c, name='ImageFormat')
    IMPORT :: C_INT, image_type
    IMPLICIT NONE
    TYPE(image_type), INTENT(inout) :: image
    INTEGER(kind=C_INT), INTENT(in), VALUE :: new_format
  END SUBROUTINE image_format

  ! Image ImageFromImage(Image image, Rectangle rec)
  FUNCTION image_from_image(image, rec) BIND(c, name='ImageFromImage')
    IMPORT :: image_type, rectangle_type
    IMPLICIT NONE
    TYPE(image_type), INTENT(in), VALUE :: image
    TYPE(rectangle_type), INTENT(in), VALUE :: rec
    TYPE(image_type) :: image_from_image
  END FUNCTION image_from_image

  ! void ImageKernelConvolution(Image *image, float *kernel, int kernelSize)
        subroutine image_kernel_convolution(image, kernel, kernel_size) bind(c, name='ImageKernelConvolution')
    IMPORT :: C_FLOAT, C_INT, image_type
    IMPLICIT NONE
    TYPE(image_type), INTENT(inout) :: image
    REAL(kind=C_FLOAT), INTENT(inout) :: kernel
    INTEGER(kind=C_INT), INTENT(in), VALUE :: kernel_size
  END SUBROUTINE image_kernel_convolution

  ! void ImageMipmaps(Image *image)
  SUBROUTINE image_mipmaps(image) BIND(c, name='ImageMipmaps')
    IMPORT :: image_type
    IMPLICIT NONE
    TYPE(image_type), INTENT(inout) :: image
  END SUBROUTINE image_mipmaps

  ! void ImageResize(Image *image, int newWidth, int newHeight)
        subroutine image_resize(image, new_width, new_height) bind(c, name='ImageResize')
    IMPORT :: C_INT, image_type
    IMPLICIT NONE
    TYPE(image_type), INTENT(inout) :: image
    INTEGER(kind=C_INT), INTENT(in), VALUE :: new_width
    INTEGER(kind=C_INT), INTENT(in), VALUE :: new_height
  END SUBROUTINE image_resize

  ! void ImageResizeCanvas(Image *image, int newWidth, int newHeight, int offsetX, int offsetY, Color fill)
        subroutine image_resize_canvas(image, new_width, new_height, offset_x, offset_y, fill) bind(c, name='ImageResizeCanvas')
    IMPORT :: C_INT, color_type, image_type
    IMPLICIT NONE
    TYPE(image_type), INTENT(inout) :: image
    INTEGER(kind=C_INT), INTENT(in), VALUE :: new_width
    INTEGER(kind=C_INT), INTENT(in), VALUE :: new_height
    INTEGER(kind=C_INT), INTENT(in), VALUE :: offset_x
    INTEGER(kind=C_INT), INTENT(in), VALUE :: offset_y
    TYPE(color_type), INTENT(in), VALUE :: fill
  END SUBROUTINE image_resize_canvas

  ! void ImageResizeNN(Image *image, int newWidth,int newHeight)
        subroutine image_resize_nn(image, new_width, new_height) bind(c, name='ImageResizeNN')
    IMPORT :: C_INT, image_type
    IMPLICIT NONE
    TYPE(image_type), INTENT(inout) :: image
    INTEGER(kind=C_INT), INTENT(in), VALUE :: new_width
    INTEGER(kind=C_INT), INTENT(in), VALUE :: new_height
  END SUBROUTINE image_resize_nn

  ! void ImageRotate(Image *image, int degrees)
  SUBROUTINE image_rotate(image, degrees) BIND(c, name='ImageRotate')
    IMPORT :: C_INT, image_type
    IMPLICIT NONE
    TYPE(image_type), INTENT(inout) :: image
    INTEGER(kind=C_INT), INTENT(in), VALUE :: degrees
  END SUBROUTINE image_rotate

  ! void ImageRotateCCW(Image *image)
  SUBROUTINE image_rotate_ccw(image) BIND(c, name='ImageRotateCCW')
    IMPORT :: image_type
    IMPLICIT NONE
    TYPE(image_type), INTENT(inout) :: image
  END SUBROUTINE image_rotate_ccw

  ! void ImageRotateCW(Image *image)
  SUBROUTINE image_rotate_cw(image) BIND(c, name='ImageRotateCW')
    IMPORT :: image_type
    IMPLICIT NONE
    TYPE(image_type), INTENT(inout) :: image
  END SUBROUTINE image_rotate_cw

  ! Image ImageText(const char *text, int fontSize, Color color)
  FUNCTION image_text(text, font_size, color) BIND(c, name='ImageText')
    IMPORT :: C_CHAR, C_INT, color_type, image_type
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: text
    INTEGER(kind=C_INT), INTENT(in), VALUE :: font_size
    TYPE(color_type), INTENT(in), VALUE :: color
    TYPE(image_type) :: image_text
  END FUNCTION image_text

  ! Image ImageTextEx(Font font, const char *text, float fontSize, float spacing, Color tint)
        function image_text_ex(font, text, font_size, spacing, tint) bind(c, name='ImageTextEx')
    IMPORT :: C_CHAR, C_FLOAT, color_type, font_type, image_type
    IMPLICIT NONE
    TYPE(font_type), INTENT(in), VALUE :: font
    CHARACTER(kind=C_CHAR), INTENT(in) :: text
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: font_size
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: spacing
    TYPE(color_type), INTENT(in), VALUE :: tint
    TYPE(image_type) :: image_text_ex
  END FUNCTION image_text_ex

  ! void ImageToPOT(Image *image, Color fill)
  SUBROUTINE image_to_pot(image, fill) BIND(c, name='ImageToPOT')
    IMPORT :: color_type, image_type
    IMPLICIT NONE
    TYPE(image_type), INTENT(inout) :: image
    TYPE(color_type), INTENT(in), VALUE :: fill
  END SUBROUTINE image_to_pot
END INTERFACE

END MODULE RaylibImageMethods
