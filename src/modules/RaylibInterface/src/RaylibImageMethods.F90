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

PUBLIC :: ImageToPOT
PUBLIC :: ImageTextEx
PUBLIC :: ImageText
PUBLIC :: ImageRotateCW
PUBLIC :: ImageRotateCCW
PUBLIC :: ImageRotate
PUBLIC :: ImageResizeNN
PUBLIC :: ImageResizeCanvas
PUBLIC :: ImageResize
PUBLIC :: ImageMipmaps
PUBLIC :: ImageKernelConvolution
PUBLIC :: ImageFromImage
PUBLIC :: ImageFormat
PUBLIC :: ImageFlipVertical
PUBLIC :: ImageFlipHorizontal
PUBLIC :: ImageDrawTextEx
PUBLIC :: ImageDrawText
PUBLIC :: ImageDrawRectangleV
PUBLIC :: ImageDrawRectangleRec
PUBLIC :: ImageDrawRectangleLines
PUBLIC :: ImageDrawRectangle
PUBLIC :: ImageDrawPixelV
PUBLIC :: ImageDrawPixel
PUBLIC :: ImageDrawLineV
PUBLIC :: ImageDrawLine
PUBLIC :: ImageDrawCircleV
PUBLIC :: ImageDrawCircleLinesV
PUBLIC :: ImageDrawCircleLines
PUBLIC :: ImageDrawCircle
PUBLIC :: ImageDraw
PUBLIC :: ImageDither
PUBLIC :: ImageCrop
PUBLIC :: ImageCopy
PUBLIC :: ImageColorTint
PUBLIC :: ImageColorReplace
PUBLIC :: ImageColorInvert
PUBLIC :: ImageColorGrayscale
PUBLIC :: ImageColorContrast
PUBLIC :: ImageColorBrightness
PUBLIC :: ImageClearBackground
PUBLIC :: ImageBlurGaussian
PUBLIC :: ImageAlphaPremultiply
PUBLIC :: ImageAlphaMask
PUBLIC :: ImageAlphaCrop
PUBLIC :: ImageAlphaClear

INTERFACE

  ! void ImageBlurGaussian(Image *image, int blurSize)
  SUBROUTINE ImageBlurGaussian(image, blur_size) BIND(c, name='ImageBlurGaussian')
    IMPORT :: C_INT, image_
    IMPLICIT NONE
    TYPE(image_), INTENT(inout) :: image
    INTEGER(kind=C_INT), INTENT(in), VALUE :: blur_size
  END SUBROUTINE ImageBlurGaussian

  ! void ImageAlphaClear(Image *image, Color color, float threshold)
        subroutine ImageAlphaClear(image, color, threshold) bind(c, name='ImageAlphaClear')
    IMPORT :: C_FLOAT, color_, image_
    IMPLICIT NONE
    TYPE(image_), INTENT(inout) :: image
    TYPE(color_), INTENT(in), VALUE :: color
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: threshold
  END SUBROUTINE ImageAlphaClear

  ! void ImageAlphaCrop(Image *image, float threshold)
  SUBROUTINE ImageAlphaCrop(image, threshold) BIND(c, name='ImageAlphaCrop')
    IMPORT :: C_FLOAT, image_
    IMPLICIT NONE
    TYPE(image_), INTENT(inout) :: image
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: threshold
  END SUBROUTINE ImageAlphaCrop

  ! void ImageAlphaMask(Image *image, Image alphaMask)
  SUBROUTINE ImageAlphaMask(image, alpha_mask) BIND(c, name='ImageAlphaMask')
    IMPORT :: image_
    IMPLICIT NONE
    TYPE(image_), INTENT(inout) :: image
    TYPE(image_), INTENT(in), VALUE :: alpha_mask
  END SUBROUTINE ImageAlphaMask

  ! void ImageAlphaPremultiply(Image *image)
 SUBROUTINE ImageAlphaPremultiply(image) BIND(c, name='ImageAlphaPremultiply')
    IMPORT :: image_
    IMPLICIT NONE
    TYPE(image_), INTENT(inout) :: image
  END SUBROUTINE ImageAlphaPremultiply

  ! void ImageClearBackground(Image *dst, Color color)
        subroutine ImageClearBackground(dst, color) bind(c, name='ImageClearBackground')
    IMPORT :: color_, image_
    IMPLICIT NONE
    TYPE(image_), INTENT(inout) :: dst
    TYPE(color_), INTENT(in), VALUE :: color
  END SUBROUTINE ImageClearBackground

  ! void ImageColorBrightness(Image *image, int brightness)
        subroutine ImageColorBrightness(image, brightness) bind(c, name='ImageColorBrightness')
    IMPORT :: C_INT, image_
    IMPLICIT NONE
    TYPE(image_), INTENT(inout) :: image
    INTEGER(kind=C_INT), INTENT(in), VALUE :: brightness
  END SUBROUTINE ImageColorBrightness

  ! void ImageColorContrast(Image *image, float contrast)
        subroutine ImageColorContrast(image, contrast) bind(c, name='ImageColorContrast')
    IMPORT :: C_FLOAT, image_
    IMPLICIT NONE
    TYPE(image_), INTENT(inout) :: image
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: contrast
  END SUBROUTINE ImageColorContrast

  ! void ImageColorGrayscale(Image *image)
  SUBROUTINE ImageColorGrayscale(image) BIND(c, name='ImageColorGrayscale')
    IMPORT :: image_
    IMPLICIT NONE
    TYPE(image_), INTENT(inout) :: image
  END SUBROUTINE ImageColorGrayscale

  ! void ImageColorInvert(Image *image)
  SUBROUTINE ImageColorInvert(image) BIND(c, name='ImageColorInvert')
    IMPORT :: image_
    IMPLICIT NONE
    TYPE(image_), INTENT(inout) :: image
  END SUBROUTINE ImageColorInvert

  ! void ImageColorReplace(Image *image, Color color, Color replace)
        subroutine ImageColorReplace(image, color, replace) bind(c, name='ImageColorReplace')
    IMPORT :: color_, image_
    IMPLICIT NONE
    TYPE(image_), INTENT(inout) :: image
    TYPE(color_), INTENT(in), VALUE :: color
    TYPE(color_), INTENT(in), VALUE :: replace
  END SUBROUTINE ImageColorReplace

  ! void ImageColorTint(Image *image, Color color)
  SUBROUTINE ImageColorTint(image, color) BIND(c, name='ImageColorTint')
    IMPORT :: color_, image_
    IMPLICIT NONE
    TYPE(image_), INTENT(inout) :: image
    TYPE(color_), INTENT(in), VALUE :: color
  END SUBROUTINE ImageColorTint

  ! Image ImageCopy(Image image)
  FUNCTION ImageCopy(image) BIND(c, name='ImageCopy')
    IMPORT :: image_
    IMPLICIT NONE
    TYPE(image_), INTENT(in), VALUE :: image
    TYPE(image_) :: ImageCopy
  END FUNCTION ImageCopy

  ! void ImageCrop(Image *image, Rectangle crop)
  SUBROUTINE ImageCrop(image, crop) BIND(c, name='ImageCrop')
    IMPORT :: image_, rectangle_
    IMPLICIT NONE
    TYPE(image_), INTENT(inout) :: image
    TYPE(rectangle_), INTENT(in), VALUE :: crop
  END SUBROUTINE ImageCrop

  ! void ImageDither(Image *image, int rBpp, int gBpp, int bBpp, int aBpp)
        subroutine ImageDither(image, r_bpp, g_bpp, b_bpp, a_bpp) bind(c, name='ImageDither')
    IMPORT :: C_INT, image_
    IMPLICIT NONE
    TYPE(image_), INTENT(inout) :: image
    INTEGER(kind=C_INT), INTENT(in), VALUE :: r_bpp
    INTEGER(kind=C_INT), INTENT(in), VALUE :: g_bpp
    INTEGER(kind=C_INT), INTENT(in), VALUE :: b_bpp
    INTEGER(kind=C_INT), INTENT(in), VALUE :: a_bpp
  END SUBROUTINE ImageDither

  ! void ImageDraw(Image *dst, Image src, Rectangle srcRec, Rectangle dstRec, Color tint)
        subroutine ImageDraw(dst, src, src_rec, dst_rec, tint) bind(c, name='ImageDraw')
    IMPORT :: color_, image_, rectangle_
    IMPLICIT NONE
    TYPE(image_), INTENT(inout) :: dst
    TYPE(image_), INTENT(in), VALUE :: src
    TYPE(rectangle_), INTENT(in), VALUE :: src_rec
    TYPE(rectangle_), INTENT(in), VALUE :: dst_rec
    TYPE(color_), INTENT(in), VALUE :: tint
  END SUBROUTINE ImageDraw

  ! void ImageDrawCircle(Image *dst, int centerX, int centerY, int radius, Color color)
        subroutine ImageDrawCircle(dst, center_x, center_y, radius, color) bind(c, name='ImageDrawCircle')
    IMPORT :: C_INT, color_, image_
    IMPLICIT NONE
    TYPE(image_), INTENT(inout) :: dst
    INTEGER(kind=C_INT), INTENT(in), VALUE :: center_x
    INTEGER(kind=C_INT), INTENT(in), VALUE :: center_y
    INTEGER(kind=C_INT), INTENT(in), VALUE :: radius
    TYPE(color_), INTENT(in), VALUE :: color
  END SUBROUTINE ImageDrawCircle

  ! void ImageDrawCircleLines(Image *dst, int centerX, int centerY, int radius, Color color)
        subroutine ImageDrawCircleLines(dst, center_x, center_y, radius, color) bind(c, name='ImageDrawCircleLines')
    IMPORT :: C_INT, color_, image_
    IMPLICIT NONE
    TYPE(image_), INTENT(inout) :: dst
    INTEGER(kind=C_INT), INTENT(in), VALUE :: center_x
    INTEGER(kind=C_INT), INTENT(in), VALUE :: center_y
    INTEGER(kind=C_INT), INTENT(in), VALUE :: radius
    TYPE(color_), INTENT(in), VALUE :: color
  END SUBROUTINE ImageDrawCircleLines

  ! void ImageDrawCircleLinesV(Image *dst, Vector2 center, int radius, Color color)
        subroutine ImageDrawCircleLinesV(dst, center, radius, color) bind(c, name='ImageDrawCircleLinesV')
    IMPORT :: C_INT, color_, image_, vector2_
    IMPLICIT NONE
    TYPE(image_), INTENT(inout) :: dst
    TYPE(vector2_), INTENT(in), VALUE :: center
    INTEGER(kind=C_INT), INTENT(in), VALUE :: radius
    TYPE(color_), INTENT(in), VALUE :: color
  END SUBROUTINE ImageDrawCircleLinesV

  ! void ImageDrawCircleV(Image *dst, Vector2 center, int radius, Color color)
        subroutine ImageDrawCircleV(dst, center, radius, color) bind(c, name='ImageDrawCircleV')
    IMPORT :: C_INT, color_, image_, vector2_
    IMPLICIT NONE
    TYPE(image_), INTENT(inout) :: dst
    TYPE(vector2_), INTENT(in), VALUE :: center
    INTEGER(kind=C_INT), INTENT(in), VALUE :: radius
    TYPE(color_), INTENT(in), VALUE :: color
  END SUBROUTINE ImageDrawCircleV

  ! void ImageDrawLine(Image *dst, int startPosX, int startPosY, int endPosX, int endPosY, Color color)
        subroutine ImageDrawLine(dst, start_pos_x, start_pos_y, end_pos_x, end_pos_y, color) &
    BIND(c, name='ImageDrawLine')
    IMPORT :: C_INT, color_, image_
    IMPLICIT NONE
    TYPE(image_), INTENT(inout) :: dst
    INTEGER(kind=C_INT), INTENT(in), VALUE :: start_pos_x
    INTEGER(kind=C_INT), INTENT(in), VALUE :: start_pos_y
    INTEGER(kind=C_INT), INTENT(in), VALUE :: end_pos_x
    INTEGER(kind=C_INT), INTENT(in), VALUE :: end_pos_y
    TYPE(color_), INTENT(in), VALUE :: color
  END SUBROUTINE ImageDrawLine

  ! void ImageDrawLineV(Image *dst, Vector2 start, Vector2 end, Color color)
        subroutine ImageDrawLineV(dst, start, end, color) bind(c, name='ImageDrawLineV')
    IMPORT :: color_, image_, vector2_
    IMPLICIT NONE
    TYPE(image_), INTENT(inout) :: dst
    TYPE(vector2_), INTENT(in), VALUE :: start
    TYPE(vector2_), INTENT(in), VALUE :: END
    TYPE(color_), INTENT(in), VALUE :: color
  END SUBROUTINE ImageDrawLineV

  ! void ImageDrawPixel(Image *dst, int posX, int posY, Color color)
        subroutine ImageDrawPixel(dst, pos_x, pos_y, color) bind(c, name='ImageDrawPixel')
    IMPORT :: C_INT, color_, image_
    IMPLICIT NONE
    TYPE(image_), INTENT(inout) :: dst
    INTEGER(kind=C_INT), INTENT(in), VALUE :: pos_x
    INTEGER(kind=C_INT), INTENT(in), VALUE :: pos_y
    TYPE(color_), INTENT(in), VALUE :: color
  END SUBROUTINE ImageDrawPixel

  ! void ImageDrawPixelV(Image *dst, Vector2 position, Color color)
        subroutine ImageDrawPixelV(dst, position, color) bind(c, name='ImageDrawPixelV')
    IMPORT :: color_, image_, vector2_
    IMPLICIT NONE
    TYPE(image_), INTENT(inout) :: dst
    TYPE(vector2_), INTENT(in), VALUE :: position
    TYPE(color_), INTENT(in), VALUE :: color
  END SUBROUTINE ImageDrawPixelV

  ! void ImageDrawRectangle(Image *dst, int posX, int posY, int width, int height, Color color)
        subroutine ImageDrawRectangle(dst, pos_x, pos_y, width, height, color) bind(c, name='ImageDrawRectangle')
    IMPORT :: C_INT, color_, image_
    IMPLICIT NONE
    TYPE(image_), INTENT(inout) :: dst
    INTEGER(kind=C_INT), INTENT(in), VALUE :: pos_x
    INTEGER(kind=C_INT), INTENT(in), VALUE :: pos_y
    INTEGER(kind=C_INT), INTENT(in), VALUE :: width
    INTEGER(kind=C_INT), INTENT(in), VALUE :: height
    TYPE(color_), INTENT(in), VALUE :: color
  END SUBROUTINE ImageDrawRectangle

  ! void ImageDrawRectangleLines(Image *dst, Rectangle rec, int thick, Color color)
        subroutine ImageDrawRectangleLines(dst, rec, thick, color) bind(c, name='ImageDrawRectangleLines')
    IMPORT :: C_INT, color_, image_, rectangle_
    IMPLICIT NONE
    TYPE(image_), INTENT(inout) :: dst
    TYPE(rectangle_), INTENT(in), VALUE :: rec
    INTEGER(kind=C_INT), INTENT(in), VALUE :: thick
    TYPE(color_), INTENT(in), VALUE :: color
  END SUBROUTINE ImageDrawRectangleLines

  ! void ImageDrawRectangleRec(Image *dst, Rectangle rec, Color color)
        subroutine ImageDrawRectangleRec(dst, rec, color) bind(c, name='ImageDrawRectangleRec')
    IMPORT :: color_, image_, rectangle_
    IMPLICIT NONE
    TYPE(image_), INTENT(inout) :: dst
    TYPE(rectangle_), INTENT(in), VALUE :: rec
    TYPE(color_), INTENT(in), VALUE :: color
  END SUBROUTINE ImageDrawRectangleRec

  ! void ImageDrawRectangleV(Image *dst, Vector2 position, Vector2 size, Color color)
        subroutine ImageDrawRectangleV(dst, position, size, color) bind(c, name='ImageDrawRectangleV')
    IMPORT :: color_, image_, vector2_
    IMPLICIT NONE
    TYPE(image_), INTENT(inout) :: dst
    TYPE(vector2_), INTENT(in), VALUE :: position
    TYPE(vector2_), INTENT(in), VALUE :: size
    TYPE(color_), INTENT(in), VALUE :: color
  END SUBROUTINE ImageDrawRectangleV

  ! void ImageDrawText(Image *dst, const char *text, int posX, int posY, int fontSize, Color color)
        subroutine ImageDrawText(dst, text, pos_x, pos_y, font_size, color) bind(c, name='ImageDrawText')
    IMPORT :: C_CHAR, C_INT, color_, image_
    IMPLICIT NONE
    TYPE(image_), INTENT(inout) :: dst
    CHARACTER(kind=C_CHAR), INTENT(in) :: text
    INTEGER(kind=C_INT), INTENT(in), VALUE :: pos_x
    INTEGER(kind=C_INT), INTENT(in), VALUE :: pos_y
    INTEGER(kind=C_INT), INTENT(in), VALUE :: font_size
    TYPE(color_), INTENT(in), VALUE :: color
  END SUBROUTINE ImageDrawText

  ! void ImageDrawTextEx(Image *dst, Font font, const char *text, Vector2 position, float fontSize, float spacing, Color tint)
        subroutine ImageDrawTextEx(dst, font, text, position, font_size, spacing, tint) bind(c, name='ImageDrawTextEx')
    IMPORT :: C_CHAR, C_FLOAT, color_, font_, image_, vector2_
    IMPLICIT NONE
    TYPE(image_), INTENT(inout) :: dst
    TYPE(font_), INTENT(in), VALUE :: font
    CHARACTER(kind=C_CHAR), INTENT(in) :: text
    TYPE(vector2_), INTENT(in), VALUE :: position
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: font_size
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: spacing
    TYPE(color_), INTENT(in), VALUE :: tint
  END SUBROUTINE ImageDrawTextEx

  ! void ImageFlipHorizontal(Image *image)
  SUBROUTINE ImageFlipHorizontal(image) BIND(c, name='ImageFlipHorizontal')
    IMPORT :: image_
    IMPLICIT NONE
    TYPE(image_), INTENT(inout) :: image
  END SUBROUTINE ImageFlipHorizontal

  ! void ImageFlipVertical(Image *image)
  SUBROUTINE ImageFlipVertical(image) BIND(c, name='ImageFlipVertical')
    IMPORT :: image_
    IMPLICIT NONE
    TYPE(image_), INTENT(inout) :: image
  END SUBROUTINE ImageFlipVertical

  ! void ImageFormat(Image *image, int newFormat)
  SUBROUTINE ImageFormat(image, new_format) BIND(c, name='ImageFormat')
    IMPORT :: C_INT, image_
    IMPLICIT NONE
    TYPE(image_), INTENT(inout) :: image
    INTEGER(kind=C_INT), INTENT(in), VALUE :: new_format
  END SUBROUTINE ImageFormat

  ! Image ImageFromImage(Image image, Rectangle rec)
  FUNCTION ImageFromImage(image, rec) BIND(c, name='ImageFromImage')
    IMPORT :: image_, rectangle_
    IMPLICIT NONE
    TYPE(image_), INTENT(in), VALUE :: image
    TYPE(rectangle_), INTENT(in), VALUE :: rec
    TYPE(image_) :: ImageFromImage
  END FUNCTION ImageFromImage

  ! void ImageKernelConvolution(Image *image, float *kernel, int kernelSize)
        subroutine ImageKernelConvolution(image, kernel, kernel_size) bind(c, name='ImageKernelConvolution')
    IMPORT :: C_FLOAT, C_INT, image_
    IMPLICIT NONE
    TYPE(image_), INTENT(inout) :: image
    REAL(kind=C_FLOAT), INTENT(inout) :: kernel
    INTEGER(kind=C_INT), INTENT(in), VALUE :: kernel_size
  END SUBROUTINE ImageKernelConvolution

  ! void ImageMipmaps(Image *image)
  SUBROUTINE ImageMipmaps(image) BIND(c, name='ImageMipmaps')
    IMPORT :: image_
    IMPLICIT NONE
    TYPE(image_), INTENT(inout) :: image
  END SUBROUTINE ImageMipmaps

  ! void ImageResize(Image *image, int newWidth, int newHeight)
        subroutine ImageResize(image, new_width, new_height) bind(c, name='ImageResize')
    IMPORT :: C_INT, image_
    IMPLICIT NONE
    TYPE(image_), INTENT(inout) :: image
    INTEGER(kind=C_INT), INTENT(in), VALUE :: new_width
    INTEGER(kind=C_INT), INTENT(in), VALUE :: new_height
  END SUBROUTINE ImageResize

  ! void ImageResizeCanvas(Image *image, int newWidth, int newHeight, int offsetX, int offsetY, Color fill)
        subroutine ImageResizeCanvas(image, new_width, new_height, offset_x, offset_y, fill) bind(c, name='ImageResizeCanvas')
    IMPORT :: C_INT, color_, image_
    IMPLICIT NONE
    TYPE(image_), INTENT(inout) :: image
    INTEGER(kind=C_INT), INTENT(in), VALUE :: new_width
    INTEGER(kind=C_INT), INTENT(in), VALUE :: new_height
    INTEGER(kind=C_INT), INTENT(in), VALUE :: offset_x
    INTEGER(kind=C_INT), INTENT(in), VALUE :: offset_y
    TYPE(color_), INTENT(in), VALUE :: fill
  END SUBROUTINE ImageResizeCanvas

  ! void ImageResizeNN(Image *image, int newWidth,int newHeight)
        subroutine ImageResizeNN(image, new_width, new_height) bind(c, name='ImageResizeNN')
    IMPORT :: C_INT, image_
    IMPLICIT NONE
    TYPE(image_), INTENT(inout) :: image
    INTEGER(kind=C_INT), INTENT(in), VALUE :: new_width
    INTEGER(kind=C_INT), INTENT(in), VALUE :: new_height
  END SUBROUTINE ImageResizeNN

  ! void ImageRotate(Image *image, int degrees)
  SUBROUTINE ImageRotate(image, degrees) BIND(c, name='ImageRotate')
    IMPORT :: C_INT, image_
    IMPLICIT NONE
    TYPE(image_), INTENT(inout) :: image
    INTEGER(kind=C_INT), INTENT(in), VALUE :: degrees
  END SUBROUTINE ImageRotate

  ! void ImageRotateCCW(Image *image)
  SUBROUTINE ImageRotateCCW(image) BIND(c, name='ImageRotateCCW')
    IMPORT :: image_
    IMPLICIT NONE
    TYPE(image_), INTENT(inout) :: image
  END SUBROUTINE ImageRotateCCW

  ! void ImageRotateCW(Image *image)
  SUBROUTINE ImageRotateCW(image) BIND(c, name='ImageRotateCW')
    IMPORT :: image_
    IMPLICIT NONE
    TYPE(image_), INTENT(inout) :: image
  END SUBROUTINE ImageRotateCW

  ! Image ImageText(const char *text, int fontSize, Color color)
  FUNCTION ImageText(text, font_size, color) BIND(c, name='ImageText')
    IMPORT :: C_CHAR, C_INT, color_, image_
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: text
    INTEGER(kind=C_INT), INTENT(in), VALUE :: font_size
    TYPE(color_), INTENT(in), VALUE :: color
    TYPE(image_) :: ImageText
  END FUNCTION ImageText

  ! Image ImageTextEx(Font font, const char *text, float fontSize, float spacing, Color tint)
        function ImageTextEx(font, text, font_size, spacing, tint) bind(c, name='ImageTextEx')
    IMPORT :: C_CHAR, C_FLOAT, color_, font_, image_
    IMPLICIT NONE
    TYPE(font_), INTENT(in), VALUE :: font
    CHARACTER(kind=C_CHAR), INTENT(in) :: text
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: font_size
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: spacing
    TYPE(color_), INTENT(in), VALUE :: tint
    TYPE(image_) :: ImageTextEx
  END FUNCTION ImageTextEx

  ! void ImageToPOT(Image *image, Color fill)
  SUBROUTINE ImageToPOT(image, fill) BIND(c, name='ImageToPOT')
    IMPORT :: color_, image_
    IMPLICIT NONE
    TYPE(image_), INTENT(inout) :: image
    TYPE(color_), INTENT(in), VALUE :: fill
  END SUBROUTINE ImageToPOT
END INTERFACE

END MODULE RaylibImageMethods
