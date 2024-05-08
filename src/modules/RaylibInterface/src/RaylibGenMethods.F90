! raylib.f90
!
! A collection of auto-generated Fortran 2018 interface bindings to
! raylib 5.1.
!
! Author:  Philipp Engel
! Licence: ISC

MODULE RaylibGenMethods
USE, INTRINSIC :: ISO_C_BINDING
USE RaylibTypes
USE RaylibEnums
IMPLICIT NONE
PRIVATE

PUBLIC :: GenImageCellular
PUBLIC :: GenImageChecked
PUBLIC :: GenImageColor
PUBLIC :: GenImageFontAtlas
PUBLIC :: GenImageGradientLinear
PUBLIC :: GenImageGradientRadial
PUBLIC :: GenImageGradientSquare
PUBLIC :: GenImageWhiteNoise
PUBLIC :: GenImagePerlinNoise
PUBLIC :: GenImageText
PUBLIC :: GenMeshCone
PUBLIC :: GenMeshCube
PUBLIC :: GenMeshCubicmap
PUBLIC :: GenMeshCylinder
PUBLIC :: GenMeshHeightmap
PUBLIC :: GenMeshHemiSphere
PUBLIC :: GenMeshKnot
PUBLIC :: GenMeshPlane
PUBLIC :: GenMeshPoly
PUBLIC :: GenMeshSphere
PUBLIC :: GenMeshTangents
PUBLIC :: GenMeshTorus
PUBLIC :: GenTextureMipmaps

INTERFACE

  ! Image GenImageCellular(int width, int height, int tileSize)
        function GenImageCellular(width, height, tile_size) bind(c, name='GenImageCellular')
    IMPORT :: C_INT, image_
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: width
    INTEGER(kind=C_INT), INTENT(in), VALUE :: height
    INTEGER(kind=C_INT), INTENT(in), VALUE :: tile_size
    TYPE(image_) :: GenImageCellular
  END FUNCTION GenImageCellular

  ! Image GenImageChecked(int width, int height, int checksX, int checksY, Color col1, Color col2)
        function GenImageChecked(width, height, checks_x, checks_y, col1, col2) bind(c, name='GenImageChecked')
    IMPORT :: C_INT, color_, image_
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: width
    INTEGER(kind=C_INT), INTENT(in), VALUE :: height
    INTEGER(kind=C_INT), INTENT(in), VALUE :: checks_x
    INTEGER(kind=C_INT), INTENT(in), VALUE :: checks_y
    TYPE(color_), INTENT(in), VALUE :: col1
    TYPE(color_), INTENT(in), VALUE :: col2
    TYPE(image_) :: GenImageChecked
  END FUNCTION GenImageChecked

  ! Image GenImageColor(int width, int height, Color color)
  FUNCTION GenImageColor(width, height, color) BIND(c, name='GenImageColor')
    IMPORT :: C_INT, color_, image_
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: width
    INTEGER(kind=C_INT), INTENT(in), VALUE :: height
    TYPE(color_), INTENT(in), VALUE :: color
    TYPE(image_) :: GenImageColor
  END FUNCTION GenImageColor

  ! Image GenImageFontAtlas(const GlyphInfo *glyphs, Rectangle **glyphRecs, int glyphCount, int fontSize, int padding, int packMethod)
        function GenImageFontAtlas(glyphs, glyph_recs, glyph_count, font_size, padding, pack_method) &
    BIND(c, name='GenImageFontAtlas')
    IMPORT :: C_INT, glyph_info_, image_, rectangle_
    IMPLICIT NONE
    TYPE(glyph_info_), INTENT(inout) :: glyphs
    TYPE(rectangle_), INTENT(inout) :: glyph_recs(*)
    INTEGER(kind=C_INT), INTENT(in), VALUE :: glyph_count
    INTEGER(kind=C_INT), INTENT(in), VALUE :: font_size
    INTEGER(kind=C_INT), INTENT(in), VALUE :: padding
    INTEGER(kind=C_INT), INTENT(in), VALUE :: pack_method
    TYPE(image_) :: GenImageFontAtlas
  END FUNCTION GenImageFontAtlas

  ! Image GenImageGradientLinear(int width, int height, int direction, Color start, Color end)
        function GenImageGradientLinear(width, height, direction, start, end) bind(c, name='GenImageGradientLinear')
    IMPORT :: C_INT, color_, image_
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: width
    INTEGER(kind=C_INT), INTENT(in), VALUE :: height
    INTEGER(kind=C_INT), INTENT(in), VALUE :: direction
    TYPE(color_), INTENT(in), VALUE :: start
    TYPE(color_), INTENT(in), VALUE :: END
    TYPE(image_) :: GenImageGradientLinear
  END FUNCTION GenImageGradientLinear

  ! Image GenImageGradientRadial(int width, int height, float density, Color inner, Color outer)
        function GenImageGradientRadial(width, height, density, inner, outer) bind(c, name='GenImageGradientRadial')
    IMPORT :: C_FLOAT, C_INT, color_, image_
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: width
    INTEGER(kind=C_INT), INTENT(in), VALUE :: height
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: density
    TYPE(color_), INTENT(in), VALUE :: inner
    TYPE(color_), INTENT(in), VALUE :: outer
    TYPE(image_) :: GenImageGradientRadial
  END FUNCTION GenImageGradientRadial

  ! Image GenImageGradientSquare(int width, int height, float density, Color inner, Color outer)
        function GenImageGradientSquare(width, height, density, inner, outer) bind(c, name='GenImageGradientSquare')
    IMPORT :: C_FLOAT, C_INT, color_, image_
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: width
    INTEGER(kind=C_INT), INTENT(in), VALUE :: height
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: density
    TYPE(color_), INTENT(in), VALUE :: inner
    TYPE(color_), INTENT(in), VALUE :: outer
    TYPE(image_) :: GenImageGradientSquare
  END FUNCTION GenImageGradientSquare

  ! Image GenImageWhiteNoise(int width, int height, float factor)
        function GenImageWhiteNoise(width, height, factor) bind(c, name='GenImageWhiteNoise')
    IMPORT :: C_FLOAT, C_INT, image_
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: width
    INTEGER(kind=C_INT), INTENT(in), VALUE :: height
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: factor
    TYPE(image_) :: GenImageWhiteNoise
  END FUNCTION GenImageWhiteNoise

  ! Image GenImagePerlinNoise(int width, int height, int offsetX, int offsetY, float scale)
        function GenImagePerlinNoise(width, height, offset_x, offset_y, scale) bind(c, name='GenImagePerlinNoise')
    IMPORT :: C_FLOAT, C_INT, image_
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: width
    INTEGER(kind=C_INT), INTENT(in), VALUE :: height
    INTEGER(kind=C_INT), INTENT(in), VALUE :: offset_x
    INTEGER(kind=C_INT), INTENT(in), VALUE :: offset_y
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: scale
    TYPE(image_) :: GenImagePerlinNoise
  END FUNCTION GenImagePerlinNoise

  ! Image GenImageText(int width, int height, const char *text)
  FUNCTION GenImageText(width, height, text) BIND(c, name='GenImageText')
    IMPORT :: C_CHAR, C_INT, image_
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: width
    INTEGER(kind=C_INT), INTENT(in), VALUE :: height
    CHARACTER(kind=C_CHAR), INTENT(in) :: text
    TYPE(image_) :: GenImageText
  END FUNCTION GenImageText

  ! Mesh GenMeshCone(float radius, float height, int slices)
  FUNCTION GenMeshCone(radius, height, slices) BIND(c, name='GenMeshCone')
    IMPORT :: C_FLOAT, C_INT, mesh_
    IMPLICIT NONE
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: radius
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: height
    INTEGER(kind=C_INT), INTENT(in), VALUE :: slices
    TYPE(mesh_) :: GenMeshCone
  END FUNCTION GenMeshCone

  ! Mesh GenMeshCube(float width, float height, float length)
  FUNCTION GenMeshCube(width, height, length) BIND(c, name='GenMeshCube')
    IMPORT :: C_FLOAT, mesh_
    IMPLICIT NONE
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: width
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: height
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: length
    TYPE(mesh_) :: GenMeshCube
  END FUNCTION GenMeshCube

  ! Mesh GenMeshCubicmap(Image cubicmap, Vector3 cubeSize)
 FUNCTION GenMeshCubicmap(cubicmap, cube_size) BIND(c, name='GenMeshCubicmap')
    IMPORT :: image_, mesh_, vector3_
    IMPLICIT NONE
    TYPE(image_), INTENT(in), VALUE :: cubicmap
    TYPE(vector3_), INTENT(in), VALUE :: cube_size
    TYPE(mesh_) :: GenMeshCubicmap
  END FUNCTION GenMeshCubicmap

  ! Mesh GenMeshCylinder(float radius, float height, int slices)
        function GenMeshCylinder(radius, height, slices) bind(c, name='GenMeshCylinder')
    IMPORT :: C_FLOAT, C_INT, mesh_
    IMPLICIT NONE
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: radius
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: height
    INTEGER(kind=C_INT), INTENT(in), VALUE :: slices
    TYPE(mesh_) :: GenMeshCylinder
  END FUNCTION GenMeshCylinder

  ! Mesh GenMeshHeightmap(Image heightmap, Vector3 size)
  FUNCTION GenMeshHeightmap(heightmap, size) BIND(c, name='GenMeshHeightmap')
    IMPORT :: image_, mesh_, vector3_
    IMPLICIT NONE
    TYPE(image_), INTENT(in), VALUE :: heightmap
    TYPE(vector3_), INTENT(in), VALUE :: size
    TYPE(mesh_) :: GenMeshHeightmap
  END FUNCTION GenMeshHeightmap

  ! Mesh GenMeshHemiSphere(float radius, int rings, int slices)
        function GenMeshHemiSphere(radius, rings, slices) bind(c, name='GenMeshHemiSphere')
    IMPORT :: C_FLOAT, C_INT, mesh_
    IMPLICIT NONE
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: radius
    INTEGER(kind=C_INT), INTENT(in), VALUE :: rings
    INTEGER(kind=C_INT), INTENT(in), VALUE :: slices
    TYPE(mesh_) :: GenMeshHemiSphere
  END FUNCTION GenMeshHemiSphere

  ! Mesh GenMeshKnot(float radius, float size, int radSeg, int sides)
FUNCTION GenMeshKnot(radius, size, rad_seg, sides) BIND(c, name='GenMeshKnot')
    IMPORT :: C_FLOAT, C_INT, mesh_
    IMPLICIT NONE
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: radius
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: size
    INTEGER(kind=C_INT), INTENT(in), VALUE :: rad_seg
    INTEGER(kind=C_INT), INTENT(in), VALUE :: sides
    TYPE(mesh_) :: GenMeshKnot
  END FUNCTION GenMeshKnot

  ! Mesh GenMeshPlane(float width, float length, int resX, int resZ)
        function GenMeshPlane(width, length, res_x, res_z) bind(c, name='GenMeshPlane')
    IMPORT :: C_FLOAT, C_INT, mesh_
    IMPLICIT NONE
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: width
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: length
    INTEGER(kind=C_INT), INTENT(in), VALUE :: res_x
    INTEGER(kind=C_INT), INTENT(in), VALUE :: res_z
    TYPE(mesh_) :: GenMeshPlane
  END FUNCTION GenMeshPlane

  ! Mesh GenMeshPoly(int sides, float radius)
  FUNCTION GenMeshPoly(sides, radius) BIND(c, name='GenMeshPoly')
    IMPORT :: C_FLOAT, C_INT, mesh_
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: sides
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: radius
    TYPE(mesh_) :: GenMeshPoly
  END FUNCTION GenMeshPoly

  ! Mesh GenMeshSphere(float radius, int rings, int slices)
  FUNCTION GenMeshSphere(radius, rings, slices) BIND(c, name='GenMeshSphere')
    IMPORT :: C_FLOAT, C_INT, mesh_
    IMPLICIT NONE
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: radius
    INTEGER(kind=C_INT), INTENT(in), VALUE :: rings
    INTEGER(kind=C_INT), INTENT(in), VALUE :: slices
    TYPE(mesh_) :: GenMeshSphere
  END FUNCTION GenMeshSphere

  ! void GenMeshTangents(Mesh *mesh)
  SUBROUTINE GenMeshTangents(mesh) BIND(c, name='GenMeshTangents')
    IMPORT :: mesh_
    IMPLICIT NONE
    TYPE(mesh_), INTENT(in) :: mesh
  END SUBROUTINE GenMeshTangents

  ! Mesh GenMeshTorus(float radius, float size, int radSeg, int sides)
        function GenMeshTorus(radius, size, rad_seg, sides) bind(c, name='GenMeshTorus')
    IMPORT :: C_FLOAT, C_INT, mesh_
    IMPLICIT NONE
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: radius
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: size
    INTEGER(kind=C_INT), INTENT(in), VALUE :: rad_seg
    INTEGER(kind=C_INT), INTENT(in), VALUE :: sides
    TYPE(mesh_) :: GenMeshTorus
  END FUNCTION GenMeshTorus

  ! void GenTextureMipmaps(Texture2D *texture)
  SUBROUTINE GenTextureMipmaps(texture) BIND(c, name='GenTextureMipmaps')
    IMPORT :: texture2d_
    IMPLICIT NONE
    TYPE(texture2d_), INTENT(inout) :: texture
  END SUBROUTINE GenTextureMipmaps

END INTERFACE

END MODULE RaylibGenMethods
