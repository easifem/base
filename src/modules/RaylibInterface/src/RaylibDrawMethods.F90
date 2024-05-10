! raylib.f90
!
! A collection of auto-generated Fortran 2018 interface bindings to
! raylib 5.1.
!
! Author:  Philipp Engel
! Licence: ISC

MODULE RaylibDrawMethods
USE, INTRINSIC :: ISO_C_BINDING
USE RaylibTypes
USE RaylibEnums
IMPLICIT NONE
PRIVATE

PUBLIC :: DrawBillboard
PUBLIC :: DrawBillboardPro
PUBLIC :: DrawBillboardRec
PUBLIC :: DrawBoundingBox
PUBLIC :: DrawCapsule
PUBLIC :: DrawCapsuleWires
PUBLIC :: DrawCircle3D
PUBLIC :: DrawCircleGradient
PUBLIC :: DrawCircleLinesV
PUBLIC :: DrawCircleLines
PUBLIC :: DrawCircleSectorLines
PUBLIC :: DrawCircleSector
PUBLIC :: DrawCircleV
PUBLIC :: DrawCircle

PUBLIC :: DrawCubeWiresV
PUBLIC :: DrawCubeWires
PUBLIC :: DrawCubeV
PUBLIC :: DrawCube

PUBLIC :: DrawCylinderWiresEx
PUBLIC :: DrawCylinderWires
PUBLIC :: DrawCylinderEx
PUBLIC :: DrawCylinder

PUBLIC :: DrawEllipseLines
PUBLIC :: DrawEllipse

PUBLIC :: DrawLineV
PUBLIC :: DrawLineStrip
PUBLIC :: DrawLineEx
PUBLIC :: DrawLineBezier
PUBLIC :: DrawLine3D
PUBLIC :: DrawLine
PUBLIC :: DrawGrid
PUBLIC :: DrawFPS

PUBLIC :: DrawMeshInstanced
PUBLIC :: DrawMesh

PUBLIC :: DrawModelWiresEx
PUBLIC :: DrawModelWires
PUBLIC :: DrawModelEx
PUBLIC :: DrawModel

PUBLIC :: draw_pixel_v
PUBLIC :: draw_pixel

PUBLIC :: DrawTriangleStrip
PUBLIC :: DrawTriangleLines
PUBLIC :: DrawTriangleFan
PUBLIC :: DrawTriangle3D
PUBLIC :: DrawTriangle

PUBLIC :: DrawTextureV
PUBLIC :: DrawTextureRec
PUBLIC :: DrawTexturePro
PUBLIC :: DrawTextureNPatch
PUBLIC :: DrawTextureEx
PUBLIC :: DrawTexture

PUBLIC :: DrawTextPro
PUBLIC :: DrawTextEx
PUBLIC :: DrawTextCodepoints
PUBLIC :: DrawTextCodepoint
PUBLIC :: DrawText
PUBLIC :: DrawSplineSegmentLinear
PUBLIC :: DrawSplineSegmentCatmullRom

PUBLIC :: DrawSplineSegmentBezierQuadratic
PUBLIC :: DrawSplineSegmentBezierCubic
PUBLIC :: DrawSplineSegmentBasis
PUBLIC :: DrawSplineLinear
PUBLIC :: DrawSplineCatmullRom
PUBLIC :: DrawSplineBezierQuadratic
PUBLIC :: DrawSplineBezierCubic
PUBLIC :: DrawSplineBasis
PUBLIC :: DrawSphereWires
PUBLIC :: DrawSphereEx
PUBLIC :: DrawSphere
PUBLIC :: DrawRingLines
PUBLIC :: DrawRing

PUBLIC :: DrawRectangleV
PUBLIC :: DrawRectangleRoundedLines
PUBLIC :: DrawRectangleRounded
PUBLIC :: DrawRectangleRec
PUBLIC :: DrawRectanglePro
PUBLIC :: DrawRectangleLinesEx
PUBLIC :: DrawRectangleLines
PUBLIC :: DrawRectangleGradientV
PUBLIC :: DrawRectangleGradientH
PUBLIC :: DrawRectangleGradientEx
PUBLIC :: DrawRectangle
PUBLIC :: DrawRay
PUBLIC :: DrawPolyLinesEx
PUBLIC :: DrawPolyLines
PUBLIC :: DrawPoly
PUBLIC :: DrawPoint3D
PUBLIC :: DrawPlane

PUBLIC :: DrawTriangleStrip3D

INTERFACE

! void DrawBillboard(Camera camera, Texture2D texture, Vector3 position, float size, Color tint)
  SUBROUTINE DrawBillboard(camera, texture, position, size, tint) &
    BIND(c, name='DrawBillboard')
    IMPORT :: C_FLOAT, camera3d_, color_, texture2d_, vector3_
    IMPLICIT NONE
    TYPE(camera3d_), INTENT(in), VALUE :: camera
    TYPE(texture2d_), INTENT(in), VALUE :: texture
    TYPE(vector3_), INTENT(in), VALUE :: position
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: size
    TYPE(color_), INTENT(in), VALUE :: tint
  END SUBROUTINE DrawBillboard

! void DrawBillboardPro(Camera camera, Texture2D texture, Rectangle source, Vector3 position, Vector3 up, Vector2 size, Vector2 origin, float rotation, Color tint)
  SUBROUTINE DrawBillboardPro(camera, texture, source, position, up &
                              , size, origin, rotation, tint) &
    BIND(c, name='DrawBillboardPro')
    IMPORT :: C_FLOAT, camera3d_, color_, rectangle_, &
      texture2d_, vector2_, vector3_
    IMPLICIT NONE
    TYPE(camera3d_), INTENT(in), VALUE :: camera
    TYPE(texture2d_), INTENT(in), VALUE :: texture
    TYPE(rectangle_), INTENT(in), VALUE :: source
    TYPE(vector3_), INTENT(in), VALUE :: position
    TYPE(vector3_), INTENT(in), VALUE :: up
    TYPE(vector2_), INTENT(in), VALUE :: size
    TYPE(vector2_), INTENT(in), VALUE :: origin
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: rotation
    TYPE(color_), INTENT(in), VALUE :: tint
  END SUBROUTINE DrawBillboardPro

! void DrawBillboardRec(Camera camera, Texture2D texture, Rectangle source, Vector3 position, Vector2 size, Color tint)
  SUBROUTINE DrawBillboardRec(camera, texture, source, position, &
                              size, tint) BIND(c, name='DrawBillboardRec')
    IMPORT :: camera3d_, color_, rectangle_, &
      texture2d_, vector2_, vector3_
    IMPLICIT NONE
    TYPE(camera3d_), INTENT(in), VALUE :: camera
    TYPE(texture2d_), INTENT(in), VALUE :: texture
    TYPE(rectangle_), INTENT(in), VALUE :: source
    TYPE(vector3_), INTENT(in), VALUE :: position
    TYPE(vector2_), INTENT(in), VALUE :: size
    TYPE(color_), INTENT(in), VALUE :: tint
  END SUBROUTINE DrawBillboardRec

! void DrawBoundingBox(BoundingBox box, Color color)
  SUBROUTINE DrawBoundingBox(box, color) BIND(c, name='DrawBoundingBox')
    IMPORT :: bounding_box_, color_
    IMPLICIT NONE
    TYPE(bounding_box_), INTENT(in), VALUE :: box
    TYPE(color_), INTENT(in), VALUE :: color
  END SUBROUTINE DrawBoundingBox

! void DrawCapsule(Vector3 startPos, Vector3 endPos, float radius, int slices, int rings, Color color)
  SUBROUTINE DrawCapsule(start_pos, end_pos, radius, slices, rings, &
                         color) BIND(c, name='DrawCapsule')
    IMPORT :: C_FLOAT, C_INT, color_, vector3_
    IMPLICIT NONE
    TYPE(vector3_), INTENT(in), VALUE :: start_pos
    TYPE(vector3_), INTENT(in), VALUE :: end_pos
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: radius
    INTEGER(kind=C_INT), INTENT(in), VALUE :: slices
    INTEGER(kind=C_INT), INTENT(in), VALUE :: rings
    TYPE(color_), INTENT(in), VALUE :: color
  END SUBROUTINE DrawCapsule

! void DrawCapsuleWires(Vector3 startPos, Vector3 endPos, float radius, int slices, int rings, Color color)
  SUBROUTINE DrawCapsuleWires(start_pos, end_pos, radius, slices, &
                              rings, color) BIND(c, name='DrawCapsuleWires')
    IMPORT :: C_FLOAT, C_INT, color_, vector3_
    IMPLICIT NONE
    TYPE(vector3_), INTENT(in), VALUE :: start_pos
    TYPE(vector3_), INTENT(in), VALUE :: end_pos
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: radius
    INTEGER(kind=C_INT), INTENT(in), VALUE :: slices
    INTEGER(kind=C_INT), INTENT(in), VALUE :: rings
    TYPE(color_), INTENT(in), VALUE :: color
  END SUBROUTINE DrawCapsuleWires

! void DrawCircle(int centerX, int centerY, float radius, Color color)
        subroutine DrawCircle(center_x, center_y, radius, color) bind(c, name='DrawCircle')
    IMPORT :: C_FLOAT, C_INT, color_
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: center_x
    INTEGER(kind=C_INT), INTENT(in), VALUE :: center_y
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: radius
    TYPE(color_), INTENT(in), VALUE :: color
  END SUBROUTINE DrawCircle

! void DrawCircle3D(Vector3 center, float radius, Vector3 rotationAxis, float rotationAngle, Color color)
  SUBROUTINE DrawCircle3D(center, radius, rotation_axis, &
                          rotation_angle, color) BIND(c, name='DrawCircle3D')
    IMPORT :: C_FLOAT, color_, vector3_
    IMPLICIT NONE
    TYPE(vector3_), INTENT(in), VALUE :: center
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: radius
    TYPE(vector3_), INTENT(in), VALUE :: rotation_axis
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: rotation_angle
    TYPE(color_), INTENT(in), VALUE :: color
  END SUBROUTINE DrawCircle3D

! void DrawCircleGradient(int centerX, int centerY, float radius, Color color1, Color color2)
  SUBROUTINE DrawCircleGradient(center_x, center_y, radius, color1, &
                                color2) BIND(c, name='DrawCircleGradient')
    IMPORT :: C_FLOAT, C_INT, color_
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: center_x
    INTEGER(kind=C_INT), INTENT(in), VALUE :: center_y
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: radius
    TYPE(color_), INTENT(in), VALUE :: color1
    TYPE(color_), INTENT(in), VALUE :: color2
  END SUBROUTINE DrawCircleGradient

! void DrawCircleLines(int centerX, int centerY, float radius, Color color)
  SUBROUTINE DrawCircleLines(center_x, center_y, radius, color) BIND &
    (c, name='DrawCircleLines')
    IMPORT :: C_FLOAT, C_INT, color_
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: center_x
    INTEGER(kind=C_INT), INTENT(in), VALUE :: center_y
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: radius
    TYPE(color_), INTENT(in), VALUE :: color
  END SUBROUTINE DrawCircleLines

! void DrawCircleLinesV(Vector2 center, float radius, Color color)
  SUBROUTINE DrawCircleLinesV(center, radius, color) BIND(c, name='DrawCircleLinesV')
    IMPORT :: C_FLOAT, color_, vector2_
    IMPLICIT NONE
    TYPE(vector2_), INTENT(in), VALUE :: center
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: radius
    TYPE(color_), INTENT(in), VALUE :: color
  END SUBROUTINE DrawCircleLinesV

! void DrawCircleSector(Vector2 center, float radius, float startAngle, float endAngle, int segments, Color color)
  SUBROUTINE DrawCircleSector(center, radius, start_angle, end_angle &
                              , segments, color) &
    BIND(c, name='DrawCircleSector')
    IMPORT :: C_FLOAT, C_INT, color_, vector2_
    IMPLICIT NONE
    TYPE(vector2_), INTENT(in), VALUE :: center
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: radius
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: start_angle
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: end_angle
    INTEGER(kind=C_INT), INTENT(in), VALUE :: segments
    TYPE(color_), INTENT(in), VALUE :: color
  END SUBROUTINE DrawCircleSector

! void DrawCircleSectorLines(Vector2 center, float radius, float startAngle, float endAngle, int segments, Color color)
  SUBROUTINE DrawCircleSectorLines(center, radius, start_angle, &
                                   end_angle, segments, color) &
    BIND(c, name='DrawCircleSectorLines')
    IMPORT :: C_FLOAT, C_INT, color_, vector2_
    IMPLICIT NONE
    TYPE(vector2_), INTENT(in), VALUE :: center
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: radius
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: start_angle
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: end_angle
    INTEGER(kind=C_INT), INTENT(in), VALUE :: segments
    TYPE(color_), INTENT(in), VALUE :: color
  END SUBROUTINE DrawCircleSectorLines

! void DrawCircleV(Vector2 center, float radius, Color color)
  SUBROUTINE DrawCircleV(center, radius, color) BIND(c, name='DrawCircleV')
    IMPORT :: C_FLOAT, color_, vector2_
    IMPLICIT NONE
    TYPE(vector2_), INTENT(in), VALUE :: center
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: radius
    TYPE(color_), INTENT(in), VALUE :: color
  END SUBROUTINE DrawCircleV

! void DrawCube(Vector3 position, float width, float height, float length, Color color)
  SUBROUTINE DrawCube(position, width, height, length, color) BIND(c &
                                                            , name='DrawCube')
    IMPORT :: C_FLOAT, color_, vector3_
    IMPLICIT NONE
    TYPE(vector3_), INTENT(in), VALUE :: position
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: width
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: height
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: length
    TYPE(color_), INTENT(in), VALUE :: color
  END SUBROUTINE DrawCube

! void DrawCubeV(Vector3 position, Vector3 size, Color color)
  SUBROUTINE DrawCubeV(position, size, color) BIND(c, name='DrawCubeV')
    IMPORT :: color_, vector3_
    IMPLICIT NONE
    TYPE(vector3_), INTENT(in), VALUE :: position
    TYPE(vector3_), INTENT(in), VALUE :: size
    TYPE(color_), INTENT(in), VALUE :: color
  END SUBROUTINE DrawCubeV

! void DrawCubeWires(Vector3 position, float width, float height, float length, Color color)
  SUBROUTINE DrawCubeWires(position, width, height, length, color) &
    BIND(c, name='DrawCubeWires')
    IMPORT :: C_FLOAT, color_, vector3_
    IMPLICIT NONE
    TYPE(vector3_), INTENT(in), VALUE :: position
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: width
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: height
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: length
    TYPE(color_), INTENT(in), VALUE :: color
  END SUBROUTINE DrawCubeWires

! void DrawCubeWiresV(Vector3 position, Vector3 size, Color color)
  SUBROUTINE DrawCubeWiresV(position, size, color) BIND(c, name='DrawCubeWiresV')
    IMPORT :: color_, vector3_
    IMPLICIT NONE
    TYPE(vector3_), INTENT(in), VALUE :: position
    TYPE(vector3_), INTENT(in), VALUE :: size
    TYPE(color_), INTENT(in), VALUE :: color
  END SUBROUTINE DrawCubeWiresV

! void DrawCylinder(Vector3 position, float radiusTop, float radiusBottom, float height, int slices, Color color)
  SUBROUTINE DrawCylinder(position, radius_top, radius_bottom, height &
                          , slices, color) BIND(c, name='DrawCylinder')
    IMPORT :: C_FLOAT, C_INT, color_, vector3_
    IMPLICIT NONE
    TYPE(vector3_), INTENT(in), VALUE :: position
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: radius_top
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: radius_bottom
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: height
    INTEGER(kind=C_INT), INTENT(in), VALUE :: slices
    TYPE(color_), INTENT(in), VALUE :: color
  END SUBROUTINE DrawCylinder

! void DrawCylinderEx(Vector3 startPos, Vector3 endPos, float startRadius, float endRadius, int sides, Color color)
  SUBROUTINE DrawCylinderEx(start_pos, end_pos, start_radius, &
                      end_radius, sides, color) BIND(c, name='DrawCylinderEx')
    IMPORT :: C_FLOAT, C_INT, color_, vector3_
    IMPLICIT NONE
    TYPE(vector3_), INTENT(in), VALUE :: start_pos
    TYPE(vector3_), INTENT(in), VALUE :: end_pos
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: start_radius
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: end_radius
    INTEGER(kind=C_INT), INTENT(in), VALUE :: sides
    TYPE(color_), INTENT(in), VALUE :: color
  END SUBROUTINE DrawCylinderEx

! void DrawCylinderWires(Vector3 position, float radiusTop, float radiusBottom, float height, int slices, Color color)
  SUBROUTINE DrawCylinderWires(position, radius_top, radius_bottom, &
                               height, slices, color) &
    BIND(c, name='DrawCylinderWires')
    IMPORT :: C_FLOAT, C_INT, color_, vector3_
    IMPLICIT NONE
    TYPE(vector3_), INTENT(in), VALUE :: position
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: radius_top
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: radius_bottom
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: height
    INTEGER(kind=C_INT), INTENT(in), VALUE :: slices
    TYPE(color_), INTENT(in), VALUE :: color
  END SUBROUTINE DrawCylinderWires

! void DrawCylinderWiresEx(Vector3 startPos, Vector3 endPos, float startRadius, float endRadius, int sides, Color color)
  SUBROUTINE DrawCylinderWiresEx(start_pos, end_pos, start_radius, &
                                 end_radius, sides, color) &
    BIND(c, name='DrawCylinderWiresEx')
    IMPORT :: C_FLOAT, C_INT, color_, vector3_
    IMPLICIT NONE
    TYPE(vector3_), INTENT(in), VALUE :: start_pos
    TYPE(vector3_), INTENT(in), VALUE :: end_pos
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: start_radius
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: end_radius
    INTEGER(kind=C_INT), INTENT(in), VALUE :: sides
    TYPE(color_), INTENT(in), VALUE :: color
  END SUBROUTINE DrawCylinderWiresEx

! void DrawEllipse(int centerX, int centerY, float radiusH, float radiusV, Color color)
  SUBROUTINE DrawEllipse(center_x, center_y, radius_h, radius_v, &
                         color) BIND(c, name='DrawEllipse')
    IMPORT :: C_FLOAT, C_INT, color_
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: center_x
    INTEGER(kind=C_INT), INTENT(in), VALUE :: center_y
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: radius_h
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: radius_v
    TYPE(color_), INTENT(in), VALUE :: color
  END SUBROUTINE DrawEllipse

! void DrawEllipseLines(int centerX, int centerY, float radiusH, float radiusV, Color color)
  SUBROUTINE DrawEllipseLines(center_x, center_y, radius_h, radius_v &
                              , color) BIND(c, name='DrawEllipseLines')
    IMPORT :: C_FLOAT, C_INT, color_
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: center_x
    INTEGER(kind=C_INT), INTENT(in), VALUE :: center_y
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: radius_h
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: radius_v
    TYPE(color_), INTENT(in), VALUE :: color
  END SUBROUTINE DrawEllipseLines

! void DrawFPS(int posX, int posY)
  SUBROUTINE DrawFPS(pos_x, pos_y) BIND(c, name='DrawFPS')
    IMPORT :: C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: pos_x
    INTEGER(kind=C_INT), INTENT(in), VALUE :: pos_y
  END SUBROUTINE DrawFPS

! void DrawGrid(int slices, float spacing)
  SUBROUTINE DrawGrid(slices, spacing) BIND(c, name='DrawGrid')
    IMPORT :: C_FLOAT, C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: slices
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: spacing
  END SUBROUTINE DrawGrid

! void DrawLine(int startPosX, int startPosY, int endPosX, int endPosY, Color color)
  SUBROUTINE DrawLine(start_pos_x, start_pos_y, end_pos_x, end_pos_y &
                      , color) BIND(c, name='DrawLine')
    IMPORT :: C_INT, color_
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: start_pos_x
    INTEGER(kind=C_INT), INTENT(in), VALUE :: start_pos_y
    INTEGER(kind=C_INT), INTENT(in), VALUE :: end_pos_x
    INTEGER(kind=C_INT), INTENT(in), VALUE :: end_pos_y
    TYPE(color_), INTENT(in), VALUE :: color
  END SUBROUTINE DrawLine

! void DrawLine3D(Vector3 startPos, Vector3 endPos, Color color)
  SUBROUTINE DrawLine3D(start_pos, end_pos, color) BIND(c, name='DrawLine3D')
    IMPORT :: color_, vector3_
    IMPLICIT NONE
    TYPE(vector3_), INTENT(in), VALUE :: start_pos
    TYPE(vector3_), INTENT(in), VALUE :: end_pos
    TYPE(color_), INTENT(in), VALUE :: color
  END SUBROUTINE DrawLine3D

! void DrawLineBezier(Vector2 startPos, Vector2 endPos, float thick, Color color)
  SUBROUTINE DrawLineBezier(start_pos, end_pos, thick, color) BIND(c &
                                                      , name='DrawLineBezier')
    IMPORT :: C_FLOAT, color_, vector2_
    IMPLICIT NONE
    TYPE(vector2_), INTENT(in), VALUE :: start_pos
    TYPE(vector2_), INTENT(in), VALUE :: end_pos
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: thick
    TYPE(color_), INTENT(in), VALUE :: color
  END SUBROUTINE DrawLineBezier

! void DrawLineEx(Vector2 startPos, Vector2 endPos, float thick, Color color)
  SUBROUTINE DrawLineEx(start_pos, end_pos, thick, color) BIND(c, &
                                                            name='DrawLineEx')
    IMPORT :: C_FLOAT, color_, vector2_
    IMPLICIT NONE
    TYPE(vector2_), INTENT(in), VALUE :: start_pos
    TYPE(vector2_), INTENT(in), VALUE :: end_pos
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: thick
    TYPE(color_), INTENT(in), VALUE :: color
  END SUBROUTINE DrawLineEx

! void DrawLineStrip(Vector2 *points, int pointCount, Color color)
  SUBROUTINE DrawLineStrip(points, point_count, color) BIND(c, name='DrawLineStrip')
    IMPORT :: C_INT, color_, vector2_
    IMPLICIT NONE
    TYPE(vector2_), INTENT(in) :: points(*)
    INTEGER(kind=C_INT), INTENT(in), VALUE :: point_count
    TYPE(color_), INTENT(in), VALUE :: color
  END SUBROUTINE DrawLineStrip

! void DrawLineV(Vector2 startPos, Vector2 endPos, Color color)
  SUBROUTINE DrawLineV(start_pos, end_pos, color) BIND(c, name='DrawLineV')
    IMPORT :: color_, vector2_
    IMPLICIT NONE
    TYPE(vector2_), INTENT(in), VALUE :: start_pos
    TYPE(vector2_), INTENT(in), VALUE :: end_pos
    TYPE(color_), INTENT(in), VALUE :: color
  END SUBROUTINE DrawLineV

! void DrawMesh(Mesh mesh, Material material, Matrix transform)
  SUBROUTINE DrawMesh(mesh, material, transform) BIND(c, name='DrawMesh')
    IMPORT :: material_, matrix_, mesh_
    IMPLICIT NONE
    TYPE(mesh_), INTENT(in), VALUE :: mesh
    TYPE(material_), INTENT(in), VALUE :: material
    TYPE(matrix_), INTENT(in), VALUE :: transform
  END SUBROUTINE DrawMesh

! void DrawMeshInstanced(Mesh mesh, Material material, const Matrix *transforms, int instances)
  SUBROUTINE DrawMeshInstanced(mesh, material, transforms, instances &
                               ) BIND(c, name='DrawMeshInstanced')
    IMPORT :: C_INT, material_, matrix_, mesh_
    IMPLICIT NONE
    TYPE(mesh_), INTENT(in), VALUE :: mesh
    TYPE(material_), INTENT(in), VALUE :: material
    TYPE(matrix_), INTENT(inout) :: transforms
    INTEGER(kind=C_INT), INTENT(in), VALUE :: instances
  END SUBROUTINE DrawMeshInstanced

! void DrawModel(Model model, Vector3 position, float scale, Color tint)
  SUBROUTINE DrawModel(model, position, scale, tint) BIND(c, name='DrawModel')
    IMPORT :: C_FLOAT, color_, model_, vector3_
    IMPLICIT NONE
    TYPE(model_), INTENT(in), VALUE :: model
    TYPE(vector3_), INTENT(in), VALUE :: position
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: scale
    TYPE(color_), INTENT(in), VALUE :: tint
  END SUBROUTINE DrawModel

! void DrawModelEx(Model model, Vector3 position, Vector3 rotationAxis, float rotationAngle, Vector3 scale, Color tint)
  SUBROUTINE DrawModelEx(model, position, rotation_axis, &
                         rotation_angle, scale, tint) &
    BIND(c, name='DrawModelEx')
    IMPORT :: C_FLOAT, color_, model_, vector3_
    IMPLICIT NONE
    TYPE(model_), INTENT(in), VALUE :: model
    TYPE(vector3_), INTENT(in), VALUE :: position
    TYPE(vector3_), INTENT(in), VALUE :: rotation_axis
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: rotation_angle
    TYPE(vector3_), INTENT(in), VALUE :: scale
    TYPE(color_), INTENT(in), VALUE :: tint
  END SUBROUTINE DrawModelEx

! void DrawModelWires(Model model, Vector3 position, float scale, Color tint)
  SUBROUTINE DrawModelWires(model, position, scale, tint) BIND(c, &
                                                        name='DrawModelWires')
    IMPORT :: C_FLOAT, color_, model_, vector3_
    IMPLICIT NONE
    TYPE(model_), INTENT(in), VALUE :: model
    TYPE(vector3_), INTENT(in), VALUE :: position
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: scale
    TYPE(color_), INTENT(in), VALUE :: tint
  END SUBROUTINE DrawModelWires

! void DrawModelWiresEx(Model model, Vector3 position, Vector3 rotationAxis, float rotationAngle, Vector3 scale, Color tint)
  SUBROUTINE DrawModelWiresEx(model, position, rotation_axis, &
                              rotation_angle, scale, tint) &
    BIND(c, name='DrawModelWiresEx')
    IMPORT :: C_FLOAT, color_, model_, vector3_
    IMPLICIT NONE
    TYPE(model_), INTENT(in), VALUE :: model
    TYPE(vector3_), INTENT(in), VALUE :: position
    TYPE(vector3_), INTENT(in), VALUE :: rotation_axis
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: rotation_angle
    TYPE(vector3_), INTENT(in), VALUE :: scale
    TYPE(color_), INTENT(in), VALUE :: tint
  END SUBROUTINE DrawModelWiresEx

! void DrawPixel(int posX, int posY, Color color)
  SUBROUTINE draw_pixel(pos_x, pos_y, color) BIND(c, name='DrawPixel')
    IMPORT :: C_INT, color_
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: pos_x
    INTEGER(kind=C_INT), INTENT(in), VALUE :: pos_y
    TYPE(color_), INTENT(in), VALUE :: color
  END SUBROUTINE draw_pixel

! void DrawPixelV(Vector2 position, Color color)
  SUBROUTINE draw_pixel_v(position, color) BIND(c, name='DrawPixelV')
    IMPORT :: color_, vector2_
    IMPLICIT NONE
    TYPE(vector2_), INTENT(in), VALUE :: position
    TYPE(color_), INTENT(in), VALUE :: color
  END SUBROUTINE draw_pixel_v

! void DrawPlane(Vector3 centerPos, Vector2 size, Color color)
  SUBROUTINE DrawPlane(center_pos, size, color) BIND(c, name='DrawPlane')
    IMPORT :: color_, vector2_, vector3_
    IMPLICIT NONE
    TYPE(vector3_), INTENT(in), VALUE :: center_pos
    TYPE(vector2_), INTENT(in), VALUE :: size
    TYPE(color_), INTENT(in), VALUE :: color
  END SUBROUTINE DrawPlane

! void DrawPoint3D(Vector3 position, Color color)
  SUBROUTINE DrawPoint3D(position, color) BIND(c, name='DrawPoint3D')
    IMPORT :: color_, vector3_
    IMPLICIT NONE
    TYPE(vector3_), INTENT(in), VALUE :: position
    TYPE(color_), INTENT(in), VALUE :: color
  END SUBROUTINE DrawPoint3D

! void DrawPoly(Vector2 center, int sides, float radius, float rotation, Color color)
  SUBROUTINE DrawPoly(center, sides, radius, rotation, color) BIND(c &
                                                            , name='DrawPoly')
    IMPORT :: C_FLOAT, C_INT, color_, vector2_
    IMPLICIT NONE
    TYPE(vector2_), INTENT(in), VALUE :: center
    INTEGER(kind=C_INT), INTENT(in), VALUE :: sides
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: radius
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: rotation
    TYPE(color_), INTENT(in), VALUE :: color
  END SUBROUTINE DrawPoly

! void DrawPolyLines(Vector2 center, int sides, float radius, float rotation, Color color)
  SUBROUTINE DrawPolyLines(center, sides, radius, rotation, color) &
    BIND(c, name='DrawPolyLines')
    IMPORT :: C_FLOAT, C_INT, color_, vector2_
    IMPLICIT NONE
    TYPE(vector2_), INTENT(in), VALUE :: center
    INTEGER(kind=C_INT), INTENT(in), VALUE :: sides
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: radius
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: rotation
    TYPE(color_), INTENT(in), VALUE :: color
  END SUBROUTINE DrawPolyLines

! void DrawPolyLinesEx(Vector2 center, int sides, float radius, float rotation, float lineThick, Color color)
  SUBROUTINE DrawPolyLinesEx(center, sides, radius, rotation, &
                             line_thick, color) &
    BIND(c, name='DrawPolyLinesEx')
    IMPORT :: C_FLOAT, C_INT, color_, vector2_
    IMPLICIT NONE
    TYPE(vector2_), INTENT(in), VALUE :: center
    INTEGER(kind=C_INT), INTENT(in), VALUE :: sides
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: radius
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: rotation
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: line_thick
    TYPE(color_), INTENT(in), VALUE :: color
  END SUBROUTINE DrawPolyLinesEx

! void DrawRay(Ray ray, Color color)
  SUBROUTINE DrawRay(ray, color) BIND(c, name='DrawRay')
    IMPORT :: color_, ray_
    IMPLICIT NONE
    TYPE(ray_), INTENT(in), VALUE :: ray
    TYPE(color_), INTENT(in), VALUE :: color
  END SUBROUTINE DrawRay

! void DrawRectangle(int posX, int posY, int width, int height, Color color)
  SUBROUTINE DrawRectangle(pos_x, pos_y, width, height, color) BIND(c &
                                                       , name='DrawRectangle')
    IMPORT :: C_INT, color_
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: pos_x
    INTEGER(kind=C_INT), INTENT(in), VALUE :: pos_y
    INTEGER(kind=C_INT), INTENT(in), VALUE :: width
    INTEGER(kind=C_INT), INTENT(in), VALUE :: height
    TYPE(color_), INTENT(in), VALUE :: color
  END SUBROUTINE DrawRectangle

! void DrawRectangleGradientEx(Rectangle rec, Color col1, Color col2, Color col3, Color col4)
  SUBROUTINE DrawRectangleGradientEx(rec, col1, col2, col3, col4) &
    BIND(c, name='DrawRectangleGradientEx')
    IMPORT :: color_, rectangle_
    IMPLICIT NONE
    TYPE(rectangle_), INTENT(in), VALUE :: rec
    TYPE(color_), INTENT(in), VALUE :: col1
    TYPE(color_), INTENT(in), VALUE :: col2
    TYPE(color_), INTENT(in), VALUE :: col3
    TYPE(color_), INTENT(in), VALUE :: col4
  END SUBROUTINE DrawRectangleGradientEx

! void DrawRectangleGradientH(int posX, int posY, int width, int height, Color color1, Color color2)
  SUBROUTINE DrawRectangleGradientH(pos_x, pos_y, width, height, &
                                    color1, color2) &
    BIND(c, name='DrawRectangleGradientH')
    IMPORT :: C_INT, color_
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: pos_x
    INTEGER(kind=C_INT), INTENT(in), VALUE :: pos_y
    INTEGER(kind=C_INT), INTENT(in), VALUE :: width
    INTEGER(kind=C_INT), INTENT(in), VALUE :: height
    TYPE(color_), INTENT(in), VALUE :: color1
    TYPE(color_), INTENT(in), VALUE :: color2
  END SUBROUTINE DrawRectangleGradientH

! void DrawRectangleGradientV(int posX, int posY, int width, int height, Color color1, Color color2)
  SUBROUTINE DrawRectangleGradientV(pos_x, pos_y, width, height, &
                                    color1, color2) &
    BIND(c, name='DrawRectangleGradientV')
    IMPORT :: C_INT, color_
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: pos_x
    INTEGER(kind=C_INT), INTENT(in), VALUE :: pos_y
    INTEGER(kind=C_INT), INTENT(in), VALUE :: width
    INTEGER(kind=C_INT), INTENT(in), VALUE :: height
    TYPE(color_), INTENT(in), VALUE :: color1
    TYPE(color_), INTENT(in), VALUE :: color2
  END SUBROUTINE DrawRectangleGradientV

! void DrawRectangleLines(int posX, int posY, int width, int height, Color color)
  SUBROUTINE DrawRectangleLines(pos_x, pos_y, width, height, color) &
    BIND(c, name='DrawRectangleLines')
    IMPORT :: C_INT, color_
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: pos_x
    INTEGER(kind=C_INT), INTENT(in), VALUE :: pos_y
    INTEGER(kind=C_INT), INTENT(in), VALUE :: width
    INTEGER(kind=C_INT), INTENT(in), VALUE :: height
    TYPE(color_), INTENT(in), VALUE :: color
  END SUBROUTINE DrawRectangleLines

! void DrawRectangleLinesEx(Rectangle rec, float lineThick, Color color)
  SUBROUTINE DrawRectangleLinesEx(rec, line_thick, color) BIND(c, &
                                                  name='DrawRectangleLinesEx')
    IMPORT :: C_FLOAT, color_, rectangle_
    IMPLICIT NONE
    TYPE(rectangle_), INTENT(in), VALUE :: rec
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: line_thick
    TYPE(color_), INTENT(in), VALUE :: color
  END SUBROUTINE DrawRectangleLinesEx

! void DrawRectanglePro(Rectangle rec, Vector2 origin, float rotation, Color color)
  SUBROUTINE DrawRectanglePro(rec, origin, rotation, color) BIND(c, &
                                                      name='DrawRectanglePro')
    IMPORT :: C_FLOAT, color_, rectangle_, vector2_
    IMPLICIT NONE
    TYPE(rectangle_), INTENT(in), VALUE :: rec
    TYPE(vector2_), INTENT(in), VALUE :: origin
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: rotation
    TYPE(color_), INTENT(in), VALUE :: color
  END SUBROUTINE DrawRectanglePro

! void DrawRectangleRec(Rectangle rec, Color color)
  SUBROUTINE DrawRectangleRec(rec, color) BIND(c, name='DrawRectangleRec')
    IMPORT :: color_, rectangle_
    IMPLICIT NONE
    TYPE(rectangle_), INTENT(in), VALUE :: rec
    TYPE(color_), INTENT(in), VALUE :: color
  END SUBROUTINE DrawRectangleRec

! void DrawRectangleRounded(Rectangle rec, float roundness, int segments, Color color)
  SUBROUTINE DrawRectangleRounded(rec, roundness, segments, color) &
    BIND(c, name='DrawRectangleRounded')
    IMPORT :: C_FLOAT, C_INT, color_, rectangle_
    IMPLICIT NONE
    TYPE(rectangle_), INTENT(in), VALUE :: rec
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: roundness
    INTEGER(kind=C_INT), INTENT(in), VALUE :: segments
    TYPE(color_), INTENT(in), VALUE :: color
  END SUBROUTINE DrawRectangleRounded

! void DrawRectangleRoundedLines(Rectangle rec, float roundness, int segments, float lineThick, Color color)
  SUBROUTINE DrawRectangleRoundedLines(rec, roundness, segments, &
                                       line_thick, color) &
    BIND(c, name='DrawRectangleRoundedLines')
    IMPORT :: C_FLOAT, C_INT, color_, rectangle_
    IMPLICIT NONE
    TYPE(rectangle_), INTENT(in), VALUE :: rec
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: roundness
    INTEGER(kind=C_INT), INTENT(in), VALUE :: segments
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: line_thick
    TYPE(color_), INTENT(in), VALUE :: color
  END SUBROUTINE DrawRectangleRoundedLines

! void DrawRectangleV(Vector2 position, Vector2 size, Color color)
  SUBROUTINE DrawRectangleV(position, size, color) BIND(c, &
                                                        name='DrawRectangleV')
    IMPORT :: color_, vector2_
    IMPLICIT NONE
    TYPE(vector2_), INTENT(in), VALUE :: position
    TYPE(vector2_), INTENT(in), VALUE :: size
    TYPE(color_), INTENT(in), VALUE :: color
  END SUBROUTINE DrawRectangleV

! void DrawRing(Vector2 center, float innerRadius, float outerRadius, float startAngle, float endAngle, int segments, Color color)
  SUBROUTINE DrawRing(center, inner_radius, outer_radius, start_angle &
                      , end_angle, segments, color) &
    BIND(c, name='DrawRing')
    IMPORT :: C_FLOAT, C_INT, color_, vector2_
    IMPLICIT NONE
    TYPE(vector2_), INTENT(in), VALUE :: center
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: inner_radius
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: outer_radius
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: start_angle
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: end_angle
    INTEGER(kind=C_INT), INTENT(in), VALUE :: segments
    TYPE(color_), INTENT(in), VALUE :: color
  END SUBROUTINE DrawRing

! void DrawRingLines(Vector2 center, float innerRadius, float outerRadius, float startAngle, float endAngle, int segments, Color color)
  SUBROUTINE DrawRingLines(center, inner_radius, outer_radius, &
                           start_angle, end_angle, segments, color) &
    BIND(c, name='DrawRingLines')
    IMPORT :: C_FLOAT, C_INT, color_, vector2_
    IMPLICIT NONE
    TYPE(vector2_), INTENT(in), VALUE :: center
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: inner_radius
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: outer_radius
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: start_angle
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: end_angle
    INTEGER(kind=C_INT), INTENT(in), VALUE :: segments
    TYPE(color_), INTENT(in), VALUE :: color
  END SUBROUTINE DrawRingLines

! void DrawSphere(Vector3 centerPos, float radius, Color color)
  SUBROUTINE DrawSphere(center_pos, radius, color) BIND(c, name='DrawSphere')
    IMPORT :: C_FLOAT, color_, vector3_
    IMPLICIT NONE
    TYPE(vector3_), INTENT(in), VALUE :: center_pos
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: radius
    TYPE(color_), INTENT(in), VALUE :: color
  END SUBROUTINE DrawSphere

! void DrawSphereEx(Vector3 centerPos, float radius, int rings, int slices, Color color)
  SUBROUTINE DrawSphereEx(center_pos, radius, rings, slices, color) &
    BIND(c, name='DrawSphereEx')
    IMPORT :: C_FLOAT, C_INT, color_, vector3_
    IMPLICIT NONE
    TYPE(vector3_), INTENT(in), VALUE :: center_pos
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: radius
    INTEGER(kind=C_INT), INTENT(in), VALUE :: rings
    INTEGER(kind=C_INT), INTENT(in), VALUE :: slices
    TYPE(color_), INTENT(in), VALUE :: color
  END SUBROUTINE DrawSphereEx

! void DrawSphereWires(Vector3 centerPos, float radius, int rings, int slices, Color color)
  SUBROUTINE DrawSphereWires(center_pos, radius, rings, slices, &
                             color) BIND(c, name='DrawSphereWires')
    IMPORT :: C_FLOAT, C_INT, color_, vector3_
    IMPLICIT NONE
    TYPE(vector3_), INTENT(in), VALUE :: center_pos
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: radius
    INTEGER(kind=C_INT), INTENT(in), VALUE :: rings
    INTEGER(kind=C_INT), INTENT(in), VALUE :: slices
    TYPE(color_), INTENT(in), VALUE :: color
  END SUBROUTINE DrawSphereWires

! void DrawSplineBasis(Vector2 *points, int pointCount, float thick, Color color)
  SUBROUTINE DrawSplineBasis(points, point_count, thick, color) BIND &
    (c, name='DrawSplineBasis')
    IMPORT :: C_FLOAT, C_INT, color_, vector2_
    IMPLICIT NONE
    TYPE(vector2_), INTENT(in) :: points(*)
    INTEGER(kind=C_INT), INTENT(in), VALUE :: point_count
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: thick
    TYPE(color_), INTENT(in), VALUE :: color
  END SUBROUTINE DrawSplineBasis

! void DrawSplineBezierCubic(Vector2 *points, int pointCount, float thick, Color color)
  SUBROUTINE DrawSplineBezierCubic(points, point_count, thick, &
                                  color) BIND(c, name='DrawSplineBezierCubic')
    IMPORT :: C_FLOAT, C_INT, color_, vector2_
    IMPLICIT NONE
    TYPE(vector2_), INTENT(in) :: points(*)
    INTEGER(kind=C_INT), INTENT(in), VALUE :: point_count
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: thick
    TYPE(color_), INTENT(in), VALUE :: color
  END SUBROUTINE DrawSplineBezierCubic

! void DrawSplineBezierQuadratic(Vector2 *points, int pointCount, float thick, Color color)
  SUBROUTINE DrawSplineBezierQuadratic(points, point_count, thick, &
                              color) BIND(c, name='DrawSplineBezierQuadratic')
    IMPORT :: C_FLOAT, C_INT, color_, vector2_
    IMPLICIT NONE
    TYPE(vector2_), INTENT(in) :: points(*)
    INTEGER(kind=C_INT), INTENT(in), VALUE :: point_count
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: thick
    TYPE(color_), INTENT(in), VALUE :: color
  END SUBROUTINE DrawSplineBezierQuadratic

! void DrawSplineCatmullRom(Vector2 *points, int pointCount, float thick, Color color)
  SUBROUTINE DrawSplineCatmullRom(points, point_count, thick, color &
                                  ) BIND(c, name='DrawSplineCatmullRom')
    IMPORT :: C_FLOAT, C_INT, color_, vector2_
    IMPLICIT NONE
    TYPE(vector2_), INTENT(in) :: points(*)
    INTEGER(kind=C_INT), INTENT(in), VALUE :: point_count
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: thick
    TYPE(color_), INTENT(in), VALUE :: color
  END SUBROUTINE DrawSplineCatmullRom

! void DrawSplineLinear(Vector2 *points, int pointCount, float thick, Color color)
  SUBROUTINE DrawSplineLinear(points, point_count, thick, color) &
    BIND(c, name='DrawSplineLinear')
    IMPORT :: C_FLOAT, C_INT, color_, vector2_
    IMPLICIT NONE
    TYPE(vector2_), INTENT(in) :: points(*)
    INTEGER(kind=C_INT), INTENT(in), VALUE :: point_count
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: thick
    TYPE(color_), INTENT(in), VALUE :: color
  END SUBROUTINE DrawSplineLinear

! void DrawSplineSegmentBasis(Vector2 p1, Vector2 p2, Vector2 p3, Vector2 p4, float thick, Color color)
  SUBROUTINE DrawSplineSegmentBasis(p1, p2, p3, p4, thick, color) &
    BIND(c, name='DrawSplineSegmentBasis')
    IMPORT :: C_FLOAT, color_, vector2_
    IMPLICIT NONE
    TYPE(vector2_), INTENT(in), VALUE :: p1
    TYPE(vector2_), INTENT(in), VALUE :: p2
    TYPE(vector2_), INTENT(in), VALUE :: p3
    TYPE(vector2_), INTENT(in), VALUE :: p4
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: thick
    TYPE(color_), INTENT(in), VALUE :: color
  END SUBROUTINE DrawSplineSegmentBasis

! void DrawSplineSegmentBezierCubic(Vector2 p1, Vector2 c2, Vector2 c3, Vector2 p4, float thick, Color color)
  SUBROUTINE DrawSplineSegmentBezierCubic(p1, c2, c3, p4, thick, &
                           color) BIND(c, name='DrawSplineSegmentBezierCubic')
    IMPORT :: C_FLOAT, color_, vector2_
    IMPLICIT NONE
    TYPE(vector2_), INTENT(in), VALUE :: p1
    TYPE(vector2_), INTENT(in), VALUE :: c2
    TYPE(vector2_), INTENT(in), VALUE :: c3
    TYPE(vector2_), INTENT(in), VALUE :: p4
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: thick
    TYPE(color_), INTENT(in), VALUE :: color
  END SUBROUTINE DrawSplineSegmentBezierCubic

! void DrawSplineSegmentBezierQuadratic(Vector2 p1, Vector2 c2, Vector2 p3, float thick, Color color)
  SUBROUTINE DrawSplineSegmentBezierQuadratic(p1, c2, p3, thick, &
                       color) BIND(c, name='DrawSplineSegmentBezierQuadratic')
    IMPORT :: C_FLOAT, color_, vector2_
    IMPLICIT NONE
    TYPE(vector2_), INTENT(in), VALUE :: p1
    TYPE(vector2_), INTENT(in), VALUE :: c2
    TYPE(vector2_), INTENT(in), VALUE :: p3
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: thick
    TYPE(color_), INTENT(in), VALUE :: color
  END SUBROUTINE DrawSplineSegmentBezierQuadratic

! void DrawSplineSegmentCatmullRom(Vector2 p1, Vector2 p2, Vector2 p3, Vector2 p4, float thick, Color color)
  SUBROUTINE DrawSplineSegmentCatmullRom(p1, p2, p3, p4, thick, &
                            color) BIND(c, name='DrawSplineSegmentCatmullRom')
    IMPORT :: C_FLOAT, color_, vector2_
    IMPLICIT NONE
    TYPE(vector2_), INTENT(in), VALUE :: p1
    TYPE(vector2_), INTENT(in), VALUE :: p2
    TYPE(vector2_), INTENT(in), VALUE :: p3
    TYPE(vector2_), INTENT(in), VALUE :: p4
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: thick
    TYPE(color_), INTENT(in), VALUE :: color
  END SUBROUTINE DrawSplineSegmentCatmullRom

! void DrawSplineSegmentLinear(Vector2 p1, Vector2 p2, float thick, Color color)
  SUBROUTINE DrawSplineSegmentLinear(p1, p2, thick, color) BIND(c, &
                                               name='DrawSplineSegmentLinear')
    IMPORT :: C_FLOAT, color_, vector2_
    IMPLICIT NONE
    TYPE(vector2_), INTENT(in), VALUE :: p1
    TYPE(vector2_), INTENT(in), VALUE :: p2
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: thick
    TYPE(color_), INTENT(in), VALUE :: color
  END SUBROUTINE DrawSplineSegmentLinear

! void DrawTriangleStrip3D(Vector3 *points, int pointCount, Color color)
  SUBROUTINE DrawTriangleStrip3D(points, point_count, color) BIND(c &
                                                 , name='DrawTriangleStrip3D')
    IMPORT :: C_INT, color_, vector3_
    IMPLICIT NONE
    TYPE(vector3_), INTENT(in) :: points(*)
    INTEGER(kind=C_INT), INTENT(in), VALUE :: point_count
    TYPE(color_), INTENT(in), VALUE :: color
  END SUBROUTINE DrawTriangleStrip3D

! void DrawText(const char *text, int posX, int posY, int fontSize, Color color)
  SUBROUTINE DrawText(text, pos_x, pos_y, font_size, color) BIND(c, &
                                                              name='DrawText')
    IMPORT :: C_CHAR, C_INT, color_
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: text
    INTEGER(kind=C_INT), INTENT(in), VALUE :: pos_x
    INTEGER(kind=C_INT), INTENT(in), VALUE :: pos_y
    INTEGER(kind=C_INT), INTENT(in), VALUE :: font_size
    TYPE(color_), INTENT(in), VALUE :: color
  END SUBROUTINE DrawText

! void DrawTextCodepoint(Font font, int codepoint, Vector2 position, float fontSize, Color tint)
  SUBROUTINE DrawTextCodepoint(font, codepoint, position, font_size &
                               , tint) BIND(c, name='DrawTextCodepoint')
    IMPORT :: C_FLOAT, C_INT, color_, font_, vector2_
    IMPLICIT NONE
    TYPE(font_), INTENT(in), VALUE :: font
    INTEGER(kind=C_INT), INTENT(in), VALUE :: codepoint
    TYPE(vector2_), INTENT(in), VALUE :: position
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: font_size
    TYPE(color_), INTENT(in), VALUE :: tint
  END SUBROUTINE DrawTextCodepoint

! void DrawTextCodepoints(Font font, const int *codepoints, int codepointCount, Vector2 position, float fontSize, float spacing, Color tint)
  SUBROUTINE DrawTextCodepoints(font, codepoints, codepointCount, &
                                position, font_size, spacing, tint) &
    BIND(c, name='DrawTextCodepoints')
    IMPORT :: C_FLOAT, C_INT, color_, font_, vector2_
    IMPLICIT NONE
    TYPE(font_), INTENT(in), VALUE :: font
    INTEGER(kind=C_INT), INTENT(inout) :: codepoints(*)
    INTEGER(kind=C_INT), INTENT(in), VALUE :: codepointCount
    TYPE(vector2_), INTENT(in), VALUE :: position
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: font_size
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: spacing
    TYPE(color_), INTENT(in), VALUE :: tint
  END SUBROUTINE DrawTextCodepoints

! void DrawTextEx(Font font, const char *text, Vector2 position, float fontSize, float spacing, Color tint)
  SUBROUTINE DrawTextEx(font, text, position, font_size, spacing, &
                        tint) BIND(c, name='DrawTextEx')
    IMPORT :: C_CHAR, C_FLOAT, color_, font_, vector2_
    IMPLICIT NONE
    TYPE(font_), INTENT(in), VALUE :: font
    CHARACTER(kind=C_CHAR), INTENT(in) :: text
    TYPE(vector2_), INTENT(in), VALUE :: position
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: font_size
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: spacing
    TYPE(color_), INTENT(in), VALUE :: tint
  END SUBROUTINE DrawTextEx

! void DrawTextPro(Font font, const char *text, Vector2 position, Vector2 origin, float rotation, float fontSize, float spacing, Color tint)
  SUBROUTINE DrawTextPro(font, text, position, origin, rotation, &
                         font_size, spacing, tint) &
    BIND(c, name='DrawTextPro')
    IMPORT :: C_CHAR, C_FLOAT, color_, font_, vector2_
    IMPLICIT NONE
    TYPE(font_), INTENT(in), VALUE :: font
    CHARACTER(kind=C_CHAR), INTENT(in) :: text
    TYPE(vector2_), INTENT(in), VALUE :: position
    TYPE(vector2_), INTENT(in), VALUE :: origin
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: rotation
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: font_size
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: spacing
    TYPE(color_), INTENT(in), VALUE :: tint
  END SUBROUTINE DrawTextPro

! void DrawTexture(Texture2D texture, int posX, int posY, Color tint)
  SUBROUTINE DrawTexture(texture, pos_x, pos_y, tint) BIND(c, name='DrawTexture')
    IMPORT :: C_INT, color_, texture2d_
    IMPLICIT NONE
    TYPE(texture2d_), INTENT(in), VALUE :: texture
    INTEGER(kind=C_INT), INTENT(in), VALUE :: pos_x
    INTEGER(kind=C_INT), INTENT(in), VALUE :: pos_y
    TYPE(color_), INTENT(in), VALUE :: tint
  END SUBROUTINE DrawTexture

! void DrawTextureEx(Texture2D texture, Vector2 position, float rotation, float scale, Color tint)
  SUBROUTINE DrawTextureEx(texture, position, rotation, scale, tint &
                           ) BIND(c, name='DrawTextureEx')
    IMPORT :: C_FLOAT, color_, texture2d_, vector2_
    IMPLICIT NONE
    TYPE(texture2d_), INTENT(in), VALUE :: texture
    TYPE(vector2_), INTENT(in), VALUE :: position
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: rotation
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: scale
    TYPE(color_), INTENT(in), VALUE :: tint
  END SUBROUTINE DrawTextureEx

! void DrawTextureNPatch(Texture2D texture, NPatchInfo nPatchInfo, Rectangle dest, Vector2 origin, float rotation, Color tint)
  SUBROUTINE DrawTextureNPatch(texture, npatch_info, dest, origin, &
                               rotation, tint) &
    BIND(c, name='DrawTextureNPatch')
    IMPORT :: C_FLOAT, color_, npatch_info_, rectangle_, texture2d_, vector2_
    IMPLICIT NONE
    TYPE(texture2d_), INTENT(in), VALUE :: texture
    TYPE(npatch_info_), INTENT(in), VALUE :: npatch_info
    TYPE(rectangle_), INTENT(in), VALUE :: dest
    TYPE(vector2_), INTENT(in), VALUE :: origin
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: rotation
    TYPE(color_), INTENT(in), VALUE :: tint
  END SUBROUTINE DrawTextureNPatch

! void DrawTexturePro(Texture2D texture, Rectangle source, Rectangle dest, Vector2 origin, float rotation, Color tint)
  SUBROUTINE DrawTexturePro(texture, source, dest, origin, rotation &
                            , tint) BIND(c, name='DrawTexturePro')
    IMPORT :: C_FLOAT, color_, rectangle_, texture2d_, vector2_
    IMPLICIT NONE
    TYPE(texture2d_), INTENT(in), VALUE :: texture
    TYPE(rectangle_), INTENT(in), VALUE :: source
    TYPE(rectangle_), INTENT(in), VALUE :: dest
    TYPE(vector2_), INTENT(in), VALUE :: origin
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: rotation
    TYPE(color_), INTENT(in), VALUE :: tint
  END SUBROUTINE DrawTexturePro

! void DrawTextureRec(Texture2D texture, Rectangle source, Vector2 position, Color tint)
  SUBROUTINE DrawTextureRec(texture, source, position, tint) BIND(c &
                                                      , name='DrawTextureRec')
    IMPORT :: color_, rectangle_, texture2d_, vector2_
    IMPLICIT NONE
    TYPE(texture2d_), INTENT(in), VALUE :: texture
    TYPE(rectangle_), INTENT(in), VALUE :: source
    TYPE(vector2_), INTENT(in), VALUE :: position
    TYPE(color_), INTENT(in), VALUE :: tint
  END SUBROUTINE DrawTextureRec

! void DrawTextureV(Texture2D texture, Vector2 position, Color tint)
  SUBROUTINE DrawTextureV(texture, position, tint) BIND(c, &
                                                        name='DrawTextureV')
    IMPORT :: color_, texture2d_, vector2_
    IMPLICIT NONE
    TYPE(texture2d_), INTENT(in), VALUE :: texture
    TYPE(vector2_), INTENT(in), VALUE :: position
    TYPE(color_), INTENT(in), VALUE :: tint
  END SUBROUTINE DrawTextureV

! void DrawTriangle(Vector2 v1, Vector2 v2, Vector2 v3, Color color)
  SUBROUTINE DrawTriangle(v1, v2, v3, color) BIND(c, name='DrawTriangle')
    IMPORT :: color_, vector2_
    IMPLICIT NONE
    TYPE(vector2_), INTENT(in), VALUE :: v1
    TYPE(vector2_), INTENT(in), VALUE :: v2
    TYPE(vector2_), INTENT(in), VALUE :: v3
    TYPE(color_), INTENT(in), VALUE :: color
  END SUBROUTINE DrawTriangle

! void DrawTriangle3D(Vector3 v1, Vector3 v2, Vector3 v3, Color color)
  SUBROUTINE DrawTriangle3D(v1, v2, v3, color) BIND(c, name='DrawTriangle3D')
    IMPORT :: color_, vector3_
    IMPLICIT NONE
    TYPE(vector3_), INTENT(in), VALUE :: v1
    TYPE(vector3_), INTENT(in), VALUE :: v2
    TYPE(vector3_), INTENT(in), VALUE :: v3
    TYPE(color_), INTENT(in), VALUE :: color
  END SUBROUTINE DrawTriangle3D

! void DrawTriangleFan(Vector2 *points, int pointCount, Color color)
  SUBROUTINE DrawTriangleFan(points, point_count, color) BIND(c, &
                                                       name='DrawTriangleFan')
    IMPORT :: C_INT, color_, vector2_
    IMPLICIT NONE
    TYPE(vector2_), INTENT(in) :: points(*)
    INTEGER(kind=C_INT), INTENT(in), VALUE :: point_count
    TYPE(color_), INTENT(in), VALUE :: color
  END SUBROUTINE DrawTriangleFan

! void DrawTriangleLines(Vector2 v1, Vector2 v2, Vector2 v3, Color color)
  SUBROUTINE DrawTriangleLines(v1, v2, v3, color) BIND(c, &
                                                     name='DrawTriangleLines')
    IMPORT :: color_, vector2_
    IMPLICIT NONE
    TYPE(vector2_), INTENT(in), VALUE :: v1
    TYPE(vector2_), INTENT(in), VALUE :: v2
    TYPE(vector2_), INTENT(in), VALUE :: v3
    TYPE(color_), INTENT(in), VALUE :: color
  END SUBROUTINE DrawTriangleLines

! void DrawTriangleStrip(Vector2 *points, int pointCount, Color color)
  SUBROUTINE DrawTriangleStrip(points, point_count, color) BIND(c, &
                                                     name='DrawTriangleStrip')
    IMPORT :: C_INT, color_, vector2_
    IMPLICIT NONE
    TYPE(vector2_), INTENT(in) :: points(*)
    INTEGER(kind=C_INT), INTENT(in), VALUE :: point_count
    TYPE(color_), INTENT(in), VALUE :: color
  END SUBROUTINE DrawTriangleStrip

END INTERFACE

END MODULE RaylibDrawMethods
