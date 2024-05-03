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

PUBLIC :: draw_billboard
PUBLIC :: draw_billboard_pro
PUBLIC :: draw_billboard_rec
PUBLIC :: draw_bounding_box
PUBLIC :: draw_capsule
PUBLIC :: draw_capsule_wires
PUBLIC :: draw_circle
PUBLIC :: draw_circle3d
PUBLIC :: draw_circle_gradient
PUBLIC :: draw_circle_lines
PUBLIC :: draw_circle_lines_v
PUBLIC :: draw_circle_sector
PUBLIC :: draw_circle_sector_lines
PUBLIC :: draw_circle_v
PUBLIC :: draw_cube
PUBLIC :: draw_cube_v
PUBLIC :: draw_cube_wires
PUBLIC :: draw_cube_wires_v
PUBLIC :: draw_cylinder
PUBLIC :: draw_cylinder_ex
PUBLIC :: draw_cylinder_wires
PUBLIC :: draw_cylinder_wires_ex
PUBLIC :: draw_ellipse
PUBLIC :: draw_ellipse_lines
PUBLIC :: draw_fps
PUBLIC :: draw_grid
PUBLIC :: draw_line
PUBLIC :: draw_line3d
PUBLIC :: draw_line_bezier
PUBLIC :: draw_line_ex
PUBLIC :: draw_line_strip
PUBLIC :: draw_line_v
PUBLIC :: draw_mesh
PUBLIC :: draw_mesh_instanced
PUBLIC :: draw_model
PUBLIC :: draw_model_ex
PUBLIC :: draw_model_wires
PUBLIC :: draw_model_wires_ex
PUBLIC :: draw_pixel
PUBLIC :: draw_pixel_v
PUBLIC :: draw_plane
PUBLIC :: draw_point3d
PUBLIC :: draw_poly
PUBLIC :: draw_poly_lines
PUBLIC :: draw_poly_lines_ex
PUBLIC :: draw_ray
PUBLIC :: draw_rectangle
PUBLIC :: draw_rectangle_gradient_ex
PUBLIC :: draw_rectangle_gradient_h
PUBLIC :: draw_rectangle_gradient_v
PUBLIC :: draw_rectangle_lines
PUBLIC :: draw_rectangle_lines_ex
PUBLIC :: draw_rectangle_pro
PUBLIC :: draw_rectangle_rec
PUBLIC :: draw_rectangle_rounded
PUBLIC :: draw_rectangle_rounded_lines
PUBLIC :: draw_rectangle_v
PUBLIC :: draw_ring
PUBLIC :: draw_ring_lines
PUBLIC :: draw_sphere
PUBLIC :: draw_sphere_ex
PUBLIC :: draw_sphere_wires
PUBLIC :: draw_spline_basis
PUBLIC :: draw_spline_bezier_cubic
PUBLIC :: draw_spline_bezier_quadratic
PUBLIC :: draw_spline_catmull_rom
PUBLIC :: draw_spline_linear
PUBLIC :: draw_spline_segment_basis
PUBLIC :: draw_spline_segment_bezier_cubic
PUBLIC :: draw_spline_segment_bezier_quadratic
PUBLIC :: draw_spline_segment_catmull_rom
PUBLIC :: draw_spline_segment_linear
PUBLIC :: draw_text
PUBLIC :: draw_text_codepoint
PUBLIC :: draw_text_codepoints
PUBLIC :: draw_text_ex
PUBLIC :: draw_text_pro
PUBLIC :: draw_texture
PUBLIC :: draw_texture_ex
PUBLIC :: draw_texture_npatch
PUBLIC :: draw_texture_pro
PUBLIC :: draw_texture_rec
PUBLIC :: draw_texture_v
PUBLIC :: draw_triangle
PUBLIC :: draw_triangle3d
PUBLIC :: draw_triangle_fan
PUBLIC :: draw_triangle_lines
PUBLIC :: draw_triangle_strip
PUBLIC :: draw_triangle_strip3d

INTERFACE

! void DrawBillboard(Camera camera, Texture2D texture, Vector3 position, float size, Color tint)
  SUBROUTINE draw_billboard(camera, texture, position, size, tint) &
    BIND(c, name='DrawBillboard')
    IMPORT :: C_FLOAT, camera3d_type, color_type, texture2d_type, vector3_type
    IMPLICIT NONE
    TYPE(camera3d_type), INTENT(in), VALUE :: camera
    TYPE(texture2d_type), INTENT(in), VALUE :: texture
    TYPE(vector3_type), INTENT(in), VALUE :: position
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: size
    TYPE(color_type), INTENT(in), VALUE :: tint
  END SUBROUTINE draw_billboard

! void DrawBillboardPro(Camera camera, Texture2D texture, Rectangle source, Vector3 position, Vector3 up, Vector2 size, Vector2 origin, float rotation, Color tint)
  SUBROUTINE draw_billboard_pro(camera, texture, source, position, up &
                                , size, origin, rotation, tint) &
    BIND(c, name='DrawBillboardPro')
    IMPORT :: C_FLOAT, camera3d_type, color_type, rectangle_type, &
      texture2d_type, vector2_type, vector3_type
    IMPLICIT NONE
    TYPE(camera3d_type), INTENT(in), VALUE :: camera
    TYPE(texture2d_type), INTENT(in), VALUE :: texture
    TYPE(rectangle_type), INTENT(in), VALUE :: source
    TYPE(vector3_type), INTENT(in), VALUE :: position
    TYPE(vector3_type), INTENT(in), VALUE :: up
    TYPE(vector2_type), INTENT(in), VALUE :: size
    TYPE(vector2_type), INTENT(in), VALUE :: origin
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: rotation
    TYPE(color_type), INTENT(in), VALUE :: tint
  END SUBROUTINE draw_billboard_pro

! void DrawBillboardRec(Camera camera, Texture2D texture, Rectangle source, Vector3 position, Vector2 size, Color tint)
  SUBROUTINE draw_billboard_rec(camera, texture, source, position, &
                                size, tint) BIND(c, name='DrawBillboardRec')
    IMPORT :: camera3d_type, color_type, rectangle_type, &
      texture2d_type, vector2_type, vector3_type
    IMPLICIT NONE
    TYPE(camera3d_type), INTENT(in), VALUE :: camera
    TYPE(texture2d_type), INTENT(in), VALUE :: texture
    TYPE(rectangle_type), INTENT(in), VALUE :: source
    TYPE(vector3_type), INTENT(in), VALUE :: position
    TYPE(vector2_type), INTENT(in), VALUE :: size
    TYPE(color_type), INTENT(in), VALUE :: tint
  END SUBROUTINE draw_billboard_rec

! void DrawBoundingBox(BoundingBox box, Color color)
  SUBROUTINE draw_bounding_box(box, color) BIND(c, name='DrawBoundingBox')
    IMPORT :: bounding_box_type, color_type
    IMPLICIT NONE
    TYPE(bounding_box_type), INTENT(in), VALUE :: box
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE draw_bounding_box

! void DrawCapsule(Vector3 startPos, Vector3 endPos, float radius, int slices, int rings, Color color)
  SUBROUTINE draw_capsule(start_pos, end_pos, radius, slices, rings, &
                          color) BIND(c, name='DrawCapsule')
    IMPORT :: C_FLOAT, C_INT, color_type, vector3_type
    IMPLICIT NONE
    TYPE(vector3_type), INTENT(in), VALUE :: start_pos
    TYPE(vector3_type), INTENT(in), VALUE :: end_pos
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: radius
    INTEGER(kind=C_INT), INTENT(in), VALUE :: slices
    INTEGER(kind=C_INT), INTENT(in), VALUE :: rings
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE draw_capsule

! void DrawCapsuleWires(Vector3 startPos, Vector3 endPos, float radius, int slices, int rings, Color color)
  SUBROUTINE draw_capsule_wires(start_pos, end_pos, radius, slices, &
                                rings, color) BIND(c, name='DrawCapsuleWires')
    IMPORT :: C_FLOAT, C_INT, color_type, vector3_type
    IMPLICIT NONE
    TYPE(vector3_type), INTENT(in), VALUE :: start_pos
    TYPE(vector3_type), INTENT(in), VALUE :: end_pos
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: radius
    INTEGER(kind=C_INT), INTENT(in), VALUE :: slices
    INTEGER(kind=C_INT), INTENT(in), VALUE :: rings
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE draw_capsule_wires

! void DrawCircle(int centerX, int centerY, float radius, Color color)
        subroutine draw_circle(center_x, center_y, radius, color) bind(c, name='DrawCircle')
    IMPORT :: C_FLOAT, C_INT, color_type
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: center_x
    INTEGER(kind=C_INT), INTENT(in), VALUE :: center_y
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: radius
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE draw_circle

! void DrawCircle3D(Vector3 center, float radius, Vector3 rotationAxis, float rotationAngle, Color color)
  SUBROUTINE draw_circle3d(center, radius, rotation_axis, &
                           rotation_angle, color) BIND(c, name='DrawCircle3D')
    IMPORT :: C_FLOAT, color_type, vector3_type
    IMPLICIT NONE
    TYPE(vector3_type), INTENT(in), VALUE :: center
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: radius
    TYPE(vector3_type), INTENT(in), VALUE :: rotation_axis
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: rotation_angle
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE draw_circle3d

! void DrawCircleGradient(int centerX, int centerY, float radius, Color color1, Color color2)
  SUBROUTINE draw_circle_gradient(center_x, center_y, radius, color1, &
                                  color2) BIND(c, name='DrawCircleGradient')
    IMPORT :: C_FLOAT, C_INT, color_type
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: center_x
    INTEGER(kind=C_INT), INTENT(in), VALUE :: center_y
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: radius
    TYPE(color_type), INTENT(in), VALUE :: color1
    TYPE(color_type), INTENT(in), VALUE :: color2
  END SUBROUTINE draw_circle_gradient

! void DrawCircleLines(int centerX, int centerY, float radius, Color color)
  SUBROUTINE draw_circle_lines(center_x, center_y, radius, color) BIND &
    (c, name='DrawCircleLines')
    IMPORT :: C_FLOAT, C_INT, color_type
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: center_x
    INTEGER(kind=C_INT), INTENT(in), VALUE :: center_y
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: radius
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE draw_circle_lines

! void DrawCircleLinesV(Vector2 center, float radius, Color color)
  SUBROUTINE draw_circle_lines_v(center, radius, color) BIND(c, name='&
&            DrawCircleLinesV')
    IMPORT :: C_FLOAT, color_type, vector2_type
    IMPLICIT NONE
    TYPE(vector2_type), INTENT(in), VALUE :: center
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: radius
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE draw_circle_lines_v

! void DrawCircleSector(Vector2 center, float radius, float startAngle, float endAngle, int segments, Color color)
  SUBROUTINE draw_circle_sector(center, radius, start_angle, end_angle &
                                , segments, color) &
    BIND(c, name='DrawCircleSector')
    IMPORT :: C_FLOAT, C_INT, color_type, vector2_type
    IMPLICIT NONE
    TYPE(vector2_type), INTENT(in), VALUE :: center
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: radius
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: start_angle
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: end_angle
    INTEGER(kind=C_INT), INTENT(in), VALUE :: segments
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE draw_circle_sector

! void DrawCircleSectorLines(Vector2 center, float radius, float startAngle, float endAngle, int segments, Color color)
  SUBROUTINE draw_circle_sector_lines(center, radius, start_angle, &
                                      end_angle, segments, color) &
    BIND(c, name='DrawCircleSectorLines')
    IMPORT :: C_FLOAT, C_INT, color_type, vector2_type
    IMPLICIT NONE
    TYPE(vector2_type), INTENT(in), VALUE :: center
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: radius
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: start_angle
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: end_angle
    INTEGER(kind=C_INT), INTENT(in), VALUE :: segments
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE draw_circle_sector_lines

! void DrawCircleV(Vector2 center, float radius, Color color)
  SUBROUTINE draw_circle_v(center, radius, color) BIND(c, name='DrawCircleV')
    IMPORT :: C_FLOAT, color_type, vector2_type
    IMPLICIT NONE
    TYPE(vector2_type), INTENT(in), VALUE :: center
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: radius
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE draw_circle_v

! void DrawCube(Vector3 position, float width, float height, float length, Color color)
  SUBROUTINE draw_cube(position, width, height, length, color) BIND(c &
                                                            , name='DrawCube')
    IMPORT :: C_FLOAT, color_type, vector3_type
    IMPLICIT NONE
    TYPE(vector3_type), INTENT(in), VALUE :: position
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: width
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: height
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: length
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE draw_cube

! void DrawCubeV(Vector3 position, Vector3 size, Color color)
  SUBROUTINE draw_cube_v(position, size, color) BIND(c, name='DrawCubeV')
    IMPORT :: color_type, vector3_type
    IMPLICIT NONE
    TYPE(vector3_type), INTENT(in), VALUE :: position
    TYPE(vector3_type), INTENT(in), VALUE :: size
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE draw_cube_v

! void DrawCubeWires(Vector3 position, float width, float height, float length, Color color)
  SUBROUTINE draw_cube_wires(position, width, height, length, color) &
    BIND(c, name='DrawCubeWires')
    IMPORT :: C_FLOAT, color_type, vector3_type
    IMPLICIT NONE
    TYPE(vector3_type), INTENT(in), VALUE :: position
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: width
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: height
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: length
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE draw_cube_wires

! void DrawCubeWiresV(Vector3 position, Vector3 size, Color color)
  SUBROUTINE draw_cube_wires_v(position, size, color) BIND(c, name='&
&            DrawCubeWiresV')
    IMPORT :: color_type, vector3_type
    IMPLICIT NONE
    TYPE(vector3_type), INTENT(in), VALUE :: position
    TYPE(vector3_type), INTENT(in), VALUE :: size
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE draw_cube_wires_v

! void DrawCylinder(Vector3 position, float radiusTop, float radiusBottom, float height, int slices, Color color)
  SUBROUTINE draw_cylinder(position, radius_top, radius_bottom, height &
                           , slices, color) BIND(c, name='DrawCylinder')
    IMPORT :: C_FLOAT, C_INT, color_type, vector3_type
    IMPLICIT NONE
    TYPE(vector3_type), INTENT(in), VALUE :: position
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: radius_top
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: radius_bottom
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: height
    INTEGER(kind=C_INT), INTENT(in), VALUE :: slices
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE draw_cylinder

! void DrawCylinderEx(Vector3 startPos, Vector3 endPos, float startRadius, float endRadius, int sides, Color color)
  SUBROUTINE draw_cylinder_ex(start_pos, end_pos, start_radius, &
                      end_radius, sides, color) BIND(c, name='DrawCylinderEx')
    IMPORT :: C_FLOAT, C_INT, color_type, vector3_type
    IMPLICIT NONE
    TYPE(vector3_type), INTENT(in), VALUE :: start_pos
    TYPE(vector3_type), INTENT(in), VALUE :: end_pos
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: start_radius
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: end_radius
    INTEGER(kind=C_INT), INTENT(in), VALUE :: sides
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE draw_cylinder_ex

! void DrawCylinderWires(Vector3 position, float radiusTop, float radiusBottom, float height, int slices, Color color)
  SUBROUTINE draw_cylinder_wires(position, radius_top, radius_bottom, &
                                 height, slices, color) &
    BIND(c, name='DrawCylinderWires')
    IMPORT :: C_FLOAT, C_INT, color_type, vector3_type
    IMPLICIT NONE
    TYPE(vector3_type), INTENT(in), VALUE :: position
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: radius_top
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: radius_bottom
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: height
    INTEGER(kind=C_INT), INTENT(in), VALUE :: slices
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE draw_cylinder_wires

! void DrawCylinderWiresEx(Vector3 startPos, Vector3 endPos, float startRadius, float endRadius, int sides, Color color)
  SUBROUTINE draw_cylinder_wires_ex(start_pos, end_pos, start_radius, &
                                    end_radius, sides, color) &
    BIND(c, name='DrawCylinderWiresEx')
    IMPORT :: C_FLOAT, C_INT, color_type, vector3_type
    IMPLICIT NONE
    TYPE(vector3_type), INTENT(in), VALUE :: start_pos
    TYPE(vector3_type), INTENT(in), VALUE :: end_pos
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: start_radius
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: end_radius
    INTEGER(kind=C_INT), INTENT(in), VALUE :: sides
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE draw_cylinder_wires_ex

! void DrawEllipse(int centerX, int centerY, float radiusH, float radiusV, Color color)
  SUBROUTINE draw_ellipse(center_x, center_y, radius_h, radius_v, &
                          color) BIND(c, name='DrawEllipse')
    IMPORT :: C_FLOAT, C_INT, color_type
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: center_x
    INTEGER(kind=C_INT), INTENT(in), VALUE :: center_y
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: radius_h
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: radius_v
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE draw_ellipse

! void DrawEllipseLines(int centerX, int centerY, float radiusH, float radiusV, Color color)
  SUBROUTINE draw_ellipse_lines(center_x, center_y, radius_h, radius_v &
                                , color) BIND(c, name='DrawEllipseLines')
    IMPORT :: C_FLOAT, C_INT, color_type
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: center_x
    INTEGER(kind=C_INT), INTENT(in), VALUE :: center_y
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: radius_h
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: radius_v
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE draw_ellipse_lines

! void DrawFPS(int posX, int posY)
  SUBROUTINE draw_fps(pos_x, pos_y) BIND(c, name='DrawFPS')
    IMPORT :: C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: pos_x
    INTEGER(kind=C_INT), INTENT(in), VALUE :: pos_y
  END SUBROUTINE draw_fps

! void DrawGrid(int slices, float spacing)
  SUBROUTINE draw_grid(slices, spacing) BIND(c, name='DrawGrid')
    IMPORT :: C_FLOAT, C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: slices
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: spacing
  END SUBROUTINE draw_grid

! void DrawLine(int startPosX, int startPosY, int endPosX, int endPosY, Color color)
  SUBROUTINE draw_line(start_pos_x, start_pos_y, end_pos_x, end_pos_y &
                       , color) BIND(c, name='DrawLine')
    IMPORT :: C_INT, color_type
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: start_pos_x
    INTEGER(kind=C_INT), INTENT(in), VALUE :: start_pos_y
    INTEGER(kind=C_INT), INTENT(in), VALUE :: end_pos_x
    INTEGER(kind=C_INT), INTENT(in), VALUE :: end_pos_y
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE draw_line

! void DrawLine3D(Vector3 startPos, Vector3 endPos, Color color)
  SUBROUTINE draw_line3d(start_pos, end_pos, color) BIND(c, name='DrawLine3D')
    IMPORT :: color_type, vector3_type
    IMPLICIT NONE
    TYPE(vector3_type), INTENT(in), VALUE :: start_pos
    TYPE(vector3_type), INTENT(in), VALUE :: end_pos
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE draw_line3d

! void DrawLineBezier(Vector2 startPos, Vector2 endPos, float thick, Color color)
  SUBROUTINE draw_line_bezier(start_pos, end_pos, thick, color) BIND(c &
                                                      , name='DrawLineBezier')
    IMPORT :: C_FLOAT, color_type, vector2_type
    IMPLICIT NONE
    TYPE(vector2_type), INTENT(in), VALUE :: start_pos
    TYPE(vector2_type), INTENT(in), VALUE :: end_pos
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: thick
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE draw_line_bezier

! void DrawLineEx(Vector2 startPos, Vector2 endPos, float thick, Color color)
  SUBROUTINE draw_line_ex(start_pos, end_pos, thick, color) BIND(c, &
                                                            name='DrawLineEx')
    IMPORT :: C_FLOAT, color_type, vector2_type
    IMPLICIT NONE
    TYPE(vector2_type), INTENT(in), VALUE :: start_pos
    TYPE(vector2_type), INTENT(in), VALUE :: end_pos
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: thick
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE draw_line_ex

! void DrawLineStrip(Vector2 *points, int pointCount, Color color)
  SUBROUTINE draw_line_strip(points, point_count, color) BIND(c, name &
                                                             ='DrawLineStrip')
    IMPORT :: C_INT, color_type, vector2_type
    IMPLICIT NONE
    TYPE(vector2_type), INTENT(in) :: points(*)
    INTEGER(kind=C_INT), INTENT(in), VALUE :: point_count
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE draw_line_strip

! void DrawLineV(Vector2 startPos, Vector2 endPos, Color color)
  SUBROUTINE draw_line_v(start_pos, end_pos, color) BIND(c, name='DrawLineV')
    IMPORT :: color_type, vector2_type
    IMPLICIT NONE
    TYPE(vector2_type), INTENT(in), VALUE :: start_pos
    TYPE(vector2_type), INTENT(in), VALUE :: end_pos
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE draw_line_v

! void DrawMesh(Mesh mesh, Material material, Matrix transform)
  SUBROUTINE draw_mesh(mesh, material, transform) BIND(c, name='DrawMesh')
    IMPORT :: material_type, matrix_type, mesh_type
    IMPLICIT NONE
    TYPE(mesh_type), INTENT(in), VALUE :: mesh
    TYPE(material_type), INTENT(in), VALUE :: material
    TYPE(matrix_type), INTENT(in), VALUE :: transform
  END SUBROUTINE draw_mesh

! void DrawMeshInstanced(Mesh mesh, Material material, const Matrix *transforms, int instances)
  SUBROUTINE draw_mesh_instanced(mesh, material, transforms, instances &
                                 ) BIND(c, name='DrawMeshInstanced')
    IMPORT :: C_INT, material_type, matrix_type, mesh_type
    IMPLICIT NONE
    TYPE(mesh_type), INTENT(in), VALUE :: mesh
    TYPE(material_type), INTENT(in), VALUE :: material
    TYPE(matrix_type), INTENT(inout) :: transforms
    INTEGER(kind=C_INT), INTENT(in), VALUE :: instances
  END SUBROUTINE draw_mesh_instanced

! void DrawModel(Model model, Vector3 position, float scale, Color tint)
 SUBROUTINE draw_model(model, position, scale, tint) BIND(c, name='DrawModel')
    IMPORT :: C_FLOAT, color_type, model_type, vector3_type
    IMPLICIT NONE
    TYPE(model_type), INTENT(in), VALUE :: model
    TYPE(vector3_type), INTENT(in), VALUE :: position
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: scale
    TYPE(color_type), INTENT(in), VALUE :: tint
  END SUBROUTINE draw_model

! void DrawModelEx(Model model, Vector3 position, Vector3 rotationAxis, float rotationAngle, Vector3 scale, Color tint)
  SUBROUTINE draw_model_ex(model, position, rotation_axis, &
                           rotation_angle, scale, tint) &
    BIND(c, name='DrawModelEx')
    IMPORT :: C_FLOAT, color_type, model_type, vector3_type
    IMPLICIT NONE
    TYPE(model_type), INTENT(in), VALUE :: model
    TYPE(vector3_type), INTENT(in), VALUE :: position
    TYPE(vector3_type), INTENT(in), VALUE :: rotation_axis
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: rotation_angle
    TYPE(vector3_type), INTENT(in), VALUE :: scale
    TYPE(color_type), INTENT(in), VALUE :: tint
  END SUBROUTINE draw_model_ex

! void DrawModelWires(Model model, Vector3 position, float scale, Color tint)
  SUBROUTINE draw_model_wires(model, position, scale, tint) BIND(c, &
                                                        name='DrawModelWires')
    IMPORT :: C_FLOAT, color_type, model_type, vector3_type
    IMPLICIT NONE
    TYPE(model_type), INTENT(in), VALUE :: model
    TYPE(vector3_type), INTENT(in), VALUE :: position
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: scale
    TYPE(color_type), INTENT(in), VALUE :: tint
  END SUBROUTINE draw_model_wires

! void DrawModelWiresEx(Model model, Vector3 position, Vector3 rotationAxis, float rotationAngle, Vector3 scale, Color tint)
  SUBROUTINE draw_model_wires_ex(model, position, rotation_axis, &
                                 rotation_angle, scale, tint) &
    BIND(c, name='DrawModelWiresEx')
    IMPORT :: C_FLOAT, color_type, model_type, vector3_type
    IMPLICIT NONE
    TYPE(model_type), INTENT(in), VALUE :: model
    TYPE(vector3_type), INTENT(in), VALUE :: position
    TYPE(vector3_type), INTENT(in), VALUE :: rotation_axis
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: rotation_angle
    TYPE(vector3_type), INTENT(in), VALUE :: scale
    TYPE(color_type), INTENT(in), VALUE :: tint
  END SUBROUTINE draw_model_wires_ex

! void DrawPixel(int posX, int posY, Color color)
  SUBROUTINE draw_pixel(pos_x, pos_y, color) BIND(c, name='DrawPixel')
    IMPORT :: C_INT, color_type
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: pos_x
    INTEGER(kind=C_INT), INTENT(in), VALUE :: pos_y
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE draw_pixel

! void DrawPixelV(Vector2 position, Color color)
  SUBROUTINE draw_pixel_v(position, color) BIND(c, name='DrawPixelV')
    IMPORT :: color_type, vector2_type
    IMPLICIT NONE
    TYPE(vector2_type), INTENT(in), VALUE :: position
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE draw_pixel_v

! void DrawPlane(Vector3 centerPos, Vector2 size, Color color)
  SUBROUTINE draw_plane(center_pos, size, color) BIND(c, name='DrawPlane')
    IMPORT :: color_type, vector2_type, vector3_type
    IMPLICIT NONE
    TYPE(vector3_type), INTENT(in), VALUE :: center_pos
    TYPE(vector2_type), INTENT(in), VALUE :: size
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE draw_plane

! void DrawPoint3D(Vector3 position, Color color)
  SUBROUTINE draw_point3d(position, color) BIND(c, name='DrawPoint3D')
    IMPORT :: color_type, vector3_type
    IMPLICIT NONE
    TYPE(vector3_type), INTENT(in), VALUE :: position
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE draw_point3d

! void DrawPoly(Vector2 center, int sides, float radius, float rotation, Color color)
  SUBROUTINE draw_poly(center, sides, radius, rotation, color) BIND(c &
                                                            , name='DrawPoly')
    IMPORT :: C_FLOAT, C_INT, color_type, vector2_type
    IMPLICIT NONE
    TYPE(vector2_type), INTENT(in), VALUE :: center
    INTEGER(kind=C_INT), INTENT(in), VALUE :: sides
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: radius
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: rotation
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE draw_poly

! void DrawPolyLines(Vector2 center, int sides, float radius, float rotation, Color color)
  SUBROUTINE draw_poly_lines(center, sides, radius, rotation, color) &
    BIND(c, name='DrawPolyLines')
    IMPORT :: C_FLOAT, C_INT, color_type, vector2_type
    IMPLICIT NONE
    TYPE(vector2_type), INTENT(in), VALUE :: center
    INTEGER(kind=C_INT), INTENT(in), VALUE :: sides
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: radius
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: rotation
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE draw_poly_lines

! void DrawPolyLinesEx(Vector2 center, int sides, float radius, float rotation, float lineThick, Color color)
  SUBROUTINE draw_poly_lines_ex(center, sides, radius, rotation, &
                                line_thick, color) &
    BIND(c, name='DrawPolyLinesEx')
    IMPORT :: C_FLOAT, C_INT, color_type, vector2_type
    IMPLICIT NONE
    TYPE(vector2_type), INTENT(in), VALUE :: center
    INTEGER(kind=C_INT), INTENT(in), VALUE :: sides
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: radius
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: rotation
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: line_thick
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE draw_poly_lines_ex

! void DrawRay(Ray ray, Color color)
  SUBROUTINE draw_ray(ray, color) BIND(c, name='DrawRay')
    IMPORT :: color_type, ray_type
    IMPLICIT NONE
    TYPE(ray_type), INTENT(in), VALUE :: ray
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE draw_ray

! void DrawRectangle(int posX, int posY, int width, int height, Color color)
  SUBROUTINE draw_rectangle(pos_x, pos_y, width, height, color) BIND(c &
                                                       , name='DrawRectangle')
    IMPORT :: C_INT, color_type
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: pos_x
    INTEGER(kind=C_INT), INTENT(in), VALUE :: pos_y
    INTEGER(kind=C_INT), INTENT(in), VALUE :: width
    INTEGER(kind=C_INT), INTENT(in), VALUE :: height
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE draw_rectangle

! void DrawRectangleGradientEx(Rectangle rec, Color col1, Color col2, Color col3, Color col4)
  SUBROUTINE draw_rectangle_gradient_ex(rec, col1, col2, col3, col4) &
    BIND(c, name='DrawRectangleGradientEx')
    IMPORT :: color_type, rectangle_type
    IMPLICIT NONE
    TYPE(rectangle_type), INTENT(in), VALUE :: rec
    TYPE(color_type), INTENT(in), VALUE :: col1
    TYPE(color_type), INTENT(in), VALUE :: col2
    TYPE(color_type), INTENT(in), VALUE :: col3
    TYPE(color_type), INTENT(in), VALUE :: col4
  END SUBROUTINE draw_rectangle_gradient_ex

! void DrawRectangleGradientH(int posX, int posY, int width, int height, Color color1, Color color2)
  SUBROUTINE draw_rectangle_gradient_h(pos_x, pos_y, width, height, &
                                       color1, color2) &
    BIND(c, name='DrawRectangleGradientH')
    IMPORT :: C_INT, color_type
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: pos_x
    INTEGER(kind=C_INT), INTENT(in), VALUE :: pos_y
    INTEGER(kind=C_INT), INTENT(in), VALUE :: width
    INTEGER(kind=C_INT), INTENT(in), VALUE :: height
    TYPE(color_type), INTENT(in), VALUE :: color1
    TYPE(color_type), INTENT(in), VALUE :: color2
  END SUBROUTINE draw_rectangle_gradient_h

! void DrawRectangleGradientV(int posX, int posY, int width, int height, Color color1, Color color2)
  SUBROUTINE draw_rectangle_gradient_v(pos_x, pos_y, width, height, &
                                       color1, color2) &
    BIND(c, name='DrawRectangleGradientV')
    IMPORT :: C_INT, color_type
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: pos_x
    INTEGER(kind=C_INT), INTENT(in), VALUE :: pos_y
    INTEGER(kind=C_INT), INTENT(in), VALUE :: width
    INTEGER(kind=C_INT), INTENT(in), VALUE :: height
    TYPE(color_type), INTENT(in), VALUE :: color1
    TYPE(color_type), INTENT(in), VALUE :: color2
  END SUBROUTINE draw_rectangle_gradient_v

! void DrawRectangleLines(int posX, int posY, int width, int height, Color color)
  SUBROUTINE draw_rectangle_lines(pos_x, pos_y, width, height, color) &
    BIND(c, name='DrawRectangleLines')
    IMPORT :: C_INT, color_type
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: pos_x
    INTEGER(kind=C_INT), INTENT(in), VALUE :: pos_y
    INTEGER(kind=C_INT), INTENT(in), VALUE :: width
    INTEGER(kind=C_INT), INTENT(in), VALUE :: height
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE draw_rectangle_lines

! void DrawRectangleLinesEx(Rectangle rec, float lineThick, Color color)
  SUBROUTINE draw_rectangle_lines_ex(rec, line_thick, color) BIND(c, &
                                                  name='DrawRectangleLinesEx')
    IMPORT :: C_FLOAT, color_type, rectangle_type
    IMPLICIT NONE
    TYPE(rectangle_type), INTENT(in), VALUE :: rec
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: line_thick
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE draw_rectangle_lines_ex

! void DrawRectanglePro(Rectangle rec, Vector2 origin, float rotation, Color color)
  SUBROUTINE draw_rectangle_pro(rec, origin, rotation, color) BIND(c, &
                                                      name='DrawRectanglePro')
    IMPORT :: C_FLOAT, color_type, rectangle_type, vector2_type
    IMPLICIT NONE
    TYPE(rectangle_type), INTENT(in), VALUE :: rec
    TYPE(vector2_type), INTENT(in), VALUE :: origin
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: rotation
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE draw_rectangle_pro

! void DrawRectangleRec(Rectangle rec, Color color)
  SUBROUTINE draw_rectangle_rec(rec, color) BIND(c, name='DrawRectangleRec')
    IMPORT :: color_type, rectangle_type
    IMPLICIT NONE
    TYPE(rectangle_type), INTENT(in), VALUE :: rec
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE draw_rectangle_rec

! void DrawRectangleRounded(Rectangle rec, float roundness, int segments, Color color)
  SUBROUTINE draw_rectangle_rounded(rec, roundness, segments, color) &
    BIND(c, name='DrawRectangleRounded')
    IMPORT :: C_FLOAT, C_INT, color_type, rectangle_type
    IMPLICIT NONE
    TYPE(rectangle_type), INTENT(in), VALUE :: rec
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: roundness
    INTEGER(kind=C_INT), INTENT(in), VALUE :: segments
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE draw_rectangle_rounded

! void DrawRectangleRoundedLines(Rectangle rec, float roundness, int segments, float lineThick, Color color)
  SUBROUTINE draw_rectangle_rounded_lines(rec, roundness, segments, &
                                          line_thick, color) &
    BIND(c, name='DrawRectangleRoundedLines')
    IMPORT :: C_FLOAT, C_INT, color_type, rectangle_type
    IMPLICIT NONE
    TYPE(rectangle_type), INTENT(in), VALUE :: rec
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: roundness
    INTEGER(kind=C_INT), INTENT(in), VALUE :: segments
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: line_thick
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE draw_rectangle_rounded_lines

! void DrawRectangleV(Vector2 position, Vector2 size, Color color)
  SUBROUTINE draw_rectangle_v(position, size, color) BIND(c, &
                                                        name='DrawRectangleV')
    IMPORT :: color_type, vector2_type
    IMPLICIT NONE
    TYPE(vector2_type), INTENT(in), VALUE :: position
    TYPE(vector2_type), INTENT(in), VALUE :: size
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE draw_rectangle_v

! void DrawRing(Vector2 center, float innerRadius, float outerRadius, float startAngle, float endAngle, int segments, Color color)
  SUBROUTINE draw_ring(center, inner_radius, outer_radius, start_angle &
                       , end_angle, segments, color) &
    BIND(c, name='DrawRing')
    IMPORT :: C_FLOAT, C_INT, color_type, vector2_type
    IMPLICIT NONE
    TYPE(vector2_type), INTENT(in), VALUE :: center
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: inner_radius
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: outer_radius
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: start_angle
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: end_angle
    INTEGER(kind=C_INT), INTENT(in), VALUE :: segments
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE draw_ring

! void DrawRingLines(Vector2 center, float innerRadius, float outerRadius, float startAngle, float endAngle, int segments, Color color)
  SUBROUTINE draw_ring_lines(center, inner_radius, outer_radius, &
                             start_angle, end_angle, segments, color) &
    BIND(c, name='DrawRingLines')
    IMPORT :: C_FLOAT, C_INT, color_type, vector2_type
    IMPLICIT NONE
    TYPE(vector2_type), INTENT(in), VALUE :: center
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: inner_radius
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: outer_radius
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: start_angle
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: end_angle
    INTEGER(kind=C_INT), INTENT(in), VALUE :: segments
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE draw_ring_lines

! void DrawSphere(Vector3 centerPos, float radius, Color color)
  SUBROUTINE draw_sphere(center_pos, radius, color) BIND(c, name='DrawSphere')
    IMPORT :: C_FLOAT, color_type, vector3_type
    IMPLICIT NONE
    TYPE(vector3_type), INTENT(in), VALUE :: center_pos
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: radius
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE draw_sphere

! void DrawSphereEx(Vector3 centerPos, float radius, int rings, int slices, Color color)
  SUBROUTINE draw_sphere_ex(center_pos, radius, rings, slices, color) &
    BIND(c, name='DrawSphereEx')
    IMPORT :: C_FLOAT, C_INT, color_type, vector3_type
    IMPLICIT NONE
    TYPE(vector3_type), INTENT(in), VALUE :: center_pos
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: radius
    INTEGER(kind=C_INT), INTENT(in), VALUE :: rings
    INTEGER(kind=C_INT), INTENT(in), VALUE :: slices
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE draw_sphere_ex

! void DrawSphereWires(Vector3 centerPos, float radius, int rings, int slices, Color color)
  SUBROUTINE draw_sphere_wires(center_pos, radius, rings, slices, &
                               color) BIND(c, name='DrawSphereWires')
    IMPORT :: C_FLOAT, C_INT, color_type, vector3_type
    IMPLICIT NONE
    TYPE(vector3_type), INTENT(in), VALUE :: center_pos
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: radius
    INTEGER(kind=C_INT), INTENT(in), VALUE :: rings
    INTEGER(kind=C_INT), INTENT(in), VALUE :: slices
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE draw_sphere_wires

! void DrawSplineBasis(Vector2 *points, int pointCount, float thick, Color color)
  SUBROUTINE draw_spline_basis(points, point_count, thick, color) BIND &
    (c, name='DrawSplineBasis')
    IMPORT :: C_FLOAT, C_INT, color_type, vector2_type
    IMPLICIT NONE
    TYPE(vector2_type), INTENT(in) :: points(*)
    INTEGER(kind=C_INT), INTENT(in), VALUE :: point_count
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: thick
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE draw_spline_basis

! void DrawSplineBezierCubic(Vector2 *points, int pointCount, float thick, Color color)
  SUBROUTINE draw_spline_bezier_cubic(points, point_count, thick, &
                                  color) BIND(c, name='DrawSplineBezierCubic')
    IMPORT :: C_FLOAT, C_INT, color_type, vector2_type
    IMPLICIT NONE
    TYPE(vector2_type), INTENT(in) :: points(*)
    INTEGER(kind=C_INT), INTENT(in), VALUE :: point_count
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: thick
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE draw_spline_bezier_cubic

! void DrawSplineBezierQuadratic(Vector2 *points, int pointCount, float thick, Color color)
  SUBROUTINE draw_spline_bezier_quadratic(points, point_count, thick, &
                              color) BIND(c, name='DrawSplineBezierQuadratic')
    IMPORT :: C_FLOAT, C_INT, color_type, vector2_type
    IMPLICIT NONE
    TYPE(vector2_type), INTENT(in) :: points(*)
    INTEGER(kind=C_INT), INTENT(in), VALUE :: point_count
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: thick
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE draw_spline_bezier_quadratic

! void DrawSplineCatmullRom(Vector2 *points, int pointCount, float thick, Color color)
  SUBROUTINE draw_spline_catmull_rom(points, point_count, thick, color &
                                     ) BIND(c, name='DrawSplineCatmullRom')
    IMPORT :: C_FLOAT, C_INT, color_type, vector2_type
    IMPLICIT NONE
    TYPE(vector2_type), INTENT(in) :: points(*)
    INTEGER(kind=C_INT), INTENT(in), VALUE :: point_count
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: thick
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE draw_spline_catmull_rom

! void DrawSplineLinear(Vector2 *points, int pointCount, float thick, Color color)
  SUBROUTINE draw_spline_linear(points, point_count, thick, color) &
    BIND(c, name='DrawSplineLinear')
    IMPORT :: C_FLOAT, C_INT, color_type, vector2_type
    IMPLICIT NONE
    TYPE(vector2_type), INTENT(in) :: points(*)
    INTEGER(kind=C_INT), INTENT(in), VALUE :: point_count
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: thick
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE draw_spline_linear

! void DrawSplineSegmentBasis(Vector2 p1, Vector2 p2, Vector2 p3, Vector2 p4, float thick, Color color)
  SUBROUTINE draw_spline_segment_basis(p1, p2, p3, p4, thick, color) &
    BIND(c, name='DrawSplineSegmentBasis')
    IMPORT :: C_FLOAT, color_type, vector2_type
    IMPLICIT NONE
    TYPE(vector2_type), INTENT(in), VALUE :: p1
    TYPE(vector2_type), INTENT(in), VALUE :: p2
    TYPE(vector2_type), INTENT(in), VALUE :: p3
    TYPE(vector2_type), INTENT(in), VALUE :: p4
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: thick
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE draw_spline_segment_basis

! void DrawSplineSegmentBezierCubic(Vector2 p1, Vector2 c2, Vector2 c3, Vector2 p4, float thick, Color color)
  SUBROUTINE draw_spline_segment_bezier_cubic(p1, c2, c3, p4, thick, &
                           color) BIND(c, name='DrawSplineSegmentBezierCubic')
    IMPORT :: C_FLOAT, color_type, vector2_type
    IMPLICIT NONE
    TYPE(vector2_type), INTENT(in), VALUE :: p1
    TYPE(vector2_type), INTENT(in), VALUE :: c2
    TYPE(vector2_type), INTENT(in), VALUE :: c3
    TYPE(vector2_type), INTENT(in), VALUE :: p4
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: thick
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE draw_spline_segment_bezier_cubic

! void DrawSplineSegmentBezierQuadratic(Vector2 p1, Vector2 c2, Vector2 p3, float thick, Color color)
  SUBROUTINE draw_spline_segment_bezier_quadratic(p1, c2, p3, thick, &
                       color) BIND(c, name='DrawSplineSegmentBezierQuadratic')
    IMPORT :: C_FLOAT, color_type, vector2_type
    IMPLICIT NONE
    TYPE(vector2_type), INTENT(in), VALUE :: p1
    TYPE(vector2_type), INTENT(in), VALUE :: c2
    TYPE(vector2_type), INTENT(in), VALUE :: p3
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: thick
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE draw_spline_segment_bezier_quadratic

! void DrawSplineSegmentCatmullRom(Vector2 p1, Vector2 p2, Vector2 p3, Vector2 p4, float thick, Color color)
  SUBROUTINE draw_spline_segment_catmull_rom(p1, p2, p3, p4, thick, &
                            color) BIND(c, name='DrawSplineSegmentCatmullRom')
    IMPORT :: C_FLOAT, color_type, vector2_type
    IMPLICIT NONE
    TYPE(vector2_type), INTENT(in), VALUE :: p1
    TYPE(vector2_type), INTENT(in), VALUE :: p2
    TYPE(vector2_type), INTENT(in), VALUE :: p3
    TYPE(vector2_type), INTENT(in), VALUE :: p4
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: thick
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE draw_spline_segment_catmull_rom

! void DrawSplineSegmentLinear(Vector2 p1, Vector2 p2, float thick, Color color)
  SUBROUTINE draw_spline_segment_linear(p1, p2, thick, color) BIND(c, &
                                               name='DrawSplineSegmentLinear')
    IMPORT :: C_FLOAT, color_type, vector2_type
    IMPLICIT NONE
    TYPE(vector2_type), INTENT(in), VALUE :: p1
    TYPE(vector2_type), INTENT(in), VALUE :: p2
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: thick
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE draw_spline_segment_linear

! void DrawTriangleStrip3D(Vector3 *points, int pointCount, Color color)
  SUBROUTINE draw_triangle_strip3d(points, point_count, color) BIND(c &
                                                 , name='DrawTriangleStrip3D')
    IMPORT :: C_INT, color_type, vector3_type
    IMPLICIT NONE
    TYPE(vector3_type), INTENT(in) :: points(*)
    INTEGER(kind=C_INT), INTENT(in), VALUE :: point_count
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE draw_triangle_strip3d

! void DrawText(const char *text, int posX, int posY, int fontSize, Color color)
  SUBROUTINE draw_text(text, pos_x, pos_y, font_size, color) BIND(c, &
                                                              name='DrawText')
    IMPORT :: C_CHAR, C_INT, color_type
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: text
    INTEGER(kind=C_INT), INTENT(in), VALUE :: pos_x
    INTEGER(kind=C_INT), INTENT(in), VALUE :: pos_y
    INTEGER(kind=C_INT), INTENT(in), VALUE :: font_size
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE draw_text

! void DrawTextCodepoint(Font font, int codepoint, Vector2 position, float fontSize, Color tint)
  SUBROUTINE draw_text_codepoint(font, codepoint, position, font_size &
                                 , tint) BIND(c, name='DrawTextCodepoint')
    IMPORT :: C_FLOAT, C_INT, color_type, font_type, vector2_type
    IMPLICIT NONE
    TYPE(font_type), INTENT(in), VALUE :: font
    INTEGER(kind=C_INT), INTENT(in), VALUE :: codepoint
    TYPE(vector2_type), INTENT(in), VALUE :: position
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: font_size
    TYPE(color_type), INTENT(in), VALUE :: tint
  END SUBROUTINE draw_text_codepoint

! void DrawTextCodepoints(Font font, const int *codepoints, int codepointCount, Vector2 position, float fontSize, float spacing, Color tint)
  SUBROUTINE draw_text_codepoints(font, codepoints, codepointCount, &
                                  position, font_size, spacing, tint) &
    BIND(c, name='DrawTextCodepoints')
    IMPORT :: C_FLOAT, C_INT, color_type, font_type, vector2_type
    IMPLICIT NONE
    TYPE(font_type), INTENT(in), VALUE :: font
    INTEGER(kind=C_INT), INTENT(inout) :: codepoints(*)
    INTEGER(kind=C_INT), INTENT(in), VALUE :: codepointCount
    TYPE(vector2_type), INTENT(in), VALUE :: position
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: font_size
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: spacing
    TYPE(color_type), INTENT(in), VALUE :: tint
  END SUBROUTINE draw_text_codepoints

! void DrawTextEx(Font font, const char *text, Vector2 position, float fontSize, float spacing, Color tint)
  SUBROUTINE draw_text_ex(font, text, position, font_size, spacing, &
                          tint) BIND(c, name='DrawTextEx')
    IMPORT :: C_CHAR, C_FLOAT, color_type, font_type, vector2_type
    IMPLICIT NONE
    TYPE(font_type), INTENT(in), VALUE :: font
    CHARACTER(kind=C_CHAR), INTENT(in) :: text
    TYPE(vector2_type), INTENT(in), VALUE :: position
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: font_size
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: spacing
    TYPE(color_type), INTENT(in), VALUE :: tint
  END SUBROUTINE draw_text_ex

! void DrawTextPro(Font font, const char *text, Vector2 position, Vector2 origin, float rotation, float fontSize, float spacing, Color tint)
  SUBROUTINE draw_text_pro(font, text, position, origin, rotation, &
                           font_size, spacing, tint) &
    BIND(c, name='DrawTextPro')
    IMPORT :: C_CHAR, C_FLOAT, color_type, font_type, vector2_type
    IMPLICIT NONE
    TYPE(font_type), INTENT(in), VALUE :: font
    CHARACTER(kind=C_CHAR), INTENT(in) :: text
    TYPE(vector2_type), INTENT(in), VALUE :: position
    TYPE(vector2_type), INTENT(in), VALUE :: origin
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: rotation
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: font_size
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: spacing
    TYPE(color_type), INTENT(in), VALUE :: tint
  END SUBROUTINE draw_text_pro

! void DrawTexture(Texture2D texture, int posX, int posY, Color tint)
  SUBROUTINE draw_texture(texture, pos_x, pos_y, tint) BIND(c, name='&
&            DrawTexture')
    IMPORT :: C_INT, color_type, texture2d_type
    IMPLICIT NONE
    TYPE(texture2d_type), INTENT(in), VALUE :: texture
    INTEGER(kind=C_INT), INTENT(in), VALUE :: pos_x
    INTEGER(kind=C_INT), INTENT(in), VALUE :: pos_y
    TYPE(color_type), INTENT(in), VALUE :: tint
  END SUBROUTINE draw_texture

! void DrawTextureEx(Texture2D texture, Vector2 position, float rotation, float scale, Color tint)
  SUBROUTINE draw_texture_ex(texture, position, rotation, scale, tint &
                             ) BIND(c, name='DrawTextureEx')
    IMPORT :: C_FLOAT, color_type, texture2d_type, vector2_type
    IMPLICIT NONE
    TYPE(texture2d_type), INTENT(in), VALUE :: texture
    TYPE(vector2_type), INTENT(in), VALUE :: position
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: rotation
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: scale
    TYPE(color_type), INTENT(in), VALUE :: tint
  END SUBROUTINE draw_texture_ex

! void DrawTextureNPatch(Texture2D texture, NPatchInfo nPatchInfo, Rectangle dest, Vector2 origin, float rotation, Color tint)
  SUBROUTINE draw_texture_npatch(texture, npatch_info, dest, origin, &
                                 rotation, tint) &
    BIND(c, name='DrawTextureNPatch')
            import :: c_float, color_type, npatch_info_type, rectangle_type, texture2d_type, vector2_type
    IMPLICIT NONE
    TYPE(texture2d_type), INTENT(in), VALUE :: texture
    TYPE(npatch_info_type), INTENT(in), VALUE :: npatch_info
    TYPE(rectangle_type), INTENT(in), VALUE :: dest
    TYPE(vector2_type), INTENT(in), VALUE :: origin
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: rotation
    TYPE(color_type), INTENT(in), VALUE :: tint
  END SUBROUTINE draw_texture_npatch

! void DrawTexturePro(Texture2D texture, Rectangle source, Rectangle dest, Vector2 origin, float rotation, Color tint)
  SUBROUTINE draw_texture_pro(texture, source, dest, origin, rotation &
                              , tint) BIND(c, name='DrawTexturePro')
   IMPORT :: C_FLOAT, color_type, rectangle_type, texture2d_type, vector2_type
    IMPLICIT NONE
    TYPE(texture2d_type), INTENT(in), VALUE :: texture
    TYPE(rectangle_type), INTENT(in), VALUE :: source
    TYPE(rectangle_type), INTENT(in), VALUE :: dest
    TYPE(vector2_type), INTENT(in), VALUE :: origin
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: rotation
    TYPE(color_type), INTENT(in), VALUE :: tint
  END SUBROUTINE draw_texture_pro

! void DrawTextureRec(Texture2D texture, Rectangle source, Vector2 position, Color tint)
  SUBROUTINE draw_texture_rec(texture, source, position, tint) BIND(c &
                                                      , name='DrawTextureRec')
    IMPORT :: color_type, rectangle_type, texture2d_type, vector2_type
    IMPLICIT NONE
    TYPE(texture2d_type), INTENT(in), VALUE :: texture
    TYPE(rectangle_type), INTENT(in), VALUE :: source
    TYPE(vector2_type), INTENT(in), VALUE :: position
    TYPE(color_type), INTENT(in), VALUE :: tint
  END SUBROUTINE draw_texture_rec

! void DrawTextureV(Texture2D texture, Vector2 position, Color tint)
  SUBROUTINE draw_texture_v(texture, position, tint) BIND(c, &
                                                          name='DrawTextureV')
    IMPORT :: color_type, texture2d_type, vector2_type
    IMPLICIT NONE
    TYPE(texture2d_type), INTENT(in), VALUE :: texture
    TYPE(vector2_type), INTENT(in), VALUE :: position
    TYPE(color_type), INTENT(in), VALUE :: tint
  END SUBROUTINE draw_texture_v

! void DrawTriangle(Vector2 v1, Vector2 v2, Vector2 v3, Color color)
  SUBROUTINE draw_triangle(v1, v2, v3, color) BIND(c, name='DrawTriangle')
    IMPORT :: color_type, vector2_type
    IMPLICIT NONE
    TYPE(vector2_type), INTENT(in), VALUE :: v1
    TYPE(vector2_type), INTENT(in), VALUE :: v2
    TYPE(vector2_type), INTENT(in), VALUE :: v3
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE draw_triangle

! void DrawTriangle3D(Vector3 v1, Vector3 v2, Vector3 v3, Color color)
  SUBROUTINE draw_triangle3d(v1, v2, v3, color) BIND(c, name='DrawTriangle3D')
    IMPORT :: color_type, vector3_type
    IMPLICIT NONE
    TYPE(vector3_type), INTENT(in), VALUE :: v1
    TYPE(vector3_type), INTENT(in), VALUE :: v2
    TYPE(vector3_type), INTENT(in), VALUE :: v3
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE draw_triangle3d

! void DrawTriangleFan(Vector2 *points, int pointCount, Color color)
  SUBROUTINE draw_triangle_fan(points, point_count, color) BIND(c, &
                                                       name='DrawTriangleFan')
    IMPORT :: C_INT, color_type, vector2_type
    IMPLICIT NONE
    TYPE(vector2_type), INTENT(in) :: points(*)
    INTEGER(kind=C_INT), INTENT(in), VALUE :: point_count
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE draw_triangle_fan

! void DrawTriangleLines(Vector2 v1, Vector2 v2, Vector2 v3, Color color)
  SUBROUTINE draw_triangle_lines(v1, v2, v3, color) BIND(c, &
                                                     name='DrawTriangleLines')
    IMPORT :: color_type, vector2_type
    IMPLICIT NONE
    TYPE(vector2_type), INTENT(in), VALUE :: v1
    TYPE(vector2_type), INTENT(in), VALUE :: v2
    TYPE(vector2_type), INTENT(in), VALUE :: v3
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE draw_triangle_lines

! void DrawTriangleStrip(Vector2 *points, int pointCount, Color color)
  SUBROUTINE draw_triangle_strip(points, point_count, color) BIND(c, &
                                                     name='DrawTriangleStrip')
    IMPORT :: C_INT, color_type, vector2_type
    IMPLICIT NONE
    TYPE(vector2_type), INTENT(in) :: points(*)
    INTEGER(kind=C_INT), INTENT(in), VALUE :: point_count
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE draw_triangle_strip

END INTERFACE

END MODULE RaylibDrawMethods
