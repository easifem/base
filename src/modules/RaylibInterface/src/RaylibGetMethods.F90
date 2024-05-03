! raylib.f90
!
! A collection of auto-generated Fortran 2018 interface bindings to
! raylib 5.1.
!
! Author:  Philipp Engel
! Licence: ISC

MODULE RaylibGetMethods
USE, INTRINSIC :: ISO_C_BINDING
USE RaylibTypes
USE RaylibEnums
IMPLICIT NONE
PRIVATE

PUBLIC :: get_application_directory
PUBLIC :: get_camera_matrix
PUBLIC :: get_camera_matrix2d
PUBLIC :: get_char_pressed
PUBLIC :: get_clipboard_text
PUBLIC :: get_codepoint
PUBLIC :: get_codepoint_count
PUBLIC :: get_codepoint_next
PUBLIC :: get_codepoint_previous
PUBLIC :: get_collision_rec
PUBLIC :: get_color
PUBLIC :: get_current_monitor
PUBLIC :: get_directory_path
PUBLIC :: get_file_extension
PUBLIC :: get_file_length
PUBLIC :: get_file_mod_time
PUBLIC :: get_file_name
PUBLIC :: get_file_name_without_ext
PUBLIC :: get_font_default
PUBLIC :: get_fps
PUBLIC :: get_frame_time
PUBLIC :: get_gamepad_axis_count
PUBLIC :: get_gamepad_axis_movement
PUBLIC :: get_gamepad_button_pressed
PUBLIC :: get_gamepad_name
PUBLIC :: get_gesture_detected
PUBLIC :: get_gesture_drag_angle
PUBLIC :: get_gesture_hold_duration
PUBLIC :: get_gesture_pinch_angle
PUBLIC :: get_glyph_atlas_rec
PUBLIC :: get_glyph_index
PUBLIC :: get_glyph_info
PUBLIC :: get_image_alpha_border
PUBLIC :: get_image_color
PUBLIC :: get_key_pressed
PUBLIC :: get_master_volume
PUBLIC :: get_mesh_bounding_box
PUBLIC :: get_model_bounding_box
PUBLIC :: get_monitor_count
PUBLIC :: get_monitor_height
PUBLIC :: get_monitor_name
PUBLIC :: get_monitor_physical_height
PUBLIC :: get_monitor_physical_width
PUBLIC :: get_monitor_refresh_rate
PUBLIC :: get_monitor_width
PUBLIC :: get_mouse_delta
PUBLIC :: get_mouse_position
PUBLIC :: get_mouse_ray
PUBLIC :: get_mouse_wheel_move
PUBLIC :: get_mouse_x
PUBLIC :: get_mouse_y
PUBLIC :: get_music_time_length
PUBLIC :: get_music_time_played
PUBLIC :: get_pixel_color
PUBLIC :: get_pixel_data_size
PUBLIC :: get_prev_directory_path
PUBLIC :: get_random_value
PUBLIC :: get_ray_collision_box
PUBLIC :: get_ray_collision_mesh
PUBLIC :: get_ray_collision_quad
PUBLIC :: get_ray_collision_sphere
PUBLIC :: get_ray_collision_triangle
PUBLIC :: get_render_height
PUBLIC :: get_render_width
PUBLIC :: get_screen_height
PUBLIC :: get_screen_to_world2d
PUBLIC :: get_screen_width
PUBLIC :: get_shader_location
PUBLIC :: get_shader_location_attrib
PUBLIC :: get_spline_point_basis
PUBLIC :: get_spline_point_bezier_cubic
PUBLIC :: get_spline_point_bezier_quad
PUBLIC :: get_spline_point_catmull_rom
PUBLIC :: get_spline_point_linear
PUBLIC :: get_time
PUBLIC :: get_touch_point_count
PUBLIC :: get_touch_point_id
PUBLIC :: get_touch_x
PUBLIC :: get_touch_y
PUBLIC :: get_window_handle
PUBLIC :: get_working_directory
PUBLIC :: get_world_to_screen2d

INTERFACE

  FUNCTION get_application_directory() BIND(c, name='GetApplicationDirectory')
    IMPORT :: C_PTR
    IMPLICIT NONE
    TYPE(C_PTR) :: get_application_directory
  END FUNCTION get_application_directory

  ! Matrix GetCameraMatrix(Camera camera)
  FUNCTION get_camera_matrix(camera) BIND(c, name='GetCameraMatrix')
    IMPORT :: camera3d_type, matrix_type
    IMPLICIT NONE
    TYPE(camera3d_type), INTENT(in), VALUE :: camera
    TYPE(matrix_type) :: get_camera_matrix
  END FUNCTION get_camera_matrix

  ! Matrix GetCameraMatrix2D(Camera2D camera)
  FUNCTION get_camera_matrix2d(camera) BIND(c, name='GetCameraMatrix2D')
    IMPORT :: camera2d_type, matrix_type
    IMPLICIT NONE
    TYPE(camera2d_type), INTENT(in), VALUE :: camera
    TYPE(matrix_type) :: get_camera_matrix2d
  END FUNCTION get_camera_matrix2d

  ! int GetCharPressed(void)
  FUNCTION get_char_pressed() BIND(c, name='GetCharPressed')
    IMPORT :: C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT) :: get_char_pressed
  END FUNCTION get_char_pressed

  ! const char *GetClipboardText(void)
  FUNCTION get_clipboard_text() BIND(c, name='GetClipboardText')
    IMPORT :: C_PTR
    IMPLICIT NONE
    TYPE(C_PTR) :: get_clipboard_text
  END FUNCTION get_clipboard_text

  ! int GetCodepoint(const char *text, int *codepointSize)
  FUNCTION get_codepoint(text, codepoint_size) BIND(c, name='GetCodepoint')
    IMPORT :: C_CHAR, C_INT
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: text
    INTEGER(kind=C_INT), INTENT(out) :: codepoint_size
    INTEGER(kind=C_INT) :: get_codepoint
  END FUNCTION get_codepoint

  ! int GetCodepointCount(const char *text)
  FUNCTION get_codepoint_count(text) BIND(c, name='GetCodepointCount')
    IMPORT :: C_CHAR, C_INT
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: text
    INTEGER(kind=C_INT) :: get_codepoint_count
  END FUNCTION get_codepoint_count

  ! int GetCodepointNext(const char *text, int *codepointSize)
  FUNCTION get_codepoint_next(text, codepoint_size) BIND(c, name= &
                                                         'GetCodepointNext')
    IMPORT :: C_CHAR, C_INT
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: text
    INTEGER(kind=C_INT), INTENT(out) :: codepoint_size
    INTEGER(kind=C_INT) :: get_codepoint_next
  END FUNCTION get_codepoint_next

  ! int GetCodepointPrevious(const char *text, int *codepointSize)
  FUNCTION get_codepoint_previous(text, codepoint_size) &
    BIND(c, name='GetCodepointPrevious')
    IMPORT :: C_CHAR, C_INT
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: text
    INTEGER(kind=C_INT), INTENT(out) :: codepoint_size
    INTEGER(kind=C_INT) :: get_codepoint_previous
  END FUNCTION get_codepoint_previous

  ! Rectangle GetCollisionRec(Rectangle rec1, Rectangle rec2)
  FUNCTION get_collision_rec(rec1, rec2) BIND(c, name='GetCollisionRec')
    IMPORT :: rectangle_type
    IMPLICIT NONE
    TYPE(rectangle_type), INTENT(in), VALUE :: rec1
    TYPE(rectangle_type), INTENT(in), VALUE :: rec2
    TYPE(rectangle_type) :: get_collision_rec
  END FUNCTION get_collision_rec

  ! Color GetColor(unsigned int hexValue)
  FUNCTION get_color(hex_value) BIND(c, name='GetColor')
    IMPORT :: c_unsigned_int, color_type
    IMPLICIT NONE
    INTEGER(kind=c_unsigned_int), INTENT(in), VALUE :: hex_value
    TYPE(color_type) :: get_color
  END FUNCTION get_color

  ! int GetCurrentMonitor(void)
  FUNCTION get_current_monitor() BIND(c, name='GetCurrentMonitor')
    IMPORT :: C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT) :: get_current_monitor
  END FUNCTION get_current_monitor

  ! const char *GetDirectoryPath(const char *filePath)
  FUNCTION get_directory_path(file_path) BIND(c, name='GetDirectoryPath')
    IMPORT :: C_CHAR, C_PTR
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: file_path
    TYPE(C_PTR) :: get_directory_path
  END FUNCTION get_directory_path

  ! int GetFPS(void)
  FUNCTION get_fps() BIND(c, name='GetFPS')
    IMPORT :: C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT) :: get_fps
  END FUNCTION get_fps

  ! const char *GetFileExtension(const char *fileName)
  FUNCTION get_file_extension(file_name) BIND(c, name='GetFileExtension')
    IMPORT :: C_CHAR, C_PTR
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: file_name
    TYPE(C_PTR) :: get_file_extension
  END FUNCTION get_file_extension

  ! int GetFileLength(const char *fileName)
  FUNCTION get_file_length(file_name) BIND(c, name='GetFileLength')
    IMPORT :: C_CHAR, C_INT
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: file_name
    INTEGER(kind=C_INT) :: get_file_length
  END FUNCTION get_file_length

  ! long GetFileModTime(const char *fileName)
  FUNCTION get_file_mod_time(file_name) BIND(c, name='GetFileModTime')
    IMPORT :: C_CHAR, C_LONG
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: file_name
    INTEGER(kind=C_LONG) :: get_file_mod_time
  END FUNCTION get_file_mod_time

  ! const char *GetFileName(const char *filePath)
  FUNCTION get_file_name(file_path) BIND(c, name='GetFileName')
    IMPORT :: C_CHAR, C_PTR
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: file_path
    TYPE(C_PTR) :: get_file_name
  END FUNCTION get_file_name

  ! const char *GetFileNameWithoutExt(const char *filePath)
  FUNCTION get_file_name_without_ext(file_path) BIND(c, name= &
                                                     'GetFileNameWithoutExt')
    IMPORT :: C_CHAR, C_PTR
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: file_path
    TYPE(C_PTR) :: get_file_name_without_ext
  END FUNCTION get_file_name_without_ext

  ! Font GetFontDefault(void)
  FUNCTION get_font_default() BIND(c, name='GetFontDefault')
    IMPORT :: font_type
    IMPLICIT NONE
    TYPE(font_type) :: get_font_default
  END FUNCTION get_font_default

  ! float GetFrameTime(void)
  FUNCTION get_frame_time() BIND(c, name='GetFrameTime')
    IMPORT :: C_FLOAT
    IMPLICIT NONE
    REAL(kind=C_FLOAT) :: get_frame_time
  END FUNCTION get_frame_time

  ! int GetGamepadAxisCount(int gamepad)
  FUNCTION get_gamepad_axis_count(gamepad) BIND(c, name='GetGamepadAxisCount')
    IMPORT :: C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: gamepad
    INTEGER(kind=C_INT) :: get_gamepad_axis_count
  END FUNCTION get_gamepad_axis_count

  ! float GetGamepadAxisMovement(int gamepad, int axis)
  FUNCTION get_gamepad_axis_movement(gamepad, axis) BIND(c, name= &
                                                     'GetGamepadAxisMovement')
    IMPORT :: C_FLOAT, C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: gamepad
    INTEGER(kind=C_INT), INTENT(in), VALUE :: axis
    REAL(kind=C_FLOAT) :: get_gamepad_axis_movement
  END FUNCTION get_gamepad_axis_movement

  ! int GetGamepadButtonPressed(void)
 FUNCTION get_gamepad_button_pressed() BIND(c, name='GetGamepadButtonPressed')
    IMPORT :: C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT) :: get_gamepad_button_pressed
  END FUNCTION get_gamepad_button_pressed

  ! const char *GetGamepadName(int gamepad)
  FUNCTION get_gamepad_name(gamepad) BIND(c, name='GetGamepadName')
    IMPORT :: C_CHAR, C_INT, C_PTR
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: gamepad
    TYPE(C_PTR) :: get_gamepad_name
  END FUNCTION get_gamepad_name

  ! int GetGestureDetected(void)
  FUNCTION get_gesture_detected() BIND(c, name='GetGestureDetected')
    IMPORT :: C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT) :: get_gesture_detected
  END FUNCTION get_gesture_detected

  ! float GetGestureDragAngle(void)
  FUNCTION get_gesture_drag_angle() BIND(c, name='GetGestureDragAngle')
    IMPORT :: C_FLOAT
    IMPLICIT NONE
    REAL(kind=C_FLOAT) :: get_gesture_drag_angle
  END FUNCTION get_gesture_drag_angle

  ! float GetGestureHoldDuration(void)
  FUNCTION get_gesture_hold_duration() BIND(c, name='GetGestureHoldDuration')
    IMPORT :: C_FLOAT
    IMPLICIT NONE
    REAL(kind=C_FLOAT) :: get_gesture_hold_duration
  END FUNCTION get_gesture_hold_duration

  ! float GetGesturePinchAngle(void)
  FUNCTION get_gesture_pinch_angle() BIND(c, name='GetGesturePinchAngle')
    IMPORT :: C_FLOAT
    IMPLICIT NONE
    REAL(kind=C_FLOAT) :: get_gesture_pinch_angle
  END FUNCTION get_gesture_pinch_angle

  ! Rectangle GetGlyphAtlasRec(Font font, int codepoint)
FUNCTION get_glyph_atlas_rec(font, codepoint) BIND(c, name='GetGlyphAtlasRec')
    IMPORT :: C_INT, font_type, rectangle_type
    IMPLICIT NONE
    TYPE(font_type), INTENT(in), VALUE :: font
    INTEGER(kind=C_INT), INTENT(in), VALUE :: codepoint
    TYPE(rectangle_type) :: get_glyph_atlas_rec
  END FUNCTION get_glyph_atlas_rec

  ! int GetGlyphIndex(Font font, int codepoint)
  FUNCTION get_glyph_index(font, codepoint) BIND(c, name='GetGlyphIndex')
    IMPORT :: C_INT, font_type
    IMPLICIT NONE
    TYPE(font_type), INTENT(in), VALUE :: font
    INTEGER(kind=C_INT), INTENT(in), VALUE :: codepoint
    INTEGER(kind=C_INT) :: get_glyph_index
  END FUNCTION get_glyph_index

  ! GlyphInfo GetGlyphInfo(Font font, int codepoint)
  FUNCTION get_glyph_info(font, codepoint) BIND(c, name='GetGlyphInfo')
    IMPORT :: C_INT, font_type, glyph_info_type
    IMPLICIT NONE
    TYPE(font_type), INTENT(in), VALUE :: font
    INTEGER(kind=C_INT), INTENT(in), VALUE :: codepoint
    TYPE(glyph_info_type) :: get_glyph_info
  END FUNCTION get_glyph_info

  ! float GetMasterVolume(void)
  FUNCTION get_master_volume() BIND(c, name='GetMasterVolume')
    IMPORT :: C_FLOAT
    IMPLICIT NONE
    REAL(kind=C_FLOAT) :: get_master_volume
  END FUNCTION get_master_volume

  ! Rectangle GetImageAlphaBorder(Image image, float threshold)
  FUNCTION get_image_alpha_border(image, threshold) BIND(c, name= &
                                                        'GetImageAlphaBorder')
    IMPORT :: C_FLOAT, image_type, rectangle_type
    IMPLICIT NONE
    TYPE(image_type), INTENT(in), VALUE :: image
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: threshold
    TYPE(rectangle_type) :: get_image_alpha_border
  END FUNCTION get_image_alpha_border

  ! Vector2 GetSplinePointBasis(Vector2 p1, Vector2 p2, Vector2 p3, Vector2 p4, float t)
  FUNCTION get_spline_point_basis(p1, p2, p3, p4, t) BIND(c, &
                                                   name='GetSplinePointBasis')
    IMPORT :: C_FLOAT, vector2_type
    IMPLICIT NONE
    TYPE(vector2_type), INTENT(in), VALUE :: p1
    TYPE(vector2_type), INTENT(in), VALUE :: p2
    TYPE(vector2_type), INTENT(in), VALUE :: p3
    TYPE(vector2_type), INTENT(in), VALUE :: p4
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: t
    TYPE(vector2_type) :: get_spline_point_basis
  END FUNCTION get_spline_point_basis

  ! Vector2 GetSplinePointBezierCubic(Vector2 p1, Vector2 c2, Vector2 c3, Vector2 p4, float t)
  FUNCTION get_spline_point_bezier_cubic(p1, c2, c3, p4, t) BIND(c, &
                                             name='GetSplinePointBezierCubic')
    IMPORT :: C_FLOAT, vector2_type
    IMPLICIT NONE
    TYPE(vector2_type), INTENT(in), VALUE :: p1
    TYPE(vector2_type), INTENT(in), VALUE :: c2
    TYPE(vector2_type), INTENT(in), VALUE :: c3
    TYPE(vector2_type), INTENT(in), VALUE :: p4
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: t
    TYPE(vector2_type) :: get_spline_point_bezier_cubic
  END FUNCTION get_spline_point_bezier_cubic

  ! Vector2 GetSplinePointBezierQuad(Vector2 p1, Vector2 c2, Vector2 p3, float t)
  FUNCTION get_spline_point_bezier_quad(p1, c2, p3, t) BIND(c, name= &
                                                   'GetSplinePointBezierQuad')
    IMPORT :: C_FLOAT, vector2_type
    IMPLICIT NONE
    TYPE(vector2_type), INTENT(in), VALUE :: p1
    TYPE(vector2_type), INTENT(in), VALUE :: c2
    TYPE(vector2_type), INTENT(in), VALUE :: p3
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: t
    TYPE(vector2_type) :: get_spline_point_bezier_quad
  END FUNCTION get_spline_point_bezier_quad

  ! Vector2 GetSplinePointCatmullRom(Vector2 p1, Vector2 p2, Vector2 p3, Vector2 p4, float t)
  FUNCTION get_spline_point_catmull_rom(p1, p2, p3, p4, t) BIND(c, &
                                              name='GetSplinePointCatmullRom')
    IMPORT :: C_FLOAT, vector2_type
    IMPLICIT NONE
    TYPE(vector2_type), INTENT(in), VALUE :: p1
    TYPE(vector2_type), INTENT(in), VALUE :: p2
    TYPE(vector2_type), INTENT(in), VALUE :: p3
    TYPE(vector2_type), INTENT(in), VALUE :: p4
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: t
    TYPE(vector2_type) :: get_spline_point_catmull_rom
  END FUNCTION get_spline_point_catmull_rom

  ! Vector2 GetSplinePointLinear(Vector2 startPos, Vector2 endPos, float t)
  FUNCTION get_spline_point_linear(start_pos, end_pos, t) BIND(c, name &
                                                      ='GetSplinePointLinear')
    IMPORT :: C_FLOAT, vector2_type
    IMPLICIT NONE
    TYPE(vector2_type), INTENT(in), VALUE :: start_pos
    TYPE(vector2_type), INTENT(in), VALUE :: end_pos
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: t
    TYPE(vector2_type) :: get_spline_point_linear
  END FUNCTION get_spline_point_linear

  ! Color GetImageColor(Image image, int x, int y)
  FUNCTION get_image_color(image, x, y) BIND(c, name='GetImageColor')
    IMPORT :: C_INT, color_type, image_type
    IMPLICIT NONE
    TYPE(image_type), INTENT(in), VALUE :: image
    INTEGER(kind=C_INT), INTENT(in), VALUE :: x
    INTEGER(kind=C_INT), INTENT(in), VALUE :: y
    TYPE(color_type) :: get_image_color
  END FUNCTION get_image_color

  ! int GetKeyPressed(void)
  FUNCTION get_key_pressed() BIND(c, name='GetKeyPressed')
    IMPORT :: C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT) :: get_key_pressed
  END FUNCTION get_key_pressed

  ! BoundingBox GetMeshBoundingBox(Mesh mesh)
  FUNCTION get_mesh_bounding_box(mesh) BIND(c, name='GetMeshBoundingBox')
    IMPORT :: bounding_box_type, mesh_type
    IMPLICIT NONE
    TYPE(mesh_type), INTENT(in), VALUE :: mesh
    TYPE(bounding_box_type) :: get_mesh_bounding_box
  END FUNCTION get_mesh_bounding_box

  ! BoundingBox GetModelBoundingBox(Model model)
  FUNCTION get_model_bounding_box(model) BIND(c, name='GetModelBoundingBox')
    IMPORT :: bounding_box_type, model_type
    IMPLICIT NONE
    TYPE(model_type), INTENT(in), VALUE :: model
    TYPE(bounding_box_type) :: get_model_bounding_box
  END FUNCTION get_model_bounding_box

  ! int GetMonitorCount(void)
  FUNCTION get_monitor_count() BIND(c, name='GetMonitorCount')
    IMPORT :: C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT) :: get_monitor_count
  END FUNCTION get_monitor_count

  ! int GetMonitorHeight(int monitor)
  FUNCTION get_monitor_height(monitor) BIND(c, name='GetMonitorHeight')
    IMPORT :: C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: monitor
    INTEGER(kind=C_INT) :: get_monitor_height
  END FUNCTION get_monitor_height

  ! const char *GetMonitorName(int monitor)
  FUNCTION get_monitor_name(monitor) BIND(c, name='GetMonitorName')
    IMPORT :: C_INT, C_PTR
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: monitor
    TYPE(C_PTR) :: get_monitor_name
  END FUNCTION get_monitor_name

  ! int GetMonitorPhysicalHeight(int monitor)
  FUNCTION get_monitor_physical_height(monitor) BIND(c, name= &
                                                   'GetMonitorPhysicalHeight')
    IMPORT :: C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: monitor
    INTEGER(kind=C_INT) :: get_monitor_physical_height
  END FUNCTION get_monitor_physical_height

  ! int GetMonitorPhysicalWidth(int monitor)
  FUNCTION get_monitor_physical_width(monitor) BIND(c, name= &
                                                    'GetMonitorPhysicalWidth')
    IMPORT :: C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: monitor
    INTEGER(kind=C_INT) :: get_monitor_physical_width
  END FUNCTION get_monitor_physical_width

  ! int GetMonitorRefreshRate(int monitor)
  FUNCTION get_monitor_refresh_rate(monitor) BIND(c, name= &
                                                  'GetMonitorRefreshRate')
    IMPORT :: C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: monitor
    INTEGER(kind=C_INT) :: get_monitor_refresh_rate
  END FUNCTION get_monitor_refresh_rate

  ! int GetMonitorWidth(int monitor)
  FUNCTION get_monitor_width(monitor) BIND(c, name='GetMonitorWidth')
    IMPORT :: C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: monitor
    INTEGER(kind=C_INT) :: get_monitor_width
  END FUNCTION get_monitor_width

  ! Vector2 GetMouseDelta(void)
  FUNCTION get_mouse_delta() BIND(c, name='GetMouseDelta')
    IMPORT :: vector2_type
    IMPLICIT NONE
    TYPE(vector2_type) :: get_mouse_delta
  END FUNCTION get_mouse_delta

  ! Vector2 GetMousePosition(void)
  FUNCTION get_mouse_position() BIND(c, name='GetMousePosition')
    IMPORT :: vector2_type
    IMPLICIT NONE
    TYPE(vector2_type) :: get_mouse_position
  END FUNCTION get_mouse_position

  ! Ray GetMouseRay(Vector2 mousePosition, Camera camera)
  FUNCTION get_mouse_ray(mouse_position, camera) BIND(c, name='GetMouseRay')
    IMPORT :: camera3d_type, ray_type, vector2_type
    IMPLICIT NONE
    TYPE(vector2_type), INTENT(in), VALUE :: mouse_position
    TYPE(camera3d_type), INTENT(in), VALUE :: camera
    TYPE(ray_type) :: get_mouse_ray
  END FUNCTION get_mouse_ray

  ! float GetMouseWheelMove(void)
  FUNCTION get_mouse_wheel_move() BIND(c, name='GetMouseWheelMove')
    IMPORT :: C_FLOAT
    IMPLICIT NONE
    REAL(kind=C_FLOAT) :: get_mouse_wheel_move
  END FUNCTION get_mouse_wheel_move

  ! int GetMouseX(void)
  FUNCTION get_mouse_x() BIND(c, name='GetMouseX')
    IMPORT :: C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT) :: get_mouse_x
  END FUNCTION get_mouse_x

  ! int GetMouseY(void)
  FUNCTION get_mouse_y() BIND(c, name='GetMouseY')
    IMPORT :: C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT) :: get_mouse_y
  END FUNCTION get_mouse_y

  ! float GetMusicTimeLength(Music music)
  FUNCTION get_music_time_length(music) BIND(c, name='GetMusicTimeLength')
    IMPORT :: C_FLOAT, music_type
    IMPLICIT NONE
    TYPE(music_type), INTENT(in), VALUE :: music
    REAL(kind=C_FLOAT) :: get_music_time_length
  END FUNCTION get_music_time_length

  ! float GetMusicTimePlayed(Music music)
  FUNCTION get_music_time_played(music) BIND(c, name='GetMusicTimePlayed')
    IMPORT :: C_FLOAT, music_type
    IMPLICIT NONE
    TYPE(music_type), INTENT(in), VALUE :: music
    REAL(kind=C_FLOAT) :: get_music_time_played
  END FUNCTION get_music_time_played

  ! Color GetPixelColor(void *srcPtr, int format)
  FUNCTION get_pixel_color(src_ptr, FORMAT) BIND(c, name='GetPixelColor')
    IMPORT :: C_INT, C_PTR, color_type
    IMPLICIT NONE
    TYPE(C_PTR), INTENT(in), VALUE :: src_ptr
    INTEGER(kind=C_INT), INTENT(in), VALUE :: FORMAT
    TYPE(color_type) :: get_pixel_color
  END FUNCTION get_pixel_color

  ! int GetPixelDataSize(int width, int height, int format)
  FUNCTION get_pixel_data_size(width, height, FORMAT) BIND(c, name= &
                                                           'GetPixelDataSize')
    IMPORT :: C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: width
    INTEGER(kind=C_INT), INTENT(in), VALUE :: height
    INTEGER(kind=C_INT), INTENT(in), VALUE :: FORMAT
    INTEGER(kind=C_INT) :: get_pixel_data_size
  END FUNCTION get_pixel_data_size

  ! const char *GetPrevDirectoryPath(const char *dirPath)
  FUNCTION get_prev_directory_path(dir_path) BIND(c, &
                                                  name='GetPrevDirectoryPath')
    IMPORT :: C_CHAR, C_PTR
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: dir_path
    TYPE(C_PTR) :: get_prev_directory_path
  END FUNCTION get_prev_directory_path

  ! int GetRandomValue(int min, int max)
  FUNCTION get_random_value(min, max) BIND(c, name='GetRandomValue')
    IMPORT :: C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: min
    INTEGER(kind=C_INT), INTENT(in), VALUE :: max
    INTEGER(kind=C_INT) :: get_random_value
  END FUNCTION get_random_value

  ! RayCollision GetRayCollisionBox(Ray ray, BoundingBox box)
  FUNCTION get_ray_collision_box(ray, box) BIND(c, name='GetRayCollisionBox')
    IMPORT :: bounding_box_type, ray_collision_type, ray_type
    IMPLICIT NONE
    TYPE(ray_type), INTENT(in), VALUE :: ray
    TYPE(bounding_box_type), INTENT(in), VALUE :: box
    TYPE(ray_collision_type) :: get_ray_collision_box
  END FUNCTION get_ray_collision_box

  ! RayCollision GetRayCollisionMesh(Ray ray, Mesh mesh, Matrix transform)
  FUNCTION get_ray_collision_mesh(ray, mesh, transform) &
    BIND(c, name='GetRayCollisionMesh')
    IMPORT :: matrix_type, mesh_type, ray_collision_type, ray_type
    IMPLICIT NONE
    TYPE(ray_type), INTENT(in), VALUE :: ray
    TYPE(mesh_type), INTENT(in), VALUE :: mesh
    TYPE(matrix_type), INTENT(in), VALUE :: transform
    TYPE(ray_collision_type) :: get_ray_collision_mesh
  END FUNCTION get_ray_collision_mesh

  ! RayCollision GetRayCollisionQuad(Ray ray, Vector3 p1, Vector3 p2, Vector3 p3, Vector3 p4)
  FUNCTION get_ray_collision_quad(ray, p1, p2, p3, p4) &
    BIND(c, name='GetRayCollisionQuad')
    IMPORT :: ray_collision_type, ray_type, vector3_type
    IMPLICIT NONE
    TYPE(ray_type), INTENT(in), VALUE :: ray
    TYPE(vector3_type), INTENT(in), VALUE :: p1
    TYPE(vector3_type), INTENT(in), VALUE :: p2
    TYPE(vector3_type), INTENT(in), VALUE :: p3
    TYPE(vector3_type), INTENT(in), VALUE :: p4
    TYPE(ray_collision_type) :: get_ray_collision_quad
  END FUNCTION get_ray_collision_quad

  ! RayCollision GetRayCollisionSphere(Ray ray, Vector3 center, float radius)
  FUNCTION get_ray_collision_sphere(ray, center, radius) &
    BIND(c, name='GetRayCollisionSphere')
    IMPORT :: C_FLOAT, ray_collision_type, ray_type, vector3_type
    IMPLICIT NONE
    TYPE(ray_type), INTENT(in), VALUE :: ray
    TYPE(vector3_type), INTENT(in), VALUE :: center
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: radius
    TYPE(ray_collision_type) :: get_ray_collision_sphere
  END FUNCTION get_ray_collision_sphere

  ! RayCollision GetRayCollisionTriangle(Ray ray, Vector3 p1, Vector3 p2, Vector3 p3)
  FUNCTION get_ray_collision_triangle(ray, p1, p2, p3) &
    BIND(c, name='GetRayCollisionTriangle')
    IMPORT :: ray_collision_type, ray_type, vector3_type
    IMPLICIT NONE
    TYPE(ray_type), INTENT(in), VALUE :: ray
    TYPE(vector3_type), INTENT(in), VALUE :: p1
    TYPE(vector3_type), INTENT(in), VALUE :: p2
    TYPE(vector3_type), INTENT(in), VALUE :: p3
    TYPE(ray_collision_type) :: get_ray_collision_triangle
  END FUNCTION get_ray_collision_triangle

  ! int GetRenderHeight(void)
  FUNCTION get_render_height() BIND(c, name='GetRenderHeight')
    IMPORT :: C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT) :: get_render_height
  END FUNCTION get_render_height

  ! int GetRenderWidth(void)
  FUNCTION get_render_width() BIND(c, name='GetRenderWidth')
    IMPORT :: C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT) :: get_render_width
  END FUNCTION get_render_width

  ! int GetScreenHeight(void)
  FUNCTION get_screen_height() BIND(c, name='GetScreenHeight')
    IMPORT :: C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT) :: get_screen_height
  END FUNCTION get_screen_height

  ! Vector2 GetScreenToWorld2D(Vector2 position, Camera2D camera)
  FUNCTION get_screen_to_world2d(position, camera) &
    BIND(c, name='GetScreenToWorld2D')
    IMPORT :: camera2d_type, vector2_type
    IMPLICIT NONE
    TYPE(vector2_type), INTENT(in), VALUE :: position
    TYPE(camera2d_type), INTENT(in), VALUE :: camera
    TYPE(vector2_type) :: get_screen_to_world2d
  END FUNCTION get_screen_to_world2d

  ! int GetScreenWidth(void)
  FUNCTION get_screen_width() BIND(c, name='GetScreenWidth')
    IMPORT :: C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT) :: get_screen_width
  END FUNCTION get_screen_width

  ! int GetShaderLocation(Shader shader, const char *uniformName)
  FUNCTION get_shader_location(shader, uniform_name) &
    BIND(c, name='GetShaderLocation')
    IMPORT :: C_CHAR, C_INT, shader_type
    IMPLICIT NONE
    TYPE(shader_type), INTENT(in), VALUE :: shader
    CHARACTER(kind=C_CHAR), INTENT(in) :: uniform_name
    INTEGER(kind=C_INT) :: get_shader_location
  END FUNCTION get_shader_location

  ! int GetShaderLocationAttrib(Shader shader, const char *attribName)
  FUNCTION get_shader_location_attrib(shader, attrib_name) &
    BIND(c, name='GetShaderLocationAttrib')
    IMPORT :: C_CHAR, C_INT, shader_type
    IMPLICIT NONE
    TYPE(shader_type), INTENT(in), VALUE :: shader
    CHARACTER(kind=C_CHAR), INTENT(in) :: attrib_name
    INTEGER(kind=C_INT) :: get_shader_location_attrib
  END FUNCTION get_shader_location_attrib

  ! double GetTime(void)
  FUNCTION get_time() BIND(c, name='GetTime')
    IMPORT :: C_DOUBLE
    IMPLICIT NONE
    REAL(kind=C_DOUBLE) :: get_time
  END FUNCTION get_time

  ! int GetTouchPointCount(void)
  FUNCTION get_touch_point_count() BIND(c, name='GetTouchPointCount')
    IMPORT :: C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT) :: get_touch_point_count
  END FUNCTION get_touch_point_count

  ! int GetTouchPointId(int index)
  FUNCTION get_touch_point_id(index) BIND(c, name='GetTouchPointId')
    IMPORT :: C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: index
    INTEGER(kind=C_INT) :: get_touch_point_id
  END FUNCTION get_touch_point_id

  ! int GetTouchX(void)
  FUNCTION get_touch_x() BIND(c, name='GetTouchX')
    IMPORT :: C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT) :: get_touch_x
  END FUNCTION get_touch_x

  ! int GetTouchY(void)
  FUNCTION get_touch_y() BIND(c, name='GetTouchY')
    IMPORT :: C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT) :: get_touch_y
  END FUNCTION get_touch_y

  ! void *GetWindowHandle(void)
  FUNCTION get_window_handle() BIND(c, name='GetWindowHandle')
    IMPORT :: C_PTR
    IMPLICIT NONE
    TYPE(C_PTR) :: get_window_handle
  END FUNCTION get_window_handle

  ! const char *GetWorkingDirectory(void)
  FUNCTION get_working_directory() BIND(c, name='GetWorkingDirectory')
    IMPORT :: C_PTR
    IMPLICIT NONE
    TYPE(C_PTR) :: get_working_directory
  END FUNCTION get_working_directory

  ! Vector2 GetWorldToScreen2D(Vector2 position, Camera2D camera)
  FUNCTION get_world_to_screen2d(position, camera) &
    BIND(c, name='GetWorldToScreen2D')
    IMPORT :: camera2d_type, vector2_type
    IMPLICIT NONE
    TYPE(vector2_type), INTENT(in), VALUE :: position
    TYPE(camera2d_type), INTENT(in), VALUE :: camera
    TYPE(vector2_type) :: get_world_to_screen2d
  END FUNCTION get_world_to_screen2d

END INTERFACE

END MODULE RaylibGetMethods
