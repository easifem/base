! raylib.f90
!
! A collection of auto-generated Fortran 2018 interface bindings to
! raylib 5.1.
!
! Author:  Philipp Engel
! Licence: ISC

MODULE RaylibMethods
USE, INTRINSIC :: ISO_C_BINDING
USE RaylibTypes
USE RaylibEnums
IMPLICIT NONE
PRIVATE

PUBLIC :: attach_audio_mixed_processor
PUBLIC :: attach_audio_stream_processor
PUBLIC :: begin_blend_mode
PUBLIC :: begin_drawing
PUBLIC :: begin_mode2d
PUBLIC :: begin_mode3d
PUBLIC :: begin_scissor_mode
PUBLIC :: begin_shader_mode
PUBLIC :: begin_texture_mode
PUBLIC :: begin_vr_stereo_mode
PUBLIC :: change_directory
PUBLIC :: check_collision_box_sphere
PUBLIC :: check_collision_boxes
PUBLIC :: check_collision_circle_rec
PUBLIC :: check_collision_circles
PUBLIC :: check_collision_lines
PUBLIC :: check_collision_point_circle
PUBLIC :: check_collision_point_line
PUBLIC :: check_collision_point_poly
PUBLIC :: check_collision_point_rec
PUBLIC :: check_collision_point_triangle
PUBLIC :: check_collision_recs
PUBLIC :: check_collision_spheres
PUBLIC :: clear_background
PUBLIC :: clear_window_state
PUBLIC :: close_audio_device
PUBLIC :: close_window
PUBLIC :: codepoint_to_utf8
PUBLIC :: color_alpha
PUBLIC :: color_alpha_blend
PUBLIC :: color_brightness
PUBLIC :: color_contrast
PUBLIC :: color_from_hsv
PUBLIC :: color_from_normalized
PUBLIC :: color_tint
PUBLIC :: color_to_int
PUBLIC :: compress_data
PUBLIC :: decode_data_base64
PUBLIC :: decompress_data
PUBLIC :: detach_audio_mixed_processor
PUBLIC :: detach_audio_stream_processor
PUBLIC :: directory_exists
PUBLIC :: disable_cursor
PUBLIC :: disable_event_waiting
PUBLIC :: enable_cursor
PUBLIC :: enable_event_waiting
PUBLIC :: encode_data_base64
PUBLIC :: end_blend_mode
PUBLIC :: end_drawing
PUBLIC :: end_mode2d
PUBLIC :: end_mode3d
PUBLIC :: end_scissor_mode
PUBLIC :: end_shader_mode
PUBLIC :: end_texture_mode
PUBLIC :: end_vr_stereo_mode
PUBLIC :: export_data_as_code
PUBLIC :: export_font_as_code
PUBLIC :: export_image
PUBLIC :: export_image_as_code
PUBLIC :: export_image_to_memory
PUBLIC :: export_mesh
PUBLIC :: export_wave
PUBLIC :: export_wave_as_code
PUBLIC :: fade
PUBLIC :: file_exists
PUBLIC :: hide_cursor
PUBLIC :: init_audio_device
PUBLIC :: init_window
PUBLIC :: is_audio_device_ready
PUBLIC :: is_audio_stream_playing
PUBLIC :: is_audio_stream_processed
PUBLIC :: is_audio_stream_ready
PUBLIC :: is_cursor_hidden
PUBLIC :: is_cursor_on_screen
PUBLIC :: is_file_dropped
PUBLIC :: is_file_extension
PUBLIC :: is_font_ready
PUBLIC :: is_gamepad_available
PUBLIC :: is_gamepad_button_down
PUBLIC :: is_gamepad_button_pressed
PUBLIC :: is_gamepad_button_released
PUBLIC :: is_gamepad_button_up
PUBLIC :: is_gesture_detected
PUBLIC :: is_image_ready
PUBLIC :: is_key_down
PUBLIC :: is_key_pressed
PUBLIC :: is_key_pressed_repeat
PUBLIC :: is_key_released
PUBLIC :: is_key_up
PUBLIC :: is_material_ready
PUBLIC :: is_model_animation_valid
PUBLIC :: is_model_ready
PUBLIC :: is_mouse_button_down
PUBLIC :: is_mouse_button_pressed
PUBLIC :: is_mouse_button_released
PUBLIC :: is_mouse_button_up
PUBLIC :: is_music_ready
PUBLIC :: is_music_stream_playing
PUBLIC :: is_path_file
PUBLIC :: is_render_texture_ready
PUBLIC :: is_shader_ready
PUBLIC :: is_sound_playing
PUBLIC :: is_sound_ready
PUBLIC :: is_texture_ready
PUBLIC :: is_wave_ready
PUBLIC :: is_window_focused
PUBLIC :: is_window_fullscreen
PUBLIC :: is_window_hidden
PUBLIC :: is_window_maximized
PUBLIC :: is_window_minimized
PUBLIC :: is_window_ready
PUBLIC :: is_window_resized
PUBLIC :: is_window_state
PUBLIC :: load_audio_stream
PUBLIC :: load_codepoints
PUBLIC :: load_directory_files
PUBLIC :: load_directory_files_ex
PUBLIC :: load_dropped_files
PUBLIC :: load_file_data
PUBLIC :: load_file_text
PUBLIC :: load_font
PUBLIC :: load_font_data
PUBLIC :: load_font_ex
PUBLIC :: load_font_from_image
PUBLIC :: load_font_from_memory
PUBLIC :: load_image
PUBLIC :: load_image_anim
PUBLIC :: load_image_colors
PUBLIC :: load_image_from_memory
PUBLIC :: load_image_from_screen
PUBLIC :: load_image_from_texture
PUBLIC :: load_image_palette
PUBLIC :: load_image_raw
PUBLIC :: load_image_svg
PUBLIC :: load_material_default
PUBLIC :: load_materials
PUBLIC :: load_model
PUBLIC :: load_model_animations
PUBLIC :: load_model_from_mesh
PUBLIC :: load_music_stream
PUBLIC :: load_music_stream_from_memory
PUBLIC :: load_random_sequence
PUBLIC :: load_render_texture
PUBLIC :: load_shader
PUBLIC :: load_shader_from_memory
PUBLIC :: load_sound
PUBLIC :: load_sound_alias
PUBLIC :: load_sound_from_wave
PUBLIC :: load_texture
PUBLIC :: load_texture_cubemap
PUBLIC :: load_texture_from_image
PUBLIC :: load_utf8
PUBLIC :: load_vr_stereo_config
PUBLIC :: load_wave
PUBLIC :: load_wave_from_memory
PUBLIC :: load_wave_samples
PUBLIC :: maximize_window
PUBLIC :: measure_text
PUBLIC :: measure_text_ex
PUBLIC :: mem_alloc
PUBLIC :: mem_free
PUBLIC :: mem_realloc
PUBLIC :: minimize_window
PUBLIC :: open_url
PUBLIC :: pause_audio_stream
PUBLIC :: pause_music_stream
PUBLIC :: pause_sound
PUBLIC :: play_audio_stream
PUBLIC :: play_music_stream
PUBLIC :: play_sound
PUBLIC :: poll_input_events
PUBLIC :: restore_window
PUBLIC :: resume_audio_stream
PUBLIC :: resume_music_stream
PUBLIC :: resume_sound
PUBLIC :: save_file_data
PUBLIC :: save_file_text
PUBLIC :: seek_music_stream
PUBLIC :: show_cursor
PUBLIC :: stop_audio_stream
PUBLIC :: stop_music_stream
PUBLIC :: stop_sound
PUBLIC :: swap_screen_buffer
PUBLIC :: take_screenshot
PUBLIC :: text_append
PUBLIC :: text_copy
PUBLIC :: text_find_index
PUBLIC :: text_insert
PUBLIC :: text_is_equal
PUBLIC :: text_join
PUBLIC :: text_length
PUBLIC :: text_replace
PUBLIC :: text_split
PUBLIC :: text_subtext
PUBLIC :: text_to_integer
PUBLIC :: text_to_lower
PUBLIC :: text_to_pascal
PUBLIC :: text_to_upper
PUBLIC :: toggle_borderless_windowed
PUBLIC :: toggle_fullscreen
PUBLIC :: trace_log
PUBLIC :: unload_audio_stream
PUBLIC :: unload_codepoints
PUBLIC :: unload_directory_files
PUBLIC :: unload_dropped_files
PUBLIC :: unload_file_data
PUBLIC :: unload_file_text
PUBLIC :: unload_font
PUBLIC :: unload_font_data
PUBLIC :: unload_image
PUBLIC :: unload_image_colors
PUBLIC :: unload_image_palette
PUBLIC :: unload_material
PUBLIC :: unload_mesh
PUBLIC :: unload_model
PUBLIC :: unload_model_animation
PUBLIC :: unload_model_animations
PUBLIC :: unload_music_stream
PUBLIC :: unload_random_sequence
PUBLIC :: unload_render_texture
PUBLIC :: unload_shader
PUBLIC :: unload_sound
PUBLIC :: unload_sound_alias
PUBLIC :: unload_texture
PUBLIC :: unload_utf8
PUBLIC :: unload_vr_stereo_config
PUBLIC :: unload_wave
PUBLIC :: unload_wave_samples
PUBLIC :: update_audio_stream
PUBLIC :: update_camera
PUBLIC :: update_mesh_buffer
PUBLIC :: update_model_animation
PUBLIC :: update_music_stream
PUBLIC :: update_sound
PUBLIC :: update_texture
PUBLIC :: update_texture_rec
PUBLIC :: upload_mesh
PUBLIC :: wait_time
PUBLIC :: wave_copy
PUBLIC :: wave_crop
PUBLIC :: wave_format
PUBLIC :: window_should_close

PUBLIC :: load_file_data_callback
PUBLIC :: save_file_data_callback
PUBLIC :: load_file_text_callback
PUBLIC :: save_file_text_callback
PUBLIC :: trace_log_callback

PUBLIC :: deg2rad
PUBLIC :: rad2deg

ABSTRACT INTERFACE
  ! unsigned char *(*LoadFileDataCallback)(const char *fileName, unsigned int *bytesRead)
  FUNCTION load_file_data_callback(file_name, bytes_read) BIND(c)
    IMPORT :: C_PTR, c_unsigned_int
    IMPLICIT NONE
    TYPE(C_PTR), INTENT(in), VALUE :: file_name
    INTEGER(kind=c_unsigned_int), INTENT(out) :: bytes_read
    TYPE(C_PTR) :: load_file_data_callback
  END FUNCTION load_file_data_callback

  ! bool (*SaveFileDataCallback)(const char *fileName, void *data, unsigned int bytesToWrite)
  FUNCTION save_file_data_callback(file_name, DATA, bytes_to_write) BIND(c)
    IMPORT :: C_BOOL, C_PTR, c_unsigned_int
    IMPLICIT NONE
    TYPE(C_PTR), INTENT(in), VALUE :: file_name
    TYPE(C_PTR), INTENT(in), VALUE :: DATA
    INTEGER(kind=c_unsigned_int), INTENT(in), VALUE :: bytes_to_write
    LOGICAL(kind=C_BOOL) :: save_file_data_callback
  END FUNCTION save_file_data_callback

  ! char *(*LoadFileTextCallback)(const char *fileName)
  FUNCTION load_file_text_callback(file_name) BIND(c)
    IMPORT :: C_PTR, c_unsigned_int
    IMPLICIT NONE
    TYPE(C_PTR), INTENT(in), VALUE :: file_name
    TYPE(C_PTR) :: load_file_text_callback
  END FUNCTION load_file_text_callback

  ! bool (*SaveFileTextCallback)(const char *fileName, char *text)
  FUNCTION save_file_text_callback(file_name, text) BIND(c)
    IMPORT :: C_BOOL, C_PTR
    IMPLICIT NONE
    TYPE(C_PTR), INTENT(in), VALUE :: file_name
    TYPE(C_PTR), INTENT(in), VALUE :: text
    LOGICAL(kind=C_BOOL) :: save_file_text_callback
  END FUNCTION save_file_text_callback

  ! void (*TraceLogCallback)(int logLevel, const char *text, va_list args)
  SUBROUTINE trace_log_callback(log_level, text, args) BIND(c)
    IMPORT :: C_INT, C_PTR
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: log_level
    TYPE(C_PTR), INTENT(in), VALUE :: text
    TYPE(C_PTR), INTENT(in) :: args(*)
  END SUBROUTINE trace_log_callback
END INTERFACE

INTERFACE
  ! void AttachAudioMixedProcessor(AudioCallback processor)
        subroutine attach_audio_mixed_processor(processor) bind(c, name='AttachAudioMixedProcessor')
    IMPORT :: C_FUNPTR
    IMPLICIT NONE
    TYPE(C_FUNPTR), INTENT(in), VALUE :: processor
  END SUBROUTINE attach_audio_mixed_processor

  ! void AttachAudioStreamProcessor(AudioStream stream, AudioCallback processor)
        subroutine attach_audio_stream_processor(stream, processor) bind(c, name='AttachAudioStreamProcessor')
    IMPORT :: audio_stream_type, C_FUNPTR
    IMPLICIT NONE
    TYPE(audio_stream_type), INTENT(in), VALUE :: stream
    TYPE(C_FUNPTR), INTENT(in), VALUE :: processor
  END SUBROUTINE attach_audio_stream_processor

  ! void BeginBlendMode(int mode)
  SUBROUTINE begin_blend_mode(mode) BIND(c, name='BeginBlendMode')
    IMPORT :: C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: mode
  END SUBROUTINE begin_blend_mode

  ! void BeginDrawing(void)
  SUBROUTINE begin_drawing() BIND(c, name='BeginDrawing')
  END SUBROUTINE begin_drawing

  ! void BeginMode2D(Camera2D camera)
  SUBROUTINE begin_mode2d(camera) BIND(c, name='BeginMode2D')
    IMPORT :: camera2d_type
    IMPLICIT NONE
    TYPE(camera2d_type), INTENT(in), VALUE :: camera
  END SUBROUTINE begin_mode2d

  ! void BeginMode3D(Camera3D camera)
  SUBROUTINE begin_mode3d(camera) BIND(c, name='BeginMode3D')
    IMPORT :: camera3d_type
    IMPLICIT NONE
    TYPE(camera3d_type), INTENT(in), VALUE :: camera
  END SUBROUTINE begin_mode3d

  ! void BeginScissorMode(int x, int y, int width, int height)
        subroutine begin_scissor_mode(x, y, width, height) bind(c, name='BeginScissorMode')
    IMPORT :: C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: x
    INTEGER(kind=C_INT), INTENT(in), VALUE :: y
    INTEGER(kind=C_INT), INTENT(in), VALUE :: width
    INTEGER(kind=C_INT), INTENT(in), VALUE :: height
  END SUBROUTINE begin_scissor_mode

  ! void BeginShaderMode(Shader shader)
  SUBROUTINE begin_shader_mode(shader) BIND(c, name='BeginShaderMode')
    IMPORT :: shader_type
    IMPLICIT NONE
    TYPE(shader_type), INTENT(in), VALUE :: shader
  END SUBROUTINE begin_shader_mode

  ! void BeginTextureMode(RenderTexture2D target)
  SUBROUTINE begin_texture_mode(TARGET) BIND(c, name='BeginTextureMode')
    IMPORT :: render_texture2d_type
    IMPLICIT NONE
    TYPE(render_texture2d_type), INTENT(in), VALUE :: TARGET
  END SUBROUTINE begin_texture_mode

  ! void BeginVrStereoMode(VrStereoConfig config)
  SUBROUTINE begin_vr_stereo_mode(config) BIND(c, name='BeginVrStereoMode')
    IMPORT :: vr_stereo_config_type
    IMPLICIT NONE
    TYPE(vr_stereo_config_type), INTENT(in), VALUE :: config
  END SUBROUTINE begin_vr_stereo_mode

  ! bool ChangeDirectory(const char *dir)
  FUNCTION change_directory(dir) BIND(c, name='ChangeDirectory')
    IMPORT :: C_BOOL, C_CHAR
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: dir
    LOGICAL(kind=C_BOOL) :: change_directory
  END FUNCTION change_directory

  ! bool CheckCollisionBoxSphere(BoundingBox box, Vector3 center, float radius)
        function check_collision_box_sphere(box, center, radius) bind(c, name='CheckCollisionBoxSphere')
    IMPORT :: bounding_box_type, C_BOOL, C_FLOAT, vector3_type
    IMPLICIT NONE
    TYPE(bounding_box_type), INTENT(in), VALUE :: box
    TYPE(vector3_type), INTENT(in), VALUE :: center
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: radius
    LOGICAL(kind=C_BOOL) :: check_collision_box_sphere
  END FUNCTION check_collision_box_sphere

  ! bool CheckCollisionBoxes(BoundingBox box1, BoundingBox box2)
FUNCTION check_collision_boxes(box1, box2) BIND(c, name='CheckCollisionBoxes')
    IMPORT :: bounding_box_type, C_BOOL
    IMPLICIT NONE
    TYPE(bounding_box_type), INTENT(in), VALUE :: box1
    TYPE(bounding_box_type), INTENT(in), VALUE :: box2
    LOGICAL(kind=C_BOOL) :: check_collision_boxes
  END FUNCTION check_collision_boxes

  ! bool CheckCollisionCircleRec(Vector2 center, float radius, Rectangle rec)
        function check_collision_circle_rec(center, radius, rec) bind(c, name='CheckCollisionCircleRec')
    IMPORT :: C_BOOL, C_FLOAT, rectangle_type, vector2_type
    IMPLICIT NONE
    TYPE(vector2_type), INTENT(in), VALUE :: center
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: radius
    TYPE(rectangle_type), INTENT(in), VALUE :: rec
    LOGICAL(kind=C_BOOL) :: check_collision_circle_rec
  END FUNCTION check_collision_circle_rec

  ! bool CheckCollisionCircles(Vector2 center1, float radius1, Vector2 center2, float radius2)
        function check_collision_circles(center1, radius1, center2, radius2) bind(c, name='CheckCollisionCircles')
    IMPORT :: C_BOOL, C_FLOAT, vector2_type
    IMPLICIT NONE
    TYPE(vector2_type), INTENT(in), VALUE :: center1
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: radius1
    TYPE(vector2_type), INTENT(in), VALUE :: center2
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: radius2
    LOGICAL(kind=C_BOOL) :: check_collision_circles
  END FUNCTION check_collision_circles

  ! bool CheckCollisionLines(Vector2 startPos1, Vector2 endPos1, Vector2 startPos2, Vector2 endPos2, Vector2 *collisionPoint)
        function check_collision_lines(start_pos1, end_pos1, start_pos2, end_pos2, collision_point) &
    BIND(c, name='CheckCollisionLines')
    IMPORT :: C_BOOL, vector2_type
    IMPLICIT NONE
    TYPE(vector2_type), INTENT(in), VALUE :: start_pos1
    TYPE(vector2_type), INTENT(in), VALUE :: end_pos1
    TYPE(vector2_type), INTENT(in), VALUE :: start_pos2
    TYPE(vector2_type), INTENT(in), VALUE :: end_pos2
    TYPE(vector2_type), INTENT(out) :: collision_point
    LOGICAL(kind=C_BOOL) :: check_collision_lines
  END FUNCTION check_collision_lines

  ! bool CheckCollisionPointCircle(Vector2 point, Vector2 center, float radius)
        function check_collision_point_circle(point, center, radius) bind(c, name='CheckCollisionPointCircle')
    IMPORT :: C_BOOL, C_FLOAT, vector2_type
    IMPLICIT NONE
    TYPE(vector2_type), INTENT(in), VALUE :: point
    TYPE(vector2_type), INTENT(in), VALUE :: center
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: radius
    LOGICAL(kind=C_BOOL) :: check_collision_point_circle
  END FUNCTION check_collision_point_circle

  ! bool CheckCollisionPointLine(Vector2 point, Vector2 p1, Vector2 p2, int threshold)
        function check_collision_point_line(point, p1, p2, threshold) bind(c, name='CheckCollisionPointLine')
    IMPORT :: C_BOOL, C_INT, vector2_type
    IMPLICIT NONE
    TYPE(vector2_type), INTENT(in), VALUE :: point
    TYPE(vector2_type), INTENT(in), VALUE :: p1
    TYPE(vector2_type), INTENT(in), VALUE :: p2
    INTEGER(kind=C_INT), INTENT(in), VALUE :: threshold
    LOGICAL(kind=C_BOOL) :: check_collision_point_line
  END FUNCTION check_collision_point_line

  ! bool CheckCollisionPointPoly(Vector2 point, Vector2 *points, int pointCount)
        function check_collision_point_poly(point, points, point_count) bind(c, name='CheckCollisionPointPoly')
    IMPORT :: C_BOOL, C_INT, vector2_type
    IMPLICIT NONE
    TYPE(vector2_type), INTENT(in), VALUE :: point
    TYPE(vector2_type), INTENT(in) :: points(*)
    INTEGER(kind=C_INT), INTENT(in), VALUE :: point_count
    LOGICAL(kind=C_BOOL) :: check_collision_point_poly
  END FUNCTION check_collision_point_poly

  ! bool CheckCollisionPointRec(Vector2 point, Rectangle rec)
        function check_collision_point_rec(point, rec) bind(c, name='CheckCollisionPointRec')
    IMPORT :: C_BOOL, rectangle_type, vector2_type
    IMPLICIT NONE
    TYPE(vector2_type), INTENT(in), VALUE :: point
    TYPE(rectangle_type), INTENT(in), VALUE :: rec
    LOGICAL(kind=C_BOOL) :: check_collision_point_rec
  END FUNCTION check_collision_point_rec

  ! bool CheckCollisionPointTriangle(Vector2 point, Vector2 p1, Vector2 p2, Vector2 p3)
        function check_collision_point_triangle(point, p1, p2, p3) bind(c, name='CheckCollisionPointTriangle')
    IMPORT :: C_BOOL, vector2_type
    IMPLICIT NONE
    TYPE(vector2_type), INTENT(in), VALUE :: point
    TYPE(vector2_type), INTENT(in), VALUE :: p1
    TYPE(vector2_type), INTENT(in), VALUE :: p2
    TYPE(vector2_type), INTENT(in), VALUE :: p3
    LOGICAL(kind=C_BOOL) :: check_collision_point_triangle
  END FUNCTION check_collision_point_triangle

  ! bool CheckCollisionRecs(Rectangle rec1, Rectangle rec2)
  FUNCTION check_collision_recs(rec1, rec2) BIND(c, name='CheckCollisionRecs')
    IMPORT :: C_BOOL, rectangle_type
    IMPLICIT NONE
    TYPE(rectangle_type), INTENT(in), VALUE :: rec1
    TYPE(rectangle_type), INTENT(in), VALUE :: rec2
    LOGICAL(kind=C_BOOL) :: check_collision_recs
  END FUNCTION check_collision_recs

  ! bool CheckCollisionSpheres(Vector3 center1, float radius1, Vector3 center2, float radius2)
        function check_collision_spheres(center1, radius1, center2, radius2) bind(c, name='CheckCollisionSpheres')
    IMPORT :: C_BOOL, C_FLOAT, vector3_type
    IMPLICIT NONE
    TYPE(vector3_type), INTENT(in), VALUE :: center1
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: radius1
    TYPE(vector3_type), INTENT(in), VALUE :: center2
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: radius2
    LOGICAL(kind=C_BOOL) :: check_collision_spheres
  END FUNCTION check_collision_spheres

  ! void ClearBackground(Color color)
  SUBROUTINE clear_background(color) BIND(c, name='ClearBackground')
    IMPORT :: color_type
    IMPLICIT NONE
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE clear_background

  ! void ClearWindowState(unsigned int flags)
  SUBROUTINE clear_window_state(flags) BIND(c, name='ClearWindowState')
    IMPORT :: c_unsigned_int
    IMPLICIT NONE
    INTEGER(kind=c_unsigned_int), INTENT(in), VALUE :: flags
  END SUBROUTINE clear_window_state

  ! void CloseAudioDevice(void)
  SUBROUTINE close_audio_device() BIND(c, name='CloseAudioDevice')
  END SUBROUTINE close_audio_device

  ! void CloseWindow(void)
  SUBROUTINE close_window() BIND(c, name='CloseWindow')
  END SUBROUTINE close_window

  ! const char *CodepointToUTF8(int codepoint, int *utf8Size)
        function codepoint_to_utf8(codepoint, utf8_size) bind(c, name='CodepointToUTF8')
    IMPORT :: C_INT, C_PTR
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: codepoint
    INTEGER(kind=C_INT), INTENT(out) :: utf8_size
    TYPE(C_PTR) :: codepoint_to_utf8
  END FUNCTION codepoint_to_utf8

  ! Color ColorAlpha(Color color, float alpha)
  FUNCTION color_alpha(color, alpha) BIND(c, name='ColorAlpha')
    IMPORT :: C_FLOAT, color_type
    IMPLICIT NONE
    TYPE(color_type), INTENT(in), VALUE :: color
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: alpha
    TYPE(color_type) :: color_alpha
  END FUNCTION color_alpha

  ! Color ColorAlphaBlend(Color dst, Color src, Color tint)
  FUNCTION color_alpha_blend(dst, src, tint) BIND(c, name='ColorAlphaBlend')
    IMPORT :: color_type
    IMPLICIT NONE
    TYPE(color_type), INTENT(in), VALUE :: dst
    TYPE(color_type), INTENT(in), VALUE :: src
    TYPE(color_type), INTENT(in), VALUE :: tint
    TYPE(color_type) :: color_alpha_blend
  END FUNCTION color_alpha_blend

  ! Color ColorBrightness(Color color, float factor)
  FUNCTION color_brightness(color, factor) BIND(c, name='ColorBrightness')
    IMPORT :: C_FLOAT, color_type
    IMPLICIT NONE
    TYPE(color_type), INTENT(in), VALUE :: color
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: factor
    TYPE(color_type) :: color_brightness
  END FUNCTION color_brightness

  ! Color ColorContrast(Color color, float contrast)
  FUNCTION color_contrast(color, contrast) BIND(c, name='ColorContrast')
    IMPORT :: C_FLOAT, color_type
    IMPLICIT NONE
    TYPE(color_type), INTENT(in), VALUE :: color
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: contrast
    TYPE(color_type) :: color_contrast
  END FUNCTION color_contrast

  ! Color ColorFromHSV(float hue, float saturation, float value)
  FUNCTION color_from_hsv(hue, saturation, VALUE) BIND(c, name='ColorFromHSV')
    IMPORT :: C_FLOAT, color_type
    IMPLICIT NONE
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: hue
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: saturation
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: VALUE
    TYPE(color_type) :: color_from_hsv
  END FUNCTION color_from_hsv

  ! Color ColorFromNormalized(Vector4 normalized)
FUNCTION color_from_normalized(normalized) BIND(c, name='ColorFromNormalized')
    IMPORT :: color_type, vector4_type
    IMPLICIT NONE
    TYPE(vector4_type), INTENT(in), VALUE :: normalized
    TYPE(color_type) :: color_from_normalized
  END FUNCTION color_from_normalized

  ! Color ColorTint(Color color, Color tint)
  FUNCTION color_tint(color, tint) BIND(c, name='ColorTint')
    IMPORT :: color_type
    IMPLICIT NONE
    TYPE(color_type), INTENT(in), VALUE :: color
    TYPE(color_type), INTENT(in), VALUE :: tint
    TYPE(color_type) :: color_tint
  END FUNCTION color_tint

  ! int ColorToInt(Color color)
  FUNCTION color_to_int(color) BIND(c, name='ColorToInt')
    IMPORT :: C_INT, color_type
    IMPLICIT NONE
    TYPE(color_type), INTENT(in), VALUE :: color
    INTEGER(kind=C_INT) :: color_to_int
  END FUNCTION color_to_int

  ! unsigned char *CompressData(const unsigned char *data, int dataSize, int *compDataSize)
        function compress_data(data, data_size, comp_data_size) bind(c, name='CompressData')
    IMPORT :: C_INT, C_PTR, c_unsigned_char
    IMPLICIT NONE
    INTEGER(kind=c_unsigned_char), INTENT(in) :: DATA
    INTEGER(kind=C_INT), INTENT(in), VALUE :: data_size
    INTEGER(kind=C_INT), INTENT(out) :: comp_data_size
    TYPE(C_PTR) :: compress_data
  END FUNCTION compress_data

  ! unsigned char *DecodeDataBase64(const unsigned char *data, int *outputSize)
        function decode_data_base64(data, output_size) bind(c, name='DecodeDataBase64')
    IMPORT :: C_INT, c_unsigned_char, C_PTR
    IMPLICIT NONE
    INTEGER(kind=c_unsigned_char), INTENT(in) :: DATA
    INTEGER(kind=C_INT), INTENT(out) :: output_size
    TYPE(C_PTR) :: decode_data_base64
  END FUNCTION decode_data_base64

  ! unsigned char *DecompressData(const unsigned char *compData, int compDataSize, int *dataSize)
        function decompress_data(comp_data, comp_data_size, data_size) bind(c, name='DecompressData')
    IMPORT :: C_INT, C_PTR, c_unsigned_char
    IMPLICIT NONE
    INTEGER(kind=c_unsigned_char), INTENT(in) :: comp_data
    INTEGER(kind=C_INT), INTENT(in), VALUE :: comp_data_size
    INTEGER(kind=C_INT), INTENT(out) :: data_size
    TYPE(C_PTR) :: decompress_data
  END FUNCTION decompress_data

  ! void DetachAudioMixedProcessor(AudioCallback processor)
        subroutine detach_audio_mixed_processor(processor) bind(c, name='DetachAudioMixedProcessor')
    IMPORT :: C_FUNPTR
    IMPLICIT NONE
    TYPE(C_FUNPTR), INTENT(in), VALUE :: processor
  END SUBROUTINE detach_audio_mixed_processor

  ! void DetachAudioStreamProcessor(AudioStream stream, AudioCallback processor)
        subroutine detach_audio_stream_processor(stream, processor) bind(c, name='DetachAudioStreamProcessor')
    IMPORT :: audio_stream_type, C_FUNPTR
    IMPLICIT NONE
    TYPE(audio_stream_type), INTENT(in), VALUE :: stream
    TYPE(C_FUNPTR), INTENT(in), VALUE :: processor
  END SUBROUTINE detach_audio_stream_processor

  ! bool DirectoryExists(const char *dirPath)
  FUNCTION directory_exists(dir_path) BIND(c, name='DirectoryExists')
    IMPORT :: C_BOOL, C_CHAR
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: dir_path
    LOGICAL(kind=C_BOOL) :: directory_exists
  END FUNCTION directory_exists

  ! void DisableCursor(void)
  SUBROUTINE disable_cursor() BIND(c, name='DisableCursor')
  END SUBROUTINE disable_cursor

  ! void DisableEventWaiting(void)
  SUBROUTINE disable_event_waiting() BIND(c, name='DisableEventWaiting')
  END SUBROUTINE disable_event_waiting

  ! void EnableCursor(void)
  SUBROUTINE enable_cursor() BIND(c, name='EnableCursor')
  END SUBROUTINE enable_cursor

  ! void EnableEventWaiting(void)
  SUBROUTINE enable_event_waiting() BIND(c, name='EnableEventWaiting')
  END SUBROUTINE enable_event_waiting

  ! char *EncodeDataBase64(const unsigned char *data, int dataSize, int *outputSize)
        function encode_data_base64(data, data_size, output_size) bind(c, name='EncodeDataBase64')
    IMPORT :: C_INT, c_unsigned_char, C_PTR
    IMPLICIT NONE
    INTEGER(kind=c_unsigned_char), INTENT(in) :: DATA
    INTEGER(kind=C_INT), INTENT(in), VALUE :: data_size
    INTEGER(kind=C_INT), INTENT(out) :: output_size
    TYPE(C_PTR) :: encode_data_base64
  END FUNCTION encode_data_base64

  ! void EndBlendMode(void)
  SUBROUTINE end_blend_mode() BIND(c, name='EndBlendMode')
  END SUBROUTINE end_blend_mode

  ! void EndDrawing(void)
  SUBROUTINE end_drawing() BIND(c, name='EndDrawing')
  END SUBROUTINE end_drawing

  ! void EndMode2D(void)
  SUBROUTINE end_mode2d() BIND(c, name='EndMode2D')
  END SUBROUTINE end_mode2d

  ! void EndMode3D(void)
  SUBROUTINE end_mode3d() BIND(c, name='EndMode3D')
  END SUBROUTINE end_mode3d

  ! void EndScissorMode(void)
  SUBROUTINE end_scissor_mode() BIND(c, name='EndScissorMode')
  END SUBROUTINE end_scissor_mode

  ! void EndShaderMode(void)
  SUBROUTINE end_shader_mode() BIND(c, name='EndShaderMode')
  END SUBROUTINE end_shader_mode

  ! void EndTextureMode(void)
  SUBROUTINE end_texture_mode() BIND(c, name='EndTextureMode')
  END SUBROUTINE end_texture_mode

  ! void EndVrStereoMode(void)
  SUBROUTINE end_vr_stereo_mode() BIND(c, name='EndVrStereoMode')
  END SUBROUTINE end_vr_stereo_mode

  ! bool ExportDataAsCode(const unsigned char *data, int dataSize, const char *fileName)
        function export_data_as_code(data, data_size, file_name) bind(c, name='ExportDataAsCode')
    IMPORT :: C_BOOL, C_CHAR, C_INT, c_unsigned_char
    IMPLICIT NONE
    INTEGER(kind=c_unsigned_char), INTENT(in) :: DATA
    INTEGER(kind=C_INT), INTENT(in), VALUE :: data_size
    CHARACTER(kind=C_CHAR), INTENT(in) :: file_name
    LOGICAL(kind=C_BOOL) :: export_data_as_code
  END FUNCTION export_data_as_code

  ! bool ExportFontAsCode(Font font, const char *fileName)
FUNCTION export_font_as_code(font, file_name) BIND(c, name='ExportFontAsCode')
    IMPORT :: C_BOOL, C_CHAR, font_type
    IMPLICIT NONE
    TYPE(font_type), INTENT(in), VALUE :: font
    CHARACTER(kind=C_CHAR), INTENT(in) :: file_name
    LOGICAL(kind=C_BOOL) :: export_font_as_code
  END FUNCTION export_font_as_code

  ! bool ExportImage(Image image, const char *fileName)
  FUNCTION export_image(image, file_name) BIND(c, name='ExportImage')
    IMPORT :: C_BOOL, C_CHAR, image_type
    IMPLICIT NONE
    TYPE(image_type), INTENT(in), VALUE :: image
    CHARACTER(kind=C_CHAR), INTENT(in) :: file_name
    LOGICAL(kind=C_BOOL) :: export_image
  END FUNCTION export_image

  ! bool ExportImageAsCode(Image image, const char *fileName)
        function export_image_as_code(image, file_name) bind(c, name='ExportImageAsCode')
    IMPORT :: C_BOOL, C_CHAR, image_type
    IMPLICIT NONE
    TYPE(image_type), INTENT(in), VALUE :: image
    CHARACTER(kind=C_CHAR), INTENT(in) :: file_name
    LOGICAL(kind=C_BOOL) :: export_image_as_code
  END FUNCTION export_image_as_code

  ! unsigned char *ExportImageToMemory(Image image, const char *fileType, int *fileSize)
        function export_image_to_memory(image, file_type, file_size) bind(c, name='ExportImageToMemory')
    IMPORT :: C_CHAR, C_INT, C_PTR, image_type
    IMPLICIT NONE
    TYPE(image_type), INTENT(in), VALUE :: image
    CHARACTER(kind=C_CHAR), INTENT(in) :: file_type
    INTEGER(kind=C_INT), INTENT(out) :: file_size
    TYPE(C_PTR) :: export_image_to_memory
  END FUNCTION export_image_to_memory

  ! bool ExportMesh(Mesh mesh, const char *fileName)
  FUNCTION export_mesh(mesh, file_name) BIND(c, name='ExportMesh')
    IMPORT :: C_BOOL, C_CHAR, mesh_type
    IMPLICIT NONE
    TYPE(mesh_type), INTENT(in), VALUE :: mesh
    CHARACTER(kind=C_CHAR), INTENT(in) :: file_name
    LOGICAL(kind=C_BOOL) :: export_mesh
  END FUNCTION export_mesh

  ! bool ExportWave(Wave wave, const char *fileName)
  FUNCTION export_wave(wave, file_name) BIND(c, name='ExportWave')
    IMPORT :: C_BOOL, C_CHAR, wave_type
    IMPLICIT NONE
    TYPE(wave_type), INTENT(in), VALUE :: wave
    CHARACTER(kind=C_CHAR), INTENT(in) :: file_name
    LOGICAL(kind=C_BOOL) :: export_wave
  END FUNCTION export_wave

  ! bool ExportWaveAsCode(Wave wave, const char *fileName)
FUNCTION export_wave_as_code(wave, file_name) BIND(c, name='ExportWaveAsCode')
    IMPORT :: C_BOOL, C_CHAR, wave_type
    IMPLICIT NONE
    TYPE(wave_type), INTENT(in), VALUE :: wave
    CHARACTER(kind=C_CHAR), INTENT(in) :: file_name
    LOGICAL(kind=C_BOOL) :: export_wave_as_code
  END FUNCTION export_wave_as_code

  ! Color Fade(Color color, float alpha)
  FUNCTION fade(color, alpha) BIND(c, name='Fade')
    IMPORT :: C_FLOAT, color_type
    IMPLICIT NONE
    TYPE(color_type), INTENT(in), VALUE :: color
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: alpha
    TYPE(color_type) :: fade
  END FUNCTION fade

  ! bool FileExists(const char *fileName)
  FUNCTION file_exists(file_name) BIND(c, name='FileExists')
    IMPORT :: C_BOOL, C_CHAR
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: file_name
    LOGICAL(kind=C_BOOL) :: file_exists
  END FUNCTION file_exists

  ! Image GenImageCellular(int width, int height, int tileSize)
        function gen_image_cellular(width, height, tile_size) bind(c, name='GenImageCellular')
    IMPORT :: C_INT, image_type
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: width
    INTEGER(kind=C_INT), INTENT(in), VALUE :: height
    INTEGER(kind=C_INT), INTENT(in), VALUE :: tile_size
    TYPE(image_type) :: gen_image_cellular
  END FUNCTION gen_image_cellular

  ! Image GenImageChecked(int width, int height, int checksX, int checksY, Color col1, Color col2)
        function gen_image_checked(width, height, checks_x, checks_y, col1, col2) bind(c, name='GenImageChecked')
    IMPORT :: C_INT, color_type, image_type
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: width
    INTEGER(kind=C_INT), INTENT(in), VALUE :: height
    INTEGER(kind=C_INT), INTENT(in), VALUE :: checks_x
    INTEGER(kind=C_INT), INTENT(in), VALUE :: checks_y
    TYPE(color_type), INTENT(in), VALUE :: col1
    TYPE(color_type), INTENT(in), VALUE :: col2
    TYPE(image_type) :: gen_image_checked
  END FUNCTION gen_image_checked

  ! Image GenImageColor(int width, int height, Color color)
  FUNCTION gen_image_color(width, height, color) BIND(c, name='GenImageColor')
    IMPORT :: C_INT, color_type, image_type
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: width
    INTEGER(kind=C_INT), INTENT(in), VALUE :: height
    TYPE(color_type), INTENT(in), VALUE :: color
    TYPE(image_type) :: gen_image_color
  END FUNCTION gen_image_color

  ! Image GenImageFontAtlas(const GlyphInfo *glyphs, Rectangle **glyphRecs, int glyphCount, int fontSize, int padding, int packMethod)
        function gen_image_font_atlas(glyphs, glyph_recs, glyph_count, font_size, padding, pack_method) &
    BIND(c, name='GenImageFontAtlas')
    IMPORT :: C_INT, glyph_info_type, image_type, rectangle_type
    IMPLICIT NONE
    TYPE(glyph_info_type), INTENT(inout) :: glyphs
    TYPE(rectangle_type), INTENT(inout) :: glyph_recs(*)
    INTEGER(kind=C_INT), INTENT(in), VALUE :: glyph_count
    INTEGER(kind=C_INT), INTENT(in), VALUE :: font_size
    INTEGER(kind=C_INT), INTENT(in), VALUE :: padding
    INTEGER(kind=C_INT), INTENT(in), VALUE :: pack_method
    TYPE(image_type) :: gen_image_font_atlas
  END FUNCTION gen_image_font_atlas

  ! Image GenImageGradientLinear(int width, int height, int direction, Color start, Color end)
        function gen_image_gradient_linear(width, height, direction, start, end) bind(c, name='GenImageGradientLinear')
    IMPORT :: C_INT, color_type, image_type
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: width
    INTEGER(kind=C_INT), INTENT(in), VALUE :: height
    INTEGER(kind=C_INT), INTENT(in), VALUE :: direction
    TYPE(color_type), INTENT(in), VALUE :: start
    TYPE(color_type), INTENT(in), VALUE :: END
    TYPE(image_type) :: gen_image_gradient_linear
  END FUNCTION gen_image_gradient_linear

  ! Image GenImageGradientRadial(int width, int height, float density, Color inner, Color outer)
        function gen_image_gradient_radial(width, height, density, inner, outer) bind(c, name='GenImageGradientRadial')
    IMPORT :: C_FLOAT, C_INT, color_type, image_type
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: width
    INTEGER(kind=C_INT), INTENT(in), VALUE :: height
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: density
    TYPE(color_type), INTENT(in), VALUE :: inner
    TYPE(color_type), INTENT(in), VALUE :: outer
    TYPE(image_type) :: gen_image_gradient_radial
  END FUNCTION gen_image_gradient_radial

  ! Image GenImageGradientSquare(int width, int height, float density, Color inner, Color outer)
        function gen_image_gradient_square(width, height, density, inner, outer) bind(c, name='GenImageGradientSquare')
    IMPORT :: C_FLOAT, C_INT, color_type, image_type
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: width
    INTEGER(kind=C_INT), INTENT(in), VALUE :: height
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: density
    TYPE(color_type), INTENT(in), VALUE :: inner
    TYPE(color_type), INTENT(in), VALUE :: outer
    TYPE(image_type) :: gen_image_gradient_square
  END FUNCTION gen_image_gradient_square

  ! Image GenImageWhiteNoise(int width, int height, float factor)
        function gen_image_white_noise(width, height, factor) bind(c, name='GenImageWhiteNoise')
    IMPORT :: C_FLOAT, C_INT, image_type
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: width
    INTEGER(kind=C_INT), INTENT(in), VALUE :: height
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: factor
    TYPE(image_type) :: gen_image_white_noise
  END FUNCTION gen_image_white_noise

  ! Image GenImagePerlinNoise(int width, int height, int offsetX, int offsetY, float scale)
        function gen_image_perlin_noise(width, height, offset_x, offset_y, scale) bind(c, name='GenImagePerlinNoise')
    IMPORT :: C_FLOAT, C_INT, image_type
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: width
    INTEGER(kind=C_INT), INTENT(in), VALUE :: height
    INTEGER(kind=C_INT), INTENT(in), VALUE :: offset_x
    INTEGER(kind=C_INT), INTENT(in), VALUE :: offset_y
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: scale
    TYPE(image_type) :: gen_image_perlin_noise
  END FUNCTION gen_image_perlin_noise

  ! Image GenImageText(int width, int height, const char *text)
  FUNCTION gen_image_text(width, height, text) BIND(c, name='GenImageText')
    IMPORT :: C_CHAR, C_INT, image_type
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: width
    INTEGER(kind=C_INT), INTENT(in), VALUE :: height
    CHARACTER(kind=C_CHAR), INTENT(in) :: text
    TYPE(image_type) :: gen_image_text
  END FUNCTION gen_image_text

  ! Mesh GenMeshCone(float radius, float height, int slices)
  FUNCTION gen_mesh_cone(radius, height, slices) BIND(c, name='GenMeshCone')
    IMPORT :: C_FLOAT, C_INT, mesh_type
    IMPLICIT NONE
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: radius
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: height
    INTEGER(kind=C_INT), INTENT(in), VALUE :: slices
    TYPE(mesh_type) :: gen_mesh_cone
  END FUNCTION gen_mesh_cone

  ! Mesh GenMeshCube(float width, float height, float length)
  FUNCTION gen_mesh_cube(width, height, length) BIND(c, name='GenMeshCube')
    IMPORT :: C_FLOAT, mesh_type
    IMPLICIT NONE
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: width
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: height
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: length
    TYPE(mesh_type) :: gen_mesh_cube
  END FUNCTION gen_mesh_cube

  ! Mesh GenMeshCubicmap(Image cubicmap, Vector3 cubeSize)
        function gen_mesh_cubicmap(cubicmap, cube_size) bind(c, name='GenMeshCubicmap')
    IMPORT :: image_type, mesh_type, vector3_type
    IMPLICIT NONE
    TYPE(image_type), INTENT(in), VALUE :: cubicmap
    TYPE(vector3_type), INTENT(in), VALUE :: cube_size
    TYPE(mesh_type) :: gen_mesh_cubicmap
  END FUNCTION gen_mesh_cubicmap

  ! Mesh GenMeshCylinder(float radius, float height, int slices)
        function gen_mesh_cylinder(radius, height, slices) bind(c, name='GenMeshCylinder')
    IMPORT :: C_FLOAT, C_INT, mesh_type
    IMPLICIT NONE
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: radius
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: height
    INTEGER(kind=C_INT), INTENT(in), VALUE :: slices
    TYPE(mesh_type) :: gen_mesh_cylinder
  END FUNCTION gen_mesh_cylinder

  ! Mesh GenMeshHeightmap(Image heightmap, Vector3 size)
 FUNCTION gen_mesh_heightmap(heightmap, size) BIND(c, name='GenMeshHeightmap')
    IMPORT :: image_type, mesh_type, vector3_type
    IMPLICIT NONE
    TYPE(image_type), INTENT(in), VALUE :: heightmap
    TYPE(vector3_type), INTENT(in), VALUE :: size
    TYPE(mesh_type) :: gen_mesh_heightmap
  END FUNCTION gen_mesh_heightmap

  ! Mesh GenMeshHemiSphere(float radius, int rings, int slices)
        function gen_mesh_hemi_sphere(radius, rings, slices) bind(c, name='GenMeshHemiSphere')
    IMPORT :: C_FLOAT, C_INT, mesh_type
    IMPLICIT NONE
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: radius
    INTEGER(kind=C_INT), INTENT(in), VALUE :: rings
    INTEGER(kind=C_INT), INTENT(in), VALUE :: slices
    TYPE(mesh_type) :: gen_mesh_hemi_sphere
  END FUNCTION gen_mesh_hemi_sphere

  ! Mesh GenMeshKnot(float radius, float size, int radSeg, int sides)
        function gen_mesh_knot(radius, size, rad_seg, sides) bind(c, name='GenMeshKnot')
    IMPORT :: C_FLOAT, C_INT, mesh_type
    IMPLICIT NONE
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: radius
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: size
    INTEGER(kind=C_INT), INTENT(in), VALUE :: rad_seg
    INTEGER(kind=C_INT), INTENT(in), VALUE :: sides
    TYPE(mesh_type) :: gen_mesh_knot
  END FUNCTION gen_mesh_knot

  ! Mesh GenMeshPlane(float width, float length, int resX, int resZ)
        function gen_mesh_plane(width, length, res_x, res_z) bind(c, name='GenMeshPlane')
    IMPORT :: C_FLOAT, C_INT, mesh_type
    IMPLICIT NONE
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: width
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: length
    INTEGER(kind=C_INT), INTENT(in), VALUE :: res_x
    INTEGER(kind=C_INT), INTENT(in), VALUE :: res_z
    TYPE(mesh_type) :: gen_mesh_plane
  END FUNCTION gen_mesh_plane

  ! Mesh GenMeshPoly(int sides, float radius)
  FUNCTION gen_mesh_poly(sides, radius) BIND(c, name='GenMeshPoly')
    IMPORT :: C_FLOAT, C_INT, mesh_type
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: sides
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: radius
    TYPE(mesh_type) :: gen_mesh_poly
  END FUNCTION gen_mesh_poly

  ! Mesh GenMeshSphere(float radius, int rings, int slices)
 FUNCTION gen_mesh_sphere(radius, rings, slices) BIND(c, name='GenMeshSphere')
    IMPORT :: C_FLOAT, C_INT, mesh_type
    IMPLICIT NONE
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: radius
    INTEGER(kind=C_INT), INTENT(in), VALUE :: rings
    INTEGER(kind=C_INT), INTENT(in), VALUE :: slices
    TYPE(mesh_type) :: gen_mesh_sphere
  END FUNCTION gen_mesh_sphere

  ! void GenMeshTangents(Mesh *mesh)
  SUBROUTINE gen_mesh_tangents(mesh) BIND(c, name='GenMeshTangents')
    IMPORT :: mesh_type
    IMPLICIT NONE
    TYPE(mesh_type), INTENT(in) :: mesh
  END SUBROUTINE gen_mesh_tangents

  ! Mesh GenMeshTorus(float radius, float size, int radSeg, int sides)
        function gen_mesh_torus(radius, size, rad_seg, sides) bind(c, name='GenMeshTorus')
    IMPORT :: C_FLOAT, C_INT, mesh_type
    IMPLICIT NONE
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: radius
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: size
    INTEGER(kind=C_INT), INTENT(in), VALUE :: rad_seg
    INTEGER(kind=C_INT), INTENT(in), VALUE :: sides
    TYPE(mesh_type) :: gen_mesh_torus
  END FUNCTION gen_mesh_torus

  ! void GenTextureMipmaps(Texture2D *texture)
  SUBROUTINE gen_texture_mipmaps(texture) BIND(c, name='GenTextureMipmaps')
    IMPORT :: texture2d_type
    IMPLICIT NONE
    TYPE(texture2d_type), INTENT(inout) :: texture
  END SUBROUTINE gen_texture_mipmaps

  ! void HideCursor(void)
  SUBROUTINE hide_cursor() BIND(c, name='HideCursor')
  END SUBROUTINE hide_cursor

  ! void InitAudioDevice(void)
  SUBROUTINE init_audio_device() BIND(c, name='InitAudioDevice')
  END SUBROUTINE init_audio_device

  ! void InitWindow(int width, int height, const char *title)
  SUBROUTINE init_window(width, height, title) BIND(c, name='InitWindow')
    IMPORT :: C_CHAR, C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: width
    INTEGER(kind=C_INT), INTENT(in), VALUE :: height
    CHARACTER(kind=C_CHAR), INTENT(in) :: title
  END SUBROUTINE init_window

  ! bool IsAudioDeviceReady(void)
  FUNCTION is_audio_device_ready() BIND(c, name='IsAudioDeviceReady')
    IMPORT :: C_BOOL
    IMPLICIT NONE
    LOGICAL(kind=C_BOOL) :: is_audio_device_ready
  END FUNCTION is_audio_device_ready

  ! bool IsAudioStreamPlaying(AudioStream stream)
 FUNCTION is_audio_stream_playing(stream) BIND(c, name='IsAudioStreamPlaying')
    IMPORT :: audio_stream_type, C_BOOL
    IMPLICIT NONE
    TYPE(audio_stream_type), INTENT(in), VALUE :: stream
    LOGICAL(kind=C_BOOL) :: is_audio_stream_playing
  END FUNCTION is_audio_stream_playing

  ! bool IsAudioStreamProcessed(AudioStream stream)
        function is_audio_stream_processed(stream) bind(c, name='IsAudioStreamProcessed')
    IMPORT :: audio_stream_type, C_BOOL
    IMPLICIT NONE
    TYPE(audio_stream_type), INTENT(in), VALUE :: stream
    LOGICAL(kind=C_BOOL) :: is_audio_stream_processed
  END FUNCTION is_audio_stream_processed

  ! bool IsAudioStreamReady(AudioStream stream)
  FUNCTION is_audio_stream_ready(stream) BIND(c, name='IsAudioStreamReady')
    IMPORT :: audio_stream_type, C_BOOL
    IMPLICIT NONE
    TYPE(audio_stream_type), INTENT(in), VALUE :: stream
    LOGICAL(kind=C_BOOL) :: is_audio_stream_ready
  END FUNCTION is_audio_stream_ready

  ! bool IsCursorHidden(void)
  FUNCTION is_cursor_hidden() BIND(c, name='IsCursorHidden')
    IMPORT :: C_BOOL
    IMPLICIT NONE
    LOGICAL(kind=C_BOOL) :: is_cursor_hidden
  END FUNCTION is_cursor_hidden

  ! bool IsCursorOnScreen(void)
  FUNCTION is_cursor_on_screen() BIND(c, name='IsCursorOnScreen')
    IMPORT :: C_BOOL
    IMPLICIT NONE
    LOGICAL(kind=C_BOOL) :: is_cursor_on_screen
  END FUNCTION is_cursor_on_screen

  ! bool IsFileDropped(void)
  FUNCTION is_file_dropped() BIND(c, name='IsFileDropped')
    IMPORT :: C_BOOL
    IMPLICIT NONE
    LOGICAL(kind=C_BOOL) :: is_file_dropped
  END FUNCTION is_file_dropped

  ! bool IsFileExtension(const char *fileName, const char *ext)
  FUNCTION is_file_extension(file_name, ext) BIND(c, name='IsFileExtension')
    IMPORT :: C_BOOL, C_CHAR
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: file_name
    CHARACTER(kind=C_CHAR), INTENT(in) :: ext
    LOGICAL(kind=C_BOOL) :: is_file_extension
  END FUNCTION is_file_extension

  ! bool IsFontReady(Font font)
  FUNCTION is_font_ready(font) BIND(c, name='IsFontReady')
    IMPORT :: C_BOOL, font_type
    IMPLICIT NONE
    TYPE(font_type), INTENT(in), VALUE :: font
    LOGICAL(kind=C_BOOL) :: is_font_ready
  END FUNCTION is_font_ready

  ! bool IsGamepadAvailable(int gamepad)
  FUNCTION is_gamepad_available(gamepad) BIND(c, name='IsGamepadAvailable')
    IMPORT :: C_BOOL, C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: gamepad
    LOGICAL(kind=C_BOOL) :: is_gamepad_available
  END FUNCTION is_gamepad_available

  ! bool IsGamepadButtonDown(int gamepad, int button)
        function is_gamepad_button_down(gamepad, button) bind(c, name='IsGamepadButtonDown')
    IMPORT :: C_BOOL, C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: gamepad
    INTEGER(kind=C_INT), INTENT(in), VALUE :: button
    LOGICAL(kind=C_BOOL) :: is_gamepad_button_down
  END FUNCTION is_gamepad_button_down

  ! bool IsGamepadButtonPressed(int gamepad, int button)
        function is_gamepad_button_pressed(gamepad, button) bind(c, name='IsGamepadButtonPressed')
    IMPORT :: C_BOOL, C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: gamepad
    INTEGER(kind=C_INT), INTENT(in), VALUE :: button
    LOGICAL(kind=C_BOOL) :: is_gamepad_button_pressed
  END FUNCTION is_gamepad_button_pressed

  ! bool IsGamepadButtonReleased(int gamepad, int button)
        function is_gamepad_button_released(gamepad, button) bind(c, name='IsGamepadButtonReleased')
    IMPORT :: C_BOOL, C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: gamepad
    INTEGER(kind=C_INT), INTENT(in), VALUE :: button
    LOGICAL(kind=C_BOOL) :: is_gamepad_button_released
  END FUNCTION is_gamepad_button_released

  ! bool IsGamepadButtonUp(int gamepad, int button)
        function is_gamepad_button_up(gamepad, button) bind(c, name='IsGamepadButtonUp')
    IMPORT :: C_BOOL, C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: gamepad
    INTEGER(kind=C_INT), INTENT(in), VALUE :: button
    LOGICAL(kind=C_BOOL) :: is_gamepad_button_up
  END FUNCTION is_gamepad_button_up

  ! bool IsGestureDetected(unsigned int gesture)
  FUNCTION is_gesture_detected(gesture) BIND(c, name='IsGestureDetected')
    IMPORT :: C_BOOL, c_unsigned_int
    IMPLICIT NONE
    INTEGER(kind=c_unsigned_int), INTENT(in), VALUE :: gesture
    LOGICAL(kind=C_BOOL) :: is_gesture_detected
  END FUNCTION is_gesture_detected

  ! bool IsImageReady(Image image)
  FUNCTION is_image_ready(image) BIND(c, name='IsImageReady')
    IMPORT :: C_BOOL, image_type
    IMPLICIT NONE
    TYPE(image_type), INTENT(in), VALUE :: image
    LOGICAL(kind=C_BOOL) :: is_image_ready
  END FUNCTION is_image_ready

  ! bool IsKeyDown(int key)
  FUNCTION is_key_down(key) BIND(c, name='IsKeyDown')
    IMPORT :: C_BOOL, C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: key
    LOGICAL(kind=C_BOOL) :: is_key_down
  END FUNCTION is_key_down

  ! bool IsKeyPressed(int key)
  FUNCTION is_key_pressed(key) BIND(c, name='IsKeyPressed')
    IMPORT :: C_BOOL, C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: key
    LOGICAL(kind=C_BOOL) :: is_key_pressed
  END FUNCTION is_key_pressed

  ! bool IsKeyPressedRepeat(int key)
  FUNCTION is_key_pressed_repeat(key) BIND(c, name='IsKeyPressedRepeat')
    IMPORT :: C_BOOL, C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: key
    LOGICAL(kind=C_BOOL) :: is_key_pressed_repeat
  END FUNCTION is_key_pressed_repeat

  ! bool IsKeyReleased(int key)
  FUNCTION is_key_released(key) BIND(c, name='IsKeyReleased')
    IMPORT :: C_BOOL, C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: key
    LOGICAL(kind=C_BOOL) :: is_key_released
  END FUNCTION is_key_released

  ! bool IsKeyUp(int key)
  FUNCTION is_key_up(key) BIND(c, name='IsKeyUp')
    IMPORT :: C_BOOL, C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: key
    LOGICAL(kind=C_BOOL) :: is_key_up
  END FUNCTION is_key_up

  ! bool IsMaterialReady(Material material)
  FUNCTION is_material_ready(material) BIND(c, name='IsMaterialReady')
    IMPORT :: C_BOOL, material_type
    IMPLICIT NONE
    TYPE(material_type), INTENT(in), VALUE :: material
    LOGICAL(kind=C_BOOL) :: is_material_ready
  END FUNCTION is_material_ready

  ! bool IsModelAnimationValid(Model model, ModelAnimation anim)
        function is_model_animation_valid(model, anim) bind(c, name='IsModelAnimationValid')
    IMPORT :: C_BOOL, model_animation_type, model_type
    IMPLICIT NONE
    TYPE(model_type), INTENT(in), VALUE :: model
    TYPE(model_animation_type), INTENT(in), VALUE :: anim
    LOGICAL(kind=C_BOOL) :: is_model_animation_valid
  END FUNCTION is_model_animation_valid

  ! bool IsModelReady(Model model)
  FUNCTION is_model_ready(model) BIND(c, name='IsModelReady')
    IMPORT :: C_BOOL, model_type
    IMPLICIT NONE
    TYPE(model_type), INTENT(in), VALUE :: model
    LOGICAL(kind=C_BOOL) :: is_model_ready
  END FUNCTION is_model_ready

  ! bool IsMouseButtonDown(int button)
  FUNCTION is_mouse_button_down(button) BIND(c, name='IsMouseButtonDown')
    IMPORT :: C_BOOL, C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: button
    LOGICAL(kind=C_BOOL) :: is_mouse_button_down
  END FUNCTION is_mouse_button_down

  ! bool IsMouseButtonPressed(int button)
 FUNCTION is_mouse_button_pressed(button) BIND(c, name='IsMouseButtonPressed')
    IMPORT :: C_BOOL, C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: button
    LOGICAL(kind=C_BOOL) :: is_mouse_button_pressed
  END FUNCTION is_mouse_button_pressed

  ! bool IsMouseButtonReleased(int button)
        function is_mouse_button_released(button) bind(c, name='IsMouseButtonReleased')
    IMPORT :: C_BOOL, C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: button
    LOGICAL(kind=C_BOOL) :: is_mouse_button_released
  END FUNCTION is_mouse_button_released

  ! bool IsMouseButtonUp(int button)
  FUNCTION is_mouse_button_up(button) BIND(c, name='IsMouseButtonUp')
    IMPORT :: C_BOOL, C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: button
    LOGICAL(kind=C_BOOL) :: is_mouse_button_up
  END FUNCTION is_mouse_button_up

  ! bool IsMusicReady(Music music)
  FUNCTION is_music_ready(music) BIND(c, name='IsMusicReady')
    IMPORT :: C_BOOL, music_type
    IMPLICIT NONE
    TYPE(music_type), INTENT(in), VALUE :: music
    LOGICAL(kind=C_BOOL) :: is_music_ready
  END FUNCTION is_music_ready

  ! bool IsMusicStreamPlaying(Music music)
  FUNCTION is_music_stream_playing(music) BIND(c, name='IsMusicStreamPlaying')
    IMPORT :: C_BOOL, music_type
    IMPLICIT NONE
    TYPE(music_type), INTENT(in), VALUE :: music
    LOGICAL(kind=C_BOOL) :: is_music_stream_playing
  END FUNCTION is_music_stream_playing

  ! bool IsPathFile(const char *path)
  FUNCTION is_path_file(path) BIND(c, name='IsPathFile')
    IMPORT :: C_BOOL, C_CHAR
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: path
    LOGICAL(kind=C_BOOL) :: is_path_file
  END FUNCTION is_path_file

  ! bool IsRenderTextureReady(RenderTexture2D target)
 FUNCTION is_render_texture_ready(TARGET) BIND(c, name='IsRenderTextureReady')
    IMPORT :: C_BOOL, render_texture2d_type
    IMPLICIT NONE
    TYPE(render_texture2d_type), INTENT(in), VALUE :: TARGET
    LOGICAL(kind=C_BOOL) :: is_render_texture_ready
  END FUNCTION is_render_texture_ready

  ! bool IsShaderReady(Shader shader)
  FUNCTION is_shader_ready(shader) BIND(c, name='IsShaderReady')
    IMPORT :: C_BOOL, shader_type
    IMPLICIT NONE
    TYPE(shader_type), INTENT(in), VALUE :: shader
    LOGICAL(kind=C_BOOL) :: is_shader_ready
  END FUNCTION is_shader_ready

  ! bool IsSoundPlaying(Sound sound)
  FUNCTION is_sound_playing(sound) BIND(c, name='IsSoundPlaying')
    IMPORT :: C_BOOL, sound_type
    IMPLICIT NONE
    TYPE(sound_type), INTENT(in), VALUE :: sound
    LOGICAL(kind=C_BOOL) :: is_sound_playing
  END FUNCTION is_sound_playing

  ! bool IsSoundReady(Sound sound)
  FUNCTION is_sound_ready(sound) BIND(c, name='IsSoundReady')
    IMPORT :: C_BOOL, sound_type
    IMPLICIT NONE
    TYPE(sound_type), INTENT(in), VALUE :: sound
    LOGICAL(kind=C_BOOL) :: is_sound_ready
  END FUNCTION is_sound_ready

  ! bool IsTextureReady(Texture2D texture)
  FUNCTION is_texture_ready(texture) BIND(c, name='IsTextureReady')
    IMPORT :: C_BOOL, texture2d_type
    IMPLICIT NONE
    TYPE(texture2d_type), INTENT(in), VALUE :: texture
    LOGICAL(kind=C_BOOL) :: is_texture_ready
  END FUNCTION is_texture_ready

  ! bool IsWaveReady(Wave wave)
  FUNCTION is_wave_ready(wave) BIND(c, name='IsWaveReady')
    IMPORT :: C_BOOL, wave_type
    IMPLICIT NONE
    TYPE(wave_type), INTENT(in), VALUE :: wave
    LOGICAL(kind=C_BOOL) :: is_wave_ready
  END FUNCTION is_wave_ready

  ! bool IsWindowFocused(void)
  FUNCTION is_window_focused() BIND(c, name='IsWindowFocused')
    IMPORT :: C_BOOL
    IMPLICIT NONE
    LOGICAL(kind=C_BOOL) :: is_window_focused
  END FUNCTION is_window_focused

  ! bool IsWindowFullscreen(void)
  FUNCTION is_window_fullscreen() BIND(c, name='IsWindowFullscreen')
    IMPORT :: C_BOOL
    IMPLICIT NONE
    LOGICAL(kind=C_BOOL) :: is_window_fullscreen
  END FUNCTION is_window_fullscreen

  ! bool IsWindowHidden(void)
  FUNCTION is_window_hidden() BIND(c, name='IsWindowHidden')
    IMPORT :: C_BOOL
    IMPLICIT NONE
    LOGICAL(kind=C_BOOL) :: is_window_hidden
  END FUNCTION is_window_hidden

  ! bool IsWindowMaximized(void)
  FUNCTION is_window_maximized() BIND(c, name='IsWindowMaximized')
    IMPORT :: C_BOOL
    IMPLICIT NONE
    LOGICAL(kind=C_BOOL) :: is_window_maximized
  END FUNCTION is_window_maximized

  ! bool IsWindowMinimized(void)
  FUNCTION is_window_minimized() BIND(c, name='IsWindowMinimized')
    IMPORT :: C_BOOL
    IMPLICIT NONE
    LOGICAL(kind=C_BOOL) :: is_window_minimized
  END FUNCTION is_window_minimized

  ! bool IsWindowReady(void)
  FUNCTION is_window_ready() BIND(c, name='IsWindowReady')
    IMPORT :: C_BOOL
    IMPLICIT NONE
    LOGICAL(kind=C_BOOL) :: is_window_ready
  END FUNCTION is_window_ready

  ! bool IsWindowResized(void)
  FUNCTION is_window_resized() BIND(c, name='IsWindowResized')
    IMPORT :: C_BOOL
    IMPLICIT NONE
    LOGICAL(kind=C_BOOL) :: is_window_resized
  END FUNCTION is_window_resized

  ! bool IsWindowState(unsigned int flag)
  FUNCTION is_window_state(flag) BIND(c, name='IsWindowState')
    IMPORT :: C_BOOL, c_unsigned_int
    IMPLICIT NONE
    INTEGER(kind=c_unsigned_int), INTENT(in), VALUE :: flag
    LOGICAL(kind=C_BOOL) :: is_window_state
  END FUNCTION is_window_state

  ! AudioStream LoadAudioStream(unsigned int sampleRate, unsigned int sampleSize, unsigned int channels)
        function load_audio_stream(sample_rate, sample_size, channels) bind(c, name='LoadAudioStream')
    IMPORT :: audio_stream_type, c_unsigned_int
    IMPLICIT NONE
    INTEGER(kind=c_unsigned_int), INTENT(in), VALUE :: sample_rate
    INTEGER(kind=c_unsigned_int), INTENT(in), VALUE :: sample_size
    INTEGER(kind=c_unsigned_int), INTENT(in), VALUE :: channels
    TYPE(audio_stream_type) :: load_audio_stream
  END FUNCTION load_audio_stream

  ! int *LoadCodepoints(const char *text, int *count)
  FUNCTION load_codepoints(text, count) BIND(c, name='LoadCodepoints')
    IMPORT :: C_CHAR, C_INT, C_PTR
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: text
    INTEGER(kind=C_INT), INTENT(out) :: count
    TYPE(C_PTR) :: load_codepoints
  END FUNCTION load_codepoints

  ! FilePathList LoadDirectoryFiles(const char *dirPath)
  FUNCTION load_directory_files(dir_path) BIND(c, name='LoadDirectoryFiles')
    IMPORT :: C_CHAR, file_path_list_type
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: dir_path
    TYPE(file_path_list_type) :: load_directory_files
  END FUNCTION load_directory_files

  ! FilePathList LoadDirectoryFilesEx(const char *basePath, const char *filter, bool scanSubdirs)
        function load_directory_files_ex(base_path, filter, scan_subdirs) bind(c, name='LoadDirectoryFilesEx')
    IMPORT :: C_BOOL, C_CHAR, file_path_list_type
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: base_path
    CHARACTER(kind=C_CHAR), INTENT(in) :: filter
    LOGICAL(kind=C_BOOL), INTENT(in), VALUE :: scan_subdirs
    TYPE(file_path_list_type) :: load_directory_files_ex
  END FUNCTION load_directory_files_ex

  ! FilePathList LoadDroppedFiles(void)
  FUNCTION load_dropped_files() BIND(c, name='LoadDroppedFiles')
    IMPORT :: file_path_list_type
    IMPLICIT NONE
    TYPE(file_path_list_type) :: load_dropped_files
  END FUNCTION load_dropped_files

  ! unsigned char *LoadFileData(const char *fileName, int *dataSize)
  FUNCTION load_file_data(file_name, data_size) BIND(c, name='LoadFileData')
    IMPORT :: C_CHAR, C_INT, C_PTR
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: file_name
    INTEGER(kind=C_INT), INTENT(out) :: data_size
    TYPE(C_PTR) :: load_file_data
  END FUNCTION load_file_data

  ! char *LoadFileText(const char *fileName)
  FUNCTION load_file_text(file_name) BIND(c, name='LoadFileText')
    IMPORT :: C_CHAR, C_PTR
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: file_name
    TYPE(C_PTR) :: load_file_text
  END FUNCTION load_file_text

  ! Font LoadFont(const char *fileName)
  FUNCTION load_font(file_name) BIND(c, name='LoadFont')
    IMPORT :: C_CHAR, font_type
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: file_name
    TYPE(font_type) :: load_font
  END FUNCTION load_font

  ! GlyphInfo *LoadFontData(const unsigned char *fileData, int dataSize, int fontSize, int *codepoints, int codepointsCount, int type)
        function load_font_data(file_data, data_size, font_size, codepoints, codepoints_count, type) &
    BIND(c, name='LoadFontData')
    IMPORT :: C_INT, C_PTR, c_unsigned_char, glyph_info_type
    IMPLICIT NONE
    INTEGER(kind=c_unsigned_char), INTENT(inout) :: file_data
    INTEGER(kind=C_INT), INTENT(in), VALUE :: data_size
    INTEGER(kind=C_INT), INTENT(in), VALUE :: font_size
    INTEGER(kind=C_INT), INTENT(inout) :: codepoints(*)
    INTEGER(kind=C_INT), INTENT(in), VALUE :: codepoints_count
    INTEGER(kind=C_INT), INTENT(in), VALUE :: TYPE
    TYPE(C_PTR) :: load_font_data
  END FUNCTION load_font_data

  ! Font LoadFontEx(const char *fileName, int fontSize, int *codepoints, int codepointsCount)
        function load_font_ex(file_name, font_size, codepoints, codepoints_count) bind(c, name='LoadFontEx')
    IMPORT :: C_CHAR, C_INT, font_type
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: file_name
    INTEGER(kind=C_INT), INTENT(in), VALUE :: font_size
    INTEGER(kind=C_INT), INTENT(inout) :: codepoints(*)
    INTEGER(kind=C_INT), INTENT(in), VALUE :: codepoints_count
    TYPE(font_type) :: load_font_ex
  END FUNCTION load_font_ex

  ! Font LoadFontFromImage(Image image, Color key, int firstChar)
        function load_font_from_image(image, key, first_char) bind(c, name='LoadFontFromImage')
    IMPORT :: C_INT, color_type, font_type, image_type
    IMPLICIT NONE
    TYPE(image_type), INTENT(in), VALUE :: image
    TYPE(color_type), INTENT(in), VALUE :: key
    INTEGER(kind=C_INT), INTENT(in), VALUE :: first_char
    TYPE(font_type) :: load_font_from_image
  END FUNCTION load_font_from_image

  ! Font LoadFontFromMemory(const char *fileType, const unsigned char *fileData, int dataSize, int fontSize, int *codepoints, int codepointsCount)
        function load_font_from_memory(file_type, file_data, data_size, font_size, codepoints, codepoints_count) &
    BIND(c, name='LoadFontFromMemory')
    IMPORT :: C_CHAR, C_INT, c_unsigned_char, font_type
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: file_type
    INTEGER(kind=c_unsigned_char), INTENT(in) :: file_data
    INTEGER(kind=C_INT), INTENT(in), VALUE :: data_size
    INTEGER(kind=C_INT), INTENT(in), VALUE :: font_size
    INTEGER(kind=C_INT), INTENT(inout) :: codepoints(*)
    INTEGER(kind=C_INT), INTENT(in), VALUE :: codepoints_count
    TYPE(font_type) :: load_font_from_memory
  END FUNCTION load_font_from_memory

  ! Image LoadImage(const char *fileName)
  FUNCTION load_image(file_name) BIND(c, name='LoadImage')
    IMPORT :: C_CHAR, image_type
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: file_name
    TYPE(image_type) :: load_image
  END FUNCTION load_image

  ! Image LoadImageAnim(const char *fileName, int *frames)
  FUNCTION load_image_anim(file_name, frames) BIND(c, name='LoadImageAnim')
    IMPORT :: C_CHAR, C_INT, image_type
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: file_name
    INTEGER(kind=C_INT), INTENT(out) :: frames
    TYPE(image_type) :: load_image_anim
  END FUNCTION load_image_anim

  ! Color *LoadImageColors(Image image)
  FUNCTION load_image_colors(image) BIND(c, name='LoadImageColors')
    IMPORT :: C_PTR, image_type
    IMPLICIT NONE
    TYPE(image_type), INTENT(in), VALUE :: image
    TYPE(C_PTR) :: load_image_colors
  END FUNCTION load_image_colors

  ! Image LoadImageFromMemory(const char *fileType, const unsigned char *fileData, int dataSize)
        function load_image_from_memory(file_type, file_data, data_size) bind(c, name='LoadImageFromMemory')
    IMPORT :: C_CHAR, C_INT, c_unsigned_char, image_type
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: file_type
    INTEGER(kind=c_unsigned_char), INTENT(in) :: file_data
    INTEGER(kind=C_INT), INTENT(in), VALUE :: data_size
    TYPE(image_type) :: load_image_from_memory
  END FUNCTION load_image_from_memory

  ! Image LoadImageFromScreen(void)
  FUNCTION load_image_from_screen() BIND(c, name='LoadImageFromScreen')
    IMPORT :: image_type
    IMPLICIT NONE
    TYPE(image_type) :: load_image_from_screen
  END FUNCTION load_image_from_screen

  ! Image LoadImageFromTexture(Texture2D texture)
FUNCTION load_image_from_texture(texture) BIND(c, name='LoadImageFromTexture')
    IMPORT :: image_type, texture2d_type
    IMPLICIT NONE
    TYPE(texture2d_type), INTENT(in), VALUE :: texture
    TYPE(image_type) :: load_image_from_texture
  END FUNCTION load_image_from_texture

  ! Color *LoadImagePalette(Image image, int maxPaletteSize, int *colorCount)
        function load_image_palette(image, max_palette_size, color_count) bind(c, name='LoadImagePalette')
    IMPORT :: C_INT, C_PTR, image_type
    IMPLICIT NONE
    TYPE(image_type), INTENT(in), VALUE :: image
    INTEGER(kind=C_INT), INTENT(in), VALUE :: max_palette_size
    INTEGER(kind=C_INT), INTENT(out) :: color_count
    TYPE(C_PTR) :: load_image_palette
  END FUNCTION load_image_palette

  ! Image LoadImageRaw(const char *fileName, int width, int height, int format, int headerSize)
        function load_image_raw(file_name, width, height, format, header_size) bind(c, name='LoadImageRaw')
    IMPORT :: C_CHAR, C_INT, image_type
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: file_name
    INTEGER(kind=C_INT), INTENT(in), VALUE :: width
    INTEGER(kind=C_INT), INTENT(in), VALUE :: height
    INTEGER(kind=C_INT), INTENT(in), VALUE :: FORMAT
    INTEGER(kind=C_INT), INTENT(in), VALUE :: header_size
    TYPE(image_type) :: load_image_raw
  END FUNCTION load_image_raw

  ! Image LoadImageSvg(const char *fileNameOrString, int width, int height)
        function load_image_svg(file_name_or_string, width, height) bind(c, name='LoadImageSvg')
    IMPORT :: C_CHAR, C_INT, image_type
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: file_name_or_string
    INTEGER(kind=C_INT), INTENT(in), VALUE :: width
    INTEGER(kind=C_INT), INTENT(in), VALUE :: height
    TYPE(image_type) :: load_image_svg
  END FUNCTION load_image_svg

  ! Material LoadMaterialDefault(void)
  FUNCTION load_material_default() BIND(c, name='LoadMaterialDefault')
    IMPORT :: material_type
    IMPLICIT NONE
    TYPE(material_type) :: load_material_default
  END FUNCTION load_material_default

  ! Material *LoadMaterials(const char *fileName, int *materialCount)
        function load_materials(file_name, material_count) bind(c, name='LoadMaterials')
    IMPORT :: C_CHAR, C_INT, C_PTR
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: file_name
    INTEGER(kind=C_INT), INTENT(out) :: material_count
    TYPE(C_PTR) :: load_materials
  END FUNCTION load_materials

  ! Model LoadModel(const char *fileName)
  FUNCTION load_model(file_name) BIND(c, name='LoadModel')
    IMPORT :: C_CHAR, model_type
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: file_name
    TYPE(model_type) :: load_model
  END FUNCTION load_model

  ! ModelAnimation *LoadModelAnimations(const char *fileName, int *animCount)
        function load_model_animations(file_name, anim_count) bind(c, name='LoadModelAnimations')
    IMPORT :: C_CHAR, C_INT, C_PTR
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: file_name
    INTEGER(kind=C_INT), INTENT(out) :: anim_count
    TYPE(C_PTR) :: load_model_animations
  END FUNCTION load_model_animations

  ! Model LoadModelFromMesh(Mesh mesh)
  FUNCTION load_model_from_mesh(mesh) BIND(c, name='LoadModelFromMesh')
    IMPORT :: mesh_type, model_type
    IMPLICIT NONE
    TYPE(mesh_type), INTENT(in), VALUE :: mesh
    TYPE(model_type) :: load_model_from_mesh
  END FUNCTION load_model_from_mesh

  ! Music LoadMusicStream(const char *fileName)
  FUNCTION load_music_stream(file_name) BIND(c, name='LoadMusicStream')
    IMPORT :: C_CHAR, music_type
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: file_name
    TYPE(music_type) :: load_music_stream
  END FUNCTION load_music_stream

  ! Music LoadMusicStreamFromMemory(const char *fileType, const unsigned char *data, int dataSize)
        function load_music_stream_from_memory(file_type, data, data_size) bind(c, name='LoadMusicStreamFromMemory')
    IMPORT :: C_CHAR, C_INT, c_unsigned_char, music_type
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: file_type
    INTEGER(kind=c_unsigned_char), INTENT(in) :: DATA
    INTEGER(kind=C_INT), INTENT(in), VALUE :: data_size
    TYPE(music_type) :: load_music_stream_from_memory
  END FUNCTION load_music_stream_from_memory

  ! int *LoadRandomSequence(unsigned int count, int min, int max)
        function load_random_sequence(count, min, max) bind(c, name='LoadRandomSequence')
    IMPORT :: C_INT, C_PTR, c_unsigned_int
    IMPLICIT NONE
    INTEGER(kind=c_unsigned_int), INTENT(in), VALUE :: count
    INTEGER(kind=C_INT), INTENT(in), VALUE :: min
    INTEGER(kind=C_INT), INTENT(in), VALUE :: max
    TYPE(C_PTR) :: load_random_sequence
  END FUNCTION load_random_sequence

  ! RenderTexture2D LoadRenderTexture(int width, int height)
 FUNCTION load_render_texture(width, height) BIND(c, name='LoadRenderTexture')
    IMPORT :: C_INT, render_texture2d_type
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: width
    INTEGER(kind=C_INT), INTENT(in), VALUE :: height
    TYPE(render_texture2d_type) :: load_render_texture
  END FUNCTION load_render_texture

  ! Shader LoadShader(const char *vsFileName, const char *fsFileName)
  FUNCTION load_shader(vs_file_name, fs_file_name) BIND(c, name='LoadShader')
    IMPORT :: C_CHAR, shader_type
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: vs_file_name
    CHARACTER(kind=C_CHAR), INTENT(in) :: fs_file_name
    TYPE(shader_type) :: load_shader
  END FUNCTION load_shader

  ! Shader LoadShaderFromMemory(const char *vsCode, const char *fsCode)
        function load_shader_from_memory(vs_code, fs_code) bind(c, name='LoadShaderFromMemory')
    IMPORT :: C_CHAR, shader_type
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: vs_code
    CHARACTER(kind=C_CHAR), INTENT(in) :: fs_code
    TYPE(shader_type) :: load_shader_from_memory
  END FUNCTION load_shader_from_memory

  ! Sound LoadSound(const char *fileName)
  FUNCTION load_sound(file_name) BIND(c, name='LoadSound')
    IMPORT :: C_CHAR, sound_type
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: file_name
    TYPE(sound_type) :: load_sound
  END FUNCTION load_sound

  ! Sound LoadSoundAlias(Sound source)
  FUNCTION load_sound_alias(source) BIND(c, name='LoadSoundAlias')
    IMPORT :: sound_type
    IMPLICIT NONE
    TYPE(sound_type), INTENT(in), VALUE :: source
    TYPE(sound_type) :: load_sound_alias
  END FUNCTION load_sound_alias

  ! Sound LoadSoundFromWave(Wave wave)
  FUNCTION load_sound_from_wave(wave) BIND(c, name='LoadSoundFromWave')
    IMPORT :: sound_type, wave_type
    IMPLICIT NONE
    TYPE(wave_type), INTENT(in), VALUE :: wave
    TYPE(sound_type) :: load_sound_from_wave
  END FUNCTION load_sound_from_wave

  ! Texture2D LoadTexture(const char *fileName)
  FUNCTION load_texture(file_name) BIND(c, name='LoadTexture')
    IMPORT :: C_CHAR, texture2d_type
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: file_name
    TYPE(texture2d_type) :: load_texture
  END FUNCTION load_texture

  ! TextureCubemap LoadTextureCubemap(Image image, int layout)
        function load_texture_cubemap(image, layout) bind(c, name='LoadTextureCubemap')
    IMPORT :: C_INT, image_type, texture_cubemap_type
    IMPLICIT NONE
    TYPE(image_type), INTENT(in), VALUE :: image
    INTEGER(kind=C_INT), INTENT(in), VALUE :: layout
    TYPE(texture_cubemap_type) :: load_texture_cubemap
  END FUNCTION load_texture_cubemap

  ! Texture2D LoadTextureFromImage(Image image)
  FUNCTION load_texture_from_image(image) BIND(c, name='LoadTextureFromImage')
    IMPORT :: image_type, texture2d_type
    IMPLICIT NONE
    TYPE(image_type), INTENT(in), VALUE :: image
    TYPE(texture2d_type) :: load_texture_from_image
  END FUNCTION load_texture_from_image

  ! char *LoadUTF8(const int *codepoints, int length)
  FUNCTION load_utf8(codepoints, length) BIND(c, name='LoadUTF8')
    IMPORT :: C_INT, C_PTR
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(out) :: codepoints(*)
    INTEGER(kind=C_INT), INTENT(in), VALUE :: length
    TYPE(C_PTR) :: load_utf8
  END FUNCTION load_utf8

  ! VrStereoConfig LoadVrStereoConfig(VrDeviceInfo device)
  FUNCTION load_vr_stereo_config(device) BIND(c, name='LoadVrStereoConfig')
    IMPORT :: vr_device_info_type, vr_stereo_config_type
    IMPLICIT NONE
    TYPE(vr_device_info_type), INTENT(in), VALUE :: device
    TYPE(vr_stereo_config_type) :: load_vr_stereo_config
  END FUNCTION load_vr_stereo_config

  ! Wave LoadWave(const char *fileName)
  FUNCTION load_wave(file_name) BIND(c, name='LoadWave')
    IMPORT :: C_CHAR, wave_type
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: file_name
    TYPE(wave_type) :: load_wave
  END FUNCTION load_wave

  ! Wave LoadWaveFromMemory(const char *fileType, const unsigned char *fileData, int dataSize)
        function load_wave_from_memory(file_type, file_data, data_size) bind(c, name='LoadWaveFromMemory')
    IMPORT :: C_CHAR, C_INT, c_unsigned_char, wave_type
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: file_type
    INTEGER(kind=c_unsigned_char), INTENT(in) :: file_data
    INTEGER(kind=C_INT), INTENT(in), VALUE :: data_size
    TYPE(wave_type) :: load_wave_from_memory
  END FUNCTION load_wave_from_memory

  ! float *LoadWaveSamples(Wave wave)
  FUNCTION load_wave_samples(wave) BIND(c, name='LoadWaveSamples')
    IMPORT :: C_PTR, wave_type
    IMPLICIT NONE
    TYPE(wave_type), INTENT(in), VALUE :: wave
    TYPE(C_PTR) :: load_wave_samples
  END FUNCTION load_wave_samples

  ! void MaximizeWindow(void)
  SUBROUTINE maximize_window() BIND(c, name='MaximizeWindow')
  END SUBROUTINE maximize_window

  ! int MeasureText(const char *text, int fontSize)
  FUNCTION measure_text(text, font_size) BIND(c, name='MeasureText')
    IMPORT :: C_CHAR, C_INT
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: text
    INTEGER(kind=C_INT), INTENT(in), VALUE :: font_size
    INTEGER(kind=C_INT) :: measure_text
  END FUNCTION measure_text

  ! Vector2 MeasureTextEx(Font font, const char *text, float fontSize, float spacing)
        function measure_text_ex(font, text, font_size, spacing) bind(c, name='MeasureTextEx')
    IMPORT :: C_CHAR, C_FLOAT, font_type, vector2_type
    IMPLICIT NONE
    TYPE(font_type), INTENT(in), VALUE :: font
    CHARACTER(kind=C_CHAR), INTENT(in) :: text
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: font_size
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: spacing
    TYPE(vector2_type) :: measure_text_ex
  END FUNCTION measure_text_ex

  ! void *MemAlloc(unsigned int size)
  FUNCTION mem_alloc(size) BIND(c, name='MemAlloc')
    IMPORT :: C_PTR, c_unsigned_int
    IMPLICIT NONE
    INTEGER(kind=c_unsigned_int), INTENT(in), VALUE :: size
    TYPE(C_PTR) :: mem_alloc
  END FUNCTION mem_alloc

  ! void MemFree(void *ptr)
  SUBROUTINE mem_free(ptr) BIND(c, name='MemFree')
    IMPORT :: C_PTR
    IMPLICIT NONE
    TYPE(C_PTR), INTENT(in), VALUE :: ptr
  END SUBROUTINE mem_free

  ! void *MemRealloc(void *ptr, unsigned int size)
  FUNCTION mem_realloc(ptr, size) BIND(c, name='MemRealloc')
    IMPORT :: C_PTR, c_unsigned_int
    IMPLICIT NONE
    TYPE(C_PTR), INTENT(in), VALUE :: ptr
    INTEGER(kind=c_unsigned_int), INTENT(in), VALUE :: size
    TYPE(C_PTR) :: mem_realloc
  END FUNCTION mem_realloc

  ! void MinimizeWindow(void)
  SUBROUTINE minimize_window() BIND(c, name='MinimizeWindow')
  END SUBROUTINE minimize_window

  ! void OpenURL(const char *url)
  SUBROUTINE open_url(url) BIND(c, name='OpenURL')
    IMPORT :: C_CHAR
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: url
  END SUBROUTINE open_url

  ! void PauseAudioStream(AudioStream stream)
  SUBROUTINE pause_audio_stream(stream) BIND(c, name='PauseAudioStream')
    IMPORT :: audio_stream_type
    IMPLICIT NONE
    TYPE(audio_stream_type), INTENT(in), VALUE :: stream
  END SUBROUTINE pause_audio_stream

  ! void PauseMusicStream(Music music)
  SUBROUTINE pause_music_stream(music) BIND(c, name='PauseMusicStream')
    IMPORT :: music_type
    IMPLICIT NONE
    TYPE(music_type), INTENT(in), VALUE :: music
  END SUBROUTINE pause_music_stream

  ! void PauseSound(Sound sound)
  SUBROUTINE pause_sound(sound) BIND(c, name='PauseSound')
    IMPORT :: sound_type
    IMPLICIT NONE
    TYPE(sound_type), INTENT(in), VALUE :: sound
  END SUBROUTINE pause_sound

  ! void PlayAudioStream(AudioStream stream)
  SUBROUTINE play_audio_stream(stream) BIND(c, name='PlayAudioStream')
    IMPORT :: audio_stream_type
    IMPLICIT NONE
    TYPE(audio_stream_type), INTENT(in), VALUE :: stream
  END SUBROUTINE play_audio_stream

  ! void PlayMusicStream(Music music)
  SUBROUTINE play_music_stream(music) BIND(c, name='PlayMusicStream')
    IMPORT :: music_type
    IMPLICIT NONE
    TYPE(music_type), INTENT(in), VALUE :: music
  END SUBROUTINE play_music_stream

  ! void PlaySound(Sound sound)
  SUBROUTINE play_sound(sound) BIND(c, name='PlaySound')
    IMPORT :: sound_type
    IMPLICIT NONE
    TYPE(sound_type), INTENT(in), VALUE :: sound
  END SUBROUTINE play_sound

  ! void PollInputEvents(void)
  SUBROUTINE poll_input_events() BIND(c, name='PollInputEvents')
  END SUBROUTINE poll_input_events

  ! void RestoreWindow(void)
  SUBROUTINE restore_window() BIND(c, name='RestoreWindow')
  END SUBROUTINE restore_window

  ! void ResumeAudioStream(AudioStream stream)
  SUBROUTINE resume_audio_stream(stream) BIND(c, name='ResumeAudioStream')
    IMPORT :: audio_stream_type
    IMPLICIT NONE
    TYPE(audio_stream_type), INTENT(in), VALUE :: stream
  END SUBROUTINE resume_audio_stream

  ! void ResumeMusicStream(Music music)
  SUBROUTINE resume_music_stream(music) BIND(c, name='ResumeMusicStream')
    IMPORT :: music_type
    IMPLICIT NONE
    TYPE(music_type), INTENT(in), VALUE :: music
  END SUBROUTINE resume_music_stream

  ! void ResumeSound(Sound sound)
  SUBROUTINE resume_sound(sound) BIND(c, name='ResumeSound')
    IMPORT :: sound_type
    IMPLICIT NONE
    TYPE(sound_type), INTENT(in), VALUE :: sound
  END SUBROUTINE resume_sound

  ! bool SaveFileData(const char *fileName, void *data, int dataSize)
        function save_file_data(file_name, data, data_size) bind(c, name='SaveFileData')
    IMPORT :: C_BOOL, C_CHAR, C_INT, C_PTR
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: file_name
    TYPE(C_PTR), INTENT(in), VALUE :: DATA
    INTEGER(kind=C_INT), INTENT(in), VALUE :: data_size
    LOGICAL(kind=C_BOOL) :: save_file_data
  END FUNCTION save_file_data

  ! bool SaveFileText(const char *fileName, char *text)
  FUNCTION save_file_text(file_name, text) BIND(c, name='SaveFileText')
    IMPORT :: C_BOOL, C_CHAR
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: file_name
    CHARACTER(kind=C_CHAR), INTENT(in) :: text
    LOGICAL(kind=C_BOOL) :: save_file_text
  END FUNCTION save_file_text

  ! void SeekMusicStream(Music music, float position)
 SUBROUTINE seek_music_stream(music, position) BIND(c, name='SeekMusicStream')
    IMPORT :: C_FLOAT, music_type
    IMPLICIT NONE
    TYPE(music_type), INTENT(in), VALUE :: music
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: position
  END SUBROUTINE seek_music_stream

  ! void ShowCursor(void)
  SUBROUTINE show_cursor() BIND(c, name='ShowCursor')
  END SUBROUTINE show_cursor

  ! void StopAudioStream(AudioStream stream)
  SUBROUTINE stop_audio_stream(stream) BIND(c, name='StopAudioStream')
    IMPORT :: audio_stream_type
    IMPLICIT NONE
    TYPE(audio_stream_type), INTENT(in), VALUE :: stream
  END SUBROUTINE stop_audio_stream

  ! void StopMusicStream(Music music)
  SUBROUTINE stop_music_stream(music) BIND(c, name='StopMusicStream')
    IMPORT :: music_type
    IMPLICIT NONE
    TYPE(music_type), INTENT(in), VALUE :: music
  END SUBROUTINE stop_music_stream

  ! void StopSound(Sound sound)
  SUBROUTINE stop_sound(sound) BIND(c, name='StopSound')
    IMPORT :: sound_type
    IMPLICIT NONE
    TYPE(sound_type), INTENT(in), VALUE :: sound
  END SUBROUTINE stop_sound

  ! void SwapScreenBuffer(void)
  SUBROUTINE swap_screen_buffer() BIND(c, name='SwapScreenBuffer')
  END SUBROUTINE swap_screen_buffer

  ! void TakeScreenshot(const char *fileName)
  SUBROUTINE take_screenshot(file_name) BIND(c, name='TakeScreenshot')
    IMPORT :: C_CHAR
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: file_name
  END SUBROUTINE take_screenshot

  ! void TextAppend(char *text, const char *append, int *position)
  SUBROUTINE text_append(text, append, position) BIND(c, name='TextAppend')
    IMPORT :: C_CHAR, C_INT
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: text
    CHARACTER(kind=C_CHAR), INTENT(in) :: append
    INTEGER(kind=C_INT), INTENT(in) :: position
  END SUBROUTINE text_append

  ! int TextCopy(char *dst, const char *src)
  FUNCTION text_copy(dst, src) BIND(c, name='TextCopy')
    IMPORT :: C_CHAR, C_INT
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: dst
    CHARACTER(kind=C_CHAR), INTENT(in) :: src
    INTEGER(kind=C_INT) :: text_copy
  END FUNCTION text_copy

  ! int TextFindIndex(const char *text, const char *find)
  FUNCTION text_find_index(text, find) BIND(c, name='TextFindIndex')
    IMPORT :: C_CHAR, C_INT
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: text
    CHARACTER(kind=C_CHAR), INTENT(in) :: find
    INTEGER(kind=C_INT) :: text_find_index
  END FUNCTION text_find_index

  ! char *TextInsert(const char *text, const char *insert, int position)
  FUNCTION text_insert(text, insert, position) BIND(c, name='TextInsert')
    IMPORT :: C_CHAR, C_INT, C_PTR
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: text
    CHARACTER(kind=C_CHAR), INTENT(in) :: insert
    INTEGER(kind=C_INT), INTENT(in), VALUE :: position
    TYPE(C_PTR) :: text_insert
  END FUNCTION text_insert

  ! bool TextIsEqual(const char *text1, const char *text2)
  FUNCTION text_is_equal(text1, text2) BIND(c, name='TextIsEqual')
    IMPORT :: C_BOOL, C_CHAR
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: text1
    CHARACTER(kind=C_CHAR), INTENT(in) :: text2
    LOGICAL(kind=C_BOOL) :: text_is_equal
  END FUNCTION text_is_equal

  ! const char *TextJoin(const char **textList, int count, const char *delimiter)
  FUNCTION text_join(text_list, count, delimiter) BIND(c, name='TextJoin')
    IMPORT :: C_CHAR, C_INT, C_PTR
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: text_list(*)
    INTEGER(kind=C_INT), INTENT(in), VALUE :: count
    CHARACTER(kind=C_CHAR), INTENT(in) :: delimiter
    TYPE(C_PTR) :: text_join
  END FUNCTION text_join

  ! unsigned int TextLength(const char *text)
  FUNCTION text_length(text) BIND(c, name='TextLength')
    IMPORT :: C_CHAR, c_unsigned_int
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: text
    INTEGER(kind=c_unsigned_int) :: text_length
  END FUNCTION text_length

  ! char *TextReplace(char *text, const char *replace, const char *by)
  FUNCTION text_replace(text, replace, by) BIND(c, name='TextReplace')
    IMPORT :: C_CHAR, C_PTR
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: text
    CHARACTER(kind=C_CHAR), INTENT(in) :: replace
    CHARACTER(kind=C_CHAR), INTENT(in) :: by
    TYPE(C_PTR) :: text_replace
  END FUNCTION text_replace

  ! const char **TextSplit(const char *text, char delimiter, int *count)
  FUNCTION text_split(text, delimiter, count) BIND(c, name='TextSplit')
    IMPORT :: C_CHAR, C_INT, C_PTR
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: text
    CHARACTER(kind=C_CHAR), INTENT(in), VALUE :: delimiter
    INTEGER(kind=C_INT), INTENT(out) :: count
    TYPE(C_PTR) :: text_split
  END FUNCTION text_split

  ! const char *TextSubtext(const char *text, int position, int length)
  FUNCTION text_subtext(text, position, length) BIND(c, name='TextSubtext')
    IMPORT :: C_CHAR, C_INT, C_PTR
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: text
    INTEGER(kind=C_INT), INTENT(in), VALUE :: position
    INTEGER(kind=C_INT), INTENT(in), VALUE :: length
    TYPE(C_PTR) :: text_subtext
  END FUNCTION text_subtext

  ! int TextToInteger(const char *text)
  FUNCTION text_to_integer(text) BIND(c, name='TextToInteger')
    IMPORT :: C_CHAR, C_INT
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: text
    INTEGER(kind=C_INT) :: text_to_integer
  END FUNCTION text_to_integer

  ! const char *TextToLower(const char *text)
  FUNCTION text_to_lower(text) BIND(c, name='TextToLower')
    IMPORT :: C_CHAR, C_PTR
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: text
    TYPE(C_PTR) :: text_to_lower
  END FUNCTION text_to_lower

  ! const char *TextToPascal(const char *text)
  FUNCTION text_to_pascal(text) BIND(c, name='TextToPascal')
    IMPORT :: C_CHAR, C_PTR
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: text
    TYPE(C_PTR) :: text_to_pascal
  END FUNCTION text_to_pascal

  ! const char *TextToUpper(const char *text)
  FUNCTION text_to_upper(text) BIND(c, name='TextToUpper')
    IMPORT :: C_CHAR, C_PTR
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: text
    TYPE(C_PTR) :: text_to_upper
  END FUNCTION text_to_upper

  ! void ToggleBorderlessWindowed(void)
        subroutine toggle_borderless_windowed() bind(c, name='ToggleBorderlessWindowed')
  END SUBROUTINE toggle_borderless_windowed

  ! void ToggleFullscreen(void)
  SUBROUTINE toggle_fullscreen() BIND(c, name='ToggleFullscreen')
  END SUBROUTINE toggle_fullscreen

  ! void TraceLog(int logLevel, const char *text)
  SUBROUTINE trace_log(log_level, text) BIND(c, name='TraceLog')
    IMPORT :: C_CHAR, C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: log_level
    CHARACTER(kind=C_CHAR), INTENT(in) :: text
  END SUBROUTINE trace_log

  ! void UnloadAudioStream(AudioStream stream)
  SUBROUTINE unload_audio_stream(stream) BIND(c, name='UnloadAudioStream')
    IMPORT :: audio_stream_type
    IMPLICIT NONE
    TYPE(audio_stream_type), INTENT(in), VALUE :: stream
  END SUBROUTINE unload_audio_stream

  ! void UnloadCodepoints(int *codepoints)
  SUBROUTINE unload_codepoints(codepoints) BIND(c, name='UnloadCodepoints')
    IMPORT :: C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(inout) :: codepoints(*)
  END SUBROUTINE unload_codepoints

  ! void UnloadDirectoryFiles(FilePathList files)
 SUBROUTINE unload_directory_files(files) BIND(c, name='UnloadDirectoryFiles')
    IMPORT :: file_path_list_type
    IMPLICIT NONE
    TYPE(file_path_list_type), INTENT(in), VALUE :: files
  END SUBROUTINE unload_directory_files

  ! void UnloadDroppedFiles(FilePathList files)
  SUBROUTINE unload_dropped_files(files) BIND(c, name='UnloadDroppedFiles')
    IMPORT :: file_path_list_type
    IMPLICIT NONE
    TYPE(file_path_list_type), INTENT(in), VALUE :: files
  END SUBROUTINE unload_dropped_files

  ! void UnloadFileData(unsigned char *data)
  SUBROUTINE unload_file_data(DATA) BIND(c, name='UnloadFileData')
    IMPORT :: c_unsigned_char
    IMPLICIT NONE
    INTEGER(kind=c_unsigned_char), INTENT(in) :: DATA
  END SUBROUTINE unload_file_data

  ! void UnloadFileText(char *text)
  SUBROUTINE unload_file_text(text) BIND(c, name='UnloadFileText')
    IMPORT :: C_CHAR
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: text
  END SUBROUTINE unload_file_text

  ! void UnloadFont(Font font)
  SUBROUTINE unload_font(font) BIND(c, name='UnloadFont')
    IMPORT :: font_type
    IMPLICIT NONE
    TYPE(font_type), INTENT(in), VALUE :: font
  END SUBROUTINE unload_font

  ! void UnloadFontData(GlyphInfo *glyphs, int glyphCount)
        subroutine unload_font_data(glyphs, glyph_count) bind(c, name='UnloadFontData')
    IMPORT :: C_INT, glyph_info_type
    IMPLICIT NONE
    TYPE(glyph_info_type), INTENT(inout) :: glyphs
    INTEGER(kind=C_INT), INTENT(in), VALUE :: glyph_count
  END SUBROUTINE unload_font_data

  ! void UnloadImage(Image image)
  SUBROUTINE unload_image(image) BIND(c, name='UnloadImage')
    IMPORT :: image_type
    IMPLICIT NONE
    TYPE(image_type), INTENT(in), VALUE :: image
  END SUBROUTINE unload_image

  ! void UnloadImageColors(Color *colors)
  SUBROUTINE unload_image_colors(colors) BIND(c, name='UnloadImageColors')
    IMPORT :: color_type
    IMPLICIT NONE
    TYPE(color_type), INTENT(inout) :: colors(*)
  END SUBROUTINE unload_image_colors

  ! void UnloadImagePalette(Color *colors)
  SUBROUTINE unload_image_palette(colors) BIND(c, name='UnloadImagePalette')
    IMPORT :: color_type
    IMPLICIT NONE
    TYPE(color_type), INTENT(inout) :: colors(*)
  END SUBROUTINE unload_image_palette

  ! void UnloadMaterial(Material material)
  SUBROUTINE unload_material(material) BIND(c, name='UnloadMaterial')
    IMPORT :: material_type
    IMPLICIT NONE
    TYPE(material_type), INTENT(in), VALUE :: material
  END SUBROUTINE unload_material

  ! void UnloadMesh(Mesh mesh)
  SUBROUTINE unload_mesh(mesh) BIND(c, name='UnloadMesh')
    IMPORT :: mesh_type
    IMPLICIT NONE
    TYPE(mesh_type), INTENT(in), VALUE :: mesh
  END SUBROUTINE unload_mesh

  ! void UnloadModel(Model model)
  SUBROUTINE unload_model(model) BIND(c, name='UnloadModel')
    IMPORT :: model_type
    IMPLICIT NONE
    TYPE(model_type), INTENT(in), VALUE :: model
  END SUBROUTINE unload_model

  ! void UnloadModelAnimation(ModelAnimation anim)
  SUBROUTINE unload_model_animation(anim) BIND(c, name='UnloadModelAnimation')
    IMPORT :: model_animation_type
    IMPLICIT NONE
    TYPE(model_animation_type), INTENT(in), VALUE :: anim
  END SUBROUTINE unload_model_animation

  ! void UnloadModelAnimations(ModelAnimation *animations, int count)
        subroutine unload_model_animations(animations, count) bind(c, name='UnloadModelAnimations')
    IMPORT :: C_INT, model_animation_type
    IMPLICIT NONE
    TYPE(model_animation_type), INTENT(inout) :: animations(*)
    INTEGER(kind=C_INT), INTENT(in), VALUE :: count
  END SUBROUTINE unload_model_animations

  ! void UnloadMusicStream(Music music)
  SUBROUTINE unload_music_stream(music) BIND(c, name='UnloadMusicStream')
    IMPORT :: music_type
    IMPLICIT NONE
    TYPE(music_type), INTENT(in), VALUE :: music
  END SUBROUTINE unload_music_stream

  ! void UnloadRandomSequence(int *sequence)
        subroutine unload_random_sequence(sequence) bind(c, name='UnloadRandomSequence')
    IMPORT :: C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(inout) :: SEQUENCE(*)
  END SUBROUTINE unload_random_sequence

  ! void UnloadRenderTexture(RenderTexture2D target)
  SUBROUTINE unload_render_texture(TARGET) BIND(c, name='UnloadRenderTexture')
    IMPORT :: render_texture2d_type
    IMPLICIT NONE
    TYPE(render_texture2d_type), INTENT(in), VALUE :: TARGET
  END SUBROUTINE unload_render_texture

  ! void UnloadShader(Shader shader)
  SUBROUTINE unload_shader(shader) BIND(c, name='UnloadShader')
    IMPORT :: shader_type
    IMPLICIT NONE
    TYPE(shader_type), INTENT(in), VALUE :: shader
  END SUBROUTINE unload_shader

  ! void UnloadSound(Sound sound)
  SUBROUTINE unload_sound(sound) BIND(c, name='UnloadSound')
    IMPORT :: sound_type
    IMPLICIT NONE
    TYPE(sound_type), INTENT(in), VALUE :: sound
  END SUBROUTINE unload_sound

  ! void UnloadSoundAlias(Sound alias)
  SUBROUTINE unload_sound_alias(alias) BIND(c, name='UnloadSoundAlias')
    IMPORT :: sound_type
    IMPLICIT NONE
    TYPE(sound_type), INTENT(in), VALUE :: alias
  END SUBROUTINE unload_sound_alias

  ! void UnloadTexture(Texture2D texture)
  SUBROUTINE unload_texture(texture) BIND(c, name='UnloadTexture')
    IMPORT :: texture2d_type
    IMPLICIT NONE
    TYPE(texture2d_type), INTENT(in), VALUE :: texture
  END SUBROUTINE unload_texture

  ! void UnloadUTF8(char *text)
  SUBROUTINE unload_utf8(text) BIND(c, name='UnloadUTF8')
    IMPORT :: C_CHAR
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: text
  END SUBROUTINE unload_utf8

  ! void UnloadVrStereoConfig(VrStereoConfig config)
        subroutine unload_vr_stereo_config(config) bind(c, name='UnloadVrStereoConfig')
    IMPORT :: vr_stereo_config_type
    IMPLICIT NONE
    TYPE(vr_stereo_config_type), INTENT(in), VALUE :: config
  END SUBROUTINE unload_vr_stereo_config

  ! void UnloadWave(Wave wave)
  SUBROUTINE unload_wave(wave) BIND(c, name='UnloadWave')
    IMPORT :: wave_type
    IMPLICIT NONE
    TYPE(wave_type), INTENT(in), VALUE :: wave
  END SUBROUTINE unload_wave

  ! void UnloadWaveSamples(float *samples)
  SUBROUTINE unload_wave_samples(samples) BIND(c, name='UnloadWaveSamples')
    IMPORT :: C_FLOAT
    IMPLICIT NONE
    REAL(kind=C_FLOAT), INTENT(inout) :: samples(*)
  END SUBROUTINE unload_wave_samples

  ! void UpdateAudioStream(AudioStream stream, const void *data, int frameCount)
        subroutine update_audio_stream(stream, data, frame_count) bind(c, name='UpdateAudioStream')
    IMPORT :: audio_stream_type, C_INT, C_PTR
    IMPLICIT NONE
    TYPE(audio_stream_type), INTENT(in), VALUE :: stream
    TYPE(C_PTR), INTENT(in), VALUE :: DATA
    INTEGER(kind=C_INT), INTENT(in), VALUE :: frame_count
  END SUBROUTINE update_audio_stream

  ! void UpdateCamera(Camera *camera, int mode)
  SUBROUTINE update_camera(camera, mode) BIND(c, name='UpdateCamera')
    IMPORT :: camera3d_type, C_INT
    IMPLICIT NONE
    TYPE(camera3d_type), INTENT(inout) :: camera
    INTEGER(kind=C_INT), INTENT(in), VALUE :: mode
  END SUBROUTINE update_camera

  ! void UpdateMeshBuffer(Mesh mesh, int index, const void *data, int dataSize, int offset)
        subroutine update_mesh_buffer(mesh, index, data, data_size, offset) bind(c, name='UpdateMeshBuffer')
    IMPORT :: C_INT, C_PTR, mesh_type
    IMPLICIT NONE
    TYPE(mesh_type), INTENT(in), VALUE :: mesh
    INTEGER(kind=C_INT), INTENT(in), VALUE :: index
    TYPE(C_PTR), INTENT(in), VALUE :: DATA
    INTEGER(kind=C_INT), INTENT(in), VALUE :: data_size
    INTEGER(kind=C_INT), INTENT(in), VALUE :: offset
  END SUBROUTINE update_mesh_buffer

  ! void UpdateModelAnimation(Model model, ModelAnimation anim, int frame)
        subroutine update_model_animation(model, anim, frame) bind(c, name='UpdateModelAnimation')
    IMPORT :: C_INT, model_animation_type, model_type
    IMPLICIT NONE
    TYPE(model_type), INTENT(in), VALUE :: model
    TYPE(model_animation_type), INTENT(in), VALUE :: anim
    INTEGER(kind=C_INT), INTENT(in), VALUE :: frame
  END SUBROUTINE update_model_animation

  ! void UpdateMusicStream(Music music)
  SUBROUTINE update_music_stream(music) BIND(c, name='UpdateMusicStream')
    IMPORT :: music_type
    IMPLICIT NONE
    TYPE(music_type), INTENT(in), VALUE :: music
  END SUBROUTINE update_music_stream

  ! void UpdateSound(Sound sound, const void *data, int sampleCount)
SUBROUTINE update_sound(sound, DATA, sample_count) BIND(c, name='UpdateSound')
    IMPORT :: C_INT, C_PTR, sound_type
    IMPLICIT NONE
    TYPE(sound_type), INTENT(in), VALUE :: sound
    TYPE(C_PTR), INTENT(in), VALUE :: DATA
    INTEGER(kind=C_INT), INTENT(in), VALUE :: sample_count
  END SUBROUTINE update_sound

  ! void UpdateTexture(Texture2D texture, const void *pixels)
  SUBROUTINE update_texture(texture, pixels) BIND(c, name='UpdateTexture')
    IMPORT :: C_PTR, texture2d_type
    IMPLICIT NONE
    TYPE(texture2d_type), INTENT(in), VALUE :: texture
    TYPE(C_PTR), INTENT(in), VALUE :: pixels
  END SUBROUTINE update_texture

  ! void UpdateTextureRec(Texture2D texture, Rectangle rec, const void *pixels)
        subroutine update_texture_rec(texture, rec, pixels) bind(c, name='UpdateTextureRec')
    IMPORT :: C_PTR, rectangle_type, texture2d_type
    IMPLICIT NONE
    TYPE(texture2d_type), INTENT(in), VALUE :: texture
    TYPE(rectangle_type), INTENT(in), VALUE :: rec
    TYPE(C_PTR), INTENT(in), VALUE :: pixels
  END SUBROUTINE update_texture_rec

  ! void UploadMesh(Mesh *mesh, bool dynamic)
  SUBROUTINE upload_mesh(mesh, dynamic) BIND(c, name='UploadMesh')
    IMPORT :: C_BOOL, mesh_type
    IMPLICIT NONE
    TYPE(mesh_type), INTENT(inout) :: mesh
    LOGICAL(kind=C_BOOL), INTENT(in), VALUE :: dynamic
  END SUBROUTINE upload_mesh

  ! void WaitTime(double seconds)
  SUBROUTINE wait_time(seconds) BIND(c, name='WaitTime')
    IMPORT :: C_DOUBLE
    IMPLICIT NONE
    REAL(kind=C_DOUBLE), INTENT(in), VALUE :: seconds
  END SUBROUTINE wait_time

  ! Wave WaveCopy(Wave wave)
  FUNCTION wave_copy(wave) BIND(c, name='WaveCopy')
    IMPORT :: wave_type
    IMPLICIT NONE
    TYPE(wave_type), INTENT(in), VALUE :: wave
    TYPE(wave_type) :: wave_copy
  END FUNCTION wave_copy

  ! void WaveCrop(Wave *wave, int initSample, int finalSample)
SUBROUTINE wave_crop(wave, init_sample, final_sample) BIND(c, name='WaveCrop')
    IMPORT :: C_INT, wave_type
    IMPLICIT NONE
    TYPE(wave_type), INTENT(in) :: wave
    INTEGER(kind=C_INT), INTENT(in), VALUE :: init_sample
    INTEGER(kind=C_INT), INTENT(in), VALUE :: final_sample
  END SUBROUTINE wave_crop

  ! void WaveFormat(Wave *wave, int sampleRate, int sampleSize, int channels)
        subroutine wave_format(wave, sample_rate, sample_size, channels) bind(c, name='WaveFormat')
    IMPORT :: C_INT, wave_type
    IMPLICIT NONE
    TYPE(wave_type), INTENT(in) :: wave
    INTEGER(kind=C_INT), INTENT(in), VALUE :: sample_rate
    INTEGER(kind=C_INT), INTENT(in), VALUE :: sample_size
    INTEGER(kind=C_INT), INTENT(in), VALUE :: channels
  END SUBROUTINE wave_format

  ! bool WindowShouldClose(void)
  FUNCTION window_should_close() BIND(c, name='WindowShouldClose')
    IMPORT :: C_BOOL
    IMPLICIT NONE
    LOGICAL(kind=C_BOOL) :: window_should_close
  END FUNCTION window_should_close
END INTERFACE
CONTAINS
ELEMENTAL REAL FUNCTION deg2rad(d) RESULT(r)
  REAL, INTENT(in) :: d

  r = d * (PI / 180.0)
END FUNCTION deg2rad

ELEMENTAL REAL FUNCTION rad2deg(r) RESULT(d)
  REAL, INTENT(in) :: r

  d = r * (180.0 / PI)
END FUNCTION rad2deg
END MODULE RaylibMethods
