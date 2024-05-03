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
PUBLIC :: gen_image_cellular
PUBLIC :: gen_image_checked
PUBLIC :: gen_image_color
PUBLIC :: gen_image_font_atlas
PUBLIC :: gen_image_gradient_linear
PUBLIC :: gen_image_gradient_radial
PUBLIC :: gen_image_gradient_square
PUBLIC :: gen_image_perlin_noise
PUBLIC :: gen_image_text
PUBLIC :: gen_image_white_noise
PUBLIC :: gen_mesh_cone
PUBLIC :: gen_mesh_cube
PUBLIC :: gen_mesh_cubicmap
PUBLIC :: gen_mesh_cylinder
PUBLIC :: gen_mesh_heightmap
PUBLIC :: gen_mesh_hemi_sphere
PUBLIC :: gen_mesh_knot
PUBLIC :: gen_mesh_plane
PUBLIC :: gen_mesh_poly
PUBLIC :: gen_mesh_sphere
PUBLIC :: gen_mesh_tangents
PUBLIC :: gen_mesh_torus
PUBLIC :: gen_texture_mipmaps
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
PUBLIC :: hide_cursor
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
PUBLIC :: set_audio_stream_buffer_size_default
PUBLIC :: set_audio_stream_pan
PUBLIC :: set_audio_stream_pitch
PUBLIC :: set_audio_stream_volume
PUBLIC :: set_camera_alt_control
PUBLIC :: set_camera_mode
PUBLIC :: set_camera_move_controls
PUBLIC :: set_camera_pan_control
PUBLIC :: set_camera_smooth_zoom_control
PUBLIC :: set_clipboard_text
PUBLIC :: set_config_flags
PUBLIC :: set_exit_key
PUBLIC :: set_gamepad_mappings
PUBLIC :: set_gestures_enabled
PUBLIC :: set_load_file_data_callback
PUBLIC :: set_load_file_text_callback
PUBLIC :: set_master_volume
PUBLIC :: set_material_texture
PUBLIC :: set_model_mesh_material
PUBLIC :: set_mouse_cursor
PUBLIC :: set_mouse_offset
PUBLIC :: set_mouse_position
PUBLIC :: set_mouse_scale
PUBLIC :: set_music_pan
PUBLIC :: set_music_pitch
PUBLIC :: set_music_volume
PUBLIC :: set_pixel_color
PUBLIC :: set_random_seed
PUBLIC :: set_save_file_data_callback
PUBLIC :: set_save_file_text_callback
PUBLIC :: set_shader_value
PUBLIC :: set_shader_value_matrix
PUBLIC :: set_shader_value_texture
PUBLIC :: set_shader_value_v
PUBLIC :: set_shapes_texture
PUBLIC :: set_sound_pan
PUBLIC :: set_sound_pitch
PUBLIC :: set_sound_volume
PUBLIC :: set_target_fps
PUBLIC :: set_text_line_spacing
PUBLIC :: set_texture_filter
PUBLIC :: set_texture_wrap
PUBLIC :: set_trace_log_callback
PUBLIC :: set_trace_log_level
PUBLIC :: set_window_focused
PUBLIC :: set_window_icon
PUBLIC :: set_window_icons
PUBLIC :: set_window_max_size
PUBLIC :: set_window_min_size
PUBLIC :: set_window_monitor
PUBLIC :: set_window_opacity
PUBLIC :: set_window_position
PUBLIC :: set_window_size
PUBLIC :: set_window_state
PUBLIC :: set_window_title
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

  ! void DrawBillboard(Camera camera, Texture2D texture, Vector3 position, float size, Color tint)
        subroutine draw_billboard(camera, texture, position, size, tint) bind(c, name='DrawBillboard')
    IMPORT :: C_FLOAT, camera3d_type, color_type, texture2d_type, vector3_type
    IMPLICIT NONE
    TYPE(camera3d_type), INTENT(in), VALUE :: camera
    TYPE(texture2d_type), INTENT(in), VALUE :: texture
    TYPE(vector3_type), INTENT(in), VALUE :: position
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: size
    TYPE(color_type), INTENT(in), VALUE :: tint
  END SUBROUTINE draw_billboard

  ! void DrawBillboardPro(Camera camera, Texture2D texture, Rectangle source, Vector3 position, Vector3 up, Vector2 size, Vector2 origin, float rotation, Color tint)
        subroutine draw_billboard_pro(camera, texture, source, position, up, size, origin, rotation, tint) &
    BIND(c, name='DrawBillboardPro')
            import :: c_float, camera3d_type, color_type, rectangle_type, texture2d_type, vector2_type, vector3_type
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
        subroutine draw_billboard_rec(camera, texture, source, position, size, tint) bind(c, name='DrawBillboardRec')
            import :: camera3d_type, color_type, rectangle_type, texture2d_type, vector2_type, vector3_type
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
        subroutine draw_capsule(start_pos, end_pos, radius, slices, rings, color) bind(c, name='DrawCapsule')
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
        subroutine draw_capsule_wires(start_pos, end_pos, radius, slices, rings, color) bind(c, name='DrawCapsuleWires')
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
        subroutine draw_circle3d(center, radius, rotation_axis, rotation_angle, color) bind(c, name='DrawCircle3D')
    IMPORT :: C_FLOAT, color_type, vector3_type
    IMPLICIT NONE
    TYPE(vector3_type), INTENT(in), VALUE :: center
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: radius
    TYPE(vector3_type), INTENT(in), VALUE :: rotation_axis
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: rotation_angle
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE draw_circle3d

  ! void DrawCircleGradient(int centerX, int centerY, float radius, Color color1, Color color2)
        subroutine draw_circle_gradient(center_x, center_y, radius, color1, color2) bind(c, name='DrawCircleGradient')
    IMPORT :: C_FLOAT, C_INT, color_type
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: center_x
    INTEGER(kind=C_INT), INTENT(in), VALUE :: center_y
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: radius
    TYPE(color_type), INTENT(in), VALUE :: color1
    TYPE(color_type), INTENT(in), VALUE :: color2
  END SUBROUTINE draw_circle_gradient

  ! void DrawCircleLines(int centerX, int centerY, float radius, Color color)
        subroutine draw_circle_lines(center_x, center_y, radius, color) bind(c, name='DrawCircleLines')
    IMPORT :: C_FLOAT, C_INT, color_type
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: center_x
    INTEGER(kind=C_INT), INTENT(in), VALUE :: center_y
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: radius
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE draw_circle_lines

  ! void DrawCircleLinesV(Vector2 center, float radius, Color color)
        subroutine draw_circle_lines_v(center, radius, color) bind(c, name='DrawCircleLinesV')
    IMPORT :: C_FLOAT, color_type, vector2_type
    IMPLICIT NONE
    TYPE(vector2_type), INTENT(in), VALUE :: center
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: radius
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE draw_circle_lines_v

  ! void DrawCircleSector(Vector2 center, float radius, float startAngle, float endAngle, int segments, Color color)
        subroutine draw_circle_sector(center, radius, start_angle, end_angle, segments, color) &
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
        subroutine draw_circle_sector_lines(center, radius, start_angle, end_angle, segments, color) &
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
        subroutine draw_cube(position, width, height, length, color) bind(c, name='DrawCube')
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
        subroutine draw_cube_wires(position, width, height, length, color) bind(c, name='DrawCubeWires')
    IMPORT :: C_FLOAT, color_type, vector3_type
    IMPLICIT NONE
    TYPE(vector3_type), INTENT(in), VALUE :: position
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: width
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: height
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: length
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE draw_cube_wires

  ! void DrawCubeWiresV(Vector3 position, Vector3 size, Color color)
        subroutine draw_cube_wires_v(position, size, color) bind(c, name='DrawCubeWiresV')
    IMPORT :: color_type, vector3_type
    IMPLICIT NONE
    TYPE(vector3_type), INTENT(in), VALUE :: position
    TYPE(vector3_type), INTENT(in), VALUE :: size
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE draw_cube_wires_v

  ! void DrawCylinder(Vector3 position, float radiusTop, float radiusBottom, float height, int slices, Color color)
        subroutine draw_cylinder(position, radius_top, radius_bottom, height, slices, color) bind(c, name='DrawCylinder')
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
        subroutine draw_cylinder_ex(start_pos, end_pos, start_radius, end_radius, sides, color) bind(c, name='DrawCylinderEx')
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
        subroutine draw_cylinder_wires(position, radius_top, radius_bottom, height, slices, color) &
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
        subroutine draw_cylinder_wires_ex(start_pos, end_pos, start_radius, end_radius, sides, color) &
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
        subroutine draw_ellipse(center_x, center_y, radius_h, radius_v, color) bind(c, name='DrawEllipse')
    IMPORT :: C_FLOAT, C_INT, color_type
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: center_x
    INTEGER(kind=C_INT), INTENT(in), VALUE :: center_y
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: radius_h
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: radius_v
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE draw_ellipse

  ! void DrawEllipseLines(int centerX, int centerY, float radiusH, float radiusV, Color color)
        subroutine draw_ellipse_lines(center_x, center_y, radius_h, radius_v, color) bind(c, name='DrawEllipseLines')
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
        subroutine draw_line(start_pos_x, start_pos_y, end_pos_x, end_pos_y, color) bind(c, name='DrawLine')
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
        subroutine draw_line_bezier(start_pos, end_pos, thick, color) bind(c, name='DrawLineBezier')
    IMPORT :: C_FLOAT, color_type, vector2_type
    IMPLICIT NONE
    TYPE(vector2_type), INTENT(in), VALUE :: start_pos
    TYPE(vector2_type), INTENT(in), VALUE :: end_pos
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: thick
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE draw_line_bezier

  ! void DrawLineEx(Vector2 startPos, Vector2 endPos, float thick, Color color)
        subroutine draw_line_ex(start_pos, end_pos, thick, color) bind(c, name='DrawLineEx')
    IMPORT :: C_FLOAT, color_type, vector2_type
    IMPLICIT NONE
    TYPE(vector2_type), INTENT(in), VALUE :: start_pos
    TYPE(vector2_type), INTENT(in), VALUE :: end_pos
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: thick
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE draw_line_ex

  ! void DrawLineStrip(Vector2 *points, int pointCount, Color color)
        subroutine draw_line_strip(points, point_count, color) bind(c, name='DrawLineStrip')
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
        subroutine draw_mesh_instanced(mesh, material, transforms, instances) bind(c, name='DrawMeshInstanced')
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
        subroutine draw_model_ex(model, position, rotation_axis, rotation_angle, scale, tint) &
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
        subroutine draw_model_wires(model, position, scale, tint) bind(c, name='DrawModelWires')
    IMPORT :: C_FLOAT, color_type, model_type, vector3_type
    IMPLICIT NONE
    TYPE(model_type), INTENT(in), VALUE :: model
    TYPE(vector3_type), INTENT(in), VALUE :: position
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: scale
    TYPE(color_type), INTENT(in), VALUE :: tint
  END SUBROUTINE draw_model_wires

  ! void DrawModelWiresEx(Model model, Vector3 position, Vector3 rotationAxis, float rotationAngle, Vector3 scale, Color tint)
        subroutine draw_model_wires_ex(model, position, rotation_axis, rotation_angle, scale, tint) &
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
        subroutine draw_poly(center, sides, radius, rotation, color) bind(c, name='DrawPoly')
    IMPORT :: C_FLOAT, C_INT, color_type, vector2_type
    IMPLICIT NONE
    TYPE(vector2_type), INTENT(in), VALUE :: center
    INTEGER(kind=C_INT), INTENT(in), VALUE :: sides
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: radius
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: rotation
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE draw_poly

  ! void DrawPolyLines(Vector2 center, int sides, float radius, float rotation, Color color)
        subroutine draw_poly_lines(center, sides, radius, rotation, color) bind(c, name='DrawPolyLines')
    IMPORT :: C_FLOAT, C_INT, color_type, vector2_type
    IMPLICIT NONE
    TYPE(vector2_type), INTENT(in), VALUE :: center
    INTEGER(kind=C_INT), INTENT(in), VALUE :: sides
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: radius
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: rotation
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE draw_poly_lines

  ! void DrawPolyLinesEx(Vector2 center, int sides, float radius, float rotation, float lineThick, Color color)
        subroutine draw_poly_lines_ex(center, sides, radius, rotation, line_thick, color) &
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
        subroutine draw_rectangle(pos_x, pos_y, width, height, color) bind(c, name='DrawRectangle')
    IMPORT :: C_INT, color_type
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: pos_x
    INTEGER(kind=C_INT), INTENT(in), VALUE :: pos_y
    INTEGER(kind=C_INT), INTENT(in), VALUE :: width
    INTEGER(kind=C_INT), INTENT(in), VALUE :: height
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE draw_rectangle

  ! void DrawRectangleGradientEx(Rectangle rec, Color col1, Color col2, Color col3, Color col4)
        subroutine draw_rectangle_gradient_ex(rec, col1, col2, col3, col4) bind(c, name='DrawRectangleGradientEx')
    IMPORT :: color_type, rectangle_type
    IMPLICIT NONE
    TYPE(rectangle_type), INTENT(in), VALUE :: rec
    TYPE(color_type), INTENT(in), VALUE :: col1
    TYPE(color_type), INTENT(in), VALUE :: col2
    TYPE(color_type), INTENT(in), VALUE :: col3
    TYPE(color_type), INTENT(in), VALUE :: col4
  END SUBROUTINE draw_rectangle_gradient_ex

  ! void DrawRectangleGradientH(int posX, int posY, int width, int height, Color color1, Color color2)
        subroutine draw_rectangle_gradient_h(pos_x, pos_y, width, height, color1, color2) &
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
        subroutine draw_rectangle_gradient_v(pos_x, pos_y, width, height, color1, color2) &
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
        subroutine draw_rectangle_lines(pos_x, pos_y, width, height, color) bind(c, name='DrawRectangleLines')
    IMPORT :: C_INT, color_type
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: pos_x
    INTEGER(kind=C_INT), INTENT(in), VALUE :: pos_y
    INTEGER(kind=C_INT), INTENT(in), VALUE :: width
    INTEGER(kind=C_INT), INTENT(in), VALUE :: height
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE draw_rectangle_lines

  ! void DrawRectangleLinesEx(Rectangle rec, float lineThick, Color color)
        subroutine draw_rectangle_lines_ex(rec, line_thick, color) bind(c, name='DrawRectangleLinesEx')
    IMPORT :: C_FLOAT, color_type, rectangle_type
    IMPLICIT NONE
    TYPE(rectangle_type), INTENT(in), VALUE :: rec
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: line_thick
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE draw_rectangle_lines_ex

  ! void DrawRectanglePro(Rectangle rec, Vector2 origin, float rotation, Color color)
        subroutine draw_rectangle_pro(rec, origin, rotation, color) bind(c, name='DrawRectanglePro')
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
        subroutine draw_rectangle_rounded(rec, roundness, segments, color) bind(c, name='DrawRectangleRounded')
    IMPORT :: C_FLOAT, C_INT, color_type, rectangle_type
    IMPLICIT NONE
    TYPE(rectangle_type), INTENT(in), VALUE :: rec
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: roundness
    INTEGER(kind=C_INT), INTENT(in), VALUE :: segments
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE draw_rectangle_rounded

  ! void DrawRectangleRoundedLines(Rectangle rec, float roundness, int segments, float lineThick, Color color)
        subroutine draw_rectangle_rounded_lines(rec, roundness, segments, line_thick, color) &
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
        subroutine draw_rectangle_v(position, size, color) bind(c, name='DrawRectangleV')
    IMPORT :: color_type, vector2_type
    IMPLICIT NONE
    TYPE(vector2_type), INTENT(in), VALUE :: position
    TYPE(vector2_type), INTENT(in), VALUE :: size
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE draw_rectangle_v

  ! void DrawRing(Vector2 center, float innerRadius, float outerRadius, float startAngle, float endAngle, int segments, Color color)
        subroutine draw_ring(center, inner_radius, outer_radius, start_angle, end_angle, segments, color) &
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
        subroutine draw_ring_lines(center, inner_radius, outer_radius, start_angle, end_angle, segments, color) &
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
        subroutine draw_sphere_ex(center_pos, radius, rings, slices, color) bind(c, name='DrawSphereEx')
    IMPORT :: C_FLOAT, C_INT, color_type, vector3_type
    IMPLICIT NONE
    TYPE(vector3_type), INTENT(in), VALUE :: center_pos
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: radius
    INTEGER(kind=C_INT), INTENT(in), VALUE :: rings
    INTEGER(kind=C_INT), INTENT(in), VALUE :: slices
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE draw_sphere_ex

  ! void DrawSphereWires(Vector3 centerPos, float radius, int rings, int slices, Color color)
        subroutine draw_sphere_wires(center_pos, radius, rings, slices, color) bind(c, name='DrawSphereWires')
    IMPORT :: C_FLOAT, C_INT, color_type, vector3_type
    IMPLICIT NONE
    TYPE(vector3_type), INTENT(in), VALUE :: center_pos
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: radius
    INTEGER(kind=C_INT), INTENT(in), VALUE :: rings
    INTEGER(kind=C_INT), INTENT(in), VALUE :: slices
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE draw_sphere_wires

  ! void DrawSplineBasis(Vector2 *points, int pointCount, float thick, Color color)
        subroutine draw_spline_basis(points, point_count, thick, color) bind(c, name='DrawSplineBasis')
    IMPORT :: C_FLOAT, C_INT, color_type, vector2_type
    IMPLICIT NONE
    TYPE(vector2_type), INTENT(in) :: points(*)
    INTEGER(kind=C_INT), INTENT(in), VALUE :: point_count
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: thick
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE draw_spline_basis

  ! void DrawSplineBezierCubic(Vector2 *points, int pointCount, float thick, Color color)
        subroutine draw_spline_bezier_cubic(points, point_count, thick, color) bind(c, name='DrawSplineBezierCubic')
    IMPORT :: C_FLOAT, C_INT, color_type, vector2_type
    IMPLICIT NONE
    TYPE(vector2_type), INTENT(in) :: points(*)
    INTEGER(kind=C_INT), INTENT(in), VALUE :: point_count
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: thick
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE draw_spline_bezier_cubic

  ! void DrawSplineBezierQuadratic(Vector2 *points, int pointCount, float thick, Color color)
        subroutine draw_spline_bezier_quadratic(points, point_count, thick, color) bind(c, name='DrawSplineBezierQuadratic')
    IMPORT :: C_FLOAT, C_INT, color_type, vector2_type
    IMPLICIT NONE
    TYPE(vector2_type), INTENT(in) :: points(*)
    INTEGER(kind=C_INT), INTENT(in), VALUE :: point_count
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: thick
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE draw_spline_bezier_quadratic

  ! void DrawSplineCatmullRom(Vector2 *points, int pointCount, float thick, Color color)
        subroutine draw_spline_catmull_rom(points, point_count, thick, color) bind(c, name='DrawSplineCatmullRom')
    IMPORT :: C_FLOAT, C_INT, color_type, vector2_type
    IMPLICIT NONE
    TYPE(vector2_type), INTENT(in) :: points(*)
    INTEGER(kind=C_INT), INTENT(in), VALUE :: point_count
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: thick
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE draw_spline_catmull_rom

  ! void DrawSplineLinear(Vector2 *points, int pointCount, float thick, Color color)
        subroutine draw_spline_linear(points, point_count, thick, color) bind(c, name='DrawSplineLinear')
    IMPORT :: C_FLOAT, C_INT, color_type, vector2_type
    IMPLICIT NONE
    TYPE(vector2_type), INTENT(in) :: points(*)
    INTEGER(kind=C_INT), INTENT(in), VALUE :: point_count
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: thick
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE draw_spline_linear

  ! void DrawSplineSegmentBasis(Vector2 p1, Vector2 p2, Vector2 p3, Vector2 p4, float thick, Color color)
        subroutine draw_spline_segment_basis(p1, p2, p3, p4, thick, color) bind(c, name='DrawSplineSegmentBasis')
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
        subroutine draw_spline_segment_bezier_cubic(p1, c2, c3, p4, thick, color) bind(c, name='DrawSplineSegmentBezierCubic')
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
        subroutine draw_spline_segment_bezier_quadratic(p1, c2, p3, thick, color) bind(c, name='DrawSplineSegmentBezierQuadratic')
    IMPORT :: C_FLOAT, color_type, vector2_type
    IMPLICIT NONE
    TYPE(vector2_type), INTENT(in), VALUE :: p1
    TYPE(vector2_type), INTENT(in), VALUE :: c2
    TYPE(vector2_type), INTENT(in), VALUE :: p3
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: thick
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE draw_spline_segment_bezier_quadratic

  ! void DrawSplineSegmentCatmullRom(Vector2 p1, Vector2 p2, Vector2 p3, Vector2 p4, float thick, Color color)
        subroutine draw_spline_segment_catmull_rom(p1, p2, p3, p4, thick, color) bind(c, name='DrawSplineSegmentCatmullRom')
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
        subroutine draw_spline_segment_linear(p1, p2, thick, color) bind(c, name='DrawSplineSegmentLinear')
    IMPORT :: C_FLOAT, color_type, vector2_type
    IMPLICIT NONE
    TYPE(vector2_type), INTENT(in), VALUE :: p1
    TYPE(vector2_type), INTENT(in), VALUE :: p2
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: thick
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE draw_spline_segment_linear

  ! void DrawTriangleStrip3D(Vector3 *points, int pointCount, Color color)
        subroutine draw_triangle_strip3d(points, point_count, color) bind(c, name='DrawTriangleStrip3D')
    IMPORT :: C_INT, color_type, vector3_type
    IMPLICIT NONE
    TYPE(vector3_type), INTENT(in) :: points(*)
    INTEGER(kind=C_INT), INTENT(in), VALUE :: point_count
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE draw_triangle_strip3d

  ! void DrawText(const char *text, int posX, int posY, int fontSize, Color color)
        subroutine draw_text(text, pos_x, pos_y, font_size, color) bind(c, name='DrawText')
    IMPORT :: C_CHAR, C_INT, color_type
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: text
    INTEGER(kind=C_INT), INTENT(in), VALUE :: pos_x
    INTEGER(kind=C_INT), INTENT(in), VALUE :: pos_y
    INTEGER(kind=C_INT), INTENT(in), VALUE :: font_size
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE draw_text

  ! void DrawTextCodepoint(Font font, int codepoint, Vector2 position, float fontSize, Color tint)
        subroutine draw_text_codepoint(font, codepoint, position, font_size, tint) bind(c, name='DrawTextCodepoint')
    IMPORT :: C_FLOAT, C_INT, color_type, font_type, vector2_type
    IMPLICIT NONE
    TYPE(font_type), INTENT(in), VALUE :: font
    INTEGER(kind=C_INT), INTENT(in), VALUE :: codepoint
    TYPE(vector2_type), INTENT(in), VALUE :: position
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: font_size
    TYPE(color_type), INTENT(in), VALUE :: tint
  END SUBROUTINE draw_text_codepoint

  ! void DrawTextCodepoints(Font font, const int *codepoints, int codepointCount, Vector2 position, float fontSize, float spacing, Color tint)
        subroutine draw_text_codepoints(font, codepoints, codepointCount, position, font_size, spacing, tint) &
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
        subroutine draw_text_ex(font, text, position, font_size, spacing, tint) bind(c, name='DrawTextEx')
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
        subroutine draw_text_pro(font, text, position, origin, rotation, font_size, spacing, tint) &
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
        subroutine draw_texture(texture, pos_x, pos_y, tint) bind(c, name='DrawTexture')
    IMPORT :: C_INT, color_type, texture2d_type
    IMPLICIT NONE
    TYPE(texture2d_type), INTENT(in), VALUE :: texture
    INTEGER(kind=C_INT), INTENT(in), VALUE :: pos_x
    INTEGER(kind=C_INT), INTENT(in), VALUE :: pos_y
    TYPE(color_type), INTENT(in), VALUE :: tint
  END SUBROUTINE draw_texture

  ! void DrawTextureEx(Texture2D texture, Vector2 position, float rotation, float scale, Color tint)
        subroutine draw_texture_ex(texture, position, rotation, scale, tint) bind(c, name='DrawTextureEx')
    IMPORT :: C_FLOAT, color_type, texture2d_type, vector2_type
    IMPLICIT NONE
    TYPE(texture2d_type), INTENT(in), VALUE :: texture
    TYPE(vector2_type), INTENT(in), VALUE :: position
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: rotation
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: scale
    TYPE(color_type), INTENT(in), VALUE :: tint
  END SUBROUTINE draw_texture_ex

  ! void DrawTextureNPatch(Texture2D texture, NPatchInfo nPatchInfo, Rectangle dest, Vector2 origin, float rotation, Color tint)
        subroutine draw_texture_npatch(texture, npatch_info, dest, origin, rotation, tint) &
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
        subroutine draw_texture_pro(texture, source, dest, origin, rotation, tint) bind(c, name='DrawTexturePro')
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
        subroutine draw_texture_rec(texture, source, position, tint) bind(c, name='DrawTextureRec')
    IMPORT :: color_type, rectangle_type, texture2d_type, vector2_type
    IMPLICIT NONE
    TYPE(texture2d_type), INTENT(in), VALUE :: texture
    TYPE(rectangle_type), INTENT(in), VALUE :: source
    TYPE(vector2_type), INTENT(in), VALUE :: position
    TYPE(color_type), INTENT(in), VALUE :: tint
  END SUBROUTINE draw_texture_rec

  ! void DrawTextureV(Texture2D texture, Vector2 position, Color tint)
        subroutine draw_texture_v(texture, position, tint) bind(c, name='DrawTextureV')
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
        subroutine draw_triangle_fan(points, point_count, color) bind(c, name='DrawTriangleFan')
    IMPORT :: C_INT, color_type, vector2_type
    IMPLICIT NONE
    TYPE(vector2_type), INTENT(in) :: points(*)
    INTEGER(kind=C_INT), INTENT(in), VALUE :: point_count
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE draw_triangle_fan

  ! void DrawTriangleLines(Vector2 v1, Vector2 v2, Vector2 v3, Color color)
        subroutine draw_triangle_lines(v1, v2, v3, color) bind(c, name='DrawTriangleLines')
    IMPORT :: color_type, vector2_type
    IMPLICIT NONE
    TYPE(vector2_type), INTENT(in), VALUE :: v1
    TYPE(vector2_type), INTENT(in), VALUE :: v2
    TYPE(vector2_type), INTENT(in), VALUE :: v3
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE draw_triangle_lines

  ! void DrawTriangleStrip(Vector2 *points, int pointCount, Color color)
        subroutine draw_triangle_strip(points, point_count, color) bind(c, name='DrawTriangleStrip')
    IMPORT :: C_INT, color_type, vector2_type
    IMPLICIT NONE
    TYPE(vector2_type), INTENT(in) :: points(*)
    INTEGER(kind=C_INT), INTENT(in), VALUE :: point_count
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE draw_triangle_strip

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

  ! const char *GetApplicationDirectory(void)
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
        function get_codepoint_next(text, codepoint_size) bind(c, name='GetCodepointNext')
    IMPORT :: C_CHAR, C_INT
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: text
    INTEGER(kind=C_INT), INTENT(out) :: codepoint_size
    INTEGER(kind=C_INT) :: get_codepoint_next
  END FUNCTION get_codepoint_next

  ! int GetCodepointPrevious(const char *text, int *codepointSize)
        function get_codepoint_previous(text, codepoint_size) bind(c, name='GetCodepointPrevious')
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
        function get_file_name_without_ext(file_path) bind(c, name='GetFileNameWithoutExt')
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
        function get_gamepad_axis_movement(gamepad, axis) bind(c, name='GetGamepadAxisMovement')
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
        function get_image_alpha_border(image, threshold) bind(c, name='GetImageAlphaBorder')
    IMPORT :: C_FLOAT, image_type, rectangle_type
    IMPLICIT NONE
    TYPE(image_type), INTENT(in), VALUE :: image
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: threshold
    TYPE(rectangle_type) :: get_image_alpha_border
  END FUNCTION get_image_alpha_border

  ! Vector2 GetSplinePointBasis(Vector2 p1, Vector2 p2, Vector2 p3, Vector2 p4, float t)
        function get_spline_point_basis(p1, p2, p3, p4, t) bind(c, name='GetSplinePointBasis')
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
        function get_spline_point_bezier_cubic(p1, c2, c3, p4, t) bind(c, name='GetSplinePointBezierCubic')
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
        function get_spline_point_bezier_quad(p1, c2, p3, t) bind(c, name='GetSplinePointBezierQuad')
    IMPORT :: C_FLOAT, vector2_type
    IMPLICIT NONE
    TYPE(vector2_type), INTENT(in), VALUE :: p1
    TYPE(vector2_type), INTENT(in), VALUE :: c2
    TYPE(vector2_type), INTENT(in), VALUE :: p3
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: t
    TYPE(vector2_type) :: get_spline_point_bezier_quad
  END FUNCTION get_spline_point_bezier_quad

  ! Vector2 GetSplinePointCatmullRom(Vector2 p1, Vector2 p2, Vector2 p3, Vector2 p4, float t)
        function get_spline_point_catmull_rom(p1, p2, p3, p4, t) bind(c, name='GetSplinePointCatmullRom')
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
        function get_spline_point_linear(start_pos, end_pos, t) bind(c, name='GetSplinePointLinear')
    IMPORT :: C_FLOAT, vector2_type
    IMPLICIT NONE
    TYPE(vector2_type), INTENT(in), VALUE :: start_pos
    TYPE(vector2_type), INTENT(in), VALUE :: end_pos
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: t
    TYPE(vector2_type) :: get_spline_point_linear
  END FUNCTION get_spline_point_linear

  ! void ImageBlurGaussian(Image *image, int blurSize)
        subroutine image_blur_gaussian(image, blur_size) bind(c, name='ImageBlurGaussian')
    IMPORT :: C_INT, image_type
    IMPLICIT NONE
    TYPE(image_type), INTENT(inout) :: image
    INTEGER(kind=C_INT), INTENT(in), VALUE :: blur_size
  END SUBROUTINE image_blur_gaussian

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
        function get_monitor_physical_height(monitor) bind(c, name='GetMonitorPhysicalHeight')
    IMPORT :: C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: monitor
    INTEGER(kind=C_INT) :: get_monitor_physical_height
  END FUNCTION get_monitor_physical_height

  ! int GetMonitorPhysicalWidth(int monitor)
        function get_monitor_physical_width(monitor) bind(c, name='GetMonitorPhysicalWidth')
    IMPORT :: C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: monitor
    INTEGER(kind=C_INT) :: get_monitor_physical_width
  END FUNCTION get_monitor_physical_width

  ! int GetMonitorRefreshRate(int monitor)
        function get_monitor_refresh_rate(monitor) bind(c, name='GetMonitorRefreshRate')
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
        function get_pixel_data_size(width, height, format) bind(c, name='GetPixelDataSize')
    IMPORT :: C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: width
    INTEGER(kind=C_INT), INTENT(in), VALUE :: height
    INTEGER(kind=C_INT), INTENT(in), VALUE :: FORMAT
    INTEGER(kind=C_INT) :: get_pixel_data_size
  END FUNCTION get_pixel_data_size

  ! const char *GetPrevDirectoryPath(const char *dirPath)
        function get_prev_directory_path(dir_path) bind(c, name='GetPrevDirectoryPath')
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
        function get_ray_collision_mesh(ray, mesh, transform) bind(c, name='GetRayCollisionMesh')
    IMPORT :: matrix_type, mesh_type, ray_collision_type, ray_type
    IMPLICIT NONE
    TYPE(ray_type), INTENT(in), VALUE :: ray
    TYPE(mesh_type), INTENT(in), VALUE :: mesh
    TYPE(matrix_type), INTENT(in), VALUE :: transform
    TYPE(ray_collision_type) :: get_ray_collision_mesh
  END FUNCTION get_ray_collision_mesh

  ! RayCollision GetRayCollisionQuad(Ray ray, Vector3 p1, Vector3 p2, Vector3 p3, Vector3 p4)
        function get_ray_collision_quad(ray, p1, p2, p3, p4) bind(c, name='GetRayCollisionQuad')
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
        function get_ray_collision_sphere(ray, center, radius) bind(c, name='GetRayCollisionSphere')
    IMPORT :: C_FLOAT, ray_collision_type, ray_type, vector3_type
    IMPLICIT NONE
    TYPE(ray_type), INTENT(in), VALUE :: ray
    TYPE(vector3_type), INTENT(in), VALUE :: center
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: radius
    TYPE(ray_collision_type) :: get_ray_collision_sphere
  END FUNCTION get_ray_collision_sphere

  ! RayCollision GetRayCollisionTriangle(Ray ray, Vector3 p1, Vector3 p2, Vector3 p3)
        function get_ray_collision_triangle(ray, p1, p2, p3) bind(c, name='GetRayCollisionTriangle')
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
        function get_screen_to_world2d(position, camera) bind(c, name='GetScreenToWorld2D')
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
        function get_shader_location(shader, uniform_name) bind(c, name='GetShaderLocation')
    IMPORT :: C_CHAR, C_INT, shader_type
    IMPLICIT NONE
    TYPE(shader_type), INTENT(in), VALUE :: shader
    CHARACTER(kind=C_CHAR), INTENT(in) :: uniform_name
    INTEGER(kind=C_INT) :: get_shader_location
  END FUNCTION get_shader_location

  ! int GetShaderLocationAttrib(Shader shader, const char *attribName)
        function get_shader_location_attrib(shader, attrib_name) bind(c, name='GetShaderLocationAttrib')
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
        function get_world_to_screen2d(position, camera) bind(c, name='GetWorldToScreen2D')
    IMPORT :: camera2d_type, vector2_type
    IMPLICIT NONE
    TYPE(vector2_type), INTENT(in), VALUE :: position
    TYPE(camera2d_type), INTENT(in), VALUE :: camera
    TYPE(vector2_type) :: get_world_to_screen2d
  END FUNCTION get_world_to_screen2d

  ! void HideCursor(void)
  SUBROUTINE hide_cursor() BIND(c, name='HideCursor')
  END SUBROUTINE hide_cursor

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

  ! void SetAudioStreamBufferSizeDefault(int size)
        subroutine set_audio_stream_buffer_size_default(size) bind(c, name='SetAudioStreamBufferSizeDefault')
    IMPORT :: C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: size
  END SUBROUTINE set_audio_stream_buffer_size_default

  ! void SetAudioStreamPan(AudioStream stream, float pan)
SUBROUTINE set_audio_stream_pan(stream, pan) BIND(c, name='SetAudioStreamPan')
    IMPORT :: audio_stream_type, C_FLOAT
    IMPLICIT NONE
    TYPE(audio_stream_type), INTENT(in), VALUE :: stream
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: pan
  END SUBROUTINE set_audio_stream_pan

  ! void SetAudioStreamPitch(AudioStream stream, float pitch)
        subroutine set_audio_stream_pitch(stream, pitch) bind(c, name='SetAudioStreamPitch')
    IMPORT :: audio_stream_type, C_FLOAT
    IMPLICIT NONE
    TYPE(audio_stream_type), INTENT(in), VALUE :: stream
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: pitch
  END SUBROUTINE set_audio_stream_pitch

  ! void SetAudioStreamVolume(AudioStream stream, float volume)
        subroutine set_audio_stream_volume(stream, volume) bind(c, name='SetAudioStreamVolume')
    IMPORT :: audio_stream_type, C_FLOAT
    IMPLICIT NONE
    TYPE(audio_stream_type), INTENT(in), VALUE :: stream
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: volume
  END SUBROUTINE set_audio_stream_volume

  ! void SetCameraAltControl(int keyAlt)
SUBROUTINE set_camera_alt_control(key_alt) BIND(c, name='SetCameraAltControl')
    IMPORT :: C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: key_alt
  END SUBROUTINE set_camera_alt_control

  ! void SetCameraMode(Camera camera, int mode)
  SUBROUTINE set_camera_mode(camera, mode) BIND(c, name='SetCameraMode')
    IMPORT :: C_INT, camera3d_type
    IMPLICIT NONE
    TYPE(camera3d_type), INTENT(in), VALUE :: camera
    INTEGER(kind=C_INT), INTENT(in), VALUE :: mode
  END SUBROUTINE set_camera_mode

  ! void SetCameraMoveControls(int keyFront, int keyBack, int keyRight, int keyLeft, int keyUp, int keyDown)
        subroutine set_camera_move_controls(key_front, key_back, key_right, key_left, key_up, key_down) &
    BIND(c, name='SetCameraMoveControls')
    IMPORT :: C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: key_front
    INTEGER(kind=C_INT), INTENT(in), VALUE :: key_back
    INTEGER(kind=C_INT), INTENT(in), VALUE :: key_right
    INTEGER(kind=C_INT), INTENT(in), VALUE :: key_left
    INTEGER(kind=C_INT), INTENT(in), VALUE :: key_up
    INTEGER(kind=C_INT), INTENT(in), VALUE :: key_down
  END SUBROUTINE set_camera_move_controls

  ! void SetCameraPanControl(int keyPan)
SUBROUTINE set_camera_pan_control(key_pan) BIND(c, name='SetCameraPanControl')
    IMPORT :: C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: key_pan
  END SUBROUTINE set_camera_pan_control

  ! void SetCameraSmoothZoomControl(int keySmoothZoom)
        subroutine set_camera_smooth_zoom_control(key_smooth_zoom) bind(c, name='SetCameraSmoothZoomControl')
    IMPORT :: C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: key_smooth_zoom
  END SUBROUTINE set_camera_smooth_zoom_control

  ! void SetClipboardText(const char *text)
  SUBROUTINE set_clipboard_text(text) BIND(c, name='SetClipboardText')
    IMPORT :: C_CHAR
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: text
  END SUBROUTINE set_clipboard_text

  ! void SetConfigFlags(unsigned int flags)
  SUBROUTINE set_config_flags(flags) BIND(c, name='SetConfigFlags')
    IMPORT :: c_unsigned_int
    IMPLICIT NONE
    INTEGER(kind=c_unsigned_int), INTENT(in), VALUE :: flags
  END SUBROUTINE set_config_flags

  ! void SetExitKey(int key)
  SUBROUTINE set_exit_key(key) BIND(c, name='SetExitKey')
    IMPORT :: C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: key
  END SUBROUTINE set_exit_key

  ! int SetGamepadMappings(const char *mappings)
  FUNCTION set_gamepad_mappings(mappings) BIND(c, name='SetGamepadMappings')
    IMPORT :: C_CHAR, C_INT
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: mappings
    INTEGER(kind=C_INT) :: set_gamepad_mappings
  END FUNCTION set_gamepad_mappings

  ! void SetGesturesEnabled(unsigned int flags)
  SUBROUTINE set_gestures_enabled(flags) BIND(c, name='SetGesturesEnabled')
    IMPORT :: c_unsigned_int
    IMPLICIT NONE
    INTEGER(kind=c_unsigned_int), INTENT(in), VALUE :: flags
  END SUBROUTINE set_gestures_enabled

  ! void SetLoadFileDataCallback(LoadFileDataCallback callback)
        subroutine set_load_file_data_callback(callback) bind(c, name='SetLoadFileDataCallback')
    IMPORT :: C_FUNPTR
    IMPLICIT NONE
    TYPE(C_FUNPTR), INTENT(in), VALUE :: callback
  END SUBROUTINE set_load_file_data_callback

  ! void SetLoadFileTextCallback(LoadFileTextCallback callback)
        subroutine set_load_file_text_callback(callback) bind(c, name='SetLoadFileTextCallback')
    IMPORT :: C_FUNPTR
    IMPLICIT NONE
    TYPE(C_FUNPTR), INTENT(in), VALUE :: callback
  END SUBROUTINE set_load_file_text_callback

  ! void SetMasterVolume(float volume)
  SUBROUTINE set_master_volume(volume) BIND(c, name='SetMasterVolume')
    IMPORT :: C_FLOAT
    IMPLICIT NONE
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: volume
  END SUBROUTINE set_master_volume

  ! void SetMaterialTexture(Material *material, int mapType, Texture2D texture)
        subroutine set_material_texture(material, map_type, texture) bind(c, name='SetMaterialTexture')
    IMPORT :: C_INT, material_type, texture2d_type
    IMPLICIT NONE
    TYPE(material_type), INTENT(inout) :: material
    INTEGER(kind=C_INT), INTENT(in), VALUE :: map_type
    TYPE(texture2d_type), INTENT(in), VALUE :: texture
  END SUBROUTINE set_material_texture

  ! void SetModelMeshMaterial(Model *model, int meshId, int materialId)
        subroutine set_model_mesh_material(model, mesh_id, material_id) bind(c, name='SetModelMeshMaterial')
    IMPORT :: C_INT, model_type
    IMPLICIT NONE
    TYPE(model_type), INTENT(inout) :: model
    INTEGER(kind=C_INT), INTENT(in), VALUE :: mesh_id
    INTEGER(kind=C_INT), INTENT(in), VALUE :: material_id
  END SUBROUTINE set_model_mesh_material

  ! void SetMouseCursor(int cursor)
  SUBROUTINE set_mouse_cursor(cursor) BIND(c, name='SetMouseCursor')
    IMPORT :: C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: cursor
  END SUBROUTINE set_mouse_cursor

  ! void SetMouseOffset(int offsetX, int offsetY)
SUBROUTINE set_mouse_offset(offset_x, offset_y) BIND(c, name='SetMouseOffset')
    IMPORT :: C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: offset_x
    INTEGER(kind=C_INT), INTENT(in), VALUE :: offset_y
  END SUBROUTINE set_mouse_offset

  ! void SetMousePosition(int x, int y)
  SUBROUTINE set_mouse_position(x, y) BIND(c, name='SetMousePosition')
    IMPORT :: C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: x
    INTEGER(kind=C_INT), INTENT(in), VALUE :: y
  END SUBROUTINE set_mouse_position

  ! void SetMouseScale(float scaleX, float scaleY)
  SUBROUTINE set_mouse_scale(scale_x, scale_y) BIND(c, name='SetMouseScale')
    IMPORT :: C_FLOAT
    IMPLICIT NONE
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: scale_x
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: scale_y
  END SUBROUTINE set_mouse_scale

  ! void SetMusicPan(Music music, float pan)
  SUBROUTINE set_music_pan(music, pan) BIND(c, name='SetMusicPan')
    IMPORT :: C_FLOAT, music_type
    IMPLICIT NONE
    TYPE(music_type), INTENT(in), VALUE :: music
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: pan
  END SUBROUTINE set_music_pan

  ! void SetMusicPitch(Music music, float pitch)
  SUBROUTINE set_music_pitch(music, pitch) BIND(c, name='SetMusicPitch')
    IMPORT :: C_FLOAT, music_type
    IMPLICIT NONE
    TYPE(music_type), INTENT(in), VALUE :: music
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: pitch
  END SUBROUTINE set_music_pitch

  ! void SetMusicVolume(Music music, float volume)
  SUBROUTINE set_music_volume(music, volume) BIND(c, name='SetMusicVolume')
    IMPORT :: C_FLOAT, music_type
    IMPLICIT NONE
    TYPE(music_type), INTENT(in), VALUE :: music
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: volume
  END SUBROUTINE set_music_volume

  ! void SetPixelColor(void *dstPtr, Color color, int format)
        subroutine set_pixel_color(dst_ptr, color, format) bind(c, name='SetPixelColor')
    IMPORT :: C_INT, C_PTR, color_type
    IMPLICIT NONE
    TYPE(C_PTR), INTENT(in), VALUE :: dst_ptr
    TYPE(color_type), INTENT(in), VALUE :: color
    INTEGER(kind=C_INT), INTENT(in), VALUE :: FORMAT
  END SUBROUTINE set_pixel_color

  ! void SetRandomSeed(unsigned int seed)
  SUBROUTINE set_random_seed(seed) BIND(c, name='SetRandomSeed')
    IMPORT :: c_unsigned_int
    IMPLICIT NONE
    INTEGER(kind=c_unsigned_int), INTENT(in), VALUE :: seed
  END SUBROUTINE set_random_seed

  ! void SetSaveFileDataCallback(SaveFileDataCallback callback)
        subroutine set_save_file_data_callback(callback) bind(c, name='SetSaveFileDataCallback')
    IMPORT :: C_FUNPTR
    IMPLICIT NONE
    TYPE(C_FUNPTR), INTENT(in), VALUE :: callback
  END SUBROUTINE set_save_file_data_callback

  ! void SetSaveFileTextCallback(SaveFileTextCallback callback)
        subroutine set_save_file_text_callback(callback) bind(c, name='SetSaveFileTextCallback')
    IMPORT :: C_FUNPTR
    IMPLICIT NONE
    TYPE(C_FUNPTR), INTENT(in), VALUE :: callback
  END SUBROUTINE set_save_file_text_callback

  ! void SetShaderValue(Shader shader, int locIndex, const void *value, int uniformType)
        subroutine set_shader_value(shader, loc_index, value, uniform_type) bind(c, name='SetShaderValue')
    IMPORT :: C_INT, C_PTR, shader_type
    IMPLICIT NONE
    TYPE(shader_type), INTENT(in), VALUE :: shader
    INTEGER(kind=C_INT), INTENT(in), VALUE :: loc_index
    TYPE(C_PTR), INTENT(in), VALUE :: VALUE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: uniform_type
  END SUBROUTINE set_shader_value

  ! void SetShaderValueMatrix(Shader shader, int locIndex, Matrix mat)
        subroutine set_shader_value_matrix(shader, loc_index, mat) bind(c, name='SetShaderValueMatrix')
    IMPORT :: C_INT, matrix_type, shader_type
    IMPLICIT NONE
    TYPE(shader_type), INTENT(in), VALUE :: shader
    INTEGER(kind=C_INT), INTENT(in), VALUE :: loc_index
    TYPE(matrix_type), INTENT(in), VALUE :: mat
  END SUBROUTINE set_shader_value_matrix

  ! void SetShaderValueTexture(Shader shader, int locIndex, Texture2D texture)
        subroutine set_shader_value_texture(shader, loc_index, texture) bind(c, name='SetShaderValueTexture')
    IMPORT :: C_INT, shader_type, texture2d_type
    IMPLICIT NONE
    TYPE(shader_type), INTENT(in), VALUE :: shader
    INTEGER(kind=C_INT), INTENT(in), VALUE :: loc_index
    TYPE(texture2d_type), INTENT(in), VALUE :: texture
  END SUBROUTINE set_shader_value_texture

  ! void SetShaderValueV(Shader shader, int locIndex, const void *value, int uniformType, int count)
        subroutine set_shader_value_v(shader, loc_index, value, uniform_type, count) bind(c, name='SetShaderValueV')
    IMPORT :: C_INT, C_PTR, shader_type
    IMPLICIT NONE
    TYPE(shader_type), INTENT(in), VALUE :: shader
    INTEGER(kind=C_INT), INTENT(in), VALUE :: loc_index
    TYPE(C_PTR), INTENT(in), VALUE :: VALUE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: uniform_type
    INTEGER(kind=C_INT), INTENT(in), VALUE :: count
  END SUBROUTINE set_shader_value_v

  ! void SetShapesTexture(Texture2D texture, Rectangle source)
        subroutine set_shapes_texture(texture, source) bind(c, name='SetShapesTexture')
    IMPORT :: rectangle_type, texture2d_type
    IMPLICIT NONE
    TYPE(texture2d_type), INTENT(in), VALUE :: texture
    TYPE(rectangle_type), INTENT(in), VALUE :: source
  END SUBROUTINE set_shapes_texture

  ! void SetSoundPan(Sound sound, float pan)
  SUBROUTINE set_sound_pan(sound, pan) BIND(c, name='SetSoundPan')
    IMPORT :: C_FLOAT, sound_type
    IMPLICIT NONE
    TYPE(sound_type), INTENT(in), VALUE :: sound
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: pan
  END SUBROUTINE set_sound_pan

  ! void SetSoundPitch(Sound sound, float pitch)
  SUBROUTINE set_sound_pitch(sound, pitch) BIND(c, name='SetSoundPitch')
    IMPORT :: C_FLOAT, sound_type
    IMPLICIT NONE
    TYPE(sound_type), INTENT(in), VALUE :: sound
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: pitch
  END SUBROUTINE set_sound_pitch

  ! void SetSoundVolume(Sound sound, float volume)
  SUBROUTINE set_sound_volume(sound, volume) BIND(c, name='SetSoundVolume')
    IMPORT :: C_FLOAT, sound_type
    IMPLICIT NONE
    TYPE(sound_type), INTENT(in), VALUE :: sound
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: volume
  END SUBROUTINE set_sound_volume

  ! void SetTargetFPS(int fps)
  SUBROUTINE set_target_fps(fps) BIND(c, name='SetTargetFPS')
    IMPORT :: C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: fps
  END SUBROUTINE set_target_fps

  ! void SetTextLineSpacing(int spacing)
  SUBROUTINE set_text_line_spacing(spacing) BIND(c, name='SetTextLineSpacing')
    IMPORT :: C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: spacing
  END SUBROUTINE set_text_line_spacing

  ! void SetTextureFilter(Texture2D texture, int filter)
        subroutine set_texture_filter(texture, filter) bind(c, name='SetTextureFilter')
    IMPORT :: C_INT, texture2d_type
    IMPLICIT NONE
    TYPE(texture2d_type), INTENT(in), VALUE :: texture
    INTEGER(kind=C_INT), INTENT(in), VALUE :: filter
  END SUBROUTINE set_texture_filter

  ! void SetTextureWrap(Texture2D texture, int wrap)
  SUBROUTINE set_texture_wrap(texture, wrap) BIND(c, name='SetTextureWrap')
    IMPORT :: C_INT, texture2d_type
    IMPLICIT NONE
    TYPE(texture2d_type), INTENT(in), VALUE :: texture
    INTEGER(kind=C_INT), INTENT(in), VALUE :: wrap
  END SUBROUTINE set_texture_wrap

  ! void SetTraceLogCallback(TraceLogCallback callback)
        subroutine set_trace_log_callback(callback) bind(c, name='SetTraceLogCallback')
    IMPORT :: C_FUNPTR
    IMPLICIT NONE
    TYPE(C_FUNPTR), INTENT(in), VALUE :: callback
  END SUBROUTINE set_trace_log_callback

  ! void SetTraceLogLevel(int logLevel)
  SUBROUTINE set_trace_log_level(log_level) BIND(c, name='SetTraceLogLevel')
    IMPORT :: C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: log_level
  END SUBROUTINE set_trace_log_level

  ! void SetWindowFocused(void)
  SUBROUTINE set_window_focused() BIND(c, name='SetWindowFocused')
  END SUBROUTINE set_window_focused

  ! void SetWindowIcon(Image image)
  SUBROUTINE set_window_icon(image) BIND(c, name='SetWindowIcon')
    IMPORT :: image_type
    IMPLICIT NONE
    TYPE(image_type), INTENT(in), VALUE :: image
  END SUBROUTINE set_window_icon

  ! void SetWindowIcons(Image *images, int count)
  SUBROUTINE set_window_icons(images, count) BIND(c, name='SetWindowIcons')
    IMPORT :: C_INT, image_type
    IMPLICIT NONE
    TYPE(image_type), INTENT(inout) :: images
    INTEGER(kind=C_INT), INTENT(in), VALUE :: count
  END SUBROUTINE set_window_icons

  ! void SetWindowMaxSize(int width, int height)
SUBROUTINE set_window_max_size(width, height) BIND(c, name='SetWindowMaxSize')
    IMPORT :: C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: width
    INTEGER(kind=C_INT), INTENT(in), VALUE :: height
  END SUBROUTINE set_window_max_size

  ! void SetWindowMinSize(int width, int height)
SUBROUTINE set_window_min_size(width, height) BIND(c, name='SetWindowMinSize')
    IMPORT :: C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: width
    INTEGER(kind=C_INT), INTENT(in), VALUE :: height
  END SUBROUTINE set_window_min_size

  ! void SetWindowMonitor(int monitor)
  SUBROUTINE set_window_monitor(monitor) BIND(c, name='SetWindowMonitor')
    IMPORT :: C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: monitor
  END SUBROUTINE set_window_monitor

  ! void SetWindowOpacity(float opacity)
  SUBROUTINE set_window_opacity(opacity) BIND(c, name='SetWindowOpacity')
    IMPORT :: C_FLOAT
    IMPLICIT NONE
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: opacity
  END SUBROUTINE set_window_opacity

  ! void SetWindowPosition(int x, int y)
  SUBROUTINE set_window_position(x, y) BIND(c, name='SetWindowPosition')
    IMPORT :: C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: x
    INTEGER(kind=C_INT), INTENT(in), VALUE :: y
  END SUBROUTINE set_window_position

  ! void SetWindowSize(int width, int height)
  SUBROUTINE set_window_size(width, height) BIND(c, name='SetWindowSize')
    IMPORT :: C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: width
    INTEGER(kind=C_INT), INTENT(in), VALUE :: height
  END SUBROUTINE set_window_size

  ! void SetWindowState(unsigned int flags)
  SUBROUTINE set_window_state(flags) BIND(c, name='SetWindowState')
    IMPORT :: c_unsigned_int
    IMPLICIT NONE
    INTEGER(kind=c_unsigned_int), INTENT(in), VALUE :: flags
  END SUBROUTINE set_window_state

  ! void SetWindowTitle(const char *title)
  SUBROUTINE set_window_title(title) BIND(c, name='SetWindowTitle')
    IMPORT :: C_CHAR
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: title
  END SUBROUTINE set_window_title

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
