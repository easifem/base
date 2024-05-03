! raylib.f90
!
! A collection of auto-generated Fortran 2018 interface bindings to
! raylib 5.1.
!
! Author:  Philipp Engel
! Licence: ISC

MODULE RaylibSetMethods
USE, INTRINSIC :: ISO_C_BINDING
USE RaylibTypes
USE RaylibEnums
IMPLICIT NONE
PRIVATE

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

INTERFACE

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

END INTERFACE

END MODULE RaylibSetMethods
