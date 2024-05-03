! raylib.f90
!
! A collection of auto-generated Fortran 2018 interface bindings to
! raylib 5.1.
!
! Author:  Philipp Engel
! Licence: ISC

MODULE RaylibIsMethods
USE, INTRINSIC :: ISO_C_BINDING
USE RaylibTypes
USE RaylibEnums
IMPLICIT NONE
PRIVATE

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

INTERFACE
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
END INTERFACE

END MODULE RaylibIsMethods
