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

PUBLIC :: IsWindowState
PUBLIC :: IsWindowResized
PUBLIC :: IsWindowReady
PUBLIC :: IsWindowMinimized
PUBLIC :: IsWindowMaximized
PUBLIC :: IsWindowHidden
PUBLIC :: IsWindowFullscreen
PUBLIC :: IsWindowFocused
PUBLIC :: IsWaveReady
PUBLIC :: IsTextureReady
PUBLIC :: IsSoundReady
PUBLIC :: IsSoundPlaying
PUBLIC :: IsShaderReady
PUBLIC :: IsRenderTextureReady
PUBLIC :: IsPathFile
PUBLIC :: IsMusicStreamPlaying
PUBLIC :: IsMusicReady
PUBLIC :: IsMouseButtonUp
PUBLIC :: IsMouseButtonReleased
PUBLIC :: IsMouseButtonPressed
PUBLIC :: IsMouseButtonDown
PUBLIC :: IsModelReady
PUBLIC :: IsModelAnimationValid
PUBLIC :: IsMaterialReady
PUBLIC :: IsKeyUp
PUBLIC :: IsKeyReleased
PUBLIC :: IsKeyPressedRepeat
PUBLIC :: IsKeyPressed
PUBLIC :: IsKeyDown
PUBLIC :: IsImageReady
PUBLIC :: IsGestureDetected
PUBLIC :: IsGamepadButtonUp
PUBLIC :: IsGamepadButtonReleased
PUBLIC :: IsGamepadButtonPressed
PUBLIC :: IsGamepadButtonDown
PUBLIC :: IsGamepadAvailable
PUBLIC :: IsFontReady
PUBLIC :: IsFileExtension
PUBLIC :: IsFileDropped
PUBLIC :: IsCursorOnScreen
PUBLIC :: IsCursorHidden
PUBLIC :: IsAudioStreamReady
PUBLIC :: IsAudioStreamProcessed
PUBLIC :: IsAudioStreamPlaying
PUBLIC :: IsAudioDeviceReady

INTERFACE
  ! bool IsAudioDeviceReady(void)
  FUNCTION IsAudioDeviceReady() BIND(c, name='IsAudioDeviceReady')
    IMPORT :: C_BOOL
    IMPLICIT NONE
    LOGICAL(kind=C_BOOL) :: IsAudioDeviceReady
  END FUNCTION IsAudioDeviceReady

  ! bool IsAudioStreamPlaying(AudioStream stream)
  FUNCTION IsAudioStreamPlaying(stream) BIND(c, name='IsAudioStreamPlaying')
    IMPORT :: audio_stream_type, C_BOOL
    IMPLICIT NONE
    TYPE(audio_stream_type), INTENT(in), VALUE :: stream
    LOGICAL(kind=C_BOOL) :: IsAudioStreamPlaying
  END FUNCTION IsAudioStreamPlaying

  ! bool IsAudioStreamProcessed(AudioStream stream)
FUNCTION IsAudioStreamProcessed(stream) BIND(c, name='IsAudioStreamProcessed')
    IMPORT :: audio_stream_type, C_BOOL
    IMPLICIT NONE
    TYPE(audio_stream_type), INTENT(in), VALUE :: stream
    LOGICAL(kind=C_BOOL) :: IsAudioStreamProcessed
  END FUNCTION IsAudioStreamProcessed

  ! bool IsAudioStreamReady(AudioStream stream)
  FUNCTION IsAudioStreamReady(stream) BIND(c, name='IsAudioStreamReady')
    IMPORT :: audio_stream_type, C_BOOL
    IMPLICIT NONE
    TYPE(audio_stream_type), INTENT(in), VALUE :: stream
    LOGICAL(kind=C_BOOL) :: IsAudioStreamReady
  END FUNCTION IsAudioStreamReady

  ! bool IsCursorHidden(void)
  FUNCTION IsCursorHidden() BIND(c, name='IsCursorHidden')
    IMPORT :: C_BOOL
    IMPLICIT NONE
    LOGICAL(kind=C_BOOL) :: IsCursorHidden
  END FUNCTION IsCursorHidden

  ! bool IsCursorOnScreen(void)
  FUNCTION IsCursorOnScreen() BIND(c, name='IsCursorOnScreen')
    IMPORT :: C_BOOL
    IMPLICIT NONE
    LOGICAL(kind=C_BOOL) :: IsCursorOnScreen
  END FUNCTION IsCursorOnScreen

  ! bool IsFileDropped(void)
  FUNCTION IsFileDropped() BIND(c, name='IsFileDropped')
    IMPORT :: C_BOOL
    IMPLICIT NONE
    LOGICAL(kind=C_BOOL) :: IsFileDropped
  END FUNCTION IsFileDropped

  ! bool IsFileExtension(const char *fileName, const char *ext)
  FUNCTION IsFileExtension(file_name, ext) BIND(c, name='IsFileExtension')
    IMPORT :: C_BOOL, C_CHAR
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: file_name
    CHARACTER(kind=C_CHAR), INTENT(in) :: ext
    LOGICAL(kind=C_BOOL) :: IsFileExtension
  END FUNCTION IsFileExtension

  ! bool IsFontReady(Font font)
  FUNCTION IsFontReady(font) BIND(c, name='IsFontReady')
    IMPORT :: C_BOOL, font_type
    IMPLICIT NONE
    TYPE(font_type), INTENT(in), VALUE :: font
    LOGICAL(kind=C_BOOL) :: IsFontReady
  END FUNCTION IsFontReady

  ! bool IsGamepadAvailable(int gamepad)
  FUNCTION IsGamepadAvailable(gamepad) BIND(c, name='IsGamepadAvailable')
    IMPORT :: C_BOOL, C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: gamepad
    LOGICAL(kind=C_BOOL) :: IsGamepadAvailable
  END FUNCTION IsGamepadAvailable

  ! bool IsGamepadButtonDown(int gamepad, int button)
        function IsGamepadButtonDown(gamepad, button) bind(c, name='IsGamepadButtonDown')
    IMPORT :: C_BOOL, C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: gamepad
    INTEGER(kind=C_INT), INTENT(in), VALUE :: button
    LOGICAL(kind=C_BOOL) :: IsGamepadButtonDown
  END FUNCTION IsGamepadButtonDown

  ! bool IsGamepadButtonPressed(int gamepad, int button)
        function IsGamepadButtonPressed(gamepad, button) bind(c, name='IsGamepadButtonPressed')
    IMPORT :: C_BOOL, C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: gamepad
    INTEGER(kind=C_INT), INTENT(in), VALUE :: button
    LOGICAL(kind=C_BOOL) :: IsGamepadButtonPressed
  END FUNCTION IsGamepadButtonPressed

  ! bool IsGamepadButtonReleased(int gamepad, int button)
        function IsGamepadButtonReleased(gamepad, button) bind(c, name='IsGamepadButtonReleased')
    IMPORT :: C_BOOL, C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: gamepad
    INTEGER(kind=C_INT), INTENT(in), VALUE :: button
    LOGICAL(kind=C_BOOL) :: IsGamepadButtonReleased
  END FUNCTION IsGamepadButtonReleased

  ! bool IsGamepadButtonUp(int gamepad, int button)
 FUNCTION IsGamepadButtonUp(gamepad, button) BIND(c, name='IsGamepadButtonUp')
    IMPORT :: C_BOOL, C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: gamepad
    INTEGER(kind=C_INT), INTENT(in), VALUE :: button
    LOGICAL(kind=C_BOOL) :: IsGamepadButtonUp
  END FUNCTION IsGamepadButtonUp

  ! bool IsGestureDetected(unsigned int gesture)
  FUNCTION IsGestureDetected(gesture) BIND(c, name='IsGestureDetected')
    IMPORT :: C_BOOL, c_unsigned_int
    IMPLICIT NONE
    INTEGER(kind=c_unsigned_int), INTENT(in), VALUE :: gesture
    LOGICAL(kind=C_BOOL) :: IsGestureDetected
  END FUNCTION IsGestureDetected

  ! bool IsImageReady(Image image)
  FUNCTION IsImageReady(image) BIND(c, name='IsImageReady')
    IMPORT :: C_BOOL, image_type
    IMPLICIT NONE
    TYPE(image_type), INTENT(in), VALUE :: image
    LOGICAL(kind=C_BOOL) :: IsImageReady
  END FUNCTION IsImageReady

  ! bool IsKeyDown(int key)
  FUNCTION IsKeyDown(key) BIND(c, name='IsKeyDown')
    IMPORT :: C_BOOL, C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: key
    LOGICAL(kind=C_BOOL) :: IsKeyDown
  END FUNCTION IsKeyDown

  ! bool IsKeyPressed(int key)
  FUNCTION IsKeyPressed(key) BIND(c, name='IsKeyPressed')
    IMPORT :: C_BOOL, C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: key
    LOGICAL(kind=C_BOOL) :: IsKeyPressed
  END FUNCTION IsKeyPressed

  ! bool IsKeyPressedRepeat(int key)
  FUNCTION IsKeyPressedRepeat(key) BIND(c, name='IsKeyPressedRepeat')
    IMPORT :: C_BOOL, C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: key
    LOGICAL(kind=C_BOOL) :: IsKeyPressedRepeat
  END FUNCTION IsKeyPressedRepeat

  ! bool IsKeyReleased(int key)
  FUNCTION IsKeyReleased(key) BIND(c, name='IsKeyReleased')
    IMPORT :: C_BOOL, C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: key
    LOGICAL(kind=C_BOOL) :: IsKeyReleased
  END FUNCTION IsKeyReleased

  ! bool IsKeyUp(int key)
  FUNCTION IsKeyUp(key) BIND(c, name='IsKeyUp')
    IMPORT :: C_BOOL, C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: key
    LOGICAL(kind=C_BOOL) :: IsKeyUp
  END FUNCTION IsKeyUp

  ! bool IsMaterialReady(Material material)
  FUNCTION IsMaterialReady(material) BIND(c, name='IsMaterialReady')
    IMPORT :: C_BOOL, material_type
    IMPLICIT NONE
    TYPE(material_type), INTENT(in), VALUE :: material
    LOGICAL(kind=C_BOOL) :: IsMaterialReady
  END FUNCTION IsMaterialReady

  ! bool IsModelAnimationValid(Model model, ModelAnimation anim)
        function IsModelAnimationValid(model, anim) bind(c, name='IsModelAnimationValid')
    IMPORT :: C_BOOL, model_animation_type, model_type
    IMPLICIT NONE
    TYPE(model_type), INTENT(in), VALUE :: model
    TYPE(model_animation_type), INTENT(in), VALUE :: anim
    LOGICAL(kind=C_BOOL) :: IsModelAnimationValid
  END FUNCTION IsModelAnimationValid

  ! bool IsModelReady(Model model)
  FUNCTION IsModelReady(model) BIND(c, name='IsModelReady')
    IMPORT :: C_BOOL, model_type
    IMPLICIT NONE
    TYPE(model_type), INTENT(in), VALUE :: model
    LOGICAL(kind=C_BOOL) :: IsModelReady
  END FUNCTION IsModelReady

  ! bool IsMouseButtonDown(int button)
  FUNCTION IsMouseButtonDown(button) BIND(c, name='IsMouseButtonDown')
    IMPORT :: C_BOOL, C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: button
    LOGICAL(kind=C_BOOL) :: IsMouseButtonDown
  END FUNCTION IsMouseButtonDown

  ! bool IsMouseButtonPressed(int button)
  FUNCTION IsMouseButtonPressed(button) BIND(c, name='IsMouseButtonPressed')
    IMPORT :: C_BOOL, C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: button
    LOGICAL(kind=C_BOOL) :: IsMouseButtonPressed
  END FUNCTION IsMouseButtonPressed

  ! bool IsMouseButtonReleased(int button)
  FUNCTION IsMouseButtonReleased(button) BIND(c, name='IsMouseButtonReleased')
    IMPORT :: C_BOOL, C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: button
    LOGICAL(kind=C_BOOL) :: IsMouseButtonReleased
  END FUNCTION IsMouseButtonReleased

  ! bool IsMouseButtonUp(int button)
  FUNCTION IsMouseButtonUp(button) BIND(c, name='IsMouseButtonUp')
    IMPORT :: C_BOOL, C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: button
    LOGICAL(kind=C_BOOL) :: IsMouseButtonUp
  END FUNCTION IsMouseButtonUp

  ! bool IsMusicReady(Music music)
  FUNCTION IsMusicReady(music) BIND(c, name='IsMusicReady')
    IMPORT :: C_BOOL, music_type
    IMPLICIT NONE
    TYPE(music_type), INTENT(in), VALUE :: music
    LOGICAL(kind=C_BOOL) :: IsMusicReady
  END FUNCTION IsMusicReady

  ! bool IsMusicStreamPlaying(Music music)
  FUNCTION IsMusicStreamPlaying(music) BIND(c, name='IsMusicStreamPlaying')
    IMPORT :: C_BOOL, music_type
    IMPLICIT NONE
    TYPE(music_type), INTENT(in), VALUE :: music
    LOGICAL(kind=C_BOOL) :: IsMusicStreamPlaying
  END FUNCTION IsMusicStreamPlaying

  ! bool IsPathFile(const char *path)
  FUNCTION IsPathFile(path) BIND(c, name='IsPathFile')
    IMPORT :: C_BOOL, C_CHAR
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: path
    LOGICAL(kind=C_BOOL) :: IsPathFile
  END FUNCTION IsPathFile

  ! bool IsRenderTextureReady(RenderTexture2D target)
  FUNCTION IsRenderTextureReady(TARGET) BIND(c, name='IsRenderTextureReady')
    IMPORT :: C_BOOL, render_texture2d_type
    IMPLICIT NONE
    TYPE(render_texture2d_type), INTENT(in), VALUE :: TARGET
    LOGICAL(kind=C_BOOL) :: IsRenderTextureReady
  END FUNCTION IsRenderTextureReady

  ! bool IsShaderReady(Shader shader)
  FUNCTION IsShaderReady(shader) BIND(c, name='IsShaderReady')
    IMPORT :: C_BOOL, shader_type
    IMPLICIT NONE
    TYPE(shader_type), INTENT(in), VALUE :: shader
    LOGICAL(kind=C_BOOL) :: IsShaderReady
  END FUNCTION IsShaderReady

  ! bool IsSoundPlaying(Sound sound)
  FUNCTION IsSoundPlaying(sound) BIND(c, name='IsSoundPlaying')
    IMPORT :: C_BOOL, sound_type
    IMPLICIT NONE
    TYPE(sound_type), INTENT(in), VALUE :: sound
    LOGICAL(kind=C_BOOL) :: IsSoundPlaying
  END FUNCTION IsSoundPlaying

  ! bool IsSoundReady(Sound sound)
  FUNCTION IsSoundReady(sound) BIND(c, name='IsSoundReady')
    IMPORT :: C_BOOL, sound_type
    IMPLICIT NONE
    TYPE(sound_type), INTENT(in), VALUE :: sound
    LOGICAL(kind=C_BOOL) :: IsSoundReady
  END FUNCTION IsSoundReady

  ! bool IsTextureReady(Texture2D texture)
  FUNCTION IsTextureReady(texture) BIND(c, name='IsTextureReady')
    IMPORT :: C_BOOL, texture2d_type
    IMPLICIT NONE
    TYPE(texture2d_type), INTENT(in), VALUE :: texture
    LOGICAL(kind=C_BOOL) :: IsTextureReady
  END FUNCTION IsTextureReady

  ! bool IsWaveReady(Wave wave)
  FUNCTION IsWaveReady(wave) BIND(c, name='IsWaveReady')
    IMPORT :: C_BOOL, wave_type
    IMPLICIT NONE
    TYPE(wave_type), INTENT(in), VALUE :: wave
    LOGICAL(kind=C_BOOL) :: IsWaveReady
  END FUNCTION IsWaveReady

  ! bool IsWindowFocused(void)
  FUNCTION IsWindowFocused() BIND(c, name='IsWindowFocused')
    IMPORT :: C_BOOL
    IMPLICIT NONE
    LOGICAL(kind=C_BOOL) :: IsWindowFocused
  END FUNCTION IsWindowFocused

  ! bool IsWindowFullscreen(void)
  FUNCTION IsWindowFullscreen() BIND(c, name='IsWindowFullscreen')
    IMPORT :: C_BOOL
    IMPLICIT NONE
    LOGICAL(kind=C_BOOL) :: IsWindowFullscreen
  END FUNCTION IsWindowFullscreen

  ! bool IsWindowHidden(void)
  FUNCTION IsWindowHidden() BIND(c, name='IsWindowHidden')
    IMPORT :: C_BOOL
    IMPLICIT NONE
    LOGICAL(kind=C_BOOL) :: IsWindowHidden
  END FUNCTION IsWindowHidden

  ! bool IsWindowMaximized(void)
  FUNCTION IsWindowMaximized() BIND(c, name='IsWindowMaximized')
    IMPORT :: C_BOOL
    IMPLICIT NONE
    LOGICAL(kind=C_BOOL) :: IsWindowMaximized
  END FUNCTION IsWindowMaximized

  ! bool IsWindowMinimized(void)
  FUNCTION IsWindowMinimized() BIND(c, name='IsWindowMinimized')
    IMPORT :: C_BOOL
    IMPLICIT NONE
    LOGICAL(kind=C_BOOL) :: IsWindowMinimized
  END FUNCTION IsWindowMinimized

  ! bool IsWindowReady(void)
  FUNCTION IsWindowReady() BIND(c, name='IsWindowReady')
    IMPORT :: C_BOOL
    IMPLICIT NONE
    LOGICAL(kind=C_BOOL) :: IsWindowReady
  END FUNCTION IsWindowReady

  ! bool IsWindowResized(void)
  FUNCTION IsWindowResized() BIND(c, name='IsWindowResized')
    IMPORT :: C_BOOL
    IMPLICIT NONE
    LOGICAL(kind=C_BOOL) :: IsWindowResized
  END FUNCTION IsWindowResized

  ! bool IsWindowState(unsigned int flag)
  FUNCTION IsWindowState(flag) BIND(c, name='IsWindowState')
    IMPORT :: C_BOOL, c_unsigned_int
    IMPLICIT NONE
    INTEGER(kind=c_unsigned_int), INTENT(in), VALUE :: flag
    LOGICAL(kind=C_BOOL) :: IsWindowState
  END FUNCTION IsWindowState
END INTERFACE

END MODULE RaylibIsMethods
