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

PUBLIC :: SetWindowTitle
PUBLIC :: SetWindowState
PUBLIC :: SetWindowSize
PUBLIC :: SetWindowPosition
PUBLIC :: SetWindowOpacity
PUBLIC :: SetWindowMonitor
PUBLIC :: SetWindowMinSize
PUBLIC :: SetWindowMaxSize
PUBLIC :: SetWindowIcons
PUBLIC :: SetWindowIcon
PUBLIC :: SetWindowFocused
PUBLIC :: SetTraceLogLevel
PUBLIC :: SetTraceLogCallback
PUBLIC :: SetTextureWrap
PUBLIC :: SetTextureFilter
PUBLIC :: SetTextLineSpacing
PUBLIC :: SetTargetFPS
PUBLIC :: SetSoundVolume
PUBLIC :: SetSoundPitch
PUBLIC :: SetSoundPan
PUBLIC :: SetShapesTexture
PUBLIC :: SetShaderValueV
PUBLIC :: SetShaderValueTexture
PUBLIC :: SetShaderValueMatrix
PUBLIC :: SetShaderValue
PUBLIC :: SetSaveFileTextCallback
PUBLIC :: SetSaveFileDataCallback
PUBLIC :: SetRandomSeed
PUBLIC :: SetPixelColor
PUBLIC :: SetMusicVolume
PUBLIC :: SetMusicPitch
PUBLIC :: SetMusicPan
PUBLIC :: SetMouseScale
PUBLIC :: SetMousePosition
PUBLIC :: SetMouseOffset
PUBLIC :: SetMouseCursor
PUBLIC :: SetModelMeshMaterial
PUBLIC :: SetMaterialTexture
PUBLIC :: SetMasterVolume
PUBLIC :: SetLoadFileTextCallback
PUBLIC :: SetLoadFileDataCallback
PUBLIC :: SetGesturesEnabled
PUBLIC :: SetGamepadMappings
PUBLIC :: SetExitKey
PUBLIC :: SetConfigFlags
PUBLIC :: SetClipboardText
PUBLIC :: SetCameraSmoothZoomControl
PUBLIC :: SetCameraPanControl
PUBLIC :: SetCameraMoveControls
PUBLIC :: SetCameraMode
PUBLIC :: SetCameraAltControl
PUBLIC :: SetAudioStreamVolume
PUBLIC :: SetAudioStreamPitch
PUBLIC :: SetAudioStreamPan
PUBLIC :: SetAudioStreamBufferSizeDefault

INTERFACE

  ! void SetAudioStreamBufferSizeDefault(int size)
        subroutine SetAudioStreamBufferSizeDefault(size) bind(c, name='SetAudioStreamBufferSizeDefault')
    IMPORT :: C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: size
  END SUBROUTINE SetAudioStreamBufferSizeDefault

  ! void SetAudioStreamPan(AudioStream stream, float pan)
  SUBROUTINE SetAudioStreamPan(stream, pan) BIND(c, name='SetAudioStreamPan')
    IMPORT :: audio_stream_type, C_FLOAT
    IMPLICIT NONE
    TYPE(audio_stream_type), INTENT(in), VALUE :: stream
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: pan
  END SUBROUTINE SetAudioStreamPan

  ! void SetAudioStreamPitch(AudioStream stream, float pitch)
        subroutine SetAudioStreamPitch(stream, pitch) bind(c, name='SetAudioStreamPitch')
    IMPORT :: audio_stream_type, C_FLOAT
    IMPLICIT NONE
    TYPE(audio_stream_type), INTENT(in), VALUE :: stream
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: pitch
  END SUBROUTINE SetAudioStreamPitch

  ! void SetAudioStreamVolume(AudioStream stream, float volume)
        subroutine SetAudioStreamVolume(stream, volume) bind(c, name='SetAudioStreamVolume')
    IMPORT :: audio_stream_type, C_FLOAT
    IMPLICIT NONE
    TYPE(audio_stream_type), INTENT(in), VALUE :: stream
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: volume
  END SUBROUTINE SetAudioStreamVolume

  ! void SetCameraAltControl(int keyAlt)
  SUBROUTINE SetCameraAltControl(key_alt) BIND(c, name='SetCameraAltControl')
    IMPORT :: C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: key_alt
  END SUBROUTINE SetCameraAltControl

  ! void SetCameraMode(Camera camera, int mode)
  SUBROUTINE SetCameraMode(camera, mode) BIND(c, name='SetCameraMode')
    IMPORT :: C_INT, camera3d_type
    IMPLICIT NONE
    TYPE(camera3d_type), INTENT(in), VALUE :: camera
    INTEGER(kind=C_INT), INTENT(in), VALUE :: mode
  END SUBROUTINE SetCameraMode

  ! void SetCameraMoveControls(int keyFront, int keyBack, int keyRight, int keyLeft, int keyUp, int keyDown)
        subroutine SetCameraMoveControls(key_front, key_back, key_right, key_left, key_up, key_down) &
    BIND(c, name='SetCameraMoveControls')
    IMPORT :: C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: key_front
    INTEGER(kind=C_INT), INTENT(in), VALUE :: key_back
    INTEGER(kind=C_INT), INTENT(in), VALUE :: key_right
    INTEGER(kind=C_INT), INTENT(in), VALUE :: key_left
    INTEGER(kind=C_INT), INTENT(in), VALUE :: key_up
    INTEGER(kind=C_INT), INTENT(in), VALUE :: key_down
  END SUBROUTINE SetCameraMoveControls

  ! void SetCameraPanControl(int keyPan)
  SUBROUTINE SetCameraPanControl(key_pan) BIND(c, name='SetCameraPanControl')
    IMPORT :: C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: key_pan
  END SUBROUTINE SetCameraPanControl

  ! void SetCameraSmoothZoomControl(int keySmoothZoom)
        subroutine SetCameraSmoothZoomControl(key_smooth_zoom) bind(c, name='SetCameraSmoothZoomControl')
    IMPORT :: C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: key_smooth_zoom
  END SUBROUTINE SetCameraSmoothZoomControl

  ! void SetClipboardText(const char *text)
  SUBROUTINE SetClipboardText(text) BIND(c, name='SetClipboardText')
    IMPORT :: C_CHAR
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: text
  END SUBROUTINE SetClipboardText

  ! void SetConfigFlags(unsigned int flags)
  SUBROUTINE SetConfigFlags(flags) BIND(c, name='SetConfigFlags')
    IMPORT :: c_unsigned_int
    IMPLICIT NONE
    INTEGER(kind=c_unsigned_int), INTENT(in), VALUE :: flags
  END SUBROUTINE SetConfigFlags

  ! void SetExitKey(int key)
  SUBROUTINE SetExitKey(key) BIND(c, name='SetExitKey')
    IMPORT :: C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: key
  END SUBROUTINE SetExitKey

  ! int SetGamepadMappings(const char *mappings)
  FUNCTION SetGamepadMappings(mappings) BIND(c, name='SetGamepadMappings')
    IMPORT :: C_CHAR, C_INT
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: mappings
    INTEGER(kind=C_INT) :: SetGamepadMappings
  END FUNCTION SetGamepadMappings

  ! void SetGesturesEnabled(unsigned int flags)
  SUBROUTINE SetGesturesEnabled(flags) BIND(c, name='SetGesturesEnabled')
    IMPORT :: c_unsigned_int
    IMPLICIT NONE
    INTEGER(kind=c_unsigned_int), INTENT(in), VALUE :: flags
  END SUBROUTINE SetGesturesEnabled

  ! void SetLoadFileDataCallback(LoadFileDataCallback callback)
        subroutine SetLoadFileDataCallback(callback) bind(c, name='SetLoadFileDataCallback')
    IMPORT :: C_FUNPTR
    IMPLICIT NONE
    TYPE(C_FUNPTR), INTENT(in), VALUE :: callback
  END SUBROUTINE SetLoadFileDataCallback

  ! void SetLoadFileTextCallback(LoadFileTextCallback callback)
        subroutine SetLoadFileTextCallback(callback) bind(c, name='SetLoadFileTextCallback')
    IMPORT :: C_FUNPTR
    IMPLICIT NONE
    TYPE(C_FUNPTR), INTENT(in), VALUE :: callback
  END SUBROUTINE SetLoadFileTextCallback

  ! void SetMasterVolume(float volume)
  SUBROUTINE SetMasterVolume(volume) BIND(c, name='SetMasterVolume')
    IMPORT :: C_FLOAT
    IMPLICIT NONE
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: volume
  END SUBROUTINE SetMasterVolume

  ! void SetMaterialTexture(Material *material, int mapType, Texture2D texture)
        subroutine SetMaterialTexture(material, map_type, texture) bind(c, name='SetMaterialTexture')
    IMPORT :: C_INT, material_type, texture2d_type
    IMPLICIT NONE
    TYPE(material_type), INTENT(inout) :: material
    INTEGER(kind=C_INT), INTENT(in), VALUE :: map_type
    TYPE(texture2d_type), INTENT(in), VALUE :: texture
  END SUBROUTINE SetMaterialTexture

  ! void SetModelMeshMaterial(Model *model, int meshId, int materialId)
        subroutine SetModelMeshMaterial(model, mesh_id, material_id) bind(c, name='SetModelMeshMaterial')
    IMPORT :: C_INT, model_type
    IMPLICIT NONE
    TYPE(model_type), INTENT(inout) :: model
    INTEGER(kind=C_INT), INTENT(in), VALUE :: mesh_id
    INTEGER(kind=C_INT), INTENT(in), VALUE :: material_id
  END SUBROUTINE SetModelMeshMaterial

  ! void SetMouseCursor(int cursor)
  SUBROUTINE SetMouseCursor(cursor) BIND(c, name='SetMouseCursor')
    IMPORT :: C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: cursor
  END SUBROUTINE SetMouseCursor

  ! void SetMouseOffset(int offsetX, int offsetY)
  SUBROUTINE SetMouseOffset(offset_x, offset_y) BIND(c, name='SetMouseOffset')
    IMPORT :: C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: offset_x
    INTEGER(kind=C_INT), INTENT(in), VALUE :: offset_y
  END SUBROUTINE SetMouseOffset

  ! void SetMousePosition(int x, int y)
  SUBROUTINE SetMousePosition(x, y) BIND(c, name='SetMousePosition')
    IMPORT :: C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: x
    INTEGER(kind=C_INT), INTENT(in), VALUE :: y
  END SUBROUTINE SetMousePosition

  ! void SetMouseScale(float scaleX, float scaleY)
  SUBROUTINE SetMouseScale(scale_x, scale_y) BIND(c, name='SetMouseScale')
    IMPORT :: C_FLOAT
    IMPLICIT NONE
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: scale_x
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: scale_y
  END SUBROUTINE SetMouseScale

  ! void SetMusicPan(Music music, float pan)
  SUBROUTINE SetMusicPan(music, pan) BIND(c, name='SetMusicPan')
    IMPORT :: C_FLOAT, music_type
    IMPLICIT NONE
    TYPE(music_type), INTENT(in), VALUE :: music
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: pan
  END SUBROUTINE SetMusicPan

  ! void SetMusicPitch(Music music, float pitch)
  SUBROUTINE SetMusicPitch(music, pitch) BIND(c, name='SetMusicPitch')
    IMPORT :: C_FLOAT, music_type
    IMPLICIT NONE
    TYPE(music_type), INTENT(in), VALUE :: music
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: pitch
  END SUBROUTINE SetMusicPitch

  ! void SetMusicVolume(Music music, float volume)
  SUBROUTINE SetMusicVolume(music, volume) BIND(c, name='SetMusicVolume')
    IMPORT :: C_FLOAT, music_type
    IMPLICIT NONE
    TYPE(music_type), INTENT(in), VALUE :: music
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: volume
  END SUBROUTINE SetMusicVolume

  ! void SetPixelColor(void *dstPtr, Color color, int format)
SUBROUTINE SetPixelColor(dst_ptr, color, FORMAT) BIND(c, name='SetPixelColor')
    IMPORT :: C_INT, C_PTR, color_type
    IMPLICIT NONE
    TYPE(C_PTR), INTENT(in), VALUE :: dst_ptr
    TYPE(color_type), INTENT(in), VALUE :: color
    INTEGER(kind=C_INT), INTENT(in), VALUE :: FORMAT
  END SUBROUTINE SetPixelColor

  ! void SetRandomSeed(unsigned int seed)
  SUBROUTINE SetRandomSeed(seed) BIND(c, name='SetRandomSeed')
    IMPORT :: c_unsigned_int
    IMPLICIT NONE
    INTEGER(kind=c_unsigned_int), INTENT(in), VALUE :: seed
  END SUBROUTINE SetRandomSeed

  ! void SetSaveFileDataCallback(SaveFileDataCallback callback)
        subroutine SetSaveFileDataCallback(callback) bind(c, name='SetSaveFileDataCallback')
    IMPORT :: C_FUNPTR
    IMPLICIT NONE
    TYPE(C_FUNPTR), INTENT(in), VALUE :: callback
  END SUBROUTINE SetSaveFileDataCallback

  ! void SetSaveFileTextCallback(SaveFileTextCallback callback)
        subroutine SetSaveFileTextCallback(callback) bind(c, name='SetSaveFileTextCallback')
    IMPORT :: C_FUNPTR
    IMPLICIT NONE
    TYPE(C_FUNPTR), INTENT(in), VALUE :: callback
  END SUBROUTINE SetSaveFileTextCallback

  ! void SetShaderValue(Shader shader, int locIndex, const void *value, int uniformType)
        subroutine SetShaderValue(shader, loc_index, value, uniform_type) bind(c, name='SetShaderValue')
    IMPORT :: C_INT, C_PTR, shader_type
    IMPLICIT NONE
    TYPE(shader_type), INTENT(in), VALUE :: shader
    INTEGER(kind=C_INT), INTENT(in), VALUE :: loc_index
    TYPE(C_PTR), INTENT(in), VALUE :: VALUE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: uniform_type
  END SUBROUTINE SetShaderValue

  ! void SetShaderValueMatrix(Shader shader, int locIndex, Matrix mat)
        subroutine SetShaderValueMatrix(shader, loc_index, mat) bind(c, name='SetShaderValueMatrix')
    IMPORT :: C_INT, matrix_type, shader_type
    IMPLICIT NONE
    TYPE(shader_type), INTENT(in), VALUE :: shader
    INTEGER(kind=C_INT), INTENT(in), VALUE :: loc_index
    TYPE(matrix_type), INTENT(in), VALUE :: mat
  END SUBROUTINE SetShaderValueMatrix

  ! void SetShaderValueTexture(Shader shader, int locIndex, Texture2D texture)
        subroutine SetShaderValueTexture(shader, loc_index, texture) bind(c, name='SetShaderValueTexture')
    IMPORT :: C_INT, shader_type, texture2d_type
    IMPLICIT NONE
    TYPE(shader_type), INTENT(in), VALUE :: shader
    INTEGER(kind=C_INT), INTENT(in), VALUE :: loc_index
    TYPE(texture2d_type), INTENT(in), VALUE :: texture
  END SUBROUTINE SetShaderValueTexture

  ! void SetShaderValueV(Shader shader, int locIndex, const void *value, int uniformType, int count)
        subroutine SetShaderValueV(shader, loc_index, value, uniform_type, count) bind(c, name='SetShaderValueV')
    IMPORT :: C_INT, C_PTR, shader_type
    IMPLICIT NONE
    TYPE(shader_type), INTENT(in), VALUE :: shader
    INTEGER(kind=C_INT), INTENT(in), VALUE :: loc_index
    TYPE(C_PTR), INTENT(in), VALUE :: VALUE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: uniform_type
    INTEGER(kind=C_INT), INTENT(in), VALUE :: count
  END SUBROUTINE SetShaderValueV

  ! void SetShapesTexture(Texture2D texture, Rectangle source)
 SUBROUTINE SetShapesTexture(texture, source) BIND(c, name='SetShapesTexture')
    IMPORT :: rectangle_type, texture2d_type
    IMPLICIT NONE
    TYPE(texture2d_type), INTENT(in), VALUE :: texture
    TYPE(rectangle_type), INTENT(in), VALUE :: source
  END SUBROUTINE SetShapesTexture

  ! void SetSoundPan(Sound sound, float pan)
  SUBROUTINE SetSoundPan(sound, pan) BIND(c, name='SetSoundPan')
    IMPORT :: C_FLOAT, sound_type
    IMPLICIT NONE
    TYPE(sound_type), INTENT(in), VALUE :: sound
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: pan
  END SUBROUTINE SetSoundPan

  ! void SetSoundPitch(Sound sound, float pitch)
  SUBROUTINE SetSoundPitch(sound, pitch) BIND(c, name='SetSoundPitch')
    IMPORT :: C_FLOAT, sound_type
    IMPLICIT NONE
    TYPE(sound_type), INTENT(in), VALUE :: sound
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: pitch
  END SUBROUTINE SetSoundPitch

  ! void SetSoundVolume(Sound sound, float volume)
  SUBROUTINE SetSoundVolume(sound, volume) BIND(c, name='SetSoundVolume')
    IMPORT :: C_FLOAT, sound_type
    IMPLICIT NONE
    TYPE(sound_type), INTENT(in), VALUE :: sound
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: volume
  END SUBROUTINE SetSoundVolume

  ! void SetTargetFPS(int fps)
  SUBROUTINE SetTargetFPS(fps) BIND(c, name='SetTargetFPS')
    IMPORT :: C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: fps
  END SUBROUTINE SetTargetFPS

  ! void SetTextLineSpacing(int spacing)
  SUBROUTINE SetTextLineSpacing(spacing) BIND(c, name='SetTextLineSpacing')
    IMPORT :: C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: spacing
  END SUBROUTINE SetTextLineSpacing

  ! void SetTextureFilter(Texture2D texture, int filter)
 SUBROUTINE SetTextureFilter(texture, filter) BIND(c, name='SetTextureFilter')
    IMPORT :: C_INT, texture2d_type
    IMPLICIT NONE
    TYPE(texture2d_type), INTENT(in), VALUE :: texture
    INTEGER(kind=C_INT), INTENT(in), VALUE :: filter
  END SUBROUTINE SetTextureFilter

  ! void SetTextureWrap(Texture2D texture, int wrap)
  SUBROUTINE SetTextureWrap(texture, wrap) BIND(c, name='SetTextureWrap')
    IMPORT :: C_INT, texture2d_type
    IMPLICIT NONE
    TYPE(texture2d_type), INTENT(in), VALUE :: texture
    INTEGER(kind=C_INT), INTENT(in), VALUE :: wrap
  END SUBROUTINE SetTextureWrap

  ! void SetTraceLogCallback(TraceLogCallback callback)
  SUBROUTINE SetTraceLogCallback(callback) BIND(c, name='SetTraceLogCallback')
    IMPORT :: C_FUNPTR
    IMPLICIT NONE
    TYPE(C_FUNPTR), INTENT(in), VALUE :: callback
  END SUBROUTINE SetTraceLogCallback

  ! void SetTraceLogLevel(int logLevel)
  SUBROUTINE SetTraceLogLevel(log_level) BIND(c, name='SetTraceLogLevel')
    IMPORT :: C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: log_level
  END SUBROUTINE SetTraceLogLevel

  ! void SetWindowFocused(void)
  SUBROUTINE SetWindowFocused() BIND(c, name='SetWindowFocused')
  END SUBROUTINE SetWindowFocused

  ! void SetWindowIcon(Image image)
  SUBROUTINE SetWindowIcon(image) BIND(c, name='SetWindowIcon')
    IMPORT :: image_type
    IMPLICIT NONE
    TYPE(image_type), INTENT(in), VALUE :: image
  END SUBROUTINE SetWindowIcon

  ! void SetWindowIcons(Image *images, int count)
  SUBROUTINE SetWindowIcons(images, count) BIND(c, name='SetWindowIcons')
    IMPORT :: C_INT, image_type
    IMPLICIT NONE
    TYPE(image_type), INTENT(inout) :: images
    INTEGER(kind=C_INT), INTENT(in), VALUE :: count
  END SUBROUTINE SetWindowIcons

  ! void SetWindowMaxSize(int width, int height)
  SUBROUTINE SetWindowMaxSize(width, height) BIND(c, name='SetWindowMaxSize')
    IMPORT :: C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: width
    INTEGER(kind=C_INT), INTENT(in), VALUE :: height
  END SUBROUTINE SetWindowMaxSize

  ! void SetWindowMinSize(int width, int height)
  SUBROUTINE SetWindowMinSize(width, height) BIND(c, name='SetWindowMinSize')
    IMPORT :: C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: width
    INTEGER(kind=C_INT), INTENT(in), VALUE :: height
  END SUBROUTINE SetWindowMinSize

  ! void SetWindowMonitor(int monitor)
  SUBROUTINE SetWindowMonitor(monitor) BIND(c, name='SetWindowMonitor')
    IMPORT :: C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: monitor
  END SUBROUTINE SetWindowMonitor

  ! void SetWindowOpacity(float opacity)
  SUBROUTINE SetWindowOpacity(opacity) BIND(c, name='SetWindowOpacity')
    IMPORT :: C_FLOAT
    IMPLICIT NONE
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: opacity
  END SUBROUTINE SetWindowOpacity

  ! void SetWindowPosition(int x, int y)
  SUBROUTINE SetWindowPosition(x, y) BIND(c, name='SetWindowPosition')
    IMPORT :: C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: x
    INTEGER(kind=C_INT), INTENT(in), VALUE :: y
  END SUBROUTINE SetWindowPosition

  ! void SetWindowSize(int width, int height)
  SUBROUTINE SetWindowSize(width, height) BIND(c, name='SetWindowSize')
    IMPORT :: C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: width
    INTEGER(kind=C_INT), INTENT(in), VALUE :: height
  END SUBROUTINE SetWindowSize

  ! void SetWindowState(unsigned int flags)
  SUBROUTINE SetWindowState(flags) BIND(c, name='SetWindowState')
    IMPORT :: c_unsigned_int
    IMPLICIT NONE
    INTEGER(kind=c_unsigned_int), INTENT(in), VALUE :: flags
  END SUBROUTINE SetWindowState

  ! void SetWindowTitle(const char *title)
  SUBROUTINE SetWindowTitle(title) BIND(c, name='SetWindowTitle')
    IMPORT :: C_CHAR
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: title
  END SUBROUTINE SetWindowTitle

END INTERFACE

END MODULE RaylibSetMethods
