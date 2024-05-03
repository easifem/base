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

PUBLIC :: AttachAudioMixedProcessor
PUBLIC :: AttachAudioStreamProcessor
PUBLIC :: BeginBlendMode
PUBLIC :: BeginDrawing
PUBLIC :: BeginMode2D
PUBLIC :: BeginMode3D
PUBLIC :: BeginScissorMode
PUBLIC :: BeginShaderMode
PUBLIC :: BeginTextureMode
PUBLIC :: BeginVrStereoMode
PUBLIC :: ChangeDirectory
PUBLIC :: ClearBackground
PUBLIC :: ClearWindowState
PUBLIC :: CloseAudioDevice
PUBLIC :: CloseWindow
PUBLIC :: CodepointToUTF8
PUBLIC :: ColorAlpha
PUBLIC :: ColorAlphaBlend
PUBLIC :: ColorBrightness
PUBLIC :: ColorContrast
PUBLIC :: ColorFromHSV
PUBLIC :: ColorFromNormalized
PUBLIC :: ColorTint
PUBLIC :: ColorToInt
PUBLIC :: CompressData
PUBLIC :: DecodeDataBase64
PUBLIC :: DecompressData
PUBLIC :: DetachAudioMixedProcessor
PUBLIC :: DetachAudioStreamProcessor
PUBLIC :: DirectoryExists
PUBLIC :: DisableCursor
PUBLIC :: DisableEventWaiting
PUBLIC :: EnableCursor
PUBLIC :: EnableEventWaiting
PUBLIC :: EncodeDataBase64
PUBLIC :: EndBlendMode
PUBLIC :: EndDrawing
PUBLIC :: EndMode2D
PUBLIC :: EndMode3D
PUBLIC :: EndScissorMode
PUBLIC :: EndShaderMode
PUBLIC :: EndTextureMode
PUBLIC :: EndVrStereoMode
PUBLIC :: ExportDataAsCode
PUBLIC :: ExportFontAsCode
PUBLIC :: ExportImage
PUBLIC :: ExportImageAsCode

PUBLIC :: ExportImageToMemory
PUBLIC :: ExportMesh
PUBLIC :: ExportWave
PUBLIC :: ExportWaveAsCode
PUBLIC :: fade
PUBLIC :: FileExists
PUBLIC :: HideCursor
PUBLIC :: InitAudioDevice
PUBLIC :: InitWindow
PUBLIC :: MaximizeWindow
PUBLIC :: MeasureText
PUBLIC :: MeasureTextEx
PUBLIC :: MemAlloc
PUBLIC :: MemFree
PUBLIC :: MemRealloc
PUBLIC :: MinimizeWindow
PUBLIC :: OpenURL
PUBLIC :: PauseAudioStream
PUBLIC :: PauseMusicStream
PUBLIC :: PauseSound
PUBLIC :: PlayAudioStream
PUBLIC :: PlayMusicStream
PUBLIC :: PlaySound
PUBLIC :: PollInputEvents
PUBLIC :: RestoreWindow
PUBLIC :: ResumeAudioStream
PUBLIC :: ResumeMusicStream
PUBLIC :: ResumeSound
PUBLIC :: SaveFileData
PUBLIC :: SaveFileText
PUBLIC :: SeekMusicStream
PUBLIC :: ShowCursor
PUBLIC :: StopAudioStream
PUBLIC :: StopMusicStream
PUBLIC :: StopSound
PUBLIC :: SwapScreenBuffer
PUBLIC :: TakeScreenshot

PUBLIC :: TextAppend
PUBLIC :: TextCopy
PUBLIC :: TextFindIndex
PUBLIC :: TextInsert
PUBLIC :: TextIsEqual
PUBLIC :: TextJoin
PUBLIC :: TextLength
PUBLIC :: TextReplace
PUBLIC :: TextSplit
PUBLIC :: TextSubtext
PUBLIC :: TextToInteger
PUBLIC :: TextToLower
PUBLIC :: TextToPascal
PUBLIC :: TextToUpper

PUBLIC :: ToggleBorderlessWindowed
PUBLIC :: ToggleFullscreen
PUBLIC :: TraceLog

PUBLIC :: UpdateAudioStream
PUBLIC :: UpdateCamera
PUBLIC :: UpdateMeshBuffer
PUBLIC :: UpdateModelAnimation
PUBLIC :: UpdateMusicStream
PUBLIC :: UpdateSound
PUBLIC :: UpdateTexture
PUBLIC :: UpdateTextureRec

PUBLIC :: UploadMesh
PUBLIC :: WaitTime
PUBLIC :: WaveCopy
PUBLIC :: WaveCrop
PUBLIC :: WaveFormat
PUBLIC :: WindowShouldClose

PUBLIC :: load_file_data_callback
PUBLIC :: SaveFileData_callback
PUBLIC :: load_file_text_callback
PUBLIC :: SaveFileText_callback
PUBLIC :: TraceLog_callback

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
  FUNCTION SaveFileData_callback(file_name, DATA, bytes_to_write) BIND(c)
    IMPORT :: C_BOOL, C_PTR, c_unsigned_int
    IMPLICIT NONE
    TYPE(C_PTR), INTENT(in), VALUE :: file_name
    TYPE(C_PTR), INTENT(in), VALUE :: DATA
    INTEGER(kind=c_unsigned_int), INTENT(in), VALUE :: bytes_to_write
    LOGICAL(kind=C_BOOL) :: SaveFileData_callback
  END FUNCTION SaveFileData_callback

  ! char *(*LoadFileTextCallback)(const char *fileName)
  FUNCTION load_file_text_callback(file_name) BIND(c)
    IMPORT :: C_PTR, c_unsigned_int
    IMPLICIT NONE
    TYPE(C_PTR), INTENT(in), VALUE :: file_name
    TYPE(C_PTR) :: load_file_text_callback
  END FUNCTION load_file_text_callback

  ! bool (*SaveFileTextCallback)(const char *fileName, char *text)
  FUNCTION SaveFileText_callback(file_name, text) BIND(c)
    IMPORT :: C_BOOL, C_PTR
    IMPLICIT NONE
    TYPE(C_PTR), INTENT(in), VALUE :: file_name
    TYPE(C_PTR), INTENT(in), VALUE :: text
    LOGICAL(kind=C_BOOL) :: SaveFileText_callback
  END FUNCTION SaveFileText_callback

  ! void (*TraceLogCallback)(int logLevel, const char *text, va_list args)
  SUBROUTINE TraceLog_callback(log_level, text, args) BIND(c)
    IMPORT :: C_INT, C_PTR
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: log_level
    TYPE(C_PTR), INTENT(in), VALUE :: text
    TYPE(C_PTR), INTENT(in) :: args(*)
  END SUBROUTINE TraceLog_callback
END INTERFACE

INTERFACE
  ! void AttachAudioMixedProcessor(AudioCallback processor)
        subroutine AttachAudioMixedProcessor(processor) bind(c, name='AttachAudioMixedProcessor')
    IMPORT :: C_FUNPTR
    IMPLICIT NONE
    TYPE(C_FUNPTR), INTENT(in), VALUE :: processor
  END SUBROUTINE AttachAudioMixedProcessor

  ! void AttachAudioStreamProcessor(AudioStream stream, AudioCallback processor)
        subroutine AttachAudioStreamProcessor(stream, processor) bind(c, name='AttachAudioStreamProcessor')
    IMPORT :: audio_stream_type, C_FUNPTR
    IMPLICIT NONE
    TYPE(audio_stream_type), INTENT(in), VALUE :: stream
    TYPE(C_FUNPTR), INTENT(in), VALUE :: processor
  END SUBROUTINE AttachAudioStreamProcessor

  ! void BeginBlendMode(int mode)
  SUBROUTINE BeginBlendMode(mode) BIND(c, name='BeginBlendMode')
    IMPORT :: C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: mode
  END SUBROUTINE BeginBlendMode

  ! void BeginDrawing(void)
  SUBROUTINE BeginDrawing() BIND(c, name='BeginDrawing')
  END SUBROUTINE BeginDrawing

  ! void BeginMode2D(Camera2D camera)
  SUBROUTINE BeginMode2D(camera) BIND(c, name='BeginMode2D')
    IMPORT :: camera2d_type
    IMPLICIT NONE
    TYPE(camera2d_type), INTENT(in), VALUE :: camera
  END SUBROUTINE BeginMode2D

  ! void BeginMode3D(Camera3D camera)
  SUBROUTINE BeginMode3D(camera) BIND(c, name='BeginMode3D')
    IMPORT :: camera3d_type
    IMPLICIT NONE
    TYPE(camera3d_type), INTENT(in), VALUE :: camera
  END SUBROUTINE BeginMode3D

  ! void BeginScissorMode(int x, int y, int width, int height)
        subroutine BeginScissorMode(x, y, width, height) bind(c, name='BeginScissorMode')
    IMPORT :: C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: x
    INTEGER(kind=C_INT), INTENT(in), VALUE :: y
    INTEGER(kind=C_INT), INTENT(in), VALUE :: width
    INTEGER(kind=C_INT), INTENT(in), VALUE :: height
  END SUBROUTINE BeginScissorMode

  ! void BeginShaderMode(Shader shader)
  SUBROUTINE BeginShaderMode(shader) BIND(c, name='BeginShaderMode')
    IMPORT :: shader_type
    IMPLICIT NONE
    TYPE(shader_type), INTENT(in), VALUE :: shader
  END SUBROUTINE BeginShaderMode

  ! void BeginTextureMode(RenderTexture2D target)
  SUBROUTINE BeginTextureMode(TARGET) BIND(c, name='BeginTextureMode')
    IMPORT :: render_texture2d_type
    IMPLICIT NONE
    TYPE(render_texture2d_type), INTENT(in), VALUE :: TARGET
  END SUBROUTINE BeginTextureMode

  ! void BeginVrStereoMode(VrStereoConfig config)
  SUBROUTINE BeginVrStereoMode(config) BIND(c, name='BeginVrStereoMode')
    IMPORT :: vr_stereo_config_type
    IMPLICIT NONE
    TYPE(vr_stereo_config_type), INTENT(in), VALUE :: config
  END SUBROUTINE BeginVrStereoMode

  ! bool ChangeDirectory(const char *dir)
  FUNCTION ChangeDirectory(dir) BIND(c, name='ChangeDirectory')
    IMPORT :: C_BOOL, C_CHAR
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: dir
    LOGICAL(kind=C_BOOL) :: ChangeDirectory
  END FUNCTION ChangeDirectory

  ! void ClearBackground(Color color)
  SUBROUTINE ClearBackground(color) BIND(c, name='ClearBackground')
    IMPORT :: color_type
    IMPLICIT NONE
    TYPE(color_type), INTENT(in), VALUE :: color
  END SUBROUTINE ClearBackground

  ! void ClearWindowState(unsigned int flags)
  SUBROUTINE ClearWindowState(flags) BIND(c, name='ClearWindowState')
    IMPORT :: c_unsigned_int
    IMPLICIT NONE
    INTEGER(kind=c_unsigned_int), INTENT(in), VALUE :: flags
  END SUBROUTINE ClearWindowState

  ! void CloseAudioDevice(void)
  SUBROUTINE CloseAudioDevice() BIND(c, name='CloseAudioDevice')
  END SUBROUTINE CloseAudioDevice

  ! void CloseWindow(void)
  SUBROUTINE CloseWindow() BIND(c, name='CloseWindow')
  END SUBROUTINE CloseWindow

  ! const char *CodepointToUTF8(int codepoint, int *utf8Size)
FUNCTION CodepointToUTF8(codepoint, utf8_size) BIND(c, name='CodepointToUTF8')
    IMPORT :: C_INT, C_PTR
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: codepoint
    INTEGER(kind=C_INT), INTENT(out) :: utf8_size
    TYPE(C_PTR) :: CodepointToUTF8
  END FUNCTION CodepointToUTF8

  ! Color ColorAlpha(Color color, float alpha)
  FUNCTION ColorAlpha(color, alpha) BIND(c, name='ColorAlpha')
    IMPORT :: C_FLOAT, color_type
    IMPLICIT NONE
    TYPE(color_type), INTENT(in), VALUE :: color
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: alpha
    TYPE(color_type) :: ColorAlpha
  END FUNCTION ColorAlpha

  ! Color ColorAlphaBlend(Color dst, Color src, Color tint)
  FUNCTION ColorAlphaBlend(dst, src, tint) BIND(c, name='ColorAlphaBlend')
    IMPORT :: color_type
    IMPLICIT NONE
    TYPE(color_type), INTENT(in), VALUE :: dst
    TYPE(color_type), INTENT(in), VALUE :: src
    TYPE(color_type), INTENT(in), VALUE :: tint
    TYPE(color_type) :: ColorAlphaBlend
  END FUNCTION ColorAlphaBlend

  ! Color ColorBrightness(Color color, float factor)
  FUNCTION ColorBrightness(color, factor) BIND(c, name='ColorBrightness')
    IMPORT :: C_FLOAT, color_type
    IMPLICIT NONE
    TYPE(color_type), INTENT(in), VALUE :: color
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: factor
    TYPE(color_type) :: ColorBrightness
  END FUNCTION ColorBrightness

  ! Color ColorContrast(Color color, float contrast)
  FUNCTION ColorContrast(color, contrast) BIND(c, name='ColorContrast')
    IMPORT :: C_FLOAT, color_type
    IMPLICIT NONE
    TYPE(color_type), INTENT(in), VALUE :: color
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: contrast
    TYPE(color_type) :: ColorContrast
  END FUNCTION ColorContrast

  ! Color ColorFromHSV(float hue, float saturation, float value)
  FUNCTION ColorFromHSV(hue, saturation, VALUE) BIND(c, name='ColorFromHSV')
    IMPORT :: C_FLOAT, color_type
    IMPLICIT NONE
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: hue
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: saturation
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: VALUE
    TYPE(color_type) :: ColorFromHSV
  END FUNCTION ColorFromHSV

  ! Color ColorFromNormalized(Vector4 normalized)
  FUNCTION ColorFromNormalized(normalized) BIND(c, name='ColorFromNormalized')
    IMPORT :: color_type, vector4_type
    IMPLICIT NONE
    TYPE(vector4_type), INTENT(in), VALUE :: normalized
    TYPE(color_type) :: ColorFromNormalized
  END FUNCTION ColorFromNormalized

  ! Color ColorTint(Color color, Color tint)
  FUNCTION ColorTint(color, tint) BIND(c, name='ColorTint')
    IMPORT :: color_type
    IMPLICIT NONE
    TYPE(color_type), INTENT(in), VALUE :: color
    TYPE(color_type), INTENT(in), VALUE :: tint
    TYPE(color_type) :: ColorTint
  END FUNCTION ColorTint

  ! int ColorToInt(Color color)
  FUNCTION ColorToInt(color) BIND(c, name='ColorToInt')
    IMPORT :: C_INT, color_type
    IMPLICIT NONE
    TYPE(color_type), INTENT(in), VALUE :: color
    INTEGER(kind=C_INT) :: ColorToInt
  END FUNCTION ColorToInt

  ! unsigned char *CompressData(const unsigned char *data, int dataSize, int *compDataSize)
        function CompressData(data, data_size, comp_data_size) bind(c, name='CompressData')
    IMPORT :: C_INT, C_PTR, c_unsigned_char
    IMPLICIT NONE
    INTEGER(kind=c_unsigned_char), INTENT(in) :: DATA
    INTEGER(kind=C_INT), INTENT(in), VALUE :: data_size
    INTEGER(kind=C_INT), INTENT(out) :: comp_data_size
    TYPE(C_PTR) :: CompressData
  END FUNCTION CompressData

  ! unsigned char *DecodeDataBase64(const unsigned char *data, int *outputSize)
 FUNCTION DecodeDataBase64(DATA, output_size) BIND(c, name='DecodeDataBase64')
    IMPORT :: C_INT, c_unsigned_char, C_PTR
    IMPLICIT NONE
    INTEGER(kind=c_unsigned_char), INTENT(in) :: DATA
    INTEGER(kind=C_INT), INTENT(out) :: output_size
    TYPE(C_PTR) :: DecodeDataBase64
  END FUNCTION DecodeDataBase64

  ! unsigned char *DecompressData(const unsigned char *compData, int compDataSize, int *dataSize)
        function DecompressData(comp_data, comp_data_size, data_size) bind(c, name='DecompressData')
    IMPORT :: C_INT, C_PTR, c_unsigned_char
    IMPLICIT NONE
    INTEGER(kind=c_unsigned_char), INTENT(in) :: comp_data
    INTEGER(kind=C_INT), INTENT(in), VALUE :: comp_data_size
    INTEGER(kind=C_INT), INTENT(out) :: data_size
    TYPE(C_PTR) :: DecompressData
  END FUNCTION DecompressData

  ! void DetachAudioMixedProcessor(AudioCallback processor)
        subroutine DetachAudioMixedProcessor(processor) bind(c, name='DetachAudioMixedProcessor')
    IMPORT :: C_FUNPTR
    IMPLICIT NONE
    TYPE(C_FUNPTR), INTENT(in), VALUE :: processor
  END SUBROUTINE DetachAudioMixedProcessor

  ! void DetachAudioStreamProcessor(AudioStream stream, AudioCallback processor)
        subroutine DetachAudioStreamProcessor(stream, processor) bind(c, name='DetachAudioStreamProcessor')
    IMPORT :: audio_stream_type, C_FUNPTR
    IMPLICIT NONE
    TYPE(audio_stream_type), INTENT(in), VALUE :: stream
    TYPE(C_FUNPTR), INTENT(in), VALUE :: processor
  END SUBROUTINE DetachAudioStreamProcessor

  ! bool DirectoryExists(const char *dirPath)
  FUNCTION DirectoryExists(dir_path) BIND(c, name='DirectoryExists')
    IMPORT :: C_BOOL, C_CHAR
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: dir_path
    LOGICAL(kind=C_BOOL) :: DirectoryExists
  END FUNCTION DirectoryExists

  ! void DisableCursor(void)
  SUBROUTINE DisableCursor() BIND(c, name='DisableCursor')
  END SUBROUTINE DisableCursor

  ! void DisableEventWaiting(void)
  SUBROUTINE DisableEventWaiting() BIND(c, name='DisableEventWaiting')
  END SUBROUTINE DisableEventWaiting

  ! void EnableCursor(void)
  SUBROUTINE EnableCursor() BIND(c, name='EnableCursor')
  END SUBROUTINE EnableCursor

  ! void EnableEventWaiting(void)
  SUBROUTINE EnableEventWaiting() BIND(c, name='EnableEventWaiting')
  END SUBROUTINE EnableEventWaiting

  ! char *EncodeDataBase64(const unsigned char *data, int dataSize, int *outputSize)
        function EncodeDataBase64(data, data_size, output_size) bind(c, name='EncodeDataBase64')
    IMPORT :: C_INT, c_unsigned_char, C_PTR
    IMPLICIT NONE
    INTEGER(kind=c_unsigned_char), INTENT(in) :: DATA
    INTEGER(kind=C_INT), INTENT(in), VALUE :: data_size
    INTEGER(kind=C_INT), INTENT(out) :: output_size
    TYPE(C_PTR) :: EncodeDataBase64
  END FUNCTION EncodeDataBase64

  ! void EndBlendMode(void)
  SUBROUTINE EndBlendMode() BIND(c, name='EndBlendMode')
  END SUBROUTINE EndBlendMode

  ! void EndDrawing(void)
  SUBROUTINE EndDrawing() BIND(c, name='EndDrawing')
  END SUBROUTINE EndDrawing

  ! void EndMode2D(void)
  SUBROUTINE EndMode2D() BIND(c, name='EndMode2D')
  END SUBROUTINE EndMode2D

  ! void EndMode3D(void)
  SUBROUTINE EndMode3D() BIND(c, name='EndMode3D')
  END SUBROUTINE EndMode3D

  ! void EndScissorMode(void)
  SUBROUTINE EndScissorMode() BIND(c, name='EndScissorMode')
  END SUBROUTINE EndScissorMode

  ! void EndShaderMode(void)
  SUBROUTINE EndShaderMode() BIND(c, name='EndShaderMode')
  END SUBROUTINE EndShaderMode

  ! void EndTextureMode(void)
  SUBROUTINE EndTextureMode() BIND(c, name='EndTextureMode')
  END SUBROUTINE EndTextureMode

  ! void EndVrStereoMode(void)
  SUBROUTINE EndVrStereoMode() BIND(c, name='EndVrStereoMode')
  END SUBROUTINE EndVrStereoMode

  ! bool ExportDataAsCode(const unsigned char *data, int dataSize, const char *fileName)
        function ExportDataAsCode(data, data_size, file_name) bind(c, name='ExportDataAsCode')
    IMPORT :: C_BOOL, C_CHAR, C_INT, c_unsigned_char
    IMPLICIT NONE
    INTEGER(kind=c_unsigned_char), INTENT(in) :: DATA
    INTEGER(kind=C_INT), INTENT(in), VALUE :: data_size
    CHARACTER(kind=C_CHAR), INTENT(in) :: file_name
    LOGICAL(kind=C_BOOL) :: ExportDataAsCode
  END FUNCTION ExportDataAsCode

  ! bool ExportFontAsCode(Font font, const char *fileName)
  FUNCTION ExportFontAsCode(font, file_name) BIND(c, name='ExportFontAsCode')
    IMPORT :: C_BOOL, C_CHAR, font_type
    IMPLICIT NONE
    TYPE(font_type), INTENT(in), VALUE :: font
    CHARACTER(kind=C_CHAR), INTENT(in) :: file_name
    LOGICAL(kind=C_BOOL) :: ExportFontAsCode
  END FUNCTION ExportFontAsCode

  ! bool ExportImage(Image image, const char *fileName)
  FUNCTION ExportImage(image, file_name) BIND(c, name='ExportImage')
    IMPORT :: C_BOOL, C_CHAR, image_type
    IMPLICIT NONE
    TYPE(image_type), INTENT(in), VALUE :: image
    CHARACTER(kind=C_CHAR), INTENT(in) :: file_name
    LOGICAL(kind=C_BOOL) :: ExportImage
  END FUNCTION ExportImage

  ! bool ExportImageAsCode(Image image, const char *fileName)
FUNCTION ExportImageAsCode(image, file_name) BIND(c, name='ExportImageAsCode')
    IMPORT :: C_BOOL, C_CHAR, image_type
    IMPLICIT NONE
    TYPE(image_type), INTENT(in), VALUE :: image
    CHARACTER(kind=C_CHAR), INTENT(in) :: file_name
    LOGICAL(kind=C_BOOL) :: ExportImageAsCode
  END FUNCTION ExportImageAsCode

  ! unsigned char *ExportImageToMemory(Image image, const char *fileType, int *fileSize)
        function ExportImageToMemory(image, file_type, file_size) bind(c, name='ExportImageToMemory')
    IMPORT :: C_CHAR, C_INT, C_PTR, image_type
    IMPLICIT NONE
    TYPE(image_type), INTENT(in), VALUE :: image
    CHARACTER(kind=C_CHAR), INTENT(in) :: file_type
    INTEGER(kind=C_INT), INTENT(out) :: file_size
    TYPE(C_PTR) :: ExportImageToMemory
  END FUNCTION ExportImageToMemory

  ! bool ExportMesh(Mesh mesh, const char *fileName)
  FUNCTION ExportMesh(mesh, file_name) BIND(c, name='ExportMesh')
    IMPORT :: C_BOOL, C_CHAR, mesh_type
    IMPLICIT NONE
    TYPE(mesh_type), INTENT(in), VALUE :: mesh
    CHARACTER(kind=C_CHAR), INTENT(in) :: file_name
    LOGICAL(kind=C_BOOL) :: ExportMesh
  END FUNCTION ExportMesh

  ! bool ExportWave(Wave wave, const char *fileName)
  FUNCTION ExportWave(wave, file_name) BIND(c, name='ExportWave')
    IMPORT :: C_BOOL, C_CHAR, wave_type
    IMPLICIT NONE
    TYPE(wave_type), INTENT(in), VALUE :: wave
    CHARACTER(kind=C_CHAR), INTENT(in) :: file_name
    LOGICAL(kind=C_BOOL) :: ExportWave
  END FUNCTION ExportWave

  ! bool ExportWaveAsCode(Wave wave, const char *fileName)
  FUNCTION ExportWaveAsCode(wave, file_name) BIND(c, name='ExportWaveAsCode')
    IMPORT :: C_BOOL, C_CHAR, wave_type
    IMPLICIT NONE
    TYPE(wave_type), INTENT(in), VALUE :: wave
    CHARACTER(kind=C_CHAR), INTENT(in) :: file_name
    LOGICAL(kind=C_BOOL) :: ExportWaveAsCode
  END FUNCTION ExportWaveAsCode

  ! Color Fade(Color color, float alpha)
  FUNCTION fade(color, alpha) BIND(c, name='Fade')
    IMPORT :: C_FLOAT, color_type
    IMPLICIT NONE
    TYPE(color_type), INTENT(in), VALUE :: color
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: alpha
    TYPE(color_type) :: fade
  END FUNCTION fade

  ! bool FileExists(const char *fileName)
  FUNCTION FileExists(file_name) BIND(c, name='FileExists')
    IMPORT :: C_BOOL, C_CHAR
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: file_name
    LOGICAL(kind=C_BOOL) :: FileExists
  END FUNCTION FileExists

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
  SUBROUTINE HideCursor() BIND(c, name='HideCursor')
  END SUBROUTINE HideCursor

  ! void InitAudioDevice(void)
  SUBROUTINE InitAudioDevice() BIND(c, name='InitAudioDevice')
  END SUBROUTINE InitAudioDevice

  ! void InitWindow(int width, int height, const char *title)
  SUBROUTINE InitWindow(width, height, title) BIND(c, name='InitWindow')
    IMPORT :: C_CHAR, C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: width
    INTEGER(kind=C_INT), INTENT(in), VALUE :: height
    CHARACTER(kind=C_CHAR), INTENT(in) :: title
  END SUBROUTINE InitWindow

  ! void MaximizeWindow(void)
  SUBROUTINE MaximizeWindow() BIND(c, name='MaximizeWindow')
  END SUBROUTINE MaximizeWindow

  ! int MeasureText(const char *text, int fontSize)
  FUNCTION MeasureText(text, font_size) BIND(c, name='MeasureText')
    IMPORT :: C_CHAR, C_INT
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: text
    INTEGER(kind=C_INT), INTENT(in), VALUE :: font_size
    INTEGER(kind=C_INT) :: MeasureText
  END FUNCTION MeasureText

  ! Vector2 MeasureTextEx(Font font, const char *text, float fontSize, float spacing)
        function MeasureTextEx(font, text, font_size, spacing) bind(c, name='MeasureTextEx')
    IMPORT :: C_CHAR, C_FLOAT, font_type, vector2_type
    IMPLICIT NONE
    TYPE(font_type), INTENT(in), VALUE :: font
    CHARACTER(kind=C_CHAR), INTENT(in) :: text
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: font_size
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: spacing
    TYPE(vector2_type) :: MeasureTextEx
  END FUNCTION MeasureTextEx

  ! void *MemAlloc(unsigned int size)
  FUNCTION MemAlloc(size) BIND(c, name='MemAlloc')
    IMPORT :: C_PTR, c_unsigned_int
    IMPLICIT NONE
    INTEGER(kind=c_unsigned_int), INTENT(in), VALUE :: size
    TYPE(C_PTR) :: MemAlloc
  END FUNCTION MemAlloc

  ! void MemFree(void *ptr)
  SUBROUTINE MemFree(ptr) BIND(c, name='MemFree')
    IMPORT :: C_PTR
    IMPLICIT NONE
    TYPE(C_PTR), INTENT(in), VALUE :: ptr
  END SUBROUTINE MemFree

  ! void *MemRealloc(void *ptr, unsigned int size)
  FUNCTION MemRealloc(ptr, size) BIND(c, name='MemRealloc')
    IMPORT :: C_PTR, c_unsigned_int
    IMPLICIT NONE
    TYPE(C_PTR), INTENT(in), VALUE :: ptr
    INTEGER(kind=c_unsigned_int), INTENT(in), VALUE :: size
    TYPE(C_PTR) :: MemRealloc
  END FUNCTION MemRealloc

  ! void MinimizeWindow(void)
  SUBROUTINE MinimizeWindow() BIND(c, name='MinimizeWindow')
  END SUBROUTINE MinimizeWindow

  ! void OpenURL(const char *url)
  SUBROUTINE OpenURL(url) BIND(c, name='OpenURL')
    IMPORT :: C_CHAR
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: url
  END SUBROUTINE OpenURL

  ! void PauseAudioStream(AudioStream stream)
  SUBROUTINE PauseAudioStream(stream) BIND(c, name='PauseAudioStream')
    IMPORT :: audio_stream_type
    IMPLICIT NONE
    TYPE(audio_stream_type), INTENT(in), VALUE :: stream
  END SUBROUTINE PauseAudioStream

  ! void PauseMusicStream(Music music)
  SUBROUTINE PauseMusicStream(music) BIND(c, name='PauseMusicStream')
    IMPORT :: music_type
    IMPLICIT NONE
    TYPE(music_type), INTENT(in), VALUE :: music
  END SUBROUTINE PauseMusicStream

  ! void PauseSound(Sound sound)
  SUBROUTINE PauseSound(sound) BIND(c, name='PauseSound')
    IMPORT :: sound_type
    IMPLICIT NONE
    TYPE(sound_type), INTENT(in), VALUE :: sound
  END SUBROUTINE PauseSound

  ! void PlayAudioStream(AudioStream stream)
  SUBROUTINE PlayAudioStream(stream) BIND(c, name='PlayAudioStream')
    IMPORT :: audio_stream_type
    IMPLICIT NONE
    TYPE(audio_stream_type), INTENT(in), VALUE :: stream
  END SUBROUTINE PlayAudioStream

  ! void PlayMusicStream(Music music)
  SUBROUTINE PlayMusicStream(music) BIND(c, name='PlayMusicStream')
    IMPORT :: music_type
    IMPLICIT NONE
    TYPE(music_type), INTENT(in), VALUE :: music
  END SUBROUTINE PlayMusicStream

  ! void PlaySound(Sound sound)
  SUBROUTINE PlaySound(sound) BIND(c, name='PlaySound')
    IMPORT :: sound_type
    IMPLICIT NONE
    TYPE(sound_type), INTENT(in), VALUE :: sound
  END SUBROUTINE PlaySound

  ! void PollInputEvents(void)
  SUBROUTINE PollInputEvents() BIND(c, name='PollInputEvents')
  END SUBROUTINE PollInputEvents

  ! void RestoreWindow(void)
  SUBROUTINE RestoreWindow() BIND(c, name='RestoreWindow')
  END SUBROUTINE RestoreWindow

  ! void ResumeAudioStream(AudioStream stream)
  SUBROUTINE ResumeAudioStream(stream) BIND(c, name='ResumeAudioStream')
    IMPORT :: audio_stream_type
    IMPLICIT NONE
    TYPE(audio_stream_type), INTENT(in), VALUE :: stream
  END SUBROUTINE ResumeAudioStream

  ! void ResumeMusicStream(Music music)
  SUBROUTINE ResumeMusicStream(music) BIND(c, name='ResumeMusicStream')
    IMPORT :: music_type
    IMPLICIT NONE
    TYPE(music_type), INTENT(in), VALUE :: music
  END SUBROUTINE ResumeMusicStream

  ! void ResumeSound(Sound sound)
  SUBROUTINE ResumeSound(sound) BIND(c, name='ResumeSound')
    IMPORT :: sound_type
    IMPLICIT NONE
    TYPE(sound_type), INTENT(in), VALUE :: sound
  END SUBROUTINE ResumeSound

  ! bool SaveFileData(const char *fileName, void *data, int dataSize)
FUNCTION SaveFileData(file_name, DATA, data_size) BIND(c, name='SaveFileData')
    IMPORT :: C_BOOL, C_CHAR, C_INT, C_PTR
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: file_name
    TYPE(C_PTR), INTENT(in), VALUE :: DATA
    INTEGER(kind=C_INT), INTENT(in), VALUE :: data_size
    LOGICAL(kind=C_BOOL) :: SaveFileData
  END FUNCTION SaveFileData

  ! bool SaveFileText(const char *fileName, char *text)
  FUNCTION SaveFileText(file_name, text) BIND(c, name='SaveFileText')
    IMPORT :: C_BOOL, C_CHAR
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: file_name
    CHARACTER(kind=C_CHAR), INTENT(in) :: text
    LOGICAL(kind=C_BOOL) :: SaveFileText
  END FUNCTION SaveFileText

  ! void SeekMusicStream(Music music, float position)
  SUBROUTINE SeekMusicStream(music, position) BIND(c, name='SeekMusicStream')
    IMPORT :: C_FLOAT, music_type
    IMPLICIT NONE
    TYPE(music_type), INTENT(in), VALUE :: music
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: position
  END SUBROUTINE SeekMusicStream

  ! void ShowCursor(void)
  SUBROUTINE ShowCursor() BIND(c, name='ShowCursor')
  END SUBROUTINE ShowCursor

  ! void StopAudioStream(AudioStream stream)
  SUBROUTINE StopAudioStream(stream) BIND(c, name='StopAudioStream')
    IMPORT :: audio_stream_type
    IMPLICIT NONE
    TYPE(audio_stream_type), INTENT(in), VALUE :: stream
  END SUBROUTINE StopAudioStream

  ! void StopMusicStream(Music music)
  SUBROUTINE StopMusicStream(music) BIND(c, name='StopMusicStream')
    IMPORT :: music_type
    IMPLICIT NONE
    TYPE(music_type), INTENT(in), VALUE :: music
  END SUBROUTINE StopMusicStream

  ! void StopSound(Sound sound)
  SUBROUTINE StopSound(sound) BIND(c, name='StopSound')
    IMPORT :: sound_type
    IMPLICIT NONE
    TYPE(sound_type), INTENT(in), VALUE :: sound
  END SUBROUTINE StopSound

  ! void SwapScreenBuffer(void)
  SUBROUTINE SwapScreenBuffer() BIND(c, name='SwapScreenBuffer')
  END SUBROUTINE SwapScreenBuffer

  ! void TakeScreenshot(const char *fileName)
  SUBROUTINE TakeScreenshot(file_name) BIND(c, name='TakeScreenshot')
    IMPORT :: C_CHAR
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: file_name
  END SUBROUTINE TakeScreenshot

  ! void TextAppend(char *text, const char *append, int *position)
  SUBROUTINE TextAppend(text, append, position) BIND(c, name='TextAppend')
    IMPORT :: C_CHAR, C_INT
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: text
    CHARACTER(kind=C_CHAR), INTENT(in) :: append
    INTEGER(kind=C_INT), INTENT(in) :: position
  END SUBROUTINE TextAppend

  ! int TextCopy(char *dst, const char *src)
  FUNCTION TextCopy(dst, src) BIND(c, name='TextCopy')
    IMPORT :: C_CHAR, C_INT
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: dst
    CHARACTER(kind=C_CHAR), INTENT(in) :: src
    INTEGER(kind=C_INT) :: TextCopy
  END FUNCTION TextCopy

  ! int TextFindIndex(const char *text, const char *find)
  FUNCTION TextFindIndex(text, find) BIND(c, name='TextFindIndex')
    IMPORT :: C_CHAR, C_INT
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: text
    CHARACTER(kind=C_CHAR), INTENT(in) :: find
    INTEGER(kind=C_INT) :: TextFindIndex
  END FUNCTION TextFindIndex

  ! char *TextInsert(const char *text, const char *insert, int position)
  FUNCTION TextInsert(text, insert, position) BIND(c, name='TextInsert')
    IMPORT :: C_CHAR, C_INT, C_PTR
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: text
    CHARACTER(kind=C_CHAR), INTENT(in) :: insert
    INTEGER(kind=C_INT), INTENT(in), VALUE :: position
    TYPE(C_PTR) :: TextInsert
  END FUNCTION TextInsert

  ! bool TextIsEqual(const char *text1, const char *text2)
  FUNCTION TextIsEqual(text1, text2) BIND(c, name='TextIsEqual')
    IMPORT :: C_BOOL, C_CHAR
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: text1
    CHARACTER(kind=C_CHAR), INTENT(in) :: text2
    LOGICAL(kind=C_BOOL) :: TextIsEqual
  END FUNCTION TextIsEqual

  ! const char *TextJoin(const char **textList, int count, const char *delimiter)
  FUNCTION TextJoin(text_list, count, delimiter) BIND(c, name='TextJoin')
    IMPORT :: C_CHAR, C_INT, C_PTR
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: text_list(*)
    INTEGER(kind=C_INT), INTENT(in), VALUE :: count
    CHARACTER(kind=C_CHAR), INTENT(in) :: delimiter
    TYPE(C_PTR) :: TextJoin
  END FUNCTION TextJoin

  ! unsigned int TextLength(const char *text)
  FUNCTION TextLength(text) BIND(c, name='TextLength')
    IMPORT :: C_CHAR, c_unsigned_int
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: text
    INTEGER(kind=c_unsigned_int) :: TextLength
  END FUNCTION TextLength

  ! char *TextReplace(char *text, const char *replace, const char *by)
  FUNCTION TextReplace(text, replace, by) BIND(c, name='TextReplace')
    IMPORT :: C_CHAR, C_PTR
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: text
    CHARACTER(kind=C_CHAR), INTENT(in) :: replace
    CHARACTER(kind=C_CHAR), INTENT(in) :: by
    TYPE(C_PTR) :: TextReplace
  END FUNCTION TextReplace

  ! const char **TextSplit(const char *text, char delimiter, int *count)
  FUNCTION TextSplit(text, delimiter, count) BIND(c, name='TextSplit')
    IMPORT :: C_CHAR, C_INT, C_PTR
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: text
    CHARACTER(kind=C_CHAR), INTENT(in), VALUE :: delimiter
    INTEGER(kind=C_INT), INTENT(out) :: count
    TYPE(C_PTR) :: TextSplit
  END FUNCTION TextSplit

  ! const char *TextSubtext(const char *text, int position, int length)
  FUNCTION TextSubtext(text, position, length) BIND(c, name='TextSubtext')
    IMPORT :: C_CHAR, C_INT, C_PTR
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: text
    INTEGER(kind=C_INT), INTENT(in), VALUE :: position
    INTEGER(kind=C_INT), INTENT(in), VALUE :: length
    TYPE(C_PTR) :: TextSubtext
  END FUNCTION TextSubtext

  ! int TextToInteger(const char *text)
  FUNCTION TextToInteger(text) BIND(c, name='TextToInteger')
    IMPORT :: C_CHAR, C_INT
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: text
    INTEGER(kind=C_INT) :: TextToInteger
  END FUNCTION TextToInteger

  ! const char *TextToLower(const char *text)
  FUNCTION TextToLower(text) BIND(c, name='TextToLower')
    IMPORT :: C_CHAR, C_PTR
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: text
    TYPE(C_PTR) :: TextToLower
  END FUNCTION TextToLower

  ! const char *TextToPascal(const char *text)
  FUNCTION TextToPascal(text) BIND(c, name='TextToPascal')
    IMPORT :: C_CHAR, C_PTR
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: text
    TYPE(C_PTR) :: TextToPascal
  END FUNCTION TextToPascal

  ! const char *TextToUpper(const char *text)
  FUNCTION TextToUpper(text) BIND(c, name='TextToUpper')
    IMPORT :: C_CHAR, C_PTR
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: text
    TYPE(C_PTR) :: TextToUpper
  END FUNCTION TextToUpper

  ! void ToggleBorderlessWindowed(void)
SUBROUTINE ToggleBorderlessWindowed() BIND(c, name='ToggleBorderlessWindowed')
  END SUBROUTINE ToggleBorderlessWindowed

  ! void ToggleFullscreen(void)
  SUBROUTINE ToggleFullscreen() BIND(c, name='ToggleFullscreen')
  END SUBROUTINE ToggleFullscreen

  ! void TraceLog(int logLevel, const char *text)
  SUBROUTINE TraceLog(log_level, text) BIND(c, name='TraceLog')
    IMPORT :: C_CHAR, C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: log_level
    CHARACTER(kind=C_CHAR), INTENT(in) :: text
  END SUBROUTINE TraceLog

  ! void UpdateAudioStream(AudioStream stream, const void *data, int frameCount)
        subroutine UpdateAudioStream(stream, data, frame_count) bind(c, name='UpdateAudioStream')
    IMPORT :: audio_stream_type, C_INT, C_PTR
    IMPLICIT NONE
    TYPE(audio_stream_type), INTENT(in), VALUE :: stream
    TYPE(C_PTR), INTENT(in), VALUE :: DATA
    INTEGER(kind=C_INT), INTENT(in), VALUE :: frame_count
  END SUBROUTINE UpdateAudioStream

  ! void UpdateCamera(Camera *camera, int mode)
  SUBROUTINE UpdateCamera(camera, mode) BIND(c, name='UpdateCamera')
    IMPORT :: camera3d_type, C_INT
    IMPLICIT NONE
    TYPE(camera3d_type), INTENT(inout) :: camera
    INTEGER(kind=C_INT), INTENT(in), VALUE :: mode
  END SUBROUTINE UpdateCamera

  ! void UpdateMeshBuffer(Mesh mesh, int index, const void *data, int dataSize, int offset)
        subroutine UpdateMeshBuffer(mesh, index, data, data_size, offset) bind(c, name='UpdateMeshBuffer')
    IMPORT :: C_INT, C_PTR, mesh_type
    IMPLICIT NONE
    TYPE(mesh_type), INTENT(in), VALUE :: mesh
    INTEGER(kind=C_INT), INTENT(in), VALUE :: index
    TYPE(C_PTR), INTENT(in), VALUE :: DATA
    INTEGER(kind=C_INT), INTENT(in), VALUE :: data_size
    INTEGER(kind=C_INT), INTENT(in), VALUE :: offset
  END SUBROUTINE UpdateMeshBuffer

  ! void UpdateModelAnimation(Model model, ModelAnimation anim, int frame)
        subroutine UpdateModelAnimation(model, anim, frame) bind(c, name='UpdateModelAnimation')
    IMPORT :: C_INT, model_animation_type, model_type
    IMPLICIT NONE
    TYPE(model_type), INTENT(in), VALUE :: model
    TYPE(model_animation_type), INTENT(in), VALUE :: anim
    INTEGER(kind=C_INT), INTENT(in), VALUE :: frame
  END SUBROUTINE UpdateModelAnimation

  ! void UpdateMusicStream(Music music)
  SUBROUTINE UpdateMusicStream(music) BIND(c, name='UpdateMusicStream')
    IMPORT :: music_type
    IMPLICIT NONE
    TYPE(music_type), INTENT(in), VALUE :: music
  END SUBROUTINE UpdateMusicStream

  ! void UpdateSound(Sound sound, const void *data, int sampleCount)
 SUBROUTINE UpdateSound(sound, DATA, sample_count) BIND(c, name='UpdateSound')
    IMPORT :: C_INT, C_PTR, sound_type
    IMPLICIT NONE
    TYPE(sound_type), INTENT(in), VALUE :: sound
    TYPE(C_PTR), INTENT(in), VALUE :: DATA
    INTEGER(kind=C_INT), INTENT(in), VALUE :: sample_count
  END SUBROUTINE UpdateSound

  ! void UpdateTexture(Texture2D texture, const void *pixels)
  SUBROUTINE UpdateTexture(texture, pixels) BIND(c, name='UpdateTexture')
    IMPORT :: C_PTR, texture2d_type
    IMPLICIT NONE
    TYPE(texture2d_type), INTENT(in), VALUE :: texture
    TYPE(C_PTR), INTENT(in), VALUE :: pixels
  END SUBROUTINE UpdateTexture

  ! void UpdateTextureRec(Texture2D texture, Rectangle rec, const void *pixels)
        subroutine UpdateTextureRec(texture, rec, pixels) bind(c, name='UpdateTextureRec')
    IMPORT :: C_PTR, rectangle_type, texture2d_type
    IMPLICIT NONE
    TYPE(texture2d_type), INTENT(in), VALUE :: texture
    TYPE(rectangle_type), INTENT(in), VALUE :: rec
    TYPE(C_PTR), INTENT(in), VALUE :: pixels
  END SUBROUTINE UpdateTextureRec

  ! void UploadMesh(Mesh *mesh, bool dynamic)
  SUBROUTINE UploadMesh(mesh, dynamic) BIND(c, name='UploadMesh')
    IMPORT :: C_BOOL, mesh_type
    IMPLICIT NONE
    TYPE(mesh_type), INTENT(inout) :: mesh
    LOGICAL(kind=C_BOOL), INTENT(in), VALUE :: dynamic
  END SUBROUTINE UploadMesh

  ! void WaitTime(double seconds)
  SUBROUTINE WaitTime(seconds) BIND(c, name='WaitTime')
    IMPORT :: C_DOUBLE
    IMPLICIT NONE
    REAL(kind=C_DOUBLE), INTENT(in), VALUE :: seconds
  END SUBROUTINE WaitTime

  ! Wave WaveCopy(Wave wave)
  FUNCTION WaveCopy(wave) BIND(c, name='WaveCopy')
    IMPORT :: wave_type
    IMPLICIT NONE
    TYPE(wave_type), INTENT(in), VALUE :: wave
    TYPE(wave_type) :: WaveCopy
  END FUNCTION WaveCopy

  ! void WaveCrop(Wave *wave, int initSample, int finalSample)
 SUBROUTINE WaveCrop(wave, init_sample, final_sample) BIND(c, name='WaveCrop')
    IMPORT :: C_INT, wave_type
    IMPLICIT NONE
    TYPE(wave_type), INTENT(in) :: wave
    INTEGER(kind=C_INT), INTENT(in), VALUE :: init_sample
    INTEGER(kind=C_INT), INTENT(in), VALUE :: final_sample
  END SUBROUTINE WaveCrop

  ! void WaveFormat(Wave *wave, int sampleRate, int sampleSize, int channels)
        subroutine WaveFormat(wave, sample_rate, sample_size, channels) bind(c, name='WaveFormat')
    IMPORT :: C_INT, wave_type
    IMPLICIT NONE
    TYPE(wave_type), INTENT(in) :: wave
    INTEGER(kind=C_INT), INTENT(in), VALUE :: sample_rate
    INTEGER(kind=C_INT), INTENT(in), VALUE :: sample_size
    INTEGER(kind=C_INT), INTENT(in), VALUE :: channels
  END SUBROUTINE WaveFormat

  ! bool WindowShouldClose(void)
  FUNCTION WindowShouldClose() BIND(c, name='WindowShouldClose')
    IMPORT :: C_BOOL
    IMPLICIT NONE
    LOGICAL(kind=C_BOOL) :: WindowShouldClose
  END FUNCTION WindowShouldClose
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
