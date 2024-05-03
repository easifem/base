! raylib.f90
!
! A collection of auto-generated Fortran 2018 interface bindings to
! raylib 5.1.
!
! Author:  Philipp Engel
! Licence: ISC

MODULE RaylibLoadMethods
USE, INTRINSIC :: ISO_C_BINDING
USE RaylibTypes
USE RaylibEnums
IMPLICIT NONE
PRIVATE

PUBLIC :: LoadWaveSamples
PUBLIC :: LoadWaveFromMemory
PUBLIC :: LoadWave
PUBLIC :: LoadVrStereoConfig
PUBLIC :: LoadUTF8
PUBLIC :: LoadTextureFromImage
PUBLIC :: LoadTextureCubemap
PUBLIC :: LoadTexture
PUBLIC :: LoadSoundFromWave
PUBLIC :: LoadSoundAlias
PUBLIC :: LoadSound
PUBLIC :: LoadShaderFromMemory
PUBLIC :: LoadShader
PUBLIC :: LoadRenderTexture
PUBLIC :: LoadRandomSequence
PUBLIC :: LoadMusicStreamFromMemory
PUBLIC :: LoadMusicStream
PUBLIC :: LoadModelFromMesh
PUBLIC :: LoadModelAnimations
PUBLIC :: LoadModel
PUBLIC :: LoadMaterials
PUBLIC :: LoadMaterialDefault
PUBLIC :: LoadImageSvg
PUBLIC :: LoadImageRaw
PUBLIC :: LoadImagePalette
PUBLIC :: LoadImageFromTexture
PUBLIC :: LoadImageFromScreen
PUBLIC :: LoadImageFromMemory
PUBLIC :: LoadImageColors
PUBLIC :: LoadImageAnim
PUBLIC :: LoadImage
PUBLIC :: LoadFontFromMemory
PUBLIC :: LoadFontFromImage
PUBLIC :: LoadFontEx
PUBLIC :: LoadFontData
PUBLIC :: LoadFont
PUBLIC :: LoadFileText
PUBLIC :: LoadFileData
PUBLIC :: LoadDroppedFiles
PUBLIC :: LoadDirectoryFilesEx
PUBLIC :: LoadDirectoryFiles
PUBLIC :: LoadCodepoints
PUBLIC :: LoadAudioStream

INTERFACE
  ! AudioStream LoadAudioStream(unsigned int sampleRate, unsigned int sampleSize, unsigned int channels)
        function LoadAudioStream(sample_rate, sample_size, channels) bind(c, name='LoadAudioStream')
    IMPORT :: audio_stream_type, c_unsigned_int
    IMPLICIT NONE
    INTEGER(kind=c_unsigned_int), INTENT(in), VALUE :: sample_rate
    INTEGER(kind=c_unsigned_int), INTENT(in), VALUE :: sample_size
    INTEGER(kind=c_unsigned_int), INTENT(in), VALUE :: channels
    TYPE(audio_stream_type) :: LoadAudioStream
  END FUNCTION LoadAudioStream

  ! int *LoadCodepoints(const char *text, int *count)
  FUNCTION LoadCodepoints(text, count) BIND(c, name='LoadCodepoints')
    IMPORT :: C_CHAR, C_INT, C_PTR
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: text
    INTEGER(kind=C_INT), INTENT(out) :: count
    TYPE(C_PTR) :: LoadCodepoints
  END FUNCTION LoadCodepoints

  ! FilePathList LoadDirectoryFiles(const char *dirPath)
  FUNCTION LoadDirectoryFiles(dir_path) BIND(c, name='LoadDirectoryFiles')
    IMPORT :: C_CHAR, file_path_list_type
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: dir_path
    TYPE(file_path_list_type) :: LoadDirectoryFiles
  END FUNCTION LoadDirectoryFiles

  ! FilePathList LoadDirectoryFilesEx(const char *basePath, const char *filter, bool scanSubdirs)
        function LoadDirectoryFilesEx(base_path, filter, scan_subdirs) bind(c, name='LoadDirectoryFilesEx')
    IMPORT :: C_BOOL, C_CHAR, file_path_list_type
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: base_path
    CHARACTER(kind=C_CHAR), INTENT(in) :: filter
    LOGICAL(kind=C_BOOL), INTENT(in), VALUE :: scan_subdirs
    TYPE(file_path_list_type) :: LoadDirectoryFilesEx
  END FUNCTION LoadDirectoryFilesEx

  ! FilePathList LoadDroppedFiles(void)
  FUNCTION LoadDroppedFiles() BIND(c, name='LoadDroppedFiles')
    IMPORT :: file_path_list_type
    IMPLICIT NONE
    TYPE(file_path_list_type) :: LoadDroppedFiles
  END FUNCTION LoadDroppedFiles

  ! unsigned char *LoadFileData(const char *fileName, int *dataSize)
  FUNCTION LoadFileData(file_name, data_size) BIND(c, name='LoadFileData')
    IMPORT :: C_CHAR, C_INT, C_PTR
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: file_name
    INTEGER(kind=C_INT), INTENT(out) :: data_size
    TYPE(C_PTR) :: LoadFileData
  END FUNCTION LoadFileData

  ! char *LoadFileText(const char *fileName)
  FUNCTION LoadFileText(file_name) BIND(c, name='LoadFileText')
    IMPORT :: C_CHAR, C_PTR
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: file_name
    TYPE(C_PTR) :: LoadFileText
  END FUNCTION LoadFileText

  ! Font LoadFont(const char *fileName)
  FUNCTION LoadFont(file_name) BIND(c, name='LoadFont')
    IMPORT :: C_CHAR, font_type
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: file_name
    TYPE(font_type) :: LoadFont
  END FUNCTION LoadFont

  ! GlyphInfo *LoadFontData(const unsigned char *fileData, int dataSize, int fontSize, int *codepoints, int codepointsCount, int type)
        function LoadFontData(file_data, data_size, font_size, codepoints, codepoints_count, type) &
    BIND(c, name='LoadFontData')
    IMPORT :: C_INT, C_PTR, c_unsigned_char, glyph_info_type
    IMPLICIT NONE
    INTEGER(kind=c_unsigned_char), INTENT(inout) :: file_data
    INTEGER(kind=C_INT), INTENT(in), VALUE :: data_size
    INTEGER(kind=C_INT), INTENT(in), VALUE :: font_size
    INTEGER(kind=C_INT), INTENT(inout) :: codepoints(*)
    INTEGER(kind=C_INT), INTENT(in), VALUE :: codepoints_count
    INTEGER(kind=C_INT), INTENT(in), VALUE :: TYPE
    TYPE(C_PTR) :: LoadFontData
  END FUNCTION LoadFontData

  ! Font LoadFontEx(const char *fileName, int fontSize, int *codepoints, int codepointsCount)
        function LoadFontEx(file_name, font_size, codepoints, codepoints_count) bind(c, name='LoadFontEx')
    IMPORT :: C_CHAR, C_INT, font_type
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: file_name
    INTEGER(kind=C_INT), INTENT(in), VALUE :: font_size
    INTEGER(kind=C_INT), INTENT(inout) :: codepoints(*)
    INTEGER(kind=C_INT), INTENT(in), VALUE :: codepoints_count
    TYPE(font_type) :: LoadFontEx
  END FUNCTION LoadFontEx

  ! Font LoadFontFromImage(Image image, Color key, int firstChar)
        function LoadFontFromImage(image, key, first_char) bind(c, name='LoadFontFromImage')
    IMPORT :: C_INT, color_type, font_type, image_type
    IMPLICIT NONE
    TYPE(image_type), INTENT(in), VALUE :: image
    TYPE(color_type), INTENT(in), VALUE :: key
    INTEGER(kind=C_INT), INTENT(in), VALUE :: first_char
    TYPE(font_type) :: LoadFontFromImage
  END FUNCTION LoadFontFromImage

  ! Font LoadFontFromMemory(const char *fileType, const unsigned char *fileData, int dataSize, int fontSize, int *codepoints, int codepointsCount)
        function LoadFontFromMemory(file_type, file_data, data_size, font_size, codepoints, codepoints_count) &
    BIND(c, name='LoadFontFromMemory')
    IMPORT :: C_CHAR, C_INT, c_unsigned_char, font_type
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: file_type
    INTEGER(kind=c_unsigned_char), INTENT(in) :: file_data
    INTEGER(kind=C_INT), INTENT(in), VALUE :: data_size
    INTEGER(kind=C_INT), INTENT(in), VALUE :: font_size
    INTEGER(kind=C_INT), INTENT(inout) :: codepoints(*)
    INTEGER(kind=C_INT), INTENT(in), VALUE :: codepoints_count
    TYPE(font_type) :: LoadFontFromMemory
  END FUNCTION LoadFontFromMemory

  ! Image LoadImage(const char *fileName)
  FUNCTION LoadImage(file_name) BIND(c, name='LoadImage')
    IMPORT :: C_CHAR, image_type
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: file_name
    TYPE(image_type) :: LoadImage
  END FUNCTION LoadImage

  ! Image LoadImageAnim(const char *fileName, int *frames)
  FUNCTION LoadImageAnim(file_name, frames) BIND(c, name='LoadImageAnim')
    IMPORT :: C_CHAR, C_INT, image_type
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: file_name
    INTEGER(kind=C_INT), INTENT(out) :: frames
    TYPE(image_type) :: LoadImageAnim
  END FUNCTION LoadImageAnim

  ! Color *LoadImageColors(Image image)
  FUNCTION LoadImageColors(image) BIND(c, name='LoadImageColors')
    IMPORT :: C_PTR, image_type
    IMPLICIT NONE
    TYPE(image_type), INTENT(in), VALUE :: image
    TYPE(C_PTR) :: LoadImageColors
  END FUNCTION LoadImageColors

  ! Image LoadImageFromMemory(const char *fileType, const unsigned char *fileData, int dataSize)
        function LoadImageFromMemory(file_type, file_data, data_size) bind(c, name='LoadImageFromMemory')
    IMPORT :: C_CHAR, C_INT, c_unsigned_char, image_type
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: file_type
    INTEGER(kind=c_unsigned_char), INTENT(in) :: file_data
    INTEGER(kind=C_INT), INTENT(in), VALUE :: data_size
    TYPE(image_type) :: LoadImageFromMemory
  END FUNCTION LoadImageFromMemory

  ! Image LoadImageFromScreen(void)
  FUNCTION LoadImageFromScreen() BIND(c, name='LoadImageFromScreen')
    IMPORT :: image_type
    IMPLICIT NONE
    TYPE(image_type) :: LoadImageFromScreen
  END FUNCTION LoadImageFromScreen

  ! Image LoadImageFromTexture(Texture2D texture)
  FUNCTION LoadImageFromTexture(texture) BIND(c, name='LoadImageFromTexture')
    IMPORT :: image_type, texture2d_type
    IMPLICIT NONE
    TYPE(texture2d_type), INTENT(in), VALUE :: texture
    TYPE(image_type) :: LoadImageFromTexture
  END FUNCTION LoadImageFromTexture

  ! Color *LoadImagePalette(Image image, int maxPaletteSize, int *colorCount)
        function LoadImagePalette(image, max_palette_size, color_count) bind(c, name='LoadImagePalette')
    IMPORT :: C_INT, C_PTR, image_type
    IMPLICIT NONE
    TYPE(image_type), INTENT(in), VALUE :: image
    INTEGER(kind=C_INT), INTENT(in), VALUE :: max_palette_size
    INTEGER(kind=C_INT), INTENT(out) :: color_count
    TYPE(C_PTR) :: LoadImagePalette
  END FUNCTION LoadImagePalette

  ! Image LoadImageRaw(const char *fileName, int width, int height, int format, int headerSize)
        function LoadImageRaw(file_name, width, height, format, header_size) bind(c, name='LoadImageRaw')
    IMPORT :: C_CHAR, C_INT, image_type
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: file_name
    INTEGER(kind=C_INT), INTENT(in), VALUE :: width
    INTEGER(kind=C_INT), INTENT(in), VALUE :: height
    INTEGER(kind=C_INT), INTENT(in), VALUE :: FORMAT
    INTEGER(kind=C_INT), INTENT(in), VALUE :: header_size
    TYPE(image_type) :: LoadImageRaw
  END FUNCTION LoadImageRaw

  ! Image LoadImageSvg(const char *fileNameOrString, int width, int height)
        function LoadImageSvg(file_name_or_string, width, height) bind(c, name='LoadImageSvg')
    IMPORT :: C_CHAR, C_INT, image_type
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: file_name_or_string
    INTEGER(kind=C_INT), INTENT(in), VALUE :: width
    INTEGER(kind=C_INT), INTENT(in), VALUE :: height
    TYPE(image_type) :: LoadImageSvg
  END FUNCTION LoadImageSvg

  ! Material LoadMaterialDefault(void)
  FUNCTION LoadMaterialDefault() BIND(c, name='LoadMaterialDefault')
    IMPORT :: material_type
    IMPLICIT NONE
    TYPE(material_type) :: LoadMaterialDefault
  END FUNCTION LoadMaterialDefault

  ! Material *LoadMaterials(const char *fileName, int *materialCount)
        function LoadMaterials(file_name, material_count) bind(c, name='LoadMaterials')
    IMPORT :: C_CHAR, C_INT, C_PTR
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: file_name
    INTEGER(kind=C_INT), INTENT(out) :: material_count
    TYPE(C_PTR) :: LoadMaterials
  END FUNCTION LoadMaterials

  ! Model LoadModel(const char *fileName)
  FUNCTION LoadModel(file_name) BIND(c, name='LoadModel')
    IMPORT :: C_CHAR, model_type
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: file_name
    TYPE(model_type) :: LoadModel
  END FUNCTION LoadModel

  ! ModelAnimation *LoadModelAnimations(const char *fileName, int *animCount)
        function LoadModelAnimations(file_name, anim_count) bind(c, name='LoadModelAnimations')
    IMPORT :: C_CHAR, C_INT, C_PTR
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: file_name
    INTEGER(kind=C_INT), INTENT(out) :: anim_count
    TYPE(C_PTR) :: LoadModelAnimations
  END FUNCTION LoadModelAnimations

  ! Model LoadModelFromMesh(Mesh mesh)
  FUNCTION LoadModelFromMesh(mesh) BIND(c, name='LoadModelFromMesh')
    IMPORT :: mesh_type, model_type
    IMPLICIT NONE
    TYPE(mesh_type), INTENT(in), VALUE :: mesh
    TYPE(model_type) :: LoadModelFromMesh
  END FUNCTION LoadModelFromMesh

  ! Music LoadMusicStream(const char *fileName)
  FUNCTION LoadMusicStream(file_name) BIND(c, name='LoadMusicStream')
    IMPORT :: C_CHAR, music_type
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: file_name
    TYPE(music_type) :: LoadMusicStream
  END FUNCTION LoadMusicStream

  ! Music LoadMusicStreamFromMemory(const char *fileType, const unsigned char *data, int dataSize)
        function LoadMusicStreamFromMemory(file_type, data, data_size) bind(c, name='LoadMusicStreamFromMemory')
    IMPORT :: C_CHAR, C_INT, c_unsigned_char, music_type
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: file_type
    INTEGER(kind=c_unsigned_char), INTENT(in) :: DATA
    INTEGER(kind=C_INT), INTENT(in), VALUE :: data_size
    TYPE(music_type) :: LoadMusicStreamFromMemory
  END FUNCTION LoadMusicStreamFromMemory

  ! int *LoadRandomSequence(unsigned int count, int min, int max)
        function LoadRandomSequence(count, min, max) bind(c, name='LoadRandomSequence')
    IMPORT :: C_INT, C_PTR, c_unsigned_int
    IMPLICIT NONE
    INTEGER(kind=c_unsigned_int), INTENT(in), VALUE :: count
    INTEGER(kind=C_INT), INTENT(in), VALUE :: min
    INTEGER(kind=C_INT), INTENT(in), VALUE :: max
    TYPE(C_PTR) :: LoadRandomSequence
  END FUNCTION LoadRandomSequence

  ! RenderTexture2D LoadRenderTexture(int width, int height)
  FUNCTION LoadRenderTexture(width, height) BIND(c, name='LoadRenderTexture')
    IMPORT :: C_INT, render_texture2d_type
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: width
    INTEGER(kind=C_INT), INTENT(in), VALUE :: height
    TYPE(render_texture2d_type) :: LoadRenderTexture
  END FUNCTION LoadRenderTexture

  ! Shader LoadShader(const char *vsFileName, const char *fsFileName)
  FUNCTION LoadShader(vs_file_name, fs_file_name) BIND(c, name='LoadShader')
    IMPORT :: C_CHAR, shader_type
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: vs_file_name
    CHARACTER(kind=C_CHAR), INTENT(in) :: fs_file_name
    TYPE(shader_type) :: LoadShader
  END FUNCTION LoadShader

  ! Shader LoadShaderFromMemory(const char *vsCode, const char *fsCode)
        function LoadShaderFromMemory(vs_code, fs_code) bind(c, name='LoadShaderFromMemory')
    IMPORT :: C_CHAR, shader_type
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: vs_code
    CHARACTER(kind=C_CHAR), INTENT(in) :: fs_code
    TYPE(shader_type) :: LoadShaderFromMemory
  END FUNCTION LoadShaderFromMemory

  ! Sound LoadSound(const char *fileName)
  FUNCTION LoadSound(file_name) BIND(c, name='LoadSound')
    IMPORT :: C_CHAR, sound_type
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: file_name
    TYPE(sound_type) :: LoadSound
  END FUNCTION LoadSound

  ! Sound LoadSoundAlias(Sound source)
  FUNCTION LoadSoundAlias(source) BIND(c, name='LoadSoundAlias')
    IMPORT :: sound_type
    IMPLICIT NONE
    TYPE(sound_type), INTENT(in), VALUE :: source
    TYPE(sound_type) :: LoadSoundAlias
  END FUNCTION LoadSoundAlias

  ! Sound LoadSoundFromWave(Wave wave)
  FUNCTION LoadSoundFromWave(wave) BIND(c, name='LoadSoundFromWave')
    IMPORT :: sound_type, wave_type
    IMPLICIT NONE
    TYPE(wave_type), INTENT(in), VALUE :: wave
    TYPE(sound_type) :: LoadSoundFromWave
  END FUNCTION LoadSoundFromWave

  ! Texture2D LoadTexture(const char *fileName)
  FUNCTION LoadTexture(file_name) BIND(c, name='LoadTexture')
    IMPORT :: C_CHAR, texture2d_type
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: file_name
    TYPE(texture2d_type) :: LoadTexture
  END FUNCTION LoadTexture

  ! TextureCubemap LoadTextureCubemap(Image image, int layout)
 FUNCTION LoadTextureCubemap(image, layout) BIND(c, name='LoadTextureCubemap')
    IMPORT :: C_INT, image_type, texture_cubemap_type
    IMPLICIT NONE
    TYPE(image_type), INTENT(in), VALUE :: image
    INTEGER(kind=C_INT), INTENT(in), VALUE :: layout
    TYPE(texture_cubemap_type) :: LoadTextureCubemap
  END FUNCTION LoadTextureCubemap

  ! Texture2D LoadTextureFromImage(Image image)
  FUNCTION LoadTextureFromImage(image) BIND(c, name='LoadTextureFromImage')
    IMPORT :: image_type, texture2d_type
    IMPLICIT NONE
    TYPE(image_type), INTENT(in), VALUE :: image
    TYPE(texture2d_type) :: LoadTextureFromImage
  END FUNCTION LoadTextureFromImage

  ! char *LoadUTF8(const int *codepoints, int length)
  FUNCTION LoadUTF8(codepoints, length) BIND(c, name='LoadUTF8')
    IMPORT :: C_INT, C_PTR
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(out) :: codepoints(*)
    INTEGER(kind=C_INT), INTENT(in), VALUE :: length
    TYPE(C_PTR) :: LoadUTF8
  END FUNCTION LoadUTF8

  ! VrStereoConfig LoadVrStereoConfig(VrDeviceInfo device)
  FUNCTION LoadVrStereoConfig(device) BIND(c, name='LoadVrStereoConfig')
    IMPORT :: vr_device_info_type, vr_stereo_config_type
    IMPLICIT NONE
    TYPE(vr_device_info_type), INTENT(in), VALUE :: device
    TYPE(vr_stereo_config_type) :: LoadVrStereoConfig
  END FUNCTION LoadVrStereoConfig

  ! Wave LoadWave(const char *fileName)
  FUNCTION LoadWave(file_name) BIND(c, name='LoadWave')
    IMPORT :: C_CHAR, wave_type
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: file_name
    TYPE(wave_type) :: LoadWave
  END FUNCTION LoadWave

  ! Wave LoadWaveFromMemory(const char *fileType, const unsigned char *fileData, int dataSize)
        function LoadWaveFromMemory(file_type, file_data, data_size) bind(c, name='LoadWaveFromMemory')
    IMPORT :: C_CHAR, C_INT, c_unsigned_char, wave_type
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: file_type
    INTEGER(kind=c_unsigned_char), INTENT(in) :: file_data
    INTEGER(kind=C_INT), INTENT(in), VALUE :: data_size
    TYPE(wave_type) :: LoadWaveFromMemory
  END FUNCTION LoadWaveFromMemory

  ! float *LoadWaveSamples(Wave wave)
  FUNCTION LoadWaveSamples(wave) BIND(c, name='LoadWaveSamples')
    IMPORT :: C_PTR, wave_type
    IMPLICIT NONE
    TYPE(wave_type), INTENT(in), VALUE :: wave
    TYPE(C_PTR) :: LoadWaveSamples
  END FUNCTION LoadWaveSamples
END INTERFACE

END MODULE RaylibLoadMethods
