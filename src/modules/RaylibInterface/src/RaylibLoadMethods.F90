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

INTERFACE
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
END INTERFACE

END MODULE RaylibLoadMethods
