! raylib.f90
!
! A collection of auto-generated Fortran 2018 interface bindings to
! raylib 5.1.
!
! Author:  Philipp Engel
! Licence: ISC

MODULE RaylibUnloadMethods
USE, INTRINSIC :: ISO_C_BINDING
USE RaylibTypes
USE RaylibEnums
IMPLICIT NONE
PRIVATE

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

INTERFACE
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
END INTERFACE

END MODULE RaylibUnloadMethods
