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

PUBLIC :: UnloadWaveSamples
PUBLIC :: UnloadWave
PUBLIC :: UnloadVrStereoConfig
PUBLIC :: UnloadUTF8
PUBLIC :: UnloadTexture
PUBLIC :: UnloadSoundAlias
PUBLIC :: UnloadSound
PUBLIC :: UnloadShader
PUBLIC :: UnloadRenderTexture
PUBLIC :: UnloadRandomSequence
PUBLIC :: UnloadMusicStream
PUBLIC :: UnloadModelAnimations
PUBLIC :: UnloadModelAnimation
PUBLIC :: UnloadModel
PUBLIC :: UnloadMesh
PUBLIC :: UnloadMaterial
PUBLIC :: UnloadImagePalette
PUBLIC :: UnloadImageColors
PUBLIC :: UnloadImage
PUBLIC :: UnloadFontData
PUBLIC :: UnloadFont
PUBLIC :: UnloadFileText
PUBLIC :: UnloadFileData
PUBLIC :: UnloadDroppedFiles
PUBLIC :: UnloadDirectoryFiles
PUBLIC :: UnloadCodepoints
PUBLIC :: UnloadAudioStream

INTERFACE
  ! void UnloadAudioStream(AudioStream stream)
  SUBROUTINE UnloadAudioStream(stream) BIND(c, name='UnloadAudioStream')
    IMPORT :: audio_stream_
    IMPLICIT NONE
    TYPE(audio_stream_), INTENT(in), VALUE :: stream
  END SUBROUTINE UnloadAudioStream

  ! void UnloadCodepoints(int *codepoints)
  SUBROUTINE UnloadCodepoints(codepoints) BIND(c, name='UnloadCodepoints')
    IMPORT :: C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(inout) :: codepoints(*)
  END SUBROUTINE UnloadCodepoints

  ! void UnloadDirectoryFiles(FilePathList files)
  SUBROUTINE UnloadDirectoryFiles(files) BIND(c, name='UnloadDirectoryFiles')
    IMPORT :: file_path_list_
    IMPLICIT NONE
    TYPE(file_path_list_), INTENT(in), VALUE :: files
  END SUBROUTINE UnloadDirectoryFiles

  ! void UnloadDroppedFiles(FilePathList files)
  SUBROUTINE UnloadDroppedFiles(files) BIND(c, name='UnloadDroppedFiles')
    IMPORT :: file_path_list_
    IMPLICIT NONE
    TYPE(file_path_list_), INTENT(in), VALUE :: files
  END SUBROUTINE UnloadDroppedFiles

  ! void UnloadFileData(unsigned char *data)
  SUBROUTINE UnloadFileData(DATA) BIND(c, name='UnloadFileData')
    IMPORT :: c_unsigned_char
    IMPLICIT NONE
    INTEGER(kind=c_unsigned_char), INTENT(in) :: DATA
  END SUBROUTINE UnloadFileData

  ! void UnloadFileText(char *text)
  SUBROUTINE UnloadFileText(text) BIND(c, name='UnloadFileText')
    IMPORT :: C_CHAR
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: text
  END SUBROUTINE UnloadFileText

  ! void UnloadFont(Font font)
  SUBROUTINE UnloadFont(font) BIND(c, name='UnloadFont')
    IMPORT :: font_
    IMPLICIT NONE
    TYPE(font_), INTENT(in), VALUE :: font
  END SUBROUTINE UnloadFont

  ! void UnloadFontData(GlyphInfo *glyphs, int glyphCount)
 SUBROUTINE UnloadFontData(glyphs, glyph_count) BIND(c, name='UnloadFontData')
    IMPORT :: C_INT, glyph_info_
    IMPLICIT NONE
    TYPE(glyph_info_), INTENT(inout) :: glyphs
    INTEGER(kind=C_INT), INTENT(in), VALUE :: glyph_count
  END SUBROUTINE UnloadFontData

  ! void UnloadImage(Image image)
  SUBROUTINE UnloadImage(image) BIND(c, name='UnloadImage')
    IMPORT :: image_
    IMPLICIT NONE
    TYPE(image_), INTENT(in), VALUE :: image
  END SUBROUTINE UnloadImage

  ! void UnloadImageColors(Color *colors)
  SUBROUTINE UnloadImageColors(colors) BIND(c, name='UnloadImageColors')
    IMPORT :: color_
    IMPLICIT NONE
    TYPE(color_), INTENT(inout) :: colors(*)
  END SUBROUTINE UnloadImageColors

  ! void UnloadImagePalette(Color *colors)
  SUBROUTINE UnloadImagePalette(colors) BIND(c, name='UnloadImagePalette')
    IMPORT :: color_
    IMPLICIT NONE
    TYPE(color_), INTENT(inout) :: colors(*)
  END SUBROUTINE UnloadImagePalette

  ! void UnloadMaterial(Material material)
  SUBROUTINE UnloadMaterial(material) BIND(c, name='UnloadMaterial')
    IMPORT :: material_
    IMPLICIT NONE
    TYPE(material_), INTENT(in), VALUE :: material
  END SUBROUTINE UnloadMaterial

  ! void UnloadMesh(Mesh mesh)
  SUBROUTINE UnloadMesh(mesh) BIND(c, name='UnloadMesh')
    IMPORT :: mesh_
    IMPLICIT NONE
    TYPE(mesh_), INTENT(in), VALUE :: mesh
  END SUBROUTINE UnloadMesh

  ! void UnloadModel(Model model)
  SUBROUTINE UnloadModel(model) BIND(c, name='UnloadModel')
    IMPORT :: model_
    IMPLICIT NONE
    TYPE(model_), INTENT(in), VALUE :: model
  END SUBROUTINE UnloadModel

  ! void UnloadModelAnimation(ModelAnimation anim)
  SUBROUTINE UnloadModelAnimation(anim) BIND(c, name='UnloadModelAnimation')
    IMPORT :: model_animation_
    IMPLICIT NONE
    TYPE(model_animation_), INTENT(in), VALUE :: anim
  END SUBROUTINE UnloadModelAnimation

  ! void UnloadModelAnimations(ModelAnimation *animations, int count)
        subroutine UnloadModelAnimations(animations, count) bind(c, name='UnloadModelAnimations')
    IMPORT :: C_INT, model_animation_
    IMPLICIT NONE
    TYPE(model_animation_), INTENT(inout) :: animations(*)
    INTEGER(kind=C_INT), INTENT(in), VALUE :: count
  END SUBROUTINE UnloadModelAnimations

  ! void UnloadMusicStream(Music music)
  SUBROUTINE UnloadMusicStream(music) BIND(c, name='UnloadMusicStream')
    IMPORT :: music_
    IMPLICIT NONE
    TYPE(music_), INTENT(in), VALUE :: music
  END SUBROUTINE UnloadMusicStream

  ! void UnloadRandomSequence(int *sequence)
SUBROUTINE UnloadRandomSequence(SEQUENCE) BIND(c, name='UnloadRandomSequence')
    IMPORT :: C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(inout) :: SEQUENCE(*)
  END SUBROUTINE UnloadRandomSequence

  ! void UnloadRenderTexture(RenderTexture2D target)
  SUBROUTINE UnloadRenderTexture(TARGET) BIND(c, name='UnloadRenderTexture')
    IMPORT :: render_texture2d_
    IMPLICIT NONE
    TYPE(render_texture2d_), INTENT(in), VALUE :: TARGET
  END SUBROUTINE UnloadRenderTexture

  ! void UnloadShader(Shader shader)
  SUBROUTINE UnloadShader(shader) BIND(c, name='UnloadShader')
    IMPORT :: shader_
    IMPLICIT NONE
    TYPE(shader_), INTENT(in), VALUE :: shader
  END SUBROUTINE UnloadShader

  ! void UnloadSound(Sound sound)
  SUBROUTINE UnloadSound(sound) BIND(c, name='UnloadSound')
    IMPORT :: sound_
    IMPLICIT NONE
    TYPE(sound_), INTENT(in), VALUE :: sound
  END SUBROUTINE UnloadSound

  ! void UnloadSoundAlias(Sound alias)
  SUBROUTINE UnloadSoundAlias(alias) BIND(c, name='UnloadSoundAlias')
    IMPORT :: sound_
    IMPLICIT NONE
    TYPE(sound_), INTENT(in), VALUE :: alias
  END SUBROUTINE UnloadSoundAlias

  ! void UnloadTexture(Texture2D texture)
  SUBROUTINE UnloadTexture(texture) BIND(c, name='UnloadTexture')
    IMPORT :: texture2d_
    IMPLICIT NONE
    TYPE(texture2d_), INTENT(in), VALUE :: texture
  END SUBROUTINE UnloadTexture

  ! void UnloadUTF8(char *text)
  SUBROUTINE UnloadUTF8(text) BIND(c, name='UnloadUTF8')
    IMPORT :: C_CHAR
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: text
  END SUBROUTINE UnloadUTF8

  ! void UnloadVrStereoConfig(VrStereoConfig config)
  SUBROUTINE UnloadVrStereoConfig(config) BIND(c, name='UnloadVrStereoConfig')
    IMPORT :: vr_stereo_config_
    IMPLICIT NONE
    TYPE(vr_stereo_config_), INTENT(in), VALUE :: config
  END SUBROUTINE UnloadVrStereoConfig

  ! void UnloadWave(Wave wave)
  SUBROUTINE UnloadWave(wave) BIND(c, name='UnloadWave')
    IMPORT :: wave_
    IMPLICIT NONE
    TYPE(wave_), INTENT(in), VALUE :: wave
  END SUBROUTINE UnloadWave

  ! void UnloadWaveSamples(float *samples)
  SUBROUTINE UnloadWaveSamples(samples) BIND(c, name='UnloadWaveSamples')
    IMPORT :: C_FLOAT
    IMPLICIT NONE
    REAL(kind=C_FLOAT), INTENT(inout) :: samples(*)
  END SUBROUTINE UnloadWaveSamples
END INTERFACE

END MODULE RaylibUnloadMethods
