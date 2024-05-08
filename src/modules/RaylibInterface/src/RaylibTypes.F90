! raylib.f90
!
! A collection of auto-generated Fortran 2018 interface bindings to
! raylib 5.1.
!
! Author:  Philipp Engel
! Licence: ISC

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-02
! summary: I have subdivided the big raylib module into smaller modules

MODULE RaylibTypes
USE, INTRINSIC :: ISO_C_BINDING
IMPLICIT NONE
PRIVATE

INTEGER, PARAMETER, PUBLIC :: c_unsigned_int = C_INT
INTEGER, PARAMETER, PUBLIC :: c_unsigned_char = C_SIGNED_CHAR

REAL, PARAMETER, PUBLIC :: PI = ACOS(-1.0)

! Vector2
TYPE, BIND(c), PUBLIC :: vector2_
  REAL(kind=C_FLOAT) :: x = 0.0
  REAL(kind=C_FLOAT) :: y = 0.0
END TYPE vector2_

! Vector3
TYPE, BIND(c), PUBLIC :: vector3_
  REAL(kind=C_FLOAT) :: x = 0.0
  REAL(kind=C_FLOAT) :: y = 0.0
  REAL(kind=C_FLOAT) :: z = 0.0
END TYPE vector3_

! Vector4
TYPE, BIND(c), PUBLIC :: vector4_
  REAL(kind=C_FLOAT) :: x = 0.0
  REAL(kind=C_FLOAT) :: y = 0.0
  REAL(kind=C_FLOAT) :: z = 0.0
  REAL(kind=C_FLOAT) :: w = 0.0
END TYPE vector4_

! Quaternion
TYPE, BIND(c), PUBLIC :: quaternion_
  REAL(kind=C_FLOAT) :: x = 0.0
  REAL(kind=C_FLOAT) :: y = 0.0
  REAL(kind=C_FLOAT) :: z = 0.0
  REAL(kind=C_FLOAT) :: w = 0.0
END TYPE quaternion_

! Matrix
TYPE, BIND(c), PUBLIC :: matrix_
  REAL(kind=C_FLOAT) :: m0 = 0.0, m4 = 0.0, m8 = 0.0, m12 = 0.0
  REAL(kind=C_FLOAT) :: m1 = 0.0, m5 = 0.0, m9 = 0.0, m13 = 0.0
  REAL(kind=C_FLOAT) :: m2 = 0.0, m6 = 0.0, m10 = 0.0, m14 = 0.0
  REAL(kind=C_FLOAT) :: m3 = 0.0, m7 = 0.0, m11 = 0.0, m15 = 0.0
END TYPE matrix_

! Color
TYPE, BIND(c), PUBLIC :: color_
  INTEGER(kind=c_unsigned_char) :: r = 0_C_UNSIGNED_CHAR
  INTEGER(kind=c_unsigned_char) :: g = 0_C_UNSIGNED_CHAR
  INTEGER(kind=c_unsigned_char) :: b = 0_C_UNSIGNED_CHAR
  INTEGER(kind=c_unsigned_char) :: a = 255_C_UNSIGNED_CHAR
END TYPE color_

! Rectangle
TYPE, BIND(c), PUBLIC :: rectangle_
  REAL(kind=C_FLOAT) :: x = 0.0
  REAL(kind=C_FLOAT) :: y = 0.0
  REAL(kind=C_FLOAT) :: width = 0.0
  REAL(kind=C_FLOAT) :: height = 0.0
END TYPE rectangle_

! Image
TYPE, BIND(c), PUBLIC :: image_
  TYPE(C_PTR) :: DATA = C_NULL_PTR !! void *
  INTEGER(kind=C_INT) :: width = 0
  INTEGER(kind=C_INT) :: height = 0
  INTEGER(kind=C_INT) :: mipmaps = 0
  INTEGER(kind=C_INT) :: FORMAT = 0
END TYPE image_

! Texture2D
TYPE, BIND(c), PUBLIC :: texture2d_
  INTEGER(kind=c_unsigned_int) :: id = 0
  INTEGER(kind=C_INT) :: width = 0
  INTEGER(kind=C_INT) :: height = 0
  INTEGER(kind=C_INT) :: mipmaps = 0
  INTEGER(kind=C_INT) :: FORMAT = 0
END TYPE texture2d_

! TextureCubemap
TYPE, BIND(c), PUBLIC :: texture_cubemap_
  INTEGER(kind=c_unsigned_int) :: id = 0_C_UNSIGNED_INT
  INTEGER(kind=C_INT) :: width = 0
  INTEGER(kind=C_INT) :: height = 0
  INTEGER(kind=C_INT) :: mipmaps = 0
  INTEGER(kind=C_INT) :: FORMAT = 0
END TYPE texture_cubemap_

! RenderTexture
TYPE, BIND(c), PUBLIC :: render_texture_
  INTEGER(kind=c_unsigned_int) :: id = 0_C_UNSIGNED_INT
  TYPE(texture2d_) :: texture
  TYPE(texture2d_) :: depth
END TYPE render_texture_

! RenderTexture2D
TYPE, BIND(c), PUBLIC :: render_texture2d_
  INTEGER(kind=c_unsigned_int) :: id = 0_C_UNSIGNED_INT
  TYPE(texture2d_) :: texture
  TYPE(texture2d_) :: depth
END TYPE render_texture2d_

! NPatchInfo
TYPE, BIND(c), PUBLIC :: npatch_info_
  TYPE(rectangle_) :: source
  INTEGER(kind=C_INT) :: left = 0
  INTEGER(kind=C_INT) :: top = 0
  INTEGER(kind=C_INT) :: right = 0
  INTEGER(kind=C_INT) :: bottom = 0
  INTEGER(kind=C_INT) :: layout = 0
END TYPE npatch_info_

! GlyphInfo
TYPE, BIND(c), PUBLIC :: glyph_info_
  INTEGER(kind=C_INT) :: VALUE = 0
  INTEGER(kind=C_INT) :: offset_x = 0
  INTEGER(kind=C_INT) :: offset_y = 0
  INTEGER(kind=C_INT) :: advance_x = 0
  TYPE(image_) :: image
END TYPE glyph_info_

! Font
TYPE, BIND(c), PUBLIC :: font_
  INTEGER(kind=C_INT) :: base_size = 0
  INTEGER(kind=C_INT) :: glyph_count = 0
  INTEGER(kind=C_INT) :: glyph_padding = 0
  TYPE(texture2d_) :: texture
  TYPE(C_PTR) :: recs = C_NULL_PTR !! Rectangle *
  TYPE(C_PTR) :: glyphs = C_NULL_PTR !! GlyphInfo *
END TYPE font_

! Camera, Camera3D
TYPE, BIND(c), PUBLIC :: camera3d_
  TYPE(vector3_) :: position
  TYPE(vector3_) :: TARGET
  TYPE(vector3_) :: up
  REAL(kind=C_FLOAT) :: fovy = 0.0
  INTEGER(kind=C_INT) :: projection = 0
END TYPE camera3d_

! Camera2D
TYPE, BIND(c), PUBLIC :: camera2d_
  TYPE(vector2_) :: offset
  TYPE(vector2_) :: TARGET
  REAL(kind=C_FLOAT) :: rotation = 0.0
  REAL(kind=C_FLOAT) :: zoom = 0.0
END TYPE camera2d_

! Mesh
TYPE, BIND(c), PUBLIC :: mesh_
  INTEGER(kind=C_INT) :: vertex_count = 0
  INTEGER(kind=C_INT) :: triangle_count = 0
  TYPE(C_PTR) :: vertices = C_NULL_PTR !! float *
  TYPE(C_PTR) :: texcoords = C_NULL_PTR !! float *
  TYPE(C_PTR) :: texcoords2 = C_NULL_PTR !! float *
  TYPE(C_PTR) :: normals = C_NULL_PTR !! float *
  TYPE(C_PTR) :: tangents = C_NULL_PTR !! float *
  TYPE(C_PTR) :: colors = C_NULL_PTR !! unsigned char *
  TYPE(C_PTR) :: indices = C_NULL_PTR !! unsigned short *
  TYPE(C_PTR) :: anim_vertices = C_NULL_PTR !! float *
  TYPE(C_PTR) :: anim_normals = C_NULL_PTR !! float *
  TYPE(C_PTR) :: bone_ids = C_NULL_PTR !! unsigned char *
  TYPE(C_PTR) :: bone_weights = C_NULL_PTR !! float *
  INTEGER(kind=c_unsigned_int) :: vao_id = 0_C_UNSIGNED_INT
  TYPE(C_PTR) :: vbo_id = C_NULL_PTR !! unsigned int *
END TYPE mesh_

! Shader
TYPE, BIND(c), PUBLIC :: shader_
  INTEGER(kind=c_unsigned_int) :: id = 0
  TYPE(C_PTR) :: locs = C_NULL_PTR !! int *
END TYPE shader_

! MaterialMap
TYPE, BIND(c), PUBLIC :: material_map_
  TYPE(texture2d_) :: texture
  TYPE(color_) :: color
  REAL(kind=C_FLOAT) :: VALUE = 0
END TYPE material_map_

! Material
TYPE, BIND(c), PUBLIC :: material_
  TYPE(shader_) :: shader
  TYPE(C_PTR) :: maps = C_NULL_PTR !! MaterialMap *
  REAL(kind=C_FLOAT) :: params(0:3) = 0.0
END TYPE material_

! Transform
TYPE, BIND(c), PUBLIC :: transform_
  TYPE(vector3_) :: translation
  TYPE(quaternion_) :: rotation
  TYPE(vector3_) :: scale
END TYPE transform_

! BoneInfo
TYPE, BIND(c), PUBLIC :: bone_info_
  CHARACTER(kind=C_CHAR) :: name(0:31) = C_NULL_CHAR
  INTEGER(kind=C_INT) :: parent = 0
END TYPE bone_info_

! Model
TYPE, BIND(c), PUBLIC :: model_
  TYPE(matrix_) :: transform
  INTEGER(kind=C_INT) :: mesh_count = 0
  INTEGER(kind=C_INT) :: material_count = 0
  TYPE(C_PTR) :: meshes = C_NULL_PTR !! Mesh *
  TYPE(C_PTR) :: materials = C_NULL_PTR !! Material *
  TYPE(C_PTR) :: mesh_material = C_NULL_PTR !! int *
  INTEGER(kind=C_INT) :: bone_count = 0
  TYPE(C_PTR) :: bones = C_NULL_PTR !! BoneInfo *
  TYPE(C_PTR) :: bind_pose = C_NULL_PTR !! Transform *
END TYPE model_

! ModelAnimation
TYPE, BIND(c), PUBLIC :: model_animation_
  INTEGER(kind=C_INT) :: bone_count = 0
  INTEGER(kind=C_INT) :: frame_count = 0
  TYPE(C_PTR) :: bones = C_NULL_PTR !! BoneInfo *
  TYPE(C_PTR) :: frame_poses = C_NULL_PTR !! Transform **
  CHARACTER(kind=C_CHAR) :: name(0:31) = C_NULL_CHAR
END TYPE model_animation_

! Ray
TYPE, BIND(c), PUBLIC :: ray_
  TYPE(vector3_) :: position
  TYPE(vector3_) :: direction
END TYPE ray_

! RayCollision
TYPE, BIND(c), PUBLIC :: ray_collision_
  LOGICAL(kind=C_BOOL) :: hit = .FALSE._C_BOOL
  REAL(kind=C_FLOAT) :: distance = 0.0
  TYPE(vector3_) :: point
  TYPE(vector3_) :: normal
END TYPE ray_collision_

! BoundingBox
TYPE, BIND(c), PUBLIC :: bounding_box_
  TYPE(vector3_) :: min
  TYPE(vector3_) :: max
END TYPE bounding_box_

! Wave
TYPE, BIND(c), PUBLIC :: wave_
  INTEGER(kind=c_unsigned_int) :: frame_count = 0_C_UNSIGNED_INT
  INTEGER(kind=c_unsigned_int) :: sample_rate = 0_C_UNSIGNED_INT
  INTEGER(kind=c_unsigned_int) :: sample_size = 0_C_UNSIGNED_INT
  INTEGER(kind=c_unsigned_int) :: channels = 0_C_UNSIGNED_INT
  TYPE(C_PTR) :: DATA = C_NULL_PTR !! void *
END TYPE wave_

! AudioStream
TYPE, BIND(c), PUBLIC :: audio_stream_
  TYPE(C_PTR) :: buffer = C_NULL_PTR !! rAudioBuffer *
  TYPE(C_PTR) :: processor = C_NULL_PTR !! rAudioProcessor *
  INTEGER(kind=c_unsigned_int) :: sample_rate = 0_C_UNSIGNED_INT
  INTEGER(kind=c_unsigned_int) :: sample_size = 0_C_UNSIGNED_INT
  INTEGER(kind=c_unsigned_int) :: channels = 0_C_UNSIGNED_INT
END TYPE audio_stream_

! Sound
TYPE, BIND(c), PUBLIC :: sound_
  TYPE(audio_stream_) :: stream
  INTEGER(kind=c_unsigned_int) :: frame_count = 0
END TYPE sound_

! Music
TYPE, BIND(c), PUBLIC :: music_
  TYPE(audio_stream_) :: stream
  INTEGER(kind=c_unsigned_int) :: frame_count = 0_C_UNSIGNED_INT
  LOGICAL(kind=C_BOOL) :: looping = .FALSE._C_BOOL
  INTEGER(kind=C_INT) :: ctx_ = 0
  TYPE(C_PTR) :: ctx_data = C_NULL_PTR !! void *
END TYPE music_

! VrDeviceInfo
TYPE, BIND(c), PUBLIC :: vr_device_info_
  INTEGER(kind=C_INT) :: h_resolution = 0
  INTEGER(kind=C_INT) :: v_resolution = 0
  REAL(kind=C_FLOAT) :: h_screen_size = 0.0
  REAL(kind=C_FLOAT) :: v_screen_size = 0.0
  REAL(kind=C_FLOAT) :: v_screen_center = 0.0
  REAL(kind=C_FLOAT) :: eye_to_screen_distance = 0.0
  REAL(kind=C_FLOAT) :: lens_separation_distance = 0.0
  REAL(kind=C_FLOAT) :: interpupillary_distance = 0.0
  REAL(kind=C_FLOAT) :: lens_distortion_values(0:3) = 0.0
  REAL(kind=C_FLOAT) :: chroma_ab_correction(0:3) = 0.0
END TYPE vr_device_info_

! VrStereoConfig
TYPE, BIND(c), PUBLIC :: vr_stereo_config_
  TYPE(matrix_) :: projection(0:1)
  TYPE(matrix_) :: view_offset(0:1)
  REAL(kind=C_FLOAT) :: left_lens_center(0:1) = 0.0
  REAL(kind=C_FLOAT) :: right_lens_center(0:1) = 0.0
  REAL(kind=C_FLOAT) :: left_screen_center(0:1) = 0.0
  REAL(kind=C_FLOAT) :: right_screen_center(0:1) = 0.0
  REAL(kind=C_FLOAT) :: SCALE(0:1) = 0.0
  REAL(kind=C_FLOAT) :: scale_in(0:1) = 0.0
END TYPE vr_stereo_config_

! FilePathList
TYPE, BIND(c), PUBLIC :: file_path_list_
  INTEGER(kind=c_unsigned_int) :: capacity = 0_C_UNSIGNED_INT
  INTEGER(kind=c_unsigned_int) :: count = 0_C_UNSIGNED_INT
  TYPE(C_PTR) :: paths = C_NULL_PTR !! char **
END TYPE file_path_list_

TYPE(color_), PARAMETER, PUBLIC :: LIGHTGRAY = &
                                   color_(200_C_UNSIGNED_CHAR, &
                200_C_UNSIGNED_CHAR, 200_C_UNSIGNED_CHAR, 255_C_UNSIGNED_CHAR)
TYPE(color_), PARAMETER, PUBLIC :: GRAY = color_(130_C_UNSIGNED_CHAR, &
                130_C_UNSIGNED_CHAR, 130_C_UNSIGNED_CHAR, 255_C_UNSIGNED_CHAR)
TYPE(color_), PARAMETER, PUBLIC :: DARKGRAY = color_(80_C_UNSIGNED_CHAR, &
                  80_C_UNSIGNED_CHAR, 80_C_UNSIGNED_CHAR, 255_C_UNSIGNED_CHAR)
TYPE(color_), PARAMETER, PUBLIC :: YELLOW = color_(253_C_UNSIGNED_CHAR, &
                  249_C_UNSIGNED_CHAR, 0_C_UNSIGNED_CHAR, 255_C_UNSIGNED_CHAR)
TYPE(color_), PARAMETER, PUBLIC :: GOLD = color_(255_C_UNSIGNED_CHAR, &
                  203_C_UNSIGNED_CHAR, 0_C_UNSIGNED_CHAR, 255_C_UNSIGNED_CHAR)
TYPE(color_), PARAMETER, PUBLIC :: ORANGE = color_(255_C_UNSIGNED_CHAR, &
                  161_C_UNSIGNED_CHAR, 0_C_UNSIGNED_CHAR, 255_C_UNSIGNED_CHAR)
TYPE(color_), PARAMETER, PUBLIC :: PINK = color_(255_C_UNSIGNED_CHAR, &
                109_C_UNSIGNED_CHAR, 194_C_UNSIGNED_CHAR, 255_C_UNSIGNED_CHAR)
TYPE(color_), PARAMETER, PUBLIC :: RED = color_(230_C_UNSIGNED_CHAR, &
                  41_C_UNSIGNED_CHAR, 55_C_UNSIGNED_CHAR, 255_C_UNSIGNED_CHAR)
TYPE(color_), PARAMETER, PUBLIC :: MAROON = color_(190_C_UNSIGNED_CHAR, &
                  33_C_UNSIGNED_CHAR, 55_C_UNSIGNED_CHAR, 255_C_UNSIGNED_CHAR)
TYPE(color_), PARAMETER, PUBLIC :: GREEN = color_(0_C_UNSIGNED_CHAR, &
                 228_C_UNSIGNED_CHAR, 48_C_UNSIGNED_CHAR, 255_C_UNSIGNED_CHAR)
TYPE(color_), PARAMETER, PUBLIC :: LIME = color_(0_C_UNSIGNED_CHAR, &
                 158_C_UNSIGNED_CHAR, 47_C_UNSIGNED_CHAR, 255_C_UNSIGNED_CHAR)
TYPE(color_), PARAMETER, PUBLIC :: DARKGREEN = color_(0_C_UNSIGNED_CHAR, &
                 117_C_UNSIGNED_CHAR, 44_C_UNSIGNED_CHAR, 255_C_UNSIGNED_CHAR)
TYPE(color_), PARAMETER, PUBLIC :: SKYBLUE = &
                                   color_(102_C_UNSIGNED_CHAR, &
                191_C_UNSIGNED_CHAR, 255_C_UNSIGNED_CHAR, 255_C_UNSIGNED_CHAR)
TYPE(color_), PARAMETER, PUBLIC :: BLUE = color_(0_C_UNSIGNED_CHAR, &
                121_C_UNSIGNED_CHAR, 241_C_UNSIGNED_CHAR, 255_C_UNSIGNED_CHAR)
TYPE(color_), PARAMETER, PUBLIC :: DARKBLUE = color_(0_C_UNSIGNED_CHAR, &
                 82_C_UNSIGNED_CHAR, 172_C_UNSIGNED_CHAR, 255_C_UNSIGNED_CHAR)
TYPE(color_), PARAMETER, PUBLIC :: PURPLE = color_(200_C_UNSIGNED_CHAR, &
                122_C_UNSIGNED_CHAR, 255_C_UNSIGNED_CHAR, 255_C_UNSIGNED_CHAR)
TYPE(color_), PARAMETER, PUBLIC :: VIOLET = color_(135_C_UNSIGNED_CHAR, &
                 60_C_UNSIGNED_CHAR, 190_C_UNSIGNED_CHAR, 255_C_UNSIGNED_CHAR)
TYPE(color_), PARAMETER, PUBLIC :: DARKPURPLE = &
                                   color_(112_C_UNSIGNED_CHAR, &
                 31_C_UNSIGNED_CHAR, 126_C_UNSIGNED_CHAR, 255_C_UNSIGNED_CHAR)
TYPE(color_), PARAMETER, PUBLIC :: BEIGE = color_(211_C_UNSIGNED_CHAR, &
                176_C_UNSIGNED_CHAR, 131_C_UNSIGNED_CHAR, 255_C_UNSIGNED_CHAR)
TYPE(color_), PARAMETER, PUBLIC :: BROWN = color_(127_C_UNSIGNED_CHAR, &
                 106_C_UNSIGNED_CHAR, 79_C_UNSIGNED_CHAR, 255_C_UNSIGNED_CHAR)
TYPE(color_), PARAMETER, PUBLIC :: DARKBROWN = color_(76_C_UNSIGNED_CHAR, &
                  63_C_UNSIGNED_CHAR, 47_C_UNSIGNED_CHAR, 255_C_UNSIGNED_CHAR)
TYPE(color_), PARAMETER, PUBLIC :: WHITE = color_(255_C_UNSIGNED_CHAR, &
                255_C_UNSIGNED_CHAR, 255_C_UNSIGNED_CHAR, 255_C_UNSIGNED_CHAR)
TYPE(color_), PARAMETER, PUBLIC :: BLACK = color_(0_C_UNSIGNED_CHAR, &
                    0_C_UNSIGNED_CHAR, 0_C_UNSIGNED_CHAR, 255_C_UNSIGNED_CHAR)
TYPE(color_), PARAMETER, PUBLIC :: BLANK = color_(0_C_UNSIGNED_CHAR, &
                      0_C_UNSIGNED_CHAR, 0_C_UNSIGNED_CHAR, 0_C_UNSIGNED_CHAR)
TYPE(color_), PARAMETER, PUBLIC :: MAGENTA = color_(255_C_UNSIGNED_CHAR, &
                  0_C_UNSIGNED_CHAR, 255_C_UNSIGNED_CHAR, 255_C_UNSIGNED_CHAR)
TYPE(color_), PARAMETER, PUBLIC :: RAYWHITE = &
                                   color_(245_C_UNSIGNED_CHAR, &
                245_C_UNSIGNED_CHAR, 245_C_UNSIGNED_CHAR, 255_C_UNSIGNED_CHAR)

END MODULE RaylibTypes
