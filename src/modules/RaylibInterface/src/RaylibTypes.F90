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
IMPLICIT NONE(TYPE, EXTERNAL)
PRIVATE

INTEGER, PARAMETER, PUBLIC :: c_unsigned_int = C_INT
INTEGER, PARAMETER, PUBLIC :: c_unsigned_char = C_SIGNED_CHAR

REAL, PARAMETER, PUBLIC :: PI = ACOS(-1.0)

! Vector2
TYPE, BIND(c), PUBLIC :: vector2_type
  REAL(kind=C_FLOAT) :: x = 0.0
  REAL(kind=C_FLOAT) :: y = 0.0
END TYPE vector2_type

! Vector3
TYPE, BIND(c), PUBLIC :: vector3_type
  REAL(kind=C_FLOAT) :: x = 0.0
  REAL(kind=C_FLOAT) :: y = 0.0
  REAL(kind=C_FLOAT) :: z = 0.0
END TYPE vector3_type

! Vector4
TYPE, BIND(c), PUBLIC :: vector4_type
  REAL(kind=C_FLOAT) :: x = 0.0
  REAL(kind=C_FLOAT) :: y = 0.0
  REAL(kind=C_FLOAT) :: z = 0.0
  REAL(kind=C_FLOAT) :: w = 0.0
END TYPE vector4_type

! Quaternion
TYPE, BIND(c), PUBLIC :: quaternion_type
  REAL(kind=C_FLOAT) :: x = 0.0
  REAL(kind=C_FLOAT) :: y = 0.0
  REAL(kind=C_FLOAT) :: z = 0.0
  REAL(kind=C_FLOAT) :: w = 0.0
END TYPE quaternion_type

! Matrix
TYPE, BIND(c), PUBLIC :: matrix_type
  REAL(kind=C_FLOAT) :: m0 = 0.0, m4 = 0.0, m8 = 0.0, m12 = 0.0
  REAL(kind=C_FLOAT) :: m1 = 0.0, m5 = 0.0, m9 = 0.0, m13 = 0.0
  REAL(kind=C_FLOAT) :: m2 = 0.0, m6 = 0.0, m10 = 0.0, m14 = 0.0
  REAL(kind=C_FLOAT) :: m3 = 0.0, m7 = 0.0, m11 = 0.0, m15 = 0.0
END TYPE matrix_type

! Color
TYPE, BIND(c), PUBLIC :: color_type
  INTEGER(kind=c_unsigned_char) :: r = 0_C_UNSIGNED_INT
  INTEGER(kind=c_unsigned_char) :: g = 0_C_UNSIGNED_INT
  INTEGER(kind=c_unsigned_char) :: b = 0_C_UNSIGNED_INT
  INTEGER(kind=c_unsigned_char) :: a = 255_C_UNSIGNED_INT
END TYPE color_type

! Rectangle
TYPE, BIND(c), PUBLIC :: rectangle_type
  REAL(kind=C_FLOAT) :: x = 0.0
  REAL(kind=C_FLOAT) :: y = 0.0
  REAL(kind=C_FLOAT) :: width = 0.0
  REAL(kind=C_FLOAT) :: height = 0.0
END TYPE rectangle_type

! Image
TYPE, BIND(c), PUBLIC :: image_type
  TYPE(C_PTR) :: DATA = C_NULL_PTR !! void *
  INTEGER(kind=C_INT) :: width = 0
  INTEGER(kind=C_INT) :: height = 0
  INTEGER(kind=C_INT) :: mipmaps = 0
  INTEGER(kind=C_INT) :: FORMAT = 0
END TYPE image_type

! Texture2D
TYPE, BIND(c), PUBLIC :: texture2d_type
  INTEGER(kind=c_unsigned_int) :: id = 0
  INTEGER(kind=C_INT) :: width = 0
  INTEGER(kind=C_INT) :: height = 0
  INTEGER(kind=C_INT) :: mipmaps = 0
  INTEGER(kind=C_INT) :: FORMAT = 0
END TYPE texture2d_type

! TextureCubemap
TYPE, BIND(c), PUBLIC :: texture_cubemap_type
  INTEGER(kind=c_unsigned_int) :: id = 0_C_UNSIGNED_INT
  INTEGER(kind=C_INT) :: width = 0
  INTEGER(kind=C_INT) :: height = 0
  INTEGER(kind=C_INT) :: mipmaps = 0
  INTEGER(kind=C_INT) :: FORMAT = 0
END TYPE texture_cubemap_type

! RenderTexture
TYPE, BIND(c), PUBLIC :: render_texture_type
  INTEGER(kind=c_unsigned_int) :: id = 0_C_UNSIGNED_INT
  TYPE(texture2d_type) :: texture
  TYPE(texture2d_type) :: depth
END TYPE render_texture_type

! RenderTexture2D
TYPE, BIND(c), PUBLIC :: render_texture2d_type
  INTEGER(kind=c_unsigned_int) :: id = 0_C_UNSIGNED_INT
  TYPE(texture2d_type) :: texture
  TYPE(texture2d_type) :: depth
END TYPE render_texture2d_type

! NPatchInfo
TYPE, BIND(c), PUBLIC :: npatch_info_type
  TYPE(rectangle_type) :: source
  INTEGER(kind=C_INT) :: left = 0
  INTEGER(kind=C_INT) :: top = 0
  INTEGER(kind=C_INT) :: right = 0
  INTEGER(kind=C_INT) :: bottom = 0
  INTEGER(kind=C_INT) :: layout = 0
END TYPE npatch_info_type

! GlyphInfo
TYPE, BIND(c), PUBLIC :: glyph_info_type
  INTEGER(kind=C_INT) :: VALUE = 0
  INTEGER(kind=C_INT) :: offset_x = 0
  INTEGER(kind=C_INT) :: offset_y = 0
  INTEGER(kind=C_INT) :: advance_x = 0
  TYPE(image_type) :: image
END TYPE glyph_info_type

! Font
TYPE, BIND(c), PUBLIC :: font_type
  INTEGER(kind=C_INT) :: base_size = 0
  INTEGER(kind=C_INT) :: glyph_count = 0
  INTEGER(kind=C_INT) :: glyph_padding = 0
  TYPE(texture2d_type) :: texture
  TYPE(C_PTR) :: recs = C_NULL_PTR !! Rectangle *
  TYPE(C_PTR) :: glyphs = C_NULL_PTR !! GlyphInfo *
END TYPE font_type

! Camera, Camera3D
TYPE, BIND(c), PUBLIC :: camera3d_type
  TYPE(vector3_type) :: position
  TYPE(vector3_type) :: TARGET
  TYPE(vector3_type) :: up
  REAL(kind=C_FLOAT) :: fov_y = 0.0
  INTEGER(kind=C_INT) :: projection = 0
END TYPE camera3d_type

! Camera2D
TYPE, BIND(c), PUBLIC :: camera2d_type
  TYPE(vector2_type) :: offset
  TYPE(vector2_type) :: TARGET
  REAL(kind=C_FLOAT) :: rotation = 0.0
  REAL(kind=C_FLOAT) :: zoom = 0.0
END TYPE camera2d_type

! Mesh
TYPE, BIND(c), PUBLIC :: mesh_type
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
END TYPE mesh_type

! Shader
TYPE, BIND(c), PUBLIC :: shader_type
  INTEGER(kind=c_unsigned_int) :: id = 0
  TYPE(C_PTR) :: locs = C_NULL_PTR !! int *
END TYPE shader_type

! MaterialMap
TYPE, BIND(c), PUBLIC :: material_map_type
  TYPE(texture2d_type) :: texture
  TYPE(color_type) :: color
  REAL(kind=C_FLOAT) :: VALUE = 0
END TYPE material_map_type

! Material
TYPE, BIND(c), PUBLIC :: material_type
  TYPE(shader_type) :: shader
  TYPE(C_PTR) :: maps = C_NULL_PTR !! MaterialMap *
  REAL(kind=C_FLOAT) :: params(0:3) = 0.0
END TYPE material_type

! Transform
TYPE, BIND(c), PUBLIC :: transform_type
  TYPE(vector3_type) :: translation
  TYPE(quaternion_type) :: rotation
  TYPE(vector3_type) :: scale
END TYPE transform_type

! BoneInfo
TYPE, BIND(c), PUBLIC :: bone_info_type
  CHARACTER(kind=C_CHAR) :: name(0:31) = C_NULL_CHAR
  INTEGER(kind=C_INT) :: parent = 0
END TYPE bone_info_type

! Model
TYPE, BIND(c), PUBLIC :: model_type
  TYPE(matrix_type) :: transform
  INTEGER(kind=C_INT) :: mesh_count = 0
  INTEGER(kind=C_INT) :: material_count = 0
  TYPE(C_PTR) :: meshes = C_NULL_PTR !! Mesh *
  TYPE(C_PTR) :: materials = C_NULL_PTR !! Material *
  TYPE(C_PTR) :: mesh_material = C_NULL_PTR !! int *
  INTEGER(kind=C_INT) :: bone_count = 0
  TYPE(C_PTR) :: bones = C_NULL_PTR !! BoneInfo *
  TYPE(C_PTR) :: bind_pose = C_NULL_PTR !! Transform *
END TYPE model_type

! ModelAnimation
TYPE, BIND(c), PUBLIC :: model_animation_type
  INTEGER(kind=C_INT) :: bone_count = 0
  INTEGER(kind=C_INT) :: frame_count = 0
  TYPE(C_PTR) :: bones = C_NULL_PTR !! BoneInfo *
  TYPE(C_PTR) :: frame_poses = C_NULL_PTR !! Transform **
  CHARACTER(kind=C_CHAR) :: name(0:31) = C_NULL_CHAR
END TYPE model_animation_type

! Ray
TYPE, BIND(c), PUBLIC :: ray_type
  TYPE(vector3_type) :: position
  TYPE(vector3_type) :: direction
END TYPE ray_type

! RayCollision
TYPE, BIND(c), PUBLIC :: ray_collision_type
  LOGICAL(kind=C_BOOL) :: hit = .FALSE._C_BOOL
  REAL(kind=C_FLOAT) :: distance = 0.0
  TYPE(vector3_type) :: point
  TYPE(vector3_type) :: normal
END TYPE ray_collision_type

! BoundingBox
TYPE, BIND(c), PUBLIC :: bounding_box_type
  TYPE(vector3_type) :: min
  TYPE(vector3_type) :: max
END TYPE bounding_box_type

! Wave
TYPE, BIND(c), PUBLIC :: wave_type
  INTEGER(kind=c_unsigned_int) :: frame_count = 0_C_UNSIGNED_INT
  INTEGER(kind=c_unsigned_int) :: sample_rate = 0_C_UNSIGNED_INT
  INTEGER(kind=c_unsigned_int) :: sample_size = 0_C_UNSIGNED_INT
  INTEGER(kind=c_unsigned_int) :: channels = 0_C_UNSIGNED_INT
  TYPE(C_PTR) :: DATA = C_NULL_PTR !! void *
END TYPE wave_type

! AudioStream
TYPE, BIND(c), PUBLIC :: audio_stream_type
  TYPE(C_PTR) :: buffer = C_NULL_PTR !! rAudioBuffer *
  TYPE(C_PTR) :: processor = C_NULL_PTR !! rAudioProcessor *
  INTEGER(kind=c_unsigned_int) :: sample_rate = 0_C_UNSIGNED_INT
  INTEGER(kind=c_unsigned_int) :: sample_size = 0_C_UNSIGNED_INT
  INTEGER(kind=c_unsigned_int) :: channels = 0_C_UNSIGNED_INT
END TYPE audio_stream_type

! Sound
TYPE, BIND(c), PUBLIC :: sound_type
  TYPE(audio_stream_type) :: stream
  INTEGER(kind=c_unsigned_int) :: frame_count = 0
END TYPE sound_type

! Music
TYPE, BIND(c), PUBLIC :: music_type
  TYPE(audio_stream_type) :: stream
  INTEGER(kind=c_unsigned_int) :: frame_count = 0_C_UNSIGNED_INT
  LOGICAL(kind=C_BOOL) :: looping = .FALSE._C_BOOL
  INTEGER(kind=C_INT) :: ctx_type = 0
  TYPE(C_PTR) :: ctx_data = C_NULL_PTR !! void *
END TYPE music_type

! VrDeviceInfo
TYPE, BIND(c), PUBLIC :: vr_device_info_type
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
END TYPE vr_device_info_type

! VrStereoConfig
TYPE, BIND(c), PUBLIC :: vr_stereo_config_type
  TYPE(matrix_type) :: projection(0:1)
  TYPE(matrix_type) :: view_offset(0:1)
  REAL(kind=C_FLOAT) :: left_lens_center(0:1) = 0.0
  REAL(kind=C_FLOAT) :: right_lens_center(0:1) = 0.0
  REAL(kind=C_FLOAT) :: left_screen_center(0:1) = 0.0
  REAL(kind=C_FLOAT) :: right_screen_center(0:1) = 0.0
  REAL(kind=C_FLOAT) :: SCALE(0:1) = 0.0
  REAL(kind=C_FLOAT) :: scale_in(0:1) = 0.0
END TYPE vr_stereo_config_type

! FilePathList
TYPE, BIND(c), PUBLIC :: file_path_list_type
  INTEGER(kind=c_unsigned_int) :: capacity = 0_C_UNSIGNED_INT
  INTEGER(kind=c_unsigned_int) :: count = 0_C_UNSIGNED_INT
  TYPE(C_PTR) :: paths = C_NULL_PTR !! char **
END TYPE file_path_list_type

TYPE(color_type), PARAMETER, PUBLIC :: LIGHTGRAY = &
                                       color_type(200, 200, 200, 255)
TYPE(color_type), PARAMETER, PUBLIC :: GRAY = color_type(130, 130, 130, 255)
TYPE(color_type), PARAMETER, PUBLIC :: DARKGRAY = color_type(80, 80, 80, 255)
TYPE(color_type), PARAMETER, PUBLIC :: YELLOW = color_type(253, 249, 0, 255)
TYPE(color_type), PARAMETER, PUBLIC :: GOLD = color_type(255, 203, 0, 255)
TYPE(color_type), PARAMETER, PUBLIC :: ORANGE = color_type(255, 161, 0, 255)
TYPE(color_type), PARAMETER, PUBLIC :: PINK = color_type(255, 109, 194, 255)
TYPE(color_type), PARAMETER, PUBLIC :: RED = color_type(230, 41, 55, 255)
TYPE(color_type), PARAMETER, PUBLIC :: MAROON = color_type(190, 33, 55, 255)
TYPE(color_type), PARAMETER, PUBLIC :: GREEN = color_type(0, 228, 48, 255)
TYPE(color_type), PARAMETER, PUBLIC :: LIME = color_type(0, 158, 47, 255)
TYPE(color_type), PARAMETER, PUBLIC :: DARKGREEN = color_type(0, 117, 44, 255)
TYPE(color_type), PARAMETER, PUBLIC :: SKYBLUE = &
                                       color_type(102, 191, 255, 255)
TYPE(color_type), PARAMETER, PUBLIC :: BLUE = color_type(0, 121, 241, 255)
TYPE(color_type), PARAMETER, PUBLIC :: DARKBLUE = color_type(0, 82, 172, 255)
TYPE(color_type), PARAMETER, PUBLIC :: PURPLE = color_type(200, 122, 255, 255)
TYPE(color_type), PARAMETER, PUBLIC :: VIOLET = color_type(135, 60, 190, 255)
TYPE(color_type), PARAMETER, PUBLIC :: DARKPURPLE = &
                                       color_type(112, 31, 126, 255)
TYPE(color_type), PARAMETER, PUBLIC :: BEIGE = color_type(211, 176, 131, 255)
TYPE(color_type), PARAMETER, PUBLIC :: BROWN = color_type(127, 106, 79, 255)
TYPE(color_type), PARAMETER, PUBLIC :: DARKBROWN = color_type(76, 63, 47, 255)
TYPE(color_type), PARAMETER, PUBLIC :: WHITE = color_type(255, 255, 255, 255)
TYPE(color_type), PARAMETER, PUBLIC :: BLACK = color_type(0, 0, 0, 255)
TYPE(color_type), PARAMETER, PUBLIC :: BLANK = color_type(0, 0, 0, 0)
TYPE(color_type), PARAMETER, PUBLIC :: MAGENTA = color_type(255, 0, 255, 255)
TYPE(color_type), PARAMETER, PUBLIC :: RAYWHITE = &
                                       color_type(245, 245, 245, 255)

END MODULE RaylibTypes
