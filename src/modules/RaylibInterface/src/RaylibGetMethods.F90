! raylib.f90
!
! A collection of auto-generated Fortran 2018 interface bindings to
! raylib 5.1.
!
! Author:  Philipp Engel
! Licence: ISC

MODULE RaylibGetMethods
USE, INTRINSIC :: ISO_C_BINDING
USE RaylibTypes
USE RaylibEnums
IMPLICIT NONE
PRIVATE

PUBLIC :: GetWorldToScreen2D
PUBLIC :: GetWorkingDirectory
PUBLIC :: GetWindowHandle
PUBLIC :: GetTouchY
PUBLIC :: GetTouchX
PUBLIC :: GetTouchPointId
PUBLIC :: GetTouchPointCount
PUBLIC :: GetTime
PUBLIC :: GetSplinePointLinear
PUBLIC :: GetSplinePointCatmullRom
PUBLIC :: GetSplinePointBezierQuad
PUBLIC :: GetSplinePointBezierCubic
PUBLIC :: GetSplinePointBasis
PUBLIC :: GetShaderLocationAttrib
PUBLIC :: GetShaderLocation
PUBLIC :: GetScreenWidth

PUBLIC :: GetScreenToWorld2D
PUBLIC :: GetScreenHeight
PUBLIC :: GetRenderWidth
PUBLIC :: GetRenderHeight
PUBLIC :: GetRayCollisionTriangle
PUBLIC :: GetRayCollisionSphere
PUBLIC :: GetRayCollisionQuad
PUBLIC :: GetRayCollisionMesh
PUBLIC :: GetRayCollisionBox
PUBLIC :: GetRandomValue

PUBLIC :: GetPrevDirectoryPath
PUBLIC :: GetPixelDataSize
PUBLIC :: GetPixelColor
PUBLIC :: GetMusicTimePlayed
PUBLIC :: GetMusicTimeLength
PUBLIC :: GetMouseY
PUBLIC :: GetMouseX
PUBLIC :: GetMouseWheelMove
PUBLIC :: GetMouseRay
PUBLIC :: GetMousePosition
PUBLIC :: GetMouseDelta
PUBLIC :: GetMonitorWidth
PUBLIC :: GetMonitorRefreshRate
PUBLIC :: GetMonitorPhysicalWidth
PUBLIC :: GetMonitorPhysicalHeight
PUBLIC :: GetMonitorName

PUBLIC :: GetMonitorHeight
PUBLIC :: GetMonitorCount
PUBLIC :: GetModelBoundingBox
PUBLIC :: GetMeshBoundingBox
PUBLIC :: GetMasterVolume
PUBLIC :: GetKeyPressed
PUBLIC :: GetImageColor
PUBLIC :: GetImageAlphaBorder
PUBLIC :: GetGlyphInfo
PUBLIC :: GetGlyphIndex
PUBLIC :: GetGlyphAtlasRec
PUBLIC :: GetGesturePinchAngle
PUBLIC :: GetGestureHoldDuration
PUBLIC :: GetGestureDragAngle
PUBLIC :: GetGestureDetected
PUBLIC :: GetGamepadName
PUBLIC :: GetGamepadButtonPressed
PUBLIC :: GetGamepadAxisMovement
PUBLIC :: GetGamepadAxisCount
PUBLIC :: GetFrameTime

PUBLIC :: GetFPS
PUBLIC :: GetFontDefault
PUBLIC :: GetFileNameWithoutExt
PUBLIC :: GetFileName
PUBLIC :: GetFileModTime
PUBLIC :: GetFileLength
PUBLIC :: GetFileExtension
PUBLIC :: GetDirectoryPath
PUBLIC :: GetCurrentMonitor
PUBLIC :: GetColor
PUBLIC :: GetCollisionRec
PUBLIC :: GetCodepointPrevious
PUBLIC :: GetCodepointNext
PUBLIC :: GetCodepointCount
PUBLIC :: GetCodepoint
PUBLIC :: GetClipboardText
PUBLIC :: GetCharPressed
PUBLIC :: GetCameraMatrix2D
PUBLIC :: GetCameraMatrix
PUBLIC :: GetApplicationDirectory

INTERFACE

  FUNCTION GetApplicationDirectory() BIND(c, name='GetApplicationDirectory')
    IMPORT :: C_PTR
    IMPLICIT NONE
    TYPE(C_PTR) :: GetApplicationDirectory
  END FUNCTION GetApplicationDirectory

  ! Matrix GetCameraMatrix(Camera camera)
  FUNCTION GetCameraMatrix(camera) BIND(c, name='GetCameraMatrix')
    IMPORT :: camera3d_, matrix_
    IMPLICIT NONE
    TYPE(camera3d_), INTENT(in), VALUE :: camera
    TYPE(matrix_) :: GetCameraMatrix
  END FUNCTION GetCameraMatrix

  ! Matrix GetCameraMatrix2D(Camera2D camera)
  FUNCTION GetCameraMatrix2D(camera) BIND(c, name='GetCameraMatrix2D')
    IMPORT :: camera2d_, matrix_
    IMPLICIT NONE
    TYPE(camera2d_), INTENT(in), VALUE :: camera
    TYPE(matrix_) :: GetCameraMatrix2D
  END FUNCTION GetCameraMatrix2D

  ! int GetCharPressed(void)
  FUNCTION GetCharPressed() BIND(c, name='GetCharPressed')
    IMPORT :: C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT) :: GetCharPressed
  END FUNCTION GetCharPressed

  ! const char *GetClipboardText(void)
  FUNCTION GetClipboardText() BIND(c, name='GetClipboardText')
    IMPORT :: C_PTR
    IMPLICIT NONE
    TYPE(C_PTR) :: GetClipboardText
  END FUNCTION GetClipboardText

  ! int GetCodepoint(const char *text, int *codepointSize)
  FUNCTION GetCodepoint(text, codepoint_size) BIND(c, name='GetCodepoint')
    IMPORT :: C_CHAR, C_INT
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: text
    INTEGER(kind=C_INT), INTENT(out) :: codepoint_size
    INTEGER(kind=C_INT) :: GetCodepoint
  END FUNCTION GetCodepoint

  ! int GetCodepointCount(const char *text)
  FUNCTION GetCodepointCount(text) BIND(c, name='GetCodepointCount')
    IMPORT :: C_CHAR, C_INT
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: text
    INTEGER(kind=C_INT) :: GetCodepointCount
  END FUNCTION GetCodepointCount

  ! int GetCodepointNext(const char *text, int *codepointSize)
  FUNCTION GetCodepointNext(text, codepoint_size) BIND(c, name='GetCodepointNext')
    IMPORT :: C_CHAR, C_INT
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: text
    INTEGER(kind=C_INT), INTENT(out) :: codepoint_size
    INTEGER(kind=C_INT) :: GetCodepointNext
  END FUNCTION GetCodepointNext

  ! int GetCodepointPrevious(const char *text, int *codepointSize)
  FUNCTION GetCodepointPrevious(text, codepoint_size) &
    BIND(c, name='GetCodepointPrevious')
    IMPORT :: C_CHAR, C_INT
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: text
    INTEGER(kind=C_INT), INTENT(out) :: codepoint_size
    INTEGER(kind=C_INT) :: GetCodepointPrevious
  END FUNCTION GetCodepointPrevious

  ! Rectangle GetCollisionRec(Rectangle rec1, Rectangle rec2)
  FUNCTION GetCollisionRec(rec1, rec2) BIND(c, name='GetCollisionRec')
    IMPORT :: rectangle_
    IMPLICIT NONE
    TYPE(rectangle_), INTENT(in), VALUE :: rec1
    TYPE(rectangle_), INTENT(in), VALUE :: rec2
    TYPE(rectangle_) :: GetCollisionRec
  END FUNCTION GetCollisionRec

  ! Color GetColor(unsigned int hexValue)
  FUNCTION GetColor(hex_value) BIND(c, name='GetColor')
    IMPORT :: c_unsigned_int, color_
    IMPLICIT NONE
    INTEGER(kind=c_unsigned_int), INTENT(in), VALUE :: hex_value
    TYPE(color_) :: GetColor
  END FUNCTION GetColor

  ! int GetCurrentMonitor(void)
  FUNCTION GetCurrentMonitor() BIND(c, name='GetCurrentMonitor')
    IMPORT :: C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT) :: GetCurrentMonitor
  END FUNCTION GetCurrentMonitor

  ! const char *GetDirectoryPath(const char *filePath)
  FUNCTION GetDirectoryPath(file_path) BIND(c, name='GetDirectoryPath')
    IMPORT :: C_CHAR, C_PTR
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: file_path
    TYPE(C_PTR) :: GetDirectoryPath
  END FUNCTION GetDirectoryPath

  ! int GetFPS(void)
  FUNCTION GetFPS() BIND(c, name='GetFPS')
    IMPORT :: C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT) :: GetFPS
  END FUNCTION GetFPS

  ! const char *GetFileExtension(const char *fileName)
  FUNCTION GetFileExtension(file_name) BIND(c, name='GetFileExtension')
    IMPORT :: C_CHAR, C_PTR
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: file_name
    TYPE(C_PTR) :: GetFileExtension
  END FUNCTION GetFileExtension

  ! int GetFileLength(const char *fileName)
  FUNCTION GetFileLength(file_name) BIND(c, name='GetFileLength')
    IMPORT :: C_CHAR, C_INT
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: file_name
    INTEGER(kind=C_INT) :: GetFileLength
  END FUNCTION GetFileLength

  ! long GetFileModTime(const char *fileName)
  FUNCTION GetFileModTime(file_name) BIND(c, name='GetFileModTime')
    IMPORT :: C_CHAR, C_LONG
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: file_name
    INTEGER(kind=C_LONG) :: GetFileModTime
  END FUNCTION GetFileModTime

  ! const char *GetFileName(const char *filePath)
  FUNCTION GetFileName(file_path) BIND(c, name='GetFileName')
    IMPORT :: C_CHAR, C_PTR
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: file_path
    TYPE(C_PTR) :: GetFileName
  END FUNCTION GetFileName

  ! const char *GetFileNameWithoutExt(const char *filePath)
  FUNCTION GetFileNameWithoutExt(file_path) BIND(c, name='GetFileNameWithoutExt')
    IMPORT :: C_CHAR, C_PTR
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: file_path
    TYPE(C_PTR) :: GetFileNameWithoutExt
  END FUNCTION GetFileNameWithoutExt

  ! Font GetFontDefault(void)
  FUNCTION GetFontDefault() BIND(c, name='GetFontDefault')
    IMPORT :: font_
    IMPLICIT NONE
    TYPE(font_) :: GetFontDefault
  END FUNCTION GetFontDefault

  ! float GetFrameTime(void)
  FUNCTION GetFrameTime() BIND(c, name='GetFrameTime')
    IMPORT :: C_FLOAT
    IMPLICIT NONE
    REAL(kind=C_FLOAT) :: GetFrameTime
  END FUNCTION GetFrameTime

  ! int GetGamepadAxisCount(int gamepad)
  FUNCTION GetGamepadAxisCount(gamepad) BIND(c, name='GetGamepadAxisCount')
    IMPORT :: C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: gamepad
    INTEGER(kind=C_INT) :: GetGamepadAxisCount
  END FUNCTION GetGamepadAxisCount

  ! float GetGamepadAxisMovement(int gamepad, int axis)
  FUNCTION GetGamepadAxisMovement(gamepad, axis) BIND(c, name='GetGamepadAxisMovement')
    IMPORT :: C_FLOAT, C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: gamepad
    INTEGER(kind=C_INT), INTENT(in), VALUE :: axis
    REAL(kind=C_FLOAT) :: GetGamepadAxisMovement
  END FUNCTION GetGamepadAxisMovement

  ! int GetGamepadButtonPressed(void)
  FUNCTION GetGamepadButtonPressed() BIND(c, name='GetGamepadButtonPressed')
    IMPORT :: C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT) :: GetGamepadButtonPressed
  END FUNCTION GetGamepadButtonPressed

  ! const char *GetGamepadName(int gamepad)
  FUNCTION GetGamepadName(gamepad) BIND(c, name='GetGamepadName')
    IMPORT :: C_CHAR, C_INT, C_PTR
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: gamepad
    TYPE(C_PTR) :: GetGamepadName
  END FUNCTION GetGamepadName

  ! int GetGestureDetected(void)
  FUNCTION GetGestureDetected() BIND(c, name='GetGestureDetected')
    IMPORT :: C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT) :: GetGestureDetected
  END FUNCTION GetGestureDetected

  ! float GetGestureDragAngle(void)
  FUNCTION GetGestureDragAngle() BIND(c, name='GetGestureDragAngle')
    IMPORT :: C_FLOAT
    IMPLICIT NONE
    REAL(kind=C_FLOAT) :: GetGestureDragAngle
  END FUNCTION GetGestureDragAngle

  ! float GetGestureHoldDuration(void)
  FUNCTION GetGestureHoldDuration() BIND(c, name='GetGestureHoldDuration')
    IMPORT :: C_FLOAT
    IMPLICIT NONE
    REAL(kind=C_FLOAT) :: GetGestureHoldDuration
  END FUNCTION GetGestureHoldDuration

  ! float GetGesturePinchAngle(void)
  FUNCTION GetGesturePinchAngle() BIND(c, name='GetGesturePinchAngle')
    IMPORT :: C_FLOAT
    IMPLICIT NONE
    REAL(kind=C_FLOAT) :: GetGesturePinchAngle
  END FUNCTION GetGesturePinchAngle

  ! Rectangle GetGlyphAtlasRec(Font font, int codepoint)
  FUNCTION GetGlyphAtlasRec(font, codepoint) BIND(c, name='GetGlyphAtlasRec')
    IMPORT :: C_INT, font_, rectangle_
    IMPLICIT NONE
    TYPE(font_), INTENT(in), VALUE :: font
    INTEGER(kind=C_INT), INTENT(in), VALUE :: codepoint
    TYPE(rectangle_) :: GetGlyphAtlasRec
  END FUNCTION GetGlyphAtlasRec

  ! int GetGlyphIndex(Font font, int codepoint)
  FUNCTION GetGlyphIndex(font, codepoint) BIND(c, name='GetGlyphIndex')
    IMPORT :: C_INT, font_
    IMPLICIT NONE
    TYPE(font_), INTENT(in), VALUE :: font
    INTEGER(kind=C_INT), INTENT(in), VALUE :: codepoint
    INTEGER(kind=C_INT) :: GetGlyphIndex
  END FUNCTION GetGlyphIndex

  ! GlyphInfo GetGlyphInfo(Font font, int codepoint)
  FUNCTION GetGlyphInfo(font, codepoint) BIND(c, name='GetGlyphInfo')
    IMPORT :: C_INT, font_, glyph_info_
    IMPLICIT NONE
    TYPE(font_), INTENT(in), VALUE :: font
    INTEGER(kind=C_INT), INTENT(in), VALUE :: codepoint
    TYPE(glyph_info_) :: GetGlyphInfo
  END FUNCTION GetGlyphInfo

  ! float GetMasterVolume(void)
  FUNCTION GetMasterVolume() BIND(c, name='GetMasterVolume')
    IMPORT :: C_FLOAT
    IMPLICIT NONE
    REAL(kind=C_FLOAT) :: GetMasterVolume
  END FUNCTION GetMasterVolume

  ! Rectangle GetImageAlphaBorder(Image image, float threshold)
  FUNCTION GetImageAlphaBorder(image, threshold) BIND(c, name='GetImageAlphaBorder')
    IMPORT :: C_FLOAT, image_, rectangle_
    IMPLICIT NONE
    TYPE(image_), INTENT(in), VALUE :: image
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: threshold
    TYPE(rectangle_) :: GetImageAlphaBorder
  END FUNCTION GetImageAlphaBorder

  ! Vector2 GetSplinePointBasis(Vector2 p1, Vector2 p2, Vector2 p3, Vector2 p4, float t)
  FUNCTION GetSplinePointBasis(p1, p2, p3, p4, t) BIND(c, &
                                                   name='GetSplinePointBasis')
    IMPORT :: C_FLOAT, vector2_
    IMPLICIT NONE
    TYPE(vector2_), INTENT(in), VALUE :: p1
    TYPE(vector2_), INTENT(in), VALUE :: p2
    TYPE(vector2_), INTENT(in), VALUE :: p3
    TYPE(vector2_), INTENT(in), VALUE :: p4
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: t
    TYPE(vector2_) :: GetSplinePointBasis
  END FUNCTION GetSplinePointBasis

  ! Vector2 GetSplinePointBezierCubic(Vector2 p1, Vector2 c2, Vector2 c3, Vector2 p4, float t)
  FUNCTION GetSplinePointBezierCubic(p1, c2, c3, p4, t) BIND(c, &
                                             name='GetSplinePointBezierCubic')
    IMPORT :: C_FLOAT, vector2_
    IMPLICIT NONE
    TYPE(vector2_), INTENT(in), VALUE :: p1
    TYPE(vector2_), INTENT(in), VALUE :: c2
    TYPE(vector2_), INTENT(in), VALUE :: c3
    TYPE(vector2_), INTENT(in), VALUE :: p4
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: t
    TYPE(vector2_) :: GetSplinePointBezierCubic
  END FUNCTION GetSplinePointBezierCubic

  ! Vector2 GetSplinePointBezierQuad(Vector2 p1, Vector2 c2, Vector2 p3, float t)
  FUNCTION GetSplinePointBezierQuad(p1, c2, p3, t) BIND(c, name='GetSplinePointBezierQuad')
    IMPORT :: C_FLOAT, vector2_
    IMPLICIT NONE
    TYPE(vector2_), INTENT(in), VALUE :: p1
    TYPE(vector2_), INTENT(in), VALUE :: c2
    TYPE(vector2_), INTENT(in), VALUE :: p3
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: t
    TYPE(vector2_) :: GetSplinePointBezierQuad
  END FUNCTION GetSplinePointBezierQuad

  ! Vector2 GetSplinePointCatmullRom(Vector2 p1, Vector2 p2, Vector2 p3, Vector2 p4, float t)
  FUNCTION GetSplinePointCatmullRom(p1, p2, p3, p4, t) BIND(c, &
                                              name='GetSplinePointCatmullRom')
    IMPORT :: C_FLOAT, vector2_
    IMPLICIT NONE
    TYPE(vector2_), INTENT(in), VALUE :: p1
    TYPE(vector2_), INTENT(in), VALUE :: p2
    TYPE(vector2_), INTENT(in), VALUE :: p3
    TYPE(vector2_), INTENT(in), VALUE :: p4
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: t
    TYPE(vector2_) :: GetSplinePointCatmullRom
  END FUNCTION GetSplinePointCatmullRom

  ! Vector2 GetSplinePointLinear(Vector2 startPos, Vector2 endPos, float t)
  FUNCTION GetSplinePointLinear(start_pos, end_pos, t) BIND(c, name='GetSplinePointLinear')
    IMPORT :: C_FLOAT, vector2_
    IMPLICIT NONE
    TYPE(vector2_), INTENT(in), VALUE :: start_pos
    TYPE(vector2_), INTENT(in), VALUE :: end_pos
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: t
    TYPE(vector2_) :: GetSplinePointLinear
  END FUNCTION GetSplinePointLinear

  ! Color GetImageColor(Image image, int x, int y)
  FUNCTION GetImageColor(image, x, y) BIND(c, name='GetImageColor')
    IMPORT :: C_INT, color_, image_
    IMPLICIT NONE
    TYPE(image_), INTENT(in), VALUE :: image
    INTEGER(kind=C_INT), INTENT(in), VALUE :: x
    INTEGER(kind=C_INT), INTENT(in), VALUE :: y
    TYPE(color_) :: GetImageColor
  END FUNCTION GetImageColor

  ! int GetKeyPressed(void)
  FUNCTION GetKeyPressed() BIND(c, name='GetKeyPressed')
    IMPORT :: C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT) :: GetKeyPressed
  END FUNCTION GetKeyPressed

  ! BoundingBox GetMeshBoundingBox(Mesh mesh)
  FUNCTION GetMeshBoundingBox(mesh) BIND(c, name='GetMeshBoundingBox')
    IMPORT :: bounding_box_, mesh_
    IMPLICIT NONE
    TYPE(mesh_), INTENT(in), VALUE :: mesh
    TYPE(bounding_box_) :: GetMeshBoundingBox
  END FUNCTION GetMeshBoundingBox

  ! BoundingBox GetModelBoundingBox(Model model)
  FUNCTION GetModelBoundingBox(model) BIND(c, name='GetModelBoundingBox')
    IMPORT :: bounding_box_, model_
    IMPLICIT NONE
    TYPE(model_), INTENT(in), VALUE :: model
    TYPE(bounding_box_) :: GetModelBoundingBox
  END FUNCTION GetModelBoundingBox

  ! int GetMonitorCount(void)
  FUNCTION GetMonitorCount() BIND(c, name='GetMonitorCount')
    IMPORT :: C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT) :: GetMonitorCount
  END FUNCTION GetMonitorCount

  ! int GetMonitorHeight(int monitor)
  FUNCTION GetMonitorHeight(monitor) BIND(c, name='GetMonitorHeight')
    IMPORT :: C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: monitor
    INTEGER(kind=C_INT) :: GetMonitorHeight
  END FUNCTION GetMonitorHeight

  ! const char *GetMonitorName(int monitor)
  FUNCTION GetMonitorName(monitor) BIND(c, name='GetMonitorName')
    IMPORT :: C_INT, C_PTR
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: monitor
    TYPE(C_PTR) :: GetMonitorName
  END FUNCTION GetMonitorName

  ! int GetMonitorPhysicalHeight(int monitor)
  FUNCTION GetMonitorPhysicalHeight(monitor) BIND(c, name='GetMonitorPhysicalHeight')
    IMPORT :: C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: monitor
    INTEGER(kind=C_INT) :: GetMonitorPhysicalHeight
  END FUNCTION GetMonitorPhysicalHeight

  ! int GetMonitorPhysicalWidth(int monitor)
  FUNCTION GetMonitorPhysicalWidth(monitor) BIND(c, name='GetMonitorPhysicalWidth')
    IMPORT :: C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: monitor
    INTEGER(kind=C_INT) :: GetMonitorPhysicalWidth
  END FUNCTION GetMonitorPhysicalWidth

  ! int GetMonitorRefreshRate(int monitor)
 FUNCTION GetMonitorRefreshRate(monitor) BIND(c, name='GetMonitorRefreshRate')
    IMPORT :: C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: monitor
    INTEGER(kind=C_INT) :: GetMonitorRefreshRate
  END FUNCTION GetMonitorRefreshRate

  ! int GetMonitorWidth(int monitor)
  FUNCTION GetMonitorWidth(monitor) BIND(c, name='GetMonitorWidth')
    IMPORT :: C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: monitor
    INTEGER(kind=C_INT) :: GetMonitorWidth
  END FUNCTION GetMonitorWidth

  ! Vector2 GetMouseDelta(void)
  FUNCTION GetMouseDelta() BIND(c, name='GetMouseDelta')
    IMPORT :: vector2_
    IMPLICIT NONE
    TYPE(vector2_) :: GetMouseDelta
  END FUNCTION GetMouseDelta

  ! Vector2 GetMousePosition(void)
  FUNCTION GetMousePosition() BIND(c, name='GetMousePosition')
    IMPORT :: vector2_
    IMPLICIT NONE
    TYPE(vector2_) :: GetMousePosition
  END FUNCTION GetMousePosition

  ! Ray GetMouseRay(Vector2 mousePosition, Camera camera)
  FUNCTION GetMouseRay(mouse_position, camera) BIND(c, name='GetMouseRay')
    IMPORT :: camera3d_, ray_, vector2_
    IMPLICIT NONE
    TYPE(vector2_), INTENT(in), VALUE :: mouse_position
    TYPE(camera3d_), INTENT(in), VALUE :: camera
    TYPE(ray_) :: GetMouseRay
  END FUNCTION GetMouseRay

  ! float GetMouseWheelMove(void)
  FUNCTION GetMouseWheelMove() BIND(c, name='GetMouseWheelMove')
    IMPORT :: C_FLOAT
    IMPLICIT NONE
    REAL(kind=C_FLOAT) :: GetMouseWheelMove
  END FUNCTION GetMouseWheelMove

  ! int GetMouseX(void)
  FUNCTION GetMouseX() BIND(c, name='GetMouseX')
    IMPORT :: C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT) :: GetMouseX
  END FUNCTION GetMouseX

  ! int GetMouseY(void)
  FUNCTION GetMouseY() BIND(c, name='GetMouseY')
    IMPORT :: C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT) :: GetMouseY
  END FUNCTION GetMouseY

  ! float GetMusicTimeLength(Music music)
  FUNCTION GetMusicTimeLength(music) BIND(c, name='GetMusicTimeLength')
    IMPORT :: C_FLOAT, music_
    IMPLICIT NONE
    TYPE(music_), INTENT(in), VALUE :: music
    REAL(kind=C_FLOAT) :: GetMusicTimeLength
  END FUNCTION GetMusicTimeLength

  ! float GetMusicTimePlayed(Music music)
  FUNCTION GetMusicTimePlayed(music) BIND(c, name='GetMusicTimePlayed')
    IMPORT :: C_FLOAT, music_
    IMPLICIT NONE
    TYPE(music_), INTENT(in), VALUE :: music
    REAL(kind=C_FLOAT) :: GetMusicTimePlayed
  END FUNCTION GetMusicTimePlayed

  ! Color GetPixelColor(void *srcPtr, int format)
  FUNCTION GetPixelColor(src_ptr, FORMAT) BIND(c, name='GetPixelColor')
    IMPORT :: C_INT, C_PTR, color_
    IMPLICIT NONE
    TYPE(C_PTR), INTENT(in), VALUE :: src_ptr
    INTEGER(kind=C_INT), INTENT(in), VALUE :: FORMAT
    TYPE(color_) :: GetPixelColor
  END FUNCTION GetPixelColor

  ! int GetPixelDataSize(int width, int height, int format)
  FUNCTION GetPixelDataSize(width, height, FORMAT) BIND(c, name='GetPixelDataSize')
    IMPORT :: C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: width
    INTEGER(kind=C_INT), INTENT(in), VALUE :: height
    INTEGER(kind=C_INT), INTENT(in), VALUE :: FORMAT
    INTEGER(kind=C_INT) :: GetPixelDataSize
  END FUNCTION GetPixelDataSize

  ! const char *GetPrevDirectoryPath(const char *dirPath)
  FUNCTION GetPrevDirectoryPath(dir_path) BIND(c, &
                                               name='GetPrevDirectoryPath')
    IMPORT :: C_CHAR, C_PTR
    IMPLICIT NONE
    CHARACTER(kind=C_CHAR), INTENT(in) :: dir_path
    TYPE(C_PTR) :: GetPrevDirectoryPath
  END FUNCTION GetPrevDirectoryPath

  ! int GetRandomValue(int min, int max)
  FUNCTION GetRandomValue(min, max) BIND(c, name='GetRandomValue')
    IMPORT :: C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: min
    INTEGER(kind=C_INT), INTENT(in), VALUE :: max
    INTEGER(kind=C_INT) :: GetRandomValue
  END FUNCTION GetRandomValue

  ! RayCollision GetRayCollisionBox(Ray ray, BoundingBox box)
  FUNCTION GetRayCollisionBox(ray, box) BIND(c, name='GetRayCollisionBox')
    IMPORT :: bounding_box_, ray_collision_, ray_
    IMPLICIT NONE
    TYPE(ray_), INTENT(in), VALUE :: ray
    TYPE(bounding_box_), INTENT(in), VALUE :: box
    TYPE(ray_collision_) :: GetRayCollisionBox
  END FUNCTION GetRayCollisionBox

  ! RayCollision GetRayCollisionMesh(Ray ray, Mesh mesh, Matrix transform)
  FUNCTION GetRayCollisionMesh(ray, mesh, transform) &
    BIND(c, name='GetRayCollisionMesh')
    IMPORT :: matrix_, mesh_, ray_collision_, ray_
    IMPLICIT NONE
    TYPE(ray_), INTENT(in), VALUE :: ray
    TYPE(mesh_), INTENT(in), VALUE :: mesh
    TYPE(matrix_), INTENT(in), VALUE :: transform
    TYPE(ray_collision_) :: GetRayCollisionMesh
  END FUNCTION GetRayCollisionMesh

  ! RayCollision GetRayCollisionQuad(Ray ray, Vector3 p1, Vector3 p2, Vector3 p3, Vector3 p4)
  FUNCTION GetRayCollisionQuad(ray, p1, p2, p3, p4) &
    BIND(c, name='GetRayCollisionQuad')
    IMPORT :: ray_collision_, ray_, vector3_
    IMPLICIT NONE
    TYPE(ray_), INTENT(in), VALUE :: ray
    TYPE(vector3_), INTENT(in), VALUE :: p1
    TYPE(vector3_), INTENT(in), VALUE :: p2
    TYPE(vector3_), INTENT(in), VALUE :: p3
    TYPE(vector3_), INTENT(in), VALUE :: p4
    TYPE(ray_collision_) :: GetRayCollisionQuad
  END FUNCTION GetRayCollisionQuad

  ! RayCollision GetRayCollisionSphere(Ray ray, Vector3 center, float radius)
  FUNCTION GetRayCollisionSphere(ray, center, radius) &
    BIND(c, name='GetRayCollisionSphere')
    IMPORT :: C_FLOAT, ray_collision_, ray_, vector3_
    IMPLICIT NONE
    TYPE(ray_), INTENT(in), VALUE :: ray
    TYPE(vector3_), INTENT(in), VALUE :: center
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: radius
    TYPE(ray_collision_) :: GetRayCollisionSphere
  END FUNCTION GetRayCollisionSphere

  ! RayCollision GetRayCollisionTriangle(Ray ray, Vector3 p1, Vector3 p2, Vector3 p3)
  FUNCTION GetRayCollisionTriangle(ray, p1, p2, p3) &
    BIND(c, name='GetRayCollisionTriangle')
    IMPORT :: ray_collision_, ray_, vector3_
    IMPLICIT NONE
    TYPE(ray_), INTENT(in), VALUE :: ray
    TYPE(vector3_), INTENT(in), VALUE :: p1
    TYPE(vector3_), INTENT(in), VALUE :: p2
    TYPE(vector3_), INTENT(in), VALUE :: p3
    TYPE(ray_collision_) :: GetRayCollisionTriangle
  END FUNCTION GetRayCollisionTriangle

  ! int GetRenderHeight(void)
  FUNCTION GetRenderHeight() BIND(c, name='GetRenderHeight')
    IMPORT :: C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT) :: GetRenderHeight
  END FUNCTION GetRenderHeight

  ! int GetRenderWidth(void)
  FUNCTION GetRenderWidth() BIND(c, name='GetRenderWidth')
    IMPORT :: C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT) :: GetRenderWidth
  END FUNCTION GetRenderWidth

  ! int GetScreenHeight(void)
  FUNCTION GetScreenHeight() BIND(c, name='GetScreenHeight')
    IMPORT :: C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT) :: GetScreenHeight
  END FUNCTION GetScreenHeight

  ! Vector2 GetScreenToWorld2D(Vector2 position, Camera2D camera)
  FUNCTION GetScreenToWorld2D(position, camera) &
    BIND(c, name='GetScreenToWorld2D')
    IMPORT :: camera2d_, vector2_
    IMPLICIT NONE
    TYPE(vector2_), INTENT(in), VALUE :: position
    TYPE(camera2d_), INTENT(in), VALUE :: camera
    TYPE(vector2_) :: GetScreenToWorld2D
  END FUNCTION GetScreenToWorld2D

  ! int GetScreenWidth(void)
  FUNCTION GetScreenWidth() BIND(c, name='GetScreenWidth')
    IMPORT :: C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT) :: GetScreenWidth
  END FUNCTION GetScreenWidth

  ! int GetShaderLocation(Shader shader, const char *uniformName)
  FUNCTION GetShaderLocation(shader, uniform_name) &
    BIND(c, name='GetShaderLocation')
    IMPORT :: C_CHAR, C_INT, shader_
    IMPLICIT NONE
    TYPE(shader_), INTENT(in), VALUE :: shader
    CHARACTER(kind=C_CHAR), INTENT(in) :: uniform_name
    INTEGER(kind=C_INT) :: GetShaderLocation
  END FUNCTION GetShaderLocation

  ! int GetShaderLocationAttrib(Shader shader, const char *attribName)
  FUNCTION GetShaderLocationAttrib(shader, attrib_name) &
    BIND(c, name='GetShaderLocationAttrib')
    IMPORT :: C_CHAR, C_INT, shader_
    IMPLICIT NONE
    TYPE(shader_), INTENT(in), VALUE :: shader
    CHARACTER(kind=C_CHAR), INTENT(in) :: attrib_name
    INTEGER(kind=C_INT) :: GetShaderLocationAttrib
  END FUNCTION GetShaderLocationAttrib

  ! double GetTime(void)
  FUNCTION GetTime() BIND(c, name='GetTime')
    IMPORT :: C_DOUBLE
    IMPLICIT NONE
    REAL(kind=C_DOUBLE) :: GetTime
  END FUNCTION GetTime

  ! int GetTouchPointCount(void)
  FUNCTION GetTouchPointCount() BIND(c, name='GetTouchPointCount')
    IMPORT :: C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT) :: GetTouchPointCount
  END FUNCTION GetTouchPointCount

  ! int GetTouchPointId(int index)
  FUNCTION GetTouchPointId(index) BIND(c, name='GetTouchPointId')
    IMPORT :: C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT), INTENT(in), VALUE :: index
    INTEGER(kind=C_INT) :: GetTouchPointId
  END FUNCTION GetTouchPointId

  ! int GetTouchX(void)
  FUNCTION GetTouchX() BIND(c, name='GetTouchX')
    IMPORT :: C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT) :: GetTouchX
  END FUNCTION GetTouchX

  ! int GetTouchY(void)
  FUNCTION GetTouchY() BIND(c, name='GetTouchY')
    IMPORT :: C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT) :: GetTouchY
  END FUNCTION GetTouchY

  ! void *GetWindowHandle(void)
  FUNCTION GetWindowHandle() BIND(c, name='GetWindowHandle')
    IMPORT :: C_PTR
    IMPLICIT NONE
    TYPE(C_PTR) :: GetWindowHandle
  END FUNCTION GetWindowHandle

  ! const char *GetWorkingDirectory(void)
  FUNCTION GetWorkingDirectory() BIND(c, name='GetWorkingDirectory')
    IMPORT :: C_PTR
    IMPLICIT NONE
    TYPE(C_PTR) :: GetWorkingDirectory
  END FUNCTION GetWorkingDirectory

  ! Vector2 GetWorldToScreen2D(Vector2 position, Camera2D camera)
  FUNCTION GetWorldToScreen2D(position, camera) &
    BIND(c, name='GetWorldToScreen2D')
    IMPORT :: camera2d_, vector2_
    IMPLICIT NONE
    TYPE(vector2_), INTENT(in), VALUE :: position
    TYPE(camera2d_), INTENT(in), VALUE :: camera
    TYPE(vector2_) :: GetWorldToScreen2D
  END FUNCTION GetWorldToScreen2D

END INTERFACE

END MODULE RaylibGetMethods
