#define C2F(a, c) IF(PRESENT(a)) CALL C_F_POINTER(obj % a, a, [c])
#define MyNullify(a) obj % a = C_NULL_PTR
#define SimpleSet(a) IF(PRESENT(a)) a = obj % a
#define SimpleNull(a) obj % a = 0
