#include "chibicc.h"

#define GP_MAX 6
#define FP_MAX 8

static FILE *output_file;
static int depth;
static char *argreg64[] = {"%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9"};
static Obj *current_fn;
static int current_tmp;

// return the QBE tmp %N containing the value
static int gen_expr_qbe(Node *node);
static void gen_stmt_qbe(Node *node);

__attribute__((format(printf, 1, 2)))
static void println(char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  vfprintf(output_file, fmt, ap);
  va_end(ap);
  fprintf(output_file, "\n");
}

__attribute__((format(printf, 1, 2)))
static void print(char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  vfprintf(output_file, fmt, ap);
  va_end(ap);
}

static void printstructident(Type* ty) {
  // Use the memory address of the type to disambiguate homonymous structs
  print("%s.%lx", (ty->ident ? ty->ident : "anon"), (long)ty);
}

static void printparamtype(Type* ty) {
  char t = 0;
  bool skip = false;
  
  switch (ty->kind) {
  case TY_VOID:
    skip = true;
    break;
    
  case TY_BOOL:
  case TY_CHAR:
    t = 'b';
    break;
    
  case TY_SHORT:
    t = 'h';
    break;
    
  case TY_INT:
  case TY_ENUM:
    t = 'w';
    break;
    
  case TY_FLOAT:
    t = 's';
    break;
    
  case TY_DOUBLE:
  case TY_LDOUBLE:
    t = 'd';
    break;

  case TY_LONG:
  case TY_PTR:
  case TY_FUNC:
  case TY_ARRAY:
  case TY_VLA: // variable-length array
    t = 'l';
    break;
    
  case TY_STRUCT:
  case TY_UNION:
    print(":");
    printstructident(ty);
    skip = true;
  }

  if (!skip)
    print("%c", t);
}

static char qbe_base_type(Type* ty) {
  switch (ty->kind) {
  case TY_VOID:
    return 'l'; // ??? Be careful where this is used
    
  case TY_BOOL:
  case TY_CHAR:
  case TY_SHORT:
  case TY_INT:
  case TY_ENUM:
    return 'w';
    
  case TY_FLOAT:
    return 's';
    
  case TY_DOUBLE:
  case TY_LDOUBLE:
    return 'd';

  case TY_LONG:
  case TY_PTR:
  case TY_FUNC:
  case TY_ARRAY:
  case TY_VLA: // variable-length array
  case TY_STRUCT:
  case TY_UNION:
    return 'l';
  }
  return '!';
}

static char qbe_ext_type(Type* ty) {
  switch (ty->kind) {
  case TY_VOID:
    return 'l'; // ??? Be careful where this is used
    
  case TY_CHAR:
  case TY_SHORT:
  case TY_INT:
  case TY_ENUM:
  case TY_BOOL:
    return 'w';
    
  case TY_FLOAT:
    return 's';
    
  case TY_DOUBLE:
  case TY_LDOUBLE:
    return 'd';

  case TY_LONG:
  case TY_PTR:
  case TY_FUNC:
  case TY_ARRAY:
  case TY_VLA: // variable-length array
  case TY_STRUCT:
  case TY_UNION:
    return 'l';
  }
  return '!';
}

static void printstructarraytype(Type* ty, unsigned long accum) {
  if (ty->kind == TY_ARRAY) {
    printstructarraytype(ty->base, accum * ty->array_len);
  }
  else {
    printparamtype(ty);
    print(" %lu", accum);
  }
}

static void printstructmembertype(Type* ty) {
  // Arrays are inlined in structs
  if (ty->kind == TY_ARRAY) {
    // Need to multiply out multi-dimensional arrays...
    printstructarraytype(ty->base, ty->array_len);
    return;
  }

  printparamtype(ty);
}

/* static void printlocalvartype(Type* ty) { */
/*   // structs and unions are just pointers, represented as long 'l' */
/*   if (ty->kind == TY_STRUCT || ty->kind == TY_UNION) { */
/*     print("%c", 'l'); */
/*     return; */
/*   } */

/*   printparamtype(ty); */
/* } */

// QBE identifier for a param or local variable
static void printlocalname(Obj* local) {
  print("%s.%d", local->name, -local->offset);
}


static int count(void) {
  static int i = 1;
  return i++;
}

static void push(void) {
  println("#  push %%rax");
  depth++;
}

static void pop(char *arg) {
  println("#  pop %s", arg);
  depth--;
}

static void pushf(void) {
  println("#  sub $8, %%rsp");
  println("#  movsd %%xmm0, (%%rsp)");
  depth++;
}

static void popf(int reg) {
  println("#  movsd (%%rsp), %%xmm%d", reg);
  println("#  add $8, %%rsp");
  depth--;
}

static char *reg_dx(int sz) {
  switch (sz) {
  case 1: return "%dl";
  case 2: return "%dx";
  case 4: return "%edx";
  case 8: return "%rdx";
  }
  unreachable();
}

static char *reg_ax(int sz) {
  switch (sz) {
  case 1: return "%al";
  case 2: return "%ax";
  case 4: return "%eax";
  case 8: return "%rax";
  }
  unreachable();
}

// Compute the absolute address of a given node.
// It's an error if a given node does not reside in memory.
// Return the temporary that the address resides in.
static int gen_addr(Node *node) {
  switch (node->kind) {
  case ND_VAR:
    // Variable-length array, which is always local.
    if (node->var->ty->kind == TY_VLA) {
      println("#  mov %d(%%rbp), %%rax", node->var->offset);
      return 654321;
    }

    // Local variable
    if (node->var->is_local) {
      println("#  lea %d(%%rbp), %%rax", node->var->offset);
      return 654321;
    }

    if (opt_fpic) {
      // Thread-local variable
      if (node->var->is_tls) {
        println("#  data16 lea %s@tlsgd(%%rip), %%rdi", node->var->name);
        println("#  .value 0x6666");
        println("#  rex64");
        println("#  call __tls_get_addr@PLT");
        return 654321;
      }

      // Function or global variable
      println("#  mov %s@GOTPCREL(%%rip), %%rax", node->var->name);
      return 654321;
    }

    // Thread-local variable
    if (node->var->is_tls) {
      println("#  mov %%fs:0, %%rax");
      println("#  add $%s@tpoff, %%rax", node->var->name);
      return 654321;
    }

    // Here, we generate an absolute address of a function or a global
    // variable. Even though they exist at a certain address at runtime,
    // their addresses are not known at link-time for the following
    // two reasons.
    //
    //  - Address randomization: Executables are loaded to memory as a
    //    whole but it is not known what address they are loaded to.
    //    Therefore, at link-time, relative address in the same
    //    exectuable (i.e. the distance between two functions in the
    //    same executable) is known, but the absolute address is not
    //    known.
    //
    //  - Dynamic linking: Dynamic shared objects (DSOs) or .so files
    //    are loaded to memory alongside an executable at runtime and
    //    linked by the runtime loader in memory. We know nothing
    //    about addresses of global stuff that may be defined by DSOs
    //    until the runtime relocation is complete.
    //
    // In order to deal with the former case, we use RIP-relative
    // addressing, denoted by `(%rip)`. For the latter, we obtain an
    // address of a stuff that may be in a shared object file from the
    // Global Offset Table using `@GOTPCREL(%rip)` notation.

    // Function
    if (node->ty->kind == TY_FUNC) {
      if (node->var->is_definition)
        println("#  lea %s(%%rip), %%rax", node->var->name);
      else
        println("#  mov %s@GOTPCREL(%%rip), %%rax", node->var->name);
      return 654321;
    }

    // Global variable
    println("#  lea %s(%%rip), %%rax", node->var->name);
    return 654321;
  case ND_DEREF:
    gen_expr_qbe(node->lhs);
    return 654321;
  case ND_COMMA:
    gen_expr_qbe(node->lhs);
    gen_addr(node->rhs);
    return 654321;
  case ND_MEMBER:
    gen_addr(node->lhs);
    println("#  add $%d, %%rax", node->member->offset);
    return 654321;
  case ND_FUNCALL:
    if (node->ret_buffer) {
      gen_expr_qbe(node);
      return 654321;
    }
    break;
  case ND_ASSIGN:
  case ND_COND:
    if (node->ty->kind == TY_STRUCT || node->ty->kind == TY_UNION) {
      gen_expr_qbe(node);
      return 654321;
    }
    break;
  case ND_VLA_PTR:
    println("#  lea %d(%%rbp), %%rax", node->var->offset);
    return 654321;
  }

  error_tok(node->tok, "not an lvalue");
}

// Load a value from where %rax is pointing to.
static void load_qbe(int from_tmp, int to_tmp, Type *ty) {
  switch (ty->kind) {
  case TY_ARRAY:
  case TY_STRUCT:
  case TY_UNION:
  case TY_FUNC:
  case TY_VLA:
    // If it is an array, do not attempt to load a value to the
    // register because in general we can't load an entire array to a
    // register. As a result, the result of an evaluation of an array
    // becomes not the array itself but the address of the array.
    // This is where "array is automatically converted to a pointer to
    // the first element of the array in C" occurs.
    return;
  case TY_FLOAT:
    println("#  movss (%%rax), %%xmm0");
    return;
  case TY_DOUBLE:
    println("#  movsd (%%rax), %%xmm0");
    return;
  case TY_LDOUBLE:
    println("#  fldt (%%rax)");
    return;
  }

  char *insn = ty->is_unsigned ? "movz" : "movs";

  // When we load a char or a short value to a register, we always
  // extend them to the size of int, so we can assume the lower half of
  // a register always contains a valid value. The upper half of a
  // register for char, short and int may contain garbage. When we load
  // a long value to a register, it simply occupies the entire register.
  if (ty->size == 1)
    println("#  %sbl (%%rax), %%eax", insn);
  else if (ty->size == 2)
    println("#  %swl (%%rax), %%eax", insn);
  else if (ty->size == 4)
    println("#  movsxd (%%rax), %%rax");
  else
    println("#  mov (%%rax), %%rax");
}

// Store %rax to an address that the stack top is pointing to.
static void store_qbe(int from_tmp, int to_tmp, Type *ty) {
  pop("%rdi");

  switch (ty->kind) {
  case TY_STRUCT:
  case TY_UNION:
    for (int i = 0; i < ty->size; i++) {
      println("#  mov %d(%%rax), %%r8b", i);
      println("#  mov %%r8b, %d(%%rdi)", i);
    }
    return;
  case TY_FLOAT:
    println("#  movss %%xmm0, (%%rdi)");
    return;
  case TY_DOUBLE:
    println("#  movsd %%xmm0, (%%rdi)");
    return;
  case TY_LDOUBLE:
    println("#  fstpt (%%rdi)");
    return;
  }

  if (ty->size == 1)
    println("#  mov %%al, (%%rdi)");
  else if (ty->size == 2)
    println("#  mov %%ax, (%%rdi)");
  else if (ty->size == 4)
    println("#  mov %%eax, (%%rdi)");
  else
    println("#  mov %%rax, (%%rdi)");
}

static void cmp_zero(Type *ty) {
  switch (ty->kind) {
  case TY_FLOAT:
    println("#  xorps %%xmm1, %%xmm1");
    println("#  ucomiss %%xmm1, %%xmm0");
    return;
  case TY_DOUBLE:
    println("#  xorpd %%xmm1, %%xmm1");
    println("#  ucomisd %%xmm1, %%xmm0");
    return;
  case TY_LDOUBLE:
    println("#  fldz");
    println("#  fucomip");
    println("#  fstp %%st(0)");
    return;
  }

  if (is_integer(ty) && ty->size <= 4)
    println("#  cmp $0, %%eax");
  else
    println("#  cmp $0, %%rax");
}

enum { I8, I16, I32, I64, U8, U16, U32, U64, F32, F64, F80 };

static int getTypeId(Type *ty) {
  switch (ty->kind) {
  case TY_CHAR:
    return ty->is_unsigned ? U8 : I8;
  case TY_SHORT:
    return ty->is_unsigned ? U16 : I16;
  case TY_INT:
    return ty->is_unsigned ? U32 : I32;
  case TY_LONG:
    return ty->is_unsigned ? U64 : I64;
  case TY_FLOAT:
    return F32;
  case TY_DOUBLE:
    return F64;
  case TY_LDOUBLE:
    return F80;
  }
  return U64;
}

// The table for type casts
static char i32i8[] = "movsbl %al, %eax";
static char i32u8[] = "movzbl %al, %eax";
static char i32i16[] = "movswl %ax, %eax";
static char i32u16[] = "movzwl %ax, %eax";
static char i32f32[] = "cvtsi2ssl %eax, %xmm0";
static char i32i64[] = "movsxd %eax, %rax";
static char i32f64[] = "cvtsi2sdl %eax, %xmm0";
static char i32f80[] = "mov %eax, -4(%rsp); fildl -4(%rsp)";

static char u32f32[] = "mov %eax, %eax; cvtsi2ssq %rax, %xmm0";
static char u32i64[] = "mov %eax, %eax";
static char u32f64[] = "mov %eax, %eax; cvtsi2sdq %rax, %xmm0";
static char u32f80[] = "mov %eax, %eax; mov %rax, -8(%rsp); fildll -8(%rsp)";

static char i64f32[] = "cvtsi2ssq %rax, %xmm0";
static char i64f64[] = "cvtsi2sdq %rax, %xmm0";
static char i64f80[] = "movq %rax, -8(%rsp); fildll -8(%rsp)";

static char u64f32[] = "cvtsi2ssq %rax, %xmm0";
static char u64f64[] =
  "test %rax,%rax; js 1f; pxor %xmm0,%xmm0; cvtsi2sd %rax,%xmm0; jmp 2f; "
  "1: mov %rax,%rdi; and $1,%eax; pxor %xmm0,%xmm0; shr %rdi; "
  "or %rax,%rdi; cvtsi2sd %rdi,%xmm0; addsd %xmm0,%xmm0; 2:";
static char u64f80[] =
  "mov %rax, -8(%rsp); fildq -8(%rsp); test %rax, %rax; jns 1f;"
  "mov $1602224128, %eax; mov %eax, -4(%rsp); fadds -4(%rsp); 1:";

static char f32i8[] = "cvttss2sil %xmm0, %eax; movsbl %al, %eax";
static char f32u8[] = "cvttss2sil %xmm0, %eax; movzbl %al, %eax";
static char f32i16[] = "cvttss2sil %xmm0, %eax; movswl %ax, %eax";
static char f32u16[] = "cvttss2sil %xmm0, %eax; movzwl %ax, %eax";
static char f32i32[] = "cvttss2sil %xmm0, %eax";
static char f32u32[] = "cvttss2siq %xmm0, %rax";
static char f32i64[] = "cvttss2siq %xmm0, %rax";
static char f32u64[] = "cvttss2siq %xmm0, %rax";
static char f32f64[] = "cvtss2sd %xmm0, %xmm0";
static char f32f80[] = "movss %xmm0, -4(%rsp); flds -4(%rsp)";

static char f64i8[] = "cvttsd2sil %xmm0, %eax; movsbl %al, %eax";
static char f64u8[] = "cvttsd2sil %xmm0, %eax; movzbl %al, %eax";
static char f64i16[] = "cvttsd2sil %xmm0, %eax; movswl %ax, %eax";
static char f64u16[] = "cvttsd2sil %xmm0, %eax; movzwl %ax, %eax";
static char f64i32[] = "cvttsd2sil %xmm0, %eax";
static char f64u32[] = "cvttsd2siq %xmm0, %rax";
static char f64i64[] = "cvttsd2siq %xmm0, %rax";
static char f64u64[] = "cvttsd2siq %xmm0, %rax";
static char f64f32[] = "cvtsd2ss %xmm0, %xmm0";
static char f64f80[] = "movsd %xmm0, -8(%rsp); fldl -8(%rsp)";

#define FROM_F80_1                                           \
  "fnstcw -10(%rsp); movzwl -10(%rsp), %eax; or $12, %ah; " \
  "mov %ax, -12(%rsp); fldcw -12(%rsp); "

#define FROM_F80_2 " -24(%rsp); fldcw -10(%rsp); "

static char f80i8[] = FROM_F80_1 "fistps" FROM_F80_2 "movsbl -24(%rsp), %eax";
static char f80u8[] = FROM_F80_1 "fistps" FROM_F80_2 "movzbl -24(%rsp), %eax";
static char f80i16[] = FROM_F80_1 "fistps" FROM_F80_2 "movzbl -24(%rsp), %eax";
static char f80u16[] = FROM_F80_1 "fistpl" FROM_F80_2 "movswl -24(%rsp), %eax";
static char f80i32[] = FROM_F80_1 "fistpl" FROM_F80_2 "mov -24(%rsp), %eax";
static char f80u32[] = FROM_F80_1 "fistpl" FROM_F80_2 "mov -24(%rsp), %eax";
static char f80i64[] = FROM_F80_1 "fistpq" FROM_F80_2 "mov -24(%rsp), %rax";
static char f80u64[] = FROM_F80_1 "fistpq" FROM_F80_2 "mov -24(%rsp), %rax";
static char f80f32[] = "fstps -8(%rsp); movss -8(%rsp), %xmm0";
static char f80f64[] = "fstpl -8(%rsp); movsd -8(%rsp), %xmm0";

static char *cast_table[][11] = {
  // i8   i16     i32     i64     u8     u16     u32     u64     f32     f64     f80  << to
  //                                                                                         vv from
  {NULL,  NULL,   NULL,   i32i64, i32u8, i32u16, NULL,   i32i64, i32f32, i32f64, i32f80}, // i8
  {i32i8, NULL,   NULL,   i32i64, i32u8, i32u16, NULL,   i32i64, i32f32, i32f64, i32f80}, // i16
  {i32i8, i32i16, NULL,   i32i64, i32u8, i32u16, NULL,   i32i64, i32f32, i32f64, i32f80}, // i32
  {i32i8, i32i16, NULL,   NULL,   i32u8, i32u16, NULL,   NULL,   i64f32, i64f64, i64f80}, // i64

  {i32i8, NULL,   NULL,   i32i64, NULL,  NULL,   NULL,   i32i64, i32f32, i32f64, i32f80}, // u8
  {i32i8, i32i16, NULL,   i32i64, i32u8, NULL,   NULL,   i32i64, i32f32, i32f64, i32f80}, // u16
  {i32i8, i32i16, NULL,   u32i64, i32u8, i32u16, NULL,   u32i64, u32f32, u32f64, u32f80}, // u32
  {i32i8, i32i16, NULL,   NULL,   i32u8, i32u16, NULL,   NULL,   u64f32, u64f64, u64f80}, // u64

  {f32i8, f32i16, f32i32, f32i64, f32u8, f32u16, f32u32, f32u64, NULL,   f32f64, f32f80}, // f32
  {f64i8, f64i16, f64i32, f64i64, f64u8, f64u16, f64u32, f64u64, f64f32, NULL,   f64f80}, // f64
  {f80i8, f80i16, f80i32, f80i64, f80u8, f80u16, f80u32, f80u64, f80f32, f80f64, NULL},   // f80
};

// Wrong way round?
static char *cast_table_qbe[][11] = {
  // i8   i16     i32     i64     u8     u16     u32     u64     f32     f64     f80  << to
  //                                                                                         vv from
  {NULL,  "extsb",   "extsb",   NULL,   "extsb", i32u16, NULL,   i32i64, i32f32, i32f64, i32f80}, // i8
  {i32i8, NULL,   NULL,   i32i64, i32u8, i32u16, NULL,   i32i64, i32f32, i32f64, i32f80}, // i16
  {i32i8, i32i16, NULL,   i32i64, i32u8, i32u16, NULL,   i32i64, i32f32, i32f64, i32f80}, // i32
  {i32i8, i32i16, NULL,   NULL,   i32u8, i32u16, NULL,   NULL,   i64f32, i64f64, i64f80}, // i64

  {i32i8, NULL,   NULL,   i32i64, NULL,  NULL,   NULL,   i32i64, i32f32, i32f64, i32f80}, // u8
  {i32i8, i32i16, NULL,   i32i64, i32u8, NULL,   NULL,   i32i64, i32f32, i32f64, i32f80}, // u16
  {i32i8, i32i16, NULL,   u32i64, i32u8, i32u16, NULL,   u32i64, u32f32, u32f64, u32f80}, // u32
  {i32i8, i32i16, NULL,   NULL,   i32u8, i32u16, NULL,   NULL,   u64f32, u64f64, u64f80}, // u64

  {f32i8, f32i16, f32i32, f32i64, f32u8, f32u16, f32u32, f32u64, NULL,   f32f64, f32f80}, // f32
  {f64i8, f64i16, f64i32, f64i64, f64u8, f64u16, f64u32, f64u64, f64f32, NULL,   f64f80}, // f64
  {f80i8, f80i16, f80i32, f80i64, f80u8, f80u16, f80u32, f80u64, f80f32, f80f64, NULL},   // f80
};

static void cast_qbe(int from_tmp, Type *from, int to_tmp, Type *to) {
  if (to->kind == TY_VOID)
    return;

  if (to->kind == TY_BOOL) {
    // TODO - from fp?
    println("  %%%d =%c cne%c %%%d, 0", to_tmp, qbe_base_type(from), qbe_base_type(to), from_tmp);
    cmp_zero(from);
    return;
  }

  if (is_integer(from) && is_integer(to)) {
    if (from->size < to->size) {
      // We sign/zero extend the 'from' type to the 'to' type
      // TODO - check the C spec here (gcc behaves oddly in some scenarios like casting u16 to i16
      char to_sign = to->is_unsigned ? 'u' : 's';
      println("  %%%d =%c ext%c%c %%%d", to_tmp, qbe_base_type(to), to_sign, qbe_ext_type(from), from_tmp);
    }
    else {
      // from->size >= to->size
      // We mask the "from" value to the "to" value size
      long mask = ((long)1 << (to->size * 8)) - 1;
      println("  %%%d =%c and %%%d, %ld # 0x%lx", to_tmp, qbe_base_type(to), from_tmp, mask, mask);
    }
    return;
  }

  // Float to int or vice-versa
  char *from_sign = is_integer(from) ? (from->is_unsigned ? "u" : "s") : "";
  char *to_sign = is_integer(to) ? (to->is_unsigned ? "u" : "s") : "";
  println("  %%%d =%c %s%cto%s%c %%%d", to_tmp, qbe_base_type(to), from_sign, qbe_base_type(from), to_sign, qbe_base_type(to), from_tmp);
}

// Structs or unions equal or smaller than 16 bytes are passed
// using up to two registers.
//
// If the first 8 bytes contains only floating-point type members,
// they are passed in an XMM register. Otherwise, they are passed
// in a general-purpose register.
//
// If a struct/union is larger than 8 bytes, the same rule is
// applied to the the next 8 byte chunk.
//
// This function returns true if `ty` has only floating-point
// members in its byte range [lo, hi).
static bool has_flonum(Type *ty, int lo, int hi, int offset) {
  if (ty->kind == TY_STRUCT || ty->kind == TY_UNION) {
    for (Member *mem = ty->members; mem; mem = mem->next)
      if (!has_flonum(mem->ty, lo, hi, offset + mem->offset))
        return false;
    return true;
  }

  if (ty->kind == TY_ARRAY) {
    for (int i = 0; i < ty->array_len; i++)
      if (!has_flonum(ty->base, lo, hi, offset + ty->base->size * i))
        return false;
    return true;
  }

  return offset < lo || hi <= offset || ty->kind == TY_FLOAT || ty->kind == TY_DOUBLE;
}

static bool has_flonum1(Type *ty) {
  return has_flonum(ty, 0, 8, 0);
}

static bool has_flonum2(Type *ty) {
  return has_flonum(ty, 8, 16, 0);
}

static void push_struct(Type *ty) {
  int sz = align_to(ty->size, 8);
  println("#  sub $%d, %%rsp", sz);
  depth += sz / 8;

  for (int i = 0; i < ty->size; i++) {
    println("#  mov %d(%%rax), %%r10b", i);
    println("#  mov %%r10b, %d(%%rsp)", i);
  }
}

static void push_args2(Node *args, bool first_pass) {
  if (!args)
    return;
  push_args2(args->next, first_pass);

  if ((first_pass && !args->pass_by_stack) || (!first_pass && args->pass_by_stack))
    return;

  gen_expr_qbe(args);

  switch (args->ty->kind) {
  case TY_STRUCT:
  case TY_UNION:
    push_struct(args->ty);
    break;
  case TY_FLOAT:
  case TY_DOUBLE:
    pushf();
    break;
  case TY_LDOUBLE:
    println("#  sub $16, %%rsp");
    println("#  fstpt (%%rsp)");
    depth += 2;
    break;
  default:
    push();
  }
}

// Load function call arguments. Arguments are already evaluated and
// stored to the stack as local variables. What we need to do in this
// function is to load them to registers or push them to the stack as
// specified by the x86-64 psABI. Here is what the spec says:
//
// - Up to 6 arguments of integral type are passed using RDI, RSI,
//   RDX, RCX, R8 and R9.
//
// - Up to 8 arguments of floating-point type are passed using XMM0 to
//   XMM7.
//
// - If all registers of an appropriate type are already used, push an
//   argument to the stack in the right-to-left order.
//
// - Each argument passed on the stack takes 8 bytes, and the end of
//   the argument area must be aligned to a 16 byte boundary.
//
// - If a function is variadic, set the number of floating-point type
//   arguments to RAX.
static int push_args(Node *node) {
  int stack = 0, gp = 0, fp = 0;

  // If the return type is a large struct/union, the caller passes
  // a pointer to a buffer as if it were the first argument.
  if (node->ret_buffer && node->ty->size > 16)
    gp++;

  // Load as many arguments to the registers as possible.
  for (Node *arg = node->args; arg; arg = arg->next) {
    Type *ty = arg->ty;

    switch (ty->kind) {
    case TY_STRUCT:
    case TY_UNION:
      if (ty->size > 16) {
        arg->pass_by_stack = true;
        stack += align_to(ty->size, 8) / 8;
      } else {
        bool fp1 = has_flonum1(ty);
        bool fp2 = has_flonum2(ty);

        if (fp + fp1 + fp2 < FP_MAX && gp + !fp1 + !fp2 < GP_MAX) {
          fp = fp + fp1 + fp2;
          gp = gp + !fp1 + !fp2;
        } else {
          arg->pass_by_stack = true;
          stack += align_to(ty->size, 8) / 8;
        }
      }
      break;
    case TY_FLOAT:
    case TY_DOUBLE:
      if (fp++ >= FP_MAX) {
        arg->pass_by_stack = true;
        stack++;
      }
      break;
    case TY_LDOUBLE:
      arg->pass_by_stack = true;
      stack += 2;
      break;
    default:
      if (gp++ >= GP_MAX) {
        arg->pass_by_stack = true;
        stack++;
      }
    }
  }

  if ((depth + stack) % 2 == 1) {
    println("#  sub $8, %%rsp");
    depth++;
    stack++;
  }

  push_args2(node->args, true);
  push_args2(node->args, false);

  // If the return type is a large struct/union, the caller passes
  // a pointer to a buffer as if it were the first argument.
  if (node->ret_buffer && node->ty->size > 16) {
    println("#  lea %d(%%rbp), %%rax", node->ret_buffer->offset);
    push();
  }

  return stack;
}

static void copy_ret_buffer(Obj *var) {
  Type *ty = var->ty;
  int gp = 0, fp = 0;

  if (has_flonum1(ty)) {
    assert(ty->size == 4 || 8 <= ty->size);
    if (ty->size == 4)
      println("#  movss %%xmm0, %d(%%rbp)", var->offset);
    else
      println("#  movsd %%xmm0, %d(%%rbp)", var->offset);
    fp++;
  } else {
    for (int i = 0; i < MIN(8, ty->size); i++) {
      println("#  mov %%al, %d(%%rbp)", var->offset + i);
      println("#  shr $8, %%rax");
    }
    gp++;
  }

  if (ty->size > 8) {
    if (has_flonum2(ty)) {
      assert(ty->size == 12 || ty->size == 16);
      if (ty->size == 12)
        println("#  movss %%xmm%d, %d(%%rbp)", fp, var->offset + 8);
      else
        println("#  movsd %%xmm%d, %d(%%rbp)", fp, var->offset + 8);
    } else {
      char *reg1 = (gp == 0) ? "%al" : "%dl";
      char *reg2 = (gp == 0) ? "%rax" : "%rdx";
      for (int i = 8; i < MIN(16, ty->size); i++) {
        println("#  mov %s, %d(%%rbp)", reg1, var->offset + i);
        println("#  shr $8, %s", reg2);
      }
    }
  }
}

static void builtin_alloca(void) {
  // Align size to 16 bytes.
  println("#  add $15, %%rdi");
  println("#  and $0xfffffff0, %%edi");

  // Shift the temporary area by %rdi.
  println("#  mov %d(%%rbp), %%rcx", current_fn->alloca_bottom->offset);
  println("#  sub %%rsp, %%rcx");
  println("#  mov %%rsp, %%rax");
  println("#  sub %%rdi, %%rsp");
  println("#  mov %%rsp, %%rdx");
  println("#1:");
  println("#  cmp $0, %%rcx");
  println("#  je 2f");
  println("#  mov (%%rax), %%r8b");
  println("#  mov %%r8b, (%%rdx)");
  println("#  inc %%rdx");
  println("#  inc %%rax");
  println("#  dec %%rcx");
  println("#  jmp 1b");
  println("#2:");

  // Move alloca_bottom pointer.
  println("#  mov %d(%%rbp), %%rax", current_fn->alloca_bottom->offset);
  println("#  sub %%rdi, %%rax");
  println("#  mov %%rax, %d(%%rbp)", current_fn->alloca_bottom->offset);
}

// Generate code for a given node.
static int gen_expr_qbe(Node *node) {

  int tmp = current_tmp++;

  switch (node->kind) {
  case ND_NULL_EXPR:
    println("  %%%d =l copy 0", tmp);
    return tmp;
  case ND_NUM: {
    switch (node->ty->kind) {
    case TY_FLOAT: {
      union { float f32; uint32_t u32; } u = { node->fval };
      println("  %%%d =s copy %u # %.9g", tmp, u.u32, u.f32);
      return tmp;
    }
    case TY_DOUBLE: {
      union { double f64; uint64_t u64; } u = { node->fval };
      println("  %%%d =d copy %lu # %.17g", tmp, u.u64, u.f64);
      return tmp;
    }
    case TY_LDOUBLE: {
      union { double f64; uint64_t u64; } u = { node->fval };
      println("  %%%d =d copy %lu # %.17g reducing long double to double!!!", tmp, u.u64, u.f64);
      return tmp;
      /* union { long double f80; uint64_t u64[2]; } u; */
      /* memset(&u, 0, sizeof(u)); */
      /* u.f80 = node->fval; */
      /* println("#  mov $%lu, %%rax  # long double %Lf", u.u64[0], node->fval); */
      /* println("#  mov %%rax, -16(%%rsp)"); */
      /* println("#  mov $%lu, %%rax", u.u64[1]); */
      /* println("#  mov %%rax, -8(%%rsp)"); */
      /* println("#  fldt -16(%%rsp)"); */
      /* return tmp; */
    }
    }

    println("  %%%d =%c copy %ld", tmp, qbe_base_type(node->ty), node->val);
    return tmp;
  }
  case ND_NEG: {
    int val_tmp = gen_expr_qbe(node->lhs);
    print("  %%%d =%c neg %%%d", tmp, qbe_base_type(node->ty), val_tmp);
    return tmp;
  }
  case ND_VAR: {
    int addr_tmp = gen_addr(node);
    load_qbe(addr_tmp, tmp, node->ty);
    return tmp;
  }
  case ND_MEMBER: {
    int addr_tmp = gen_addr(node);
    load_qbe(addr_tmp, tmp, node->ty);

    Member *mem = node->member;
    if (mem->is_bitfield) {
      error_tok(node->tok, "bitfield not yet supported by QBE/RPJ");
      /* println("#  shl $%d, %%rax", 64 - mem->bit_width - mem->bit_offset); */
      /* if (mem->ty->is_unsigned) */
      /*   println("#  shr $%d, %%rax", 64 - mem->bit_width); */
      /* else */
      /*   println("#  sar $%d, %%rax", 64 - mem->bit_width); */
    }
    return tmp;
  }
  case ND_DEREF: {
    int addr_tmp = gen_expr_qbe(node->lhs);
    load_qbe(addr_tmp, tmp, node->ty);
    return tmp;
  }
  case ND_ADDR: {
    int addr_tmp = gen_addr(node->lhs);
    println("  %%%d =l copy %%%d", tmp, addr_tmp);
    return tmp;
  }
  case ND_ASSIGN: {
    int addr_tmp = gen_addr(node->lhs);
    int val_tmp = gen_expr_qbe(node->rhs);

    if (node->lhs->kind == ND_MEMBER && node->lhs->member->is_bitfield) {
      error_tok(node->tok, "bitfield not yet supported by QBE/RPJ");
      /* println("#  mov %%rax, %%r8"); */

      /* // If the lhs is a bitfield, we need to read the current value */
      /* // from memory and merge it with a new value. */
      /* Member *mem = node->lhs->member; */
      /* println("#  mov %%rax, %%rdi"); */
      /* println("#  and $%ld, %%rdi", (1L << mem->bit_width) - 1); */
      /* println("#  shl $%d, %%rdi", mem->bit_offset); */

      /* println("#  mov (%%rsp), %%rax"); */
      /* load(mem->ty); */

      /* long mask = ((1L << mem->bit_width) - 1) << mem->bit_offset; */
      /* println("#  mov $%ld, %%r9", ~mask); */
      /* println("#  and %%r9, %%rax"); */
      /* println("#  or %%rdi, %%rax"); */
      /* store(node->ty); */
      /* println("#  mov %%r8, %%rax"); */
      /* return tmp; */
    }

    store_qbe(val_tmp, addr_tmp, node->ty);
    println("  %%%d =%c copy %%%d", tmp, qbe_base_type(node->ty), val_tmp);
    return tmp;
  }
  case ND_STMT_EXPR:
    for (Node *n = node->body; n; n = n->next)
      gen_stmt_qbe(n);
    println("  %%%d =%c copy 0", tmp, qbe_base_type(node->ty));
    return tmp;
  case ND_COMMA: {
    gen_expr_qbe(node->lhs);
    int val_tmp = gen_expr_qbe(node->rhs);
    println("  %%%d =%c copy %%%d", tmp, qbe_base_type(node->ty), val_tmp);
    return tmp;
  }
  case ND_CAST: {
    int val_tmp = gen_expr_qbe(node->lhs);
    cast_qbe(val_tmp, node->lhs->ty, tmp, node->ty);
    return tmp;
  }
  case ND_MEMZERO:
    error_tok(node->tok, "memzero not yet supported by QBE/RPJ");
    /* // `rep stosb` is equivalent to `memset(%rdi, %al, %rcx)`. */
    /* println("#  mov $%d, %%rcx", node->var->ty->size); */
    /* println("#  lea %d(%%rbp), %%rdi", node->var->offset); */
    /* println("#  mov $0, %%al"); */
    /* println("#  rep stosb"); */
    return tmp;
  case ND_COND: {
    // TODO - struct/union copy? Maybe handled by the memcpy SNAFU already?
    int c = count();
    int cond_tmp = gen_expr_qbe(node->cond);
    //cmp_zero(node->cond->ty);
    println("  jnz %%%d @q.%d.then @q.%d.else", cond_tmp, c, c);
    println("@q.%d.then", c);
    int then_tmp = gen_expr_qbe(node->then);
    println("  %%%d =%c copy %%%d", tmp, qbe_base_type(node->ty), then_tmp);
    println("  jmp @q.%d.end", c);
    println("@q.%d.else", c);
    int else_tmp = gen_expr_qbe(node->els);
    println("  %%%d =%c copy %%%d", tmp, qbe_base_type(node->ty), else_tmp);
    println("@q.%d.end", c);
    return tmp;
  }
  case ND_NOT: {
    int val_tmp = gen_expr_qbe(node->lhs);
    //cmp_zero(node->lhs->ty);
    println("  %%%d =%c ceq %%%d", tmp, qbe_base_type(node->ty), val_tmp);
    return tmp;
  }
  case ND_BITNOT: {
    int val_tmp = gen_expr_qbe(node->lhs);
    println(" %%%d =%c xor %%%d, -1", tmp, qbe_base_type(node->ty), val_tmp);
    return tmp;
  }
  case ND_LOGAND: {
    int c = count();
    int lhs_tmp = gen_expr_qbe(node->lhs);
    println("  jnz @and.%d.false @q.%d.true", c, c);
    println("@and.%d.true", c);
    println("  %%%d =%c copy %%%d", tmp, qbe_base_type(node->ty), lhs_tmp);
    println("  jmp @and.%d.end", c);
    println("@and.%d.false", c);
    int rhs_tmp = gen_expr_qbe(node->rhs);
    println("  %%%d =%c copy %%%d", tmp, qbe_base_type(node->ty), rhs_tmp);
    println("@and.%d.end", c);
    return tmp;
  }
  case ND_LOGOR: {
    int c = count();
    int lhs_tmp = gen_expr_qbe(node->lhs);
    println("  jnz @and.%d.true @q.%d.false", c, c);
    println("@and.%d.true", c);
    println("  %%%d =%c copy %%%d", tmp, qbe_base_type(node->ty), lhs_tmp);
    println("  jmp @and.%d.end", c);
    println("@and.%d.false", c);
    int rhs_tmp = gen_expr_qbe(node->rhs);
    println("  %%%d =%c copy %%%d", tmp, qbe_base_type(node->ty), rhs_tmp);
    println("@and.%d.end", c);
    return tmp;
  }
  case ND_FUNCALL: {
    if (node->lhs->kind == ND_VAR && !strcmp(node->lhs->var->name, "alloca")) {
      error_tok(node->tok, "alloca builtin not yet supported by RPJ/QBE");
      /* gen_expr_qbe(node->args); */
      /* println("#  mov %%rax, %%rdi"); */
      /* builtin_alloca(); */
      return tmp;
    }

    // Generate a tmp for each arg
    for (Node *arg = node->args; arg; arg = arg->next) {
      arg->val_tmp = current_tmp++;
    }

    // Generate the args
    // TODO - is this the right order - I see the original code generates in the opposite order which will be inverse side-effecty
    // Apparently it's unspecified, so fine...
    for (Node *arg = node->args; arg; arg = arg->next) {
      arg->val_tmp = gen_expr_qbe(arg);
    }
    
    int fn_addr_tmp = gen_expr_qbe(node->lhs);

    if (node->ty->kind == TY_VOID) {
      // probably not necessary, but...
      println("  %%%d =l copy 0", tmp);
      print("  ");
    }
    else {
      print("  %%%d =", tmp);
      printparamtype(node->ty);
      print(" ");
    }

    // TODO remove fn_addr_tmp for most calls - ugh! Maybe QBE will do this for us?
    print("call %%%d(", fn_addr_tmp);
    
    for (Node *arg = node->args; arg; arg = arg->next) {
      printparamtype(arg->ty);
      print(" %%%d,", arg->val_tmp);
    }
    println(")");

    return tmp;
  }
  case ND_LABEL_VAL:
    error_tok(node->tok, "label values not yet supported by RPJ/QBE");
    /* println("#  lea %s(%%rip), %%rax", node->unique_label); */
    return tmp;
  case ND_CAS: {
    error_tok(node->tok, "CaS not yet supported by RPJ/QBE");
    /* gen_expr_qbe(node->cas_addr); */
    /* push(); */
    /* gen_expr_qbe(node->cas_new); */
    /* push(); */
    /* gen_expr_qbe(node->cas_old); */
    /* println("#  mov %%rax, %%r8"); */
    /* load(node->cas_old->ty->base); */
    /* pop("%rdx"); // new */
    /* pop("%rdi"); // addr */

    /* int sz = node->cas_addr->ty->base->size; */
    /* println("#  lock cmpxchg %s, (%%rdi)", reg_dx(sz)); */
    /* println("#  sete %%cl"); */
    /* println("#  je 1f"); */
    /* println("#  mov %s, (%%r8)", reg_ax(sz)); */
    /* println("#1:"); */
    /* println("#  movzbl %%cl, %%eax"); */
    return tmp;
  }
  case ND_EXCH: {
    error_tok(node->tok, "XChg not yet supported by RPJ/QBE");
    /* gen_expr_qbe(node->lhs); */
    /* push(); */
    /* gen_expr_qbe(node->rhs); */
    /* pop("%rdi"); */

    /* int sz = node->lhs->ty->base->size; */
    /* println("#  xchg %s, (%%rdi)", reg_ax(sz)); */
    return tmp;
  }
  }

  switch (node->lhs->ty->kind) {
  case TY_FLOAT:
  case TY_DOUBLE: {
    gen_expr_qbe(node->rhs);
    pushf();
    gen_expr_qbe(node->lhs);
    popf(1);

    char *sz = (node->lhs->ty->kind == TY_FLOAT) ? "ss" : "sd";

    switch (node->kind) {
    case ND_ADD:
      println("#  add%s %%xmm1, %%xmm0", sz);
      return tmp;
    case ND_SUB:
      println("#  sub%s %%xmm1, %%xmm0", sz);
      return tmp;
    case ND_MUL:
      println("#  mul%s %%xmm1, %%xmm0", sz);
      return tmp;
    case ND_DIV:
      println("#  div%s %%xmm1, %%xmm0", sz);
      return tmp;
    case ND_EQ:
    case ND_NE:
    case ND_LT:
    case ND_LE:
      println("#  ucomi%s %%xmm0, %%xmm1", sz);

      if (node->kind == ND_EQ) {
        println("#  sete %%al");
        println("#  setnp %%dl");
        println("#  and %%dl, %%al");
      } else if (node->kind == ND_NE) {
        println("#  setne %%al");
        println("#  setp %%dl");
        println("#  or %%dl, %%al");
      } else if (node->kind == ND_LT) {
        println("#  seta %%al");
      } else {
        println("#  setae %%al");
      }

      println("#  and $1, %%al");
      println("#  movzb %%al, %%rax");
      return tmp;
    }

    error_tok(node->tok, "invalid expression");
  }
  case TY_LDOUBLE: {
    gen_expr_qbe(node->lhs);
    gen_expr_qbe(node->rhs);

    switch (node->kind) {
    case ND_ADD:
      println("#  faddp");
      return tmp;
    case ND_SUB:
      println("#  fsubrp");
      return tmp;
    case ND_MUL:
      println("#  fmulp");
      return tmp;
    case ND_DIV:
      println("#  fdivrp");
      return tmp;
    case ND_EQ:
    case ND_NE:
    case ND_LT:
    case ND_LE:
      println("#  fcomip");
      println("#  fstp %%st(0)");

      if (node->kind == ND_EQ)
        println("#  sete %%al");
      else if (node->kind == ND_NE)
        println("#  setne %%al");
      else if (node->kind == ND_LT)
        println("#  seta %%al");
      else
        println("#  setae %%al");

      println("#  movzb %%al, %%rax");
      return tmp;
    }

    error_tok(node->tok, "invalid expression");
  }
  }

  gen_expr_qbe(node->rhs);
  push();
  gen_expr_qbe(node->lhs);
  pop("%rdi");

  char *ax, *di, *dx;

  if (node->lhs->ty->kind == TY_LONG || node->lhs->ty->base) {
    ax = "%rax";
    di = "%rdi";
    dx = "%rdx";
  } else {
    ax = "%eax";
    di = "%edi";
    dx = "%edx";
  }

  switch (node->kind) {
  case ND_ADD:
    println("#  add %s, %s", di, ax);
    return tmp;
  case ND_SUB:
    println("#  sub %s, %s", di, ax);
    return tmp;
  case ND_MUL:
    println("#  imul %s, %s", di, ax);
    return tmp;
  case ND_DIV:
  case ND_MOD:
    if (node->ty->is_unsigned) {
      println("#  mov $0, %s", dx);
      println("#  div %s", di);
    } else {
      if (node->lhs->ty->size == 8)
        println("#  cqo");
      else
        println("#  cdq");
      println("#  idiv %s", di);
    }

    if (node->kind == ND_MOD)
      println("#  mov %%rdx, %%rax");
    return tmp;
  case ND_BITAND:
    println("#  and %s, %s", di, ax);
    return tmp;
  case ND_BITOR:
    println("#  or %s, %s", di, ax);
    return tmp;
  case ND_BITXOR:
    println("#  xor %s, %s", di, ax);
    return tmp;
  case ND_EQ:
  case ND_NE:
  case ND_LT:
  case ND_LE:
    println("#  cmp %s, %s", di, ax);

    if (node->kind == ND_EQ) {
      println("#  sete %%al");
    } else if (node->kind == ND_NE) {
      println("#  setne %%al");
    } else if (node->kind == ND_LT) {
      if (node->lhs->ty->is_unsigned)
        println("#  setb %%al");
      else
        println("#  setl %%al");
    } else if (node->kind == ND_LE) {
      if (node->lhs->ty->is_unsigned)
        println("#  setbe %%al");
      else
        println("#  setle %%al");
    }

    println("#  movzb %%al, %%rax");
    return tmp;
  case ND_SHL:
    println("#  mov %%rdi, %%rcx");
    println("#  shl %%cl, %s", ax);
    return tmp;
  case ND_SHR:
    println("#  mov %%rdi, %%rcx");
    if (node->lhs->ty->is_unsigned)
      println("#  shr %%cl, %s", ax);
    else
      println("#  sar %%cl, %s", ax);
    return tmp;
  }

  error_tok(node->tok, "invalid expression");
}

static void gen_stmt_qbe(Node *node) {

  switch (node->kind) {
  case ND_IF: {
    int c = count();
    int cond_tmp = gen_expr_qbe(node->cond);
    println("  jnz %%%d @if.%d.then @if.%d.else", cond_tmp, c, c);
    println("@if.%d.then", c);
    gen_stmt_qbe(node->then);
    println("  jmp @if.%d.fi", c);
    println("@if.%d.else", c);
    if (node->els)
      gen_stmt_qbe(node->els);
    println("@if.%d.fi", c);
    return;
  }
  case ND_FOR: {
    int c = count();
    // for break/continue we probs want to set node->c - better than cont_label/break_label?
    if (node->init)
      gen_stmt_qbe(node->init);
    println("@for.%d.condition", c);
    if (node->cond) {
      int cond_tmp = gen_expr_qbe(node->cond);
      println("  jnz %%%d @for.%d.body @for.%d.break", cond_tmp, c, c);
    }
    println("@for.%d.body", c);
    gen_stmt_qbe(node->then);
    println("@for.%d.continue", c);
    if (node->inc)
      gen_expr_qbe(node->inc);
    println("  jmp @for.%d.condition", c);
    println("@for.%d.break", c);
    return;
  }
  case ND_DO: {
    int c = count();
    // for break/continue we probs want to set node->c - better than cont_label/break_label?
    println("@do.%d.body", c);
    gen_stmt_qbe(node->then);
    println("@do.%d.continue", c);
    int cond_tmp = gen_expr_qbe(node->cond);
    println("  jnz %%%d @do.%d.body @do.%d.break", cond_tmp, c, c);
    println("@do.%d.break", c);
    return;
  }
  case ND_SWITCH:
    error_tok(node->tok, "switch not yet supported by QBE/RPJ");
    gen_expr_qbe(node->cond);

    for (Node *n = node->case_next; n; n = n->case_next) {
      char *ax = (node->cond->ty->size == 8) ? "%rax" : "%eax";
      char *di = (node->cond->ty->size == 8) ? "%rdi" : "%edi";

      if (n->begin == n->end) {
        println("#  cmp $%ld, %s", n->begin, ax);
        println("#  je %s", n->label);
        continue;
      }

      // [GNU] Case ranges
      println("#  mov %s, %s", ax, di);
      println("#  sub $%ld, %s", n->begin, di);
      println("#  cmp $%ld, %s", n->end - n->begin, di);
      println("#  jbe %s", n->label);
    }

    if (node->default_case)
      println("#  jmp %s", node->default_case->label);

    println("#  jmp %s", node->brk_label);
    gen_stmt_qbe(node->then);
    println("#%s:", node->brk_label);
    return;
  case ND_CASE:
    error_tok(node->tok, "case not yet supported by QBE/RPJ");
    println("#%s:", node->label);
    gen_stmt_qbe(node->lhs);
    return;
  case ND_BLOCK:
    for (Node *n = node->body; n; n = n->next)
      gen_stmt_qbe(n);
    return;
  case ND_GOTO:
    println("  jmp @label.%s\n", node->unique_label);
    return;
  case ND_GOTO_EXPR:
    error_tok(node->tok, "computed goto not supported by QBE");
    return;
  case ND_LABEL:
    println("\n@label.%s", node->unique_label);
    gen_stmt_qbe(node->lhs);
    return;
  case ND_RETURN:
    if (node->lhs) {
      // TODO - will struct/union by val work? Actually maybe... we're just copying the pointer from another var; it's copying structs/unions by val that will be tricky
      int val_tmp = gen_expr_qbe(node->lhs);
      println("  ret %%%d", val_tmp);
      return;
    }

    println("  ret");
    return;
  case ND_EXPR_STMT:
    gen_expr_qbe(node->lhs);
    return;
  case ND_ASM:
    error_tok(node->tok, "inline assembly not supported by QBE");
    println("#  %s", node->asm_str);
    return;
  }

  error_tok(node->tok, "invalid statement");
}

// Assign offsets to local variables.
static void assign_lvar_offsets(Obj *prog) {
  for (Obj *fn = prog; fn; fn = fn->next) {
    if (!fn->is_function)
      continue;

    // If a function has many parameters, some parameters are
    // inevitably passed by stack rather than by register.
    // The first passed-by-stack parameter resides at RBP+16.
    int top = 16;
    int bottom = 0;

    int gp = 0, fp = 0;

    // Assign offsets to pass-by-stack parameters.
    for (Obj *var = fn->params; var; var = var->next) {
      Type *ty = var->ty;

      switch (ty->kind) {
      case TY_STRUCT:
      case TY_UNION:
        if (ty->size <= 16) {
          bool fp1 = has_flonum(ty, 0, 8, 0);
          bool fp2 = has_flonum(ty, 8, 16, 8);
          if (fp + fp1 + fp2 < FP_MAX && gp + !fp1 + !fp2 < GP_MAX) {
            fp = fp + fp1 + fp2;
            gp = gp + !fp1 + !fp2;
            continue;
          }
        }
        break;
      case TY_FLOAT:
      case TY_DOUBLE:
        if (fp++ < FP_MAX)
          continue;
        break;
      case TY_LDOUBLE:
        break;
      default:
        if (gp++ < GP_MAX)
          continue;
      }

      top = align_to(top, 8);
      var->offset = top;
      top += var->ty->size;
    }

    // Assign offsets to pass-by-register parameters and local variables.
    for (Obj *var = fn->locals; var; var = var->next) {
      if (var->offset)
        continue;

      // AMD64 System V ABI has a special alignment rule for an array of
      // length at least 16 bytes. We need to align such array to at least
      // 16-byte boundaries. See p.14 of
      // https://github.com/hjl-tools/x86-psABI/wiki/x86-64-psABI-draft.pdf.
      int align = (var->ty->kind == TY_ARRAY && var->ty->size >= 16)
        ? MAX(16, var->align) : var->align;

      bottom += var->ty->size;
      bottom = align_to(bottom, align);
      var->offset = -bottom;
    }

    fn->stack_size = align_to(bottom, 16);
  }
}

static void emit_type_qbe(Type* ty);

static void emit_member_types_qbe(Member* members) {
  for(Member* member = members; member; member = member->next) {
    emit_type_qbe(member->ty);
  }
}

static void emit_struct_members_qbe(Member* members) {
  for(Member* member = members; member; member = member->next) {
    print(" ");
    printstructmembertype(member->ty);
    print(",");
  }
}

static void emit_struct_type_qbe(Type* ty) {
  emit_member_types_qbe(ty->members);

  print("type :");
  printstructident(ty);
  print(" = align %d {", ty->align);
  emit_struct_members_qbe(ty->members);
  println(" }%s\n", (ty->is_packed ? " # TODO packed" :""));
}

static void emit_union_type_qbe(Type* ty) {
  emit_member_types_qbe(ty->members);

  // We treat unions as opaque types
  print("type :");
  printstructident(ty);
  println(" = align %d { %d }\n", ty->align, ty->size);
}

static void emit_type_qbe(Type* ty) {
  if (ty->emitted) {
    return;
  }
  
  if (ty->kind == TY_STRUCT) {
    emit_struct_type_qbe(ty);
  }
  else if(ty->kind == TY_UNION) {
    emit_union_type_qbe(ty);
  }

  ty->emitted = true;
}

static void emit_function_types_qbe(Type* ty) {
  emit_type_qbe(ty->return_ty);

  for(Type* param_ty = ty->params; param_ty; param_ty = param_ty->next) {
    emit_type_qbe(param_ty);
  }
}

static void emit_fn_call_types_in_stmt_qbe(Node* node);
static void emit_fn_call_types_in_expr_qbe(Node* node);

static void emit_fn_call_types_in_addr_qbe(Node* node) {
  switch (node->kind) {
  case ND_VAR:
    return;
  case ND_DEREF:
    emit_fn_call_types_in_expr_qbe(node->lhs);
    return;
  case ND_COMMA:
    emit_fn_call_types_in_expr_qbe(node->lhs);
    emit_fn_call_types_in_addr_qbe(node->rhs);
    return;
  case ND_MEMBER:
    emit_fn_call_types_in_addr_qbe(node->lhs);
    return;
  case ND_FUNCALL:
    if (node->ret_buffer) {
      emit_fn_call_types_in_expr_qbe(node);
      return;
    }
    break;
  case ND_ASSIGN:
  case ND_COND:
    if (node->ty->kind == TY_STRUCT || node->ty->kind == TY_UNION) {
      emit_fn_call_types_in_expr_qbe(node);
      return;
    }
    break;
  case ND_VLA_PTR:
    return;
  }

  error_tok(node->tok, "not an lvalue");
}

static void emit_fn_call_types_in_expr_qbe(Node* node) {
  switch (node->kind) {
  case ND_NULL_EXPR:
    return;
  case ND_NUM:
    return;
  case ND_NEG:
    emit_fn_call_types_in_expr_qbe(node->lhs);
    return;
  case ND_VAR:
    emit_fn_call_types_in_addr_qbe(node);
    return;
  case ND_MEMBER:
    emit_fn_call_types_in_addr_qbe(node);
    return;
  case ND_DEREF:
    emit_fn_call_types_in_expr_qbe(node->lhs);
    load(node->ty);
    return;
  case ND_ADDR:
    emit_fn_call_types_in_addr_qbe(node->lhs);
    return;
  case ND_ASSIGN:
    emit_fn_call_types_in_addr_qbe(node->lhs);
    emit_fn_call_types_in_expr_qbe(node->rhs);
    return;
  case ND_STMT_EXPR:
    for (Node *n = node->body; n; n = n->next)
      emit_fn_call_types_in_stmt_qbe(n);
    return;
  case ND_COMMA:
    emit_fn_call_types_in_expr_qbe(node->lhs);
    emit_fn_call_types_in_expr_qbe(node->rhs);
    return;
  case ND_CAST:
    emit_fn_call_types_in_expr_qbe(node->lhs);
    return;
  case ND_MEMZERO:
    return;
  case ND_COND:
    emit_fn_call_types_in_expr_qbe(node->cond);
    emit_fn_call_types_in_expr_qbe(node->then);
    emit_fn_call_types_in_expr_qbe(node->els);
    return;
  case ND_NOT:
    emit_fn_call_types_in_expr_qbe(node->lhs);
    return;
  case ND_BITNOT:
    emit_fn_call_types_in_expr_qbe(node->lhs);
    return;
  case ND_LOGAND:
    emit_fn_call_types_in_expr_qbe(node->lhs);
    emit_fn_call_types_in_expr_qbe(node->rhs);
    return;
  case ND_LOGOR:
    emit_fn_call_types_in_expr_qbe(node->lhs);
    emit_fn_call_types_in_expr_qbe(node->rhs);
    return;
  case ND_FUNCALL: {
    for (Node* arg = node->args; arg; arg = arg->next)
      emit_fn_call_types_in_expr_qbe(arg);
    emit_fn_call_types_in_expr_qbe(node->lhs);

    // return type
    emit_type_qbe(node->ty);

    // param types
    for (Node *arg = node->args; arg; arg = arg->next) {
      Type *ty = arg->ty;

      emit_type_qbe(ty);
    }

    return;
  }
  case ND_LABEL_VAL:
    return;
  case ND_CAS:
    emit_fn_call_types_in_expr_qbe(node->cas_addr);
    emit_fn_call_types_in_expr_qbe(node->cas_new);
    emit_fn_call_types_in_expr_qbe(node->cas_old);
    return;
  case ND_EXCH:
    emit_fn_call_types_in_expr_qbe(node->lhs);
    emit_fn_call_types_in_expr_qbe(node->rhs);
    return;
  }

  switch (node->lhs->ty->kind) {
  case TY_FLOAT:
  case TY_DOUBLE: {
    emit_fn_call_types_in_expr_qbe(node->rhs);
    emit_fn_call_types_in_expr_qbe(node->lhs);

    switch (node->kind) {
    case ND_ADD:
      return;
    case ND_SUB:
      return;
    case ND_MUL:
      return;
    case ND_DIV:
      return;
    case ND_EQ:
    case ND_NE:
    case ND_LT:
    case ND_LE:
      return;
    }

    error_tok(node->tok, "invalid expression");
  }
  case TY_LDOUBLE: {
    emit_fn_call_types_in_expr_qbe(node->lhs);
    emit_fn_call_types_in_expr_qbe(node->rhs);

    switch (node->kind) {
    case ND_ADD:
      return;
    case ND_SUB:
      return;
    case ND_MUL:
      return;
    case ND_DIV:
      return;
    case ND_EQ:
    case ND_NE:
    case ND_LT:
    case ND_LE:
      return;
    }

    error_tok(node->tok, "invalid expression");
  }
  }

  emit_fn_call_types_in_expr_qbe(node->rhs);
  emit_fn_call_types_in_expr_qbe(node->lhs);

  switch (node->kind) {
  case ND_ADD:
    return;
  case ND_SUB:
    return;
  case ND_MUL:
    return;
  case ND_DIV:
  case ND_MOD:
    return;
  case ND_BITAND:
    return;
  case ND_BITOR:
    return;
  case ND_BITXOR:
    return;
  case ND_EQ:
  case ND_NE:
  case ND_LT:
  case ND_LE:
    return;
  case ND_SHL:
    return;
  case ND_SHR:
    return;

  error_tok(node->tok, "invalid expression");
}
}

static void emit_fn_call_types_in_stmt_qbe(Node* node) {
  switch (node->kind) {
  case ND_IF: {
    emit_fn_call_types_in_expr_qbe(node->cond);
    emit_fn_call_types_in_stmt_qbe(node->then);
    if (node->els)
      emit_fn_call_types_in_stmt_qbe(node->els);
    return;
  }
  case ND_FOR: {
    if (node->init)
      emit_fn_call_types_in_stmt_qbe(node->init);
    if (node->cond) {
      emit_fn_call_types_in_expr_qbe(node->cond);
    }
    emit_fn_call_types_in_stmt_qbe(node->then);
    if (node->inc)
      emit_fn_call_types_in_expr_qbe(node->inc);
    return;
  }
  case ND_DO: {
    emit_fn_call_types_in_stmt_qbe(node->then);
    emit_fn_call_types_in_expr_qbe(node->cond);
    return;
  }
  case ND_SWITCH:
    emit_fn_call_types_in_expr_qbe(node->cond);
    emit_fn_call_types_in_stmt_qbe(node->then);
    return;
  case ND_CASE:
    emit_fn_call_types_in_stmt_qbe(node->lhs);
    return;
  case ND_BLOCK:
    for (Node *n = node->body; n; n = n->next)
      emit_fn_call_types_in_stmt_qbe(n);
    return;
  case ND_GOTO:
    return;
  case ND_GOTO_EXPR:
    emit_fn_call_types_in_expr_qbe(node->lhs);
    return;
  case ND_LABEL:
    emit_fn_call_types_in_stmt_qbe(node->lhs);
    return;
  case ND_RETURN:
    if (node->lhs)
      emit_fn_call_types_in_expr_qbe(node->lhs);
    return;
  case ND_EXPR_STMT:
    emit_fn_call_types_in_expr_qbe(node->lhs);
    return;
  case ND_ASM:
    return;
  }

  error_tok(node->tok, "invalid statement");
}

static void emit_fn_call_types_qbe(Obj* fn) {
  for (Node *n = fn->body; n; n = n->next)
    emit_fn_call_types_in_stmt_qbe(n);
}

// TODO - proper union defs { {} } etc.
static void emit_types_qbe(Obj *prog) {
  for (Obj *obj = prog; obj; obj = obj->next) {
    if (obj->is_function) {
      // For function definition ABI
      emit_function_types_qbe(obj->ty);
      // For function call ABI
      emit_fn_call_types_qbe(obj);
      continue;
    }

    emit_type_qbe(obj->ty);
  }
}

static void emit_data_qbe(Obj *prog) {
  for (Obj *var = prog; var; var = var->next) {
    if (var->is_function || !var->is_definition)
      continue;

    if (!var->is_static) {
      println("export");
    }

    int align = (var->ty->kind == TY_ARRAY && var->ty->size >= 16)
      ? MAX(16, var->align) : var->align;

    // Common symbol
    if (opt_fcommon && var->is_tentative) {
      println("section \".comm\"");
      println("data $%s = align %d { z %d }\n", var->name, align, var->ty->size);
      continue;
    }

    // .data or .tdata
    if (var->init_data) {
      if (var->is_tls) {
        println("section \".tdata\" \"\\\"awT\\\",@progbits\"");
      }
      else {
	println("section \".data\"");
      }

      print("data $%s = { ", var->name);
      
      Relocation *rel = var->rel;
      int pos = 0;
      while (pos < var->ty->size) {
        if (rel && rel->offset == pos) {
	  //println("  .type %s, @object", var->name); ???
	  print("l $%s", *rel->label);
	  if (rel->addend) {
	    // TODO - is +N valid QBE?
	    print("%+ld, ", rel->addend);
	  }
	  print(", ");
          rel = rel->next;
          pos += 8;
        } else {
	  print("b %d, ", var->init_data[pos++]);
        }
      }
      println("}\n");

      continue;
    }

    // RPJ - never get here?
    
    // .bss or .tbss
    if (var->is_tls) {
      println("section \".tbss\" \"\\\"awT\\\",@nobits\"");
    }
    else {
      println("section \".bss\"");
    }

    println("data $%s = { z %d }\n", var->name, var->ty->size);
  }
}

static void emit_text_qbe(Obj *prog) {
  for (Obj *fn = prog; fn; fn = fn->next) {
    if (!fn->is_function || !fn->is_definition)
      continue;

    // No code is emitted for "static inline" functions
    // if no one is referencing them.
    if (!fn->is_live)
      continue;

    if (!fn->is_static)
      println("export");

    println("section \".text\"");
    print("function ");
    
    if (fn->ty->return_ty != TY_VOID) {
      printparamtype(fn->ty->return_ty);
      print(" ");
    }
    
    print("$%s (", fn->name);
    for (Obj *var = fn->params; var; var = var->next) {
      printparamtype(var->ty);
      print(" %%");
      printlocalname(var);
      if (var->next)
	print(", ");
    }
    if (fn->va_area) {
      print("%s...", (fn->params ? ", " : ""));
    }
    println(") {");

    println("@start\n");
    
    current_fn = fn;
    current_tmp = 1;

    // TODO
    //printf("RPJ params:\n");
    for (Obj *var = fn->params; var; var = var->next) {
      var->is_param = true;
      //printf("         rpj param     0x%lx '%s' offset %d is_local %s is_param %s align %d size %d\n", (long)var, var->name, var->offset, (var->is_local ? "true" : "false"), (var->is_param ? "true" : "false"), var->align, var->ty->size);
    }
    //printf("RPJ locals:\n");
    for (Obj *var = fn->locals; var; var = var->next) {
      if (var->is_param)
	continue;

      int align = MAX(4, var->align);
      print("  %%");
      printlocalname(var);
      println(" =l alloc%d %d", align, var->ty->size);
      //printf("         rpj local var 0x%lx '%s' offset %d is_local %s is_param %s align %d size %d\n", (long)var, var->name, var->offset, (var->is_local ? "true" : "false"), (var->is_param ? "true" : "false"), var->align, var->ty->size);
    }

    print("\n");
    
    // Emit code
    gen_stmt_qbe(fn->body);
    assert(depth == 0);

    // [https://www.sigbus.info/n1570#5.1.2.2.3p1] The C spec defines
    // a special rule for the main function. Reaching the end of the
    // main function is equivalent to returning 0, even though the
    // behavior is undefined for the other functions.
    if (strcmp(fn->name, "main") == 0)
      // TODO
      println("#  mov $0, %%rax");

    // Epilogue
    println("}\n");
    //println("#.L.return.%s:", fn->name);
  }
}

void codegen_qbe(Obj *prog, FILE *out) {
  output_file = out;

  File **files = get_input_files();
  for (int i = 0; files[i]; i++)
    println("#![qbe]  .file %d \"%s\"", files[i]->file_no, files[i]->name);
  print("\n");

  // Used to disambiguate homonymous local vars
  assign_lvar_offsets(prog);

  // Struct and union types used for fn ABI wrangling
  emit_types_qbe(prog);
  emit_data_qbe(prog);
  emit_text_qbe(prog);
}
