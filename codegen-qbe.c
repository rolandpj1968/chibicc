#include "chibicc.h"

#define GP_MAX 6
#define FP_MAX 8

static FILE *output_file;
static int depth;
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

// Compute the absolute address of a given node.
// It's an error if a given node does not reside in memory.
// Return the temporary that the address resides in.
static int gen_addr_qbe(Node *node) {
  int tmp = current_tmp++;

  switch (node->kind) {
  case ND_VAR:
    // Local variable
    if (node->var->is_local) {
      print("  %%.%d =l copy %%", tmp);
      printlocalname(node->var);
      print("\n");
      return tmp;
    }

    // Thread-local variable
    if (node->var->is_tls) {
      error_tok(node->tok, "thread-local variables not yet supported by RPJ/QBE");
      /* println("#  mov %%fs:0, %%rax"); */
      /* println("#  add $%s@tpoff, %%rax", node->var->name); */
      return tmp;
    }

    // Global var/fn
    println("  %%.%d =l copy $%s", tmp, node->var->name);
    return tmp;
  case ND_DEREF:
    return gen_expr_qbe(node->lhs);
  case ND_COMMA:
    gen_expr_qbe(node->lhs); // value unused
    return gen_addr_qbe(node->rhs);
  case ND_MEMBER: {
    int base_addr_tmp = gen_addr_qbe(node->lhs);
    println("  %%.%d =l add %%.%d, %d", tmp, base_addr_tmp, node->member->offset);
    return tmp;
  }
  case ND_FUNCALL:
    if (node->ret_buffer) {
      return gen_expr_qbe(node);
    }
    break;
  case ND_ASSIGN:
  case ND_COND:
    // TODO - this seems dodgy - what are we doing here?
    if (node->ty->kind == TY_STRUCT || node->ty->kind == TY_UNION) {
      return gen_expr_qbe(node);
    }
    // TODO - nothing!!! ???
    break;
  case ND_VLA_PTR:
    // TODO - not sure about this
    error_tok(node->tok, "ND_VLA_PTR not yet supported by RPJ/QBE");
    /* println("#  lea %d(%%rbp), %%rax", node->var->offset); */
    return tmp;
  }

  error_tok(node->tok, "not an lvalue");
}

// return temporary containing the loaded value
static int load_qbe(int from_tmp, Type *ty) {
  int to_tmp = current_tmp++;
  char sign = ty->is_unsigned ? 'u' : 's';

  switch (ty->kind) {
  case TY_BOOL:
    // TODO check this - byte?
    println("  %%.%d =l loadub %%.%d", to_tmp, from_tmp);
    return to_tmp;
  case TY_CHAR:
    println("  %%.%d =l load%cb %%.%d", to_tmp, sign, from_tmp);
    return to_tmp;
  case TY_SHORT:
    println("  %%.%d =l load%ch %%.%d", to_tmp, sign, from_tmp);
    return to_tmp;
  case TY_INT:
    println("  %%.%d =l load%cw %%.%d", to_tmp, sign, from_tmp);
    return to_tmp;
  case TY_LONG:
  case TY_PTR:
    println("  %%.%d =l loadd %%.%d", to_tmp, from_tmp);
    return to_tmp;
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
    return from_tmp;
  case TY_FLOAT:
    println("  %%.%d =l loads %%.%d", to_tmp, from_tmp);
    return to_tmp;
  case TY_DOUBLE:
    println("  %%.%d =l loadd %%.%d", to_tmp, from_tmp);
    return to_tmp;
  case TY_LDOUBLE:
    println("  %%.%d =l loadd %%.%d", to_tmp, from_tmp);
    return to_tmp;
  }

  error("BUG: unhandled type %d in load_qbe() in RPJ/QBE", ty->kind);

}

static void store_qbe(int val_tmp, int addr_tmp, Type *ty) {
  println("  store%c %%.%d, %%.%d", qbe_ext_type(ty), val_tmp, addr_tmp);
}

// return tmp with the cast value
static int cast_qbe(int from_tmp, Type *from, Type *to) {
  int to_tmp = current_tmp++;
  
  char from_base_type = qbe_base_type(from);
  char to_base_type = qbe_base_type(to);
  
  if (to->kind == TY_VOID) {
    // Is this necessary?
    println("  %%.%d =l copy 0", to_tmp);
    return to_tmp;
  }

  if (to->kind == from->kind) {
    return from_tmp;
  }

  if (to->kind == TY_BOOL) {
    println("  %%.%d =%c cne%c %%.%d, 0", to_tmp, from_base_type, to_base_type, from_tmp);
    return to_tmp;
  }

  // Integer to integer casts
  if (is_integer(from) && is_integer(to)) {

    if (from->is_unsigned && from->size <= to->size) {
      println("  %%.%d =%c copy %%.%d", to_tmp, to_base_type, from_tmp);
      return to_tmp;
    }
    
    if (from->size < to->size) {
      // We sign/zero extend the 'from' type to the 'to' type
      // TODO - check the C spec here (gcc behaves oddly in some scenarios like casting u16 to i16
      char to_sign = to->is_unsigned ? 'u' : 's';
      println("  %%.%d =%c ext%c%c %%.%d", to_tmp, to_base_type, to_sign, qbe_ext_type(from), from_tmp);
      return to_tmp;
    }

    // from->size >= to->size
    // We mask the "from" value to the "to" value size
    long mask = ((long)1 << (to->size * 8)) - 1;
    println("  %%.%d =%c and %%.%d, %ld # 0x%lx", to_tmp, to_base_type, from_tmp, mask, mask);
    return to_tmp;
  }

  // Numeric casts where at least one of from or to is floating point
  if (is_numeric(from) && is_numeric(to)) {
    // Float to int or vice-versa
    char *from_sign = is_integer(from) ? (from->is_unsigned ? "u" : "s") : "";
    char *to_sign = is_integer(to) ? (to->is_unsigned ? "u" : "s") : "";
    println("  %%.%d =%c %s%cto%s%c %%.%d", to_tmp, to_base_type, from_sign, from_base_type, to_sign, to_base_type, from_tmp);
    return to_tmp;
  }

  // Pointer to long and vice versa - NOP
  if (from_base_type == 'l' && to_base_type == 'l') {
    return from_tmp;
  }

  error("BUG: unhandled types %d to %d in cast_qbe() in RPJ/QBE", from->kind, to->kind);
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

// TODO...
/* static void builtin_alloca(void) { */
/*   error_tok(node->tok, "alloca builtin not yet supported by RPJ/QBE"); */
/*   // Align size to 16 bytes. */
/*   println("#  add $15, %%rdi"); */
/*   println("#  and $0xfffffff0, %%edi"); */

/*   // Shift the temporary area by %rdi. */
/*   println("#  mov %d(%%rbp), %%rcx", current_fn->alloca_bottom->offset); */
/*   println("#  sub %%rsp, %%rcx"); */
/*   println("#  mov %%rsp, %%rax"); */
/*   println("#  sub %%rdi, %%rsp"); */
/*   println("#  mov %%rsp, %%rdx"); */
/*   println("#1:"); */
/*   println("#  cmp $0, %%rcx"); */
/*   println("#  je 2f"); */
/*   println("#  mov (%%rax), %%r8b"); */
/*   println("#  mov %%r8b, (%%rdx)"); */
/*   println("#  inc %%rdx"); */
/*   println("#  inc %%rax"); */
/*   println("#  dec %%rcx"); */
/*   println("#  jmp 1b"); */
/*   println("#2:"); */

/*   // Move alloca_bottom pointer. */
/*   println("#  mov %d(%%rbp), %%rax", current_fn->alloca_bottom->offset); */
/*   println("#  sub %%rdi, %%rax"); */
/*   println("#  mov %%rax, %d(%%rbp)", current_fn->alloca_bottom->offset); */
/* } */

// Generate code for a given node.
static int gen_expr_qbe(Node *node) {
  int tmp = current_tmp++;

  switch (node->kind) {
  case ND_NULL_EXPR:
    println("  %%.%d =l copy 0", tmp);
    return tmp;
  case ND_NUM: {
    switch (node->ty->kind) {
    case TY_FLOAT: {
      union { float f32; uint32_t u32; } u = { node->fval };
      println("  %%.%d =s copy %u # %.9g", tmp, u.u32, u.f32);
      return tmp;
    }
    case TY_DOUBLE: {
      union { double f64; uint64_t u64; } u = { node->fval };
      println("  %%.%d =d copy %lu # %.17g", tmp, u.u64, u.f64);
      return tmp;
    }
    case TY_LDOUBLE: {
      union { double f64; uint64_t u64; } u = { node->fval };
      println("  %%.%d =d copy %lu # %.17g reducing long double to double!!!", tmp, u.u64, u.f64);
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

    println("  %%.%d =%c copy %ld", tmp, qbe_base_type(node->ty), node->val);
    return tmp;
  }
  case ND_NEG: {
    int val_tmp = gen_expr_qbe(node->lhs);
    print("  %%.%d =%c neg %%.%d", tmp, qbe_base_type(node->ty), val_tmp);
    return tmp;
  }
  case ND_VAR: {
    int addr_tmp = gen_addr_qbe(node);
    return load_qbe(addr_tmp, node->ty);
  }
  case ND_MEMBER: {
    int addr_tmp = gen_addr_qbe(node);
    tmp = load_qbe(addr_tmp, node->ty);

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
    return load_qbe(addr_tmp, node->ty);
  }
  case ND_ADDR:
    return gen_addr_qbe(node->lhs);
  case ND_ASSIGN: {
    int addr_tmp = gen_addr_qbe(node->lhs);
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
    return val_tmp;
  }
  case ND_STMT_EXPR:
    for (Node *n = node->body; n; n = n->next)
      gen_stmt_qbe(n);
    // TODO ???
    println("  %%.%d =%c copy 0", tmp, qbe_base_type(node->ty));
    return tmp;
  case ND_COMMA:
    gen_expr_qbe(node->lhs);
    return gen_expr_qbe(node->rhs);
  case ND_CAST: {
    int val_tmp = gen_expr_qbe(node->lhs);
    return cast_qbe(val_tmp, node->lhs->ty, node->ty);
  }
  case ND_MEMZERO: {
    // TODO also things like small arrays
    if (node->var->ty->size > 8) {
      error_tok(node->tok, "memzero not yet properly supported by QBE/RPJ");
    }
    print("  store%c 0, %%", qbe_ext_type(node->var->ty));
    printlocalname(node->var);
    print("\n");
    println("  %%.%d =l copy 0", tmp);
    return tmp;
  }
  case ND_COND: {
    // TODO - struct/union copy? Maybe handled by the memcpy SNAFU already?
    int c = count();
    int cond_tmp = gen_expr_qbe(node->cond);
    println("  jnz %%.%d, @q.%d.then, @q.%d.else", cond_tmp, c, c);
    println("@q.%d.then", c);
    int then_tmp = gen_expr_qbe(node->then);
    println("  %%.%d =%c copy %%.%d", tmp, qbe_base_type(node->ty), then_tmp);
    println("  jmp @q.%d.end", c);
    println("@q.%d.else", c);
    int else_tmp = gen_expr_qbe(node->els);
    println("  %%.%d =%c copy %%.%d", tmp, qbe_base_type(node->ty), else_tmp);
    println("@q.%d.end", c);
    return tmp;
  }
  case ND_NOT: {
    int val_tmp = gen_expr_qbe(node->lhs);
    println("  %%.%d =%c ceq %%.%d", tmp, qbe_base_type(node->ty), val_tmp);
    return tmp;
  }
  case ND_BITNOT: {
    int val_tmp = gen_expr_qbe(node->lhs);
    println(" %%.%d =%c xor %%.%d, -1", tmp, qbe_base_type(node->ty), val_tmp);
    return tmp;
  }
  case ND_LOGAND: {
    int c = count();
    int lhs_tmp = gen_expr_qbe(node->lhs);
    println("  jnz %%%d, @and.%d.false, @q.%d.true", lhs_tmp, c, c);
    println("@and.%d.true", c);
    println("  %%.%d =%c copy %%.%d", tmp, qbe_base_type(node->ty), lhs_tmp);
    println("  jmp @and.%d.end", c);
    println("@and.%d.false", c);
    int rhs_tmp = gen_expr_qbe(node->rhs);
    println("  %%.%d =%c copy %%.%d", tmp, qbe_base_type(node->ty), rhs_tmp);
    println("@and.%d.end", c);
    return tmp;
  }
  case ND_LOGOR: {
    int c = count();
    int lhs_tmp = gen_expr_qbe(node->lhs);
    println("  jnz %%%d, @and.%d.true, @q.%d.false", lhs_tmp, c, c);
    println("@and.%d.true", c);
    println("  %%.%d =%c copy %%.%d", tmp, qbe_base_type(node->ty), lhs_tmp);
    println("  jmp @and.%d.end", c);
    println("@and.%d.false", c);
    int rhs_tmp = gen_expr_qbe(node->rhs);
    println("  %%.%d =%c copy %%.%d", tmp, qbe_base_type(node->ty), rhs_tmp);
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

    // Avoid creating a temporary for a direct function call - is_tls is probably unnecessary
    bool is_direct = node->lhs->kind == ND_VAR && !(node->lhs->var->is_local) && !(node->lhs->var->is_tls);
    int fn_addr_tmp = 0;
    if (!is_direct)
      fn_addr_tmp = gen_expr_qbe(node->lhs);

    if (node->ty->kind == TY_VOID) {
      print("  ");
    }
    else {
      print("  %%.%d =", tmp);
      printparamtype(node->ty);
      print(" ");
    }

    // TODO remove fn_addr_tmp for most calls - ugh! Maybe QBE will do this for us?
    print("call ");
    if (is_direct)
      print("$%s", node->lhs->var->name);
    else 
      print("%%.%d", fn_addr_tmp);

    print("(");
    
    for (Node *arg = node->args; arg; arg = arg->next) {
      printparamtype(arg->ty);
      print(" %%.%d,", arg->val_tmp);
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
    error_tok(node->tok, "xchg not yet supported by RPJ/QBE");
    /* gen_expr_qbe(node->lhs); */
    /* push(); */
    /* gen_expr_qbe(node->rhs); */
    /* pop("%rdi"); */

    /* int sz = node->lhs->ty->base->size; */
    /* println("#  xchg %s, (%%rdi)", reg_ax(sz)); */
    return tmp;
  }
  }

  char base_type = qbe_base_type(node->ty);
  char lhs_base_type = qbe_base_type(node->lhs->ty);

  switch (node->lhs->ty->kind) {
  case TY_FLOAT:
  case TY_DOUBLE:
  case TY_LDOUBLE: {
    int lhs_tmp = gen_expr_qbe(node->lhs);
    int rhs_tmp = gen_expr_qbe(node->rhs);

    switch (node->kind) {
    case ND_ADD:
      println("  %%.%d =%c add %%.%d, %%.%d", tmp, base_type, lhs_tmp, rhs_tmp);
      return tmp;
    case ND_SUB:
      println("  %%.%d =%c sub %%.%d, %%.%d", tmp, base_type, lhs_tmp, rhs_tmp);
      return tmp;
    case ND_MUL:
      println("  %%.%d =%c mul %%.%d, %%.%d", tmp, base_type, lhs_tmp, rhs_tmp);
      return tmp;
    case ND_DIV:
      println("  %%.%d =%c div %%.%d, %%.%d", tmp, base_type, lhs_tmp, rhs_tmp);
      return tmp;
    case ND_EQ:
      println("  %%.%d =%c ceq%c %%.%d, %%.%d", tmp, base_type, lhs_base_type, lhs_tmp, rhs_tmp);
      return tmp;
    case ND_NE:
      println("  %%.%d =%c cne%c %%.%d, %%.%d", tmp, base_type, lhs_base_type, lhs_tmp, rhs_tmp);
      return tmp;
    case ND_LT:
      println("  %%.%d =%c clt%c %%.%d, %%.%d", tmp, base_type, lhs_base_type, lhs_tmp, rhs_tmp);
      return tmp;
    case ND_LE:
      println("  %%.%d =%c cle%c %%.%d, %%.%d", tmp, base_type, lhs_base_type, lhs_tmp, rhs_tmp);
      return tmp;
    }

    error_tok(node->tok, "invalid expression");
  }
  }

  int lhs_tmp = gen_expr_qbe(node->lhs);
  int rhs_tmp = gen_expr_qbe(node->rhs);

  char sign = node->lhs->ty->is_unsigned ? 'u' : 's';

  switch (node->kind) {
  case ND_ADD:
    println("  %%.%d =%c add %%.%d, %%.%d", tmp, base_type, lhs_tmp, rhs_tmp);
    return tmp;
  case ND_SUB:
    println("  %%.%d =%c sub %%.%d, %%.%d", tmp, base_type, lhs_tmp, rhs_tmp);
    return tmp;
  case ND_MUL:
    println("  %%.%d =%c mul %%.%d, %%.%d", tmp, base_type, lhs_tmp, rhs_tmp);
    return tmp;
  case ND_DIV:
    println("  %%.%d =%c %s %%.%d, %%.%d", tmp, base_type, (node->ty->is_unsigned ? "udiv" : "div"), lhs_tmp, rhs_tmp);
    return tmp;
  case ND_MOD:
    println("  %%.%d =%c %s %%.%d, %%.%d", tmp, base_type, (node->ty->is_unsigned ? "urem" : "rem"), lhs_tmp, rhs_tmp);
    return tmp;
  case ND_BITAND:
    println("  %%.%d =%c and %%.%d, %%.%d", tmp, base_type, lhs_tmp, rhs_tmp);
    return tmp;
  case ND_BITOR:
    println("  %%.%d =%c or %%.%d, %%.%d", tmp, base_type, lhs_tmp, rhs_tmp);
    return tmp;
  case ND_BITXOR:
    println("  %%.%d =%c xor %%.%d, %%.%d", tmp, base_type, lhs_tmp, rhs_tmp);
    return tmp;
  case ND_EQ:
    println("  %%.%d =%c ceq%c %%.%d, %%.%d", tmp, base_type, lhs_base_type, lhs_tmp, rhs_tmp);
    return tmp;
  case ND_NE:
    println("  %%.%d =%c cne%c %%.%d, %%.%d", tmp, base_type, lhs_base_type, lhs_tmp, rhs_tmp);
    return tmp;
  case ND_LT:
    println("  %%.%d =%c c%clt%c %%.%d, %%.%d", tmp, base_type, sign, lhs_base_type, lhs_tmp, rhs_tmp);
    return tmp;
  case ND_LE:
    println("  %%.%d =%c c%cle%c %%.%d, %%.%d", tmp, base_type, sign, lhs_base_type, lhs_tmp, rhs_tmp);
    return tmp;
  case ND_SHL:
    println("  %%.%d =%c shl %%.%d, %%.%d", tmp, base_type, lhs_tmp, rhs_tmp);
    return tmp;
  case ND_SHR:
    if (node->lhs->ty->is_unsigned)
      println("  %%.%d =%c shr %%.%d, %%.%d", tmp, base_type, lhs_tmp, rhs_tmp);
    else
      println("  %%.%d =%c sar %%.%d, %%.%d", tmp, base_type, lhs_tmp, rhs_tmp);
    return tmp;
  }

  error_tok(node->tok, "invalid expression");
  }

static void gen_stmt_qbe(Node *node) {

  switch (node->kind) {
  case ND_IF: {
    int c = count();
    int cond_tmp = gen_expr_qbe(node->cond);
    println("  jnz %%.%d, @if.%d.then, @if.%d.else", cond_tmp, c, c);
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
    if (node->init)
      gen_stmt_qbe(node->init);
    println("@for.%d.condition", c);
    if (node->cond) {
      int cond_tmp = gen_expr_qbe(node->cond);
      println("  jnz %%.%d, @for.%d.body, @for.%d.break", cond_tmp, c, c);
    }
    println("@for.%d.body", c);
    gen_stmt_qbe(node->then);
    println("@for.%d.continue", c);
    // chibicc label
    println("@label.%s", node->cont_label);
    if (node->inc)
      gen_expr_qbe(node->inc);
    println("  jmp @for.%d.condition", c);
    println("@for.%d.break", c);
    // chibicc label
    println("@label.%s", node->brk_label);
    return;
  }
  case ND_DO: {
    int c = count();
    println("@do.%d.body", c);
    gen_stmt_qbe(node->then);
    println("@do.%d.continue", c);
    // chibicc label
    println("@label.%s", node->cont_label);
    int cond_tmp = gen_expr_qbe(node->cond);
    println("  jnz %%.%d, @do.%d.body, @do.%d.break", cond_tmp, c, c);
    println("@do.%d.break", c);
    // chibicc label
    println("@label.%s", node->brk_label);
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
  case ND_GOTO: {
    int c = count();
    println("  jmp @label.%s\n", node->unique_label);
    // Add a dummy label to keep QBE happy
    println("@.goto.%d", c);
  }
    return;
  case ND_GOTO_EXPR:
    error_tok(node->tok, "computed goto not yet supported by RPJ/QBE");
    return;
  case ND_LABEL:
    println("\n@label.%s", node->unique_label);
    gen_stmt_qbe(node->lhs);
    return;
  case ND_RETURN: {
    int c = count();
    if (node->lhs) {
      // TODO - will struct/union by val work? Actually maybe... we're just copying the pointer from another var; it's copying structs/unions by val that will be tricky
      int val_tmp = gen_expr_qbe(node->lhs);
      println("  ret %%.%d", val_tmp);
      // Add a dummy label to keep QBE happy
      println("@.ret.%d", c);
      return;
    }

    println("  ret");
    // Add a dummy label to keep QBE happy
    println("@.ret.%d", c);
    return;
  }
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
      println("# section \".comm\"");
      println("data $%s = align %d { z %d }\n", var->name, align, var->ty->size);
      continue;
    }

    // .data or .tdata
    if (var->init_data) {
      if (var->is_tls) {
        println("# section \".tdata\" \"\\\"awT\\\",@progbits\"");
      }
      else {
	println("# section \".data\"");
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
      println("# section \".tbss\" \"\\\"awT\\\",@nobits\"");
    }
    else {
      println("# section \".bss\"");
    }

    println("data $%s = { z %d }\n", var->name, var->ty->size);
  }
}

static void emit_text_qbe(Obj *prog) {
  for (Obj *fn = prog; fn; fn = fn->next) {
    if (!fn->is_function || !fn->is_definition)
      continue;

    // Chibicc generates these but not needed for QBE
    // TODO - remove from chibicc
    if (!strcmp(fn->name, "__va_arg_fp") || !strcmp(fn->name, "__va_arg_gp") || !strcmp(fn->name, "__va_arg_mem")) {
      continue;
    }
    
    // No code is emitted for "static inline" functions
    // if no one is referencing them.
    if (!fn->is_live)
      continue;

    if (!fn->is_static)
      println("export");

    println("# section \".text\"");
    print("function ");
    
    if (fn->ty->return_ty->kind != TY_VOID) {
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

    println("@.start");
    
    current_fn = fn;
    current_tmp = 1;

    for (Obj *var = fn->params; var; var = var->next) {
      var->is_param = true;
    }

    for (Obj *var = fn->locals; var; var = var->next) {
      if (var->is_param)
	continue;

      // Chibicc generates these but not needed for QBE
      // TODO - remove from chibicc
      if (!strcmp(var->name, "__alloca_size__") || !strcmp(var->name, "__va_area__")) {
	continue;
      }

      int align = MAX(4, var->align);
      print("  %%");
      printlocalname(var);
      println(" =l alloc%d %d", align, var->ty->size);
    }

    print("\n");
    
    // Emit code
    gen_stmt_qbe(fn->body);
    assert(depth == 0);

    // [https://www.sigbus.info/n1570#5.1.2.2.3p1] The C spec defines
    // a special rule for the main function. Reaching the end of the
    // main function is equivalent to returning 0, even though the
    // behavior is undefined for the other functions.
    //
    // For QBE we always generate a final return just to make QBE happy
    println("\n@.final.ret");
    println("  ret%s", (fn->ty->return_ty->kind == TY_VOID ? "" : " 0"));

    // Epilogue
    println("}\n");
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
