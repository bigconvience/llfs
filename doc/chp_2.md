# LLVM全局变量初始化

标签（空格分隔）： LLVM

---

## 创建全局变量

全局变量的创建与初始化所对应的LLVM IR的生产代码步骤如下：
```
GlobalVariable *createGlobalVar(Ast *yucNode) {
  string name = yucNode->name;
  CType *ctype = yucNode->type;
  std::cout << "createGlobalVar kind:" << CType::ctypeKindString(ctype->kind);
  if (ctype->base) {
    std::cout << " base " << CType::ctypeKindString(ctype->base->kind);
  }
  if (ctype->origin) {
    std::cout << " origin " << CType::ctypeKindString(ctype->origin->kind);
  }
  std::cout << endl;
  // 获取全局变量的类型
  Type *DesiredType = yuc2LLVMType(ctype);
  // 注册全局变量
  TheModule->getOrInsertGlobal(name, DesiredType);
  GlobalVariable *gVar = TheModule->getNamedGlobal(name);
  // 设置该全局变量的align属性
  gVar->setAlignment(MaybeAlign(yucNode->align));
  // 设置该全局变量的DSOLocal属性
  if (!yucNode->is_static) {
    gVar->setDSOLocal(true);
  }
  // 创建对应的常量
  Constant *initializer = yuc2Constants(DesiredType, yucNode);
  // 初始化全局变量
  gVar->setInitializer(initializer);
  // 设置Linkage属性
  gVar->setLinkage(yuc2LinkageType(yucNode));
  return gVar;
}
```

## 数据类型转换
将C的数据类型CType转成LLVM IR的数据类型llvm::Type的代码如下:
```
static Type *yuc2LLVMType(CType *ctype) {
  cout << "yuc2LLVMType start kind: "
      << CType::ctypeKindString(ctype->kind) << endl;
  Type *type;
  switch (ctype->kind) {
    case CType::TY_CHAR:
      type = Builder->getInt8Ty();
      break;
    case CType::TY_SHORT:
      type = Builder->getInt16Ty();
      break;
    case CType::TY_INT:
      type = Builder->getInt32Ty();
      break;
    case CType::TY_LONG:
      type = Builder->getInt64Ty();
      break;
    case CType::TY_PTR:
      type = yuc2PointerType(ctype);
      break;
    case CType::TY_ARRAY:
      type = yuc2ArrayType(ctype);
      break;
    case CType::TY_UNION:
    case CType::TY_STRUCT:
      type = ConvertRecordDeclType(ctype);
      break;
    default:
      type = Builder->getInt32Ty();
      break;
  }
  return type;
}
```

## 常量创建
C的初始化数据在init_data(char *)中，将char *类型的数据转换成llvm::Constant的过程，会根据不同的llvm::Type不同而分为如下几类
 1. 整数类型
 2. 浮点数类型
 3. 数组类型
 4. 创建struct和union类型
 5. 指针类型
 6. 枚举类型
 
后续文章将详细介绍每种数据类型的创建方法。

##  测试用例及输出
### 测试用例
```
char g3 = 3;
short g4 = 4;
int g5 = 5;
long g6 = 6;
int g9[3] = {0, 1, 2};
struct {char a; int b;} g11[2] = {{1, 2}, {3, 4}};
struct {int a[2];} g12[2] = {{{1, 2}}};
union { int a; char b[8]; } g13[2] = {0x01020304, 0x05060708};
char g17[] = "foobar";
char g18[10] = "foobar";
char g19[3] = "foobar";
char *g20 = g17+0;
char *g21 = g17+3;
char *g22 = &g17-3;
char *g23[] = {g17+0, g17+3, g17-3};
int g24=3;
int *g25=&g24;
int g26[3] = {1, 2, 3};
int *g27 = g26 + 1;
int *g28 = &g11[1].a;
long g29 = (long)(long)g26;
struct { struct { int a[3]; } a; } g30 = {{{1,2,3}}};
int *g31=g30.a.a;
struct {int a[2];} g40[2] = {{1, 2}, 3, 4};
struct {int a[2];} g41[2] = {1, 2, 3, 4};
char g43[][4] = {'f', 'o', 'o', 0, 'b', 'a', 'r', 0};
char *g44 = {"foo"};
union { int a; char b[4]; } g50 = {.b[2]=0x12};
union { int a; } g51[2] = {};

typedef char T60[];
T60 g60 = {1, 2, 3};
T60 g61 = {1, 2, 3, 4, 5, 6};

typedef struct { char a, b[]; } T65;
T65 g65 = {'f','o','o',0};
T65 g66 = {'f','o','o','b','a','r',0};
```
### 输出的IR
```
; ModuleID = './test2/initializer.c'
source_filename = "./test2/initializer.c"

%struct.anon = type { i8, i32 }
%struct.anon.0 = type { [2 x i32] }
%union.anon = type { i32, [4 x i8] }
%struct.anon.2 = type { %struct.anon.1 }
%struct.anon.1 = type { [3 x i32] }
%struct.anon.3 = type { [2 x i32] }
%struct.anon.4 = type { [2 x i32] }
%union.anon.6 = type { i32 }

@g3 = dso_local global i8 3, align 1
@g4 = dso_local global i16 4, align 2
@g5 = dso_local global i32 5, align 4
@g6 = dso_local global i64 6, align 8
@g9 = dso_local global [3 x i32] [i32 0, i32 1, i32 2], align 4
@g11 = dso_local global [2 x %struct.anon] [%struct.anon { i8 1, i32 2 }, %struct.anon { i8 3, i32 4 }], align 16
@g12 = dso_local global [2 x %struct.anon.0] [%struct.anon.0 { [2 x i32] [i32 1, i32 2] }, %struct.anon.0 zeroinitializer], align 16
@g13 = dso_local global [2 x %union.anon] [%union.anon { i32 16909060, [4 x i8] undef }, %union.anon { i32 84281096, [4 x i8] undef }], align 16
@g17 = dso_local global [7 x i8] c"foobar\00", align 1
@g18 = dso_local global [10 x i8] c"foobar\00\00\00\00", align 1
@g19 = dso_local global [3 x i8] c"foo", align 1
@g20 = dso_local global i8* getelementptr inbounds ([7 x i8], [7 x i8]* @g17, i32 0, i32 0), align 8
@g21 = dso_local global i8* getelementptr inbounds ([7 x i8], [7 x i8]* @g17, i32 0, i64 3), align 8
@g22 = dso_local global i8* getelementptr ([7 x i8], [7 x i8]* @g17, i32 0, i64 -21), align 8
@g23 = dso_local global [3 x i8*] [i8* getelementptr inbounds ([7 x i8], [7 x i8]* @g17, i32 0, i32 0), i8* getelementptr inbounds ([7 x i8], [7 x i8]* @g17, i32 0, i64 3), i8* getelementptr ([7 x i8], [7 x i8]* @g17, i32 0, i64 -3)], align 16
@g24 = dso_local global i32 3, align 4
@g25 = dso_local global i32* @g24, align 8
@g26 = dso_local global [3 x i32] [i32 1, i32 2, i32 3], align 4
@g27 = dso_local global i32* bitcast (i8* getelementptr (i8, i8* bitcast ([3 x i32]* @g26 to i8*), i64 4) to i32*), align 8
@g28 = dso_local global i32* bitcast (i8* getelementptr (i8, i8* getelementptr inbounds ([2 x %struct.anon], [2 x %struct.anon]* @g11, i32 0, i32 0, i32 0), i64 8) to i32*), align 8
@g29 = dso_local global i64 ptrtoint ([3 x i32]* @g26 to i64), align 8
@g30 = dso_local global %struct.anon.2 { %struct.anon.1 { [3 x i32] [i32 1, i32 2, i32 3] } }, align 4
@g31 = dso_local global i32* getelementptr inbounds (%struct.anon.2, %struct.anon.2* @g30, i32 0, i32 0, i32 0, i32 0), align 8
@g40 = dso_local global [2 x %struct.anon.3] [%struct.anon.3 { [2 x i32] [i32 1, i32 2] }, %struct.anon.3 { [2 x i32] [i32 3, i32 4] }], align 16
@g41 = dso_local global [2 x %struct.anon.4] [%struct.anon.4 { [2 x i32] [i32 1, i32 2] }, %struct.anon.4 { [2 x i32] [i32 3, i32 4] }], align 16
@g43 = dso_local global [2 x [4 x i8]] [[4 x i8] c"foo\00", [4 x i8] c"bar\00"], align 1
@g44 = dso_local global i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str, i32 0, i32 0), align 8
@.str = private unnamed_addr constant [4 x i8] c"foo\00", align 1
@g50 = dso_local global { [4 x i8] } { [4 x i8] c"\00\00\12\00" }, align 4
@g51 = dso_local global [2 x %union.anon.6] zeroinitializer, align 4
@g60 = dso_local global [3 x i8] c"\01\02\03", align 1
@g61 = dso_local global [6 x i8] c"\01\02\03\04\05\06", align 1
@g65 = dso_local global { i8, [3 x i8] } { i8 102, [3 x i8] c"oo\00" }, align 1
@g66 = dso_local global { i8, [6 x i8] } { i8 102, [6 x i8] c"oobar\00" }, align 1
```

## 源码地址
[llfs:chp1](https://github.com/bigconvience/llfs/tree/chp_2)

