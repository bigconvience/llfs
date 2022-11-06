# LLVM创建数union

标签（空格分隔）： LLVM

---

## 规则介绍
### 类型转换规则
LLVM中没有直接描述C语言枚举的IR, 而是通过StructType来描述。对于一个枚举类型，若枚举初始化的是第一个字段，则会有一个对应的identified struct.若枚举初始化的不是第一个字段，则会生产一个对应的literal struct。C语言的union和llvm::StructType的转换规则如下：
```
static llvm::StructType *yuc2StructType(CType *ctype) {
  cout << "yuc2StructType" << endl;
  llvm::SmallVector<llvm::Type *, 16> Types;
  Types.reserve(ctype->memberCount);
  CMember *member = ctype->union_field;
  llvm::StructType *type;
  int DesiredSize = ctype->size;
  cout << "yuc2StructType DesiredSize:" << DesiredSize << endl;
  // union
  if (member) {
    int Size = member->ty->size;
    cout << "union member idx: " << member->idx << " size: " << Size << endl;
    
    Types.push_back(yuc2LLVMType(member->ty));
    if (DesiredSize > Size) {
      int PadSize = DesiredSize - Size;
      cout << "PadSize: " << PadSize << endl;
      llvm::Type *paddingType = getPaddingType(PadSize);
      Types.push_back(paddingType);
    }

    if (member->idx) {
      type = llvm::StructType::get(*TheContext, Types, false);
    } else {
      type = llvm::StructType::create(*TheContext, Types, "", false);
    }
    cout << "final type: " << Types.size() << endl;
    return type;
  }
  // struct
  for (CMember *member = ctype->members; member; member = member->next) {
    Types.push_back(yuc2LLVMType(member->ty));
  }
  if (ctype->is_typedef) {
    type = llvm::StructType::get(*TheContext, Types, false);
  } else {
    type = llvm::StructType::create(*TheContext, Types, "", false);
  }
  return type;
}
```
### union常量创建规则
```
llvm::Constant *EmitUnionInitialization(llvm::StructType *Ty, CType *ctype, char *buf, int offset) {
  int DesiredSize = ctype->size;
  int AlignedSize = ctype->align; 
  SmallVector<llvm::Constant *, 16> Elements;
  unsigned NumElements = ctype->memberCount;
  std::cout << "EmitUnionInitialization NumElements： " << NumElements 
    << " DesiredSize: " <<  DesiredSize
    << " AlignedSize: " <<  AlignedSize 
    << endl;
  Elements.reserve(NumElements);
  bool Packed = false;
  CMember *member = ctype->union_field;
  if (member) {
    int Size = member->ty->size;
    std::cout << "EmitUnionInitialization union Size " << Size << endl;
    Type *Ty = yuc2LLVMType(member->ty);
    llvm::Constant *constantValue = buffer2Constants(Ty, member->ty, NULL, buf, offset + member->offset);
    Elements.push_back(constantValue);
    if (DesiredSize > Size) {
      Elements.push_back(getPadding(DesiredSize - Size));
    }
  }
  llvm::StructType *STy = llvm::ConstantStruct::getTypeForElements(CGM().getLLVMContext(), Elements, Packed);
  return llvm::ConstantStruct::get(STy, Elements);
}
```
## 用例和IR
### 用例
```
// identified struct
union { int a; char b[8]; } g13[2] = {0x01020304, 0x05060708};
union { int a; } g51[2] = {};
// literal struct
union { int a; char b[4]; } g50 = {.b[2]=0x12};
```
### IR
```
%union.anon = type { i32, [4 x i8] }
%union.anon.0 = type { i32 }

@g13 = dso_local global [2 x %union.anon] [%union.anon { i32 16909060, [4 x i8] undef }, %union.anon { i32 84281096, [4 x i8] undef }], align 16, !dbg !0
@g51 = dso_local global [2 x %union.anon.0] zeroinitializer, align 4, !dbg !5
@g50 = dso_local global { [4 x i8] } { [4 x i8] c"\00\00\12\00" }, align 4, !dbg !15
```
## 源码地址
[llfs:chp1](https://github.com/bigconvience/llfs/tree/chp_2)