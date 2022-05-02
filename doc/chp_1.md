# 1 LLFS:简介

标签（空格分隔）： LLVM chibicc

---

本专栏介绍如何使用LLVM, 基于chibicc c++版本生成的AS, 构建一个C编译器。其目的是从实战角度学习LLLVM API的使用
## 开发环境配置
笔者所用开发环境为Ubuntu 20.4 + LLVM-13, 详细配置过程和LLVM使用命令可参考[安装LLVM](https://zhuanlan.zhihu.com/p/450602843)
## chibicc 简介
[chibicc](https://github.com/rui314/chibicc) 是一个实现了大多数 C11 特性的 C 编译器，能编译 Git, SQLite, libpng 等大型 C 项目。其代码使用C语言开发，可读性良好，适合学习C编译器的开发。考虑到LLVM使用C++开发，故笔者对chibicc进行简单修改，移植了一个C++版本[chibicc_cpp](https://github.com/bigconvience/chibicc_cpp)
## 整体架构
![LLFS 架构][1]

  [1]: https://pic2.zhimg.com/80/v2-ad86694daf5e4234aab3c55de7ecaec3_720w.png
  LLFS 架构

笔者定义了一个yuc ast, 在chibicc生成汇编代码的同时生成yuc ast。 该as随t会后会被解析成LLVM IR.
  
##  小试牛刀：创建全局变量
### 4.1 C代码分析
对如下C代码 hello.c
```
int variable = 21;
static int staticVal = -3;

int main(){
	staticVal = variable + staticVal;
	return staticVal;
}
```
使用*clang -emit-llvm -S hello.c* 生成的hello.ll中，变量variable和staticVal的定义如下：
```
@variable = dso_local global i32 21, align 4
@staticVal = internal global i32 -3, align 4
```
### 4.2 yuc ast
yuc.h中定义的ast是chibicc ast通往LLVM ir的桥梁：
```
#include <string>
#include <fstream>
#include <iostream>
using namespace std;

namespace yuc {
	enum LinkageTypes {
	  ExternalLinkage = 0, AvailableExternallyLinkage, LinkOnceAnyLinkage, LinkOnceODRLinkage,
	  WeakAnyLinkage, WeakODRLinkage, AppendingLinkage, InternalLinkage,
	  PrivateLinkage, ExternalWeakLinkage, CommonLinkage
	};

	typedef enum {
		ArrayType = 0, TypedPointerType, FunctionType, IntegerType,
		PointerType, StructType, VectorType
	} CTypeKind;

	struct CType {
		CTypeKind kind;
		int size;
		bool is_unsigned;
	};

	struct CValue {
		int64_t val;
	};

	struct Ast {
		bool is_function;
		bool is_static;
		string name;
		int align;
		Ast *next;
		LinkageTypes linkage_type;
		bool is_preemptable;

		CType *type;
		CValue *initializer;
	};

	void ir_gen(Ast *ast, ofstream &out, const string &moduleName);
}
```
目前只提供一个api: 
```
void ir_gen(Ast *ast, ofstream &out, const string &moduleName);
```
codegen.cpp代码中，emit_data()和emit_text()分别创建变量ast和函数ast，最后调用ir_gen来生成LLVM ir.目前ast只适配了int类型，后续不断完善其它类型。
### 4.3 运行程序
运行build.sh脚本，console中输出：
```
; ModuleID = './test2/hello.c'
source_filename = "./test2/hello.c"

@staticVal = internal global i32 -3, align 4
@variable = global i32 21, align 4
```
## 源码地址
[llfs:chp1](https://github.com/bigconvience/llfs/tree/chp_1)