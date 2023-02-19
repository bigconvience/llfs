; ModuleID = './test2/hello.c'
source_filename = "./test2/hello.c"

%struct.anon = type { i8, i32 }
%struct.t = type { i32, i32 }

@.str = private unnamed_addr constant [44 x i8] c"({ struct {char a; int b;} x; sizeof(x); })\00", align 1
@.str.1 = private unnamed_addr constant [57 x i8] c"({ struct t {int a; int b;} x; struct t y; sizeof(y); })\00", align 1
@.str.2 = private unnamed_addr constant [4 x i8] c"OK\0A\00", align 1

declare dso_local i32 @memcmp(i8*, i8*, i64)

declare dso_local i8* @memcpy(i8*, i8*, i64)

declare dso_local void @exit(i32)

declare dso_local i32 @printf(i8*, ...)

declare dso_local void @assert(i32, i32, i8*)

define dso_local i32 @main() {
  %1 = alloca i32, align 4
  %2 = alloca %struct.anon, align 4
  %3 = alloca i64, align 8
  %4 = alloca %struct.t, align 4
  %5 = alloca %struct.t, align 4
  %6 = alloca i64, align 8
  store i32 0, i32* %1, align 4
  store i64 8, i64* %3, align 4
  %7 = load i64, i64* %3, align 4
  %8 = trunc i64 %7 to i32
  call void @assert(i32 8, i32 %8, i8* getelementptr inbounds ([44 x i8], [44 x i8]* @.str, i64 0, i64 0))
  store i64 8, i64* %6, align 4
  %9 = load i64, i64* %6, align 4
  %10 = trunc i64 %9 to i32
  call void @assert(i32 8, i32 %10, i8* getelementptr inbounds ([57 x i8], [57 x i8]* @.str.1, i64 0, i64 0))
  %11 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str.2, i64 0, i64 0))
  ret i32 0
}
