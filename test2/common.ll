; ModuleID = './test2/common.c'
source_filename = "./test2/common.c"

@.str = private unnamed_addr constant [10 x i8] c"%s => %d\0A\00", align 1
@.str.1 = private unnamed_addr constant [30 x i8] c"%s => %d expected but got %d\0A\00", align 1

declare dso_local void @exit(i32)

declare dso_local i32 @printf(i8*, ...)

define dso_local void @assert(i32 %0, i32 %1, i8* %2) {
  %4 = alloca i8*, align 8
  %5 = alloca i32, align 4
  %6 = alloca i32, align 4
  store i32 %0, i32* %6, align 4
  store i32 %1, i32* %5, align 4
  store i8* %2, i8** %4, align 8
  %7 = load i32, i32* %6, align 4
  %8 = load i32, i32* %5, align 4
  %9 = icmp eq i32 %7, %8
  br i1 %9, label %10, label %14

10:                                               ; preds = %3
  %11 = load i8*, i8** %4, align 8
  %12 = load i32, i32* %5, align 4
  %13 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([10 x i8], [10 x i8]* @.str, i64 0, i64 0), i8* %11, i32 %12)
  br label %19

14:                                               ; preds = %3
  %15 = load i8*, i8** %4, align 8
  %16 = load i32, i32* %6, align 4
  %17 = load i32, i32* %5, align 4
  %18 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([30 x i8], [30 x i8]* @.str.1, i64 0, i64 0), i8* %15, i32 %16, i32 %17)
  call void @exit(i32 1)
  unreachable

19:                                               ; preds = <null operand!>, %10
  ret void
}
