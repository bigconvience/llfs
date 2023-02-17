; ModuleID = './test2/hello.c'
source_filename = "./test2/hello.c"

@.str = private unnamed_addr constant [40 x i8] c"({ int x; if (2-1) x=2; else x=3; x; })\00", align 1
@.str.1 = private unnamed_addr constant [58 x i8] c"({ int i=0; int j=0; for (i=0; i<=10; i=i+1) j=i+j; j; })\00", align 1
@.str.2 = private unnamed_addr constant [37 x i8] c"({ int i=0; while(i<10) i=i+1; i; })\00", align 1
@.str.3 = private unnamed_addr constant [4 x i8] c"OK\0A\00", align 1

declare dso_local i32 @memcmp(i8*, i8*, i64)

declare dso_local i8* @memcpy(i8*, i8*, i64)

declare dso_local void @exit(i32)

declare dso_local i32 @printf(i8*, ...)

declare dso_local void @assert(i32, i32, i8*)

define dso_local i32 @main() {
  %1 = alloca i32, align 4
  %2 = alloca i32, align 4
  %3 = alloca i32, align 4
  %4 = alloca i32, align 4
  %5 = alloca i32, align 4
  %6 = alloca i32, align 4
  %7 = alloca i32, align 4
  %8 = alloca i32, align 4
  store i32 0, i32* %1, align 4
  store i32 2, i32* %2, align 4
  %9 = load i32, i32* %2, align 4
  store i32 %9, i32* %3, align 4
  %10 = load i32, i32* %3, align 4
  call void @assert(i32 2, i32 %10, i8* getelementptr inbounds ([40 x i8], [40 x i8]* @.str, i64 0, i64 0))
  store i32 0, i32* %4, align 4
  store i32 0, i32* %5, align 4
  store i32 0, i32* %4, align 4
  br label %11

11:                                               ; preds = %18, %0
  %12 = load i32, i32* %4, align 4
  %13 = icmp sle i32 %12, 10
  br i1 %13, label %14, label %21

14:                                               ; preds = %11
  %15 = load i32, i32* %4, align 4
  %16 = load i32, i32* %5, align 4
  %17 = add nsw i32 %15, %16
  store i32 %17, i32* %5, align 4
  br label %18

18:                                               ; preds = %14
  %19 = load i32, i32* %4, align 4
  %20 = add nsw i32 %19, 1
  store i32 %20, i32* %4, align 4
  br label %11

21:                                               ; preds = %11
  %22 = load i32, i32* %5, align 4
  store i32 %22, i32* %6, align 4
  %23 = load i32, i32* %6, align 4
  call void @assert(i32 55, i32 %23, i8* getelementptr inbounds ([58 x i8], [58 x i8]* @.str.1, i64 0, i64 0))
  store i32 0, i32* %7, align 4
  br label %24

24:                                               ; preds = %27, %21
  %25 = load i32, i32* %7, align 4
  %26 = icmp slt i32 %25, 10
  br i1 %26, label %27, label %30

27:                                               ; preds = %24
  %28 = load i32, i32* %7, align 4
  %29 = add nsw i32 %28, 1
  store i32 %29, i32* %7, align 4
  br label %24

30:                                               ; preds = %24
  %31 = load i32, i32* %7, align 4
  store i32 %31, i32* %8, align 4
  %32 = load i32, i32* %8, align 4
  call void @assert(i32 10, i32 %32, i8* getelementptr inbounds ([37 x i8], [37 x i8]* @.str.2, i64 0, i64 0))
  %33 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str.3, i64 0, i64 0))
  ret i32 0
}
