; ModuleID = './test2/hello.c'
source_filename = "./test2/hello.c"

%struct.anon = type { i8, i8 }
%struct.anon.0 = type { i8, i8 }
%struct.anon.1 = type { i8, i8 }
%struct.anon.2 = type { i8, i8 }

@.str = private unnamed_addr constant [64 x i8] c"({ struct {char a; char b;} x[3]; char *p=x; p[0]=0; x[0].a; })\00", align 1
@.str.1 = private unnamed_addr constant [64 x i8] c"({ struct {char a; char b;} x[3]; char *p=x; p[1]=1; x[0].b; })\00", align 1
@.str.2 = private unnamed_addr constant [64 x i8] c"({ struct {char a; char b;} x[3]; char *p=x; p[2]=2; x[1].a; })\00", align 1
@.str.3 = private unnamed_addr constant [64 x i8] c"({ struct {char a; char b;} x[3]; char *p=x; p[3]=3; x[1].b; })\00", align 1
@.str.4 = private unnamed_addr constant [4 x i8] c"OK\0A\00", align 1

declare dso_local i32 @memcmp(i8*, i8*, i64)

declare dso_local i8* @memcpy(i8*, i8*, i64)

declare dso_local void @exit(i32)

declare dso_local i32 @printf(i8*, ...)

declare dso_local void @assert(i32, i32, i8*)

define dso_local i32 @main() {
  %1 = alloca i32, align 4
  %2 = alloca [3 x %struct.anon], align 1
  %3 = alloca i8*, align 8
  %4 = alloca i8, align 1
  %5 = alloca [3 x %struct.anon.0], align 1
  %6 = alloca i8*, align 8
  %7 = alloca i8, align 1
  %8 = alloca [3 x %struct.anon.1], align 1
  %9 = alloca i8*, align 8
  %10 = alloca i8, align 1
  %11 = alloca [3 x %struct.anon.2], align 1
  %12 = alloca i8*, align 8
  %13 = alloca i8, align 1
  store i32 0, i32* %1, align 4
  %14 = getelementptr inbounds [3 x %struct.anon], [3 x %struct.anon]* %2, i64 0, i64 0
  %15 = bitcast %struct.anon* %14 to i8*
  store i8* %15, i8** %3, align 8
  %16 = load i8*, i8** %3, align 8
  %17 = getelementptr inbounds i8, i8* %16, i64 0
  store i8 0, i8* %17, align 1
  %18 = getelementptr inbounds [3 x %struct.anon], [3 x %struct.anon]* %2, i64 0, i64 0
  %19 = getelementptr inbounds %struct.anon, %struct.anon* %18, i32 0, i32 0
  %20 = load i8, i8* %19, align 1
  store i8 %20, i8* %4, align 1
  %21 = load i8, i8* %4, align 1
  %22 = sext i8 %21 to i32
  call void @assert(i32 0, i32 %22, i8* getelementptr inbounds ([64 x i8], [64 x i8]* @.str, i64 0, i64 0))
  %23 = getelementptr inbounds [3 x %struct.anon.0], [3 x %struct.anon.0]* %5, i64 0, i64 0
  %24 = bitcast %struct.anon.0* %23 to i8*
  store i8* %24, i8** %6, align 8
  %25 = load i8*, i8** %6, align 8
  %26 = getelementptr inbounds i8, i8* %25, i64 1
  store i8 1, i8* %26, align 1
  %27 = getelementptr inbounds [3 x %struct.anon.0], [3 x %struct.anon.0]* %5, i64 0, i64 0
  %28 = getelementptr inbounds %struct.anon.0, %struct.anon.0* %27, i32 0, i32 1
  %29 = load i8, i8* %28, align 1
  store i8 %29, i8* %7, align 1
  %30 = load i8, i8* %7, align 1
  %31 = sext i8 %30 to i32
  call void @assert(i32 1, i32 %31, i8* getelementptr inbounds ([64 x i8], [64 x i8]* @.str.1, i64 0, i64 0))
  %32 = getelementptr inbounds [3 x %struct.anon.1], [3 x %struct.anon.1]* %8, i64 0, i64 0
  %33 = bitcast %struct.anon.1* %32 to i8*
  store i8* %33, i8** %9, align 8
  %34 = load i8*, i8** %9, align 8
  %35 = getelementptr inbounds i8, i8* %34, i64 2
  store i8 2, i8* %35, align 1
  %36 = getelementptr inbounds [3 x %struct.anon.1], [3 x %struct.anon.1]* %8, i64 0, i64 1
  %37 = getelementptr inbounds %struct.anon.1, %struct.anon.1* %36, i32 0, i32 0
  %38 = load i8, i8* %37, align 1
  store i8 %38, i8* %10, align 1
  %39 = load i8, i8* %10, align 1
  %40 = sext i8 %39 to i32
  call void @assert(i32 2, i32 %40, i8* getelementptr inbounds ([64 x i8], [64 x i8]* @.str.2, i64 0, i64 0))
  %41 = getelementptr inbounds [3 x %struct.anon.2], [3 x %struct.anon.2]* %11, i64 0, i64 0
  %42 = bitcast %struct.anon.2* %41 to i8*
  store i8* %42, i8** %12, align 8
  %43 = load i8*, i8** %12, align 8
  %44 = getelementptr inbounds i8, i8* %43, i64 3
  store i8 3, i8* %44, align 1
  %45 = getelementptr inbounds [3 x %struct.anon.2], [3 x %struct.anon.2]* %11, i64 0, i64 1
  %46 = getelementptr inbounds %struct.anon.2, %struct.anon.2* %45, i32 0, i32 1
  %47 = load i8, i8* %46, align 1
  store i8 %47, i8* %13, align 1
  %48 = load i8, i8* %13, align 1
  %49 = sext i8 %48 to i32
  call void @assert(i32 3, i32 %49, i8* getelementptr inbounds ([64 x i8], [64 x i8]* @.str.3, i64 0, i64 0))
  %50 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str.4, i64 0, i64 0))
  ret i32 0
}
