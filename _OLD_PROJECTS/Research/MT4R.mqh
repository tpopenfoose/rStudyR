//+1--2--3--4--5--6--7--8--9--0--1--2--3--4--5--6--7--8--9--+
//+	头文件 - 包含文件                                     +
//+1--2--3--4--5--6--7--8--9--0--1--2--3--4--5--6--7--8--9--+
//#define RPATH "D:/SOFTWARE/PROGRAMMING/R/R-3.2.3/bin/x64/Rterm.exe --no-save" // HOME_PATH
//#define RPATH "C:/Program Files/R/R-3.2.3/bin/x64/Rterm.exe --no-save" //  COMPANY_PATH
#define RPATH "D:/HawkSoftware/R-3.2.5/bin/x64/Rterm.exe --no-save"  //  COMPANY_PATH

#define MT4R_VERSION_MAJOR 1
#define MT4R_VERSION_MINOR 4
#import "MT4Rb7.dll"
   int      RGetDllVersion();
   int      RInit_(string commandline, int debuglevel);
   void     RDeinit(int rhandle);
   bool     RIsRunning(int rhandle);
   bool     RIsBusy(int rhandle);
   void     RExecuteAsync(int rhandle, string code);
   void     RExecute(int rhandle, string code);
   void     RAssignBool(int rhandle, string variable, bool value);
   void     RAssignInteger(int rhandle, string variable, int value);
   void     RAssignDouble(int rhandle, string variable, double value);
   void     RAssignString(int rhandle, string variable, string value);
   void     RAssignVector(int rhandle, string variable, double &vector[], int size);
   void     RAssignStringVector(int rhandle, string variable, string &vector[], int size);
   void     RAssignMatrix(int rhandle, string variable, double &matrix[], int rows, int cols);
   void     RAppendMatrixRow(int rhandle, string variable, double &vector[], int size);
   bool     RExists(int rhandle, string variable);
   bool     RGetBool(int rhandle, string expression);
   int      RGetInteger(int rhandle, string expression);
   double   RGetDouble(int rhandle, string expression);
   int      RGetVector(int rhandle, string expression, double &vector[], int size);
   void     RPrint(int rhandle, string expression);
#import
//+1--2--3--4--5--6--7--8--9--0--1--2--3--4--5--6--7--8--9--0--1--2--3--4--5--6--7--8--9--0--1--2--3--4--5--6--7--8--9--0--1--2--3--4--5--6--7--8--9--+
//+	C: C_MT4R
//+	MT4和R连接类
//+   Source:  New
//+	Version: 1.2
//+1--2--3--4--5--6--7--8--9--0--1--2--3--4--5--6--7--8--9--0--1--2--3--4--5--6--7--8--9--0--1--2--3--4--5--6--7--8--9--0--1--2--3--4--5--6--7--8--9--+
//+	Update:  2016-02-24  1.2   添加了Handle的Getter，Setter函数，以便两个R实例共享对象
//+                        1.2   添加了Source函数
//+            2016-02-20  1.1   添加了GetS函数，通过将String转换成Double Vector，再转成Int，最后还原为String
//+                        1.1   GetV函数测试似乎无用，用for循环重写了代码
//+            2016-01-08  1.0   完成Version 1.0  |  <dll中不含GetString，计划以后添加>
//+	         2016-01-07  0.0   创建
//+1--2--3--4--5--6--7--8--9--0--1--2--3--4--5--6--7--8--9--0--1--2--3--4--5--6--7--8--9--0--1--2--3--4--5--6--7--8--9--0--1--2--3--4--5--6--7--8--9--+
class C_MT4R {
private:
//<<-------------------------->> 成员变量
   int                        miHandle;
//<<-------------------------------------------------------------------------------------->> 
public:
                              C_MT4R(void);                 //    构造
                             ~C_MT4R(void);                 //    析构
   
   int                        Init(                         //+   i  初始化
                                 string   psCommandLine,
                                 int      piDebugLevel
                              );
   int                        Handle(void);                 //+   i  获取Handle
   void                       Handle(                       //+   v  设置Handle
                                 int      piHandle
                              );
   void                       Start(                        //+   v  启动R
                                 string   psPath   = "",
                                 int      piDebug  = 1
                              );
   void                       Stop(void);                   //+   v  停止R
   bool                       IsRunning(void);              //+   b  判断R是否在运行中
   bool                       IsBusy(void);                 //+   b  判断R是否在执行一个命令
   void                       ExecuteAsync(                 //+   v  执行R代码不等待
                                 string   psCode
                              );
   void                       Execute(                      //+   v  执行R代码
                                 string   psCode
                              );
   void                       AssignB(                      //+   v  bool类型赋值
                                 string   psName,
                                 bool     pbBoolValue
                              );
   void                       AssignI(                      //+   v  int类型赋值
                                 string   psName,
                                 int      piIntValue
                              );
   void                       AssignS(                      //+   v  string类型赋值
                                 string   psName,
                                 string   psStringValue
                              );
   void                       AssignD(                      //+   v  double类型赋值
                                 string   psName,
                                 double   pdDoubleValue
                              );
   void                       AssignV(                      //+   v  向量类型赋值
                                 string   psName,
                                 double&  pdaVector[]
                              );
   void                       AssignF(                      //+   v  因子类型赋值
                                 string   psName,
                                 string&  psaFactor[]
                              );
   void                       AssignM(                      //+   v  矩阵类型赋值
                                 string   psName,
                                 double&  pdaMatrix[],
                                 int      piRows,
                                 int      piCols
                              );
   void                       AppendMatrixRow(              //+   v  添加一行向量到矩阵
                                 string   psName,
                                 double&  pdaVector[],
                                 int      piSize
                              );
   bool                       Exists(                       //+   b  判断变量是否存在
                                 string   psName
                              );
   int                        GetI(                         //+   i  获取int类型值
                                 string   psExpression
                              );
   double                     GetD(                         //+   d  获取double类型值
                                 string   psExpression
                              );
   bool                       GetB(                         //+   b  获取bool类型值
                                 string   psExpression
                              );
   string                     GetS(                         //+   s  获取String类型值
                                 string   psExpression
                              );
   void                       GetV(                         //+   v  获取Vector
                                 string   psName,
                                 double&  pdaVector[]
                              );
   void                       Source(                       //+   v  加载资源文件
                                 string   psScriptLink
                              );
   void                       PrintR(                       //+   v  打印表达式
                                 string   psExpression
                              );
//+1--2--3--4--5--6--7--8--9--0--1--2--3--4--5--6--7--8--9--0--1--2--3--4--5--6--7--8--9--0--1--2--3--4--5--6--7--8--9--+
};
//+1--2--3--4--5--6--7--8--9--0--1--2--3--4--5--6--7--8--9--+
//+	构造                                                  +
//+1--2--3--4--5--6--7--8--9--0--1--2--3--4--5--6--7--8--9--+
C_MT4R::C_MT4R(void)  { }
//+1--2--3--4--5--6--7--8--9--0--1--2--3--4--5--6--7--8--9--+
//+	析构                                                  +
//+1--2--3--4--5--6--7--8--9--0--1--2--3--4--5--6--7--8--9--+
C_MT4R::~C_MT4R(void) { }

//+1--2--3--4--5--6--7--8--9--0--1--2--3--4--5--6--7--8--9--+
//+   i  初始化                                             +
//+1--2--3--4--5--6--7--8--9--0--1--2--3--4--5--6--7--8--9--+
int      C_MT4R::Init(
            string   psCommandLine,
            int      piDebugLevel
         ) {
   int      liDllVersion = RGetDllVersion();
   if(liDllVersion == (MT4R_VERSION_MAJOR << 16) + MT4R_VERSION_MINOR) return RInit_(psCommandLine, piDebugLevel);
   int      liDllMajor  = liDllVersion >> 16;
   int      liDllMinor  = liDllVersion & 0xffff;
   string   lsError     = StringConcatenate("Version mismatch! MT4R.dll expected Version ",
                              IntegerToString(MT4R_VERSION_MAJOR), ".",IntegerToString(MT4R_VERSION_MINOR),
                              " - Found dll Version ", IntegerToString(liDllMajor), ".", IntegerToString(liDllMinor));
   Print(lsError);
   return 0;
}
//+1--2--3--4--5--6--7--8--9--0--1--2--3--4--5--6--7--8--9--+
//+   i  获取Handle                                         +
//+1--2--3--4--5--6--7--8--9--0--1--2--3--4--5--6--7--8--9--+
int      C_MT4R::Handle(void) {
   return miHandle;
}
//+1--2--3--4--5--6--7--8--9--0--1--2--3--4--5--6--7--8--9--+
//+   v  设置Handle                                         +
//+1--2--3--4--5--6--7--8--9--0--1--2--3--4--5--6--7--8--9--+
void     C_MT4R::Handle(
            int   piHandle
         ) {
   miHandle = piHandle;
}
//+1--2--3--4--5--6--7--8--9--0--1--2--3--4--5--6--7--8--9--+
//+   v  启动R                                              +
//+1--2--3--4--5--6--7--8--9--0--1--2--3--4--5--6--7--8--9--+
void     C_MT4R::Start(
            string   psPath   = "",
            int      piDebug  = 1
         ) {
   if(psPath=="") psPath = RPATH;
   miHandle = Init(psPath, piDebug);
}
//+1--2--3--4--5--6--7--8--9--0--1--2--3--4--5--6--7--8--9--+
//+   v  停止R                                              +
//+1--2--3--4--5--6--7--8--9--0--1--2--3--4--5--6--7--8--9--+
void     C_MT4R::Stop(void) {
   RDeinit(miHandle);
}
//+1--2--3--4--5--6--7--8--9--0--1--2--3--4--5--6--7--8--9--+
//+   b  判断R是否在运行中                                  +
//+1--2--3--4--5--6--7--8--9--0--1--2--3--4--5--6--7--8--9--+
bool     C_MT4R::IsRunning(void) {
   return RIsRunning(miHandle);
}
//+1--2--3--4--5--6--7--8--9--0--1--2--3--4--5--6--7--8--9--+
//+   b  判断R是否在执行一个命令                            +
//+1--2--3--4--5--6--7--8--9--0--1--2--3--4--5--6--7--8--9--+
bool     C_MT4R::IsBusy(void){
   return RIsBusy(miHandle);
}
//+1--2--3--4--5--6--7--8--9--0--1--2--3--4--5--6--7--8--9--+
//+   v  执行R代码不等待                                    +
//+1--2--3--4--5--6--7--8--9--0--1--2--3--4--5--6--7--8--9--+
void     C_MT4R::ExecuteAsync(
            string   psCode
         ) {
   RExecuteAsync(miHandle, psCode);
}
//+1--2--3--4--5--6--7--8--9--0--1--2--3--4--5--6--7--8--9--+
//+   v  执行R代码                                          +
//+1--2--3--4--5--6--7--8--9--0--1--2--3--4--5--6--7--8--9--+
void     C_MT4R::Execute(
            string   psCode
         ) {
   RExecute(miHandle, psCode);
}
//+1--2--3--4--5--6--7--8--9--0--1--2--3--4--5--6--7--8--9--+
//+   v  bool类型赋值                                       +
//+1--2--3--4--5--6--7--8--9--0--1--2--3--4--5--6--7--8--9--+
void     C_MT4R::AssignB(
            string   psName,
            bool     pbBoolValue
         ) {
   RAssignBool(miHandle, psName, pbBoolValue);
}
//+1--2--3--4--5--6--7--8--9--0--1--2--3--4--5--6--7--8--9--+
//+   v  int类型赋值                                        +
//+1--2--3--4--5--6--7--8--9--0--1--2--3--4--5--6--7--8--9--+
void     C_MT4R::AssignI(
            string   psName,
            int      piIntValue
         ) {
   RAssignInteger(miHandle, psName, piIntValue);
}
//+1--2--3--4--5--6--7--8--9--0--1--2--3--4--5--6--7--8--9--+
//+   v  string类型赋值                                     +
//+1--2--3--4--5--6--7--8--9--0--1--2--3--4--5--6--7--8--9--+
void     C_MT4R::AssignS(
            string   psName,
            string   psStringValue
         ) {
   RAssignString(miHandle, psName, psStringValue);
}
//+1--2--3--4--5--6--7--8--9--0--1--2--3--4--5--6--7--8--9--+
//+   v  double类型赋值                                     +
//+1--2--3--4--5--6--7--8--9--0--1--2--3--4--5--6--7--8--9--+
void     C_MT4R::AssignD(
            string   psName,
            double   pdDoubleValue
         ) {
   RAssignDouble(miHandle, psName, pdDoubleValue);
}
//+1--2--3--4--5--6--7--8--9--0--1--2--3--4--5--6--7--8--9--+
//+   v  向量类型赋值                                       +
//+1--2--3--4--5--6--7--8--9--0--1--2--3--4--5--6--7--8--9--+
void     C_MT4R::AssignV(
            string   psName,
            double&  pdaVector[]
         ) {
   RAssignVector(miHandle, psName, pdaVector, ArraySize(pdaVector));
}
//+1--2--3--4--5--6--7--8--9--0--1--2--3--4--5--6--7--8--9--+
//+   v  因子类型赋值                                       +
//+1--2--3--4--5--6--7--8--9--0--1--2--3--4--5--6--7--8--9--+
void     C_MT4R::AssignF(
            string   psName,
            string&  psaFactor[]
         ) {
   RAssignStringVector(miHandle, psName, psaFactor, ArraySize(psaFactor));
   Execute(StringConcatenate(psName, " <- as.factor(", psName, ")"));
}
//+1--2--3--4--5--6--7--8--9--0--1--2--3--4--5--6--7--8--9--+
//+   v  矩阵类型赋值                                       +
//+1--2--3--4--5--6--7--8--9--0--1--2--3--4--5--6--7--8--9--+
void     C_MT4R::AssignM(
            string   psName,
            double&  pdaMatrix[],
            int      piRows,
            int      piCols
         ) {
   RAssignMatrix(miHandle, psName, pdaMatrix, piRows, piCols);
}
//+1--2--3--4--5--6--7--8--9--0--1--2--3--4--5--6--7--8--9--+
//+   v  添加一行向量到矩阵                                 +
//+1--2--3--4--5--6--7--8--9--0--1--2--3--4--5--6--7--8--9--+
void     C_MT4R::AppendMatrixRow(
            string   psName,
            double&  pdaVector[],
            int      piSize
         ) {
   RAppendMatrixRow(miHandle, psName, pdaVector, ArraySize(pdaVector));
}
//+1--2--3--4--5--6--7--8--9--0--1--2--3--4--5--6--7--8--9--+
//+   b  判断变量是否存在                                   +
//+1--2--3--4--5--6--7--8--9--0--1--2--3--4--5--6--7--8--9--+
bool     C_MT4R::Exists(
            string   psName
         ) {
   return RExists(miHandle, psName);
}
//+1--2--3--4--5--6--7--8--9--0--1--2--3--4--5--6--7--8--9--+
//+   i  获取int类型值                                      +
//+1--2--3--4--5--6--7--8--9--0--1--2--3--4--5--6--7--8--9--+
int      C_MT4R::GetI(
            string   psExpression
         ) {
   return RGetInteger(miHandle, psExpression);
}
//+1--2--3--4--5--6--7--8--9--0--1--2--3--4--5--6--7--8--9--+
//+   d  获取double类型值                                   +
//+1--2--3--4--5--6--7--8--9--0--1--2--3--4--5--6--7--8--9--+
double   C_MT4R::GetD(
            string   psExpression
         ) {
   return RGetDouble(miHandle, psExpression);
}
//+1--2--3--4--5--6--7--8--9--0--1--2--3--4--5--6--7--8--9--+
//+   b  获取bool类型值                                     +
//+1--2--3--4--5--6--7--8--9--0--1--2--3--4--5--6--7--8--9--+
bool     C_MT4R::GetB(
            string   psExpression
         ) {
   return RGetBool(miHandle, psExpression);
}
//+1--2--3--4--5--6--7--8--9--0--1--2--3--4--5--6--7--8--9--+
//+   s  获取string类型值                                   +
//+1--2--3--4--5--6--7--8--9--0--1--2--3--4--5--6--7--8--9--+
string   C_MT4R::GetS(
            string   psExpression
         ) {
   if(RGetBool(miHandle, StringConcatenate("class(", psExpression, ") != 'character'"))) {
      Print(StringConcatenate(psExpression," is not character!"));
      return "";
   }
   string   lsName = "S2I";
   while(Exists(lsName)) {
      lsName = StringConcatenate(lsName, "9");
   }
   string   lsCode = StringConcatenate(lsName, " <- as.numeric(charToRaw(", psExpression, "))");
   Execute(lsCode);
   double   ldaVector[];
   GetV(lsName, ldaVector);
   char     liaVector[];
   int      liCopied = ArrayCopy(liaVector, ldaVector);
   string   lsString = CharArrayToString(liaVector);
   lsCode = StringConcatenate("rm(", lsName, ")");
   Execute(lsCode);
   return lsString;
}
//+1--2--3--4--5--6--7--8--9--0--1--2--3--4--5--6--7--8--9--+
//+   v  获取Vector                                         +
//+1--2--3--4--5--6--7--8--9--0--1--2--3--4--5--6--7--8--9--+
void     C_MT4R::GetV(
            string   psName,
            double&  pdaVector[]
         ) {
   //RGetVector(miHandle, psName, pdaVector, ArraySize(pdaVector)); 测试暂时不可用
   int      liLen = RGetInteger(miHandle, StringConcatenate("length(", psName, ")"));
   ArrayResize(pdaVector, liLen);
   for(int i=0; i<liLen; i++)   pdaVector[i] = GetD(StringConcatenate(psName, "[", (i+1), "]"));
}
//+1--2--3--4--5--6--7--8--9--0--1--2--3--4--5--6--7--8--9--+
//+   v  加载资源文件                                       +
//+1--2--3--4--5--6--7--8--9--0--1--2--3--4--5--6--7--8--9--+
void     C_MT4R::Source(
            string   psScriptLink
         ) {
   string   lsRCode = StringConcatenate("source('", psScriptLink, "', encoding = 'UTF-8')");
   Execute(lsRCode);
}
//+1--2--3--4--5--6--7--8--9--0--1--2--3--4--5--6--7--8--9--+
//+   v  打印表达式                                         +
//+1--2--3--4--5--6--7--8--9--0--1--2--3--4--5--6--7--8--9--+
void     C_MT4R::PrintR(
            string   psExpression
         ) {
   RPrint(miHandle, psExpression);
}
//+1--2--3--4--5--6--7--8--9--0--1--2--3--4--5--6--7--8--9--0--1--2--3--4--5--6--7--8--9--0--1--2--3--4--5--6--7--8--9--+