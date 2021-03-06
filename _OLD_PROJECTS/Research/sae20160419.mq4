#property copyright     "Mihawk, Accretion Investment Corporation"
#property link          "http:// "
#property version       "1.00"
#property description   "  "

#property strict
//+1--2--3--4--5--6--7--8--9--0--1--2--3--4--5--6--7--8--9--+
//+	Inputs                                                +
//+1--2--3--4--5--6--7--8--9--0--1--2--3--4--5--6--7--8--9--+
input int      TIMER = 5;                                   // 计时器时间间隔

input int            BarCount = 200;


input int         Magic = 20160419;
//+1--2--3--4--5--6--7--8--9--0--1--2--3--4--5--6--7--8--9--+
//+	头文件 - 包含文件
//+1--2--3--4--5--6--7--8--9--0--1--2--3--4--5--6--7--8--9--+
#include <\PRIVATES\ACCRETION2016\EXTERN\R\MT4R.mqh>
//+1--2--3--4--5--6--7--8--9--0--1--2--3--4--5--6--7--8--9--0--1--2--3--4--5--6--7--8--9--0--1--2--3--4--5--6--7--8--9--0--1--2--3--4--5--6--7--8--9--+
//+	EA:   SAE
//+	新测试，这次要进行深度测试，不轻易放弃
//+	Version: Editing
//+	Update:  2016-04-19  此版本多处写死，仅作简化测试之用
//+1--2--3--4--5--6--7--8--9--0--1--2--3--4--5--6--7--8--9--0--1--2--3--4--5--6--7--8--9--0--1--2--3--4--5--6--7--8--9--0--1--2--3--4--5--6--7--8--9--+
C_MT4R   R;

int   giLastTime = -1;
//+1--2--3--4--5--6--7--8--9--0--1--2--3--4--5--6--7--8--9--+
//+	ExpertAdvisor - 初始化函数
//+1--2--3--4--5--6--7--8--9--0--1--2--3--4--5--6--7--8--9--+
int OnInit() {
//##  
   if(!IsDllsAllowed()) {
      MessageBox("You need to turn on \'Allow DLL imports\'");
   }
   if(IsTradeAllowed() == false) Alert("Trading forbidden!");
   EventSetTimer(TIMER);
//##  启动R
   R.Start();
   
   string   lsSourceCode = StringConcatenate("source('F:/sae2016v0/sae20160419.R', encoding = 'UTF-8')");
//Print(lsSourceCode);
   R.Execute(lsSourceCode);
//Print(R.IsRunning());
   return INIT_SUCCEEDED;
}
//+1--2--3--4--5--6--7--8--9--0--1--2--3--4--5--6--7--8--9--+
//+	ExpertAdvisor - 反初始化函数
//+1--2--3--4--5--6--7--8--9--0--1--2--3--4--5--6--7--8--9--+
void     OnDeinit (
            const int      reason
         ) {
//##  
   
   R.Stop();
}
//+1--2--3--4--5--6--7--8--9--0--1--2--3--4--5--6--7--8--9--+
//+	ExpertAdvisor - 运行函数
//+1--2--3--4--5--6--7--8--9--0--1--2--3--4--5--6--7--8--9--+
void     OnTick() {

}
//+1--2--3--4--5--6--7--8--9--0--1--2--3--4--5--6--7--8--9--+
//+	ExpertAdvisor - 图表事件函数
//+1--2--3--4--5--6--7--8--9--0--1--2--3--4--5--6--7--8--9--+
void     OnChartEvent (
            const int      id,
            const long    &lparam,
            const double  &dparam,
            const string  &sparam
         ) {
   
}
//+1--2--3--4--5--6--7--8--9--0--1--2--3--4--5--6--7--8--9--+
//+	ExpertAdvisor - 计时器函数                            +
//+1--2--3--4--5--6--7--8--9--0--1--2--3--4--5--6--7--8--9--+
void     OnTimer() {
//Print(R.IsRunning());
//Print(GetSignal(Symbol(), Period()));
   if(Time[0] == giLastTime) return;
   int liSig = GetSignal(Symbol(), Period());
   Request(liSig);
   
//Print(GetSignal(Symbol(), Period()));
   giLastTime = Time[0];
}


int      GetSignal(
            string   psSymbol,
            int      piPeriod
         ) {
   int      liCopied;
   datetime ltaTime[];
   double   ldaTime[];
   double   ldaOpen[];
   double   ldaHigh[];
   double   ldaLow[];
   double   ldaClose[];
   liCopied = CopyTime(psSymbol, piPeriod, 1, BarCount, ltaTime);
   ArrayCopy(ldaTime, ltaTime);
   liCopied = CopyOpen(psSymbol, piPeriod, 1, BarCount, ldaOpen);
   liCopied = CopyHigh(psSymbol, piPeriod, 1, BarCount, ldaHigh);
   liCopied = CopyLow(psSymbol, piPeriod, 1, BarCount, ldaLow);
   liCopied = CopyClose(psSymbol, piPeriod, 1, BarCount, ldaClose);
   R.AssignV("T", ldaTime);
   R.AssignV("O", ldaOpen);
   R.AssignV("H", ldaHigh);
   R.AssignV("L", ldaLow);
   R.AssignV("C", ldaClose);
   //string   lsPriceName = StringConcatenate("PRICE_", psSymbol, "_", piPeriod);
   
   //string   lsRCode = StringConcatenate(lsPriceName, " <- MT4DataToXTS('", psSymbol, "', '", IntegerToString(piPeriod), "', T, O, H, L, C)");
   string   lsRCode = StringConcatenate("price <- MT4DataToXTS('", psSymbol, "', '", IntegerToString(piPeriod), "', T, O, H, L, C)");

   R.Execute(lsRCode);
//Print("1:"+R.IsRunning());
   lsRCode = StringConcatenate("input <- Data.Input(price)");
   R.Execute(lsRCode);
//Print("2:"+R.IsRunning());
   lsRCode = StringConcatenate("signal <- GetSignals(model.building, input)");
   R.Execute(lsRCode);
//Print("3:"+R.IsRunning());
   return(R.GetI("signal"));
}


int   Request(int sig) {
   int liHolding = CheckHolding();
   if(sig == 1) {
      if(liHolding == 1) return 0;
      if(liHolding == -1) DoClose();
      OrderSend(Symbol(), OP_BUY, 1.00, Ask, 10, 0, 0, NULL, Magic);
   } else if(sig == -1) {
      if(liHolding == -1) return 0;
      if(liHolding == 1) DoClose();
      OrderSend(Symbol(), OP_SELL, 1.00, Bid, 10, 0, 0, NULL, Magic);
   }
   return 0;
   
}

int   CheckHolding() {
   int liOrdersTotal = OrdersTotal();
   if(liOrdersTotal == 0) return 0;
   if(OrderSelect(0, SELECT_BY_POS)) {
      if(OrderSymbol() == Symbol() && OrderMagicNumber() == Magic) {
         if(OrderType() == OP_BUY)  return 1;
         else if(OrderType() == OP_SELL)return -1;
      }
   }
   return 0;
}

int   DoClose() {
   if(OrderSelect(0, SELECT_BY_POS)) {
      if(OrderSymbol() == Symbol() && OrderMagicNumber() == Magic) {
         OrderClose(OrderTicket(), OrderLots(), OrderClosePrice(), 10);
      }
   }
   return 0;
}