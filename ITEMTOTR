title "Item Discrimination and Test Internal Consistency";
proc corr data=meg.test2 alpha nocorr ;
   var
item1-item25

;

ods output CronbachAlphaDel=itemr;
run;
data itemr;
set itemr;
RawItemTotalCorrelation=RawCorr;
Item=Variable;
title "Corrected Item-Total Correlations";
proc print data=itemr;
var Item RawItemTotalCorrelation ;
run;
title "Mean, Upper, and Lower";
title2 "Corrected Raw Item-Total Correlations";
proc means data=itemr n mean min max maxdec=2 fw=5;
var RawItemTotalCorrelation;
run;
title;
data itemr;
set itemr;
CorrectedItemTotalCorrelation=Stdcorr;
title "Standardized Item-Total Correlations";
proc print data=itemr;
var Item CorrectedItemTotalCorrelation ;
run;
title "Mean, Upper, and Lower";
title2 "Standardized Corrected Item-Total Correlations";
proc means data=itemr n mean min max maxdec=2 fw=5;
var CorrectedItemTotalCorrelation;
run;
title;
