data sum;
infile "/home/u59374413/Measurement/0315/ADH.DAT" end=end;
input subject gender group @15 (item1-item24)(1.);
run;

proc transpose data = sum prefix=subject name = items  out=transRes;
var item1-item24;
run;

title "Item Difficulty (Easiness/Popularity/Prevalence) Indexes";
title3 "Analysis for All Items";
title5 "+ = Easy Item         - = Difficult Item";

data item;
set transRes;
itemDiff = mean(of subject1-subject1400 );
if itemDiff ge .70 then Level='+';
if itemDiff lt .30 then Level='-';
run;


proc means Data=sum n mean std min max maxdec=2 fw=5;
title "Analysis for All Items";
var ItemDifficultyIndex;
run;

data sum;
set sum;
if Level =' ';
title "Item Difficulty (Easiness/Popularity/Prevalence) Indexes";
title3 "Analysis for Retained Items";
proc print;
var Item ItemDifficultyIndex;
run;
title;
proc means Data=sum n mean std min max maxdec=2 fw=5;
title3 "Analysis for Retained Items";
var ItemDifficultyIndex;
run;
title;
