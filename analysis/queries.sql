-- fr
select sum(cserved) * 1.0 /sum(ctreq) from data_perf;
select sum(eserved) * 1.0 /sum(etreq) from data_perf;

-- ecpm
select sum(cburn) * 1.0/sum(cserved) from data_perf;
select sum(eburn) * 1.0/sum(eserved) from data_perf;

 -- ecpr
select sum(cburn) * 1.0/sum(ctreq) from data_perf;
select sum(eburn) * 1.0/sum(etreq) from data_perf;



-- fr
select sum(cserved) * 1.0 /sum(ctreq) from data_rtb;
select sum(eserved) * 1.0 /sum(etreq) from data_rtb;

-- ecpm
select sum(cburn) * 1.0/sum(cserved) from data_rtb;
select sum(eburn) * 1.0/sum(eserved) from data_rtb;

-- ecpr
select sum(cburn) * 1.0/sum(ctreq) from data_rtb;
select sum(eburn) * 1.0/sum(etreq) from data_rtb;


-- rollup to site level
create temporary table rtb as
select 
"site.id" as site, sum(cburn) as cburn, sum(ctreq) as ctreq, sum(cserved) as cserved, sum(crendered) as crendered, 
sum(eburn) as eburn, sum(etreq) as etreq,  sum(eserved) as eserved, sum(erendered) as erendered
from data_rtb group by "site.id"; 


-- pick details
select 
site, eburn * 1000.0/erendered as eecpm, cburn * 1000.0/crendered as cecpm, eserved * 1.0/etreq as efr,  cserved * 1.0/ctreq as cfr,
eburn * 1.0 / etreq as eecpr, cburn * 1.0 / ctreq as cecpr, etreq, ctreq, (etreq - ctreq) * 100.0 / ctreq, (eburn - cburn) as deltaburn
from rtb order by deltaburn asc limit 10;


-- compare request numbers.
select etreq, ctreq, (etreq - ctreq) * 100.0 / ctreq from rtb order by 1 desc;

