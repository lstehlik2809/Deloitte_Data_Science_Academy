use AA18Q3_DSA_GDELT
go

-- Creating two observation dates
if object_id('obs_date') is not null drop table obs_date;

create table obs_date
(obs_date date);

insert into obs_date
 values ('2014-12-31'),('2015-12-31');

with actors as (-- Selecting distinct actors as of >= 2014-01-01
select distinct(a.ActorId)  
from input.actor a
left join input.event e on (a.ActorId = e.Actor1Id or a.ActorId = e.Actor2Id)   
where e.sqldate >= '2014-01-01'
),

base as(-- Creating base
select * from actors
cross join obs_date
),

event as(-- Preparing smaller event table & creating obs_date_cat variable for later group_by operations
select EventCode
     , Actor1Id
	 , Actor2Id
	 , NumMentions
	 , NumSources
	 , NumArticles
	 , AvgTone
     , case when (e.sqldate >= '2014-01-01' and e.sqldate <= '2014-12-31') then '2014-12-31' else '2015-12-31' end as obs_date_cat   
from input.event e
where e.sqldate >= '2014-01-01'
),

supp as(-- Supp table for calculating predictors across last 12 months for individual actors in 2014 and 2015 
select b.ActorId
     , b.obs_date
	 , e.Actor1Id
	 , e.Actor2Id
     , e.AvgTone
	 , e.NumArticles
	 , e.NumMentions
	 , e.NumSources
	 , e.EventCode
from base b
right join event e on (b.ActorId = e.Actor1Id or b.ActorId = e.Actor2Id) and b.obs_date = e.obs_date_cat
),

supp2 as(-- adding Goldstein's score 
select s.*, ec.GoldsteinScale
from supp s
left join input.eventcode ec on s.EventCode = ec.EventCode
),

preds as(-- predictors for individual actors in 2014 and 2015 
select ActorId, obs_date
     , count(1) as n_events_Sum_12M
     , avg(AvgTone) as AvgTone_Avg_12M
	 , max(AvgTone) as AvgTone_Max_12M
	 , min(AvgTone) as AvgTone_Min_12M
	 , sum(NumMentions) as NumMentions_Sum_12M
	 , avg(NumMentions) as NumMentions_Avg_12M
	 , sum(NumSources) as NumSources_Sum_12M
	 , avg(NumSources) as NumSources_Avg_12M
	 , avg(GoldsteinScale) as GoldsteinScale_Avg_12M
	 , max(GoldsteinScale) as GoldsteinScale_Max_12M
	 , min(GoldsteinScale) as GoldsteinScale_Min_12M
from supp2
group by ActorId, obs_date
)

select * from preds  
