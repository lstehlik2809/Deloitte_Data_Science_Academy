use AA18Q3_DSA_GDELT
go

-- Home Assignment 2: 
-- List top three geo locations for each country. Locations in each country should be ranked 
-- by number of events through ActionGeoFeatureId.

with t1 as ( -- country 
select c.CountryName, g.GeoFullName 
from input.event e
left join input.geo g on e.ActionGeoFeatureId = g.GeoFeatureId
left join input.countrycode c on g.GeoCountryCode = c.CountryCode 
), 

t2 as (-- n_events
select CountryName, GeoFullName, n_events = count(1)
from t1
group by CountryName, GeoFullName
),

t3 as ( -- seq
select t2.*, seq = row_number() over (partition by CountryName order by n_events desc)
from t2 
)

select CountryName, GeoFullName, n_events 
from t3
where seq <= 3
order by CountryName