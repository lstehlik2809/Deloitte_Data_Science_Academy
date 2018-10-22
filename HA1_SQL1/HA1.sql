use AA18Q3_DSA_GDELT
go

-- Home Assignment 1: 
-- List all religions for which an actor does not exist who has the religion as either first or second religion

select a.ReligionCode, a.ReligionName, b.ActorReligion1Code, c.ActorReligion2Code 
from input.religioncode a
left join input.actor b on a.ReligionCode = b.ActorReligion1Code 
left join input.actor c on a.ReligionCode = c.ActorReligion2Code
where b.ActorReligion1Code is NULL and c.ActorReligion2Code is NULL 
order by a.ReligionCode 