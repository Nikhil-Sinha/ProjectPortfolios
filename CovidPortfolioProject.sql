SELECT * FROM Projectportfolio.coviddeath
where iso_code not like "%OWID_AFR%";

SELECT * FROM Projectportfolio.covidvacc;

SELECT location, date, total_cases,new_cases,total_deaths, population
FROM Projectportfolio.coviddeath
where continent is not null
order by 1;

-- looking at total cases vs total deaths
SELECT location, date, total_cases,total_deaths, (total_deaths/total_cases)*100 as DeathPercentage
FROM Projectportfolio.coviddeath
where location like "%United Kingdom%" and continent is not null
order by 1; 

-- looking at the total cases vs the population
SELECT location, date, population, total_cases,(total_cases/population)*100 as InfectionRate
FROM Projectportfolio.coviddeath
where location like "%United Kingdom%" and continent is not null
order by 1; 

-- looking at the total cases vs the population
SELECT location, date, population, total_cases,(total_cases/population)*100 as InfectionRate
FROM Projectportfolio.coviddeath
-- where location like "%United Kingdom%"
order by 1; 

-- Looking at country that has the highest infection rate comapred to the population
SELECT location, population, max(total_cases) as HighestInfectionCount, Max((total_cases/population))*100 as MaxInfectionRate
FROM Projectportfolio.coviddeath
where continent is not null
group by location,population
order by MaxInfectionRate;

-- looking at the country that have the highest death count per population 
SELECT location, Max(cast(total_deaths as signed)) as HighestDeathCount
FROM Projectportfolio.coviddeath
where iso_code not like "%OWID_AFR%" and location not in ("Europe", "High income", "North America", "South America", "European Union")
group by location
order by HighestDeathCount desc;

-- looking at the continent that have the highest death count per population 
SELECT continent, Max(cast(total_deaths as signed)) as HighestDeathCount
FROM Projectportfolio.coviddeath
where continent is not null
group by continent
order by HighestDeathCount desc;

Select *
from Projectportfolio.covidvacc;



-- joining both the tables 
select *
from Projectportfolio.coviddeath as dea
join Projectportfolio.covidvacc as vac
on dea.location = vac.location
and dea.date = vac.date;


-- looking at total population vs vaccination 
select dea.continent, dea.location, dea.date, dea.population, vac.new_vaccinations as 'new vacc per day'
from Projectportfolio.coviddeath as dea
join Projectportfolio.covidvacc as vac
on dea.location = vac.location
and dea.date = vac.date
order by 2,3;

-- looking at total population vs vaccination 
select dea.continent, dea.location, dea.date, dea.population, vac.new_vaccinations as 'new vacc per day', 
		sum(cast(vac.new_vaccinations as signed)) over (partition by dea.location order by dea.location, dea.date) as RollingPeopleVaccinated
from Projectportfolio.coviddeath as dea
join Projectportfolio.covidvacc as vac
on dea.location = vac.location
and dea.date = vac.date
order by 2,3;

-- using a common table expression 
with PopvsVacc (continent, location, date, population, new_vaccinations, RollingPeopleVaccinated)
as
(
select dea.continent, dea.location, dea.date, dea.population, vac.new_vaccinations,
		sum(cast(vac.new_vaccinations as signed)) over (partition by dea.location order by dea.location, dea.date) as RollingPeopleVaccinated
        -- (RollingPeopleVaccinated/population)*100
from Projectportfolio.coviddeath as dea
join Projectportfolio.covidvacc as vac
on dea.location = vac.location
and dea.date = vac.date
where dea.continent is not null
-- order by 2,3;
)
select *,
 (RollingPeopleVaccinated/population)*100
from PopvsVacc;

-- creating a temp table 
drop table if exists Percentpopvaccincated;
create table Percentpopvaccincated
(Continent varchar(255),
location varchar(255),
date datetime,
population int,
new_vaccinations int,
RollingPeopleVaccinated int
); 

Insert into Percentpopvaccincated
select dea.continent, dea.location,
STR_TO_DATE(dea.date, '%d/%m/%Y'),
dea.population, 
NULLIF(vac.new_vaccinations, ''),
		sum(vac.new_vaccinations) over (partition by dea.location order by dea.location, dea.date) as RollingPeopleVaccinated
        -- (RollingPeopleVaccinated/population)*100
from Projectportfolio.coviddeath as dea
join Projectportfolio.covidvacc as vac
on dea.location = vac.location
and dea.date = vac.date
where dea.continent is not null;
-- order by 2,3;

select *, (RollingPeopleVaccinated/population)*100
from Percentpopvaccincated;


-- Creating view to store data for later visualitaion

create view Percentpopvaccincated as
select dea.continent, dea.location,
STR_TO_DATE(dea.date, '%d/%m/%Y'),
dea.population, 
NULLIF(vac.new_vaccinations, ''),
		sum(vac.new_vaccinations) over (partition by dea.location order by dea.location, dea.date) as RollingPeopleVaccinated
        -- (RollingPeopleVaccinated/population)*100
from Projectportfolio.coviddeath as dea
join Projectportfolio.covidvacc as vac
on dea.location = vac.location
and dea.date = vac.date
where dea.continent is not null;
-- order by 2,3; 



























