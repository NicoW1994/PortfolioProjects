-- Data Exploration in SQL
-- Nico Wagner
-- 23.02.2022

-- covid deaths dataset
-- source data: https://ourworldindata.org/covid-deaths

-- data consists of 2 different tables
-- only select data that ist country specific
Select *
From covid_data.covid_deaths
Where continent is not null
order by 3,4;

Select *
From covid_vaccines
Where continent is not NULL
order by 3,4;

-- Select Data that is going to be used for the project
Select Location, date, total_cases, new_cases, total_deaths, population
From covid_data.covid_deaths
order by 1,2;

-- Looking at Total Cases vs Total Deaths
-- Shows likelihood of dying if you caught covid in your country
Select Location, date, total_cases, new_cases, total_deaths, (total_deaths/total_cases)*100 as death_percentage
From covid_data.covid_deaths
Where location = 'Germany'
order by 1,2;

-- Looking at Total Cases vs Population
-- Shows what percentage of population got Covid
Select Location, date, Population, total_cases, (total_cases/population)*100 as cases_per_capita
From covid_data.covid_deaths
Where location = 'Germany'
order by 1,2;

-- Countries with the highest infection rate
Select Location, Population, MAX(total_cases) as highest_infection_count, MAX((total_cases/population)*100) as percent_population_infected
From covid_data.covid_deaths
Where continent is not NULL
Group by Location, Population
order by percent_population_infected desc;

-- Separate data by continents
Select continent, MAX(total_deaths) as total_death_count
From covid_data.covid_deaths
Where continent is not NULL
Group by continent
order by total_death_count desc;

-- Countries with the highest death count per Population
Select Location, MAX(total_deaths) as total_death_count
From covid_data.covid_deaths
Where continent is not NULL
Group by Location
order by total_death_count desc;

-- Global numbers
Select date, SUM(new_cases) as total_cases, SUM(new_deaths) as total_deaths, SUM(new_deaths)/SUM(new_cases)*100 as death_percentage
From covid_data.covid_deaths
-- Where location = 'Germany'
where continent is not NULL
Group by date
order by 1,2;

-- total new_cases
Select SUM(new_cases) as total_cases, SUM(new_deaths) as total_deaths, SUM(new_deaths)/SUM(new_cases)*100 as death_percentage
From covid_data.covid_deaths
-- Where location = 'Germany'
where continent is not NULL
order by 1,2;

-- Lookinokg at total population vs vaccinations
-- join tables
-- USE CTE
-- numbers of columns have to match

With PopvsVac (Continent, Location, Date, Population, New_Vaccinations ,Rolling_people_vaccinated)
AS(
Select deaths.continent, deaths.location, deaths.date, deaths.population, vacs.new_vaccinations, SUM(vacs.new_vaccinations)
 Over (Partition by deaths.location Order by deaths.location, deaths.date) as Rolling_people_vaccinated
From covid_data.covid_deaths deaths
Join covid_data.covid_vaccines vacs
    On deaths.location = vacs.location
    and deaths.date = vacs.date
where deaths.continent is not null 
-- order by 2,3
)
Select *, (Rolling_people_vaccinated/Population)*100
From PopvsVac;

-- Temp table
DROP Table if exists PercentPopulationVaccinated;

Create Table PercentPopulationVaccinated (
	Continent varchar(255),
	Location varchar(255),
	date datetime,
	Population numeric,
	New_vaccinations numeric,
    RollingPeopleVaccinated bigint
);

Insert into PercentPopulationVaccinated
Select deaths.continent, deaths.location, deaths.date ,deaths.population, vacs.new_vaccinations, 
SUM(vacs.new_vaccinations) Over (Partition by deaths.location Order by deaths.location, deaths.date) as RollingPeopleVaccinated
From covid_data.covid_deaths deaths
Join covid_data.covid_vaccines vacs
    On deaths.location = vacs.location
    and deaths.date = vacs.date
-- where deaths.continent is not null
-- order by 2,3
;

Select *, (RollingPeopleVaccinated/Population)*100
From PercentPopulationVaccinated;

-- Creating View to store data for later visualisations
Create View Percent_population_vaccinated as
Select deaths.continent, deaths.location, deaths.date, deaths.population, vacs.new_vaccinations, 
SUM(vacs.new_vaccinations) Over (Partition by deaths.location Order by deaths.location, deaths.date) as Rolling_people_vaccinated
From covid_data.covid_deaths deaths
Join covid_data.covid_vaccines vacs
    On deaths.location = vacs.location
    and deaths.date = vacs.date
where deaths.continent is not null;

Select * From percent_population_vaccinated;


-- END
