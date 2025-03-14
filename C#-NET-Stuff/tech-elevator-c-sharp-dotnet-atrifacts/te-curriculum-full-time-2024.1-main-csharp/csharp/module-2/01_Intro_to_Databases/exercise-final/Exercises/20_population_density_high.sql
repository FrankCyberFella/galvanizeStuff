-- 20. The name, population, area, and population density (name the column 'population_density') of states, territories, and districts with more than 100 people per square kilometer.
-- Population density is expressed as people per square kilometer. In other words, population divided by area.
-- Order the results by population density, highest first.
-- (12 rows)

SELECT state_name, population, area, (population/area) AS population_density
    FROM state
    WHERE (population/area) > 100
    ORDER BY population_density DESC;
