-- 7. The name, date established, and area of the 10 smallest parks.
-- (10 rows)

SELECT TOP 10 park_name, date_established, area
    FROM park
    ORDER BY area;
