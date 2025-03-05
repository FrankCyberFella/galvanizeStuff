-- 21. The name and nickname for the largest state.
-- (1 row)

SELECT TOP 1 state_name, state_nickname
    FROM state
    ORDER BY area DESC;
