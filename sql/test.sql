-- !preview conn=con

CREATE TABLE test_table (
    id INT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    name VARCHAR(100)
);
