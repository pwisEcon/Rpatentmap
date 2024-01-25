--Aggregate per firm, country, kind_code and year
SELECT
  SUBSTR(CAST(grant_date AS STRING), 1, 4) AS year,
  kind_code,
  assignee,
  country_code,
  COUNT(DISTINCT publication_number) AS n
FROM
  `patents-public-data.patents.publications`,
  UNNEST(assignee) as assignee
GROUP BY
  year,
  kind_code,
  assignee,
  country_code
  
--Aggregate per country, kind_code and year
SELECT
  SUBSTR(CAST(grant_date AS STRING), 1, 4) AS year,
  kind_code,
  country_code,
  COUNT(*) AS n
FROM
  `patents-public-data.patents.publications`
GROUP BY
  year,
  kind_code,
  country_code
  
--Aggregate per assignee, in the US, 2011
WITH aggreg1 AS (
  SELECT
    assignee.name,
    COUNT(DISTINCT publication_number) AS n
  FROM
    `statsapp_data.final_kindB`,
    UNNEST(assignee_harmonized) as assignee
  WHERE 
    grant_date>20109999 AND grant_date<20119999
  GROUP BY
    assignee.name
)

SELECT *
FROM aggreg1
WHERE n>10