-- Rpatentmap in BigQuery
-- Author: Patryk Wisniewski
-- Created: 04/2024
-- Cost: ~150GB


CREATE TABLE `feisty-ceiling-417014.rpatentmap2.ll_b2` AS (
SELECT 
  gp.publication_number, 
  gp.application_number,
  gp.publication_date,
  gp.filing_date,
  gp.assignee_harmonized,
FROM `feisty-ceiling-417014.rpatentmap2.gpatent_bus` as gp
WHERE gp.kind_code="B2" AND gp.year>2005 AND gp.year<2020
);

CREATE TABLE `feisty-ceiling-417014.rpatentmap2.ll_a1` AS (
SELECT 
  gp.publication_number, 
  gp.application_number,
  gp.publication_date,
  gp.filing_date,
  gp.assignee_harmonized,
FROM `feisty-ceiling-417014.rpatentmap2.gpatent_bus` as gp
INNER JOIN `feisty-ceiling-417014.rpatentmap2.ll_b2` as gp2
ON gp.application_number=gp2.application_number
WHERE gp.kind_code="A1" 
);


CREATE TABLE `feisty-ceiling-417014.rpatentmap2.cit_a1` AS (
SELECT 
  lla1.publication_number,
  lla1.application_number,
  lla1.publication_date,
  gp.publication_number as pub_cit, 
  gp.publication_date as date_pub_cit,
  gp.filing_date as date_fil_cit
FROM 
`feisty-ceiling-417014.rpatentmap2.gpatent_bus` as gp,
UNNEST(gp.citation) as cit
JOIN `feisty-ceiling-417014.rpatentmap2.ll_a1` as lla1
ON cit.publication_number = lla1.publication_number
);

CREATE TABLE `feisty-ceiling-417014.rpatentmap2.cit_b2` AS (
SELECT 
  llb2.publication_number,
  llb2.application_number,
  llb2.publication_date,
  llb2.filing_date,
  gp.publication_number as pub_cit, 
  gp.publication_date as date_pub_cit,
  gp.filing_date as date_fil_cit
FROM 
`feisty-ceiling-417014.rpatentmap2.gpatent_bus` as gp,
UNNEST(gp.citation) as cit
JOIN `feisty-ceiling-417014.rpatentmap2.ll_b2` as llb2
ON cit.publication_number = llb2.publication_number
);

