-- Rpatentmap in BigQuery
-- Author: Patryk Wisniewski
-- Created: 11/2023
-- Cost: ~400GB

-- CREATE NEW SCHEMA
CREATE SCHEMA `feisty-ceiling-417014.rpatentmap`; --replace feisty-ceiling-417014 with your project ID

-- SELECT USA B1/B2 PATENTS FROM GATENTS AS 'GPATENT_BUS'
CREATE TABLE `feisty-ceiling-417014.rpatentmap.gpatent_bus` AS (
  SELECT 
    gp.publication_number, 
    gp.country_code,
    gp.kind_code,
    gp.application_kind,
    gp.application_number_formatted,
    gp.pct_number,
    gp.family_id,
    gp.publication_date,
    gp.filing_date,
    gp.grant_date,
    gp.inventor_harmonized,
    gp.assignee_harmonized,
    gp.uspc, 
    gp.ipc, 
    gp.cpc, 
    gp.citation,
    gp.parent, 
    gp.child,
    cast(gp.publication_date/10000 as int) as year
  FROM 
    `patents-public-data.patents.publications` as gp
  WHERE 
    gp.country_code="US" AND (gp.kind_code="B2" OR gp.kind_code="B1")
);

-- SELECT ALL PATENTS FROM GPATENTS DATABASE TO GET CITATIONS AS 'GPATENT_ALL'
CREATE TABLE `feisty-ceiling-417014.rpatentmap.gpatent_all` AS (
  SELECT 
    gp.publication_number, 
    gp.country_code,
    gp.kind_code,
    gp.publication_date,
    gp.filing_date,
    gp.grant_date,
    gp.assignee_harmonized,
    gp.ipc, 
    gp.cpc,
    cast(gp.publication_date/10000 as int) as year
  FROM 
    `patents-public-data.patents.publications` as gp
);

-- SELECT ALL DISTINCT PUBLICATION_NUMBERS CITED IN "GPATENT-BUS" AS 'BUS_CITED'
CREATE TABLE `feisty-ceiling-417014.rpatentmap.bus_cited` AS(
  SELECT DISTINCT 
    bus.publication_number,
    cit.publication_number AS citation_pub_num
  FROM `feisty-ceiling-417014.rpatentmap.gpatent_bus` bus,
  UNNEST(bus.citation) AS cit
);

-- SIMILAR PATENTS GOOGLE
CREATE TABLE `feisty-ceiling-417014.rpatentmap.g_res` AS(
  SELECT 
    pat.publication_number,
    pat.similar,
    SUBSTR(pat.publication_number, 1, 2) AS country_code
  FROM 
    `patents-public-data.google_patents_research.publications` as pat
); 

-- UNNEST ASSIGNEES CODES FROM "GRES" (SIMILAR)
CREATE TABLE `feisty-ceiling-417014.rpatentmap.simpat_us` AS(
  SELECT 
    g_res.publication_number,
    g_res.country_code,
    spat.publication_number as publication_number_sim
  FROM 
    `feisty-ceiling-417014.rpatentmap.g_res` as g_res,
    UNNEST(g_res.similar) as spat
  WHERE
    g_res.country_code="US" AND 
    SUBSTR(spat.publication_number, 1, 2)="US"
);

-- DATABASE TO BE EXPORTED -------------------------------------

-- MAIN DATABASE: US-PATENTS B1/B2
CREATE TABLE `feisty-ceiling-417014.rpatentmap.exp_main_bus` AS(
  SELECT 
    bus.publication_number,
    bus.publication_date,
    bus.year,
    bus.kind_code
  FROM `feisty-ceiling-417014.rpatentmap.gpatent_bus` as bus
);  

-- UNNEST CPC CODES FROM "GPATENT_BUS"
CREATE TABLE `feisty-ceiling-417014.rpatentmap.exp_cpc_bus` AS(
  SELECT 
    bus.publication_number, 
    cpc.code,
    cpc.first, 
    cpc.inventive
  FROM 
    `feisty-ceiling-417014.rpatentmap.gpatent_bus` as bus,
    UNNEST(bus.cpc) as cpc
);

-- UNNEST ASSIGNEES CODES FROM "GPATENT_ALL"
CREATE TABLE `feisty-ceiling-417014.rpatentmap.exp_assignees_all` AS(
  SELECT 
    g_all.publication_number, 
    g_all.country_code,
    g_all.kind_code,
    g_all.year,
    asg.name as assignee_name,
    asg.country_code as assignee_country
  FROM 
    `feisty-ceiling-417014.rpatentmap.gpatent_all` as g_all,
    UNNEST(g_all.assignee_harmonized) as asg
);

-- JOIN 'BUS_CITED' WITH 'GPATENT_ALL' AS 'CITATIONS_DATA'
CREATE TABLE `feisty-ceiling-417014.rpatentmap.exp_citations_allbus` AS(
  SELECT 
    c_bus.publication_number,
    c_bus.citation_pub_num, 
    g_all.country_code,
    g_all.kind_code,
    g_all.publication_date,
    g_all.year
  FROM `feisty-ceiling-417014.rpatentmap.bus_cited` as c_bus
  JOIN `feisty-ceiling-417014.rpatentmap.gpatent_all` as g_all 
  ON c_bus.citation_pub_num = g_all.publication_number
);  

-- CREATE 'BCK_COUNTRY' AS SOME CITED ASSIGNEES'S COUNTRY CODES ARE NOT DEFINED
CREATE TABLE `feisty-ceiling-417014.rpatentmap.exp_bck_country` AS(
  SELECT
    DISTINCT(assignee.name) as assignee_name,
    assignee.country_code as assignee_country_backup
  FROM 
    `patents-public-data.patents.publications`,
    UNNEST(assignee_harmonized) as assignee
  WHERE 
    assignee.country_code!=""
);

CREATE TABLE `feisty-ceiling-417014.rpatentmap.simpat_bus` AS(
  SELECT 
    gbus.publication_number,
    gbus.year,
    simpat.publication_number_sim,
    REGEXP_EXTRACT(simpat.publication_number, '-([^ -]*)$') AS kind_code,
    REGEXP_EXTRACT(simpat.publication_number_sim, '-([^ -]*)$') AS kind_code_sim
  FROM `feisty-ceiling-417014.rpatentmap.gpatent_bus` as gbus
  JOIN `feisty-ceiling-417014.rpatentmap.simpat_us` as simpat 
  ON gbus.publication_number = simpat.publication_number
);  

DELETE FROM `feisty-ceiling-417014.rpatentmap.simpat_bus`
WHERE kind_code NOT IN ("B1","B2", "A") OR kind_code_sim NOT IN ("B1","B2", "A");

-- note: we exported in the apache parquet format