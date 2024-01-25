--STEP 1: SELECT ALL VARIABLES OF INTEREST FROM THE PATENTS DATABASE
SELECT 
	publication_number, 
	country_code,
	kind_code,
	publication_date,
	grant_date,
	assignee_harmonized,
	ipc, 
	cpc,
	SUBSTR(CAST(grant_date AS STRING), 1, 4) AS year
FROM 
	`patents-public-data.patents.publications` 
--save data as `full_ipc_cpc`


--STEP 2: SELECT ALL DISTINCT PUBLICATION_NUMBER CITED IN THE US-B1-B2 DATABASE (final_kindB)
SELECT DISTINCT citation_detail.publication_number AS citation_pub_num
FROM `uplifted-nuance-405910.statsapp_data.final_kindB` fb
CROSS JOIN UNNEST(fb.citation) AS citation_detail
--save data as `all_citation`


--STEP 3: 
SELECT *
FROM `statsapp_data.all_citation`
JOIN `uplifted-nuance-405910.statsapp_data.full_ipc_cpc` f 
ON citation_pub_num = f.publication_number
--data exported
