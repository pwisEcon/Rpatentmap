SELECT 
	publication_number, 
	application_number,
	country_code,
	kind_code,
	application_kind,
	application_number_formatted,
	pct_number,
	family_id,
	spif_publication_number,
	spif_application_number,
	publication_date,
	filing_date,
	grant_date,
	inventor_harmonized,
	assignee_harmonized,
	examiner,
	uspc, 
	ipc, 
	cpc, 
	citation,
	parent, 
	child,
	entity_status,
	art_unit,
	SUBSTR(CAST(grant_date AS STRING), 1, 4) AS year
FROM 
	`patents-public-data.patents.publications` 
WHERE 
	country_code="US" AND (kind_code="B2" OR kind_code="B1")
--save data as `final_kindB`
