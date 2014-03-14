WITH RECURSIVE parents AS (
	SELECT *, 0 as level FROM revisions WHERE project_id = ? and parent is null
   	UNION ALL
  	SELECT revisions.*, parents.level + 1 as level FROM revisions JOIN parents ON revisions.project_id = parents.project_id and revisions.parent = parents.number)
SELECT * FROM parents