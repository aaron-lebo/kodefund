select id, 're:' as title, comment as content, time from comments 
	where project_id = ? 
union 
select id, title, update as content, time from updates 
	where project_id = ?
union
select id, 're:update' as title, comment as content, time from comments 
	where update_id in (select id from updates where project_id = ?) 
union
select id, 'revision ' || number as title, reason as content, date as time from revisions
	where project_id = ?
order by time desc limit 10