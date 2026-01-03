-- 1. THE COMMENT TRAP
-- We need to update the status; do not forget!
CREATE TABLE logs (id INT, entry TEXT);

/* 2. THE BLOCK COMMENT TRAP
   Everything inside here; is ignored by SQLite.
*/
INSERT INTO logs VALUES (1, 'System initialized');

-- 3. THE STRING LITERAL TRAP
INSERT INTO logs VALUES (2, 'Error: User typed a semicolon; right here');

-- 4. THE TRIGGER/PROCEDURAL TRAP
CREATE TRIGGER update_log_time AFTER INSERT ON logs
BEGIN
    UPDATE logs SET entry = entry || ' [PROCESSED]' WHERE id = NEW.id;
END;
